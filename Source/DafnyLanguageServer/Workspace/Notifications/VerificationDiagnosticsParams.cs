using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection.Metadata;
using DafnyCore.Verifier;
using MediatR;
using Microsoft.Boogie;
using Microsoft.Dafny.LanguageServer.Language;
using OmniSharp.Extensions.JsonRpc;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

#pragma warning disable CS8618 // Non-nullable field must contain a non-null value when exiting constructor. Consider declaring as nullable.

namespace Microsoft.Dafny.LanguageServer.Workspace.Notifications {
  /// <summary>
  /// DTO used to communicate the current verification diagnostics to the LSP client.
  /// </summary>
  [Method(DafnyRequestNames.VerificationStatusGutter, Direction.ServerToClient)]
  public record VerificationStatusGutter(
    DocumentUri Uri,
    int? Version,
    LineVerificationStatus[] PerLineStatus
    ) : IRequest {

    public static VerificationStatusGutter ComputeFrom(
        DocumentUri uri,
        int? version,
        ICollection<VerificationTree> verificationTrees,
        Container<Diagnostic> resolutionErrors,
        int linesCount,
        bool verificationStarted) {
      var perLineStatus = RenderPerLineDiagnostics(uri, verificationTrees, linesCount, verificationStarted, resolutionErrors);
      return new VerificationStatusGutter(uri, version, perLineStatus);
    }

    public static LineVerificationStatus[] RenderPerLineDiagnostics(
      DocumentUri uri,
      ICollection<VerificationTree> verificationTrees,
      int numberOfLines,
      bool verificationStarted,
      Container<Diagnostic> parseAndResolutionErrors) {
      var result = new LineVerificationStatus[numberOfLines];

      if (verificationTrees.Count == 0 && !parseAndResolutionErrors.Any() && verificationStarted) {
        for (var line = 0; line < numberOfLines; line++) {
          result[line] = LineVerificationStatus.Verified;
        }

        return result;
      }

      var oneMemberHasOnly =
        verificationTrees.Any(tree => tree is TopLevelDeclMemberVerificationTree { MarkedAsOnly: true });

      // Render verification tree content into lines.
      foreach (var verificationTree in verificationTrees) {
        if (verificationTree.Uri == uri) {
          if (oneMemberHasOnly && verificationTree is TopLevelDeclMemberVerificationTree { MarkedAsOnly: false }) {
            verificationTree.StatusCurrent = CurrentStatus.Current;
            verificationTree.StatusVerification = GutterVerificationStatus.Skipped;
          }
          verificationTree.RenderInto(result);
        }
      }

      // Fill in the missing "Unknown" based on the surrounding content
      // The filling only takes Verified an Error
      var previousNotUnknown = LineVerificationStatus.Nothing;
      var lineDelta = 1;
      // Two passes so that we can fill gaps based on what happened before AND after
      for (var line = 0; 0 <= line; line += lineDelta) {
        if (line == numberOfLines) {
          lineDelta = -1;
          previousNotUnknown = LineVerificationStatus.Nothing;
          continue;
        }
        if (previousNotUnknown != LineVerificationStatus.Verified &&
            previousNotUnknown != LineVerificationStatus.VerifiedObsolete &&
            previousNotUnknown != LineVerificationStatus.VerifiedVerifying) {
          previousNotUnknown = LineVerificationStatus.Nothing;
        }
        if (result[line] == LineVerificationStatus.Nothing) {
          result[line] = previousNotUnknown;
        } else {
          previousNotUnknown = result[line];
        }
      }

      foreach (var diagnostic in parseAndResolutionErrors) {
        if (diagnostic.Range.Start.Line >= 0 && diagnostic.Range.Start.Line < result.Length) {
          result[diagnostic.Range.Start.Line] = LineVerificationStatus.ResolutionError;
        }
      }
      return result;
    }
  }

  public record TopLevelDeclMemberVerificationTree(
    string Kind,
    string DisplayName,
    // Used to re-trigger the verification of some diagnostics.
    string Identifier,
    string Filename,
    Uri Uri,
    // The range of this node.
    Range Range,
    Position Position,
    bool MarkedAsOnly
  ) : VerificationTree(Kind, DisplayName, Identifier, Filename, Uri, Range, Position) {
    // Recomputed from the children which are ImplementationVerificationTree
    public ImmutableDictionary<AssertionBatchIndex, AssertionBatchVerificationTree> AssertionBatches { get; private set; } =
      new Dictionary<AssertionBatchIndex, AssertionBatchVerificationTree>().ToImmutableDictionary();

    public override VerificationTree GetCopyForNotification() {
      return this with {
        Children = Children.Select(child => child.GetCopyForNotification()).ToList(),
        AssertionBatches = AssertionBatches
          .Select(keyValuePair =>
            (keyValuePair.Key, (AssertionBatchVerificationTree)keyValuePair.Value.GetCopyForNotification()))
          .ToImmutableDictionary(keyValuePair => keyValuePair.Item1, KeyValuePair => KeyValuePair.Item2)
      };
    }

    public void RecomputeAssertionBatchNodeDiagnostics() {
      var result = new Dictionary<AssertionBatchIndex, AssertionBatchVerificationTree>();
      var implementationNumber = 0;
      foreach (var implementationNode in Children.OfType<ImplementationVerificationTree>()) {
        implementationNumber++;
        foreach (var vcNum in implementationNode.AssertionBatchMetrics.Keys.OrderBy(x => x)) {
          var children = implementationNode.Children.OfType<AssertionVerificationTree>().Where(
            assertionNode => assertionNode.AssertionBatchNum == vcNum).Cast<VerificationTree>().ToList();
          var minPosition = children.Count > 0 ? children.MinBy(child => child.Position)!.Range.Start : Range.Start;
          var maxPosition = children.Count > 0 ? children.MaxBy(child => child.Range.End)!.Range.End : Range.Start;
          result[new AssertionBatchIndex(implementationNumber, vcNum)] = new AssertionBatchVerificationTree(
            $"Assertion batch #{result.Count + 1}",
            implementationNode.VerboseName,
            $"assertion-batch-{implementationNumber}-{vcNum}",
            Filename,
            Uri,
            new Range(minPosition, maxPosition)
          ) {
            Children = children,
            ResourceCount = implementationNode.AssertionBatchMetrics[vcNum].ResourceCount,
            CoveredIds = implementationNode.AssertionBatchMetrics[vcNum].CoveredIds,
            RelativeNumber = result.Count + 1,
          }.WithDuration(implementationNode.StartTime, implementationNode.AssertionBatchMetrics[vcNum].Time);
        }
      }

      AssertionBatches = result.ToImmutableDictionary();
    }

    public AssertionBatchVerificationTree? GetCostlierAssertionBatch() =>
      !AssertionBatches.Any() ? null :
      AssertionBatches.Values.MaxBy(assertionBatch => assertionBatch.ResourceCount);

    public List<int> AssertionBatchTimes =>
      AssertionBatches.Values.Select(assertionBatch => assertionBatch.TimeSpent).ToList();

    // Currently the best estimate of the number of assertion batches
    public int AssertionBatchCount =>
      AssertionBatches.Keys.GroupBy(key => key.ImplementationIndex).Select(group =>
        group.Select(key => key.RelativeIndex).Max()).Sum();

    public int LongestAssertionBatchTime => AssertionBatches.Any() ? AssertionBatchTimes.Max() : 0;

    public int LongestAssertionBatchTimeIndex => LongestAssertionBatchTime != 0 ? AssertionBatchTimes.IndexOf(LongestAssertionBatchTime) : -1;
  }

  // Invariant: There is at least 1 child for every assertion batch
  public record AssertionBatchVerificationTree(
    string DisplayName,
    string VerboseName,
    // Used to re-trigger the verification of some diagnostics.
    string Identifier,
    string Filename,
    Uri Uri,
    // The range of this node.
    Range Range
  ) : VerificationTree("Assertion Batch", DisplayName, Identifier, Filename, Uri, Range, Range.Start) {
    public int NumberOfAssertions => Children.Count;

    public AssertionBatchVerificationTree WithDuration(DateTime parentStartTime, int implementationNodeAssertionBatchTime) {
      Started = true;
      Finished = true;
      StartTime = parentStartTime;
      EndTime = parentStartTime.AddMilliseconds(implementationNodeAssertionBatchTime);
      return this;
    }
    public override VerificationTree GetCopyForNotification() {
      return this with {
        Children = Children.Select(child => child.GetCopyForNotification()).ToList()
      };
    }

    public int RelativeNumber { get; init; }
  }

  public record AssertionBatchMetrics(
    int Time,
    int ResourceCount,
    List<TrackedNodeComponent> CoveredIds
  );

  public record ImplementationVerificationTree(
    string DisplayName,
    string VerboseName,
    // Used to re-trigger the verification of some diagnostics.
    string Identifier,
    string Filename,
    Uri Uri,
    // The range of this node.
    Range Range,
    // The position as used by Boogie
    Position Position
  ) : VerificationTree("Implementation", DisplayName, Identifier, Filename, Uri, Range, Position) {
    // The index of ImplementationVerificationTree.AssertionBatchTimes
    // is the same as the AssertionVerificationTree.AssertionBatchIndex
    public ImmutableDictionary<int, AssertionBatchMetrics> AssertionBatchMetrics { get; private set; } =
      new Dictionary<int, AssertionBatchMetrics>().ToImmutableDictionary();
    private Dictionary<int, AssertionBatchMetrics> NewAssertionBatchMetrics { get; set; } =
      new Dictionary<int, AssertionBatchMetrics>();

    public override VerificationTree GetCopyForNotification() {
      return this with {
        Children = Children.Select(child => child.GetCopyForNotification()).ToList(),
        AssertionBatchMetrics = new Dictionary<int, AssertionBatchMetrics>(AssertionBatchMetrics).ToImmutableDictionary()
      };
    }

    private Implementation? implementation = null;

    public void AddAssertionBatchMetrics(int vcNum, int milliseconds, int resourceCount, List<TrackedNodeComponent> coveredIds) {
      NewAssertionBatchMetrics[vcNum] = new AssertionBatchMetrics(milliseconds, resourceCount, coveredIds);
    }

    public override bool Start() {
      if (base.Start()) {
        NewAssertionBatchMetrics = new Dictionary<int, AssertionBatchMetrics>();
        return true;
      }

      return false;
    }

    public override bool Stop() {
      if (base.Stop()) {
        AssertionBatchMetrics = NewAssertionBatchMetrics.ToImmutableDictionary();
        NewAssertionBatchMetrics = new Dictionary<int, AssertionBatchMetrics>();
        SaveNewChildren();
        return true;
      }

      return false;
    }

    public Implementation? GetImplementation() {
      return implementation;
    }

    public ImplementationVerificationTree WithImplementation(Implementation impl) {
      implementation = impl;
      return this;
    }
  };

  public record AssertionVerificationTree(
    string DisplayName,
    // Used to re-trigger the verification of some diagnostics.
    string Identifier,
    string Filename,
    Uri Uri,
    // Used to relocate a assertion verification tree and to determine which function is currently verifying
    Position? SecondaryPosition,
    // The range of this node.
    Range Range
  ) : VerificationTree("Assertion", DisplayName, Identifier, Filename, Uri, Range, Range.Start) {
    public AssertionVerificationTree WithDuration(DateTime parentStartTime, int batchTime) {
      Started = true;
      Finished = true;
      StartTime = parentStartTime;
      EndTime = parentStartTime.AddMilliseconds(batchTime);
      return this;
    }

    protected override bool IsFinalError => true;

    // Ranges that should also display an error
    // TODO: Will need to compute this statically for the tests
    public List<Range> ImmediatelyRelatedRanges { get; set; } = new();
    public List<Range> DynamicallyRelatedRanges { get; set; } = new();

    /// <summary>
    /// Which assertion batch this assertion was taken from in its implementation node
    /// </summary>
    public int AssertionBatchNum { get; init; }

    public AssertionVerificationTree
      WithAssertionAndCounterExample(AssertCmd? inAssertion, Counterexample? inCounterExample) {
      this.assertion = inAssertion;
      this.counterExample = inCounterExample;
      return WithImmediatelyRelatedChanges().WithDynamicallyRelatedChanges();
    }

    private AssertionVerificationTree WithImmediatelyRelatedChanges() {
      if (assertion == null) {
        ImmediatelyRelatedRanges = new();
        return this;
      }

      var tok = assertion.tok;
      var result = new List<Range>();
      while (tok is NestedToken nestedToken) {
        tok = nestedToken.Inner;
        if (tok.filename == assertion.tok.filename) {
          result.Add(tok.GetLspRange());
        }
      }

      if (counterExample is ReturnCounterexample returnCounterexample) {
        tok = returnCounterexample.FailingReturn.tok;
        if (tok.filename == assertion.tok.filename) {
          result.Add(returnCounterexample.FailingReturn.tok.GetLspRange());
        }
      }

      ImmediatelyRelatedRanges = result;
      return this;
    }

    private AssertionVerificationTree WithDynamicallyRelatedChanges() {
      // Ranges that should highlight when stepping on one error.
      if (assertion == null) {
        DynamicallyRelatedRanges = new();
        return this;
      }
      var result = new List<Range>();
      if (counterExample is CallCounterexample callCounterexample) {
        result.Add(callCounterexample.FailingRequires.tok.GetLspRange());
      }
      DynamicallyRelatedRanges = result;
      return this;
    }

    public override void RenderInto(LineVerificationStatus[] perLineDiagnostics, bool contextHasErrors = false,
      bool contextIsPending = false, Range? otherRange = null, Range? contextRange = null) {
      base.RenderInto(perLineDiagnostics, contextHasErrors, contextIsPending, otherRange, contextRange);
      if (StatusVerification == GutterVerificationStatus.Error) {
        foreach (var range in ImmediatelyRelatedRanges) {
          if (contextRange != null && contextRange.Contains(range)) {
            base.RenderInto(perLineDiagnostics, contextHasErrors, contextIsPending, range, contextRange);
          }
        }
      }
    }

    // Contains permanent secondary positions to this node (e.g. return branch positions)
    // Helps to distinguish between assertions with the same position (i.e. ensures for different branches)
    private AssertCmd? assertion;
    private Counterexample? counterExample;


    public AssertCmd? GetAssertion() {
      return assertion;
    }

    public AssertionVerificationTree WithAssertion(AssertCmd cmd) {
      assertion = cmd;
      return this;
    }


    public Counterexample? GetCounterExample() {
      return counterExample;
    }

    public AssertionVerificationTree WithCounterExample(Counterexample? c) {
      counterExample = c;
      return this;
    }
  }

  public record AssertionBatchIndex(int ImplementationIndex, int RelativeIndex);
}
