#nullable enable
using System;
using System.Collections.Generic;
using Microsoft.Boogie;
using Microsoft.Dafny;
using Microsoft.Dafny.LanguageServer.Language;
using Microsoft.Dafny.LanguageServer.Workspace.Notifications;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace DafnyCore.Compilations.GutterIcons;

#pragma warning disable CS8618

// Invariant: There is at least 1 child for every assertion batch
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