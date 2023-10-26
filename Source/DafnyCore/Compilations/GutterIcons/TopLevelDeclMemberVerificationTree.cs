using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.Dafny.LanguageServer.Workspace.Notifications;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace DafnyCore.Compilations.GutterIcons;

#pragma warning disable CS8618
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
      foreach (var vcNum in Enumerable.OrderBy<int, int>(implementationNode.AssertionBatchMetrics.Keys, x => x)) {
        var children = Enumerable.Cast<VerificationTree>(implementationNode.Children.OfType<AssertionVerificationTree>().Where(
          assertionNode => assertionNode.AssertionBatchNum == vcNum)).ToList();
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
    Enumerable.ToList<int>(AssertionBatches.Values.Select(assertionBatch => assertionBatch.TimeSpent));

  // Currently the best estimate of the number of assertion batches
  public int AssertionBatchCount =>
    Enumerable.Sum((IEnumerable<int>)AssertionBatches.Keys.GroupBy(key => key.ImplementationIndex).Select(group =>
      Enumerable.Max((IEnumerable<int>)group.Select(key => key.RelativeIndex))));

  public int LongestAssertionBatchTime => AssertionBatches.Any() ? AssertionBatchTimes.Max() : 0;

  public int LongestAssertionBatchTimeIndex => LongestAssertionBatchTime != 0 ? AssertionBatchTimes.IndexOf(LongestAssertionBatchTime) : -1;
}