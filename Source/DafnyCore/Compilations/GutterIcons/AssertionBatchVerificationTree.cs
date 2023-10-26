using System;
using System.Linq;
using Microsoft.Dafny.LanguageServer.Workspace.Notifications;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace DafnyCore.Compilations.GutterIcons;

#pragma warning disable CS8618
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