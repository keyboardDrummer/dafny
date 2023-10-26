using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.Boogie;
using Microsoft.Dafny.LanguageServer.Workspace.Notifications;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace DafnyCore.Compilations.GutterIcons;

#pragma warning disable CS8618
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