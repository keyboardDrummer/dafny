using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.Boogie;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Microsoft.Dafny.LanguageServer.Workspace.Notifications;

#pragma warning disable CS8618
/// <summary>
/// A verification tree is an abstraction over the code to represent the verification
/// status of a region of the document, useful for IDE verification inspection.
/// A verification tree can contain other child trees.
/// It can currently be rendered linearly, e.g. for gutter display, or used as a tree in a test-like display. 
/// The verification status consists of two orthogonal concepts:
/// - StatusVerification: Nothing (initial), Error, Verified, or Inconclusive
/// - StatusCurrent: Current (Up-to-date), Obsolete (outdated), and Verifying (as notified by the verifier)
///
/// The difference between "Range" and "Position" is that "Range" contains two positions that include the entire tree,
/// whereas "Position" is a single position that uniquely determines the range, e.g. a symbol position.
/// That position typically serves as an placeholder to uniquely determine a method.
///  
/// For example:
/// 
///     method Test() {}
///     ^Range.Start   ^Range.End
///            ^ Position 
/// </summary>
/// <param name="DisplayName">A user-facing name of this node, to be displayed in an IDE explorer</param>
/// <param name="Identifier">A unique identifier, to be used by the IDE to request re-verification</param>
/// <param name="Filename">The name of the file this region of the document is contained in</param>
/// <param name="Range">The range of this region of the document</param>
/// <param name="Position">The position that uniquely identify this range</param>
public record VerificationTree(
  // Method, Function, Subset type, Constant, Document, Assertion...
  string Kind,
  // User-facing short name
  string DisplayName,
  // Used to re-trigger the verification of some diagnostics.
  string Identifier,
  string Filename,
  Uri Uri,
  // The start and end of this verification tree
  Range Range,
  // The position of the symbol name attached to this node, or Range.Start if it's anonymous
  Position Position
) {
  public string PrefixedDisplayName => Kind + " `" + DisplayName + "`";

  // Overriden by checking children if there are some
  public GutterVerificationStatus StatusVerification { get; set; } = GutterVerificationStatus.Nothing;

  // Overriden by checking children if there are some
  public CurrentStatus StatusCurrent { get; set; } = CurrentStatus.Obsolete;

  /// Time and Resource diagnostics
  public bool Started { get; set; } = false;
  public bool Finished { get; set; } = false;
  public DateTime StartTime { get; protected set; }
  public DateTime EndTime { get; protected set; }
  public int TimeSpent => (int)(Finished ? ((TimeSpan)(EndTime - StartTime)).TotalMilliseconds : Started ? (DateTime.Now - StartTime).TotalMilliseconds : 0);
  // Resources allocated at the end of the computation.
  public long ResourceCount { get; set; } = 0;

  public List<TrackedNodeComponent> CoveredIds { get; set; } = new();

  // Sub-diagnostics if any
  public List<VerificationTree> Children { get; set; } = new();
  public List<VerificationTree> NewChildren { get; set; } = new();

  public void Visit(Action<VerificationTree> action) {
    action(this);
    foreach (var child in Children) {
      child.Visit(action);
    }
  }

  public int GetNewChildrenCount() {
    return NewChildren.Count;
  }

  public void AddNewChild(VerificationTree newChild) {
    NewChildren.Add(newChild);
  }

  public void SaveNewChildren() {
    Children = NewChildren;
    ResetNewChildren();
  }

  public void ResetNewChildren() {
    NewChildren = new();
  }

  public VerificationTree SetObsolete() {
    if (StatusCurrent != CurrentStatus.Obsolete) {
      StatusCurrent = CurrentStatus.Obsolete;
      foreach (var child in Children) {
        child.SetObsolete();
      }
    }

    return this;
  }

  // Returns true if it started the method, false if it was already started
  public virtual bool Start() {
    if (StatusCurrent != CurrentStatus.Verifying || !Started) {
      StartTime = DateTime.Now;
      StatusCurrent = CurrentStatus.Verifying;
      foreach (var child in Children) {
        child.Start();
      }
      Started = true;
      return true;
    }

    return false;
  }

  // Returns true if it did stop the current node, false if it was already stopped
  public virtual bool Stop() {
    if (StatusCurrent != CurrentStatus.Current || !Finished) {
      EndTime = DateTime.Now;
      StatusCurrent = CurrentStatus.Current;
      foreach (var child in Children) {
        child.Stop();
      }
      Finished = true;
      return true;
    }

    return false;
  }

  public void PropagateChildrenErrorsUp() {
    var childrenHaveErrors = false;
    foreach (var child in Children) {
      child.PropagateChildrenErrorsUp();
      if (child.StatusVerification == GutterVerificationStatus.Error) {
        childrenHaveErrors = true;
      }
    }

    if (childrenHaveErrors) {
      StatusVerification = GutterVerificationStatus.Error;
    }
  }

  public static LineVerificationStatus RenderLineVerificationStatus(
    bool isFinalError, bool contextHasErrors, bool contextIsPending,
    CurrentStatus currentStatus, GutterVerificationStatus verificationStatus) {
    LineVerificationStatus simpleStatus = verificationStatus switch {
      GutterVerificationStatus.Skipped => LineVerificationStatus.Skipped, // Always current
      GutterVerificationStatus.Nothing => LineVerificationStatus.Nothing,
      // let's be careful to no display "Verified" for a range if the context does not have errors and is pending
      // because there might be other errors on the same range.
      GutterVerificationStatus.Verified =>
        contextHasErrors
          ? isFinalError // Sub-implementations that are verified do not count
            ? LineVerificationStatus.AssertionVerifiedInErrorContext
            : LineVerificationStatus.ErrorContext
          : contextIsPending && !isFinalError
            ? LineVerificationStatus.Nothing
            : LineVerificationStatus.Verified,
      // We don't display inconclusive on the gutter (user should focus on errors),
      // We display an error range instead
      GutterVerificationStatus.Inconclusive => isFinalError
        ? LineVerificationStatus.AssertionFailed
        : LineVerificationStatus.ErrorContext,
      GutterVerificationStatus.Error => isFinalError
        ? LineVerificationStatus.AssertionFailed
        : LineVerificationStatus.ErrorContext,
      _ => throw new ArgumentOutOfRangeException()
    };
    return
      simpleStatus == LineVerificationStatus.Skipped ? simpleStatus :
        (LineVerificationStatus)((int)simpleStatus + (int)currentStatus);
  }

  protected virtual bool IsFinalError => false;

  // Requires PropagateChildrenErrorsUp to have been called before.
  public virtual void RenderInto(LineVerificationStatus[] perLineDiagnostics, bool contextHasErrors = false, bool contextIsPending = false, Range? otherRange = null, Range? contextRange = null) {
    Range range = otherRange ?? Range;
    var isFinalError = range.Start.Line == range.End.Line || IsFinalError;
    LineVerificationStatus targetStatus = RenderLineVerificationStatus(isFinalError, contextHasErrors, contextIsPending, StatusCurrent, StatusVerification);
    for (var line = range.Start.Line; line <= range.End.Line; line++) {
      if (line < 0 || perLineDiagnostics.Length <= line) {
        // An error occurred? We don't want null pointer exceptions anyway
        continue;
      }
      if ((int)perLineDiagnostics[line] < (int)(targetStatus)) {
        perLineDiagnostics[line] = targetStatus;
      }
    }
    foreach (var child in Children) {
      child.RenderInto(perLineDiagnostics,
        contextHasErrors || StatusVerification == GutterVerificationStatus.Error,
        contextIsPending ||
        StatusCurrent == CurrentStatus.Obsolete ||
        StatusCurrent == CurrentStatus.Verifying,
        null,
        Range);
    }
    // Ensure that if this is an ImplementationVerificationTree, and children "painted" verified,
    // and this node is still pending
    // at least the first line should show pending.
    if (range.Start.Line >= 0 && range.End.Line < perLineDiagnostics.Length) {
      if (StatusCurrent == CurrentStatus.Verifying &&
          perLineDiagnostics.ToList().GetRange(range.Start.Line, range.End.Line - range.Start.Line + 1).All(
            line => line == LineVerificationStatus.Verified)) {
        perLineDiagnostics[range.Start.Line] = targetStatus;
      }
    }
  }

  // If the verification never starts on this node, it means there is nothing to verify about it.
  // Returns true if a status was updated
  public bool SetVerifiedIfPending() {
    if (StatusCurrent == CurrentStatus.Obsolete) {
      StatusCurrent = CurrentStatus.Current;
      StatusVerification = GutterVerificationStatus.Verified;
      foreach (var child in Children) {
        child.SetVerifiedIfPending();
      }
      return true;
    }

    return false;
  }

  public virtual VerificationTree GetCopyForNotification() {
    return this with {
      Children = Children.Select(child => child.GetCopyForNotification()).ToList()
    };
  }
}

public enum GutterVerificationStatus {
  Nothing = 0,
  Verified = 200,
  Skipped = 250,
  Inconclusive = 270,
  Error = 400
}

public enum CurrentStatus {
  Current = 0,
  Obsolete = 1,
  Verifying = 2
}

public enum LineVerificationStatus {
  // Default value for every line, before the renderer figures it out.
  Nothing = 0,
  // For first-time computation not actively computing but soon. Synonym of "obsolete"
  // (scheduledComputation)
  Scheduled = 1,
  // For first-time computations, actively computing
  Verifying = 2,
  VerifiedObsolete = 201,
  VerifiedVerifying = 202,
  // Also applicable for empty spaces if they are not surrounded by errors.
  Verified = 200,
  Skipped = 250,
  // For trees containing children with errors (e.g. functions, methods, fields, subset types)
  ErrorContextObsolete = 301,
  ErrorContextVerifying = 302,
  ErrorContext = 300,
  // For individual assertions in error contexts
  AssertionVerifiedInErrorContextObsolete = 351,
  AssertionVerifiedInErrorContextVerifying = 352,
  AssertionVerifiedInErrorContext = 350,
  // For specific lines which have errors on it. They take over verified assertions
  AssertionFailedObsolete = 401,
  AssertionFailedVerifying = 402,
  AssertionFailed = 400,
  // For lines containing resolution or parse errors
  ResolutionError = 500
}

public record AssertionBatchIndex(int ImplementationIndex, int RelativeIndex);

public record AssertionBatchMetrics(
  int Time,
  int ResourceCount,
  List<TrackedNodeComponent> CoveredIds
);