using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection.Metadata;
using DafnyCore.Compilations.GutterIcons;
using DafnyCore.Verifier;
using MediatR;
using OmniSharp.Extensions.JsonRpc;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

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
}
