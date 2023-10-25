using System;
using System.Linq;
using Microsoft.Dafny.LanguageServer.Language;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Microsoft.Dafny.LanguageServer.Workspace.Notifications;

#pragma warning disable CS8618
public record DocumentVerificationTree(
    INode Program,
    Uri Uri)
  : VerificationTree("Document", Uri.ToString(), Uri.ToString(), Uri.ToString(), Uri, ComputeRange(Program, Uri), new Position(0, 0)) {

  private static Range ComputeRange(INode node, Uri uri) {
    if (node is not Program program) {
      return new Range(0, 0, 0, 0);
    }
    var end = program.Files.FirstOrDefault(f => f.RangeToken.Uri == uri)?.EndToken ?? Token.NoToken;
    while (end.Next != null) {
      end = end.Next;
    }

    var endPosition = end.GetLspPosition();
    var endTriviaLines = end.TrailingTrivia.Split("\n");
    endPosition = new Position(endPosition.Line + endTriviaLines.Length - 1,
      endPosition.Character + endTriviaLines[^1].Length);

    return new Range(new Position(0, 0), endPosition);
  }
}