using System;
using System.Collections.Generic;
using System.Diagnostics;
using JetBrains.Annotations;
using Microsoft.Boogie;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Microsoft.Dafny;

public abstract class AstNode {
  [CanBeNull] public AstNodeSyntax Syntax { get; }

  public abstract IEnumerable<AstNode> Children { get; }

  protected AstNode(AstNodeSyntax syntax) {
    Syntax = syntax;
  }
}

public record SyntaxFromTokens(IToken Start, IToken End) : AstNodeSyntax {
  public Range Range => new Range(new Position(Start.pos), new Position(End.pos));
  public IReadOnlyList<string> Trivia => new string[] { }; // TODO traverse from Start.next to End and collect trivia.
  public Uri File => new Uri(Start.filename);
  public IToken ToBoogieToken => Start;
}

public record SyntaxFromToken(IToken Token) : AstNodeSyntax {
  public Range Range => new Range(new Position(Token.pos), new Position(Token.pos + Token.val.Length));
  public IReadOnlyList<string> Trivia => new string[] { }; // TODO use leading an trailing trivia
  public Uri File => new Uri(Token.filename);
  public IToken ToBoogieToken => Token;
}

public record FileRange(Range Range, Uri File);

public record Range(Position Start, Position End);

public interface AstNodeSyntax {
  Range Range { get; }
  IReadOnlyList<string> Trivia { get; }

  Uri File { get; }

  FileRange FileRange => new FileRange(Range, File);

  Boogie.IToken ToBoogieToken { get; }
}

public record Position(int Index);