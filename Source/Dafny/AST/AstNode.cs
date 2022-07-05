using System;
using System.Collections.Generic;
using JetBrains.Annotations;
using Microsoft.Boogie;

namespace Microsoft.Dafny;

public abstract class AstNode {
  [CanBeNull] public AstNodeSyntax Syntax { get; }

  public abstract IEnumerable<AstNode> Children { get; }

  protected AstNode(AstNodeSyntax syntax) {
    Syntax = syntax;
  }
}

public record SyntaxFromTokens(IToken Start, IToken End) : AstNodeSyntax {
  public DfyRange Range => new DfyRange(new DfyPosition(Start.line, Start.col), new DfyPosition(End.line, End.col));
  public IReadOnlyList<string> Trivia => new string[] { }; // TODO traverse from Start.next to End and collect trivia.
  public Uri File => new Uri(Start.filename);
  public IToken ToBoogieToken => Start;
}

public record SyntaxFromToken(IToken Token) : AstNodeSyntax {
  public DfyRange Range => new DfyRange(new DfyPosition(Token.line, Token.col), new DfyPosition(Token.line, Token.col + Token.val.Length));
  public IReadOnlyList<string> Trivia => new string[] { }; // TODO use leading an trailing trivia
  public Uri File => new Uri(Token.filename);
  public IToken ToBoogieToken => Token;
}

public record FileRange(DfyRange Range, Uri File) {
  public FileRange Outer => this;
}

public record DfyRange(DfyPosition Start, DfyPosition End);

public interface AstNodeSyntax {
  DfyRange Range { get; }
  IReadOnlyList<string> Trivia { get; }

  Uri File { get; }

  FileRange FileRange => new FileRange(Range, File);

  Boogie.IToken ToBoogieToken { get; }
}

//public record Position(int Index);
public record DfyPosition(int Row, int Column);