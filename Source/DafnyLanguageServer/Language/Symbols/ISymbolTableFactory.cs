using System;
using System.Threading;
using DafnyCore.Compilations;
using Microsoft.Dafny.LanguageServer.Workspace;

namespace Microsoft.Dafny.LanguageServer.Language.Symbols {
  /// <summary>
  /// Factory definition to generate a symbol table out of a given dafny program and compilation unit.
  /// </summary>
  public interface ISymbolTableFactory {
    LegacySignatureAndCompletionTable CreateFrom(Uri uri, Program program, CancellationToken cancellationToken);
  }
}
