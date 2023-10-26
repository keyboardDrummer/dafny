using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Dafny;
using Microsoft.Dafny.LanguageServer.Workspace;
using Microsoft.Dafny.LanguageServer.Workspace.Notifications;

namespace DafnyCore.Compilations {
  /// <summary>
  /// Implementations are responsible to load a specified language server document and generate
  /// a dafny document out of it.
  /// </summary>
  public interface ITextDocumentLoader {


    /// <summary>
    /// Creates a dafny document from the given text document without loading it.
    /// </summary>
    /// <param name="compilation"></param>
    /// <returns>The unloaded dafny document.</returns>
    /// <exception cref="System.OperationCanceledException">Thrown when the cancellation was requested before completion.</exception>
    /// <exception cref="System.ObjectDisposedException">Thrown if the cancellation token was disposed before the completion.</exception>
    // IdeState CreateUnloaded(Compilation compilation);


    // public IdeState CreateUnloaded(Compilation compilation) {
    //   return CreateDocumentWithEmptySymbolTable(compilation, ImmutableDictionary<Uri, IReadOnlyList<Diagnostic>>.Empty);
    // }

    // private IdeState CreateDocumentWithEmptySymbolTable(Compilation compilation,
    //   IReadOnlyDictionary<Uri, IReadOnlyList<Diagnostic>> resolutionDiagnostics) {
    //   var dafnyOptions = DafnyOptions.Default;
    //   var program = new EmptyNode();
    //   return new IdeState(
    //     compilation.Version,
    //     compilation,
    //     program,
    //     resolutionDiagnostics,
    //     SymbolTable.Empty(),
    //     LegacySignatureAndCompletionTable.Empty(dafnyOptions, compilation.Project),
    //     ImmutableDictionary<Uri, Dictionary<Range, IdeVerificationResult>>.Empty,
    //     Array.Empty<Counterexample>(),
    //     ImmutableDictionary<Uri, IReadOnlyList<Range>>.Empty,
    //     ImmutableDictionary<Uri, DocumentVerificationTree>.Empty
    //   );
    // }

    Task<CompilationAfterParsing> ParseAsync(DafnyOptions options, Compilation compilation,
      IReadOnlyDictionary<Uri, DocumentVerificationTree> migratedVerificationTrees, CancellationToken cancellationToken);

    Task<CompilationAfterResolution> ResolveAsync(DafnyOptions options, CompilationAfterParsing compilation,
      CancellationToken cancellationToken);
  }
}
