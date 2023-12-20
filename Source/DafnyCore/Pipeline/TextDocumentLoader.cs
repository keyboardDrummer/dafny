﻿#nullable enable
using Microsoft.Dafny.LanguageServer.Language;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Dafny.LanguageServer.Language.Symbols;
using Microsoft.Extensions.Logging;

namespace Microsoft.Dafny.LanguageServer.Workspace {
  /// <summary>
  /// Text document loader implementation that offloads the whole load procedure on one dedicated
  /// thread with a stack size of 256MB. Since only one thread is used, document loading is implicitely synchronized.
  /// The verification runs on the calling thread.
  /// </summary>
  /// <remarks>
  /// The increased stack size is necessary to solve the issue https://github.com/dafny-lang/dafny/issues/1447.
  /// </remarks>
  public class TextDocumentLoader : ITextDocumentLoader {
    private readonly ILogger<ITextDocumentLoader> logger;
    private readonly IDafnyParser parser;
    private readonly ISymbolResolver symbolResolver;

    public TextDocumentLoader(
      ILogger<ITextDocumentLoader> documentLoader,
      IDafnyParser parser,
      ISymbolResolver symbolResolver) {
      this.logger = documentLoader;
      this.parser = parser;
      this.symbolResolver = symbolResolver;
    }

    public async Task<Program> ParseAsync(Compilation compilation, CancellationToken cancellationToken) {
#pragma warning disable CS1998
      return await await DafnyMain.LargeStackFactory.StartNew(
        async () => parser.Parse(compilation, cancellationToken), cancellationToken
#pragma warning restore CS1998
      );
    }

    public async Task<ResolutionResult> ResolveAsync(CompilationInput input,
      Program program,
      CancellationToken cancellationToken) {
#pragma warning disable CS1998
      return await await DafnyMain.LargeStackFactory.StartNew(
        async () => ResolveInternal(input, program, cancellationToken), cancellationToken);
#pragma warning restore CS1998
    }

    private ResolutionResult ResolveInternal(CompilationInput input, Program program, CancellationToken cancellationToken) {

      var errorReporter = (ObservableErrorReporter)program.Reporter;
      if (errorReporter.HasErrors) {
        throw new TaskCanceledException();
      }

      symbolResolver.ResolveSymbols(input.Project, program, cancellationToken);

      List<ICanVerify>? verifiables;
      if (errorReporter.HasErrorsUntilResolver) {
        verifiables = null;
      } else {
        var symbols = SymbolExtensions.GetSymbolDescendants(program.DefaultModule);
        verifiables = symbols.OfType<ICanVerify>().Where(v => !AutoGeneratedToken.Is(v.RangeToken) &&
                                                              v.ContainingModule.ShouldVerify(program.Compilation) &&
                                                              v.ShouldVerify(program.Compilation) &&
                                                              v.ShouldVerify).ToList();
      }

      return new ResolutionResult(
        program,
        verifiables
      );
    }
  }
}
