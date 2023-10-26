using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Boogie;
using Microsoft.Dafny;
using Microsoft.Dafny.Compilers;
using Microsoft.Dafny.LanguageServer.Workspace;
using Microsoft.Dafny.LanguageServer.Workspace.Notifications;
using Microsoft.Extensions.Logging;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Program = Microsoft.Dafny.Program;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace DafnyCore.Compilations {
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
    private readonly IGhostStateDiagnosticCollector ghostStateDiagnosticCollector;

    protected TextDocumentLoader(
      ILogger<ITextDocumentLoader> documentLoader,
      IDafnyParser parser,
      ISymbolResolver symbolResolver,
      IGhostStateDiagnosticCollector ghostStateDiagnosticCollector) {
      this.logger = documentLoader;
      this.parser = parser;
      this.symbolResolver = symbolResolver;
      this.symbolTableFactory = symbolTableFactory;
      this.ghostStateDiagnosticCollector = ghostStateDiagnosticCollector;
    }

    public static TextDocumentLoader Create(
      IDafnyParser parser,
      IGhostStateDiagnosticCollector ghostStateDiagnosticCollector,
      ILogger<ITextDocumentLoader> logger
      ) {
      return new TextDocumentLoader(logger, parser, ghostStateDiagnosticCollector);
    }

    public async Task<CompilationAfterParsing> ParseAsync(DafnyOptions options, Compilation compilation,
      IReadOnlyDictionary<Uri, DocumentVerificationTree> migratedVerificationTrees, CancellationToken cancellationToken) {
#pragma warning disable CS1998
      return await await DafnyMain.LargeStackFactory.StartNew(
        async () => ParseInternal(options, compilation, migratedVerificationTrees, cancellationToken), cancellationToken
#pragma warning restore CS1998
      );
    }

    private CompilationAfterParsing ParseInternal(DafnyOptions options, Compilation compilation,
      IReadOnlyDictionary<Uri, DocumentVerificationTree> migratedVerificationTrees,
      CancellationToken cancellationToken) {
      var project = compilation.Project;
      var errorReporter = new DiagnosticErrorReporter(options, project.Uri);
      var program = parser.Parse(compilation, errorReporter, cancellationToken);
      compilation.Project.Errors.CopyDiagnostics(program.Reporter);
      var projectPath = compilation.Project.Uri.LocalPath;
      if (projectPath.EndsWith(DafnyProject.FileName)) {
        var projectDirectory = Path.GetDirectoryName(projectPath)!;
        var filesMessage = string.Join("\n", compilation.RootUris.Select(uri => Path.GetRelativePath(projectDirectory, uri.LocalPath)));
        if (filesMessage.Any()) {
          program.Reporter.Info(MessageSource.Parser, compilation.Project.StartingToken, "Files referenced by project are:" + Environment.NewLine + filesMessage);
        } else {
          program.Reporter.Warning(MessageSource.Parser, CompilerErrors.ErrorId.None, compilation.Project.StartingToken, "Project references no files");
        }
      }
      var compilationAfterParsing = new CompilationAfterParsing(compilation, program, errorReporter.AllDiagnosticsCopy,
        compilation.RootUris.ToDictionary(uri => uri,
          uri => migratedVerificationTrees.GetValueOrDefault(uri) ?? new DocumentVerificationTree(program, uri)));

      return compilationAfterParsing;
    }

    public async Task<CompilationAfterResolution> ResolveAsync(DafnyOptions options,
      CompilationAfterParsing compilation,
      CancellationToken cancellationToken) {
#pragma warning disable CS1998
      return await await DafnyMain.LargeStackFactory.StartNew(
        async () => ResolveInternal(compilation, cancellationToken), cancellationToken);
#pragma warning restore CS1998
    }

    private CompilationAfterResolution ResolveInternal(CompilationAfterParsing compilation, CancellationToken cancellationToken) {

      var program = compilation.Program;
      var errorReporter = (DiagnosticErrorReporter)program.Reporter;
      if (errorReporter.HasErrors) {
        throw new TaskCanceledException();
      }

      var cloner = new Cloner(true, false);
      var programClone = new Program(cloner, program);

      var project = compilation.Project;

      var compilationUnit = symbolResolver.ResolveSymbols(project, programClone, cancellationToken);
      var legacySymbolTable = symbolTableFactory.CreateFrom(compilationUnit, cancellationToken);

      var newSymbolTable = errorReporter.HasErrors
        ? null
        : CreateFrom(programClone, compilation, cancellationToken);

      var ghostDiagnostics = ghostStateDiagnosticCollector.GetGhostStateDiagnostics(legacySymbolTable, cancellationToken);

      List<ICanVerify>? verifiables;
      if (errorReporter.HasErrorsUntilResolver) {
        verifiables = null;
      } else {
        var symbols = SymbolExtensions.GetSymbolDescendants(programClone.DefaultModule);
        verifiables = symbols.OfType<ICanVerify>().Where(v => !AutoGeneratedToken.Is(v.RangeToken) &&
                                                              v.ContainingModule.ShouldVerify(program.Compilation) &&
                                                              v.ShouldVerify(programClone.Compilation) &&
                                                              v.ShouldVerify).ToList();
      }

      var compilationAfterParsingWithProgramClone = new CompilationAfterParsing(compilation, programClone, compilation.ResolutionDiagnostics, compilation.VerificationTrees);
      return new CompilationAfterResolution(compilationAfterParsingWithProgramClone,
        errorReporter.AllDiagnosticsCopy,
        newSymbolTable,
        legacySymbolTable,
        ghostDiagnostics,
        verifiables,
        new(),
        new()
      );
    }
    
    public SymbolTable CreateFrom(Program program, Compilation compilation, CancellationToken cancellationToken) {
      var visited = program.Visit(a => true, b => { });

      var usages = visited.OfType<IHasUsages>().Where(v => !AutoGeneratedToken.Is(v.NameToken))
        .SelectMany(r => r.GetResolvedDeclarations().Select(declaration =>
          ((IDeclarationOrUsage)r, declaration))).ToList();

      var relevantDafnySymbolKinds = new HashSet<DafnySymbolKind> {
        DafnySymbolKind.Function,
        DafnySymbolKind.Class,
        DafnySymbolKind.Enum,
        DafnySymbolKind.Method,
        DafnySymbolKind.EnumMember,
        DafnySymbolKind.Struct,
        DafnySymbolKind.Interface,
        DafnySymbolKind.Namespace,
      };
      // Since these definitions are checked for whether they
      // contain substrings when answering workspace/resolve queries,
      // performance can be improved by storing their names in a
      // data structure that makes this operation cheaper, such as
      // a suffix tree.
      var definitions = visited
        .OfType<ISymbol>()
        .Where(symbol => relevantDafnySymbolKinds.Contains(symbol.Kind))
        .ToImmutableList();

      return new SymbolTable(usages, definitions);
    }
  }
}
