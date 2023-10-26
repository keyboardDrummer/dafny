#nullable enable
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Reactive;
using System.Threading.Tasks;
using DafnyCore.Compilations;
using Microsoft.Boogie;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Microsoft.Dafny.LanguageServer.Workspace;

public class CompilationAfterResolution : CompilationAfterParsing {

  public CompilationAfterResolution(CompilationAfterParsing compilationAfterParsing,
    IReadOnlyDictionary<Uri, List<DafnyDiagnostic>> diagnostics,
    SymbolTable? symbolTable,
    IReadOnlyDictionary<Uri, IReadOnlyList<Range>> ghostDiagnostics,
    IReadOnlyList<ICanVerify>? verifiables,
    LazyConcurrentDictionary<ModuleDefinition, Task<IReadOnlyDictionary<FilePosition, IReadOnlyList<IImplementationTask>>>> translatedModules,
    List<Counterexample> counterexamples
    ) :
    base(compilationAfterParsing, compilationAfterParsing.Program, diagnostics, compilationAfterParsing.VerificationTrees) {
    SymbolTable = symbolTable;
    GhostDiagnostics = ghostDiagnostics;
    Verifiables = verifiables;
    TranslatedModules = translatedModules;
    Counterexamples = counterexamples;
  }
  public List<Counterexample> Counterexamples { get; set; }
  public SymbolTable? SymbolTable { get; }
  public IReadOnlyDictionary<Uri, IReadOnlyList<Range>> GhostDiagnostics { get; }
  public IReadOnlyList<ICanVerify>? Verifiables { get; }
  public ConcurrentDictionary<ICanVerify, Unit> VerifyingOrVerifiedSymbols { get; } = new();
  public LazyConcurrentDictionary<ICanVerify, Dictionary<string, ImplementationState>> ImplementationsPerVerifiable { get; } = new();

  /// <summary>
  /// FilePosition is required because the default module lives in multiple files
  /// </summary>
  public LazyConcurrentDictionary<ModuleDefinition, Task<IReadOnlyDictionary<FilePosition, IReadOnlyList<IImplementationTask>>>> TranslatedModules { get; }

  public override IEnumerable<DafnyDiagnostic> GetDiagnostics(Uri uri) {
    var implementationsForUri = ImplementationsPerVerifiable.
      Where(kv => kv.Key.Tok.Uri == uri).
      Select(kv => kv.Value).ToList();
    var verificationDiagnostics = implementationsForUri.SelectMany(view =>
      view.Values.SelectMany(v => v.Diagnostics));
    return base.GetDiagnostics(uri).Concat(verificationDiagnostics);
  }


  public void RefreshDiagnosticsFromProgramReporter() {
    ResolutionDiagnostics =
      ((DiagnosticErrorReporter)Program.Reporter).AllDiagnosticsCopy;
  }
}
