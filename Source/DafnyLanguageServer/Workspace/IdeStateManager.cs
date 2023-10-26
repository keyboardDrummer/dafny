using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using DafnyCore.Compilations;
using Microsoft.Boogie;
using Microsoft.Dafny.LanguageServer.Language;
using Microsoft.Dafny.LanguageServer.Language.Symbols;
using Microsoft.Dafny.LanguageServer.Workspace.Notifications;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Microsoft.Dafny.LanguageServer.Workspace;

public class IdeStateManager {
  private readonly IGhostStateDiagnosticCollector ghostStateDiagnosticCollector;
  private readonly ITelemetryPublisher telemetryPublisher;
  private readonly ISymbolTableFactory symbolTableFactory;

  public IdeStateManager(
    IGhostStateDiagnosticCollector ghostStateDiagnosticCollector,
    ITelemetryPublisher telemetryPublisher,
    ISymbolTableFactory symbolTableFactory) {
    this.ghostStateDiagnosticCollector = ghostStateDiagnosticCollector;
    this.telemetryPublisher = telemetryPublisher;
    this.symbolTableFactory = symbolTableFactory;
  }

  public IdeState InitialIdeState(Compilation initialCompilation) {
    var program = new EmptyNode();
    return UpdateIdeState(new IdeState(initialCompilation.Version, initialCompilation, program,
      ImmutableDictionary<Uri, IReadOnlyList<Diagnostic>>.Empty,
      SymbolTable.Empty(),
      LegacySignatureAndCompletionTable.Empty(initialCompilation.Options, initialCompilation.Project),
      ImmutableDictionary<Uri, Dictionary<Range, IdeVerificationResult>>.Empty,
      Array.Empty<Counterexample>(),
      ImmutableDictionary<Uri, IReadOnlyList<Range>>.Empty,
      initialCompilation.RootUris.ToDictionary(uri => uri, uri => new DocumentVerificationTree(program, uri))
    ), initialCompilation);
  }

  public IdeState UpdateIdeState(IdeState previousState, Compilation newCompilation) {
    var result = previousState with {
      Compilation = newCompilation
    };

    if (newCompilation is CompilationAfterParsing compilationAfterParsing) {

      // We may only use the new diagnostics if they block resolution,
      // otherwise we publish without resolution diagnostics which then appear again, which leads to flickering if the previous state already had these.
      // Since we currently do not separately track parse and resolution diagnostics,
      // we can not take the new parse and the existing resolution diagnostics, which would be ideal.
      var useNewDiagnostics = compilationAfterParsing.ResolutionDiagnostics.Values.Any(ds => ds.Any(d => d.Level == ErrorLevel.Error));
      result = result with {
        Program = compilationAfterParsing.Program,
        ResolutionDiagnostics = !useNewDiagnostics ? previousState.ResolutionDiagnostics : compilationAfterParsing.ResolutionDiagnostics.ToDictionary(
          kv => kv.Key,
          kv => (IReadOnlyList<Diagnostic>)kv.Value.Select(d => d.ToLspDiagnostic()).ToList()),
        VerificationTrees = compilationAfterParsing.VerificationTrees
      };
    }

    if (newCompilation is CompilationAfterResolution compilationAfterResolution) {
      // TODO do better than CancellationToken.None ?
      var cancellationToken = CancellationToken.None;
      
      // TODO Should IDE state have an original and migrated version, so we don't need previousState.SignatureAndCompletionTable.Migrated ?
      var computeLegacySymbolTableAndGhostRanges = true; // TODO optimise previousState.SignatureAndCompletionTable.Migrated;
      var legacyTable = computeLegacySymbolTableAndGhostRanges
        ? GetLegacyTable(compilationAfterResolution, cancellationToken)
        : previousState.SignatureAndCompletionTable;

      result = result with {
        SymbolTable = compilationAfterResolution.SymbolTable ?? previousState.SymbolTable,
        SignatureAndCompletionTable = legacyTable.Resolved ? legacyTable : previousState.SignatureAndCompletionTable,
        // TODO do we not have a PR where ghost ranges are computed without the legacy table? Let's merge that part.
        GhostRanges = computeLegacySymbolTableAndGhostRanges
          ? ghostStateDiagnosticCollector.GetGhostStateDiagnostics(legacyTable, cancellationToken)
          : previousState.GhostRanges,
        Counterexamples = new List<Counterexample>(compilationAfterResolution.Counterexamples),
        ResolutionDiagnostics = compilationAfterResolution.ResolutionDiagnostics.ToDictionary(
          kv => kv.Key,
          kv => (IReadOnlyList<Diagnostic>)kv.Value.Select(d => d.ToLspDiagnostic()).ToList()),
        VerificationTrees = compilationAfterResolution.VerificationTrees.ToDictionary(kv => kv.Key, kv => (DocumentVerificationTree)kv.Value.GetCopyForNotification()),
        VerificationResults = compilationAfterResolution.Verifiables == null ? previousState.VerificationResults : compilationAfterResolution.Verifiables.GroupBy(l => l.NameToken.Uri).ToImmutableDictionary(k => k.Key,
          k => k.GroupBy(l => l.NameToken.GetLspRange()).ToDictionary(
            l => l.Key,
            l => MergeResults(l.Select(canVerify => MergeVerifiable(compilationAfterResolution, previousState, canVerify)))))
      };
    }

    return result;
  }

  static IdeVerificationResult MergeVerifiable(CompilationAfterResolution compilationAfterResolution,
    IdeState previousState, ICanVerify canVerify) {
    var range = canVerify.NameToken.GetLspRange();
    var previousImplementations =
      previousState.GetVerificationResults(canVerify.NameToken.Uri).GetValueOrDefault(range)?.Implementations ??
      ImmutableDictionary<string, IdeImplementationView>.Empty;
    if (!compilationAfterResolution.ImplementationsPerVerifiable.TryGetValue(canVerify, out var implementationsPerName)) {
      var progress = compilationAfterResolution.VerifyingOrVerifiedSymbols.ContainsKey(canVerify)
        ? VerificationPreparationState.InProgress
        : VerificationPreparationState.NotStarted;
      return new IdeVerificationResult(PreparationProgress: progress,
        Implementations: previousImplementations.ToDictionary(kv => kv.Key, kv => kv.Value with {
          Status = PublishedVerificationStatus.Stale,
          Diagnostics = MarkDiagnosticsAsOutdated(kv.Value.Diagnostics).ToList()
        }));
    }

    var implementations = implementationsPerName!.ToDictionary(kv => kv.Key, kv => {
      var implementationView = kv.Value;
      var diagnostics = implementationView.Diagnostics.Select(d => d.ToLspDiagnostic());
      if (implementationView.Status < PublishedVerificationStatus.Error) {
        var previousDiagnostics = previousImplementations.GetValueOrDefault(kv.Key)?.Diagnostics;
        if (previousDiagnostics != null) {
          diagnostics = MarkDiagnosticsAsOutdated(previousDiagnostics);
        }
      }

      // If we're trying to verify this symbol, its status is at least queued.
      var status = implementationView.Status == PublishedVerificationStatus.Stale && compilationAfterResolution.VerifyingOrVerifiedSymbols.ContainsKey(canVerify)
        ? PublishedVerificationStatus.Queued : implementationView.Status;
      return new IdeImplementationView(implementationView.Task.Implementation.tok.GetLspRange(true),
        status, diagnostics.ToList(), implementationView.HitErrorLimit);
    });
    return new IdeVerificationResult(VerificationPreparationState.Done, implementations);

  }

  static VerificationPreparationState MergeStates(VerificationPreparationState a, VerificationPreparationState b) {
    return new[] { a, b }.Max();
  }

  static IdeVerificationResult MergeResults(IEnumerable<IdeVerificationResult> results) {
    return results.Aggregate((a, b) => new IdeVerificationResult(
      MergeStates(a.PreparationProgress, b.PreparationProgress),
      a.Implementations.ToImmutableDictionary().Merge(b.Implementations,
        (aView, bView) => new IdeImplementationView(
          aView.Range,
          Combine(aView.Status, bView.Status),
          aView.Diagnostics.Concat(bView.Diagnostics).ToList(), aView.HitErrorLimit || bView.HitErrorLimit))));
  }

  static PublishedVerificationStatus Combine(PublishedVerificationStatus first, PublishedVerificationStatus second) {
    return new[] { first, second }.Min();
  }

  public const string OutdatedPrefix = "Outdated: ";
  private static IEnumerable<Diagnostic> MarkDiagnosticsAsOutdated(IEnumerable<Diagnostic> diagnostics) {
    return diagnostics.Select(diagnostic => diagnostic with {
      Severity = diagnostic.Severity == DiagnosticSeverity.Error ? DiagnosticSeverity.Warning : diagnostic.Severity,
      Message = diagnostic.Message.StartsWith(OutdatedPrefix)
        ? diagnostic.Message
        : OutdatedPrefix + diagnostic.Message
    });
  }

  private LegacySignatureAndCompletionTable GetLegacyTable(
    CompilationAfterParsing compilation,
    CancellationToken cancellationToken) {
    if (compilation.Program.Reporter.HasErrorsUntilResolver) {
      // We cannot proceed without a successful resolution. Due to the contracts in dafny-lang, we cannot
      // access a property without potential contract violations. For example, a variable may have an
      // unresolved type represented by null. However, the contract prohibits the use of the type property
      // because it must not be null.
      return LegacySignatureAndCompletionTable.Empty(compilation.Options, compilation.Project);
    } else {
      var beforeLegacyServerResolution = DateTime.Now;
      var result = symbolTableFactory.CreateFrom(compilation.Project.Uri, compilation.Program, cancellationToken);
      telemetryPublisher.PublishTime("LegacyServerResolution", compilation.Project.Uri.ToString(), DateTime.Now - beforeLegacyServerResolution);
      return result;
    }
  }
}