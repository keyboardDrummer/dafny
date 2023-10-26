using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.CommandLine;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Reactive.Disposables;
using System.Reactive.Linq;
using System.Threading;
using System.Threading.Tasks;
using DafnyCore.Compilations;
using IntervalTree;
using Microsoft.Boogie;
using Microsoft.Dafny.LanguageServer.Language;
using Microsoft.Dafny.LanguageServer.Language.Symbols;
using Microsoft.Dafny.LanguageServer.Workspace.ChangeProcessors;
using Microsoft.Dafny.LanguageServer.Workspace.Notifications;
using Microsoft.Extensions.Logging;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Microsoft.Dafny.LanguageServer.Workspace;

public delegate ProjectManager CreateProjectManager(
  CustomStackSizePoolTaskScheduler scheduler,
  VerificationResultCache verificationCache,
  DafnyProject project);

/// <summary>
/// Handles operation on a single document.
/// Handles migration of previously published document state
/// </summary>
public class ProjectManager : IDisposable {

  public const int DefaultThrottleTime = 100;
  public static readonly Option<int> UpdateThrottling = new("--update-throttling", () => DefaultThrottleTime,
    @"How many milliseconds the server will wait before sending new document updates to the client. Higher values reduce bandwidth at the cost of responsiveness".TrimStart()) {
    IsHidden = true
  };

  public static readonly Option<VerifyOnMode> Verification = new("--verify-on", () => VerifyOnMode.Change, @"
(experimental)
Determine when to automatically verify the program. Choose from: Never, OnChange (verify everything in a file when changing the file), OnChangeProject or OnSave.".TrimStart()) {
    ArgumentHelpName = "event"
  };

  private readonly CreateMigrator createMigrator;
  public DafnyProject Project { get; }

  private readonly IdeStateObserver observer;
  public CompilationManager CompilationManager { get; private set; }
  private IDisposable observerSubscription;
  private readonly ITelemetryPublisher telemetryPublisher;
  private readonly INotificationPublisher notificationPublisher;
  private readonly IGutterIconAndHoverVerificationDetailsManager gutterIconManager;
  private ISymbolTableFactory symbolTableFactory;
  private readonly IGhostStateDiagnosticCollector ghostStateDiagnosticCollector;
  private readonly ILogger<ProjectManager> logger;

  /// <summary>
  /// The version of this project.
  /// Is incremented when any file in the project is updated.
  /// Is used as part of project-wide notifications.
  /// Can be used by the client to ignore outdated notifications
  /// </summary>
  private int version;

  private int openFileCount;

  private VerifyOnMode AutomaticVerificationMode => options.Get(Verification);

  private bool VerifyOnSave => options.Get(Verification) == VerifyOnMode.Save;
  public List<Location> RecentChanges { get; set; } = new();

  private readonly DafnyOptions options;
  private readonly DafnyOptions serverOptions;
  private readonly CreateCompilationManager createCompilationManager;
  private readonly ExecutionEngine boogieEngine;
  private readonly IFileSystem fileSystem;
  private Lazy<IdeState> latestIdeState;

  public ProjectManager(
    DafnyOptions serverOptions,
    ILogger<ProjectManager> logger,
    CreateMigrator createMigrator,
    IFileSystem fileSystem,
    INotificationPublisher notificationPublisher,
    IGutterIconAndHoverVerificationDetailsManager gutterIconManager,
    IGhostStateDiagnosticCollector ghostStateDiagnosticCollector,
    ITelemetryPublisher telemetryPublisher, 
    ISymbolTableFactory symbolTableFactory,
    CreateCompilationManager createCompilationManager,
    CreateIdeStateObserver createIdeStateObserver,
    CustomStackSizePoolTaskScheduler scheduler,
    VerificationResultCache cache,
    DafnyProject project) 
  {
    Project = project;
    this.ghostStateDiagnosticCollector = ghostStateDiagnosticCollector;
    this.telemetryPublisher = telemetryPublisher;
    this.symbolTableFactory = symbolTableFactory;
    this.gutterIconManager = gutterIconManager;
    this.notificationPublisher = notificationPublisher;
    this.serverOptions = serverOptions;
    this.fileSystem = fileSystem;
    this.createCompilationManager = createCompilationManager;
    this.createMigrator = createMigrator;
    this.logger = logger;

    options = DetermineProjectOptions(project, serverOptions);
    options.Printer = new OutputLogger(logger);
    this.boogieEngine = new ExecutionEngine(options, cache, scheduler);
    var initialCompilation = CreateInitialCompilation();
    var initialIdeState = InitialIdeState(initialCompilation);
    latestIdeState = new Lazy<IdeState>(initialIdeState);

    observer = createIdeStateObserver(initialIdeState);
    CompilationManager = createCompilationManager(
        options, boogieEngine, initialCompilation, ImmutableDictionary<Uri, DocumentVerificationTree>.Empty
    );

    observerSubscription = Disposable.Empty;
  }

  private Compilation CreateInitialCompilation() {
    var rootUris = Project.GetRootSourceUris(fileSystem).Concat(options.CliRootSourceUris).ToList();
    return new Compilation(options, version, Project, rootUris);
  }

  private const int MaxRememberedChanges = 100;
  private const int MaxRememberedChangedVerifiables = 5;

  public void UpdateDocument(DidChangeTextDocumentParams documentChange) {
    var migrator = createMigrator(documentChange, CancellationToken.None);
    Lazy<IdeState> lazyPreviousCompilationLastIdeState = latestIdeState;
    var upcomingVersion = version + 1;
    latestIdeState = new Lazy<IdeState>(() => {
      // If we migrate the observer before accessing latestIdeState, we can be sure it's migrated before it receives new events.
      observer.Migrate(migrator, upcomingVersion);
      return lazyPreviousCompilationLastIdeState.Value.Migrate(migrator, upcomingVersion);
    });
    StartNewCompilation();

    lock (RecentChanges) {
      var newChanges = documentChange.ContentChanges.Where(c => c.Range != null).
        Select(contentChange => new Location {
          Range = contentChange.Range!,
          Uri = documentChange.TextDocument.Uri
        });
      var migratedChanges = RecentChanges.Select(location => {
        if (location.Uri != documentChange.TextDocument.Uri) {
          return location;
        }

        var newRange = migrator.MigrateRange(location.Range);
        if (newRange == null) {
          return null;
        }
        return new Location {
          Range = newRange,
          Uri = location.Uri
        };
      }).Where(r => r != null);
      RecentChanges = newChanges.Concat(migratedChanges).Take(MaxRememberedChanges).ToList()!;
    }
    TriggerVerificationForFile(documentChange.TextDocument.Uri.ToUri());
  }

  private void StartNewCompilation() {
    var compilationVersion = ++version;
    logger.LogDebug("Clearing result for workCompletedForCurrentVersion");

    Lazy<IdeState> migratedLazyPreviousCompilationLastIdeState = latestIdeState;
    observerSubscription.Dispose();

    CompilationManager.Dispose();
    CompilationManager = createCompilationManager(
      options,
      boogieEngine,
      CreateInitialCompilation(),
      latestIdeState.Value.VerificationTrees);

    var migratedUpdates = CompilationManager.CompilationUpdates.Select(compilation => {
      if (compilation.Version == compilationVersion) {
        latestIdeState =
          new Lazy<IdeState>(() => UpdateIdeState(migratedLazyPreviousCompilationLastIdeState.Value, compilation));
      }

      return latestIdeState;
    });
    var throttleTime = options.Get(UpdateThrottling);
    var throttledUpdates = throttleTime == 0 ? migratedUpdates : migratedUpdates.Sample(TimeSpan.FromMilliseconds(throttleTime));
    observerSubscription = throttledUpdates.
      Select(x => x.Value).Subscribe(observer);

    CompilationManager.Start();
  }

  private void TriggerVerificationForFile(Uri triggeringFile) {
    if (AutomaticVerificationMode is VerifyOnMode.Change or VerifyOnMode.ChangeProject) {
      var _ = VerifyEverythingAsync(AutomaticVerificationMode == VerifyOnMode.Change ? triggeringFile : null);
    } else {
      logger.LogDebug("Setting result for workCompletedForCurrentVersion");
    }
  }

  private static DafnyOptions DetermineProjectOptions(DafnyProject projectOptions, DafnyOptions serverOptions) {
    var result = new DafnyOptions(serverOptions);

    foreach (var option in LanguageServer.Options) {
      var hasProjectFileValue = projectOptions.TryGetValue(option, TextWriter.Null, out var projectFileValue);
      if (hasProjectFileValue) {
        result.Options.OptionArguments[option] = projectFileValue;
        result.ApplyBinding(option);
      }
    }

    if (result.SolverIdentifier == "Z3") {
      result.SolverVersion = null;
    }

    result.ApplyDefaultOptionsWithoutSettingsDefault();

    return result;
  }

  public void Save(TextDocumentIdentifier documentId) {
    if (VerifyOnSave) {
      logger.LogDebug("Clearing result for workCompletedForCurrentVersion");
      _ = VerifyEverythingAsync(documentId.Uri.ToUri());
    }
  }

  /// <summary>
  /// Needs to be thread-safe
  /// </summary>
  /// <returns></returns>
  public bool CloseDocument(out Task close) {
    if (Interlocked.Decrement(ref openFileCount) == 0) {
      close = CloseAsync();
      return true;
    }

    close = Task.CompletedTask;
    return false;
  }

  public async Task CloseAsync() {
    CompilationManager.Dispose();
    try {
      await CompilationManager.LastDocument;
      observer.OnCompleted();
    } catch (OperationCanceledException) {
    }
    Dispose();
  }

  public async Task<CompilationAfterParsing> GetLastDocumentAsync() {
    logger.LogDebug($"GetLastDocumentAsync passed ProjectManager check for {Project.Uri}");
    return await CompilationManager.LastDocument;
  }

  public async Task<IdeState> GetStateAfterParsingAsync() {
    try {
      var parsedCompilation = await CompilationManager.ParsedCompilation;
      logger.LogDebug($"GetSnapshotAfterParsingAsync returns compilation version {parsedCompilation.Version}");
    } catch (OperationCanceledException) {
      logger.LogDebug($"GetSnapshotAfterResolutionAsync caught OperationCanceledException for parsed compilation {Project.Uri}");
    }

    logger.LogDebug($"GetSnapshotAfterParsingAsync returns state version {latestIdeState.Value.Version}");
    return latestIdeState.Value;
  }

  public async Task<IdeState> GetStateAfterResolutionAsync() {
    try {
      var resolvedCompilation = await CompilationManager.ResolvedCompilation;
      logger.LogDebug($"GetStateAfterResolutionAsync returns compilation version {resolvedCompilation.Version}");
      logger.LogDebug($"GetStateAfterResolutionAsync returns state version {latestIdeState.Value.Version}");
      return latestIdeState.Value;
    } catch (OperationCanceledException) {
      logger.LogDebug($"GetSnapshotAfterResolutionAsync caught OperationCanceledException for resolved compilation {Project.Uri}");
      throw;
    }

  }

  public async Task<IdeState> GetIdeStateAfterVerificationAsync() {
    try {
      await GetLastDocumentAsync();
    } catch (OperationCanceledException) {
    }

    return latestIdeState.Value;
  }


  /// <summary>
  /// This property and related code will be removed once we replace server gutter icons with client side computed gutter icons
  /// </summary>
  public static bool GutterIconTesting = false;

  public async Task VerifyEverythingAsync(Uri? uri) {
    var compilationManager = CompilationManager;
    try {
      compilationManager.IncrementJobs();
      var resolvedCompilation = await compilationManager.ResolvedCompilation;

      var verifiables = resolvedCompilation.Verifiables?.ToList();
      if (verifiables == null) {
        return;
      }

      if (uri != null) {
        verifiables = verifiables.Where(d => d.Tok.Uri == uri).ToList();
      }

      List<FilePosition> changedVerifiables;
      lock (RecentChanges) {
        changedVerifiables = GetChangedVerifiablesFromRanges(verifiables, RecentChanges).ToList();
      }

      int GetPriorityAttribute(ISymbol symbol) {
        if (symbol is IAttributeBearingDeclaration hasAttributes &&
            hasAttributes.HasUserAttribute("priority", out var attribute) &&
            attribute.Args.Count >= 1 && attribute.Args[0] is LiteralExpr { Value: BigInteger priority }) {
          return (int)priority;
        }
        return 0;
      }

      int TopToBottomPriority(ISymbol symbol) {
        return symbol.Tok.pos;
      }
      var implementationOrder = changedVerifiables.Select((v, i) => (v, i)).ToDictionary(k => k.v, k => k.i);
      var orderedVerifiables = verifiables.OrderByDescending(GetPriorityAttribute).CreateOrderedEnumerable(
        t => implementationOrder.GetOrDefault(t.Tok.GetFilePosition(), () => int.MaxValue),
        null, false).CreateOrderedEnumerable(TopToBottomPriority, null, false).ToList();
      logger.LogDebug($"Ordered verifiables: {string.Join(", ", orderedVerifiables.Select(v => v.NameToken.val))}");

      var orderedVerifiableLocations = orderedVerifiables.Select(v => v.NameToken.GetFilePosition()).ToList();
      if (GutterIconTesting) {
        foreach (var canVerify in orderedVerifiableLocations) {
          await compilationManager.VerifySymbol(canVerify, true);
        }

        logger.LogDebug($"Finished translation in VerifyEverything for {Project.Uri}");
      }

      foreach (var canVerify in orderedVerifiableLocations) {
        // Wait for each task to try and run, so the order is respected.
        await compilationManager.VerifySymbol(canVerify);
      }
    }
    finally {
      logger.LogDebug("Setting result for workCompletedForCurrentVersion");
      compilationManager.DecrementJobs();
    }
  }

  private IEnumerable<FilePosition> GetChangedVerifiablesFromRanges(IReadOnlyList<ICanVerify> verifiables, IEnumerable<Location> changedRanges) {
    IntervalTree<Position, Position> GetTree(Uri uri) {
      var intervalTree = new IntervalTree<Position, Position>();
      foreach (var canVerify in verifiables) {
        if (canVerify.Tok.Uri == uri) {
          intervalTree.Add(
            canVerify.RangeToken.StartToken.GetLspPosition(),
            canVerify.RangeToken.EndToken.GetLspPosition(true),
            canVerify.NameToken.GetLspPosition());
        }
      }
      return intervalTree;
    }

    Dictionary<Uri, IntervalTree<Position, Position>> trees = new();

    return changedRanges.SelectMany(changeRange => {
      var uri = changeRange.Uri.ToUri();
      var tree = trees.GetOrCreate(uri, () => GetTree(uri));
      return tree.Query(changeRange.Range.Start, changeRange.Range.End).Select(position => new FilePosition(uri, position));
    }).Distinct();
  }

  public void OpenDocument(Uri uri, bool triggerCompilation) {
    Interlocked.Increment(ref openFileCount);

    if (triggerCompilation) {
      StartNewCompilation();
      TriggerVerificationForFile(uri);
    }
  }

  public void Dispose() {
    boogieEngine.Dispose();
    observerSubscription.Dispose();
    CompilationManager.Dispose();
  }

  private IdeState InitialIdeState(Compilation initialCompilation) {
    var program = new EmptyNode();
    return UpdateIdeState(new IdeState(initialCompilation.Version, initialCompilation, program,
      ImmutableDictionary<Uri, IReadOnlyList<Diagnostic>>.Empty,
      SymbolTable.Empty(), 
      new Lazy<LegacySignatureAndCompletionTable>(() => LegacySignatureAndCompletionTable.Empty(options, initialCompilation.Project)),
      ImmutableDictionary<Uri, Dictionary<Range, IdeVerificationResult>>.Empty,
      Array.Empty<Counterexample>(),
      ImmutableDictionary<Uri, IReadOnlyList<Range>>.Empty,
      initialCompilation.RootUris.ToDictionary(uri => uri, uri => new DocumentVerificationTree(program, uri))
    ), initialCompilation);
  }

  private IdeState UpdateIdeState(IdeState previousState, Compilation newCompilation) {
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
      bool computeResolveState = previousState.Compilation is not CompilationAfterResolution
        || previousState.Version < compilationAfterResolution.Version;
      // TODO do better than CancellationToken.None ?
      var cancellationToken = CancellationToken.None;
      var legacyTable = computeResolveState
        ? GetLegacyTable(compilationAfterResolution.Program, cancellationToken)
        : previousState.SignatureAndCompletionTable;
        
      result = result with {
        SymbolTable = compilationAfterResolution.SymbolTable ?? previousState.SymbolTable,
        // Remove the lazy part
        LazySignatureAndCompletionTable = new Lazy<LegacySignatureAndCompletionTable>(() => {
          return legacyTable.Resolved ? legacyTable : previousState.SignatureAndCompletionTable;
        }),
        GhostRanges = computeResolveState ? 
          ghostStateDiagnosticCollector.GetGhostStateDiagnostics(legacyTable, cancellationToken)
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
        (a, b) => new IdeImplementationView(
          a.Range,
          Combine(a.Status, b.Status),
          a.Diagnostics.Concat(b.Diagnostics).ToList(), a.HitErrorLimit || b.HitErrorLimit))));
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
    Program program,
    CancellationToken cancellationToken) {
    if (program.Reporter.HasErrorsUntilResolver) {
      // We cannot proceed without a successful resolution. Due to the contracts in dafny-lang, we cannot
      // access a property without potential contract violations. For example, a variable may have an
      // unresolved type represented by null. However, the contract prohibits the use of the type property
      // because it must not be null.
      return LegacySignatureAndCompletionTable.Empty(options, Project);
    } else {
      var beforeLegacyServerResolution = DateTime.Now;
      var result = symbolTableFactory.CreateFrom(Project.Uri, program, cancellationToken);
      telemetryPublisher.PublishTime("LegacyServerResolution", Project.Uri.ToString(), DateTime.Now - beforeLegacyServerResolution);
      return result;
    }

  }
}
