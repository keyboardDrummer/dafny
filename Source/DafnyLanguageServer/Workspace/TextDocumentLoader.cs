﻿using Microsoft.Dafny.LanguageServer.Language;
using Microsoft.Dafny.LanguageServer.Language.Symbols;
using Microsoft.Dafny.LanguageServer.Workspace.Notifications;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Boogie;
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
    private const int ResolverMaxStackSize = 0x10000000; // 256MB

    private readonly IDafnyParser parser;
    private readonly ISymbolResolver symbolResolver;
    private readonly ISymbolTableFactory symbolTableFactory;
    private readonly IGhostStateDiagnosticCollector ghostStateDiagnosticCollector;
    protected readonly ICompilationStatusNotificationPublisher statusPublisher;
    protected readonly ILoggerFactory loggerFactory;
    protected readonly INotificationPublisher NotificationPublisher;

    protected TextDocumentLoader(
      ILoggerFactory loggerFactory,
      IDafnyParser parser,
      ISymbolResolver symbolResolver,
      ISymbolTableFactory symbolTableFactory,
      IGhostStateDiagnosticCollector ghostStateDiagnosticCollector,
      ICompilationStatusNotificationPublisher statusPublisher,
      INotificationPublisher notificationPublisher) {
      this.parser = parser;
      this.symbolResolver = symbolResolver;
      this.symbolTableFactory = symbolTableFactory;
      this.ghostStateDiagnosticCollector = ghostStateDiagnosticCollector;
      this.statusPublisher = statusPublisher;
      this.loggerFactory = loggerFactory;
      NotificationPublisher = notificationPublisher;
    }

    public static TextDocumentLoader Create(
      DafnyOptions options,
      IDafnyParser parser,
      ISymbolResolver symbolResolver,
      ISymbolTableFactory symbolTableFactory,
      IGhostStateDiagnosticCollector ghostStateDiagnosticCollector,
      ICompilationStatusNotificationPublisher statusPublisher,
      ILoggerFactory loggerFactory,
      INotificationPublisher notificationPublisher
      ) {
      return new TextDocumentLoader(loggerFactory, parser, symbolResolver, symbolTableFactory, ghostStateDiagnosticCollector, statusPublisher, notificationPublisher);
    }

    public IdeState CreateUnloaded(DocumentTextBuffer textDocument, CancellationToken cancellationToken) {
      return CreateDocumentWithEmptySymbolTable(textDocument,
        new[] { new Diagnostic {
          // This diagnostic never gets sent to the client,
          // instead it forces the first computed diagnostics for a document to always be sent.
          // The message here describes the implicit client state before the first diagnostics have been sent.
          Message = "Resolution diagnostics have not been computed yet.",
          Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(0, 0, 0,0)
        }}
      );
    }

    public async Task<DocumentAfterParsing> LoadAsync(DafnyOptions options, DocumentTextBuffer textDocument,
      CancellationToken cancellationToken) {
#pragma warning disable CS1998
      return await await DafnyMain.LargeStackFactory.StartNew(
        async () => LoadInternal(options, textDocument, cancellationToken), cancellationToken
#pragma warning restore CS1998
        );
    }

    private DocumentAfterParsing LoadInternal(DafnyOptions options, DocumentTextBuffer textDocument,
      CancellationToken cancellationToken) {
      var errorReporter = new DiagnosticErrorReporter(options, textDocument.Text, textDocument.Uri);
      statusPublisher.SendStatusNotification(textDocument, CompilationStatus.Parsing);
      var program = parser.Parse(textDocument, errorReporter, cancellationToken);
      var documentAfterParsing = new DocumentAfterParsing(textDocument, program, errorReporter.AllDiagnosticsCopy);
      if (errorReporter.HasErrors) {
        statusPublisher.SendStatusNotification(textDocument, CompilationStatus.ParsingFailed);
        return documentAfterParsing;
      }

      statusPublisher.SendStatusNotification(textDocument, CompilationStatus.ResolutionStarted);
      try {
        var compilationUnit = symbolResolver.ResolveSymbols(textDocument, program, cancellationToken);
        var legacySymbolTable = symbolTableFactory.CreateFrom(compilationUnit, cancellationToken);

        var newSymbolTable = errorReporter.HasErrors
          ? null
          : symbolTableFactory.CreateFrom(program, documentAfterParsing, cancellationToken);
        if (errorReporter.HasErrors) {
          statusPublisher.SendStatusNotification(textDocument, CompilationStatus.ResolutionFailed);
        } else {
          statusPublisher.SendStatusNotification(textDocument, CompilationStatus.CompilationSucceeded);
        }

        var ghostDiagnostics = ghostStateDiagnosticCollector.GetGhostStateDiagnostics(legacySymbolTable, cancellationToken)
          .ToArray();

        return new DocumentAfterResolution(textDocument,
          program,
          errorReporter.AllDiagnosticsCopy,
          newSymbolTable,
          legacySymbolTable,
          ghostDiagnostics
        );
      } catch (OperationCanceledException) {
        return documentAfterParsing;
      }
    }

    private IdeState CreateDocumentWithEmptySymbolTable(
      DocumentTextBuffer textDocument,
      IReadOnlyList<Diagnostic> diagnostics
    ) {
      return new IdeState(
        textDocument,
        diagnostics,
        SymbolTable.Empty(),
        SignatureAndCompletionTable.Empty(DafnyOptions.Default, textDocument),
        new Dictionary<ImplementationId, IdeImplementationView>(),
        Array.Empty<Counterexample>(),
        false,
        Array.Empty<Diagnostic>(),
        new DocumentVerificationTree(textDocument)
      );
    }
  }
}


public record ImplementationId(Position NamedVerificationTask, string Name);
