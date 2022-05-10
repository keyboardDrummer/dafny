using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using Microsoft.Dafny.LanguageServer.Workspace.Notifications;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;
using System.Linq;
using Microsoft.Boogie;
using Microsoft.Dafny.LanguageServer.Language;
using Microsoft.Dafny.LanguageServer.Util;
using OmniSharp.Extensions.LanguageServer.Protocol;
using VC;

namespace Microsoft.Dafny.LanguageServer.Workspace {
  public class DiagnosticPublisher : IDiagnosticPublisher {
    public const string VerificationStatusNotification = "textDocument/verificationStatus";
    private readonly ILanguageServerFacade languageServer;

    public DiagnosticPublisher(ILanguageServerFacade languageServer) {
      this.languageServer = languageServer;
    }

    private readonly ConcurrentDictionary<DocumentUri, PublishDiagnosticsParams> previouslyPublishedDiagnostics = new();
    private readonly ConcurrentDictionary<DocumentUri, GhostDiagnosticsParams> previouslyPublishedGhostDiagnostics = new();

    public void PublishDiagnostics(DafnyDocument document) {
      if (document.LoadCanceled) {
        // We leave the responsibility to shift the error locations to the LSP clients.
        // Therefore, we do not republish the errors when the document (re-)load was canceled.
        return;
      }

      PublishDocumentDiagnostics(document);
      PublishGhostDiagnostics(document);
    }

    private void PublishDocumentDiagnostics(DafnyDocument document) {
      var newParams = new PublishDiagnosticsParams {
        Uri = document.Uri,
        Version = document.Version,
        Diagnostics = document.Diagnostics.ToArray(),
      };
      if (previouslyPublishedDiagnostics.TryGetValue(document.Uri, out var previousParams) && previousParams.Diagnostics.Equals(newParams.Diagnostics)) {
        return;
      }

      previouslyPublishedDiagnostics.AddOrUpdate(document.Uri, _ => newParams, (_, _) => newParams);
      languageServer.TextDocument.PublishDiagnostics(newParams);
    }

    private void PublishGhostDiagnostics(DafnyDocument document) {

      var newParams = new GhostDiagnosticsParams {
        Uri = document.Uri,
        Version = document.Version,
        Diagnostics = document.GhostDiagnostics.ToArray(),
      };
      if (previouslyPublishedGhostDiagnostics.TryGetValue(document.Uri, out var previousParams) && previousParams.Diagnostics.Equals(newParams.Diagnostics)) {
        return;
      }
      previouslyPublishedGhostDiagnostics.AddOrUpdate(document.Uri, _ => newParams, (_, _) => newParams);
      languageServer.TextDocument.SendNotification(newParams);
    }

    public void HideDiagnostics(TextDocumentIdentifier documentId) {
      languageServer.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams {
        Uri = documentId.Uri,
        Diagnostics = new Container<Diagnostic>()
      });
    }
  }
}
