using System;
using System.Collections.Generic;
using System.Linq;
using DafnyCore.Compilations.GutterIcons;
using Microsoft.Extensions.Logging;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;

namespace Microsoft.Dafny.LanguageServer.Workspace.Notifications;

class GutterNotificationPublisher : IGutterNotificationPublisher {
  private readonly Dictionary<Uri, VerificationStatusGutter> previouslyPublishedIcons = new();
  private readonly LanguageServerFilesystem filesystem;
  private readonly ILanguageServerFacade languageServer;
  private readonly ILogger<GutterNotificationPublisher> logger;
  
  public GutterNotificationPublisher(
    LanguageServerFilesystem filesystem, 
    ILogger<GutterNotificationPublisher> logger, 
    ILanguageServerFacade languageServer) 
  {
    this.filesystem = filesystem;
    this.logger = logger;
    this.languageServer = languageServer;
  }
  
  public void PublishGutterIcons(Uri uri, CompilationAfterParsing compilation, bool verificationStarted) {
    IdeState state = compilation.InitialIdeState(compilation, compilation.Program.Reporter.Options);
    
    if (!compilation.Options.Get(GutterIconAndHoverVerificationDetailsManager.LineVerificationStatus)) {
      return;
    }

    var errors = state.ResolutionDiagnostics.GetOrDefault(uri, Enumerable.Empty<Diagnostic>).
      Where(x => x.Severity == DiagnosticSeverity.Error).ToList();
    var tree = state.VerificationTrees[uri];

    var linesCount = tree.Range.End.Line + 1;
    var fileVersion = filesystem.GetVersion(uri);
    var verificationStatusGutter = VerificationStatusGutter.ComputeFrom(
      DocumentUri.From(uri),
      fileVersion,
      tree.Children,
      errors,
      linesCount,
      verificationStarted
    );
    if (logger.IsEnabled(LogLevel.Trace)) {
      var icons = string.Join(' ', verificationStatusGutter.PerLineStatus.Select(s => LineVerificationStatusToString[s]));
      logger.LogDebug($"Sending gutter icons for compilation {state.Compilation.Project.Uri}, comp version {state.Version}, file version {fileVersion}" +
                      $"icons: {icons}\n" +
                      $"stacktrace:\n{Environment.StackTrace}");
    };


    lock (previouslyPublishedIcons) {
      var previous = previouslyPublishedIcons.GetValueOrDefault(uri);
      if (previous == null || !previous.PerLineStatus.SequenceEqual(verificationStatusGutter.PerLineStatus)) {
        previouslyPublishedIcons[uri] = verificationStatusGutter;
        languageServer.TextDocument.SendNotification(verificationStatusGutter);
      }
    }
  }

  public static Dictionary<LineVerificationStatus, string> LineVerificationStatusToString = new() {
    { LineVerificationStatus.Nothing, "   " },
    { LineVerificationStatus.Scheduled, " . " },
    { LineVerificationStatus.Verifying, " S " },
    { LineVerificationStatus.VerifiedObsolete, " I " },
    { LineVerificationStatus.VerifiedVerifying, " $ " },
    { LineVerificationStatus.Verified, " | " },
    { LineVerificationStatus.ErrorContextObsolete, "[I]" },
    { LineVerificationStatus.ErrorContextVerifying, "[S]" },
    { LineVerificationStatus.ErrorContext, "[ ]" },
    { LineVerificationStatus.AssertionFailedObsolete, "[-]" },
    { LineVerificationStatus.AssertionFailedVerifying, "[~]" },
    { LineVerificationStatus.AssertionFailed, "[=]" },
    { LineVerificationStatus.AssertionVerifiedInErrorContextObsolete, "[o]" },
    { LineVerificationStatus.AssertionVerifiedInErrorContextVerifying, "[Q]" },
    { LineVerificationStatus.AssertionVerifiedInErrorContext, "[O]" },
    { LineVerificationStatus.ResolutionError, @"/!\" },
    { LineVerificationStatus.Skipped, @" ? " }
  };
}