using System;
using Microsoft.Dafny.LanguageServer.Workspace;

namespace DafnyCore.Compilations.GutterIcons; 

public interface IGutterNotificationPublisher {

  /// <summary>
  /// Publishes the more precise real-time verification diagnostics to the connected LSP client
  /// </summary>
  void PublishGutterIcons(Uri uri, CompilationAfterParsing compilation, bool verificationStarted);
  
}