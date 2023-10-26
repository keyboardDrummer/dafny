using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.Dafny.LanguageServer.Language;
using Microsoft.Dafny.LanguageServer.Workspace.Notifications;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace Microsoft.Dafny.LanguageServer.Workspace;

public class CompilationAfterParsing : Compilation {
  public IReadOnlyDictionary<Uri, List<DafnyDiagnostic>> ResolutionDiagnostics { get; set; }
  public Dictionary<Uri, DocumentVerificationTree> VerificationTrees { get; }

  public CompilationAfterParsing(Compilation compilation,
    Program program,
    IReadOnlyDictionary<Uri, List<DafnyDiagnostic>> diagnostics,
    Dictionary<Uri, DocumentVerificationTree> verificationTrees)
    : base(compilation.Options, compilation.Version, compilation.Project, compilation.RootUris) {
    ResolutionDiagnostics = diagnostics;
    VerificationTrees = verificationTrees;
    Program = program;
  }

  public Program Program { get; }

  public override IEnumerable<DafnyDiagnostic> GetDiagnostics(Uri uri) {
    return ResolutionDiagnostics.GetOrDefault(uri, Enumerable.Empty<DafnyDiagnostic>);
  }
  
  public VerificationTree GetVerificationTree(Uri uri) {
    return VerificationTrees[uri];
  }
}