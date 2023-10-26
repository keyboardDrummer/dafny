using System;
using OmniSharp.Extensions.LanguageServer.Protocol;
using System.Collections.Generic;
using System.Linq;
using Microsoft.Boogie;

namespace Microsoft.Dafny.LanguageServer.Workspace {


  /// <summary>
  /// Internal representation of a specific version of a Dafny document.
  ///
  /// Only one instance should exist of a specific version.
  /// Asynchronous compilation tasks use this instance to synchronise on.
  ///
  /// When verification starts, no new instances of Compilation will be created for this version.
  /// There can be different verification threads that update the state of this object.
  /// </summary>
  public class Compilation {
    /// <summary>
    /// These do not have to be owned
    /// </summary>
    public IReadOnlyList<Uri> RootUris { get; }
    public int Version { get; }
    public DafnyOptions Options { get; }
    public DafnyProject Project { get; }
    public DocumentUri Uri => Project.Uri;

    public Compilation(DafnyOptions options, int version, DafnyProject project, IReadOnlyList<Uri> rootUris) {
      this.RootUris = rootUris;
      Options = options;
      Version = version;
      Project = project;
    }

    public virtual IEnumerable<DafnyDiagnostic> GetDiagnostics(Uri uri) => Enumerable.Empty<DafnyDiagnostic>();

  }

  public record ImplementationState(IImplementationTask Task, PublishedVerificationStatus Status,
    IReadOnlyList<DafnyDiagnostic> Diagnostics, bool HitErrorLimit);

  public record BufferLine(int LineNumber, int StartIndex, int EndIndex);
}
