using System.Threading;
using Microsoft.Dafny;
using Microsoft.Dafny.LanguageServer.Workspace;

namespace DafnyCore.Compilations {
  /// <summary>
  /// Interface exposing parse methods to generate a syntax tree out of an arbitrary dafny source.
  /// </summary>
  /// <remarks>
  /// Any implementation has to guarantee thread-safety of its public members.
  /// </remarks>
  public interface IDafnyParser {
    Program Parse(Compilation compilation, ErrorReporter reporter, CancellationToken cancellationToken);
  }
}
