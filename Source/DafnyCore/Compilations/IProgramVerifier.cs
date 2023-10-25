using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Boogie;
using Microsoft.Dafny;
using Microsoft.Dafny.LanguageServer.Workspace;

namespace DafnyCore.Compilations;

/// <summary>
/// Implementations of this interface are responsible to verify the correctness of a program.
/// </summary>
public interface IProgramVerifier {
  /// <summary>
  /// Applies the program verification to the specified dafny program.
  /// </summary>
  /// <param name="engine"></param>
  /// <param name="compilation">The dafny document to verify.</param>
  /// <param name="moduleDefinition"></param>
  /// <param name="cancellationToken"></param>
  /// <param name="progressReporter"></param>
  /// <returns>The result of the verification run.</returns>
  /// <exception cref="System.OperationCanceledException">Thrown when the cancellation was requested before completion.</exception>
  /// <exception cref="System.ObjectDisposedException">Thrown if the cancellation token was disposed before the completion.</exception>
  Task<IReadOnlyList<IImplementationTask>> GetVerificationTasksAsync(ExecutionEngine engine,
    CompilationAfterResolution compilation,
    ModuleDefinition moduleDefinition,
    CancellationToken cancellationToken);
}