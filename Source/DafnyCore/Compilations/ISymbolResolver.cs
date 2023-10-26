using System.Threading;
using Microsoft.Dafny;

namespace DafnyCore.Compilations {
  public interface ISymbolResolver {
    void ResolveSymbols(DafnyProject project, Program program, CancellationToken cancellationToken);
  }
}
