using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System.Collections.Generic;

namespace Microsoft.Dafny.LanguageServer.Handlers.Custom {
  public class CounterExampleItem {
    public DfyPosition Position { get; }
    public IDictionary<string, string> Variables { get; }

    public CounterExampleItem(DfyPosition position, IDictionary<string, string> variables) {
      Position = position;
      Variables = variables;
    }
  }
}
