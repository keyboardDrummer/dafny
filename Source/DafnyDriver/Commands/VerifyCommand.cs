using System.Collections.Generic;
using System.CommandLine;
using System.CommandLine.Invocation;
using System.Linq;

namespace Microsoft.Dafny;

class VerifyCommand : ICommandSpec {
  public IEnumerable<Option> Options => new Option[] {
    BoogieOptionBag.BoogieFilter,
  }.Concat(ICommandSpec.VerificationOptions.Except(new[] { BoogieOptionBag.NoVerify })).
    Concat(ICommandSpec.CommonOptions);

  public Command Create() {
    var result = new Command("verify", "Verify the program.");
    result.AddArgument(ICommandSpec.FilesArgument);
    return result;
  }

  public void PostProcess(DafnyOptions dafnyOptions, Options options, InvocationContext context) {
    dafnyOptions.Compile = false;
  }
}
