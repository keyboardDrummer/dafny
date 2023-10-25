using System;
using System.Collections.Generic;
using Microsoft.Boogie;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using VC;

namespace Microsoft.Dafny.LanguageServer.Language {

  public record ProgramVerificationTasks(IReadOnlyList<IImplementationTask> Tasks);
}
