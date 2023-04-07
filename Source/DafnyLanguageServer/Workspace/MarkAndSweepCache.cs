using System;
using System.Collections;
using System.Collections.Generic;
using Serilog.Events;
using VC;

namespace Microsoft.Dafny.LanguageServer.Workspace;

class MarkAndSweepCache {
  private readonly Dictionary<object, object> cachedValues = new();
  private readonly object myLock = new ();
  private readonly ISet<object> marked = new HashSet<object>();
    
  public T? GetOrUpdate<T>(object key, Func<T> getValue) {
    lock (myLock) {
      marked.Add(key);
      if (!cachedValues.TryGetValue(key, out object? result)) {
        result = getValue();
        cachedValues[key] = result!;
      }

      return (T?)result;
    }
  }

  public void Sweep() {
    lock (myLock) {
      foreach (var key in cachedValues.Keys) {
        if (!marked.Contains(key)) {
          cachedValues.Remove(key);
        }
      }
    }
  }
}

class ReplayingErrorReporter : ErrorReporter {

  public void Replay(ErrorReporter reporter) {
    foreach (var call in calls) {
      reporter.Message(call.Source, call.Level, call.ErrorId, call.Token, call.Message);
    }
  }
  
  record MessageCallData(MessageSource Source, ErrorLevel Level, string ErrorId, IToken Token, string Message);

  private readonly List<MessageCallData> calls = new();
  public ReplayingErrorReporter(DafnyOptions options) : base(options)
  {
  }

  public override bool Message(MessageSource source, ErrorLevel level, string errorId, IToken tok, string msg) {
    calls.Add(new MessageCallData(source, level, errorId, tok, msg));
    return true;
  }

  public override int Count(ErrorLevel level) {
    throw new NotSupportedException();
  }

  public override int CountExceptVerifierAndCompiler(ErrorLevel level) {
    throw new NotSupportedException();
  }
}

record ParseKey(IDictionary<object, object> ParseRelatedOptions, string FileContents, Uri DocumentUri);
record ParseResult(IReadOnlyList<ModuleDefinition> Modules, ReplayingErrorReporter Errors, BuiltIns BuiltIns);

class CachedParser {
  public void Bla() {
    
    // int errorCount = Parser.Parse(
    //   useStdin: false,
    //   dafnyFile.SourceFileName,
    //   include,
    //   module,
    //   builtIns,
    //   errors,
    //   verifyThisFile: false,
    //   compileThisFile: false
    // );
    //
    //
    // var parseErrors = Parser.Parse(
    //   document.Text,
    //   // We use the full path as filename so we can better re-construct the DocumentUri for the definition lookup.
    //   document.Uri.ToString(),
    //   document.Uri.ToString(),
    //   program.DefaultModule,
    //   program.BuiltIns,
    //   errorReporter
    // );
  }
}