// Copyright by the contributors to the Dafny Project
// SPDX-License-Identifier: MIT

using Microsoft.Boogie;
using System;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.Linq;

namespace Microsoft.Dafny {

  public abstract record ReportingLocation
  {
    public abstract FileRange FileRange { get; }

    public static implicit operator ReportingLocation(FileRange fileRange) {
      return new ReportingLocationFromFileRange(fileRange);
    }
  }

  record ReportingLocationFromFileRange(FileRange FileRange) : ReportingLocation {
    public override FileRange FileRange { get; } = FileRange;
  }

  public record ReportingLocationFromToken(IToken Token) : ReportingLocation {
    public override FileRange FileRange =>
      new(new DfyRange(new DfyPosition(Token.line, Token.col), new DfyPosition(Token.line, Token.col + Token.val.Length)),
        new Uri(Token.filename));
  }

  record LocationInInclude(ReportingLocation Location, Include Include) : ReportingLocation {
    public override FileRange FileRange => Location.FileRange;
  }

  public record LocationWithRelatedOnes(ReportingLocation Outer, ReportingLocation Inner, string InnerMessage) : ReportingLocation {
    public override FileRange FileRange => Outer.FileRange;
  }

  public enum ErrorLevel {
    Info, Warning, Error
  }

  public enum MessageSource {
    Parser, Cloner, RefinementTransformer, Rewriter, Resolver, Translator, Verifier, Compiler
  }

  public struct ErrorMessage {
    public FileRange FileRange;
    public string message;
    public MessageSource source;
  }

  public abstract class ErrorReporter {
    public bool ErrorsOnly { get; set; }

    public bool HasErrors => ErrorCount > 0;
    public int ErrorCount => Count(ErrorLevel.Error);
    public bool HasErrorsUntilResolver => ErrorCountUntilResolver > 0;
    public int ErrorCountUntilResolver => CountExceptVerifierAndCompiler(ErrorLevel.Error);


    public abstract bool Message(MessageSource source, ErrorLevel level, ReportingLocation location, string msg);

    public void Error(MessageSource source, ReportingLocation location, string msg) {
      Contract.Requires(msg != null);
      // if the tok is IncludeToken, we need to indicate to the including file
      // that there are errors in the included file.

      if (location is LocationInInclude locationInInclude) {
        if (!locationInInclude.Include.ErrorReported) {
          Message(source, ErrorLevel.Error, locationInInclude.Include.Syntax.FileRange, "the included file " + location.FileRange.File + " contains error(s)");
          locationInInclude.Include.ErrorReported = true;
        }
      }
      Message(source, ErrorLevel.Error, location, msg);
    }

    public abstract int Count(ErrorLevel level);
    public abstract int CountExceptVerifierAndCompiler(ErrorLevel level);

    // This method required by the Parser
    internal void Error(MessageSource source, string filename, int line, int col, string msg) {
      var tok = new Token(line, col);
      tok.filename = filename;
      Error(source, tok, msg);
    }

    public void Error(MessageSource source, IToken tok, string msg, params object[] args) {
      Contract.Requires(tok != null);
      Contract.Requires(msg != null);
      Contract.Requires(args != null);
      Error(source, new ReportingLocationFromToken(tok), String.Format(msg, args));
    }

    public void Error(MessageSource source, ReportingLocation location, string msg, params object[] args) {
      Contract.Requires(location != null);
      Contract.Requires(msg != null);
      Contract.Requires(args != null);
      Error(source, location, String.Format(msg, args));
    }

    public void Error(MessageSource source, Declaration d, string msg, params object[] args) {
      Contract.Requires(d != null);
      Contract.Requires(msg != null);
      Contract.Requires(args != null);
      Error(source, d.tok, msg, args);
    }

    public void Error(MessageSource source, Statement s, string msg, params object[] args) {
      Contract.Requires(s != null);
      Contract.Requires(msg != null);
      Contract.Requires(args != null);
      Error(source, s.Tok, msg, args);
    }

    public void Error(MessageSource source, IVariable v, string msg, params object[] args) {
      Contract.Requires(v != null);
      Contract.Requires(msg != null);
      Contract.Requires(args != null);
      Error(source, v.Tok, msg, args);
    }

    public void Error(MessageSource source, Expression e, string msg, params object[] args) {
      Contract.Requires(e != null);
      Contract.Requires(msg != null);
      Contract.Requires(args != null);
      Error(source, e.tok, msg, args);
    }

    public void Warning(MessageSource source, ReportingLocation location, string msg) {
      Contract.Requires(location != null);
      Contract.Requires(msg != null);
      if (DafnyOptions.O.WarningsAsErrors) {
        Error(source, location, msg);
      } else {
        Message(source, ErrorLevel.Warning, location, msg);
      }
    }

    public void Deprecated(MessageSource source, IToken tok, string msg, params object[] args) {
      Contract.Requires(tok != null);
      Contract.Requires(msg != null);
      Contract.Requires(args != null);
      if (DafnyOptions.O.DeprecationNoise != 0) {
        Warning(source, tok, String.Format(msg, args));
      }
    }

    public void DeprecatedStyle(MessageSource source, IToken tok, string msg, params object[] args) {
      Contract.Requires(tok != null);
      Contract.Requires(msg != null);
      Contract.Requires(args != null);
      if (DafnyOptions.O.DeprecationNoise == 2) {
        Warning(source, tok, String.Format(msg, args));
      }
    }

    public void Warning(MessageSource source, IToken tok, string msg, params object[] args) {
      Contract.Requires(tok != null);
      Contract.Requires(msg != null);
      Contract.Requires(args != null);
      Warning(source, tok, String.Format(msg, args));
    }

    public void Info(MessageSource source, IToken tok, string msg) {
      Contract.Requires(tok != null);
      Contract.Requires(msg != null);
      Message(source, ErrorLevel.Info, new ReportingLocationFromToken(tok), msg);
    }

    public void Info(MessageSource source, IToken tok, string msg, params object[] args) {
      Contract.Requires(tok != null);
      Contract.Requires(msg != null);
      Contract.Requires(args != null);
      Info(source, tok, String.Format(msg, args));
    }

    public static string ErrorToString(ErrorLevel header, string locationString, string msg) {
      return $"{locationString}: {header.ToString()}{": " + msg}";
    }

    public static string TokenToString(IToken tok) {
      return $"{tok.filename}({tok.line},{tok.col - 1})";
    }
  }

  public abstract class BatchErrorReporter : ErrorReporter {
    protected readonly Dictionary<ErrorLevel, List<ErrorMessage>> AllMessages;

    protected BatchErrorReporter() {
      ErrorsOnly = false;
      AllMessages = new Dictionary<ErrorLevel, List<ErrorMessage>> {
        [ErrorLevel.Error] = new(),
        [ErrorLevel.Warning] = new(),
        [ErrorLevel.Info] = new()
      };
    }

    public override bool Message(MessageSource source, ErrorLevel level, ReportingLocation location, string msg) {
      if (ErrorsOnly && level != ErrorLevel.Error) {
        // discard the message
        return false;
      }
      AllMessages[level].Add(new ErrorMessage { FileRange = location.FileRange, message = msg, source = source });
      return true;
    }

    public override int Count(ErrorLevel level) {
      return AllMessages[level].Count;
    }

    public override int CountExceptVerifierAndCompiler(ErrorLevel level) {
      return AllMessages[level].Count(message => message.source != MessageSource.Verifier &&
                                                 message.source != MessageSource.Compiler);
    }
  }

  public class ConsoleErrorReporter : BatchErrorReporter {
    private ConsoleColor ColorForLevel(ErrorLevel level) {
      switch (level) {
        case ErrorLevel.Error:
          return ConsoleColor.Red;
        case ErrorLevel.Warning:
          return ConsoleColor.Yellow;
        case ErrorLevel.Info:
          return ConsoleColor.Green;
        default:
          throw new cce.UnreachableException();
      }
    }

    public override bool Message(MessageSource source, ErrorLevel level, ReportingLocation location, string msg) {
      if (base.Message(source, level, location, msg) && ((DafnyOptions.O != null && DafnyOptions.O.PrintTooltips) || level != ErrorLevel.Info)) {
        // Extra indent added to make it easier to distinguish multiline error messages for clients that rely on the CLI
        msg = msg.Replace("\n", "\n ");

        ConsoleColor previousColor = Console.ForegroundColor;
        Console.ForegroundColor = ColorForLevel(level);
        var errorLine = ErrorToString(level, location.ToString(), msg);
        while (location is LocationWithRelatedOnes relatedOnes) {
          location = relatedOnes.Inner;
          if (location.FileRange == relatedOnes.Outer) {
            continue;
          }
          msg = relatedOnes.InnerMessage ?? "[Related location]";
          errorLine += $" {msg} {location}";
        }
        Console.WriteLine(errorLine);

        Console.ForegroundColor = previousColor;
        return true;
      } else {
        return false;
      }
    }
  }

  public class ErrorReporterSink : ErrorReporter {
    public ErrorReporterSink() { }

    public override bool Message(MessageSource source, ErrorLevel level, ReportingLocation location, string msg) {
      return false;
    }

    public override int Count(ErrorLevel level) {
      return 0;
    }

    public override int CountExceptVerifierAndCompiler(ErrorLevel level) {
      return 0;
    }
  }

  public class ErrorReporterWrapper : BatchErrorReporter {

    private string msgPrefix;
    public readonly ErrorReporter WrappedReporter;

    public ErrorReporterWrapper(ErrorReporter reporter, string msgPrefix) {
      this.msgPrefix = msgPrefix;
      this.WrappedReporter = reporter;
    }

    public override bool Message(MessageSource source, ErrorLevel level, ReportingLocation location, string msg) {
      base.Message(source, level, location, msg);
      return WrappedReporter.Message(source, level, location, msgPrefix + msg);
    }
  }
}
