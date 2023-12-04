using System;
using System.Collections.Generic;
using System.CommandLine.Help;
using System.CommandLine.Invocation;
using System.CommandLine.IO;
using System.Diagnostics.Contracts;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.Boogie;
using Microsoft.Dafny.Plugins;

namespace Microsoft.Dafny;

public interface ILegacyParseArguments { }

// TODO: Refactor so that non-errors (NOT_VERIFIED, DONT_PROCESS_FILES) don't result in non-zero exit codes
public enum ExitValue { SUCCESS = 0, PREPROCESSING_ERROR, DAFNY_ERROR, COMPILE_ERROR, VERIFICATION_ERROR, FORMAT_ERROR }

public record ParsedOptions(DafnyOptions DafnyOptions) : ILegacyParseArguments;
record ExitImmediately(ExitValue ExitValue) : ILegacyParseArguments;

// TODO reduce usages
public static class DafnyBackwardsCompatibleCli {

  public static int Main(string[] args) {
    return MainWithWriters(Console.Out, Console.Error, Console.In, args);
  }

  public static int MainWithWriters(TextWriter outputWriter, TextWriter errorWriter, TextReader inputReader,
    string[] args) {
    // Code that shouldn't be needed, but prevents some exceptions when running the integration tests in parallel
    // outputWriter = new UndisposableTextWriter(outputWriter);
    // errorWriter = new UndisposableTextWriter(errorWriter);
    // outputWriter = TextWriter.Synchronized(outputWriter);
    // errorWriter = TextWriter.Synchronized(errorWriter);

#pragma warning disable VSTHRD002
    var exitCode = Task.Run(() => ThreadMain(outputWriter, errorWriter, inputReader, args)).Result;
    return exitCode;
#pragma warning restore VSTHRD002
  }

  private static Task<int> ThreadMain(TextWriter outputWriter, TextWriter errorWriter, TextReader inputReader, string[] args) {
    Contract.Requires(cce.NonNullElements(args));
    return Execute(outputWriter, errorWriter, inputReader, args, async parseArgumentResult => {

      switch (parseArgumentResult) {
        case ParsedOptions success:
          var options = success.DafnyOptions;
          return await CompilerDriver.RunLegacyCompiler(options);
        case ExitImmediately failure:
          return (int)failure.ExitValue;
        default: throw new Exception("unreachable");
      }
    });
  }

  private static async Task<int> Execute(TextWriter outputWriter,
    TextWriter errorWriter,
    TextReader inputReader, string[] arguments,
    Func<ILegacyParseArguments, Task<int>> onLegacyArguments) {

    var legacyResult = TryLegacyArgumentParser(inputReader, outputWriter, errorWriter, arguments);
    if (legacyResult != null) {
      return await onLegacyArguments(legacyResult);
    }

    return await DafnyNewCli.Execute(outputWriter, errorWriter, inputReader, arguments);
  }

  private static ILegacyParseArguments TryLegacyArgumentParser(
    TextReader inputReader,
    TextWriter outputWriter,
    TextWriter errorWriter,
    string[] arguments) {
    if (arguments.Length == 0) {
      return null;
    }
    var dafnyOptions = new DafnyOptions(inputReader, outputWriter, errorWriter) {
      Environment = "Command-line arguments: " + string.Join(" ", arguments)
    };

    var first = arguments[0];
    var keywordForNewMode = DafnyNewCli.RootCommand.Subcommands.Select(c => c.Name).Union(new[]
      { "--version", "-h", DafnyNewCli.ToolchainDebuggingHelpName, "--help", "[parse]", "[suggest]" });
    if (!keywordForNewMode.Contains(first)) {
      if (first.Length > 0 && first[0] != '/' && first[0] != '-' && !File.Exists(first) &&
          first.IndexOf('.') == -1) {
        dafnyOptions.Printer.ErrorWriteLine(dafnyOptions.OutputWriter,
          "*** Error: '{0}': The first input must be a command or a legacy option or file with supported extension",
          first);
        return new ExitImmediately(ExitValue.PREPROCESSING_ERROR);
      } else {
        var oldOptions = new DafnyOptions(dafnyOptions.Input, dafnyOptions.OutputWriter, dafnyOptions.ErrorWriter);
        try {
          if (oldOptions.Parse(arguments)) {
            // If requested, print version number, help, attribute help, etc. and exit.
            if (oldOptions.ProcessInfoFlags()) {
              return new ExitImmediately(ExitValue.SUCCESS);
            }

            return new ParsedOptions(oldOptions);
          }

          return new ExitImmediately(ExitValue.PREPROCESSING_ERROR);
        } catch (ProverException pe) {
          new DafnyConsolePrinter(dafnyOptions).ErrorWriteLine(dafnyOptions.OutputWriter,
            "*** ProverException: {0}", pe.Message);
          return new ExitImmediately(ExitValue.PREPROCESSING_ERROR);
        }
      }
    }

    return null;
  }
}