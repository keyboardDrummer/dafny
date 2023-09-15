﻿using System.CommandLine;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using CommandLine;
using Microsoft.Dafny;
using Microsoft.Dafny.Plugins;
using Microsoft.Extensions.Logging.Abstractions;
using XUnitExtensions;
using XUnitExtensions.Lit;

namespace TestDafny;

[Verb("for-each-compiler", HelpText = "Execute the given test file for every compiler, and assert the output matches the <test file>.expect file.")]
public class ForEachCompilerOptions {

  [Value(0, Required = true, MetaName = "Test file", HelpText = "The *.dfy file to test.")]
  public string? TestFile { get; set; } = null;

  [Option("dafny", HelpText = "The dafny CLI to test with. Defaults to the locally built DafnyDriver project.")]
  public string? DafnyCliPath { get; set; } = null;

  [Value(1, MetaName = "Dafny CLI arguments", HelpText = "Any arguments following '--' will be passed to the dafny CLI unaltered.")]
  public IEnumerable<string> OtherArgs { get; set; } = Array.Empty<string>();
}

[Verb("features", HelpText = "Print the Markdown content documenting feature support for each compiler.")]
public class FeaturesOptions {
  [Value(1)]
  public IEnumerable<string> OtherArgs { get; set; } = Array.Empty<string>();
}

public class MultiBackendTest {
  private static readonly Assembly DafnyAssembly = typeof(Dafny.Dafny).Assembly;
  private readonly TextReader input;
  private readonly TextWriter output;
  private readonly TextWriter errorWriter;

  public MultiBackendTest(TextReader input, TextWriter output, TextWriter errorWriter) {
    this.input = input;
    this.output = output;
    this.errorWriter = errorWriter;
  }

  public static int Main(string[] args) {
    return new MultiBackendTest(Console.In, Console.Out, Console.Error).Start(args.ToList());
  }

  public int Start(IEnumerable<string> args) {
    var result = -1;
    var parser = new CommandLine.Parser(with => {
      with.EnableDashDash = true;
      with.HelpWriter = errorWriter;
    });
    var parseResult = parser.ParseArguments<ForEachCompilerOptions, FeaturesOptions>(args);
    parseResult.WithParsed<ForEachCompilerOptions>(options => { result = ForEachCompiler(options); })
      .WithParsed<FeaturesOptions>(options => { result = GenerateCompilerTargetSupportTable(options); });

    return result;
  }

  private DafnyOptions? ParseDafnyOptions(IEnumerable<string> dafnyArgs) {
    var dafnyOptions = new DafnyOptions(input, output, errorWriter);
    var success = dafnyOptions.Parse(dafnyArgs.ToArray());
    return success ? dafnyOptions : null;
  }

  private int ForEachCompiler(ForEachCompilerOptions options) {
    var parseResult = CommandRegistry.Create(TextWriter.Null, TextWriter.Null, TextReader.Null,
      new string[] { "verify", options.TestFile! }.Concat(options.OtherArgs).ToArray());
    var dafnyOptions = ((ParseArgumentSuccess)parseResult).DafnyOptions;

    // First verify the file (and assume that verification should be successful).
    // Older versions of test files that now use %testDafnyForEachCompiler were sensitive to the number
    // of verification conditions (i.e. the X in "Dafny program verifier finished with X verified, 0 errors"),
    // but this was never meaningful and only added maintenance burden.
    // Here we only ensure that the exit code is 0.

    // We also use --(r|b)print to catch bugs with valid but unprintable programs.
    string fileName = Path.GetFileName(options.TestFile!);
    var testDir = Path.GetDirectoryName(options.TestFile!);
    var tmpDPrint = Path.Join(testDir, "Output", $"{fileName}.dprint");
    var tmpRPrint = Path.Join(testDir, "Output", $"{fileName}.rprint");
    var tmpPrint = Path.Join(testDir, "Output", $"{fileName}.print");

    var dafnyArgs = new List<string>() {
      $"verify",
      options.TestFile!,
      $"--print:{tmpDPrint}",
      $"--rprint:{tmpRPrint}",
      $"--bprint:{tmpPrint}"
    }.Concat(options.OtherArgs.Where(OptionAppliesToVerifyCommand)).ToArray();

    output.WriteLine("Verifying...");

    var (exitCode, outputString, error) = RunDafny(options.DafnyCliPath, dafnyArgs);
    // If there is a .verifier.expect file, then we expect the output to match the .verifier.expect file contents. Otherwise, we
    // expect the output to be empty.
    var expectedOutput = "";
    var expectFileForVerifier = $"{options.TestFile}.verifier.expect";
    if (File.Exists(expectFileForVerifier)) {
      expectedOutput = File.ReadAllText(expectFileForVerifier);
    }
    // Chop off the "Dafny program verifier finished with..." trailer
    var trailer = new Regex("\r?\nDafny program verifier[^\r\n]*\r?\n").Match(outputString);
    var actualOutput = outputString.Remove(trailer.Index, trailer.Length);
    var diffMessage = AssertWithDiff.GetDiffMessage(expectedOutput, actualOutput);
    if (diffMessage != null) {
      output.WriteLine(diffMessage);
      return 1;
    }
    // We expect verification to return exit code 0.
    if (exitCode != 0) {
      output.WriteLine("Verification failed. Output:");
      output.WriteLine(outputString);
      output.WriteLine("Error:");
      output.WriteLine(error);
      return exitCode;
    }

    // Then execute the program for each available compiler.

    string expectFile = options.TestFile + ".expect";
    var commonExpectedOutput = File.ReadAllText(expectFile);

    var success = true;
    foreach (var plugin in dafnyOptions.Plugins) {
      foreach (var compiler in plugin.GetCompilers(dafnyOptions)) {
        if (!compiler.IsStable) {
          // Some tests still fail when using the lib back-end, for example due to disallowed assumptions being present in the test,
          // Such as empty constructors with ensures clauses, generated from iterators
          continue;
        }

        // Check for backend-specific exceptions (because of known bugs or inconsistencies)
        expectedOutput = commonExpectedOutput;
        string? checkFile = null;
        var expectFileForBackend = $"{options.TestFile}.{compiler.TargetId}.expect";
        if (File.Exists(expectFileForBackend)) {
          expectedOutput = File.ReadAllText(expectFileForBackend);
        }
        var checkFileForBackend = $"{options.TestFile}.{compiler.TargetId}.check";
        if (File.Exists(checkFileForBackend)) {
          checkFile = checkFileForBackend;
        }

        var result = RunWithCompiler(options, compiler, expectedOutput, checkFile);
        if (result != 0) {
          success = false;
        }
      }
    }

    if (success) {
      output.WriteLine(
        $"All executions were successful and matched the expected output (or reported errors for known unsupported features)!");
      return 0;
    } else {
      return -1;
    }
  }

  // Necessary to avoid passing invalid options to the first `dafny verify` command.
  // Ideally we could hook into the general `dafny` options parsing logic
  // and `ICommandSpec` commands instead.
  private static bool OptionAppliesToVerifyCommand(string option) {
    var name = option[2..].Split(':')[0];

    var compileOptions = new List<Option> {
      CommonOptionBag.SpillTranslation,
      CommonOptionBag.OptimizeErasableDatatypeWrapper,
      CommonOptionBag.AddCompileSuffix
    }.Select(o => o.Name);

    return !compileOptions.Contains(name);
  }

  private int RunWithCompiler(ForEachCompilerOptions options, IExecutableBackend backend, string expectedOutput, string? checkFile) {
    output.WriteLine($"Executing on {backend.TargetName}...");
    IEnumerable<string> dafnyArgs = new List<string> {
      "run",
      "--no-verify",
      $"--target:{backend.TargetId}",
      options.TestFile!,
    }.Concat(options.OtherArgs);

    var (exitCode, outputString, error) = RunDafny(options.DafnyCliPath, dafnyArgs);
    var compilationOutputPrior = new Regex("\r?\nDafny program verifier[^\r\n]*\r?\n").Match(outputString);
    if (compilationOutputPrior.Success) {
      outputString = outputString.Remove(0, compilationOutputPrior.Index + compilationOutputPrior.Length);
    }

    if (exitCode == 0) {
      var diffMessage = AssertWithDiff.GetDiffMessage(expectedOutput, outputString);
      if (diffMessage == null) {
        return 0;
      }

      output.WriteLine(diffMessage);
      return 1;
    }

    // If we hit errors, check for known unsupported features or bugs for this compilation target
    if (error == "" && OnlyUnsupportedFeaturesErrors(backend, outputString)) {
      return 0;
    }

    if (checkFile != null) {
      var outputLines = new List<string>();
      // Concatenate stdout and stderr so either can be checked against
      outputLines.AddRange(ReadAllLines(outputString));
      outputLines.AddRange(ReadAllLines(error));
      var checkDirectives = OutputCheckCommand.ParseCheckFile(checkFile);
      var (checkResult, checkOutput, checkError) = OutputCheckCommand.Execute(outputLines, checkDirectives);
      if (checkResult != 0) {
        output.WriteLine($"OutputCheck on {checkFile} failed:");
        output.WriteLine(checkOutput);
        output.WriteLine("Error:");
        output.WriteLine(checkError);
      }

      return checkResult;
    }

    output.WriteLine("Execution failed, for reasons other than known unsupported features. Output:");
    output.WriteLine(outputString);
    output.WriteLine("Error:");
    output.WriteLine(error);
    return exitCode;
  }

  public static IList<string> ReadAllLines(string s) {
    var result = new List<string>();
    var reader = new StringReader(s);
    while (reader.ReadLine() is { } line) {
      result.Add(line);
    }
    return result;
  }

  private static (int, string, string) RunDafny(IEnumerable<string> arguments) {
    var argumentsWithDefaults = arguments.Concat(DafnyDriver.NewDefaultArgumentsForTesting);
    var outputWriter = new StringWriter();
    var errorWriter = new StringWriter();
    var exitCode = DafnyDriver.MainWithWriters(outputWriter, errorWriter, TextReader.Null, argumentsWithDefaults.ToArray());
    var outputString = outputWriter.ToString();
    var error = errorWriter.ToString();
    return (exitCode, outputString, error);
  }


  private static (int, string, string) RunDafny(string? dafnyCLIPath, IEnumerable<string> arguments) {
    if (dafnyCLIPath == null) {
      return RunDafny(arguments);
    }

    var argumentsWithDefaults = arguments.Concat(DafnyDriver.NewDefaultArgumentsForTesting);
    ILitCommand command = new ShellLitCommand(dafnyCLIPath, argumentsWithDefaults, DafnyDriver.ReferencedEnvironmentVariables);

    return command.Execute(TextReader.Null, TextWriter.Null, TextWriter.Null);
  }

  private static bool OnlyUnsupportedFeaturesErrors(IExecutableBackend backend, string output) {
    using StringReader sr = new StringReader(output);
    while (sr.ReadLine() is { } line) {
      if (!IsAllowedOutputLine(backend, line)) {
        return false;
      }
    }

    return true;
  }

  private static bool IsAllowedOutputLine(IExecutableBackend backend, string line) {
    line = line.Trim();
    if (line.Length == 0) {
      return true;
    }

    // This is output if the compiler emits any errors
    if (line.StartsWith("Wrote textual form of partial target program to")) {
      return true;
    }

    // This is output if included files have errors,
    // which is expected if we're including another test file and testing different CLI options
    if (Regex.IsMatch(line, "Error: the included file .* contains error\\(s\\)")) {
      return true;
    }

    var prefixIndex = line.IndexOf(UnsupportedFeatureException.MessagePrefix, StringComparison.Ordinal);
    if (prefixIndex < 0) {
      return false;
    }

    var featureDescription = line[(prefixIndex + UnsupportedFeatureException.MessagePrefix.Length)..];
    var feature = FeatureDescriptionAttribute.ForDescription(featureDescription);
    if (backend.UnsupportedFeatures.Contains(feature)) {
      return true;
    }

    // This is an internal inconsistency error
    throw new Exception(
      $"Compiler rejected feature '{feature}', which is not an element of its UnsupportedFeatures set");
  }

  private int GenerateCompilerTargetSupportTable(FeaturesOptions featuresOptions) {
    var dafnyOptions = ParseDafnyOptions(featuresOptions.OtherArgs);
    if (dafnyOptions == null) {
      return (int)DafnyDriver.CommandLineArgumentsResult.PREPROCESSING_ERROR;
    }

    var allCompilers = dafnyOptions.Plugins
      .SelectMany(p => p.GetCompilers(dafnyOptions))
      .Where(c => !c.IsInternal)
      .ToList();

    // Header
    output.Write("| Feature |");
    foreach (var compiler in allCompilers) {
      output.Write($" {compiler.TargetName} |");
    }

    output.WriteLine();

    // Horizontal rule ("|----|---|...")
    output.Write("|-|");
    foreach (var _ in allCompilers) {
      output.Write($"-|");
    }

    output.WriteLine();

    var footnotes = new StringBuilder();
    foreach (var feature in Enum.GetValues(typeof(Feature)).Cast<Feature>()) {
      var description = FeatureDescriptionAttribute.GetDescription(feature);
      var footnoteLink = "";
      if (description.FootnoteIdentifier != null) {
        footnoteLink = $"[^{description.FootnoteIdentifier}]";
        footnotes.AppendLine($"{footnoteLink}: {description.Footnote}");
        footnotes.AppendLine();
      }

      output.Write($"| [{description.Description}](#{description.ReferenceManualSection}){footnoteLink} |");
      foreach (var compiler in allCompilers) {
        var supported = !compiler.UnsupportedFeatures.Contains(feature);
        var cell = supported ? " X " : "";
        output.Write($" {cell} |");
      }

      output.WriteLine();
    }

    output.WriteLine();
    output.WriteLine(footnotes);

    return 0;
  }
}
