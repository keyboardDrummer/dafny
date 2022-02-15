using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.Boogie;
using Microsoft.Dafny;
using Xunit;
using Xunit.Abstractions;
using BoogieProgram = Microsoft.Boogie.Program;
using Parser = Microsoft.Dafny.Parser;

namespace DafnyPipeline.Test {
  // Main.Resolve has static shared state (TypeConstraint.ErrorsToBeReported for example)
  // so we can't execute tests that use it in parallel.
  [Collection("Singleton Test Collection - Resolution")]
  public class IntraMethodVerificationStability {
    private readonly ITestOutputHelper testOutputHelper;

    // All types of top level declarations.
    readonly string originalProgram = @"
module SomeModule {

  module NestedModule {
    class C {
      var f: int
      constructor ()
    }
  }

  method m() {
    var x: NestedModule.C;
    x := new NestedModule.C();
    x.f := 4;
  }
}

import opened SomeModule

type FooSynonym<T> = FooClass

class FooClass {
  var f: int
  constructor ()
}

datatype Friends = Agnes | Agatha | Jermaine

function method SomeFunc(funcFormal: int): nat { 3 }

method SomeMethod(methodFormal: int) returns (result: bool)
  requires methodFormal == 2
  ensures result == true 
  // ensures forall x :: x == methodFormal
{
  m();
  var lambdaExpr := x => x + 1;
  result := methodFormal == SomeFunc(42);
}
";

    readonly string renamedProgram = @"   

module SomeModule2 {

  module NestedModule2 {
      class C2 {
        var f2: int
        constructor ()
      }
    }

    method m2() {
      var x2: NestedModule2.C2;
      x2 := new NestedModule2.C2();
      x2.f2 := 4;
    }
}

import opened SomeModule2

type FooSynonym2<T> = FooClass2

class FooClass2 {
  var f: int
  constructor ()
}

datatype Friends2 = Agnes2 | Agatha2 | Jermaine2

function method SomeFunc2(funcFormal: int): nat { 3 }

method SomeMethod2(methodFormal2: int) returns (result2: bool) 
  requires methodFormal2 == 2
  ensures result2 == true
  // ensures forall x :: x == methodFormal
{
  m2();
  var lambdaExpr2 := x => x + 1;
  result2 := methodFormal2 == SomeFunc2(42);
}
";

    readonly string reorderedProgram = @"
method SomeMethod(methodFormal: int) returns (result: bool)
  requires methodFormal == 2
  ensures result == true
  // ensures forall x :: x == methodFormal
{
  m();
  var lambdaExpr := x => x + 1;
  result := methodFormal == SomeFunc(42);
}

function method SomeFunc(funcFormal: int): nat { 3 }

datatype Friends = Agnes | Agatha | Jermaine

class FooClass {
  var f: int
  constructor ()
}

type FooSynonym<T> = FooClass

import opened SomeModule

module SomeModule {

  module NestedModule {
    class C {
      var f: int
      constructor ()
    }
  }

  method m() {
    var x: NestedModule.C;
    x := new NestedModule.C();
    x.f := 4;
  }
}
";

    public IntraMethodVerificationStability(ITestOutputHelper testOutputHelper) {
      this.testOutputHelper = testOutputHelper;
    }

    [Fact]
    public void NoUniqueLinesWhenConcatenatingUnrelatedPrograms() {
      DafnyOptions.Install(DafnyOptions.FromArguments());

      var regularBoogie = GetBoogie(originalProgram).ToList();
      var renamedBoogie = GetBoogie(renamedProgram).ToList();
      var regularBoogieText = GetBoogieText(regularBoogie);
      var renamedBoogieText = GetBoogieText(renamedBoogie);
      var separate = UniqueNonCommentLines(regularBoogieText + renamedBoogieText);
      var combinedBoogie = GetBoogieText(GetBoogie(originalProgram + renamedProgram));
      var together = UniqueNonCommentLines(combinedBoogie);

      var uniqueLines = separate.Union(together).Except(separate.Intersect(together)).ToList();
      Assert.Equal(Enumerable.Empty<string>(), uniqueLines);
    }

    [Fact]
    public void EqualProverLogWhenReorderingProgram() {
      DafnyOptions.Install(DafnyOptions.FromArguments());
      DafnyOptions.O.ProcsToCheck.Add("*SomeMethod");

      var reorderedProverLog = GetProverLogForProgram(GetBoogie(reorderedProgram));
      var regularProverLog = GetProverLogForProgram(GetBoogie(originalProgram));
      Assert.Equal(regularProverLog, reorderedProverLog);
    }

    [Fact]
    public void EqualProverLogWhenRenamingProgram() {

      DafnyOptions.Install(DafnyOptions.FromArguments());
      DafnyOptions.O.ProcsToCheck.Add("*SomeMethod*");

      var renamedProverLog = GetProverLogForProgram(GetBoogie(renamedProgram));
      var regularProverLog = GetProverLogForProgram(GetBoogie(originalProgram));
      Assert.Equal(regularProverLog, renamedProverLog);
    }

    [Fact]
    public void EqualProverLogWhenAddingUnrelatedProgram() {

      DafnyOptions.Install(DafnyOptions.FromArguments());
      DafnyOptions.O.ProcsToCheck.Add("*SomeMethod");

      var renamedProverLog = GetProverLogForProgram(GetBoogie(renamedProgram + originalProgram));
      var regularProverLog = GetProverLogForProgram(GetBoogie(originalProgram));
      Assert.Equal(regularProverLog, renamedProverLog);
    }

    private string GetProverLogForProgram(IEnumerable<Microsoft.Boogie.Program> boogiePrograms) {
      var logs = GetProverLogsForProgram(boogiePrograms).ToList();
      Assert.Single(logs);
      return logs[0];
    }

    private IEnumerable<string> GetProverLogsForProgram(IEnumerable<Microsoft.Boogie.Program> boogiePrograms) {
      string directory = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName());
      Directory.CreateDirectory(directory);
      var temp1 = directory + "/proverLog";
      testOutputHelper.WriteLine("proverLog: " + temp1);
      DafnyOptions.O.ProverLogFilePath = temp1;
      foreach (var boogieProgram in boogiePrograms) {
        Main.BoogieOnce(DafnyOptions.O, "", "", boogieProgram, "programId", out _, out var outcome);
        testOutputHelper.WriteLine("outcome: " + outcome);
        foreach (var proverFile in Directory.GetFiles(directory)) {
          yield return File.ReadAllText(proverFile);
        }
      }
    }

    ISet<string> UniqueNonCommentLines(string input) {
      return input.Split('\n').Where(line => !line.TrimStart().StartsWith("//")).ToHashSet();
    }

    string PrintBoogie(BoogieProgram program) {
      var result = new StringWriter();
      var writer = new TokenTextWriter(result);
      program.Emit(writer);
      return result.ToString();
    }

    string GetBoogieText(IEnumerable<BoogieProgram> boogieProgram) {
      return string.Join('\n', boogieProgram.Select(PrintBoogie));
    }

    IEnumerable<BoogieProgram> GetBoogie(string dafnyProgramText) {
      var module = new LiteralModuleDecl(new DefaultModuleDecl(), null);
      var fullFilePath = "foo";
      Microsoft.Dafny.Type.ResetScopes();
      var builtIns = new BuiltIns();
      var errorReporter = new ConsoleErrorReporter();
      var parseResult = Parser.Parse(dafnyProgramText, fullFilePath, "foo", module, builtIns, errorReporter);
      Assert.Equal(0, parseResult);
      var dafnyProgram = new Microsoft.Dafny.Program(fullFilePath, module, builtIns, errorReporter);
      Main.Resolve(dafnyProgram, errorReporter);
      Assert.Equal(0, errorReporter.ErrorCount);
      return Translator.Translate(dafnyProgram, errorReporter).Select(t => t.Item2).ToList();
    }
  }


}