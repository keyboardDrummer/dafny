using System.IO;
using System.Threading.Tasks;
using Microsoft.Dafny.LanguageServer.IntegrationTest.Extensions;
using Microsoft.Dafny.LanguageServer.IntegrationTest.Util;
using Xunit;
using Xunit.Abstractions;

namespace Microsoft.Dafny.LanguageServer.IntegrationTest; 

public class ProjectFilesTest : ClientBasedLanguageServerTest {
  [Fact]
  public async Task ClosestProjectFileIsFound() {
    var filePath = Path.Combine(Directory.GetCurrentDirectory(), "ProjectFiles/TestFiles/Nesting/src/foo.dfy");
    var source = await File.ReadAllTextAsync(filePath);
    var documentItem = CreateTestDocument(source, filePath);
    await client.OpenDocumentAndWaitAsync(documentItem, CancellationToken);
    var diagnostics = await GetLastDiagnostics(documentItem, CancellationToken);

    Assert.Single(diagnostics);
    Assert.Equal("Shadowed local-variable name: x", diagnostics[0].Message);
  }

  [Fact]
  public async Task ProjectFileOverridesOptions() {
    await SetUp(options => options.WarnShadowing = true);
    var filePath = Path.Combine(Directory.GetCurrentDirectory(), "ProjectFiles/TestFiles/noWarnShadowing.dfy");
    var source = await File.ReadAllTextAsync(filePath);
    var documentItem = CreateTestDocument(source, filePath);
    await client.OpenDocumentAndWaitAsync(documentItem, CancellationToken);
    var diagnostics = await GetLastDiagnostics(documentItem, CancellationToken);

    Assert.Empty(diagnostics);
  }

  [Fact]
  public async Task FileOnlyAttachedToProjectFileThatAppliesToIt() {
    await SetUp(options => options.WarnShadowing = false);

    var projectFileSource = @"
[options]
warn-shadowing = true
";
    var directory = Path.GetTempPath();
    var outerProjectFile = CreateTestDocument(projectFileSource, Path.Combine(directory, "dfyconfig.toml"));
    await client.OpenDocumentAndWaitAsync(outerProjectFile, CancellationToken);
    
    var innerProjectFile = CreateTestDocument("includes = []", Path.Combine(directory, "nested", "dfyconfig.toml"));
    await client.OpenDocumentAndWaitAsync(innerProjectFile, CancellationToken);
      
    var filePath = Path.Combine(Directory.GetCurrentDirectory(), "ProjectFiles/TestFiles/noWarnShadowing.dfy");
    var source = await File.ReadAllTextAsync(filePath);
    var documentItem = CreateTestDocument(source, Path.Combine(directory, "nested/A.dfy"));
    await client.OpenDocumentAndWaitAsync(documentItem, CancellationToken);
    var diagnostics = await GetLastDiagnostics(documentItem, CancellationToken);
    Assert.Single(diagnostics);
    Assert.Contains("Shadowed", diagnostics[0].Message);
  }
  
  [Fact]
  public async Task InMemoryProjectFileOverridesOptions() {
    await SetUp(options => options.WarnShadowing = false);

    var projectFileSource = @"
[options]
warn-shadowing = true
";
    var directory = Path.GetTempPath();
    var projectFile = CreateTestDocument(projectFileSource, Path.Combine(directory, "dfyconfig.toml"));
    await client.OpenDocumentAndWaitAsync(projectFile, CancellationToken);
      
    var filePath = Path.Combine(Directory.GetCurrentDirectory(), "ProjectFiles/TestFiles/noWarnShadowing.dfy");
    var source = await File.ReadAllTextAsync(filePath);
    var documentItem = CreateTestDocument(source, Path.Combine(directory, "A.dfy"));
    await client.OpenDocumentAndWaitAsync(documentItem, CancellationToken);
    var diagnostics = await GetLastDiagnostics(documentItem, CancellationToken);
    Assert.Single(diagnostics);
    Assert.Contains("Shadowed", diagnostics[0].Message);
  }

  public ProjectFilesTest(ITestOutputHelper output) : base(output) {
  }
}