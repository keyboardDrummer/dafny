using System;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.Dafny;
using TypeTransformers;
using Xunit;
using Type = System.Type;

namespace DafnyPipeline.Test; 

public class TypeTransformerUser {
  [Fact]
  public async Task DeghostingSimple() {
    
    var compUnit = SyntaxFactory.ParseCompilationUnit(@"
using System.Diagnostics.Contracts;
using IToken = Microsoft.Boogie.IToken;
using System.Numerics;
using Microsoft.Boogie;
System.Collections.Generic;");

    await using var streamWriter = File.CreateText("/Users/rwillems/Documents/SourceCode/dafny/Source/Dafny/AST/DafnyAstDeghostedSimple.cs");
    await TypeTransformer.TransformType(
      new Uri("/Users/rwillems/Documents/SourceCode/dafny/Source/Dafny/AST/DafnyAst.cs"),
      streamWriter,
      "Microsoft.Dafny.V2", "Microsoft.Dafny", 
      compUnit.Usings,
      true, false, 
      typeof(Program), Utilities.GetUnionsInNamespace(typeof(Program)), 
      new Dictionary<Type, TypeMutation>() {
        { typeof(Method), new TypeMutation(new HashSet<string>() {
          nameof(Method.Req), 
          nameof(Method.Ens)
        }) }
      });
  }
  
  [Fact]
  public async Task Deghosting() {
    
    var compUnit = SyntaxFactory.ParseCompilationUnit(@"
using System.Diagnostics.Contracts;
using IToken = Microsoft.Boogie.IToken;
using System.Numerics;
using Microsoft.Boogie;
System.Collections.Generic;");

    await using var streamWriter = File.CreateText("/Users/rwillems/Documents/SourceCode/dafny/Source/Dafny/AST/DafnyAst2.cs");
    await TypeTransformer.TransformType(
      new Uri("/Users/rwillems/Documents/SourceCode/dafny/Source/Dafny/AST/DafnyAst.cs"),
      streamWriter,
      "Microsoft.Dafny.V2", "Microsoft.Dafny", 
      compUnit.Usings,
      true, false, 
      typeof(Program), Utilities.GetUnionsInNamespace(typeof(Program)), 
      new Dictionary<Type, TypeMutation>() {
        { typeof(MemberDecl), new TypeMutation(new HashSet<string>() { nameof(MemberDecl.IsGhost)}, new Dictionary<string, Type>() {
        })}
      });
  }
  
  // [Fact]
  // public void Use() {
  //   var usings = new [] {
  //     SyntaxFactory.UsingDirective(
  //       SyntaxFactory.NameEquals(SyntaxFactory.IdentifierName("IToken")), 
  //       SyntaxFactory.ParseName("Microsoft.Boogie.IToken")), 
  //     SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("System.Collections.Generic"))
  //   };
  //   
  //   TypeTransformer.TransformType(new Uri("/Users/rwillems/Documents/SourceCode/dafny/Source/Dafny/AST/DafnyAst2.cs"),
  //     "Microsoft.Dafny.V2", "Microsoft.Dafny", usings,false, true, 
  //     typeof(Program), TypeTransformer.GetUnionsInNamespace(typeof(Program)), 
  //     new Dictionary<Type, TypeMutation>() {
  //       { typeof(UpdateStmt), new TypeMutation(new HashSet<string>() { nameof(UpdateStmt.Rhss)}, new Dictionary<string, Type>() {
  //         { "Rhss2", typeof(IReadOnlyList<AssignmentRhs>) }
  //       })}
  //     });
  // }
}