using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.Dafny;
using TypeTransformers;
using Xunit;
using Type = System.Type;

namespace DafnyPipeline.Test; 

public class TypeTransformerUser {
  [Fact]
  public void Use() {
    var usings = new [] { 
      SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("System.Collections.Generic")), 
      SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("Microsoft.Boogie"))
    };
    
    TypeTransformer.TransformType(new Uri("/Users/rwillems/Documents/SourceCode/dafny/Source/Dafny/AST/DafnyAst2.cs"),
      "Microsoft.Dafny.V2", "Microsoft.Dafny", usings,false, true, 
      typeof(Program), TypeTransformer.GetUnionsInNamespace(typeof(Program)), 
      new Dictionary<Type, TypeMutation>() {
        { typeof(UpdateStmt), new TypeMutation(new HashSet<string>() { nameof(UpdateStmt.Lhss)}, new Dictionary<string, Type>() {
          { "Lhss2", typeof(IReadOnlyList<AssignmentRhs>) }
        })}
      });
  }
}