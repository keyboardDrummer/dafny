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
  public void Deghosting() {
    var usings = new [] {
      SyntaxFactory.UsingDirective(
        SyntaxFactory.NameEquals(SyntaxFactory.IdentifierName("IToken")), 
        SyntaxFactory.ParseName("Microsoft.Boogie.IToken")), 
      SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("System.Collections.Generic"))
    };
    
    TypeTransformer.TransformType(new Uri("/Users/rwillems/Documents/SourceCode/dafny/Source/Dafny/AST/DafnyAst2.cs"),
      "Microsoft.Dafny.V2", "Microsoft.Dafny", usings,true, false, 
      typeof(Program), TypeTransformer.GetUnionsInNamespace(typeof(Program)), 
      new Dictionary<Type, TypeMutation>() {
        { typeof(MemberDecl), new TypeMutation(new HashSet<string>() { nameof(MemberDecl.IsGhost)}, new Dictionary<string, Type>() {
        })}
      });
  }
  
  [Fact]
  public void Use() {
    var usings = new [] {
      SyntaxFactory.UsingDirective(
        SyntaxFactory.NameEquals(SyntaxFactory.IdentifierName("IToken")), 
        SyntaxFactory.ParseName("Microsoft.Boogie.IToken")), 
      SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("System.Collections.Generic"))
    };
    
    TypeTransformer.TransformType(new Uri("/Users/rwillems/Documents/SourceCode/dafny/Source/Dafny/AST/DafnyAst2.cs"),
      "Microsoft.Dafny.V2", "Microsoft.Dafny", usings,false, true, 
      typeof(Program), TypeTransformer.GetUnionsInNamespace(typeof(Program)), 
      new Dictionary<Type, TypeMutation>() {
        { typeof(UpdateStmt), new TypeMutation(new HashSet<string>() { nameof(UpdateStmt.Rhss)}, new Dictionary<string, Type>() {
          { "Rhss2", typeof(IReadOnlyList<AssignmentRhs>) }
        })}
      });
  }
}