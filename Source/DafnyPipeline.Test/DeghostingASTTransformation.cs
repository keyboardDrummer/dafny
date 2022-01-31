using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.Dafny;
using TypeTransformers;
using Xunit;
using Type = System.Type;

namespace DafnyPipeline.Test; 

public class DeghostingASTTransformation {
  [Fact]
  public async Task DeghostingSimple() {
    

    await using var streamWriter = File.CreateText("../../../../Dafny/AST/DafnyAstDeghostedSimple.cs");
    await TypeTransformer.TransformType(
      new Uri(Path.GetFullPath("../../../../Dafny/AST/DafnyAst.cs")),
      streamWriter,
      "Microsoft.Dafny.Deghosted", 
      "Microsoft.Dafny", 
      Array.Empty<UsingDirectiveSyntax>(),
      typeof(Program), 
      unions: Utilities.GetUnionsInNamespace(typeof(Program), new HashSet<Type> {
        typeof(GreatestLemma), typeof(LeastLemma), typeof(ExtremeLemma), typeof(PrefixLemma), typeof(TwoStateLemma), typeof(Lemma)
      }), 
      new Dictionary<System.Type, TypeMutation> {
        { typeof(Method), new TypeMutation(new HashSet<string> {
          nameof(Method.Req), 
          nameof(Method.Ens),
          nameof(Method.Mod),
          nameof(Method.Decreases)
        })
        }
      }, new TransformOptions(true, false, true));
    
    /* TODO
     * Remove IsGhost fields and discard ghost objects
     * Translate Dafny functions to methods, or do we do that in a separate AST transformation?
     */
  }

  // class DeghostingTransformation : Microsoft.Dafny.Deghosted.ForwardTransformer {
  //
  //   public override Microsoft.Dafny.Deghosted.Method Transform(Method value) {
  //     return new Microsoft.Dafny.Deghosted.Method(value.Tok, value.Name, value.HasStaticKeyword, value.IsGhost, value.TypeArgs,
  //       value.Ins, value.Outs, value.Body, value.Attributes, value.SignatureEllipsis, value.IsByMethod);
  //     
  //     // TODO automatically reconstruct objects whose type only had fields removed.
  //   }
  //
  //   public override Microsoft.Dafny.Deghosted.Constructor Transform(Microsoft.Dafny.Constructor value) {
  //     return new Microsoft.Dafny.Deghosted.Constructor(value.Tok, value.Name, value.IsGhost, value.TypeArgs, value.Ins, value.DividedBody,
  //       value.Attributes, value.SignatureEllipsis);
  //   }
  //   
  //   public override ClassDecl TransformUnion(ClassDecl value) {
  //     return new ClassDecl(value.Tok, value.Name, Transform(value.EnclosingModuleDefinition), value.TypeArgs, 
  //       Transform_TopLevelDeclWithMembers_Members(value.Members).ToList(), Transform(value.Attributes), value.IsRefining, value.ParentTraits.Select(TransformUnion).ToList());
  //     
  //     // TODO enable overriding the transformation of individual fields such as TopDeclWithMembers.Members
  //     // Then we don't need to override each subclass of TopDeclWithMembers and reconstruct the whole object.
  //   }
  //   
  //   public override NewtypeDecl Transform(NewtypeDecl value) {
  //     return new NewtypeDecl(value.Tok, value.Name, Transform(value.EnclosingModuleDefinition), TransformUnion(value.BaseType), 
  //       Transform_TopLevelDeclWithMembers_Members(value.Members).ToList(),
  //       Transform(value.Attributes), value.IsRefining);
  //     
  //   }
  //   
  //   // TODO, make it possible to have this method override
  //   public List<MemberDecl> Transform_TopLevelDeclWithMembers_Members(List<MemberDecl> value) {
  //     return value.Where(m => !m.IsGhost).ToList();
  //   }
  // }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  [Fact]
  public async Task DeghostingIsGhost() {

    await using var streamWriter = File.CreateText("../../../../Dafny/AST/DafnyAst2.cs");
    await TypeTransformer.TransformType(
      new Uri(Path.GetFullPath("../../../../Dafny/AST/DafnyAst.cs")),
      streamWriter,
      "Microsoft.Dafny.V4", "Microsoft.Dafny", 
      new List<UsingDirectiveSyntax>(),
      typeof(Program), 
      unions: Utilities.GetUnionsInNamespace(typeof(Program), new HashSet<Type> {
        typeof(GreatestLemma), typeof(LeastLemma), typeof(ExtremeLemma), typeof(PrefixLemma), typeof(TwoStateLemma), typeof(Lemma),
        typeof(ExtremePredicate), typeof(Predicate), typeof(PrefixPredicate), typeof(LeastPredicate), typeof(GreatestPredicate), 
        typeof(TwoStatePredicate),
        typeof(TwoStateFunction),
      }), 
      new Dictionary<System.Type, TypeMutation>() {
        { typeof(MemberDecl), new TypeMutation(new HashSet<string>() { nameof(MemberDecl.IsGhost)}, new Dictionary<string, System.Type>() {
        })}
      }, new TransformOptions(true, false, true));
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