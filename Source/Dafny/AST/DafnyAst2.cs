namespace Microsoft.Dafny.V2
{
    using System;
    using System.Linq;
    using Microsoft.Dafny;
    using IToken = Microsoft.Boogie.IToken;
    using System.Collections.Generic;

    class ClassDecl : TopLevelDeclWithMembers
    {
        IToken tok; String name; ModuleDefinition enclosingModuleDefinition; List<TypeParameter> typeArgs; List<Microsoft.Dafny.V2.MemberDecl> members; Attributes attributes; Boolean isRefining; List<Boogie.Type> parentTraits; 
        public ClassDecl(IToken tok, String name, ModuleDefinition enclosingModuleDefinition, List<TypeParameter> typeArgs, List<Microsoft.Dafny.V2.MemberDecl> members, Attributes attributes, Boolean isRefining, List<Boogie.Type> parentTraits) : base(tok, name, enclosingModuleDefinition, typeArgs, members, attributes, isRefining)
        {
            this.tok = tok;
            this.name = name;
            this.enclosingModuleDefinition = enclosingModuleDefinition;
            this.typeArgs = typeArgs;
            this.members = members;
            this.attributes = attributes;
            this.isRefining = isRefining;
            this.parentTraits = parentTraits;
        }
    }

    class MemberDecl : Declaration
    {
        IToken tok; String name; Boolean hasStaticKeyword; Boolean isGhost; Attributes attributes; Boolean isRefining; 
        public MemberDecl(IToken tok, String name, Boolean hasStaticKeyword, Boolean isGhost, Attributes attributes, Boolean isRefining) : base(tok, name, attributes, isRefining)
        {
            this.tok = tok;
            this.name = name;
            this.hasStaticKeyword = hasStaticKeyword;
            this.isGhost = isGhost;
            this.attributes = attributes;
            this.isRefining = isRefining;
        }
    }

    class TraitDecl : ClassDecl
    {
        IToken tok; String name; ModuleDefinition enclosingModuleDefinition; List<TypeParameter> typeArgs; List<Microsoft.Dafny.V2.MemberDecl> members; Attributes attributes; Boolean isRefining; List<Boogie.Type> parentTraits; 
        public TraitDecl(IToken tok, String name, ModuleDefinition enclosingModuleDefinition, List<TypeParameter> typeArgs, List<Microsoft.Dafny.V2.MemberDecl> members, Attributes attributes, Boolean isRefining, List<Boogie.Type> parentTraits) : base(tok, name, enclosingModuleDefinition, typeArgs, members, attributes, isRefining, parentTraits)
        {
            this.tok = tok;
            this.name = name;
            this.enclosingModuleDefinition = enclosingModuleDefinition;
            this.typeArgs = typeArgs;
            this.members = members;
            this.attributes = attributes;
            this.isRefining = isRefining;
            this.parentTraits = parentTraits;
        }
    }

    class DefaultClassDecl : ClassDecl
    {
        ModuleDefinition enclosingModuleDefinition; List<Microsoft.Dafny.V2.MemberDecl> members; 
        public DefaultClassDecl(ModuleDefinition enclosingModuleDefinition, List<Microsoft.Dafny.V2.MemberDecl> members) : base(enclosingModuleDefinition, members)
        {
            this.enclosingModuleDefinition = enclosingModuleDefinition;
            this.members = members;
        }
    }

    class IndDatatypeDecl : DatatypeDecl
    {
        IToken tok; String name; ModuleDefinition enclosingModuleDefinition; List<TypeParameter> typeArgs; List<DatatypeCtor> ctors; List<Microsoft.Dafny.V2.MemberDecl> members; Attributes attributes; Boolean isRefining; 
        public IndDatatypeDecl(IToken tok, String name, ModuleDefinition enclosingModuleDefinition, List<TypeParameter> typeArgs, List<DatatypeCtor> ctors, List<Microsoft.Dafny.V2.MemberDecl> members, Attributes attributes, Boolean isRefining) 
          : base(tok, name, enclosingModuleDefinition, typeArgs, ctors, members, attributes, isRefining)
        {
            this.tok = tok;
            this.name = name;
            this.enclosingModuleDefinition = enclosingModuleDefinition;
            this.typeArgs = typeArgs;
            this.ctors = ctors;
            this.members = members;
            this.attributes = attributes;
            this.isRefining = isRefining;
        }
    }

    class CoDatatypeDecl : DatatypeDecl
    {
        IToken tok; String name; ModuleDefinition enclosingModuleDefinition; List<TypeParameter> typeArgs; List<DatatypeCtor> ctors; List<Microsoft.Dafny.V2.MemberDecl> members; Attributes attributes; Boolean isRefining; 
        public CoDatatypeDecl(IToken tok, String name, ModuleDefinition enclosingModuleDefinition, List<TypeParameter> typeArgs, List<DatatypeCtor> ctors, List<Microsoft.Dafny.V2.MemberDecl> members, Attributes attributes, Boolean isRefining) 
          : base(tok, name, typeArgs, ctors, members, attributes, isRefining)
        {
            this.tok = tok;
            this.name = name;
            this.enclosingModuleDefinition = enclosingModuleDefinition;
            this.typeArgs = typeArgs;
            this.ctors = ctors;
            this.members = members;
            this.attributes = attributes;
            this.isRefining = isRefining;
        }
    }

    class OpaqueTypeDecl : TopLevelDeclWithMembers
    {
        IToken tok; String name; ModuleDefinition enclosingModuleDefinition; TypeParameter.TypeParameterCharacteristics characteristics; List<TypeParameter> typeArgs; List<Microsoft.Dafny.V2.MemberDecl> members; Attributes attributes; Boolean isRefining; 
        public OpaqueTypeDecl(IToken tok, String name, ModuleDefinition enclosingModuleDefinition, TypeParameter.TypeParameterCharacteristics characteristics, List<TypeParameter> typeArgs, List<Microsoft.Dafny.V2.MemberDecl> members, Attributes attributes, Boolean isRefining) 
          : base(tok, name, enclosingModuleDefinition, typeArgs, members, attributes, isRefining)
        {
            this.tok = tok;
            this.name = name;
            this.enclosingModuleDefinition = enclosingModuleDefinition;
            this.characteristics = characteristics;
            this.typeArgs = typeArgs;
            this.members = members;
            this.attributes = attributes;
            this.isRefining = isRefining;
        }
    }

    class NewtypeDecl : TopLevelDeclWithMembers
    {
        IToken tok; String name; ModuleDefinition enclosingModuleDefinition; Boogie.Type baseType; List<Microsoft.Dafny.V2.MemberDecl> members; Attributes attributes; Boolean isRefining; 
        public NewtypeDecl(IToken tok, String name, ModuleDefinition enclosingModuleDefinition, Boogie.Type baseType, List<Microsoft.Dafny.V2.MemberDecl> members, Attributes attributes, Boolean isRefining) 
          : base(tok, name, enclosingModuleDefinition, new List<TypeParameter>(), members, attributes, isRefining)
        {
            this.tok = tok;
            this.name = name;
            this.enclosingModuleDefinition = enclosingModuleDefinition;
            this.baseType = baseType;
            this.members = members;
            this.attributes = attributes;
            this.isRefining = isRefining;
        }
    }

    class NonNullTypeDecl : SubsetTypeDecl
    {
        Microsoft.Dafny.V2.ClassDecl classDecl; public NonNullTypeDecl(Microsoft.Dafny.V2.ClassDecl classDecl) : base()
        {
            this.classDecl = classDecl;
        }
    }

    class ForwardTransformer
    {
        public virtual TopLevelDecl TransformUnion(TopLevelDecl value) =>
          value switch
          {
              Microsoft.Dafny.TypeParameter subType => subType,
              Microsoft.Dafny.ModuleDecl subType => TransformUnion(subType),
              Microsoft.Dafny.TopLevelDeclWithMembers subType => TransformUnion(subType),
              Microsoft.Dafny.ValuetypeDecl subType => Transform(subType),
              Microsoft.Dafny.TypeSynonymDeclBase subType => TransformUnion(subType),
              Microsoft.Dafny.Resolver.AmbiguousTopLevelDecl subType => subType,
          };

        public virtual ModuleDecl TransformUnion(ModuleDecl value) =>
          value switch
          {
              Microsoft.Dafny.LiteralModuleDecl subType => Transform(subType),
              Microsoft.Dafny.AliasModuleDecl subType => Transform(subType),
              Microsoft.Dafny.AbstractModuleDecl subType => Transform(subType),
              Microsoft.Dafny.ModuleExportDecl subType => Transform(subType),
          };

        public virtual ModuleDefinition TransformUnion(ModuleDefinition value) =>
          value switch
          {
              Microsoft.Dafny.DefaultModuleDecl subType => subType,
          };

        public virtual ClassDecl TransformUnion(ClassDecl value) =>
          value switch
          {
              Microsoft.Dafny.TraitDecl subType => Transform(subType),
              Microsoft.Dafny.DefaultClassDecl subType => Transform(subType),
              Microsoft.Dafny.ArrayClassDecl subType => Transform(subType),
              Microsoft.Dafny.ArrowTypeDecl subType => Transform(subType),
              Microsoft.Dafny.IteratorDecl subType => Transform(subType),
          };

        public virtual TopLevelDeclWithMembers TransformUnion(TopLevelDeclWithMembers value) =>
          value switch
          {
              Microsoft.Dafny.ClassDecl subType => TransformUnion(subType),
              Microsoft.Dafny.DatatypeDecl subType => TransformUnion(subType),
              Microsoft.Dafny.OpaqueTypeDecl subType => Transform(subType),
              Microsoft.Dafny.NewtypeDecl subType => Transform(subType),
          };

        public virtual DatatypeDecl TransformUnion(DatatypeDecl value) =>
          value switch
          {
              Microsoft.Dafny.IndDatatypeDecl subType => TransformUnion(subType),
              Microsoft.Dafny.CoDatatypeDecl subType => Transform(subType),
          };

        public virtual IndDatatypeDecl TransformUnion(IndDatatypeDecl value) =>
          value switch
          {
              Microsoft.Dafny.TupleTypeDecl subType => Transform(subType),
          };

        public virtual MemberDecl TransformUnion(MemberDecl value) =>
          value switch
          {
              Microsoft.Dafny.Field subType => subType,
              Microsoft.Dafny.Function subType => TransformUnion(subType),
              Microsoft.Dafny.Method subType => subType,
          };

        public virtual Function TransformUnion(Function value) =>
          value switch
          {
              Microsoft.Dafny.SpecialFunction subType => Transform(subType),
              Microsoft.Dafny.Predicate subType => subType,
              Microsoft.Dafny.PrefixPredicate subType => subType,
              Microsoft.Dafny.ExtremePredicate subType => subType,
              Microsoft.Dafny.TwoStateFunction subType => subType,
          };

        public virtual TypeSynonymDeclBase TransformUnion(TypeSynonymDeclBase value) =>
          value switch
          {
              Microsoft.Dafny.TypeSynonymDecl subType => TransformUnion(subType),
              Microsoft.Dafny.InternalTypeSynonymDecl subType => Transform(subType),
          };

        public virtual TypeSynonymDecl TransformUnion(TypeSynonymDecl value) =>
          value switch
          {
              Microsoft.Dafny.SubsetTypeDecl subType => TransformUnion(subType),
          };

        public virtual SubsetTypeDecl TransformUnion(SubsetTypeDecl value) =>
          value switch
          {
              Microsoft.Dafny.NonNullTypeDecl subType => Transform(subType),
          };

        public virtual LiteralModuleDecl Transform(LiteralModuleDecl value)
        {
            return new LiteralModuleDecl(TransformUnion(value.ModuleDef), TransformUnion(value.EnclosingModuleDefinition));
        }

        public virtual ModuleDefinition Transform(ModuleDefinition value)
        {
            return new ModuleDefinition(value.Tok, value.Name, value.PrefixIds, value.IsAbstract, value.IsFacade, value.RefinementQId, TransformUnion(value.EnclosingModule), value.Attributes, value.IsBuiltinName, value.IsToBeVerified, value.IsToBeCompiled, value.TopLevelDecls.Select(TransformUnion).ToList());
        }

        public virtual Microsoft.Dafny.ClassDecl Transform(Microsoft.Dafny.ClassDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Function Transform(Function value)
        {
            return new Function(value.Tok, value.Name, value.HasStaticKeyword, value.IsGhost, value.TypeArgs, value.Formals, value.Result, value.ResultType, value.Req, value.Reads, value.Ens, value.Decreases, value.Body, value.ByMethodTok, value.ByMethodBody, value.Attributes, value.SignatureEllipsis);
        }

        public virtual SpecialFunction Transform(SpecialFunction value)
        {
            return new SpecialFunction(value.Tok, value.Name, TransformUnion(value.Module), value.HasStaticKeyword, value.IsGhost, value.TypeArgs, value.Formals, value.ResultType, value.Req, value.Reads, value.Ens, value.Decreases, value.Body, value.Attributes, value.SignatureEllipsis);
        }

        public virtual Microsoft.Dafny.TraitDecl Transform(Microsoft.Dafny.TraitDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.DefaultClassDecl Transform(Microsoft.Dafny.DefaultClassDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual ArrayClassDecl Transform(ArrayClassDecl value)
        {
            return new ArrayClassDecl(value.Dims, TransformUnion(value.EnclosingModuleDefinition), value.Attributes);
        }

        public virtual ArrowTypeDecl Transform(ArrowTypeDecl value)
        {
            return new ArrowTypeDecl(value.TypeArgs, TransformUnion(value.Requires), TransformUnion(value.Reads), TransformUnion(value.EnclosingModuleDefinition), value.Attributes);
        }

        public virtual IteratorDecl Transform(IteratorDecl value)
        {
            return new IteratorDecl(value.Tok, value.Name, TransformUnion(value.EnclosingModuleDefinition), value.TypeArgs, value.Ins, value.Outs, value.Reads, value.Modifies, value.Decreases, value.Requires, value.Ensures, value.YieldRequires, value.YieldEnsures, value.Body, value.Attributes, value.SignatureEllipsis);
        }

        public virtual Microsoft.Dafny.IndDatatypeDecl Transform(Microsoft.Dafny.IndDatatypeDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual TupleTypeDecl Transform(TupleTypeDecl value)
        {
            return new TupleTypeDecl(value.ArgumentGhostness, TransformUnion(value.EnclosingModuleDefinition), value.Attributes);
        }

        public virtual Microsoft.Dafny.CoDatatypeDecl Transform(Microsoft.Dafny.CoDatatypeDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.OpaqueTypeDecl Transform(Microsoft.Dafny.OpaqueTypeDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.NewtypeDecl Transform(Microsoft.Dafny.NewtypeDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual ValuetypeDecl Transform(ValuetypeDecl value)
        {
            return new ValuetypeDecl(value.Name, TransformUnion(value.EnclosingModuleDefinition), value.TypeParameterCount, value.TypeTester, value.TypeCreator);
        }

        public virtual TypeSynonymDecl Transform(TypeSynonymDecl value)
        {
            return new TypeSynonymDecl(value.Tok, value.Name, value.Characteristics, value.TypeArgs, TransformUnion(value.EnclosingModuleDefinition), value.Rhs, value.Attributes);
        }

        public virtual SubsetTypeDecl Transform(SubsetTypeDecl value)
        {
            return new SubsetTypeDecl(value.Tok, value.Name, value.Characteristics, value.TypeArgs, TransformUnion(value.EnclosingModuleDefinition), value.BoundVar, value.Constraint, value.WitnessKind, value.Witness, value.Attributes);
        }

        public virtual Microsoft.Dafny.NonNullTypeDecl Transform(Microsoft.Dafny.NonNullTypeDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual InternalTypeSynonymDecl Transform(InternalTypeSynonymDecl value)
        {
            return new InternalTypeSynonymDecl(value.Tok, value.Name, value.Characteristics, value.TypeArgs, TransformUnion(value.EnclosingModuleDefinition), value.Rhs, value.Attributes);
        }

        public virtual AliasModuleDecl Transform(AliasModuleDecl value)
        {
            return new AliasModuleDecl(value.TargetQId, value.Tok, TransformUnion(value.EnclosingModuleDefinition), value.Opened, value.Exports);
        }

        public virtual AbstractModuleDecl Transform(AbstractModuleDecl value)
        {
            return new AbstractModuleDecl(value.QId, value.Tok, TransformUnion(value.EnclosingModuleDefinition), value.Opened, value.Exports);
        }

        public virtual ModuleExportDecl Transform(ModuleExportDecl value)
        {
            return new ModuleExportDecl(value.Tok, TransformUnion(value.EnclosingModuleDefinition), value.Exports, value.Extends, value.ProvideAll, value.RevealAll, value.IsDefault, value.IsRefining);
        }

        public virtual Program Transform(Program value)
        {
            return new Program(value.Name, TransformUnion(value.DefaultModule), value.BuiltIns, value.Reporter);
        }
    }
}