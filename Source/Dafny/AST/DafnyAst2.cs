namespace Microsoft.Dafny.V2
{
    using System;
    using System.Linq;
    using Microsoft.Dafny;
    using System.Collections.Generic;
    using IToken = Microsoft.Boogie.IToken;

    record UpdateStmt(IToken tok, IToken endTok, List<AssignmentRhs> rhss, Boolean mutate, IReadOnlyList<AssignmentRhs> Lhss2) : ConcreteUpdateStatement;

    class BackwardTransformer
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
              Microsoft.Dafny.Method subType => TransformUnion(subType),
              Microsoft.Dafny.Resolver.AmbiguousMemberDecl subType => subType,
          };

        public virtual Function TransformUnion(Function value) =>
          value switch
          {
              Microsoft.Dafny.SpecialFunction subType => Transform(subType),
              Microsoft.Dafny.Predicate subType => Transform(subType),
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

        public virtual Dafny.TypeSynonymDecl TransformUnion(Dafny.TypeSynonymDecl value) =>
          value switch
          {
              Microsoft.Dafny.SubsetTypeDecl subType => TransformUnion(subType),
          };

        public virtual SubsetTypeDecl TransformUnion(SubsetTypeDecl value) =>
          value switch
          {
              Microsoft.Dafny.NonNullTypeDecl subType => Transform(subType),
          };

        public virtual Method TransformUnion(Method value) =>
          value switch
          {
              Microsoft.Dafny.Lemma subType => Transform(subType),
              Microsoft.Dafny.TwoStateLemma subType => Transform(subType),
              Microsoft.Dafny.Constructor subType => Transform(subType),
              Microsoft.Dafny.PrefixLemma subType => Transform(subType),
              Microsoft.Dafny.ExtremeLemma subType => TransformUnion(subType),
          };

        public virtual ExtremeLemma TransformUnion(ExtremeLemma value) =>
          value switch
          {
              Microsoft.Dafny.LeastLemma subType => Transform(subType),
              Microsoft.Dafny.GreatestLemma subType => Transform(subType),
          };

        public virtual Statement TransformUnion(Statement value) =>
          value switch
          {
              Microsoft.Dafny.PredicateStmt subType => TransformUnion(subType),
              Microsoft.Dafny.PrintStmt subType => subType,
              Microsoft.Dafny.RevealStmt subType => subType,
              Microsoft.Dafny.BreakStmt subType => subType,
              Microsoft.Dafny.ProduceStmt subType => subType,
              Microsoft.Dafny.VarDeclStmt subType => Transform(subType),
              Microsoft.Dafny.VarDeclPattern subType => subType,
              Microsoft.Dafny.ConcreteUpdateStatement subType => TransformUnion(subType),
              Microsoft.Dafny.AssignStmt subType => subType,
              Microsoft.Dafny.CallStmt subType => subType,
              Microsoft.Dafny.BlockStmt subType => TransformUnion(subType),
              Microsoft.Dafny.IfStmt subType => Transform(subType),
              Microsoft.Dafny.AlternativeStmt subType => Transform(subType),
              Microsoft.Dafny.LoopStmt subType => TransformUnion(subType),
              Microsoft.Dafny.ForallStmt subType => Transform(subType),
              Microsoft.Dafny.ModifyStmt subType => Transform(subType),
              Microsoft.Dafny.CalcStmt subType => Transform(subType),
              Microsoft.Dafny.MatchStmt subType => Transform(subType),
              Microsoft.Dafny.SkeletonStatement subType => subType,
              Microsoft.Dafny.ConcreteSyntaxStatement subType => TransformUnion(subType),
          };

        public virtual PredicateStmt TransformUnion(PredicateStmt value) =>
          value switch
          {
              Microsoft.Dafny.AssertStmt subType => Transform(subType),
              Microsoft.Dafny.ExpectStmt subType => subType,
              Microsoft.Dafny.AssumeStmt subType => subType,
          };

        public virtual ConcreteUpdateStatement TransformUnion(ConcreteUpdateStatement value) =>
          value switch
          {
              Microsoft.Dafny.AssignSuchThatStmt subType => subType,
              Microsoft.Dafny.UpdateStmt subType => subType,
              Microsoft.Dafny.AssignOrReturnStmt subType => subType,
          };

        public virtual BlockStmt TransformUnion(BlockStmt value) =>
          value switch
          {
              Microsoft.Dafny.DividedBlockStmt subType => Transform(subType),
          };

        public virtual LoopStmt TransformUnion(LoopStmt value) =>
          value switch
          {
              Microsoft.Dafny.OneBodyLoopStmt subType => TransformUnion(subType),
              Microsoft.Dafny.AlternativeLoopStmt subType => Transform(subType),
          };

        public virtual OneBodyLoopStmt TransformUnion(OneBodyLoopStmt value) =>
          value switch
          {
              Microsoft.Dafny.WhileStmt subType => TransformUnion(subType),
              Microsoft.Dafny.ForLoopStmt subType => Transform(subType),
          };

        public virtual WhileStmt TransformUnion(WhileStmt value) =>
          value switch
          {
              Microsoft.Dafny.RefinedWhileStmt subType => Transform(subType),
          };

        public virtual ConcreteSyntaxStatement TransformUnion(ConcreteSyntaxStatement value) =>
          value switch
          {
              Microsoft.Dafny.NestedMatchStmt subType => Transform(subType),
          };

        public virtual LiteralModuleDecl Transform(LiteralModuleDecl value)
        {
            return new LiteralModuleDecl(TransformUnion(value.ModuleDef), TransformUnion(value.EnclosingModuleDefinition));
        }

        public virtual ModuleDefinition Transform(ModuleDefinition value)
        {
            return new ModuleDefinition(value.Tok, value.Name, value.PrefixIds, value.IsAbstract, value.IsFacade, value.RefinementQId, TransformUnion(value.EnclosingModule), value.Attributes, value.IsBuiltinName, value.IsToBeVerified, value.IsToBeCompiled, value.TopLevelDecls.Select(TransformUnion).ToList());
        }

        public virtual ClassDecl Transform(ClassDecl value)
        {
            return new ClassDecl(value.Tok, value.Name, TransformUnion(value.EnclosingModuleDefinition), value.TypeArgs, value.Members.Select(TransformUnion).ToList(), value.Attributes, value.IsRefining, value.ParentTraits);
        }

        public virtual Dafny.Function Transform(Dafny.Function value)
        {
            return new Dafny.Function(value.Tok, value.Name, value.HasStaticKeyword, value.IsGhost, value.TypeArgs, value.Formals, value.Result, value.ResultType, value.Req, value.Reads, value.Ens, value.Decreases, value.Body, value.ByMethodTok, TransformUnion(value.ByMethodBody), value.Attributes, value.SignatureEllipsis);
        }

        public virtual BlockStmt Transform(BlockStmt value)
        {
            return new BlockStmt(value.Tok, value.EndTok, value.Body.Select(TransformUnion).ToList());
        }

        public virtual AssertStmt Transform(AssertStmt value)
        {
            return new AssertStmt(value.Tok, value.EndTok, value.Expr, TransformUnion(value.Proof), value.Label, value.Attributes);
        }

        public virtual VarDeclStmt Transform(VarDeclStmt value)
        {
            return new VarDeclStmt(value.Tok, value.EndTok, value.Locals, TransformUnion(value.Update));
        }

        public virtual Microsoft.Dafny.UpdateStmt Transform(Microsoft.Dafny.V2.UpdateStmt value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual IfStmt Transform(IfStmt value)
        {
            return new IfStmt(value.Tok, value.EndTok, value.IsBindingGuard, value.Guard, TransformUnion(value.Thn), TransformUnion(value.Els));
        }

        public virtual AlternativeStmt Transform(AlternativeStmt value)
        {
            return new AlternativeStmt(value.Tok, value.EndTok, value.Alternatives.Select(Transform).ToList(), value.UsesOptionalBraces);
        }

        public virtual GuardedAlternative Transform(GuardedAlternative value)
        {
            return new GuardedAlternative(value.Tok, value.IsBindingGuard, value.Guard, value.Body.Select(TransformUnion).ToList());
        }

        public virtual WhileStmt Transform(WhileStmt value)
        {
            return new WhileStmt(value.Tok, value.EndTok, value.Guard, value.Invariants, value.Decreases, value.Mod, TransformUnion(value.Body));
        }

        public virtual RefinedWhileStmt Transform(RefinedWhileStmt value)
        {
            return new RefinedWhileStmt(value.Tok, value.EndTok, value.Guard, value.Invariants, value.Decreases, value.Mod, TransformUnion(value.Body));
        }

        public virtual ForLoopStmt Transform(ForLoopStmt value)
        {
            return new ForLoopStmt(value.Tok, value.EndTok, value.LoopIndex, value.Start, value.End, value.GoingUp, value.Invariants, value.Decreases, value.Mod, TransformUnion(value.Body), value.Attributes);
        }

        public virtual AlternativeLoopStmt Transform(AlternativeLoopStmt value)
        {
            return new AlternativeLoopStmt(value.Tok, value.EndTok, value.Invariants, value.Decreases, value.Mod, value.Alternatives.Select(Transform).ToList(), value.UsesOptionalBraces);
        }

        public virtual ForallStmt Transform(ForallStmt value)
        {
            return new ForallStmt(value.Tok, value.EndTok, value.BoundVars, value.Attributes, value.Range, value.Ens, TransformUnion(value.Body));
        }

        public virtual ModifyStmt Transform(ModifyStmt value)
        {
            return new ModifyStmt(value.Tok, value.EndTok, value.Mod, value.Attributes, TransformUnion(value.Body));
        }

        public virtual CalcStmt Transform(CalcStmt value)
        {
            return new CalcStmt(value.Tok, value.EndTok, value.UserSuppliedOp, value.Lines, value.Hints.Select(TransformUnion).ToList(), value.StepOps, value.Attributes);
        }

        public virtual MatchStmt Transform(MatchStmt value)
        {
            return new MatchStmt(value.Tok, value.EndTok, value.Source, value.Cases.Select(Transform).ToList(), value.UsesOptionalBraces, value.Context);
        }

        public virtual MatchCaseStmt Transform(MatchCaseStmt value)
        {
            return new MatchCaseStmt(value.Tok, value.Ctor, value.Arguments, value.Body.Select(TransformUnion).ToList(), value.Attributes);
        }

        public virtual NestedMatchStmt Transform(NestedMatchStmt value)
        {
            return new NestedMatchStmt(value.Tok, value.EndTok, value.Source, value.Cases.Select(Transform).ToList(), value.UsesOptionalBraces, value.Attributes);
        }

        public virtual NestedMatchCaseStmt Transform(NestedMatchCaseStmt value)
        {
            return new NestedMatchCaseStmt(value.Tok, value.Pat, value.Body.Select(TransformUnion).ToList());
        }

        public virtual DividedBlockStmt Transform(DividedBlockStmt value)
        {
            return new DividedBlockStmt(value.Tok, value.EndTok, value.BodyInit.Select(TransformUnion).ToList(), value.SeparatorTok, value.BodyProper.Select(TransformUnion).ToList());
        }

        public virtual SpecialFunction Transform(SpecialFunction value)
        {
            return new SpecialFunction(value.Tok, value.Name, TransformUnion(value.Module), value.HasStaticKeyword, value.IsGhost, value.TypeArgs, value.Formals, value.ResultType, value.Req, value.Reads, value.Ens, value.Decreases, value.Body, value.Attributes, value.SignatureEllipsis);
        }

        public virtual Predicate Transform(Predicate value)
        {
            return new Predicate(value.Tok, value.Name, value.HasStaticKeyword, value.IsGhost, value.TypeArgs, value.Formals, value.Req, value.Reads, value.Ens, value.Decreases, value.Body, value.BodyOrigin, value.ByMethodTok, TransformUnion(value.ByMethodBody), value.Attributes, value.SignatureEllipsis);
        }

        public virtual Method Transform(Method value)
        {
            return new Method(value.Tok, value.Name, value.HasStaticKeyword, value.IsGhost, value.TypeArgs, value.Ins, value.Outs, value.Req, value.Mod, value.Ens, value.Decreases, TransformUnion(value.Body), value.Attributes, value.SignatureEllipsis, value.IsByMethod);
        }

        public virtual Lemma Transform(Lemma value)
        {
            return new Lemma(value.Tok, value.Name, value.HasStaticKeyword, value.TypeArgs, value.Ins, value.Outs, value.Req, value.Mod, value.Ens, value.Decreases, TransformUnion(value.Body), value.Attributes, value.SignatureEllipsis);
        }

        public virtual TwoStateLemma Transform(TwoStateLemma value)
        {
            return new TwoStateLemma(value.Tok, value.Name, value.HasStaticKeyword, value.TypeArgs, value.Ins, value.Outs, value.Req, value.Mod, value.Ens, value.Decreases, TransformUnion(value.Body), value.Attributes, value.SignatureEllipsis);
        }

        public virtual Constructor Transform(Constructor value)
        {
            return new Constructor(value.Tok, value.Name, value.IsGhost, value.TypeArgs, value.Ins, value.Req, value.Mod, value.Ens, value.Decreases, Transform(value.Body), value.Attributes, value.SignatureEllipsis);
        }

        public virtual PrefixLemma Transform(PrefixLemma value)
        {
            return new PrefixLemma(value.Tok, value.Name, value.HasStaticKeyword, value.TypeArgs, value.K, value.Ins, value.Outs, value.Req, value.Mod, value.Ens, value.Decreases, TransformUnion(value.Body), value.Attributes, TransformUnion(value.ExtremeLemma));
        }

        public virtual LeastLemma Transform(LeastLemma value)
        {
            return new LeastLemma(value.Tok, value.Name, value.HasStaticKeyword, value.TypeOfK, value.TypeArgs, value.Ins, value.Outs, value.Req, value.Mod, value.Ens, value.Decreases, TransformUnion(value.Body), value.Attributes, value.SignatureEllipsis);
        }

        public virtual GreatestLemma Transform(GreatestLemma value)
        {
            return new GreatestLemma(value.Tok, value.Name, value.HasStaticKeyword, value.TypeOfK, value.TypeArgs, value.Ins, value.Outs, value.Req, value.Mod, value.Ens, value.Decreases, TransformUnion(value.Body), value.Attributes, value.SignatureEllipsis);
        }

        public virtual TraitDecl Transform(TraitDecl value)
        {
            return new TraitDecl(value.Tok, value.Name, TransformUnion(value.EnclosingModuleDefinition), value.TypeArgs, value.Members.Select(TransformUnion).ToList(), value.Attributes, value.IsRefining, value.Traits);
        }

        public virtual DefaultClassDecl Transform(DefaultClassDecl value)
        {
            return new DefaultClassDecl(TransformUnion(value.EnclosingModuleDefinition), value.Members.Select(TransformUnion).ToList());
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
            return new IteratorDecl(value.Tok, value.Name, TransformUnion(value.EnclosingModuleDefinition), value.TypeArgs, value.Ins, value.Outs, value.Reads, value.Mod, value.Decreases, value.Requires, value.Ensures, value.YieldRequires, value.YieldEnsures, TransformUnion(value.Body), value.Attributes, value.SignatureEllipsis);
        }

        public virtual IndDatatypeDecl Transform(IndDatatypeDecl value)
        {
            return new IndDatatypeDecl(value.Tok, value.Name, TransformUnion(value.EnclosingModuleDefinition), value.TypeArgs, value.Ctors, value.Members.Select(TransformUnion).ToList(), value.Attributes, value.IsRefining);
        }

        public virtual TupleTypeDecl Transform(TupleTypeDecl value)
        {
            return new TupleTypeDecl(value.ArgumentGhostness, TransformUnion(value.EnclosingModuleDefinition), value.Attributes);
        }

        public virtual CoDatatypeDecl Transform(CoDatatypeDecl value)
        {
            return new CoDatatypeDecl(value.Tok, value.Name, TransformUnion(value.EnclosingModuleDefinition), value.TypeArgs, value.Ctors, value.Members.Select(TransformUnion).ToList(), value.Attributes, value.IsRefining);
        }

        public virtual OpaqueTypeDecl Transform(OpaqueTypeDecl value)
        {
            return new OpaqueTypeDecl(value.Tok, value.Name, TransformUnion(value.EnclosingModuleDefinition), value.Characteristics, value.TypeArgs, value.Members.Select(TransformUnion).ToList(), value.Attributes, value.IsRefining);
        }

        public virtual NewtypeDecl Transform(NewtypeDecl value)
        {
            return new NewtypeDecl(value.Tok, value.Name, TransformUnion(value.EnclosingModuleDefinition), value.BaseType, value.Members.Select(TransformUnion).ToList(), value.Attributes, value.IsRefining);
        }

        public virtual ValuetypeDecl Transform(ValuetypeDecl value)
        {
            return new ValuetypeDecl(value.Name, TransformUnion(value.EnclosingModuleDefinition), value.TypeParameterCount, value.TypeTester, value.TypeCreator);
        }

        public virtual Dafny.TypeSynonymDecl Transform(Dafny.TypeSynonymDecl value)
        {
            return new Dafny.TypeSynonymDecl(value.Tok, value.Name, value.Characteristics, value.TypeArgs, TransformUnion(value.Module), value.Rhs, value.Attributes);
        }

        public virtual SubsetTypeDecl Transform(SubsetTypeDecl value)
        {
            return new SubsetTypeDecl(value.Tok, value.Name, value.Characteristics, value.TypeArgs, TransformUnion(value.Module), value.BoundVar, value.Constraint, value.WitnessKind, value.Witness, value.Attributes);
        }

        public virtual NonNullTypeDecl Transform(NonNullTypeDecl value)
        {
            return new NonNullTypeDecl(TransformUnion(value.ClassDecl));
        }

        public virtual InternalTypeSynonymDecl Transform(InternalTypeSynonymDecl value)
        {
            return new InternalTypeSynonymDecl(value.Tok, value.Name, value.Characteristics, value.TypeArgs, TransformUnion(value.Module), value.Rhs, value.Attributes);
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

        public virtual Dafny.Program Transform(Dafny.Program value)
        {
            return new Dafny.Program(value.Name, TransformUnion(value.DefaultModule), value.BuiltIns, value.Reporter);
        }
    }
}