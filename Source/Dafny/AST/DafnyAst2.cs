namespace Microsoft.Dafny.V2
{
    using System;
    using System.Linq;
    using Microsoft.Dafny;
    using System.Collections.Generic;
    using Microsoft.Boogie;

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
            return new LiteralModuleDecl(TransformUnion(value.module), TransformUnion(value.parent));
        }

        public virtual ModuleDefinition Transform(ModuleDefinition value)
        {
            return new ModuleDefinition(value.tok, value.name, value.prefixIds, value.isAbstract, value.isFacade, value.refinementQId, TransformUnion(value.parent), value.attributes, value.isBuiltinName, value.isToBeVerified, value.isToBeCompiled, value.TopLevelDecls.Select(TransformUnion).ToList());
        }

        public virtual ClassDecl Transform(ClassDecl value)
        {
            return new ClassDecl(value.tok, value.name, TransformUnion(value.module), value.typeArgs, value.members.Select(TransformUnion).ToList(), value.attributes, value.isRefining, value.traits);
        }

        public virtual Function Transform(Function value)
        {
            return new Function(value.tok, value.name, value.hasStaticKeyword, value.isGhost, value.typeArgs, value.formals, value.result, value.resultType, value.req, value.reads, value.ens, value.decreases, value.body, value.byMethodTok, TransformUnion(value.byMethodBody), value.attributes, value.signatureEllipsis);
        }

        public virtual BlockStmt Transform(BlockStmt value)
        {
            return new BlockStmt(value.tok, value.endTok, value.body.Select(TransformUnion).ToList());
        }

        public virtual AssertStmt Transform(AssertStmt value)
        {
            return new AssertStmt(value.tok, value.endTok, value.expr, TransformUnion(value.proof), value.label, value.attrs);
        }

        public virtual VarDeclStmt Transform(VarDeclStmt value)
        {
            return new VarDeclStmt(value.tok, value.endTok, value.locals, TransformUnion(value.update));
        }

        public virtual Microsoft.Dafny.UpdateStmt Transform(Microsoft.Dafny.V2.UpdateStmt value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual IfStmt Transform(IfStmt value)
        {
            return new IfStmt(value.tok, value.endTok, value.isBindingGuard, value.guard, TransformUnion(value.thn), TransformUnion(value.els));
        }

        public virtual AlternativeStmt Transform(AlternativeStmt value)
        {
            return new AlternativeStmt(value.tok, value.endTok, value.alternatives.Select(Transform).ToList(), value.usesOptionalBraces);
        }

        public virtual GuardedAlternative Transform(GuardedAlternative value)
        {
            return new GuardedAlternative(value.tok, value.isBindingGuard, value.guard, value.body.Select(TransformUnion).ToList());
        }

        public virtual WhileStmt Transform(WhileStmt value)
        {
            return new WhileStmt(value.tok, value.endTok, value.guard, value.invariants, value.decreases, value.mod, TransformUnion(value.body));
        }

        public virtual RefinedWhileStmt Transform(RefinedWhileStmt value)
        {
            return new RefinedWhileStmt(value.tok, value.endTok, value.guard, value.invariants, value.decreases, value.mod, TransformUnion(value.body));
        }

        public virtual ForLoopStmt Transform(ForLoopStmt value)
        {
            return new ForLoopStmt(value.tok, value.endTok, value.loopIndexVariable, value.start, value.end, value.goingUp, value.invariants, value.decreases, value.mod, TransformUnion(value.body), value.attrs);
        }

        public virtual AlternativeLoopStmt Transform(AlternativeLoopStmt value)
        {
            return new AlternativeLoopStmt(value.tok, value.endTok, value.invariants, value.decreases, value.mod, value.alternatives.Select(Transform).ToList(), value.usesOptionalBraces);
        }

        public virtual ForallStmt Transform(ForallStmt value)
        {
            return new ForallStmt(value.tok, value.endTok, value.boundVars, value.attrs, value.range, value.ens, TransformUnion(value.body));
        }

        public virtual ModifyStmt Transform(ModifyStmt value)
        {
            return new ModifyStmt(value.tok, value.endTok, value.mod, value.attrs, TransformUnion(value.body));
        }

        public virtual CalcStmt Transform(CalcStmt value)
        {
            return new CalcStmt(value.tok, value.endTok, value.userSuppliedOp, value.lines, value.hints.Select(TransformUnion).ToList(), value.stepOps, value.attrs);
        }

        public virtual MatchStmt Transform(MatchStmt value)
        {
            return new MatchStmt(value.tok, value.endTok, value.source, value.cases.Select(Transform).ToList(), value.usesOptionalBraces, value.context);
        }

        public virtual MatchCaseStmt Transform(MatchCaseStmt value)
        {
            return new MatchCaseStmt(value.tok, value.ctor, value.arguments, value.body.Select(TransformUnion).ToList(), value.attrs);
        }

        public virtual NestedMatchStmt Transform(NestedMatchStmt value)
        {
            return new NestedMatchStmt(value.tok, value.endTok, value.source, value.cases.Select(Transform).ToList(), value.usesOptionalBraces, value.attrs);
        }

        public virtual NestedMatchCaseStmt Transform(NestedMatchCaseStmt value)
        {
            return new NestedMatchCaseStmt(value.tok, value.pat, value.body.Select(TransformUnion).ToList());
        }

        public virtual DividedBlockStmt Transform(DividedBlockStmt value)
        {
            return new DividedBlockStmt(value.tok, value.endTok, value.bodyInit.Select(TransformUnion).ToList(), value.separatorTok, value.bodyProper.Select(TransformUnion).ToList());
        }

        public virtual SpecialFunction Transform(SpecialFunction value)
        {
            return new SpecialFunction(value.tok, value.name, TransformUnion(value.module), value.hasStaticKeyword, value.isGhost, value.typeArgs, value.formals, value.resultType, value.req, value.reads, value.ens, value.decreases, value.body, value.attributes, value.signatureEllipsis);
        }

        public virtual Predicate Transform(Predicate value)
        {
            return new Predicate(value.tok, value.name, value.hasStaticKeyword, value.isGhost, value.typeArgs, value.formals, value.req, value.reads, value.ens, value.decreases, value.body, value.bodyOrigin, value.byMethodTok, TransformUnion(value.byMethodBody), value.attributes, value.signatureEllipsis);
        }

        public virtual Method Transform(Method value)
        {
            return new Method(value.tok, value.name, value.hasStaticKeyword, value.isGhost, value.typeArgs, value.ins, value.outs, value.req, value.mod, value.ens, value.decreases, TransformUnion(value.body), value.attributes, value.signatureEllipsis, value.isByMethod);
        }

        public virtual Lemma Transform(Lemma value)
        {
            return new Lemma(value.tok, value.name, value.hasStaticKeyword, value.typeArgs, value.ins, value.outs, value.req, value.mod, value.ens, value.decreases, TransformUnion(value.body), value.attributes, value.signatureEllipsis);
        }

        public virtual TwoStateLemma Transform(TwoStateLemma value)
        {
            return new TwoStateLemma(value.tok, value.name, value.hasStaticKeyword, value.typeArgs, value.ins, value.outs, value.req, value.mod, value.ens, value.decreases, TransformUnion(value.body), value.attributes, value.signatureEllipsis);
        }

        public virtual Constructor Transform(Constructor value)
        {
            return new Constructor(value.tok, value.name, value.isGhost, value.typeArgs, value.ins, value.req, value.mod, value.ens, value.decreases, Transform(value.body), value.attributes, value.signatureEllipsis);
        }

        public virtual PrefixLemma Transform(PrefixLemma value)
        {
            return new PrefixLemma(value.tok, value.name, value.hasStaticKeyword, value.typeArgs, value.k, value.ins, value.outs, value.req, value.mod, value.ens, value.decreases, TransformUnion(value.body), value.attributes, TransformUnion(value.extremeLemma));
        }

        public virtual LeastLemma Transform(LeastLemma value)
        {
            return new LeastLemma(value.tok, value.name, value.hasStaticKeyword, value.typeOfK, value.typeArgs, value.ins, value.outs, value.req, value.mod, value.ens, value.decreases, TransformUnion(value.body), value.attributes, value.signatureEllipsis);
        }

        public virtual GreatestLemma Transform(GreatestLemma value)
        {
            return new GreatestLemma(value.tok, value.name, value.hasStaticKeyword, value.typeOfK, value.typeArgs, value.ins, value.outs, value.req, value.mod, value.ens, value.decreases, TransformUnion(value.body), value.attributes, value.signatureEllipsis);
        }

        public virtual TraitDecl Transform(TraitDecl value)
        {
            return new TraitDecl(value.tok, value.name, TransformUnion(value.module), value.typeArgs, value.members.Select(TransformUnion).ToList(), value.attributes, value.isRefining, value.traits);
        }

        public virtual DefaultClassDecl Transform(DefaultClassDecl value)
        {
            return new DefaultClassDecl(TransformUnion(value.module), value.members.Select(TransformUnion).ToList());
        }

        public virtual ArrayClassDecl Transform(ArrayClassDecl value)
        {
            return new ArrayClassDecl(value.dims, TransformUnion(value.module), value.attrs);
        }

        public virtual ArrowTypeDecl Transform(ArrowTypeDecl value)
        {
            return new ArrowTypeDecl(value.tps, TransformUnion(value.req), TransformUnion(value.reads), TransformUnion(value.module), value.attributes);
        }

        public virtual IteratorDecl Transform(IteratorDecl value)
        {
            return new IteratorDecl(value.tok, value.name, TransformUnion(value.module), value.typeArgs, value.ins, value.outs, value.reads, value.mod, value.decreases, value.requires, value.ensures, value.yieldRequires, value.yieldEnsures, TransformUnion(value.body), value.attributes, value.signatureEllipsis);
        }

        public virtual IndDatatypeDecl Transform(IndDatatypeDecl value)
        {
            return new IndDatatypeDecl(value.tok, value.name, TransformUnion(value.module), value.typeArgs, value.ctors, value.members.Select(TransformUnion).ToList(), value.attributes, value.isRefining);
        }

        public virtual TupleTypeDecl Transform(TupleTypeDecl value)
        {
            return new TupleTypeDecl(value.argumentGhostness, TransformUnion(value.systemModule), value.attributes);
        }

        public virtual CoDatatypeDecl Transform(CoDatatypeDecl value)
        {
            return new CoDatatypeDecl(value.tok, value.name, TransformUnion(value.module), value.typeArgs, value.ctors, value.members.Select(TransformUnion).ToList(), value.attributes, value.isRefining);
        }

        public virtual OpaqueTypeDecl Transform(OpaqueTypeDecl value)
        {
            return new OpaqueTypeDecl(value.tok, value.name, TransformUnion(value.module), value.characteristics, value.typeArgs, value.members.Select(TransformUnion).ToList(), value.attributes, value.isRefining);
        }

        public virtual NewtypeDecl Transform(NewtypeDecl value)
        {
            return new NewtypeDecl(value.tok, value.name, TransformUnion(value.module), value.baseType, value.members.Select(TransformUnion).ToList(), value.attributes, value.isRefining);
        }

        public virtual ValuetypeDecl Transform(ValuetypeDecl value)
        {
            return new ValuetypeDecl(value.name, TransformUnion(value.module), value.typeParameterCount, value.typeTester, value.typeCreator);
        }

        public virtual TypeSynonymDecl Transform(TypeSynonymDecl value)
        {
            return new TypeSynonymDecl(value.tok, value.name, value.characteristics, value.typeArgs, TransformUnion(value.module), value.rhs, value.attributes);
        }

        public virtual SubsetTypeDecl Transform(SubsetTypeDecl value)
        {
            return new SubsetTypeDecl(value.tok, value.name, value.characteristics, value.typeArgs, TransformUnion(value.module), value.id, value.constraint, value.witnessKind, value.witness, value.attributes);
        }

        public virtual NonNullTypeDecl Transform(NonNullTypeDecl value)
        {
            return new NonNullTypeDecl(TransformUnion(value.cl));
        }

        public virtual InternalTypeSynonymDecl Transform(InternalTypeSynonymDecl value)
        {
            return new InternalTypeSynonymDecl(value.tok, value.name, value.characteristics, value.typeArgs, TransformUnion(value.module), value.rhs, value.attributes);
        }

        public virtual AliasModuleDecl Transform(AliasModuleDecl value)
        {
            return new AliasModuleDecl(value.path, value.name, TransformUnion(value.parent), value.opened, value.exports);
        }

        public virtual AbstractModuleDecl Transform(AbstractModuleDecl value)
        {
            return new AbstractModuleDecl(value.qid, value.name, TransformUnion(value.parent), value.opened, value.exports);
        }

        public virtual ModuleExportDecl Transform(ModuleExportDecl value)
        {
            return new ModuleExportDecl(value.tok, TransformUnion(value.parent), value.exports, value.extends, value.provideAll, value.revealAll, value.isDefault, value.isRefining);
        }

        public virtual Program Transform(Program value)
        {
            return new Program(value.name, TransformUnion(value.module), value.builtIns, value.reporter);
        }
    }
}