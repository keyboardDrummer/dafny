using Microsoft.Dafny;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Diagnostics.Contracts;
using IToken = Microsoft.Boogie.IToken;
using System.Numerics;
using Microsoft.Boogie;

namespace Microsoft.Dafny.V2
{
    public class Method : MemberDecl, TypeParameter.ParentType, IMethodCodeContext
    {
        public override string WhatKind { get { return "method"; } }
        public bool SignatureIsOmitted { get { return SignatureEllipsis != null; } }
        public readonly IToken SignatureEllipsis;
        public readonly bool IsByMethod;
        public bool MustReverify;
        public bool IsEntryPoint = false;
        public readonly List<TypeParameter> TypeArgs;
        public readonly List<Formal> Ins;
        public readonly List<Formal> Outs;
        public readonly Specification<FrameExpression> Mod;
        public readonly Specification<Expression> Decreases;
        private BlockStmt methodBody;  // Body is readonly after construction, except for any kind of rewrite that may take place around the time of resolution (note that "methodBody" is a "DividedBlockStmt" for any "Method" that is a "Constructor")
        public bool IsRecursive;  // filled in during resolution
        public bool IsTailRecursive;  // filled in during resolution
        public readonly ISet<IVariable> AssignedAssumptionVariables = new HashSet<IVariable>();
        public Method OverriddenMethod;
        public Method Original => OverriddenMethod == null ? this : OverriddenMethod.Original;
        public override bool IsOverrideThatAddsBody => base.IsOverrideThatAddsBody && Body != null;
        private static BlockStmt emptyBody = new BlockStmt(Token.NoToken, Token.NoToken, new List<Statement>());

        public override IEnumerable<Expression> SubExpressions
        {
            get
            {
                foreach (var formal in Ins.Where(f => f.DefaultValue != null))
                {
                    yield return formal.DefaultValue;
                }
                // foreach (var e in Req)
                // {
                //     yield return e.E;
                // }
                foreach (var e in Mod.Expressions)
                {
                    yield return e.E;
                }
                // foreach (var e in Ens)
                // {
                //     yield return e.E;
                // }
                foreach (var e in Decreases.Expressions)
                {
                    yield return e;
                }
            }
        }

        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(cce.NonNullElements(TypeArgs));
            Contract.Invariant(cce.NonNullElements(Ins));
            Contract.Invariant(cce.NonNullElements(Outs));
            Contract.Invariant(Mod != null);
            Contract.Invariant(Decreases != null);
        }

        public Method(IToken tok, string name,
                      bool hasStaticKeyword, bool isGhost,
                      [Captured] List<TypeParameter> typeArgs,
                      [Captured] List<Formal> ins, [Captured] List<Formal> outs,
                      [Captured] List<AttributedExpression> req, [Captured] Specification<FrameExpression> mod,
                      [Captured] List<AttributedExpression> ens,
                      [Captured] Specification<Expression> decreases,
                      [Captured] BlockStmt body,
                      Attributes attributes, IToken signatureEllipsis, bool isByMethod = false)
          : base(tok, name, hasStaticKeyword, isGhost, attributes, signatureEllipsis != null)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(cce.NonNullElements(typeArgs));
            Contract.Requires(cce.NonNullElements(ins));
            Contract.Requires(cce.NonNullElements(outs));
            Contract.Requires(cce.NonNullElements(req));
            Contract.Requires(mod != null);
            Contract.Requires(cce.NonNullElements(ens));
            Contract.Requires(decreases != null);
            this.TypeArgs = typeArgs;
            this.Ins = ins;
            this.Outs = outs;
            this.Mod = mod;
            this.Decreases = decreases;
            this.methodBody = body;
            this.SignatureEllipsis = signatureEllipsis;
            this.IsByMethod = isByMethod;
            MustReverify = false;
        }

        bool ICodeContext.IsGhost { get { return this.IsGhost; } }
        List<TypeParameter> ICodeContext.TypeArgs { get { return this.TypeArgs; } }
        List<Formal> ICodeContext.Ins { get { return this.Ins; } }
        List<Formal> IMethodCodeContext.Outs { get { return this.Outs; } }
        Specification<FrameExpression> IMethodCodeContext.Modifies { get { return Mod; } }
        IToken ICallable.Tok { get { return this.Tok; } }
        string ICallable.NameRelativeToModule
        {
            get
            {
                if (EnclosingClass is DefaultClassDecl)
                {
                    return Name;
                }
                else
                {
                    return EnclosingClass.Name + "." + Name;
                }
            }
        }
        Specification<Expression> ICallable.Decreases { get { return this.Decreases; } }
        bool _inferredDecr;
        bool ICallable.InferredDecreases
        {
            set { _inferredDecr = value; }
            get { return _inferredDecr; }
        }

        public virtual bool AllowsAllocation => true;

        ModuleDefinition ICodeContext.EnclosingModule
        {
            get
            {
                Contract.Assert(this.EnclosingClass != null);  // this getter is supposed to be called only after signature-resolution is complete
                return this.EnclosingClass.EnclosingModuleDefinition;
            }
        }
        bool ICodeContext.MustReverify { get { return this.MustReverify; } }
        public bool AllowsNontermination
        {
            get
            {
                return Contract.Exists(Decreases.Expressions, e => e is WildcardExpr);
            }
        }

        public override string CompileName
        {
            get
            {
                var nm = base.CompileName;
                if (nm == Dafny.Compiler.DefaultNameMain && IsStatic && !IsEntryPoint)
                {
                    // for a static method that is named "Main" but is not a legal "Main" method,
                    // change its name.
                    nm = EnclosingClass.Name + "_" + nm;
                }
                return nm;
            }
        }

        public BlockStmt Body
        {
            get
            {
                // Lemma from included files do not need to be resolved and translated
                // so we return emptyBody. This is to speed up resolvor and translator.
                if (methodBody != null && IsLemmaLike && this.Tok is IncludeToken && !DafnyOptions.O.VerifyAllModules)
                {
                    return Method.emptyBody;
                }
                else
                {
                    return methodBody;
                }
            }
            set
            {
                methodBody = value;
            }
        }

        public bool IsLemmaLike => this is Lemma || this is TwoStateLemma || this is ExtremeLemma || this is PrefixLemma;

        public BlockStmt BodyForRefinement
        {
            // For refinement, we still need to merge in the body
            // a lemma that is in the refinement base that is defined
            // in a include file.
            get
            {
                return methodBody;
            }
        }
    }

    public class Lemma : Method
    {
        public override string WhatKind { get { return "lemma"; } }
        public Lemma(IToken tok, string name,
                     bool hasStaticKeyword,
                     [Captured] List<TypeParameter> typeArgs,
                     [Captured] List<Formal> ins, [Captured] List<Formal> outs,
                     [Captured] List<AttributedExpression> req, [Captured] Specification<FrameExpression> mod,
                     [Captured] List<AttributedExpression> ens,
                     [Captured] Specification<Expression> decreases,
                     [Captured] BlockStmt body,
                     Attributes attributes, IToken signatureEllipsis)
          : base(tok, name, hasStaticKeyword, true, typeArgs, ins, outs, req, mod, ens, decreases, body, attributes, signatureEllipsis)
        {
        }

        public override bool AllowsAllocation => false;
    }

    public class TwoStateLemma : Method
    {
        public override string WhatKind { get { return "twostate lemma"; } }
        public TwoStateLemma(IToken tok, string name,
                     bool hasStaticKeyword,
                     [Captured] List<TypeParameter> typeArgs,
                     [Captured] List<Formal> ins, [Captured] List<Formal> outs,
                     [Captured] List<AttributedExpression> req,
                     [Captured] Specification<FrameExpression> mod,
                     [Captured] List<AttributedExpression> ens,
                     [Captured] Specification<Expression> decreases,
                     [Captured] BlockStmt body,
                     Attributes attributes, IToken signatureEllipsis)
          : base(tok, name, hasStaticKeyword, true, typeArgs, ins, outs, req, mod, ens, decreases, body, attributes, signatureEllipsis)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(typeArgs != null);
            Contract.Requires(ins != null);
            Contract.Requires(outs != null);
            Contract.Requires(req != null);
            Contract.Requires(mod != null);
            Contract.Requires(ens != null);
            Contract.Requires(decreases != null);
        }

        public override bool AllowsAllocation => false;
    }

    public class Constructor : Method
    {

        public DividedBlockStmt DividedBody { get; private set; }

        public override string WhatKind { get { return "constructor"; } }
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(Body == null || Body is DividedBlockStmt);
        }
        public List<Statement> BodyInit
        {  // first part of Body's statements
            get
            {
                if (Body == null)
                {
                    return null;
                }
                else
                {
                    return ((DividedBlockStmt)Body).BodyInit;
                }
            }
        }
        public List<Statement> BodyProper
        {  // second part of Body's statements
            get
            {
                if (Body == null)
                {
                    return null;
                }
                else
                {
                    return ((DividedBlockStmt)Body).BodyProper;
                }
            }
        }
        public Constructor(IToken tok, string name,
                      bool isGhost,
                      List<TypeParameter> typeArgs,
                      List<Formal> ins,
                      List<AttributedExpression> req, [Captured] Specification<FrameExpression> mod,
                      List<AttributedExpression> ens,
                      Specification<Expression> decreases,
                      DividedBlockStmt dividedBody,
                      Attributes attributes, IToken signatureEllipsis)
          : base(tok, name, false, isGhost, typeArgs, ins, new List<Formal>(), req, mod, ens, decreases, dividedBody, attributes, signatureEllipsis)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(cce.NonNullElements(typeArgs));
            Contract.Requires(cce.NonNullElements(ins));
            Contract.Requires(cce.NonNullElements(req));
            Contract.Requires(mod != null);
            Contract.Requires(cce.NonNullElements(ens));
            Contract.Requires(decreases != null);
            this.DividedBody = dividedBody;
        }

        public bool HasName
        {
            get
            {
                return Name != "_ctor";
            }
        }
    }

    public abstract class ExtremeLemma : Method
    {
        public readonly Microsoft.Dafny.ExtremePredicate.KType TypeOfK;
        public bool KNat
        {
            get
            {
                return TypeOfK == ExtremePredicate.KType.Nat;
            }
        }
        public PrefixLemma PrefixLemma;  // filled in during resolution (name registration)

        public ExtremeLemma(IToken tok, string name,
                             bool hasStaticKeyword, Microsoft.Dafny.ExtremePredicate.KType typeOfK,
                             List<TypeParameter> typeArgs,
                             List<Formal> ins, [Captured] List<Formal> outs,
                             List<AttributedExpression> req, [Captured] Specification<FrameExpression> mod,
                             List<AttributedExpression> ens,
                             Specification<Expression> decreases,
                             BlockStmt body,
                             Attributes attributes, IToken signatureEllipsis)
          : base(tok, name, hasStaticKeyword, true, typeArgs, ins, outs, req, mod, ens, decreases, body, attributes, signatureEllipsis)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(cce.NonNullElements(typeArgs));
            Contract.Requires(cce.NonNullElements(ins));
            Contract.Requires(cce.NonNullElements(outs));
            Contract.Requires(cce.NonNullElements(req));
            Contract.Requires(mod != null);
            Contract.Requires(cce.NonNullElements(ens));
            Contract.Requires(decreases != null);
            TypeOfK = typeOfK;
        }

        public override bool AllowsAllocation => false;
    }

    /// <summary>
    /// A PrefixLemma is the inductive unrolling M# implicitly declared for every extreme lemma M.
    /// </summary>
    public class PrefixLemma : Method
    {
        public override string WhatKind { get { return "prefix lemma"; } }
        public readonly Formal K;
        public readonly ExtremeLemma ExtremeLemma;
        public PrefixLemma(IToken tok, string name, bool hasStaticKeyword,
                           List<TypeParameter> typeArgs, Formal k, List<Formal> ins, List<Formal> outs,
                           List<AttributedExpression> req, Specification<FrameExpression> mod, List<AttributedExpression> ens, Specification<Expression> decreases,
                           BlockStmt body, Attributes attributes, ExtremeLemma extremeLemma)
          : base(tok, name, hasStaticKeyword, true, typeArgs, ins, outs, req, mod, ens, decreases, body, attributes, null)
        {
            Contract.Requires(k != null);
            Contract.Requires(ins != null && 1 <= ins.Count && ins[0] == k);
            Contract.Requires(extremeLemma != null);
            K = k;
            ExtremeLemma = extremeLemma;
        }

        public override bool AllowsAllocation => false;
    }

    public class LeastLemma : ExtremeLemma
    {
        public override string WhatKind { get { return "least lemma"; } }

        public LeastLemma(IToken tok, string name,
                              bool hasStaticKeyword, Microsoft.Dafny.ExtremePredicate.KType typeOfK,
                              List<TypeParameter> typeArgs,
                              List<Formal> ins, [Captured] List<Formal> outs,
                              List<AttributedExpression> req, [Captured] Specification<FrameExpression> mod,
                              List<AttributedExpression> ens,
                              Specification<Expression> decreases,
                              BlockStmt body,
                              Attributes attributes, IToken signatureEllipsis)
          : base(tok, name, hasStaticKeyword, typeOfK, typeArgs, ins, outs, req, mod, ens, decreases, body, attributes, signatureEllipsis)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(cce.NonNullElements(typeArgs));
            Contract.Requires(cce.NonNullElements(ins));
            Contract.Requires(cce.NonNullElements(outs));
            Contract.Requires(cce.NonNullElements(req));
            Contract.Requires(mod != null);
            Contract.Requires(cce.NonNullElements(ens));
            Contract.Requires(decreases != null);
        }
    }

    public class GreatestLemma : ExtremeLemma
    {
        public override string WhatKind { get { return "greatest lemma"; } }

        public GreatestLemma(IToken tok, string name,
                       bool hasStaticKeyword, Microsoft.Dafny.ExtremePredicate.KType typeOfK,
                       List<TypeParameter> typeArgs,
                       List<Formal> ins, [Captured] List<Formal> outs,
                       List<AttributedExpression> req, [Captured] Specification<FrameExpression> mod,
                       List<AttributedExpression> ens,
                       Specification<Expression> decreases,
                       BlockStmt body,
                       Attributes attributes, IToken signatureEllipsis)
          : base(tok, name, hasStaticKeyword, typeOfK, typeArgs, ins, outs, req, mod, ens, decreases, body, attributes, signatureEllipsis)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(cce.NonNullElements(typeArgs));
            Contract.Requires(cce.NonNullElements(ins));
            Contract.Requires(cce.NonNullElements(outs));
            Contract.Requires(cce.NonNullElements(req));
            Contract.Requires(mod != null);
            Contract.Requires(cce.NonNullElements(ens));
            Contract.Requires(decreases != null);
        }
    }

    class ForwardTransformer
    {
        public virtual Microsoft.Dafny.Attributes TransformUnion(Microsoft.Dafny.Attributes value) =>
        value switch
        {
            Microsoft.Dafny.UserSuppliedAttributes subType => subType,
        };

        public virtual Microsoft.Dafny.Type TransformUnion(Microsoft.Dafny.Type value) =>
        value switch
        {
            Microsoft.Dafny.ArtificialType subType => subType,
            Microsoft.Dafny.NonProxyType subType => TransformUnion(subType),
            Microsoft.Dafny.TypeProxy subType => TransformUnion(subType),
            Microsoft.Dafny.Resolver_IdentifierExpr.ResolverType subType => subType,
        };

        public virtual Microsoft.Dafny.NonProxyType TransformUnion(Microsoft.Dafny.NonProxyType value) =>
        value switch
        {
            Microsoft.Dafny.BasicType subType => subType,
            Microsoft.Dafny.SelfType subType => Transform(subType),
            Microsoft.Dafny.CollectionType subType => TransformUnion(subType),
            Microsoft.Dafny.UserDefinedType subType => TransformUnion(subType),
        };

        public virtual Microsoft.Dafny.UserDefinedType TransformUnion(Microsoft.Dafny.UserDefinedType value) =>
        value switch
        {
            Microsoft.Dafny.ArrowType subType => subType,
        };

        public virtual Microsoft.Dafny.CollectionType TransformUnion(Microsoft.Dafny.CollectionType value) =>
        value switch
        {
            Microsoft.Dafny.SetType subType => subType,
            Microsoft.Dafny.MultiSetType subType => subType,
            Microsoft.Dafny.SeqType subType => subType,
            Microsoft.Dafny.MapType subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.TypeProxy TransformUnion(Microsoft.Dafny.TypeProxy value) =>
        value switch
        {
            Microsoft.Dafny.InferredTypeProxy subType => subType,
            Microsoft.Dafny.ParamTypeProxy subType => subType,
        };

        public virtual Microsoft.Dafny.TopLevelDecl TransformUnion(Microsoft.Dafny.TopLevelDecl value) =>
        value switch
        {
            Microsoft.Dafny.TypeParameter subType => subType,
            Microsoft.Dafny.ModuleDecl subType => TransformUnion(subType),
            Microsoft.Dafny.TopLevelDeclWithMembers subType => TransformUnion(subType),
            Microsoft.Dafny.ValuetypeDecl subType => Transform(subType),
            Microsoft.Dafny.TypeSynonymDeclBase subType => TransformUnion(subType),
        };

        public virtual Microsoft.Dafny.ModuleDecl TransformUnion(Microsoft.Dafny.ModuleDecl value) =>
        value switch
        {
            Microsoft.Dafny.LiteralModuleDecl subType => Transform(subType),
            Microsoft.Dafny.AliasModuleDecl subType => Transform(subType),
            Microsoft.Dafny.AbstractModuleDecl subType => Transform(subType),
            Microsoft.Dafny.ModuleExportDecl subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.ModuleDefinition TransformUnion(Microsoft.Dafny.ModuleDefinition value) =>
        value switch
        {
            Microsoft.Dafny.DefaultModuleDecl subType => subType,
        };

        public virtual Microsoft.Dafny.Declaration TransformUnion(Microsoft.Dafny.Declaration value) =>
        value switch
        {
            Microsoft.Dafny.TopLevelDecl subType => TransformUnion(subType),
            Microsoft.Dafny.DatatypeCtor subType => Transform(subType),
            Microsoft.Dafny.MemberDecl subType => TransformUnion(subType),
        };

        public virtual Microsoft.Dafny.ClassDecl TransformUnion(Microsoft.Dafny.ClassDecl value) =>
        value switch
        {
            Microsoft.Dafny.TraitDecl subType => subType,
            Microsoft.Dafny.DefaultClassDecl subType => subType,
            Microsoft.Dafny.ArrayClassDecl subType => subType,
            Microsoft.Dafny.ArrowTypeDecl subType => Transform(subType),
            Microsoft.Dafny.IteratorDecl subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.TopLevelDeclWithMembers TransformUnion(Microsoft.Dafny.TopLevelDeclWithMembers value) =>
        value switch
        {
            Microsoft.Dafny.ClassDecl subType => TransformUnion(subType),
            Microsoft.Dafny.DatatypeDecl subType => TransformUnion(subType),
            Microsoft.Dafny.OpaqueTypeDecl subType => subType,
            Microsoft.Dafny.NewtypeDecl subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.DatatypeDecl TransformUnion(Microsoft.Dafny.DatatypeDecl value) =>
        value switch
        {
            Microsoft.Dafny.IndDatatypeDecl subType => TransformUnion(subType),
            Microsoft.Dafny.CoDatatypeDecl subType => subType,
        };

        public virtual Microsoft.Dafny.IndDatatypeDecl TransformUnion(Microsoft.Dafny.IndDatatypeDecl value) =>
        value switch
        {
            Microsoft.Dafny.TupleTypeDecl subType => subType,
        };

        public virtual Microsoft.Dafny.MemberDecl TransformUnion(Microsoft.Dafny.MemberDecl value) =>
        value switch
        {
            Microsoft.Dafny.Field subType => TransformUnion(subType),
            Microsoft.Dafny.Function subType => TransformUnion(subType),
            Microsoft.Dafny.Method subType => TransformUnion(subType),
        };

        public virtual Microsoft.Dafny.Function TransformUnion(Microsoft.Dafny.Function value) =>
        value switch
        {
            Microsoft.Dafny.SpecialFunction subType => Transform(subType),
            Microsoft.Dafny.Predicate subType => subType,
            Microsoft.Dafny.PrefixPredicate subType => Transform(subType),
            Microsoft.Dafny.ExtremePredicate subType => TransformUnion(subType),
            Microsoft.Dafny.TwoStateFunction subType => subType,
        };

        public virtual Microsoft.Dafny.Field TransformUnion(Microsoft.Dafny.Field value) =>
        value switch
        {
            Microsoft.Dafny.SpecialField subType => TransformUnion(subType),
        };

        public virtual Microsoft.Dafny.SpecialField TransformUnion(Microsoft.Dafny.SpecialField value) =>
        value switch
        {
            Microsoft.Dafny.DatatypeDestructor subType => Transform(subType),
            Microsoft.Dafny.ConstantField subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.TypeSynonymDeclBase TransformUnion(Microsoft.Dafny.TypeSynonymDeclBase value) =>
        value switch
        {
            Microsoft.Dafny.TypeSynonymDecl subType => TransformUnion(subType),
            Microsoft.Dafny.InternalTypeSynonymDecl subType => subType,
        };

        public virtual Microsoft.Dafny.TypeSynonymDecl TransformUnion(Microsoft.Dafny.TypeSynonymDecl value) =>
        value switch
        {
            Microsoft.Dafny.SubsetTypeDecl subType => TransformUnion(subType),
        };

        public virtual Microsoft.Dafny.SubsetTypeDecl TransformUnion(Microsoft.Dafny.SubsetTypeDecl value) =>
        value switch
        {
            Microsoft.Dafny.NonNullTypeDecl subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.Formal TransformUnion(Microsoft.Dafny.Formal value) =>
        value switch
        {
            Microsoft.Dafny.ImplicitFormal subType => subType,
        };

        public virtual Microsoft.Dafny.ExtremePredicate TransformUnion(Microsoft.Dafny.ExtremePredicate value) =>
        value switch
        {
            Microsoft.Dafny.LeastPredicate subType => subType,
            Microsoft.Dafny.GreatestPredicate subType => subType,
        };

        public virtual Microsoft.Dafny.V2.Method TransformUnion(Microsoft.Dafny.Method value) =>
        value switch
        {
            Microsoft.Dafny.Lemma subType => Transform(subType),
            Microsoft.Dafny.TwoStateLemma subType => Transform(subType),
            Microsoft.Dafny.Constructor subType => Transform(subType),
            Microsoft.Dafny.PrefixLemma subType => Transform(subType),
            Microsoft.Dafny.ExtremeLemma subType => TransformUnion(subType),
        };

        public virtual Microsoft.Dafny.V2.ExtremeLemma TransformUnion(Microsoft.Dafny.ExtremeLemma value) =>
        value switch
        {
            Microsoft.Dafny.LeastLemma subType => Transform(subType),
            Microsoft.Dafny.GreatestLemma subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.Statement TransformUnion(Microsoft.Dafny.Statement value) =>
        value switch
        {
            Microsoft.Dafny.PredicateStmt subType => TransformUnion(subType),
            Microsoft.Dafny.PrintStmt subType => Transform(subType),
            Microsoft.Dafny.RevealStmt subType => Transform(subType),
            Microsoft.Dafny.BreakStmt subType => Transform(subType),
            Microsoft.Dafny.ProduceStmt subType => TransformUnion(subType),
            Microsoft.Dafny.VarDeclStmt subType => Transform(subType),
            Microsoft.Dafny.VarDeclPattern subType => Transform(subType),
            Microsoft.Dafny.ConcreteUpdateStatement subType => TransformUnion(subType),
            Microsoft.Dafny.AssignStmt subType => Transform(subType),
            Microsoft.Dafny.CallStmt subType => Transform(subType),
            Microsoft.Dafny.BlockStmt subType => TransformUnion(subType),
            Microsoft.Dafny.IfStmt subType => Transform(subType),
            Microsoft.Dafny.AlternativeStmt subType => Transform(subType),
            Microsoft.Dafny.LoopStmt subType => TransformUnion(subType),
            Microsoft.Dafny.ForallStmt subType => Transform(subType),
            Microsoft.Dafny.ModifyStmt subType => Transform(subType),
            Microsoft.Dafny.CalcStmt subType => Transform(subType),
            Microsoft.Dafny.MatchStmt subType => Transform(subType),
            Microsoft.Dafny.SkeletonStatement subType => Transform(subType),
            Microsoft.Dafny.ConcreteSyntaxStatement subType => TransformUnion(subType),
        };

        public virtual Microsoft.Dafny.PredicateStmt TransformUnion(Microsoft.Dafny.PredicateStmt value) =>
        value switch
        {
            Microsoft.Dafny.AssertStmt subType => Transform(subType),
            Microsoft.Dafny.ExpectStmt subType => Transform(subType),
            Microsoft.Dafny.AssumeStmt subType => subType,
        };

        public virtual Microsoft.Dafny.ProduceStmt TransformUnion(Microsoft.Dafny.ProduceStmt value) =>
        value switch
        {
            Microsoft.Dafny.ReturnStmt subType => subType,
            Microsoft.Dafny.YieldStmt subType => subType,
        };

        public virtual Microsoft.Dafny.AssignmentRhs TransformUnion(Microsoft.Dafny.AssignmentRhs value) =>
        value switch
        {
            Microsoft.Dafny.ExprRhs subType => Transform(subType),
            Microsoft.Dafny.TypeRhs subType => Transform(subType),
            Microsoft.Dafny.HavocRhs subType => subType,
        };

        public virtual Microsoft.Dafny.ConcreteUpdateStatement TransformUnion(Microsoft.Dafny.ConcreteUpdateStatement value) =>
        value switch
        {
            Microsoft.Dafny.AssignSuchThatStmt subType => Transform(subType),
            Microsoft.Dafny.UpdateStmt subType => Transform(subType),
            Microsoft.Dafny.AssignOrReturnStmt subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.BlockStmt TransformUnion(Microsoft.Dafny.BlockStmt value) =>
        value switch
        {
            Microsoft.Dafny.DividedBlockStmt subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.LoopStmt TransformUnion(Microsoft.Dafny.LoopStmt value) =>
        value switch
        {
            Microsoft.Dafny.OneBodyLoopStmt subType => TransformUnion(subType),
            Microsoft.Dafny.AlternativeLoopStmt subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.OneBodyLoopStmt TransformUnion(Microsoft.Dafny.OneBodyLoopStmt value) =>
        value switch
        {
            Microsoft.Dafny.WhileStmt subType => TransformUnion(subType),
            Microsoft.Dafny.ForLoopStmt subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.WhileStmt TransformUnion(Microsoft.Dafny.WhileStmt value) =>
        value switch
        {
            Microsoft.Dafny.RefinedWhileStmt subType => subType,
        };

        public virtual Microsoft.Dafny.LiteralExpr TransformUnion(Microsoft.Dafny.LiteralExpr value) =>
        value switch
        {
            Microsoft.Dafny.StaticReceiverExpr subType => Transform(subType),
            Microsoft.Dafny.CharLiteralExpr subType => subType,
            Microsoft.Dafny.StringLiteralExpr subType => subType,
        };

        public virtual Microsoft.Dafny.Expression TransformUnion(Microsoft.Dafny.Expression value) =>
        value switch
        {
            Microsoft.Dafny.LiteralExpr subType => TransformUnion(subType),
            Microsoft.Dafny.DatatypeValue subType => Transform(subType),
            Microsoft.Dafny.ThisExpr subType => subType,
            Microsoft.Dafny.IdentifierExpr subType => subType,
            Microsoft.Dafny.Resolver_IdentifierExpr subType => Transform(subType),
            Microsoft.Dafny.DisplayExpression subType => TransformUnion(subType),
            Microsoft.Dafny.MapDisplayExpr subType => Transform(subType),
            Microsoft.Dafny.MemberSelectExpr subType => Transform(subType),
            Microsoft.Dafny.SeqSelectExpr subType => Transform(subType),
            Microsoft.Dafny.MultiSelectExpr subType => Transform(subType),
            Microsoft.Dafny.SeqUpdateExpr subType => Transform(subType),
            Microsoft.Dafny.ApplyExpr subType => Transform(subType),
            Microsoft.Dafny.FunctionCallExpr subType => Transform(subType),
            Microsoft.Dafny.SeqConstructionExpr subType => Transform(subType),
            Microsoft.Dafny.MultiSetFormingExpr subType => Transform(subType),
            Microsoft.Dafny.OldExpr subType => Transform(subType),
            Microsoft.Dafny.UnchangedExpr subType => Transform(subType),
            Microsoft.Dafny.UnaryExpr subType => TransformUnion(subType),
            Microsoft.Dafny.BinaryExpr subType => Transform(subType),
            Microsoft.Dafny.TernaryExpr subType => Transform(subType),
            Microsoft.Dafny.LetExpr subType => TransformUnion(subType),
            Microsoft.Dafny.ComprehensionExpr subType => TransformUnion(subType),
            Microsoft.Dafny.WildcardExpr subType => subType,
            Microsoft.Dafny.StmtExpr subType => Transform(subType),
            Microsoft.Dafny.ITEExpr subType => Transform(subType),
            Microsoft.Dafny.MatchExpr subType => Transform(subType),
            Microsoft.Dafny.BoxingCastExpr subType => Transform(subType),
            Microsoft.Dafny.UnboxingCastExpr subType => Transform(subType),
            Microsoft.Dafny.ConcreteSyntaxExpression subType => TransformUnion(subType),
        };

        public virtual Microsoft.Dafny.DisplayExpression TransformUnion(Microsoft.Dafny.DisplayExpression value) =>
        value switch
        {
            Microsoft.Dafny.SetDisplayExpr subType => subType,
            Microsoft.Dafny.MultiSetDisplayExpr subType => subType,
            Microsoft.Dafny.SeqDisplayExpr subType => subType,
        };

        public virtual Microsoft.Dafny.UnaryExpr TransformUnion(Microsoft.Dafny.UnaryExpr value) =>
        value switch
        {
            Microsoft.Dafny.UnaryOpExpr subType => subType,
            Microsoft.Dafny.TypeUnaryExpr subType => TransformUnion(subType),
        };

        public virtual Microsoft.Dafny.TypeUnaryExpr TransformUnion(Microsoft.Dafny.TypeUnaryExpr value) =>
        value switch
        {
            Microsoft.Dafny.ConversionExpr subType => subType,
            Microsoft.Dafny.TypeTestExpr subType => subType,
        };

        public virtual Microsoft.Dafny.ConcreteSyntaxExpression TransformUnion(Microsoft.Dafny.ConcreteSyntaxExpression value) =>
        value switch
        {
            Microsoft.Dafny.LetOrFailExpr subType => Transform(subType),
            Microsoft.Dafny.NestedMatchExpr subType => Transform(subType),
            Microsoft.Dafny.ParensExpression subType => TransformUnion(subType),
            Microsoft.Dafny.DatatypeUpdateExpr subType => Transform(subType),
            Microsoft.Dafny.DefaultValueExpression subType => Transform(subType),
            Microsoft.Dafny.NegationExpression subType => Transform(subType),
            Microsoft.Dafny.ChainingExpression subType => Transform(subType),
            Microsoft.Dafny.SuffixExpr subType => TransformUnion(subType),
            Microsoft.Dafny.NameSegment subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.ComprehensionExpr TransformUnion(Microsoft.Dafny.ComprehensionExpr value) =>
        value switch
        {
            Microsoft.Dafny.QuantifierExpr subType => TransformUnion(subType),
            Microsoft.Dafny.SetComprehension subType => subType,
            Microsoft.Dafny.MapComprehension subType => Transform(subType),
            Microsoft.Dafny.LambdaExpr subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.QuantifierExpr TransformUnion(Microsoft.Dafny.QuantifierExpr value) =>
        value switch
        {
            Microsoft.Dafny.ForallExpr subType => subType,
            Microsoft.Dafny.ExistsExpr subType => subType,
        };

        public virtual Microsoft.Dafny.MatchingContext TransformUnion(Microsoft.Dafny.MatchingContext value) =>
        value switch
        {
            Microsoft.Dafny.LitCtx subType => Transform(subType),
            Microsoft.Dafny.HoleCtx subType => subType,
            Microsoft.Dafny.ForallCtx subType => subType,
            Microsoft.Dafny.IdCtx subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.ExtendedPattern TransformUnion(Microsoft.Dafny.ExtendedPattern value) =>
        value switch
        {
            Microsoft.Dafny.LitPattern subType => Transform(subType),
            Microsoft.Dafny.IdPattern subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.ConcreteSyntaxStatement TransformUnion(Microsoft.Dafny.ConcreteSyntaxStatement value) =>
        value switch
        {
            Microsoft.Dafny.NestedMatchStmt subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.ParensExpression TransformUnion(Microsoft.Dafny.ParensExpression value) =>
        value switch
        {
            Microsoft.Dafny.TypeExpr subType => Transform(subType),
            Microsoft.Dafny.AutoGeneratedExpression subType => subType,
        };

        public virtual Microsoft.Dafny.SuffixExpr TransformUnion(Microsoft.Dafny.SuffixExpr value) =>
        value switch
        {
            Microsoft.Dafny.ExprDotName subType => Transform(subType),
            Microsoft.Dafny.ApplySuffix subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.ComprehensionExpr.BoundedPool TransformUnion(Microsoft.Dafny.ComprehensionExpr.BoundedPool value) =>
        value switch
        {
            Microsoft.Dafny.AssignSuchThatStmt.WiggleWaggleBound subType => subType,
            Microsoft.Dafny.ComprehensionExpr.ExactBoundedPool subType => Transform(subType),
            Microsoft.Dafny.ComprehensionExpr.BoolBoundedPool subType => subType,
            Microsoft.Dafny.ComprehensionExpr.CharBoundedPool subType => subType,
            Microsoft.Dafny.ComprehensionExpr.AllocFreeBoundedPool subType => Transform(subType),
            Microsoft.Dafny.ComprehensionExpr.ExplicitAllocatedBoundedPool subType => subType,
            Microsoft.Dafny.ComprehensionExpr.SpecialAllocIndependenceAllocatedBoundedPool subType => subType,
            Microsoft.Dafny.ComprehensionExpr.IntBoundedPool subType => Transform(subType),
            Microsoft.Dafny.ComprehensionExpr.CollectionBoundedPool subType => TransformUnion(subType),
            Microsoft.Dafny.ComprehensionExpr.SubSetBoundedPool subType => Transform(subType),
            Microsoft.Dafny.ComprehensionExpr.SuperSetBoundedPool subType => Transform(subType),
            Microsoft.Dafny.ComprehensionExpr.DatatypeBoundedPool subType => Transform(subType),
            Microsoft.Dafny.ComprehensionExpr.DatatypeInclusionBoundedPool subType => subType,
        };

        public virtual Microsoft.Dafny.CalcStmt.CalcOp TransformUnion(Microsoft.Dafny.CalcStmt.CalcOp value) =>
        value switch
        {
            Microsoft.Dafny.CalcStmt.BinaryCalcOp subType => subType,
            Microsoft.Dafny.CalcStmt.TernaryCalcOp subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.ComprehensionExpr.CollectionBoundedPool TransformUnion(Microsoft.Dafny.ComprehensionExpr.CollectionBoundedPool value) =>
        value switch
        {
            Microsoft.Dafny.ComprehensionExpr.SetBoundedPool subType => Transform(subType),
            Microsoft.Dafny.ComprehensionExpr.MultiSetBoundedPool subType => Transform(subType),
            Microsoft.Dafny.ComprehensionExpr.MapBoundedPool subType => Transform(subType),
            Microsoft.Dafny.ComprehensionExpr.SeqBoundedPool subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.LetExpr TransformUnion(Microsoft.Dafny.LetExpr value) =>
        value switch
        {

        };

        public virtual Microsoft.Dafny.Program Transform(Microsoft.Dafny.Program value)
        {
            return new Microsoft.Dafny.Program(value.Name, TransformUnion(value.DefaultModule), Transform(value.BuiltIns), value.Reporter);
        }

        public virtual Microsoft.Dafny.SelfType Transform(Microsoft.Dafny.SelfType value)
        {
            return new Microsoft.Dafny.SelfType();
        }

        public virtual Microsoft.Dafny.UserDefinedType Transform(Microsoft.Dafny.UserDefinedType value)
        {
            return new Microsoft.Dafny.UserDefinedType(value.Tok, TransformUnion(value.NamePath));
        }

        public virtual Microsoft.Dafny.MapType Transform(Microsoft.Dafny.MapType value)
        {
            return new Microsoft.Dafny.MapType(value.Finite, TransformUnion(value.Domain), TransformUnion(value.Range));
        }

        public virtual Microsoft.Dafny.ValuetypeDecl Transform(Microsoft.Dafny.ValuetypeDecl value) {
          throw new Exception();
          // return new Microsoft.Dafny.ValuetypeDecl(value.Name, TransformUnion(value.EnclosingModuleDefinition), 
          //   value.TypeParameterCount, 
          //   Transform(value.TypeTester, value => TransformUnion(value), value => value), 
          //   Transform(value.TypeCreator, value => Transform(value, value => TransformUnion(value)), value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.LiteralModuleDecl Transform(Microsoft.Dafny.LiteralModuleDecl value)
        {
            return new Microsoft.Dafny.LiteralModuleDecl(TransformUnion(value.ModuleDef), TransformUnion(value.EnclosingModuleDefinition));
        }

        public virtual Microsoft.Dafny.AliasModuleDecl Transform(Microsoft.Dafny.AliasModuleDecl value)
        {
            return new Microsoft.Dafny.AliasModuleDecl(Transform(value.TargetQId), value.Tok, TransformUnion(value.EnclosingModuleDefinition), value.Opened, value.Exports);
        }

        public virtual Microsoft.Dafny.AbstractModuleDecl Transform(Microsoft.Dafny.AbstractModuleDecl value)
        {
            return new Microsoft.Dafny.AbstractModuleDecl(Transform(value.QId), value.Tok, TransformUnion(value.EnclosingModuleDefinition), value.Opened, value.Exports);
        }

        public virtual Microsoft.Dafny.ModuleExportDecl Transform(Microsoft.Dafny.ModuleExportDecl value)
        {
            return new Microsoft.Dafny.ModuleExportDecl(value.Tok, TransformUnion(value.EnclosingModuleDefinition), Transform(value.Exports, value => Transform(value)), value.Extends, value.ProvideAll, value.RevealAll, value.IsDefault, value.IsRefining);
        }

        public virtual Microsoft.Dafny.DatatypeCtor Transform(Microsoft.Dafny.DatatypeCtor value)
        {
            return new Microsoft.Dafny.DatatypeCtor(value.Tok, value.Name, Transform(value.Formals, value => TransformUnion(value)), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.ArrowTypeDecl Transform(Microsoft.Dafny.ArrowTypeDecl value)
        {
            return new Microsoft.Dafny.ArrowTypeDecl(value.TypeArgs, TransformUnion(value.Requires), TransformUnion(value.Reads), TransformUnion(value.EnclosingModuleDefinition), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.IteratorDecl Transform(Microsoft.Dafny.IteratorDecl value)
        {
            return new Microsoft.Dafny.IteratorDecl(value.Tok, value.Name, TransformUnion(value.EnclosingModuleDefinition), value.TypeArgs, Transform(value.Ins, value => TransformUnion(value)), Transform(value.Outs, value => TransformUnion(value)), Transform(value.Reads, value => Transform(value)), Transform(value.Modifies, value => Transform(value)), Transform(value.Decreases, value => TransformUnion(value)), Transform(value.Requires, value => Transform(value)), Transform(value.Ensures, value => Transform(value)), Transform(value.YieldRequires, value => Transform(value)), Transform(value.YieldEnsures, value => Transform(value)), TransformUnion(value.Body), TransformUnion(value.Attributes), value.SignatureEllipsis);
        }

        public virtual Microsoft.Dafny.ClassDecl Transform(Microsoft.Dafny.ClassDecl value)
        {
            return new Microsoft.Dafny.ClassDecl(value.Tok, value.Name, TransformUnion(value.EnclosingModuleDefinition), value.TypeArgs, Transform(value.Members, value => TransformUnion(value)), TransformUnion(value.Attributes), value.IsRefining, Transform(value.ParentTraits, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.NewtypeDecl Transform(Microsoft.Dafny.NewtypeDecl value)
        {
            return new Microsoft.Dafny.NewtypeDecl(value.Tok, value.Name, TransformUnion(value.EnclosingModuleDefinition), TransformUnion(value.BaseType), Transform(value.Members, value => TransformUnion(value)), TransformUnion(value.Attributes), value.IsRefining);
        }

        public virtual Microsoft.Dafny.IndDatatypeDecl Transform(Microsoft.Dafny.IndDatatypeDecl value)
        {
            return new Microsoft.Dafny.IndDatatypeDecl(value.Tok, value.Name, TransformUnion(value.EnclosingModuleDefinition), value.TypeArgs, Transform(value.Ctors, value => Transform(value)), Transform(value.Members, value => TransformUnion(value)), TransformUnion(value.Attributes), value.IsRefining);
        }

        public virtual Microsoft.Dafny.Field Transform(Microsoft.Dafny.Field value)
        {
            return new Microsoft.Dafny.Field(value.Tok, value.Name, value.IsGhost, TransformUnion(value.Type), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.Function Transform(Microsoft.Dafny.Function value)
        {
            return new Microsoft.Dafny.Function(value.Tok, value.Name, value.HasStaticKeyword, value.IsGhost, value.TypeArgs, Transform(value.Formals, value => TransformUnion(value)), TransformUnion(value.Result), TransformUnion(value.ResultType), Transform(value.Req, value => Transform(value)), Transform(value.Reads, value => Transform(value)), Transform(value.Ens, value => Transform(value)), Transform(value.Decreases, value => TransformUnion(value)), TransformUnion(value.Body), value.ByMethodTok, TransformUnion(value.ByMethodBody), TransformUnion(value.Attributes), value.SignatureEllipsis);
        }

        public virtual Microsoft.Dafny.V2.Method Transform(Microsoft.Dafny.Method value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.SpecialFunction Transform(Microsoft.Dafny.SpecialFunction value)
        {
            return new Microsoft.Dafny.SpecialFunction(value.Tok, value.Name, TransformUnion(value.Module), value.HasStaticKeyword, value.IsGhost, value.TypeArgs, Transform(value.Formals, value => TransformUnion(value)), TransformUnion(value.ResultType), Transform(value.Req, value => Transform(value)), Transform(value.Reads, value => Transform(value)), Transform(value.Ens, value => Transform(value)), Transform(value.Decreases, value => TransformUnion(value)), TransformUnion(value.Body), TransformUnion(value.Attributes), value.SignatureEllipsis);
        }

        public virtual Microsoft.Dafny.PrefixPredicate Transform(Microsoft.Dafny.PrefixPredicate value)
        {
            return new Microsoft.Dafny.PrefixPredicate(value.Tok, value.Name, value.HasStaticKeyword, value.TypeArgs, TransformUnion(value.K), Transform(value.Formals, value => TransformUnion(value)), Transform(value.Req, value => Transform(value)), Transform(value.Reads, value => Transform(value)), Transform(value.Ens, value => Transform(value)), Transform(value.Decreases, value => TransformUnion(value)), TransformUnion(value.Body), TransformUnion(value.Attributes), TransformUnion(value.ExtremePred));
        }

        public virtual Microsoft.Dafny.SpecialField Transform(Microsoft.Dafny.SpecialField value)
        {
            return new Microsoft.Dafny.SpecialField(value.Tok, value.Name, value.SpecialId, value.IdParam, value.IsGhost, value.IsMutable, value.IsUserMutable, TransformUnion(value.Type), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.DatatypeDestructor Transform(Microsoft.Dafny.DatatypeDestructor value) {
          throw new Exception();
            // return new Microsoft.Dafny.DatatypeDestructor(value.Tok, Transform(value.EnclosingCtor), TransformUnion(value.CorrespondingFormal), value.Name, value.CompiledName, value.IsGhost, TransformUnion(value.Type), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.ConstantField Transform(Microsoft.Dafny.ConstantField value)
        {
            return new Microsoft.Dafny.ConstantField(value.Tok, value.Name, TransformUnion(value.Rhs), value.HasStaticKeyword, value.IsGhost, TransformUnion(value.Type), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.TypeSynonymDecl Transform(Microsoft.Dafny.TypeSynonymDecl value)
        {
            return new Microsoft.Dafny.TypeSynonymDecl(value.Tok, value.Name, value.Characteristics, value.TypeArgs, TransformUnion(value.EnclosingModuleDefinition), TransformUnion(value.Rhs), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.SubsetTypeDecl Transform(Microsoft.Dafny.SubsetTypeDecl value)
        {
            return new Microsoft.Dafny.SubsetTypeDecl(value.Tok, value.Name, value.Characteristics, value.TypeArgs, TransformUnion(value.EnclosingModuleDefinition), value.BoundVar, TransformUnion(value.Constraint), value.WitnessKind, TransformUnion(value.Witness), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.NonNullTypeDecl Transform(Microsoft.Dafny.NonNullTypeDecl value)
        {
            return new Microsoft.Dafny.NonNullTypeDecl(TransformUnion(value.ClassDecl));
        }

        public virtual Microsoft.Dafny.Formal Transform(Microsoft.Dafny.Formal value)
        {
            return new Microsoft.Dafny.Formal(value.Tok, value.Name, TransformUnion(value.Type), value.InParam, value.IsGhost, TransformUnion(value.DefaultValue), value.IsOld, value.IsNameOnly, value.NameForCompilation);
        }

        public virtual Microsoft.Dafny.V2.Lemma Transform(Microsoft.Dafny.Lemma value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.TwoStateLemma Transform(Microsoft.Dafny.TwoStateLemma value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.Constructor Transform(Microsoft.Dafny.Constructor value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.PrefixLemma Transform(Microsoft.Dafny.PrefixLemma value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.LeastLemma Transform(Microsoft.Dafny.LeastLemma value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.GreatestLemma Transform(Microsoft.Dafny.GreatestLemma value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.PrintStmt Transform(Microsoft.Dafny.PrintStmt value)
        {
            return new Microsoft.Dafny.PrintStmt(value.Tok, value.EndTok, Transform(value.Args, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.RevealStmt Transform(Microsoft.Dafny.RevealStmt value)
        {
            return new Microsoft.Dafny.RevealStmt(value.Tok, value.EndTok, Transform(value.Exprs, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.BreakStmt Transform(Microsoft.Dafny.BreakStmt value)
        {
            return new Microsoft.Dafny.BreakStmt(value.Tok, value.EndTok, value.TargetLabel);
        }

        public virtual Microsoft.Dafny.VarDeclStmt Transform(Microsoft.Dafny.VarDeclStmt value)
        {
            return new Microsoft.Dafny.VarDeclStmt(value.Tok, value.EndTok, Transform(value.Locals, value => Transform(value)), TransformUnion(value.Update));
        }

        public virtual Microsoft.Dafny.VarDeclPattern Transform(Microsoft.Dafny.VarDeclPattern value)
        {
            return new Microsoft.Dafny.VarDeclPattern(value.Tok, value.EndTok, Transform(value.Lhs, value => Transform(value)), TransformUnion(value.Rhs), value.HasGhostModifier);
        }

        public virtual Microsoft.Dafny.AssignStmt Transform(Microsoft.Dafny.AssignStmt value)
        {
            return new Microsoft.Dafny.AssignStmt(value.Tok, value.EndTok, TransformUnion(value.Lhs), TransformUnion(value.Rhs));
        }

        public virtual Microsoft.Dafny.CallStmt Transform(Microsoft.Dafny.CallStmt value)
        {
            return new Microsoft.Dafny.CallStmt(value.Tok, value.EndTok, Transform(value.Lhs, value => TransformUnion(value)), Transform(value.MethodSelect), Transform(value.BindingArgs, value => Transform(value)));
        }

        public virtual Microsoft.Dafny.BlockStmt Transform(Microsoft.Dafny.BlockStmt value)
        {
            return new Microsoft.Dafny.BlockStmt(value.Tok, value.EndTok, Transform(value.Body, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.IfStmt Transform(Microsoft.Dafny.IfStmt value)
        {
            return new Microsoft.Dafny.IfStmt(value.Tok, value.EndTok, value.IsBindingGuard, TransformUnion(value.Guard), TransformUnion(value.Thn), TransformUnion(value.Els));
        }

        public virtual Microsoft.Dafny.AlternativeStmt Transform(Microsoft.Dafny.AlternativeStmt value)
        {
            return new Microsoft.Dafny.AlternativeStmt(value.Tok, value.EndTok, Transform(value.Alternatives, value => Transform(value)), value.UsesOptionalBraces);
        }

        public virtual Microsoft.Dafny.ForallStmt Transform(Microsoft.Dafny.ForallStmt value)
        {
            return new Microsoft.Dafny.ForallStmt(value.Tok, value.EndTok, value.BoundVars, TransformUnion(value.Attributes), TransformUnion(value.Range), Transform(value.Ens, value => Transform(value)), TransformUnion(value.Body));
        }

        public virtual Microsoft.Dafny.ModifyStmt Transform(Microsoft.Dafny.ModifyStmt value)
        {
            return new Microsoft.Dafny.ModifyStmt(value.Tok, value.EndTok, Transform(value.Mod, value => Transform(value)), TransformUnion(value.Body));
        }

        public virtual Microsoft.Dafny.CalcStmt Transform(Microsoft.Dafny.CalcStmt value)
        {
            return new Microsoft.Dafny.CalcStmt(value.Tok, value.EndTok, TransformUnion(value.UserSuppliedOp), Transform(value.Lines, value => TransformUnion(value)), Transform(value.Hints, value => TransformUnion(value)), Transform(value.StepOps, value => TransformUnion(value)), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.MatchStmt Transform(Microsoft.Dafny.MatchStmt value)
        {
            return new Microsoft.Dafny.MatchStmt(value.Tok, value.EndTok, TransformUnion(value.Source), Transform(value.Cases, value => Transform(value)), value.UsesOptionalBraces, TransformUnion(value.Context));
        }

        public virtual Microsoft.Dafny.SkeletonStatement Transform(Microsoft.Dafny.SkeletonStatement value)
        {
            return new Microsoft.Dafny.SkeletonStatement(value.Tok, value.EndTok);
        }

        public virtual Microsoft.Dafny.AssertStmt Transform(Microsoft.Dafny.AssertStmt value)
        {
            return new Microsoft.Dafny.AssertStmt(value.Tok, value.EndTok, TransformUnion(value.Expr), TransformUnion(value.Proof), value.Label, TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.ExpectStmt Transform(Microsoft.Dafny.ExpectStmt value)
        {
            return new Microsoft.Dafny.ExpectStmt(value.Tok, value.EndTok, TransformUnion(value.Expr), TransformUnion(value.Message), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.ExprRhs Transform(Microsoft.Dafny.ExprRhs value)
        {
            return new Microsoft.Dafny.ExprRhs(TransformUnion(value.Expr), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.TypeRhs Transform(Microsoft.Dafny.TypeRhs value)
        {
            return new Microsoft.Dafny.TypeRhs(value.Tok, TransformUnion(value.Type), Transform(value.ArrayDimensions, value => TransformUnion(value)), TransformUnion(value.ElementInit));
        }

        public virtual Microsoft.Dafny.AssignSuchThatStmt Transform(Microsoft.Dafny.AssignSuchThatStmt value)
        {
            return new Microsoft.Dafny.AssignSuchThatStmt(value.Tok, value.EndTok, Transform(value.Lhss, value => TransformUnion(value)), TransformUnion(value.Expr), value.AssumeToken, TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.UpdateStmt Transform(Microsoft.Dafny.UpdateStmt value)
        {
            return new Microsoft.Dafny.UpdateStmt(value.Tok, value.EndTok, Transform(value.Lhss, value => TransformUnion(value)), Transform(value.Rhss, value => TransformUnion(value)), value.CanMutateKnownState);
        }

        public virtual Microsoft.Dafny.AssignOrReturnStmt Transform(Microsoft.Dafny.AssignOrReturnStmt value)
        {
            return new Microsoft.Dafny.AssignOrReturnStmt(value.Tok, value.EndTok, Transform(value.Lhss, value => TransformUnion(value)), TransformUnion(value.Rhs), value.KeywordToken, Transform(value.Rhss, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.DividedBlockStmt Transform(Microsoft.Dafny.DividedBlockStmt value)
        {
            return new Microsoft.Dafny.DividedBlockStmt(value.Tok, value.EndTok, Transform(value.BodyInit, value => TransformUnion(value)), value.SeparatorTok, Transform(value.BodyProper, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.AlternativeLoopStmt Transform(Microsoft.Dafny.AlternativeLoopStmt value)
        {
            return new Microsoft.Dafny.AlternativeLoopStmt(value.Tok, value.EndTok, Transform(value.Invariants, value => Transform(value)), Transform(value.Decreases, value => TransformUnion(value)), Transform(value.Mod, value => Transform(value)), Transform(value.Alternatives, value => Transform(value)), value.UsesOptionalBraces);
        }

        public virtual Microsoft.Dafny.WhileStmt Transform(Microsoft.Dafny.WhileStmt value)
        {
            return new Microsoft.Dafny.WhileStmt(value.Tok, value.EndTok, TransformUnion(value.Guard), Transform(value.Invariants, value => Transform(value)), Transform(value.Decreases, value => TransformUnion(value)), Transform(value.Mod, value => Transform(value)), TransformUnion(value.Body));
        }

        public virtual Microsoft.Dafny.ForLoopStmt Transform(Microsoft.Dafny.ForLoopStmt value)
        {
            return new Microsoft.Dafny.ForLoopStmt(value.Tok, value.EndTok, value.LoopIndex, TransformUnion(value.Start), TransformUnion(value.End), value.GoingUp, Transform(value.Invariants, value => Transform(value)), Transform(value.Decreases, value => TransformUnion(value)), Transform(value.Mod, value => Transform(value)), TransformUnion(value.Body), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.MatchCaseStmt Transform(Microsoft.Dafny.MatchCaseStmt value)
        {
            return new Microsoft.Dafny.MatchCaseStmt(value.Tok, Transform(value.Ctor), value.Arguments, Transform(value.Body, value => TransformUnion(value)), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.MatchCaseExpr Transform(Microsoft.Dafny.MatchCaseExpr value)
        {
            return new Microsoft.Dafny.MatchCaseExpr(value.Tok, Transform(value.Ctor), value.Arguments, TransformUnion(value.Body), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.StaticReceiverExpr Transform(Microsoft.Dafny.StaticReceiverExpr value)
        {
            return new Microsoft.Dafny.StaticReceiverExpr(value.Tok, TransformUnion(value.UnresolvedType), value.IsImplicit);
        }

        public virtual Microsoft.Dafny.LiteralExpr Transform(Microsoft.Dafny.LiteralExpr value)
        {
            return new Microsoft.Dafny.LiteralExpr(value.Tok);
        }

        public virtual Microsoft.Dafny.DatatypeValue Transform(Microsoft.Dafny.DatatypeValue value)
        {
            return new Microsoft.Dafny.DatatypeValue(value.Tok, value.DatatypeName, value.MemberName, Transform(value.ArgumentBindings, value => Transform(value)));
        }

        public virtual Microsoft.Dafny.Resolver_IdentifierExpr Transform(Microsoft.Dafny.Resolver_IdentifierExpr value)
        {
            return new Microsoft.Dafny.Resolver_IdentifierExpr(value.Tok, value.Decl, Transform(value.TypeArgs, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.MapDisplayExpr Transform(Microsoft.Dafny.MapDisplayExpr value)
        {
            return new Microsoft.Dafny.MapDisplayExpr(value.Tok, value.Finite, Transform(value.Elements, value => Transform(value)));
        }

        public virtual Microsoft.Dafny.MemberSelectExpr Transform(Microsoft.Dafny.MemberSelectExpr value)
        {
            return new Microsoft.Dafny.MemberSelectExpr(value.Tok, TransformUnion(value.Obj), value.MemberName);
        }

        public virtual Microsoft.Dafny.SeqSelectExpr Transform(Microsoft.Dafny.SeqSelectExpr value)
        {
            return new Microsoft.Dafny.SeqSelectExpr(value.Tok, value.SelectOne, TransformUnion(value.Seq), TransformUnion(value.E0), TransformUnion(value.E1));
        }

        public virtual Microsoft.Dafny.MultiSelectExpr Transform(Microsoft.Dafny.MultiSelectExpr value)
        {
            return new Microsoft.Dafny.MultiSelectExpr(value.Tok, TransformUnion(value.Array), Transform(value.Indices, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.SeqUpdateExpr Transform(Microsoft.Dafny.SeqUpdateExpr value)
        {
            return new Microsoft.Dafny.SeqUpdateExpr(value.Tok, TransformUnion(value.Seq), TransformUnion(value.Index), TransformUnion(value.Value));
        }

        public virtual Microsoft.Dafny.ApplyExpr Transform(Microsoft.Dafny.ApplyExpr value)
        {
            return new Microsoft.Dafny.ApplyExpr(value.Tok, TransformUnion(value.Function), Transform(value.Args, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.FunctionCallExpr Transform(Microsoft.Dafny.FunctionCallExpr value)
        {
            return new Microsoft.Dafny.FunctionCallExpr(value.Tok, value.Name, TransformUnion(value.Receiver), value.OpenParen, Transform(value.ArgumentBindings, value => Transform(value)), value.AtLabel);
        }

        public virtual Microsoft.Dafny.SeqConstructionExpr Transform(Microsoft.Dafny.SeqConstructionExpr value)
        {
            return new Microsoft.Dafny.SeqConstructionExpr(value.Tok, TransformUnion(value.ExplicitElementType), TransformUnion(value.N), TransformUnion(value.Initializer));
        }

        public virtual Microsoft.Dafny.MultiSetFormingExpr Transform(Microsoft.Dafny.MultiSetFormingExpr value)
        {
            return new Microsoft.Dafny.MultiSetFormingExpr(value.Tok, TransformUnion(value.E));
        }

        public virtual Microsoft.Dafny.OldExpr Transform(Microsoft.Dafny.OldExpr value)
        {
            return new Microsoft.Dafny.OldExpr(value.Tok, TransformUnion(value.E), value.At);
        }

        public virtual Microsoft.Dafny.UnchangedExpr Transform(Microsoft.Dafny.UnchangedExpr value)
        {
            return new Microsoft.Dafny.UnchangedExpr(value.Tok, Transform(value.Frame, value => Transform(value)), value.At);
        }

        public virtual Microsoft.Dafny.BinaryExpr Transform(Microsoft.Dafny.BinaryExpr value)
        {
            return new Microsoft.Dafny.BinaryExpr(value.Tok, value.Op, TransformUnion(value.E0), TransformUnion(value.E1));
        }

        public virtual Microsoft.Dafny.TernaryExpr Transform(Microsoft.Dafny.TernaryExpr value)
        {
            return new Microsoft.Dafny.TernaryExpr(value.Tok, value.Op, TransformUnion(value.E0), TransformUnion(value.E1), TransformUnion(value.E2));
        }

        public virtual Microsoft.Dafny.LetExpr Transform(Microsoft.Dafny.LetExpr value)
        {
            return new Microsoft.Dafny.LetExpr(value.Tok, Transform(value.Lhss, value => Transform(value, value => value)), Transform(value.Rhss, value => TransformUnion(value)), TransformUnion(value.Body), value.Exact, TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.StmtExpr Transform(Microsoft.Dafny.StmtExpr value)
        {
            return new Microsoft.Dafny.StmtExpr(value.Tok, value.S, TransformUnion(value.E));
        }

        public virtual Microsoft.Dafny.ITEExpr Transform(Microsoft.Dafny.ITEExpr value)
        {
            return new Microsoft.Dafny.ITEExpr(value.Tok, value.IsBindingGuard, TransformUnion(value.Test), TransformUnion(value.Thn), TransformUnion(value.Els));
        }

        public virtual Microsoft.Dafny.MatchExpr Transform(Microsoft.Dafny.MatchExpr value)
        {
            return new Microsoft.Dafny.MatchExpr(value.Tok, TransformUnion(value.Source), Transform(value.Cases, value => Transform(value)), value.UsesOptionalBraces, TransformUnion(value.Context));
        }

        public virtual Microsoft.Dafny.BoxingCastExpr Transform(Microsoft.Dafny.BoxingCastExpr value)
        {
            return new Microsoft.Dafny.BoxingCastExpr(TransformUnion(value.E), TransformUnion(value.FromType), TransformUnion(value.ToType));
        }

        public virtual Microsoft.Dafny.UnboxingCastExpr Transform(Microsoft.Dafny.UnboxingCastExpr value)
        {
            return new Microsoft.Dafny.UnboxingCastExpr(TransformUnion(value.E), TransformUnion(value.FromType), TransformUnion(value.ToType));
        }

        public virtual Microsoft.Dafny.LetOrFailExpr Transform(Microsoft.Dafny.LetOrFailExpr value)
        {
            return new Microsoft.Dafny.LetOrFailExpr(value.Tok, Transform(value.Lhs, value => value), TransformUnion(value.Rhs), TransformUnion(value.Body));
        }

        public virtual Microsoft.Dafny.NestedMatchExpr Transform(Microsoft.Dafny.NestedMatchExpr value)
        {
            return new Microsoft.Dafny.NestedMatchExpr(value.Tok, TransformUnion(value.Source), Transform(value.Cases, value => Transform(value)), value.UsesOptionalBraces, TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.ParensExpression Transform(Microsoft.Dafny.ParensExpression value)
        {
            return new Microsoft.Dafny.ParensExpression(value.Tok, TransformUnion(value.E));
        }

        public virtual Microsoft.Dafny.DatatypeUpdateExpr Transform(Microsoft.Dafny.DatatypeUpdateExpr value) {
          throw new Exception();
          //return new Microsoft.Dafny.DatatypeUpdateExpr(value.Tok, TransformUnion(value.Root), Transform(value.Updates, value => Transform(value, value => value, value => value, value => TransformUnion(value))));
        }

        public virtual Microsoft.Dafny.DefaultValueExpression Transform(Microsoft.Dafny.DefaultValueExpression value)
        {
            return new Microsoft.Dafny.DefaultValueExpression(value.Tok, TransformUnion(value.Formal), TransformUnion(value.Receiver), Transform(value.SubstMap, value => value, value => TransformUnion(value)), Transform(value.TypeMap, value => value, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.NegationExpression Transform(Microsoft.Dafny.NegationExpression value)
        {
            return new Microsoft.Dafny.NegationExpression(value.Tok, TransformUnion(value.E));
        }

        public virtual Microsoft.Dafny.ChainingExpression Transform(Microsoft.Dafny.ChainingExpression value)
        {
            return new Microsoft.Dafny.ChainingExpression(value.Tok, Transform(value.Operands, value => TransformUnion(value)), value.Operators, value.OperatorLocs, Transform(value.PrefixLimits, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.NameSegment Transform(Microsoft.Dafny.NameSegment value)
        {
            return new Microsoft.Dafny.NameSegment(value.Tok, value.Name, Transform(value.OptTypeArguments, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.MapComprehension Transform(Microsoft.Dafny.MapComprehension value)
        {
            return new Microsoft.Dafny.MapComprehension(value.Tok, value.BodyEndTok, value.Finite, value.BoundVars, TransformUnion(value.Range), TransformUnion(value.TermLeft), TransformUnion(value.TermRight), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.LambdaExpr Transform(Microsoft.Dafny.LambdaExpr value)
        {
            return new Microsoft.Dafny.LambdaExpr(value.Tok, value.BodyEndTok, value.BoundVars, TransformUnion(value.Range), Transform(value.Reads, value => Transform(value)), TransformUnion(value.Body));
        }

        public virtual Microsoft.Dafny.LitCtx Transform(Microsoft.Dafny.LitCtx value)
        {
            return new Microsoft.Dafny.LitCtx(TransformUnion(value.Lit));
        }

        public virtual Microsoft.Dafny.IdCtx Transform(Microsoft.Dafny.IdCtx value)
        {
            return new Microsoft.Dafny.IdCtx(value.Id, Transform(value.Arguments, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.LitPattern Transform(Microsoft.Dafny.LitPattern value)
        {
            return new Microsoft.Dafny.LitPattern(value.Tok, TransformUnion(value.OrigLit), value.IsGhost);
        }

        public virtual Microsoft.Dafny.IdPattern Transform(Microsoft.Dafny.IdPattern value)
        {
            return new Microsoft.Dafny.IdPattern(value.Tok, value.Id, Transform(value.Arguments, value => TransformUnion(value)), value.IsGhost);
        }

        public virtual Microsoft.Dafny.NestedMatchCaseExpr Transform(Microsoft.Dafny.NestedMatchCaseExpr value)
        {
            return new Microsoft.Dafny.NestedMatchCaseExpr(value.Tok, TransformUnion(value.Pat), TransformUnion(value.Body), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.NestedMatchCaseStmt Transform(Microsoft.Dafny.NestedMatchCaseStmt value)
        {
            return new Microsoft.Dafny.NestedMatchCaseStmt(value.Tok, TransformUnion(value.Pat), Transform(value.Body, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.NestedMatchStmt Transform(Microsoft.Dafny.NestedMatchStmt value)
        {
            return new Microsoft.Dafny.NestedMatchStmt(value.Tok, value.EndTok, TransformUnion(value.Source), Transform(value.Cases, value => Transform(value)), value.UsesOptionalBraces, TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.TypeExpr Transform(Microsoft.Dafny.TypeExpr value)
        {
            return new Microsoft.Dafny.TypeExpr(value.Tok, TransformUnion(value.E), TransformUnion(value.T));
        }

        public virtual Microsoft.Dafny.ExprDotName Transform(Microsoft.Dafny.ExprDotName value)
        {
            return new Microsoft.Dafny.ExprDotName(value.Tok, TransformUnion(value.Lhs), value.SuffixName, Transform(value.OptTypeArguments, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.ApplySuffix Transform(Microsoft.Dafny.ApplySuffix value)
        {
            return new Microsoft.Dafny.ApplySuffix(value.Tok, value.AtTok, TransformUnion(value.Lhs), 
              Transform(value.ArgumentBindings, value => Transform(value)));
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.ExactBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.ExactBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.ExactBoundedPool(TransformUnion(value.E));
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.AllocFreeBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.AllocFreeBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.AllocFreeBoundedPool(TransformUnion(value.Type));
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.IntBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.IntBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.IntBoundedPool(TransformUnion(value.LowerBound), TransformUnion(value.UpperBound));
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.SubSetBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.SubSetBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.SubSetBoundedPool(TransformUnion(value.UpperBound), value.IsFiniteCollection);
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.SuperSetBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.SuperSetBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.SuperSetBoundedPool(TransformUnion(value.LowerBound));
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.DatatypeBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.DatatypeBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.DatatypeBoundedPool(TransformUnion(value.Decl));
        }

        public virtual Microsoft.Dafny.CalcStmt.TernaryCalcOp Transform(Microsoft.Dafny.CalcStmt.TernaryCalcOp value)
        {
            return new Microsoft.Dafny.CalcStmt.TernaryCalcOp(TransformUnion(value.Index));
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.SetBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.SetBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.SetBoundedPool(TransformUnion(value.Set), TransformUnion(value.BoundVariableType), TransformUnion(value.CollectionElementType), value.IsFiniteCollection);
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.MultiSetBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.MultiSetBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.MultiSetBoundedPool(TransformUnion(value.MultiSet), TransformUnion(value.BoundVariableType), TransformUnion(value.CollectionElementType));
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.MapBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.MapBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.MapBoundedPool(TransformUnion(value.Map), TransformUnion(value.BoundVariableType), TransformUnion(value.CollectionElementType), value.IsFiniteCollection);
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.SeqBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.SeqBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.SeqBoundedPool(TransformUnion(value.Seq), TransformUnion(value.BoundVariableType), TransformUnion(value.CollectionElementType));
        }

        public virtual Microsoft.Dafny.ModuleDefinition Transform(Microsoft.Dafny.ModuleDefinition value)
        {
            return new Microsoft.Dafny.ModuleDefinition(value.Tok, value.Name, value.PrefixIds, value.IsAbstract, value.IsFacade, Transform(value.RefinementQId), TransformUnion(value.EnclosingModule), TransformUnion(value.Attributes), value.IsBuiltinName, value.IsToBeVerified, value.IsToBeCompiled, Transform(value.TopLevelDecls, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.ModuleSignature Transform(Microsoft.Dafny.ModuleSignature value)
        {
            return new Microsoft.Dafny.ModuleSignature();
        }

        public virtual Microsoft.Dafny.BuiltIns Transform(Microsoft.Dafny.BuiltIns value)
        {
            return new Microsoft.Dafny.BuiltIns();
        }

        public virtual Microsoft.Dafny.ModuleQualifiedId Transform(Microsoft.Dafny.ModuleQualifiedId value)
        {
            return new Microsoft.Dafny.ModuleQualifiedId(value.Path);
        }

        public virtual Microsoft.Dafny.ExportSignature Transform(Microsoft.Dafny.ExportSignature value)
        {
            return new Microsoft.Dafny.ExportSignature(value.ClassIdTok, value.ClassId, value.Tok, value.Id, value.Opaque);
        }

        public virtual Microsoft.Dafny.Specification<ToT> Transform<FromT, ToT>(Microsoft.Dafny.Specification<FromT> value, Func<FromT, ToT> convT)
          where ToT : class
          where FromT : class
        {
            return new Microsoft.Dafny.Specification<ToT>(Transform(value.Expressions, convT), TransformUnion(value.Attributes));
        }

        public virtual Microsoft.Dafny.FrameExpression Transform(Microsoft.Dafny.FrameExpression value)
        {
            return new Microsoft.Dafny.FrameExpression(value.Tok, TransformUnion(value.E), value.FieldName);
        }

        public virtual Microsoft.Dafny.AttributedExpression Transform(Microsoft.Dafny.AttributedExpression value)
        {
            return new Microsoft.Dafny.AttributedExpression(TransformUnion(value.E));
        }

        public virtual Microsoft.Dafny.LocalVariable Transform(Microsoft.Dafny.LocalVariable value)
        {
            return new Microsoft.Dafny.LocalVariable(value.Tok, value.EndTok, value.Name, TransformUnion(value.Type), value.IsGhost);
        }

        public virtual Microsoft.Dafny.CasePattern<ToVT> Transform<FromVT, ToVT>(Microsoft.Dafny.CasePattern<FromVT> value, Func<FromVT, ToVT> convVT)
          where ToVT : IVariable 
          where FromVT : IVariable
        {
            return new Microsoft.Dafny.CasePattern<ToVT>(value.Tok, value.Id, Transform(value.Arguments, value => Transform(value, value => convVT(value))));
        }

        public virtual Microsoft.Dafny.ActualBindings Transform(Microsoft.Dafny.ActualBindings value)
        {
            return new Microsoft.Dafny.ActualBindings(Transform(value.ArgumentBindings, value => Transform(value)));
        }

        public virtual Microsoft.Dafny.GuardedAlternative Transform(Microsoft.Dafny.GuardedAlternative value)
        {
            return new Microsoft.Dafny.GuardedAlternative(value.Tok, value.IsBindingGuard, TransformUnion(value.Guard), Transform(value.Body, value => TransformUnion(value)));
        }

        public virtual Microsoft.Dafny.Attributes Transform(Microsoft.Dafny.Attributes value)
        {
            return new Microsoft.Dafny.Attributes(value.Name, Transform(value.Args, value => TransformUnion(value)), TransformUnion(value.Prev));
        }

        public virtual Microsoft.Dafny.ExpressionPair Transform(Microsoft.Dafny.ExpressionPair value)
        {
            return new Microsoft.Dafny.ExpressionPair(TransformUnion(value.A), TransformUnion(value.B));
        }

        public virtual Microsoft.Dafny.ActualBinding Transform(Microsoft.Dafny.ActualBinding value)
        {
            return new Microsoft.Dafny.ActualBinding(value.FormalParameterName, TransformUnion(value.Actual), value.IsGhost);
        }

        public virtual (K2, V2) Transform<K, V, K2, V2>((K, V) value, Func<K, K2> mapItem1, Func<V, V2> mapItem2)
        {
            return (mapItem1(value.Item1), mapItem2(value.Item2));
        }
        public virtual List<U> Transform<T, U>(IEnumerable<T> value, Func<T, U> f)
        {
            return value.Select(f).ToList();
        }
        public virtual Dictionary<K2, V2> Transform<K, V, K2, V2>(IReadOnlyDictionary<K, V> value, Func<K, K2> mapKey, Func<V, V2> mapValue)
        {
            return value.ToDictionary(kv => mapKey(kv.Key), kv => mapValue(kv.Value));
        }
        public virtual Dictionary<K2, V2> Transform<K, V, K2, V2>(IDictionary<K, V> value, Func<K, K2> mapKey, Func<V, V2> mapValue)
        {
            return value.ToDictionary(kv => mapKey(kv.Key), kv => mapValue(kv.Value));
        }
    }
}