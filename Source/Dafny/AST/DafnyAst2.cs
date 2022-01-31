#define TI_DEBUG_PRINT
//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
// Copyright by the contributors to the Dafny Project
// SPDX-License-Identifier: MIT
//
//-----------------------------------------------------------------------------
using System;
using System.Collections;
using System.Text;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.Numerics;
using System.Linq;
using Microsoft.Boogie;
using System.Diagnostics;
using Microsoft.Dafny;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Microsoft.Dafny.V4
{
    public class SpecialField : Field
    {
        public enum ID
        {
            UseIdParam,  // IdParam is a string
            ArrayLength,  // IdParam is null for .Length; IdParam is an int "x" for GetLength(x)
            ArrayLengthInt,  // same as ArrayLength, but produces int instead of BigInteger
            Floor,
            IsLimit,
            IsSucc,
            Offset,
            IsNat,
            Keys,
            Values,
            Items,
            Reads,
            Modifies,
            New,
        }
        public readonly ID SpecialId;
        public readonly object IdParam;
        public SpecialField(IToken tok, string name, ID specialId, object idParam, bool isMutable, bool isUserMutable, Type type, Attributes attributes)
          : this(tok, name, specialId, idParam, false, isMutable, isUserMutable, type, attributes)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(!isUserMutable || isMutable);
            Contract.Requires(type != null);
        }

        public SpecialField(IToken tok, string name, ID specialId, object idParam, bool hasStaticKeyword, bool isMutable, bool isUserMutable, Type type, Attributes attributes)
          : base(tok, name, hasStaticKeyword, isMutable, isUserMutable, type, attributes)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(!isUserMutable || isMutable);
            Contract.Requires(type != null);

            SpecialId = specialId;
            IdParam = idParam;
        }

        public override string FullName
        {
            get
            {
                Contract.Ensures(Contract.Result<string>() != null);
                return EnclosingClass != null ? EnclosingClass.FullName + "." + Name : Name;
            }
        }

        public override string FullSanitizedName
        {
            get
            {
                Contract.Ensures(Contract.Result<string>() != null);
                return EnclosingClass != null ? EnclosingClass.FullSanitizedName + "." + CompileName : CompileName;
            }
        }

        public override string FullSanitizedRefinementName
        {
            get
            {
                Contract.Ensures(Contract.Result<string>() != null);
                return EnclosingClass != null ? EnclosingClass.FullSanitizedRefinementName + "." + CompileName : CompileName;
            }
        }

        public override string FullNameInContext(ModuleDefinition context)
        {
            Contract.Ensures(Contract.Result<string>() != null);
            return EnclosingClass != null ? EnclosingClass.FullNameInContext(context) + "." + Name : Name;
        }

        public override string CompileName
        {
            get
            {
                Contract.Ensures(Contract.Result<string>() != null);
                return EnclosingClass != null ? base.CompileName : Name;
            }
        }

        public override string FullCompileName
        {
            get
            {
                Contract.Ensures(Contract.Result<string>() != null);
                var cn = Declaration.IdProtect(CompileName);
                return EnclosingClass != null ? EnclosingClass.FullCompileName + "." + cn : cn;
            }
        }
    }

    public class DatatypeDestructor : SpecialField
    {
        public readonly List<DatatypeCtor> EnclosingCtors = new List<DatatypeCtor>();  // is always a nonempty list
        public readonly List<Formal> CorrespondingFormals = new List<Formal>();  // is always a nonempty list
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(EnclosingCtors != null);
            Contract.Invariant(CorrespondingFormals != null);
            Contract.Invariant(EnclosingCtors.Count > 0);
            Contract.Invariant(EnclosingCtors.Count == CorrespondingFormals.Count);
        }

        public string CompiledName => (string)this.IdParam;

        public DatatypeDestructor(IToken tok, List<DatatypeCtor> enclosingCtors, List<Formal> correspondingFormals, string name, string compiledName, Type type, Attributes attributes)
          : base(tok, name, SpecialField.ID.UseIdParam, compiledName, false, false, type, attributes)
        {
            this.EnclosingCtors = enclosingCtors;
            this.CorrespondingFormals = correspondingFormals;
        }

        public DatatypeDestructor(IToken tok, DatatypeCtor enclosingCtor, Formal correspondingFormal, string name, string compiledName, Type type, Attributes attributes)
          : base(tok, name, SpecialField.ID.UseIdParam, compiledName, false, false, type, attributes)
        {
            Contract.Requires(tok != null);
            Contract.Requires(enclosingCtor != null);
            Contract.Requires(correspondingFormal != null);
            Contract.Requires(name != null);
            Contract.Requires(type != null);
            EnclosingCtors.Add(enclosingCtor);  // more enclosing constructors may be added later during resolution
            CorrespondingFormals.Add(correspondingFormal);  // more corresponding formals may be added later during resolution
        }

        /// <summary>
        /// To be called only by the resolver. Called to share this datatype destructor between multiple constructors
        /// of the same datatype.
        /// </summary>
        internal void AddAnotherEnclosingCtor(DatatypeCtor ctor, Formal formal)
        {
            Contract.Requires(ctor != null);
            Contract.Requires(formal != null);
            EnclosingCtors.Add(ctor);  // more enclosing constructors may be added later during resolution
            CorrespondingFormals.Add(formal);  // more corresponding formals may be added later during resolution
        }

        internal string EnclosingCtorNames(string grammaticalConjunction)
        {
            Contract.Requires(grammaticalConjunction != null);
            return PrintableCtorNameList(EnclosingCtors, grammaticalConjunction);
        }

        static internal string PrintableCtorNameList(List<DatatypeCtor> ctors, string grammaticalConjunction)
        {
            Contract.Requires(ctors != null);
            Contract.Requires(grammaticalConjunction != null);
            var n = ctors.Count;
            if (n == 1)
            {
                return string.Format("'{0}'", ctors[0].Name);
            }
            else if (n == 2)
            {
                return string.Format("'{0}' {1} '{2}'", ctors[0].Name, grammaticalConjunction, ctors[1].Name);
            }
            else
            {
                var s = "";
                for (int i = 0; i < n - 1; i++)
                {
                    s += string.Format("'{0}', ", ctors[i].Name);
                }
                return s + string.Format("{0} '{1}'", grammaticalConjunction, ctors[n - 1].Name);
            }
        }
    }

    public class ConstantField : SpecialField, ICallable
    {
        IToken ICallable.Tok => base.Tok;
        public override string WhatKind { get { return "const field"; } }
        public readonly Expression Rhs;
        public ConstantField(IToken tok, string name, Expression/*?*/ rhs, bool hasStaticKeyword, Type type, Attributes attributes)
          : base(tok, name, SpecialField.ID.UseIdParam, NonglobalVariable.CompilerizeName(name), hasStaticKeyword, false, false, type, attributes)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(type != null);
            this.Rhs = rhs;
        }

        public override bool CanBeRevealed()
        {
            return true;
        }

        //
        public new bool IsGhost { get { return this.isGhost; } }
        public List<TypeParameter> TypeArgs { get { return new List<TypeParameter>(); } }
        public List<Formal> Ins { get { return new List<Formal>(); } }
        public ModuleDefinition EnclosingModule { get { return this.EnclosingClass.EnclosingModuleDefinition; } }
        public bool MustReverify { get { return false; } }
        public bool AllowsNontermination { get { throw new cce.UnreachableException(); } }
        public string NameRelativeToModule
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
        public Specification<Expression> Decreases { get { throw new cce.UnreachableException(); } }
        public bool InferredDecreases
        {
            get { throw new cce.UnreachableException(); }
            set { throw new cce.UnreachableException(); }
        }
        public bool AllowsAllocation => true;
    }

    public abstract class MemberDecl : Declaration
    {
        public abstract string WhatKind { get; }
        public readonly bool HasStaticKeyword;
        public virtual bool IsStatic
        {
            get
            {
                return HasStaticKeyword || (EnclosingClass is ClassDecl && ((ClassDecl)EnclosingClass).IsDefaultClass);
            }
        }
        protected readonly bool isGhost;
        public bool IsGhost { get { return isGhost; } }

        /// <summary>
        /// The term "instance independent" can be confusing. It means that the constant does not get its value in
        /// a constructor. (But the RHS of the const's declaration may mention "this".)
        /// </summary>
        public bool IsInstanceIndependentConstant => this is ConstantField cf && cf.Rhs != null;

        public TopLevelDecl EnclosingClass;  // filled in during resolution
        public MemberDecl RefinementBase;  // filled in during the pre-resolution refinement transformation; null if the member is new here
        public MemberDecl OverriddenMember;  // filled in during resolution; non-null if the member overrides a member in a parent trait
        public virtual bool IsOverrideThatAddsBody => OverriddenMember != null;

        /// <summary>
        /// Returns "true" if "this" is a (possibly transitive) override of "possiblyOverriddenMember".
        /// </summary>
        public bool Overrides(MemberDecl possiblyOverriddenMember)
        {
            Contract.Requires(possiblyOverriddenMember != null);
            for (var th = this; th != null; th = th.OverriddenMember)
            {
                if (th == possiblyOverriddenMember)
                {
                    return true;
                }
            }
            return false;
        }

        public MemberDecl(IToken tok, string name, bool hasStaticKeyword, Attributes attributes, bool isRefining)
          : base(tok, name, attributes, isRefining)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            HasStaticKeyword = hasStaticKeyword;
            this.isGhost = isGhost;
        }
        /// <summary>
        /// Returns className+"."+memberName.  Available only after resolution.
        /// </summary>
        public virtual string FullDafnyName
        {
            get
            {
                Contract.Requires(EnclosingClass != null);
                Contract.Ensures(Contract.Result<string>() != null);
                string n = EnclosingClass.FullDafnyName;
                return (n.Length == 0 ? n : (n + ".")) + Name;
            }
        }
        public virtual string FullName
        {
            get
            {
                Contract.Requires(EnclosingClass != null);
                Contract.Ensures(Contract.Result<string>() != null);

                return EnclosingClass.FullName + "." + Name;
            }
        }
        public virtual string FullSanitizedName
        {
            get
            {
                Contract.Requires(EnclosingClass != null);
                Contract.Ensures(Contract.Result<string>() != null);

                if (Name == "requires")
                {
                    return Translator.Requires(((ArrowTypeDecl)EnclosingClass).Arity);
                }
                else if (Name == "reads")
                {
                    return Translator.Reads(((ArrowTypeDecl)EnclosingClass).Arity);
                }
                else
                {
                    return EnclosingClass.FullSanitizedName + "." + CompileName;
                }
            }
        }
        public virtual string FullSanitizedRefinementName
        {
            get
            {
                Contract.Requires(EnclosingClass != null);
                Contract.Ensures(Contract.Result<string>() != null);

                if (Name == "requires")
                {
                    return Translator.Requires(((ArrowTypeDecl)EnclosingClass).Arity);
                }
                else if (Name == "reads")
                {
                    return Translator.Reads(((ArrowTypeDecl)EnclosingClass).Arity);
                }
                else
                {
                    return EnclosingClass.FullSanitizedRefinementName + "." + CompileName;
                }
            }
        }
        public virtual string FullNameInContext(ModuleDefinition context)
        {
            Contract.Requires(EnclosingClass != null);
            Contract.Ensures(Contract.Result<string>() != null);

            return EnclosingClass.FullNameInContext(context) + "." + Name;
        }
        public override string CompileName
        {
            get
            {
                var nm = base.CompileName;
                if (this.Name == EnclosingClass.Name)
                {
                    nm = "_" + nm;
                }
                return nm;
            }
        }
        public virtual string FullCompileName
        {
            get
            {
                Contract.Requires(EnclosingClass != null);
                Contract.Ensures(Contract.Result<string>() != null);

                return EnclosingClass.FullCompileName + "." + Declaration.IdProtect(CompileName);
            }
        }
        public virtual IEnumerable<Expression> SubExpressions
        {
            get
            {
                yield break;
            }
        }
    }

    public class Field : MemberDecl
    {
        public override string WhatKind { get { return "field"; } }
        public readonly bool IsMutable;  // says whether or not the field can ever change values
        public readonly bool IsUserMutable;  // says whether or not code is allowed to assign to the field (IsUserMutable implies IsMutable)
        public readonly Type Type;
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(Type != null);
            Contract.Invariant(!IsUserMutable || IsMutable);  // IsUserMutable ==> IsMutable
        }

        public Field(IToken tok, string name, Type type, Attributes attributes)
          : this(tok, name, false, true, true, type, attributes)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(type != null);
        }

        public Field(IToken tok, string name, bool hasStaticKeyword, bool isMutable, bool isUserMutable, Type type, Attributes attributes)
          : base(tok, name, hasStaticKeyword, attributes, false)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(type != null);
            Contract.Requires(!isUserMutable || isMutable);
            IsMutable = isMutable;
            IsUserMutable = isUserMutable;
            Type = type;
        }
    }

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
        public readonly List<AttributedExpression> Req;
        public readonly Specification<FrameExpression> Mod;
        public readonly List<AttributedExpression> Ens;
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
                foreach (var e in Req)
                {
                    yield return e.E;
                }
                foreach (var e in Mod.Expressions)
                {
                    yield return e.E;
                }
                foreach (var e in Ens)
                {
                    yield return e.E;
                }
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
            Contract.Invariant(cce.NonNullElements(Req));
            Contract.Invariant(Mod != null);
            Contract.Invariant(cce.NonNullElements(Ens));
            Contract.Invariant(Decreases != null);
        }

        public Method(IToken tok, string name, bool hasStaticKeyword, [Captured] List<TypeParameter> typeArgs, [Captured] List<Formal> ins, [Captured] List<Formal> outs, [Captured] List<AttributedExpression> req, [Captured] Specification<FrameExpression> mod, [Captured] List<AttributedExpression> ens, [Captured] Specification<Expression> decreases, [Captured] BlockStmt body, Attributes attributes, IToken signatureEllipsis, bool isByMethod = false)
          : base(tok, name, hasStaticKeyword, attributes, signatureEllipsis != null)
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
            this.Req = req;
            this.Mod = mod;
            this.Ens = ens;
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
        public Constructor(IToken tok, string name, List<TypeParameter> typeArgs, List<Formal> ins, List<AttributedExpression> req, [Captured] Specification<FrameExpression> mod, List<AttributedExpression> ens, Specification<Expression> decreases, DividedBlockStmt dividedBody, Attributes attributes, IToken signatureEllipsis)
          : base(tok, name, false, typeArgs, ins, new List<Formal>(), req, mod, ens, decreases, dividedBody, attributes, signatureEllipsis)
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

    public class Function : MemberDecl, TypeParameter.ParentType, ICallable
    {
        public override string WhatKind { get { return "function"; } }
        public override bool CanBeRevealed() { return true; }
        public bool IsRecursive;  // filled in during resolution
        public TailStatus TailRecursion = TailStatus.NotTailRecursive;  // filled in during resolution; NotTailRecursive = no tail recursion; TriviallyTailRecursive is never used here
        public bool IsTailRecursive => TailRecursion != TailStatus.NotTailRecursive;
        public bool IsAccumulatorTailRecursive => IsTailRecursive && TailRecursion != Function.TailStatus.TailRecursive;
        public bool IsFueled;  // filled in during resolution if anyone tries to adjust this function's fuel
        public readonly List<TypeParameter> TypeArgs;
        public readonly List<Formal> Formals;
        public readonly Formal Result;
        public readonly Type ResultType;
        public readonly List<AttributedExpression> Req;
        public readonly List<FrameExpression> Reads;
        public readonly List<AttributedExpression> Ens;
        public readonly Specification<Expression> Decreases;
        public Expression Body;  // an extended expression; Body is readonly after construction, except for any kind of rewrite that may take place around the time of resolution
        public IToken/*?*/ ByMethodTok; // null iff ByMethodBody is null
        public BlockStmt/*?*/ ByMethodBody;
        public Method/*?*/ ByMethodDecl; // filled in by resolution, if ByMethodBody is non-null
        public bool SignatureIsOmitted { get { return SignatureEllipsis != null; } }  // is "false" for all Function objects that survive into resolution
        public readonly IToken SignatureEllipsis;
        public Function OverriddenFunction;
        public Function Original => OverriddenFunction == null ? this : OverriddenFunction.Original;
        public override bool IsOverrideThatAddsBody => base.IsOverrideThatAddsBody && Body != null;
        public bool AllowsAllocation => true;

        public bool containsQuantifier;
        public bool ContainsQuantifier
        {
            set { containsQuantifier = value; }
            get { return containsQuantifier; }
        }

        public enum TailStatus
        {
            TriviallyTailRecursive, // contains no recursive calls (in non-ghost expressions)
            TailRecursive, // all recursive calls (in non-ghost expressions) are tail calls
            NotTailRecursive, // contains some non-ghost recursive call outside of a tail-call position
                              // E + F or F + E, where E has no tail call and F is a tail call
            Accumulate_Add,
            AccumulateRight_Sub,
            Accumulate_Mul,
            Accumulate_SetUnion,
            AccumulateRight_SetDifference,
            Accumulate_MultiSetUnion,
            AccumulateRight_MultiSetDifference,
            AccumulateLeft_Concat,
            AccumulateRight_Concat,
        }

        public override IEnumerable<Expression> SubExpressions
        {
            get
            {
                foreach (var formal in Formals.Where(f => f.DefaultValue != null))
                {
                    yield return formal.DefaultValue;
                }
                foreach (var e in Req)
                {
                    yield return e.E;
                }
                foreach (var e in Reads)
                {
                    yield return e.E;
                }
                foreach (var e in Ens)
                {
                    yield return e.E;
                }
                foreach (var e in Decreases.Expressions)
                {
                    yield return e;
                }
                if (Body != null)
                {
                    yield return Body;
                }
            }
        }

        public Type GetMemberType(ArrowTypeDecl atd)
        {
            Contract.Requires(atd != null);
            Contract.Requires(atd.Arity == Formals.Count);

            // Note, the following returned type can contain type parameters from the function and its enclosing class
            return new ArrowType(Tok, atd, Formals.ConvertAll(f => f.Type), ResultType);
        }

        public bool AllowsNontermination
        {
            get
            {
                return Contract.Exists(Decreases.Expressions, e => e is WildcardExpr);
            }
        }

        /// <summary>
        /// The "AllCalls" field is used for non-ExtremePredicate, non-PrefixPredicate functions only (so its value should not be relied upon for ExtremePredicate and PrefixPredicate functions).
        /// It records all function calls made by the Function, including calls made in the body as well as in the specification.
        /// The field is filled in during resolution (and used toward the end of resolution, to attach a helpful "decreases" prefix to functions in clusters
        /// with co-recursive calls.
        /// </summary>
        public readonly List<FunctionCallExpr> AllCalls = new List<FunctionCallExpr>();
        public enum CoCallClusterInvolvement
        {
            None,  // the SCC containing the function does not involve any co-recursive calls
            IsMutuallyRecursiveTarget,  // the SCC contains co-recursive calls, and this function is the target of some non-self recursive call
            CoRecursiveTargetAllTheWay,  // the SCC contains co-recursive calls, and this function is the target only of self-recursive calls and co-recursive calls
        }
        public CoCallClusterInvolvement CoClusterTarget = CoCallClusterInvolvement.None;

        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(cce.NonNullElements(TypeArgs));
            Contract.Invariant(cce.NonNullElements(Formals));
            Contract.Invariant(ResultType != null);
            Contract.Invariant(cce.NonNullElements(Req));
            Contract.Invariant(cce.NonNullElements(Reads));
            Contract.Invariant(cce.NonNullElements(Ens));
            Contract.Invariant(Decreases != null);
        }

        /// <summary>
        /// Note, functions are "ghost" by default; a non-ghost function is called a "function method".
        /// </summary>
        public Function(IToken tok, string name, bool hasStaticKeyword, List<TypeParameter> typeArgs, List<Formal> formals, Formal result, Type resultType, List<AttributedExpression> req, List<FrameExpression> reads, List<AttributedExpression> ens, Specification<Expression> decreases, Expression/*?*/ body, IToken/*?*/ byMethodTok, BlockStmt/*?*/ byMethodBody, Attributes attributes, IToken/*?*/ signatureEllipsis)
          : base(tok, name, hasStaticKeyword, attributes, signatureEllipsis != null)
        {

            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(cce.NonNullElements(typeArgs));
            Contract.Requires(cce.NonNullElements(formals));
            Contract.Requires(resultType != null);
            Contract.Requires(cce.NonNullElements(req));
            Contract.Requires(cce.NonNullElements(reads));
            Contract.Requires(cce.NonNullElements(ens));
            Contract.Requires(decreases != null);
            Contract.Requires(byMethodBody == null || (!isGhost && body != null)); // function-by-method has a ghost expr and non-ghost stmt, but to callers appears like a functiion-method
            this.IsFueled = false;  // Defaults to false.  Only set to true if someone mentions this function in a fuel annotation
            this.TypeArgs = typeArgs;
            this.Formals = formals;
            this.Result = result;
            this.ResultType = result != null ? result.Type : resultType;
            this.Req = req;
            this.Reads = reads;
            this.Ens = ens;
            this.Decreases = decreases;
            this.Body = body;
            this.ByMethodTok = byMethodTok;
            this.ByMethodBody = byMethodBody;
            this.SignatureEllipsis = signatureEllipsis;

            if (attributes != null)
            {
                List<Expression> args = Attributes.FindExpressions(attributes, "fuel");
                if (args != null)
                {
                    if (args.Count == 1)
                    {
                        LiteralExpr literal = args[0] as LiteralExpr;
                        if (literal != null && literal.Value is BigInteger)
                        {
                            this.IsFueled = true;
                        }
                    }
                    else if (args.Count == 2)
                    {
                        LiteralExpr literalLow = args[0] as LiteralExpr;
                        LiteralExpr literalHigh = args[1] as LiteralExpr;

                        if (literalLow != null && literalLow.Value is BigInteger && literalHigh != null && literalHigh.Value is BigInteger)
                        {
                            this.IsFueled = true;
                        }
                    }
                }
            }
        }

        bool ICodeContext.IsGhost { get { return this.IsGhost; } }
        List<TypeParameter> ICodeContext.TypeArgs { get { return this.TypeArgs; } }
        List<Formal> ICodeContext.Ins { get { return this.Formals; } }
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
        ModuleDefinition ICodeContext.EnclosingModule { get { return this.EnclosingClass.EnclosingModuleDefinition; } }
        bool ICodeContext.MustReverify { get { return false; } }

        [Pure]
        public bool IsFuelAware() { return IsRecursive || IsFueled || (OverriddenFunction != null && OverriddenFunction.IsFuelAware()); }
        public virtual bool ReadsHeap { get { return Reads.Count != 0; } }
    }

    public class SpecialFunction : Function, ICodeContext, ICallable
    {
        public readonly ModuleDefinition Module;
        public SpecialFunction(IToken tok, string name, ModuleDefinition module, bool hasStaticKeyword, List<TypeParameter> typeArgs, List<Formal> formals, Type resultType, List<AttributedExpression> req, List<FrameExpression> reads, List<AttributedExpression> ens, Specification<Expression> decreases, Expression body, Attributes attributes, IToken signatureEllipsis)
          : base(tok, name, hasStaticKeyword, typeArgs, formals, null, resultType, req, reads, ens, decreases, body, null, null, attributes, signatureEllipsis)
        {
            Module = module;
        }
        ModuleDefinition ICodeContext.EnclosingModule { get { return this.Module; } }
        string ICallable.NameRelativeToModule { get { return Name; } }
    }
    public class ForwardTransformer
    {
        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.Attributes TransformUnion(Microsoft.Dafny.Attributes value) =>
        value switch
        {
            Microsoft.Dafny.UserSuppliedAttributes subType => subType,
            _ => Transform(value)
        };

        // Transforming because type-chain [Microsoft.Dafny.NonProxyType, Microsoft.Dafny.UserDefinedType, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.Type TransformUnion(Microsoft.Dafny.Type value) =>
        value switch
        {
            Microsoft.Dafny.ArtificialType subType => subType,
            Microsoft.Dafny.NonProxyType subType => TransformUnion(subType),
            Microsoft.Dafny.TypeProxy subType => TransformUnion(subType),
            Microsoft.Dafny.Resolver_IdentifierExpr.ResolverType subType => subType,
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.UserDefinedType, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.NonProxyType TransformUnion(Microsoft.Dafny.NonProxyType value) =>
        value switch
        {
            Microsoft.Dafny.BasicType subType => subType,
            Microsoft.Dafny.SelfType subType => Transform(subType),
            Microsoft.Dafny.CollectionType subType => TransformUnion(subType),
            Microsoft.Dafny.UserDefinedType subType => TransformUnion(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.UserDefinedType TransformUnion(Microsoft.Dafny.UserDefinedType value) =>
        value switch
        {
            Microsoft.Dafny.ArrowType subType => subType,
            _ => Transform(value)
        };

        // Transforming because type-chain [Microsoft.Dafny.Type, Microsoft.Dafny.NonProxyType, Microsoft.Dafny.UserDefinedType, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.CollectionType TransformUnion(Microsoft.Dafny.CollectionType value) =>
        value switch
        {
            Microsoft.Dafny.SetType subType => subType,
            Microsoft.Dafny.MultiSetType subType => subType,
            Microsoft.Dafny.SeqType subType => subType,
            Microsoft.Dafny.MapType subType => Transform(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.Type, Microsoft.Dafny.NonProxyType, Microsoft.Dafny.UserDefinedType, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.TypeProxy TransformUnion(Microsoft.Dafny.TypeProxy value) =>
        value switch
        {
            Microsoft.Dafny.InferredTypeProxy subType => subType,
            Microsoft.Dafny.ParamTypeProxy subType => subType,
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.ModuleDecl, Microsoft.Dafny.ModuleSignature, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.TopLevelDecl TransformUnion(Microsoft.Dafny.TopLevelDecl value) =>
        value switch
        {
            Microsoft.Dafny.TypeParameter subType => subType,
            Microsoft.Dafny.ModuleDecl subType => TransformUnion(subType),
            Microsoft.Dafny.TopLevelDeclWithMembers subType => TransformUnion(subType),
            Microsoft.Dafny.ValuetypeDecl subType => Transform(subType),
            Microsoft.Dafny.TypeSynonymDeclBase subType => TransformUnion(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.ModuleSignature, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ModuleDecl TransformUnion(Microsoft.Dafny.ModuleDecl value) =>
        value switch
        {
            Microsoft.Dafny.LiteralModuleDecl subType => Transform(subType),
            Microsoft.Dafny.AliasModuleDecl subType => Transform(subType),
            Microsoft.Dafny.AbstractModuleDecl subType => Transform(subType),
            Microsoft.Dafny.ModuleExportDecl subType => Transform(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.TopLevelDecl, Microsoft.Dafny.ModuleDecl, Microsoft.Dafny.ModuleSignature, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ModuleDefinition TransformUnion(Microsoft.Dafny.ModuleDefinition value) =>
        value switch
        {
            Microsoft.Dafny.DefaultModuleDecl subType => subType,
            _ => Transform(value)
        };

        // Transforming because type-chain [Microsoft.Dafny.Attributes, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.Declaration TransformUnion(Microsoft.Dafny.Declaration value) =>
        value switch
        {
            Microsoft.Dafny.TopLevelDecl subType => TransformUnion(subType),
            Microsoft.Dafny.DatatypeCtor subType => Transform(subType),
            Microsoft.Dafny.MemberDecl subType => TransformUnion(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.ArrowTypeDecl, Microsoft.Dafny.Function] was transformed.
        public virtual Microsoft.Dafny.ClassDecl TransformUnion(Microsoft.Dafny.ClassDecl value) =>
        value switch
        {
            Microsoft.Dafny.TraitDecl subType => subType,
            Microsoft.Dafny.DefaultClassDecl subType => subType,
            Microsoft.Dafny.ArrayClassDecl subType => subType,
            Microsoft.Dafny.ArrowTypeDecl subType => Transform(subType),
            Microsoft.Dafny.IteratorDecl subType => Transform(subType),
            _ => Transform(value)
        };

        // Transforming because type-chain [Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.TopLevelDeclWithMembers TransformUnion(Microsoft.Dafny.TopLevelDeclWithMembers value) =>
        value switch
        {
            Microsoft.Dafny.ClassDecl subType => TransformUnion(subType),
            Microsoft.Dafny.DatatypeDecl subType => TransformUnion(subType),
            Microsoft.Dafny.OpaqueTypeDecl subType => subType,
            Microsoft.Dafny.NewtypeDecl subType => Transform(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.DatatypeCtor, Microsoft.Dafny.Formal, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.DatatypeDecl TransformUnion(Microsoft.Dafny.DatatypeDecl value) =>
        value switch
        {
            Microsoft.Dafny.IndDatatypeDecl subType => TransformUnion(subType),
            Microsoft.Dafny.CoDatatypeDecl subType => subType,
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.DatatypeCtor, Microsoft.Dafny.Formal, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.IndDatatypeDecl TransformUnion(Microsoft.Dafny.IndDatatypeDecl value) =>
        value switch
        {
            Microsoft.Dafny.TupleTypeDecl subType => subType,
            _ => Transform(value)
        };

        // Transforming because this type was [indirectly] mutated.
        public virtual Microsoft.Dafny.V4.MemberDecl TransformUnion(Microsoft.Dafny.MemberDecl value) =>
        value switch
        {
            Microsoft.Dafny.Field subType => TransformUnion(subType),
            Microsoft.Dafny.Function subType => TransformUnion(subType),
            Microsoft.Dafny.Method subType => TransformUnion(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because this type was [indirectly] mutated.
        public virtual Microsoft.Dafny.V4.Function TransformUnion(Microsoft.Dafny.Function value) =>
        value switch
        {
            Microsoft.Dafny.SpecialFunction subType => Transform(subType),
            _ => Transform(value)
        };

        // Transforming because this type was [indirectly] mutated.
        public virtual Microsoft.Dafny.V4.Field TransformUnion(Microsoft.Dafny.Field value) =>
        value switch
        {
            Microsoft.Dafny.SpecialField subType => TransformUnion(subType),
            _ => Transform(value)
        };

        // Transforming because this type was [indirectly] mutated.
        public virtual Microsoft.Dafny.V4.SpecialField TransformUnion(Microsoft.Dafny.SpecialField value) =>
        value switch
        {
            Microsoft.Dafny.DatatypeDestructor subType => Transform(subType),
            Microsoft.Dafny.ConstantField subType => Transform(subType),
            _ => Transform(value)
        };

        // Transforming because type-chain [Microsoft.Dafny.TypeSynonymDecl, Microsoft.Dafny.SubsetTypeDecl, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.TypeSynonymDeclBase TransformUnion(Microsoft.Dafny.TypeSynonymDeclBase value) =>
        value switch
        {
            Microsoft.Dafny.TypeSynonymDecl subType => TransformUnion(subType),
            Microsoft.Dafny.InternalTypeSynonymDecl subType => subType,
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.SubsetTypeDecl, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.TypeSynonymDecl TransformUnion(Microsoft.Dafny.TypeSynonymDecl value) =>
        value switch
        {
            Microsoft.Dafny.SubsetTypeDecl subType => TransformUnion(subType),
            _ => Transform(value)
        };

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.SubsetTypeDecl TransformUnion(Microsoft.Dafny.SubsetTypeDecl value) =>
        value switch
        {
            Microsoft.Dafny.NonNullTypeDecl subType => Transform(subType),
            _ => Transform(value)
        };

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.Formal TransformUnion(Microsoft.Dafny.Formal value) =>
        value switch
        {
            Microsoft.Dafny.ImplicitFormal subType => subType,
            _ => Transform(value)
        };

        // Transforming because this type was [indirectly] mutated.
        public virtual Microsoft.Dafny.V4.Method TransformUnion(Microsoft.Dafny.Method value) =>
        value switch
        {
            Microsoft.Dafny.Constructor subType => Transform(subType),
            _ => Transform(value)
        };

        // Transforming because type-chain [Microsoft.Dafny.ConcreteUpdateStatement, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
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
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.PredicateStmt TransformUnion(Microsoft.Dafny.PredicateStmt value) =>
        value switch
        {
            Microsoft.Dafny.AssertStmt subType => Transform(subType),
            Microsoft.Dafny.ExpectStmt subType => Transform(subType),
            Microsoft.Dafny.AssumeStmt subType => subType,
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.UpdateStmt, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ProduceStmt TransformUnion(Microsoft.Dafny.ProduceStmt value) =>
        value switch
        {
            Microsoft.Dafny.ReturnStmt subType => subType,
            Microsoft.Dafny.YieldStmt subType => subType,
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.Attributes, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.AssignmentRhs TransformUnion(Microsoft.Dafny.AssignmentRhs value) =>
        value switch
        {
            Microsoft.Dafny.ExprRhs subType => Transform(subType),
            Microsoft.Dafny.TypeRhs subType => Transform(subType),
            Microsoft.Dafny.HavocRhs subType => subType,
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ConcreteUpdateStatement TransformUnion(Microsoft.Dafny.ConcreteUpdateStatement value) =>
        value switch
        {
            Microsoft.Dafny.AssignSuchThatStmt subType => Transform(subType),
            Microsoft.Dafny.UpdateStmt subType => Transform(subType),
            Microsoft.Dafny.AssignOrReturnStmt subType => Transform(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.Statement, Microsoft.Dafny.ConcreteUpdateStatement, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.BlockStmt TransformUnion(Microsoft.Dafny.BlockStmt value) =>
        value switch
        {
            Microsoft.Dafny.DividedBlockStmt subType => Transform(subType),
            _ => Transform(value)
        };

        // Transforming because type-chain [Microsoft.Dafny.AttributedExpression, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.LoopStmt TransformUnion(Microsoft.Dafny.LoopStmt value) =>
        value switch
        {
            Microsoft.Dafny.OneBodyLoopStmt subType => TransformUnion(subType),
            Microsoft.Dafny.AlternativeLoopStmt subType => Transform(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.BlockStmt, Microsoft.Dafny.Statement, Microsoft.Dafny.ConcreteUpdateStatement, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.OneBodyLoopStmt TransformUnion(Microsoft.Dafny.OneBodyLoopStmt value) =>
        value switch
        {
            Microsoft.Dafny.WhileStmt subType => TransformUnion(subType),
            Microsoft.Dafny.ForLoopStmt subType => Transform(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.WhileStmt TransformUnion(Microsoft.Dafny.WhileStmt value) =>
        value switch
        {
            Microsoft.Dafny.RefinedWhileStmt subType => subType,
            _ => Transform(value)
        };

        // Transforming because type-chain [Microsoft.Dafny.StaticReceiverExpr, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.LiteralExpr TransformUnion(Microsoft.Dafny.LiteralExpr value) =>
        value switch
        {
            Microsoft.Dafny.StaticReceiverExpr subType => Transform(subType),
            Microsoft.Dafny.CharLiteralExpr subType => subType,
            Microsoft.Dafny.StringLiteralExpr subType => subType,
            _ => Transform(value)
        };

        // Transforming because type-chain [Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
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
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.DisplayExpression TransformUnion(Microsoft.Dafny.DisplayExpression value) =>
        value switch
        {
            Microsoft.Dafny.SetDisplayExpr subType => subType,
            Microsoft.Dafny.MultiSetDisplayExpr subType => subType,
            Microsoft.Dafny.SeqDisplayExpr subType => subType,
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.UnaryExpr TransformUnion(Microsoft.Dafny.UnaryExpr value) =>
        value switch
        {
            Microsoft.Dafny.UnaryOpExpr subType => subType,
            Microsoft.Dafny.TypeUnaryExpr subType => TransformUnion(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.Type, Microsoft.Dafny.NonProxyType, Microsoft.Dafny.UserDefinedType, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.TypeUnaryExpr TransformUnion(Microsoft.Dafny.TypeUnaryExpr value) =>
        value switch
        {
            Microsoft.Dafny.ConversionExpr subType => subType,
            Microsoft.Dafny.TypeTestExpr subType => subType,
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
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
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ComprehensionExpr TransformUnion(Microsoft.Dafny.ComprehensionExpr value) =>
        value switch
        {
            Microsoft.Dafny.QuantifierExpr subType => TransformUnion(subType),
            Microsoft.Dafny.SetComprehension subType => subType,
            Microsoft.Dafny.MapComprehension subType => Transform(subType),
            Microsoft.Dafny.LambdaExpr subType => Transform(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.QuantifierExpr TransformUnion(Microsoft.Dafny.QuantifierExpr value) =>
        value switch
        {
            Microsoft.Dafny.ForallExpr subType => subType,
            Microsoft.Dafny.ExistsExpr subType => subType,
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.LitCtx, Microsoft.Dafny.LiteralExpr, Microsoft.Dafny.StaticReceiverExpr, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.MatchingContext TransformUnion(Microsoft.Dafny.MatchingContext value) =>
        value switch
        {
            Microsoft.Dafny.LitCtx subType => Transform(subType),
            Microsoft.Dafny.HoleCtx subType => subType,
            Microsoft.Dafny.ForallCtx subType => subType,
            Microsoft.Dafny.IdCtx subType => Transform(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.LitPattern, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ExtendedPattern TransformUnion(Microsoft.Dafny.ExtendedPattern value) =>
        value switch
        {
            Microsoft.Dafny.LitPattern subType => Transform(subType),
            Microsoft.Dafny.IdPattern subType => Transform(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.Statement, Microsoft.Dafny.ConcreteUpdateStatement, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ConcreteSyntaxStatement TransformUnion(Microsoft.Dafny.ConcreteSyntaxStatement value) =>
        value switch
        {
            Microsoft.Dafny.NestedMatchStmt subType => Transform(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ParensExpression TransformUnion(Microsoft.Dafny.ParensExpression value) =>
        value switch
        {
            Microsoft.Dafny.TypeExpr subType => Transform(subType),
            Microsoft.Dafny.AutoGeneratedExpression subType => subType,
            _ => Transform(value)
        };

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.SuffixExpr TransformUnion(Microsoft.Dafny.SuffixExpr value) =>
        value switch
        {
            Microsoft.Dafny.ExprDotName subType => Transform(subType),
            Microsoft.Dafny.ApplySuffix subType => Transform(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.ComprehensionExpr.ExactBoundedPool, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
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
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.CalcStmt.TernaryCalcOp, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.CalcStmt.CalcOp TransformUnion(Microsoft.Dafny.CalcStmt.CalcOp value) =>
        value switch
        {
            Microsoft.Dafny.CalcStmt.BinaryCalcOp subType => subType,
            Microsoft.Dafny.CalcStmt.TernaryCalcOp subType => Transform(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.ComprehensionExpr.SetBoundedPool, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ComprehensionExpr.CollectionBoundedPool TransformUnion(Microsoft.Dafny.ComprehensionExpr.CollectionBoundedPool value) =>
        value switch
        {
            Microsoft.Dafny.ComprehensionExpr.SetBoundedPool subType => Transform(subType),
            Microsoft.Dafny.ComprehensionExpr.MultiSetBoundedPool subType => Transform(subType),
            Microsoft.Dafny.ComprehensionExpr.MapBoundedPool subType => Transform(subType),
            Microsoft.Dafny.ComprehensionExpr.SeqBoundedPool subType => Transform(subType),
            _ => throw new Exception($"type {value.GetType()} not specified by unions.")
        };

        // Transforming because type-chain [Microsoft.Dafny.CasePattern<Microsoft.Dafny.BoundVar>, Microsoft.Dafny.DatatypeCtor, Microsoft.Dafny.Formal, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.LetExpr TransformUnion(Microsoft.Dafny.LetExpr value) =>
        value switch
        {
            _ => Transform(value)
        };

        // Transforming because type-chain [Microsoft.Dafny.ModuleDefinition, Microsoft.Dafny.TopLevelDecl, Microsoft.Dafny.ModuleDecl, Microsoft.Dafny.ModuleSignature, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.Program Transform(Microsoft.Dafny.Program value)
        {
            return new Microsoft.Dafny.Program(value.Name, TransformUnion(value.DefaultModule), Transform(value.BuiltIns), value.Reporter);
        }

        // Transforming because type-chain [Microsoft.Dafny.Type, Microsoft.Dafny.NonProxyType, Microsoft.Dafny.UserDefinedType, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.SelfType Transform(Microsoft.Dafny.SelfType value)
        {
            return new Microsoft.Dafny.SelfType();
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.UserDefinedType Transform(Microsoft.Dafny.UserDefinedType value)
        {
            return new Microsoft.Dafny.UserDefinedType(value.Tok, TransformUnion(value.NamePath));
        }

        // Transforming because type-chain [Microsoft.Dafny.Type, Microsoft.Dafny.NonProxyType, Microsoft.Dafny.UserDefinedType, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.MapType Transform(Microsoft.Dafny.MapType value)
        {
            return new Microsoft.Dafny.MapType(value.Finite, TransformUnion(value.Domain), TransformUnion(value.Range));
        }

        // Transforming because type-chain [Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ValuetypeDecl Transform(Microsoft.Dafny.ValuetypeDecl value)
        {
            return new Microsoft.Dafny.ValuetypeDecl(value.Name, TransformUnion(value.EnclosingModuleDefinition), value.TypeParameterCount, value.TypeTester);
        }

        // Transforming because type-chain [Microsoft.Dafny.ModuleSignature, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.LiteralModuleDecl Transform(Microsoft.Dafny.LiteralModuleDecl value)
        {
            return new Microsoft.Dafny.LiteralModuleDecl(TransformUnion(value.ModuleDef), TransformUnion(value.EnclosingModuleDefinition));
        }

        // Transforming because type-chain [Microsoft.Dafny.ModuleQualifiedId, Microsoft.Dafny.ModuleDecl, Microsoft.Dafny.ModuleSignature, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.AliasModuleDecl Transform(Microsoft.Dafny.AliasModuleDecl value)
        {
            return new Microsoft.Dafny.AliasModuleDecl(Transform(value.TargetQId), value.Tok, TransformUnion(value.EnclosingModuleDefinition), value.Opened, value.Exports);
        }

        // Transforming because type-chain [Microsoft.Dafny.ModuleDecl, Microsoft.Dafny.ModuleSignature, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.AbstractModuleDecl Transform(Microsoft.Dafny.AbstractModuleDecl value)
        {
            return new Microsoft.Dafny.AbstractModuleDecl(Transform(value.QId), value.Tok, TransformUnion(value.EnclosingModuleDefinition), value.Opened, value.Exports);
        }

        // Transforming because type-chain [Microsoft.Dafny.ExportSignature, Microsoft.Dafny.Declaration, Microsoft.Dafny.Attributes, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ModuleExportDecl Transform(Microsoft.Dafny.ModuleExportDecl value)
        {
            return new Microsoft.Dafny.ModuleExportDecl(value.Tok, TransformUnion(value.EnclosingModuleDefinition), Transform(value.Exports, value => Transform(value)), value.Extends, value.ProvideAll, value.RevealAll, value.IsDefault, value.IsRefining);
        }

        // Transforming because type-chain [Microsoft.Dafny.Formal, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.DatatypeCtor Transform(Microsoft.Dafny.DatatypeCtor value)
        {
            return new Microsoft.Dafny.DatatypeCtor(value.Tok, value.Name, Transform(value.Formals, value => TransformUnion(value)), TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.Function] was transformed.
        public virtual Microsoft.Dafny.ArrowTypeDecl Transform(Microsoft.Dafny.ArrowTypeDecl value)
        {
            return new Microsoft.Dafny.ArrowTypeDecl(value.TypeArgs, TransformUnion(value.Requires), TransformUnion(value.Reads), TransformUnion(value.EnclosingModuleDefinition), TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.Formal, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.IteratorDecl Transform(Microsoft.Dafny.IteratorDecl value)
        {
            return new Microsoft.Dafny.IteratorDecl(value.Tok, value.Name, TransformUnion(value.EnclosingModuleDefinition), value.TypeArgs, Transform(value.Ins, value => TransformUnion(value)), Transform(value.Outs, value => TransformUnion(value)), Transform(value.Reads, value => Transform(value)), Transform(value.Modifies, value => Transform(value)), Transform(value.Decreases, value => TransformUnion(value)), Transform(value.Requires, value => Transform(value)), Transform(value.Ensures, value => Transform(value)), Transform(value.YieldRequires, value => Transform(value)), Transform(value.YieldEnsures, value => Transform(value)), TransformUnion(value.Body), TransformUnion(value.Attributes), value.SignatureEllipsis);
        }

        // Transforming because type-chain [Microsoft.Dafny.ArrowTypeDecl, Microsoft.Dafny.Function] was transformed.
        public virtual Microsoft.Dafny.ClassDecl Transform(Microsoft.Dafny.ClassDecl value)
        {
            return new Microsoft.Dafny.ClassDecl(value.Tok, value.Name, TransformUnion(value.EnclosingModuleDefinition), value.TypeArgs, Transform(value.Members, value => TransformUnion(value)), TransformUnion(value.Attributes), value.IsRefining, Transform(value.ParentTraits, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.NewtypeDecl Transform(Microsoft.Dafny.NewtypeDecl value)
        {
            return new Microsoft.Dafny.NewtypeDecl(value.Tok, value.Name, TransformUnion(value.EnclosingModuleDefinition), TransformUnion(value.BaseType), Transform(value.Members, value => TransformUnion(value)), TransformUnion(value.Attributes), value.IsRefining);
        }

        // Transforming because type-chain [Microsoft.Dafny.DatatypeCtor, Microsoft.Dafny.Formal, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.IndDatatypeDecl Transform(Microsoft.Dafny.IndDatatypeDecl value)
        {
            return new Microsoft.Dafny.IndDatatypeDecl(value.Tok, value.Name, TransformUnion(value.EnclosingModuleDefinition), value.TypeArgs, Transform(value.Ctors, value => Transform(value)), Transform(value.Members, value => TransformUnion(value)), TransformUnion(value.Attributes), value.IsRefining);
        }

        // Transforming because this type was [indirectly] mutated.
        public virtual Microsoft.Dafny.V4.Field Transform(Microsoft.Dafny.Field value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        // Transforming because this type was [indirectly] mutated.
        public virtual Microsoft.Dafny.V4.Function Transform(Microsoft.Dafny.Function value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        // Transforming because this type was [indirectly] mutated.
        public virtual Microsoft.Dafny.V4.Method Transform(Microsoft.Dafny.Method value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        // Transforming because this type was [indirectly] mutated.
        public virtual Microsoft.Dafny.V4.SpecialFunction Transform(Microsoft.Dafny.SpecialFunction value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        // Transforming because this type was [indirectly] mutated.
        public virtual Microsoft.Dafny.V4.SpecialField Transform(Microsoft.Dafny.SpecialField value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        // Transforming because this type was [indirectly] mutated.
        public virtual Microsoft.Dafny.V4.DatatypeDestructor Transform(Microsoft.Dafny.DatatypeDestructor value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        // Transforming because this type was [indirectly] mutated.
        public virtual Microsoft.Dafny.V4.ConstantField Transform(Microsoft.Dafny.ConstantField value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        // Transforming because type-chain [Microsoft.Dafny.SubsetTypeDecl, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.TypeSynonymDecl Transform(Microsoft.Dafny.TypeSynonymDecl value)
        {
            return new Microsoft.Dafny.TypeSynonymDecl(value.Tok, value.Name, value.Characteristics, value.TypeArgs, TransformUnion(value.EnclosingModuleDefinition), TransformUnion(value.Rhs), TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.SubsetTypeDecl Transform(Microsoft.Dafny.SubsetTypeDecl value)
        {
            return new Microsoft.Dafny.SubsetTypeDecl(value.Tok, value.Name, value.Characteristics, value.TypeArgs, TransformUnion(value.EnclosingModuleDefinition), value.BoundVar, TransformUnion(value.Constraint), value.WitnessKind, TransformUnion(value.Witness), TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.ClassDecl, Microsoft.Dafny.ArrowTypeDecl, Microsoft.Dafny.Function] was transformed.
        public virtual Microsoft.Dafny.NonNullTypeDecl Transform(Microsoft.Dafny.NonNullTypeDecl value)
        {
            return new Microsoft.Dafny.NonNullTypeDecl(TransformUnion(value.ClassDecl));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.Formal Transform(Microsoft.Dafny.Formal value)
        {
            return new Microsoft.Dafny.Formal(value.Tok, value.Name, TransformUnion(value.Type), value.InParam, value.IsGhost, TransformUnion(value.DefaultValue), value.IsOld, value.IsNameOnly, value.NameForCompilation);
        }

        // Transforming because this type was [indirectly] mutated.
        public virtual Microsoft.Dafny.V4.Constructor Transform(Microsoft.Dafny.Constructor value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.PrintStmt Transform(Microsoft.Dafny.PrintStmt value)
        {
            return new Microsoft.Dafny.PrintStmt(value.Tok, value.EndTok, Transform(value.Args, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.RevealStmt Transform(Microsoft.Dafny.RevealStmt value)
        {
            return new Microsoft.Dafny.RevealStmt(value.Tok, value.EndTok, Transform(value.Exprs, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.Statement, Microsoft.Dafny.ConcreteUpdateStatement, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.BreakStmt Transform(Microsoft.Dafny.BreakStmt value)
        {
            return new Microsoft.Dafny.BreakStmt(value.Tok, value.EndTok, value.TargetLabel);
        }

        // Transforming because type-chain [Microsoft.Dafny.ConcreteUpdateStatement, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.VarDeclStmt Transform(Microsoft.Dafny.VarDeclStmt value)
        {
            return new Microsoft.Dafny.VarDeclStmt(value.Tok, value.EndTok, Transform(value.Locals, value => Transform(value)), TransformUnion(value.Update));
        }

        // Transforming because type-chain [Microsoft.Dafny.CasePattern<Microsoft.Dafny.LocalVariable>, Microsoft.Dafny.DatatypeCtor, Microsoft.Dafny.Formal, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.VarDeclPattern Transform(Microsoft.Dafny.VarDeclPattern value)
        {
            return new Microsoft.Dafny.VarDeclPattern(value.Tok, value.EndTok, Transform(value.Lhs, value => Transform(value)), TransformUnion(value.Rhs), value.HasGhostModifier);
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.AssignStmt Transform(Microsoft.Dafny.AssignStmt value)
        {
            return new Microsoft.Dafny.AssignStmt(value.Tok, value.EndTok, TransformUnion(value.Lhs), TransformUnion(value.Rhs));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.CallStmt Transform(Microsoft.Dafny.CallStmt value)
        {
            return new Microsoft.Dafny.CallStmt(value.Tok, value.EndTok, Transform(value.Lhs, value => TransformUnion(value)), Transform(value.MethodSelect), Transform(value.BindingArgs, value => Transform(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.Statement, Microsoft.Dafny.ConcreteUpdateStatement, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.BlockStmt Transform(Microsoft.Dafny.BlockStmt value)
        {
            return new Microsoft.Dafny.BlockStmt(value.Tok, value.EndTok, Transform(value.Body, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.IfStmt Transform(Microsoft.Dafny.IfStmt value)
        {
            return new Microsoft.Dafny.IfStmt(value.Tok, value.EndTok, value.IsBindingGuard, TransformUnion(value.Guard), TransformUnion(value.Thn), TransformUnion(value.Els));
        }

        // Transforming because type-chain [Microsoft.Dafny.GuardedAlternative, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.AlternativeStmt Transform(Microsoft.Dafny.AlternativeStmt value)
        {
            return new Microsoft.Dafny.AlternativeStmt(value.Tok, value.EndTok, Transform(value.Alternatives, value => Transform(value)), value.UsesOptionalBraces);
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ForallStmt Transform(Microsoft.Dafny.ForallStmt value)
        {
            return new Microsoft.Dafny.ForallStmt(value.Tok, value.EndTok, value.BoundVars, TransformUnion(value.Attributes), TransformUnion(value.Range), Transform(value.Ens, value => Transform(value)), TransformUnion(value.Body));
        }

        // Transforming because type-chain [Microsoft.Dafny.Specification<Microsoft.Dafny.FrameExpression>, Microsoft.Dafny.FrameExpression, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ModifyStmt Transform(Microsoft.Dafny.ModifyStmt value)
        {
            return new Microsoft.Dafny.ModifyStmt(value.Tok, value.EndTok, Transform(value.Mod, value => Transform(value)), TransformUnion(value.Body));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.CalcStmt Transform(Microsoft.Dafny.CalcStmt value)
        {
            return new Microsoft.Dafny.CalcStmt(value.Tok, value.EndTok, TransformUnion(value.UserSuppliedOp), Transform(value.Lines, value => TransformUnion(value)), Transform(value.Hints, value => TransformUnion(value)), Transform(value.StepOps, value => TransformUnion(value)), TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.MatchStmt Transform(Microsoft.Dafny.MatchStmt value)
        {
            return new Microsoft.Dafny.MatchStmt(value.Tok, value.EndTok, TransformUnion(value.Source), Transform(value.Cases, value => Transform(value)), value.UsesOptionalBraces, TransformUnion(value.Context));
        }

        // Transforming because type-chain [Microsoft.Dafny.Statement, Microsoft.Dafny.ConcreteUpdateStatement, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.SkeletonStatement Transform(Microsoft.Dafny.SkeletonStatement value)
        {
            return new Microsoft.Dafny.SkeletonStatement(value.Tok, value.EndTok);
        }

        // Transforming because type-chain [Microsoft.Dafny.BlockStmt, Microsoft.Dafny.Statement, Microsoft.Dafny.ConcreteUpdateStatement, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.AssertStmt Transform(Microsoft.Dafny.AssertStmt value)
        {
            return new Microsoft.Dafny.AssertStmt(value.Tok, value.EndTok, TransformUnion(value.Expr), TransformUnion(value.Proof), value.Label, TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ExpectStmt Transform(Microsoft.Dafny.ExpectStmt value)
        {
            return new Microsoft.Dafny.ExpectStmt(value.Tok, value.EndTok, TransformUnion(value.Expr), TransformUnion(value.Message), TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ExprRhs Transform(Microsoft.Dafny.ExprRhs value)
        {
            return new Microsoft.Dafny.ExprRhs(TransformUnion(value.Expr), TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.TypeRhs Transform(Microsoft.Dafny.TypeRhs value)
        {
            return new Microsoft.Dafny.TypeRhs(value.Tok, TransformUnion(value.Type), Transform(value.ArrayDimensions, value => TransformUnion(value)), TransformUnion(value.ElementInit));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.AssignSuchThatStmt Transform(Microsoft.Dafny.AssignSuchThatStmt value)
        {
            return new Microsoft.Dafny.AssignSuchThatStmt(value.Tok, value.EndTok, Transform(value.Lhss, value => TransformUnion(value)), TransformUnion(value.Expr), value.AssumeToken, TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.UpdateStmt Transform(Microsoft.Dafny.UpdateStmt value)
        {
            return new Microsoft.Dafny.UpdateStmt(value.Tok, value.EndTok, Transform(value.Lhss, value => TransformUnion(value)), Transform(value.Rhss, value => TransformUnion(value)), value.CanMutateKnownState);
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.AssignOrReturnStmt Transform(Microsoft.Dafny.AssignOrReturnStmt value)
        {
            return new Microsoft.Dafny.AssignOrReturnStmt(value.Tok, value.EndTok, Transform(value.Lhss, value => TransformUnion(value)), TransformUnion(value.Rhs), value.KeywordToken, Transform(value.Rhss, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.Statement, Microsoft.Dafny.ConcreteUpdateStatement, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.DividedBlockStmt Transform(Microsoft.Dafny.DividedBlockStmt value)
        {
            return new Microsoft.Dafny.DividedBlockStmt(value.Tok, value.EndTok, Transform(value.BodyInit, value => TransformUnion(value)), value.SeparatorTok, Transform(value.BodyProper, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.GuardedAlternative, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.AlternativeLoopStmt Transform(Microsoft.Dafny.AlternativeLoopStmt value)
        {
            return new Microsoft.Dafny.AlternativeLoopStmt(value.Tok, value.EndTok, Transform(value.Invariants, value => Transform(value)), Transform(value.Decreases, value => TransformUnion(value)), Transform(value.Mod, value => Transform(value)), Transform(value.Alternatives, value => Transform(value)), value.UsesOptionalBraces);
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.WhileStmt Transform(Microsoft.Dafny.WhileStmt value)
        {
            return new Microsoft.Dafny.WhileStmt(value.Tok, value.EndTok, TransformUnion(value.Guard), Transform(value.Invariants, value => Transform(value)), Transform(value.Decreases, value => TransformUnion(value)), Transform(value.Mod, value => Transform(value)), TransformUnion(value.Body));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ForLoopStmt Transform(Microsoft.Dafny.ForLoopStmt value)
        {
            return new Microsoft.Dafny.ForLoopStmt(value.Tok, value.EndTok, value.LoopIndex, TransformUnion(value.Start), TransformUnion(value.End), value.GoingUp, Transform(value.Invariants, value => Transform(value)), Transform(value.Decreases, value => TransformUnion(value)), Transform(value.Mod, value => Transform(value)), TransformUnion(value.Body), TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.Statement, Microsoft.Dafny.ConcreteUpdateStatement, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.MatchCaseStmt Transform(Microsoft.Dafny.MatchCaseStmt value)
        {
            return new Microsoft.Dafny.MatchCaseStmt(value.Tok, Transform(value.Ctor), value.Arguments, Transform(value.Body, value => TransformUnion(value)), TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.MatchCaseExpr Transform(Microsoft.Dafny.MatchCaseExpr value)
        {
            return new Microsoft.Dafny.MatchCaseExpr(value.Tok, Transform(value.Ctor), value.Arguments, TransformUnion(value.Body), TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.StaticReceiverExpr Transform(Microsoft.Dafny.StaticReceiverExpr value)
        {
            return new Microsoft.Dafny.StaticReceiverExpr(value.Tok, TransformUnion(value.UnresolvedType), value.IsImplicit);
        }

        // Transforming because type-chain [Microsoft.Dafny.StaticReceiverExpr, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.LiteralExpr Transform(Microsoft.Dafny.LiteralExpr value)
        {
            return new Microsoft.Dafny.LiteralExpr(value.Tok);
        }

        // Transforming because type-chain [Microsoft.Dafny.ActualBindings, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.DatatypeValue Transform(Microsoft.Dafny.DatatypeValue value)
        {
            return new Microsoft.Dafny.DatatypeValue(value.Tok, value.DatatypeName, value.MemberName, Transform(value.ArgumentBindings, value => Transform(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.TopLevelDecl, Microsoft.Dafny.ModuleDecl, Microsoft.Dafny.ModuleSignature, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.Resolver_IdentifierExpr Transform(Microsoft.Dafny.Resolver_IdentifierExpr value)
        {
            return new Microsoft.Dafny.Resolver_IdentifierExpr(value.Tok, value.Decl, Transform(value.TypeArgs, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.ExpressionPair, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.MapDisplayExpr Transform(Microsoft.Dafny.MapDisplayExpr value)
        {
            return new Microsoft.Dafny.MapDisplayExpr(value.Tok, value.Finite, Transform(value.Elements, value => Transform(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.MemberSelectExpr Transform(Microsoft.Dafny.MemberSelectExpr value)
        {
            return new Microsoft.Dafny.MemberSelectExpr(value.Tok, TransformUnion(value.Obj), value.MemberName);
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.SeqSelectExpr Transform(Microsoft.Dafny.SeqSelectExpr value)
        {
            return new Microsoft.Dafny.SeqSelectExpr(value.Tok, value.SelectOne, TransformUnion(value.Seq), TransformUnion(value.E0), TransformUnion(value.E1));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.MultiSelectExpr Transform(Microsoft.Dafny.MultiSelectExpr value)
        {
            return new Microsoft.Dafny.MultiSelectExpr(value.Tok, TransformUnion(value.Array), Transform(value.Indices, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.SeqUpdateExpr Transform(Microsoft.Dafny.SeqUpdateExpr value)
        {
            return new Microsoft.Dafny.SeqUpdateExpr(value.Tok, TransformUnion(value.Seq), TransformUnion(value.Index), TransformUnion(value.Value));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ApplyExpr Transform(Microsoft.Dafny.ApplyExpr value)
        {
            return new Microsoft.Dafny.ApplyExpr(value.Tok, TransformUnion(value.Function), Transform(value.Args, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.Function] was transformed.
        public virtual Microsoft.Dafny.FunctionCallExpr Transform(Microsoft.Dafny.FunctionCallExpr value)
        {
            return new Microsoft.Dafny.FunctionCallExpr(value.Tok, value.Name, TransformUnion(value.Receiver), value.OpenParen, Transform(value.ArgumentBindings, value => Transform(value)), value.AtLabel);
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.SeqConstructionExpr Transform(Microsoft.Dafny.SeqConstructionExpr value)
        {
            return new Microsoft.Dafny.SeqConstructionExpr(value.Tok, TransformUnion(value.ExplicitElementType), TransformUnion(value.N), TransformUnion(value.Initializer));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.MultiSetFormingExpr Transform(Microsoft.Dafny.MultiSetFormingExpr value)
        {
            return new Microsoft.Dafny.MultiSetFormingExpr(value.Tok, TransformUnion(value.E));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.OldExpr Transform(Microsoft.Dafny.OldExpr value)
        {
            return new Microsoft.Dafny.OldExpr(value.Tok, TransformUnion(value.E), value.At);
        }

        // Transforming because type-chain [Microsoft.Dafny.FrameExpression, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.UnchangedExpr Transform(Microsoft.Dafny.UnchangedExpr value)
        {
            return new Microsoft.Dafny.UnchangedExpr(value.Tok, Transform(value.Frame, value => Transform(value)), value.At);
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.BinaryExpr Transform(Microsoft.Dafny.BinaryExpr value)
        {
            return new Microsoft.Dafny.BinaryExpr(value.Tok, value.Op, TransformUnion(value.E0), TransformUnion(value.E1));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.TernaryExpr Transform(Microsoft.Dafny.TernaryExpr value)
        {
            return new Microsoft.Dafny.TernaryExpr(value.Tok, value.Op, TransformUnion(value.E0), TransformUnion(value.E1), TransformUnion(value.E2));
        }

        // Transforming because type-chain [Microsoft.Dafny.CasePattern<Microsoft.Dafny.BoundVar>, Microsoft.Dafny.DatatypeCtor, Microsoft.Dafny.Formal, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.LetExpr Transform(Microsoft.Dafny.LetExpr value)
        {
            return new Microsoft.Dafny.LetExpr(value.Tok, Transform(value.Lhss, value => Transform(value, value => value)), Transform(value.Rhss, value => TransformUnion(value)), TransformUnion(value.Body), value.Exact, TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.Statement, Microsoft.Dafny.ConcreteUpdateStatement, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.StmtExpr Transform(Microsoft.Dafny.StmtExpr value)
        {
            return new Microsoft.Dafny.StmtExpr(value.Tok, TransformUnion(value.S), TransformUnion(value.E));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ITEExpr Transform(Microsoft.Dafny.ITEExpr value)
        {
            return new Microsoft.Dafny.ITEExpr(value.Tok, value.IsBindingGuard, TransformUnion(value.Test), TransformUnion(value.Thn), TransformUnion(value.Els));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.MatchExpr Transform(Microsoft.Dafny.MatchExpr value)
        {
            return new Microsoft.Dafny.MatchExpr(value.Tok, TransformUnion(value.Source), Transform(value.Cases, value => Transform(value)), value.UsesOptionalBraces, TransformUnion(value.Context));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.BoxingCastExpr Transform(Microsoft.Dafny.BoxingCastExpr value)
        {
            return new Microsoft.Dafny.BoxingCastExpr(TransformUnion(value.E), TransformUnion(value.FromType), TransformUnion(value.ToType));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.UnboxingCastExpr Transform(Microsoft.Dafny.UnboxingCastExpr value)
        {
            return new Microsoft.Dafny.UnboxingCastExpr(TransformUnion(value.E), TransformUnion(value.FromType), TransformUnion(value.ToType));
        }

        // Transforming because type-chain [Microsoft.Dafny.CasePattern<Microsoft.Dafny.BoundVar>, Microsoft.Dafny.DatatypeCtor, Microsoft.Dafny.Formal, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.LetOrFailExpr Transform(Microsoft.Dafny.LetOrFailExpr value)
        {
            return new Microsoft.Dafny.LetOrFailExpr(value.Tok, Transform(value.Lhs, value => value), TransformUnion(value.Rhs), TransformUnion(value.Body));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.NestedMatchExpr Transform(Microsoft.Dafny.NestedMatchExpr value)
        {
            return new Microsoft.Dafny.NestedMatchExpr(value.Tok, TransformUnion(value.Source), Transform(value.Cases, value => Transform(value)), value.UsesOptionalBraces, TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ParensExpression Transform(Microsoft.Dafny.ParensExpression value)
        {
            return new Microsoft.Dafny.ParensExpression(value.Tok, TransformUnion(value.E));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.DatatypeUpdateExpr Transform(Microsoft.Dafny.DatatypeUpdateExpr value)
        {
            return new Microsoft.Dafny.DatatypeUpdateExpr(value.Tok, TransformUnion(value.Root), Transform(value.Updates, value => Transform(value, value => value, value => value, value => TransformUnion(value))));
        }

        // Transforming because type-chain [Microsoft.Dafny.Formal, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.DefaultValueExpression Transform(Microsoft.Dafny.DefaultValueExpression value)
        {
            return new Microsoft.Dafny.DefaultValueExpression(value.Tok, TransformUnion(value.Formal), TransformUnion(value.Receiver), Transform(value.SubstMap, value => value, value => TransformUnion(value)), Transform(value.TypeMap, value => value, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.NegationExpression Transform(Microsoft.Dafny.NegationExpression value)
        {
            return new Microsoft.Dafny.NegationExpression(value.Tok, TransformUnion(value.E));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ChainingExpression Transform(Microsoft.Dafny.ChainingExpression value)
        {
            return new Microsoft.Dafny.ChainingExpression(value.Tok, Transform(value.Operands, value => TransformUnion(value)), value.Operators, value.OperatorLocs, Transform(value.PrefixLimits, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.Type, Microsoft.Dafny.NonProxyType, Microsoft.Dafny.UserDefinedType, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.NameSegment Transform(Microsoft.Dafny.NameSegment value)
        {
            return new Microsoft.Dafny.NameSegment(value.Tok, value.Name, Transform(value.OptTypeArguments, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.MapComprehension Transform(Microsoft.Dafny.MapComprehension value)
        {
            return new Microsoft.Dafny.MapComprehension(value.Tok, value.BodyEndTok, value.Finite, value.BoundVars, TransformUnion(value.Range), TransformUnion(value.TermLeft), TransformUnion(value.TermRight), TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.FrameExpression, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.LambdaExpr Transform(Microsoft.Dafny.LambdaExpr value)
        {
            return new Microsoft.Dafny.LambdaExpr(value.Tok, value.BodyEndTok, value.BoundVars, TransformUnion(value.Range), Transform(value.Reads, value => Transform(value)), TransformUnion(value.Body));
        }

        // Transforming because type-chain [Microsoft.Dafny.LiteralExpr, Microsoft.Dafny.StaticReceiverExpr, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.LitCtx Transform(Microsoft.Dafny.LitCtx value)
        {
            return new Microsoft.Dafny.LitCtx(TransformUnion(value.Lit));
        }

        // Transforming because type-chain [Microsoft.Dafny.MatchingContext, Microsoft.Dafny.LitCtx, Microsoft.Dafny.LiteralExpr, Microsoft.Dafny.StaticReceiverExpr, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.IdCtx Transform(Microsoft.Dafny.IdCtx value)
        {
            return new Microsoft.Dafny.IdCtx(value.Id, Transform(value.Arguments, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.LitPattern Transform(Microsoft.Dafny.LitPattern value)
        {
            return new Microsoft.Dafny.LitPattern(value.Tok, TransformUnion(value.OrigLit), value.IsGhost);
        }

        // Transforming because type-chain [Microsoft.Dafny.LiteralExpr, Microsoft.Dafny.StaticReceiverExpr, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.IdPattern Transform(Microsoft.Dafny.IdPattern value)
        {
            return new Microsoft.Dafny.IdPattern(value.Tok, value.Id, Transform(value.Arguments, value => TransformUnion(value)), value.IsGhost);
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.NestedMatchCaseExpr Transform(Microsoft.Dafny.NestedMatchCaseExpr value)
        {
            return new Microsoft.Dafny.NestedMatchCaseExpr(value.Tok, TransformUnion(value.Pat), TransformUnion(value.Body), TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.Statement, Microsoft.Dafny.ConcreteUpdateStatement, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.NestedMatchCaseStmt Transform(Microsoft.Dafny.NestedMatchCaseStmt value)
        {
            return new Microsoft.Dafny.NestedMatchCaseStmt(value.Tok, TransformUnion(value.Pat), Transform(value.Body, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.NestedMatchStmt Transform(Microsoft.Dafny.NestedMatchStmt value)
        {
            return new Microsoft.Dafny.NestedMatchStmt(value.Tok, value.EndTok, TransformUnion(value.Source), Transform(value.Cases, value => Transform(value)), value.UsesOptionalBraces, TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.Type, Microsoft.Dafny.NonProxyType, Microsoft.Dafny.UserDefinedType, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.TypeExpr Transform(Microsoft.Dafny.TypeExpr value)
        {
            return new Microsoft.Dafny.TypeExpr(value.Tok, TransformUnion(value.E), TransformUnion(value.T));
        }

        // Transforming because type-chain [Microsoft.Dafny.Type, Microsoft.Dafny.NonProxyType, Microsoft.Dafny.UserDefinedType, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ExprDotName Transform(Microsoft.Dafny.ExprDotName value)
        {
            return new Microsoft.Dafny.ExprDotName(value.Tok, TransformUnion(value.Lhs), value.SuffixName, Transform(value.OptTypeArguments, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.ActualBindings, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ApplySuffix Transform(Microsoft.Dafny.ApplySuffix value)
        {
            return new Microsoft.Dafny.ApplySuffix(value.Tok, value.AtTok, TransformUnion(value.Lhs), Transform(value.ArgumentBindings, value => Transform(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ComprehensionExpr.ExactBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.ExactBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.ExactBoundedPool(TransformUnion(value.E));
        }

        // Transforming because type-chain [Microsoft.Dafny.Type, Microsoft.Dafny.NonProxyType, Microsoft.Dafny.UserDefinedType, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ComprehensionExpr.AllocFreeBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.AllocFreeBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.AllocFreeBoundedPool(TransformUnion(value.Type));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ComprehensionExpr.IntBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.IntBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.IntBoundedPool(TransformUnion(value.LowerBound), TransformUnion(value.UpperBound));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ComprehensionExpr.SubSetBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.SubSetBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.SubSetBoundedPool(TransformUnion(value.UpperBound), value.IsFiniteCollection);
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ComprehensionExpr.SuperSetBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.SuperSetBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.SuperSetBoundedPool(TransformUnion(value.LowerBound));
        }

        // Transforming because type-chain [Microsoft.Dafny.DatatypeDecl, Microsoft.Dafny.DatatypeCtor, Microsoft.Dafny.Formal, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ComprehensionExpr.DatatypeBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.DatatypeBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.DatatypeBoundedPool(TransformUnion(value.Decl));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.CalcStmt.TernaryCalcOp Transform(Microsoft.Dafny.CalcStmt.TernaryCalcOp value)
        {
            return new Microsoft.Dafny.CalcStmt.TernaryCalcOp(TransformUnion(value.Index));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ComprehensionExpr.SetBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.SetBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.SetBoundedPool(TransformUnion(value.Set), TransformUnion(value.BoundVariableType), TransformUnion(value.CollectionElementType), value.IsFiniteCollection);
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ComprehensionExpr.MultiSetBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.MultiSetBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.MultiSetBoundedPool(TransformUnion(value.MultiSet), TransformUnion(value.BoundVariableType), TransformUnion(value.CollectionElementType));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ComprehensionExpr.MapBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.MapBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.MapBoundedPool(TransformUnion(value.Map), TransformUnion(value.BoundVariableType), TransformUnion(value.CollectionElementType), value.IsFiniteCollection);
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ComprehensionExpr.SeqBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.SeqBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.SeqBoundedPool(TransformUnion(value.Seq), TransformUnion(value.BoundVariableType), TransformUnion(value.CollectionElementType));
        }

        // Transforming because type-chain [Microsoft.Dafny.TopLevelDecl, Microsoft.Dafny.ModuleDecl, Microsoft.Dafny.ModuleSignature, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ModuleDefinition Transform(Microsoft.Dafny.ModuleDefinition value)
        {
            return new Microsoft.Dafny.ModuleDefinition(value.Tok, value.Name, value.PrefixIds, value.IsAbstract, value.IsFacade, Transform(value.RefinementQId), TransformUnion(value.EnclosingModule), TransformUnion(value.Attributes), value.IsBuiltinName, value.IsToBeVerified, value.IsToBeCompiled, Transform(value.TopLevelDecls, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ModuleSignature Transform(Microsoft.Dafny.ModuleSignature value)
        {
            return new Microsoft.Dafny.ModuleSignature();
        }

        // Transforming because type-chain [Microsoft.Dafny.SpecialField] was transformed.
        public virtual Microsoft.Dafny.BuiltIns Transform(Microsoft.Dafny.BuiltIns value)
        {
            return new Microsoft.Dafny.BuiltIns();
        }

        // Transforming because type-chain [Microsoft.Dafny.ModuleDecl, Microsoft.Dafny.ModuleSignature, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ModuleQualifiedId Transform(Microsoft.Dafny.ModuleQualifiedId value)
        {
            return new Microsoft.Dafny.ModuleQualifiedId(value.Path);
        }

        // Transforming because type-chain [Microsoft.Dafny.Declaration, Microsoft.Dafny.Attributes, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ExportSignature Transform(Microsoft.Dafny.ExportSignature value)
        {
            return new Microsoft.Dafny.ExportSignature(value.ClassIdTok, value.ClassId, value.Tok, value.Id, value.Opaque);
        }

        // Transforming because type-chain [Microsoft.Dafny.FrameExpression, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.Specification<ToT> Transform<FromT, ToT>(Microsoft.Dafny.Specification<FromT> value, Func<FromT, ToT> convT) where FromT : class
        where ToT : class
        {
            return new Microsoft.Dafny.Specification<ToT>(Transform(value.Expressions, value => convT(value)), TransformUnion(value.Attributes));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.FrameExpression Transform(Microsoft.Dafny.FrameExpression value)
        {
            return new Microsoft.Dafny.FrameExpression(value.Tok, TransformUnion(value.E), value.FieldName);
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.AttributedExpression Transform(Microsoft.Dafny.AttributedExpression value)
        {
            return new Microsoft.Dafny.AttributedExpression(TransformUnion(value.E));
        }

        // Transforming because type-chain [Microsoft.Dafny.Attributes, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.LocalVariable Transform(Microsoft.Dafny.LocalVariable value)
        {
            return new Microsoft.Dafny.LocalVariable(value.Tok, value.EndTok, value.Name, TransformUnion(value.Type), value.IsGhost);
        }

        // Transforming because type-chain [Microsoft.Dafny.DatatypeCtor, Microsoft.Dafny.Formal, Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.CasePattern<ToVT> Transform<FromVT, ToVT>(Microsoft.Dafny.CasePattern<FromVT> value, Func<FromVT, ToVT> convVT) where FromVT : IVariable
        where ToVT : IVariable
        {
            return new Microsoft.Dafny.CasePattern<ToVT>(value.Tok, value.Id, Transform(value.Arguments, value => Transform(value, value => convVT(value))));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ActualBindings Transform(Microsoft.Dafny.ActualBindings value)
        {
            return new Microsoft.Dafny.ActualBindings(Transform(value.ArgumentBindings, value => Transform(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.GuardedAlternative Transform(Microsoft.Dafny.GuardedAlternative value)
        {
            return new Microsoft.Dafny.GuardedAlternative(value.Tok, value.IsBindingGuard, TransformUnion(value.Guard), Transform(value.Body, value => TransformUnion(value)));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.Attributes Transform(Microsoft.Dafny.Attributes value)
        {
            return new Microsoft.Dafny.Attributes(value.Name, Transform(value.Args, value => TransformUnion(value)), TransformUnion(value.Prev));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ExpressionPair Transform(Microsoft.Dafny.ExpressionPair value)
        {
            return new Microsoft.Dafny.ExpressionPair(TransformUnion(value.A), TransformUnion(value.B));
        }

        // Transforming because type-chain [Microsoft.Dafny.Expression, Microsoft.Dafny.MemberSelectExpr, Microsoft.Dafny.MemberDecl] was transformed.
        public virtual Microsoft.Dafny.ActualBinding Transform(Microsoft.Dafny.ActualBinding value)
        {
            return new Microsoft.Dafny.ActualBinding(value.FormalParameterName, TransformUnion(value.Actual), value.IsGhost);
        }

        public virtual (U1, U2) Transform<T1, U1, T2, U2>((T1, T2) value, Func<T1, U1> mapItem1, Func<T2, U2> mapItem2)
        {
            return (mapItem1(value.Item1), mapItem2(value.Item2));
        }
        public virtual (U1, U2, U3) Transform<T1, U1, T2, U2, T3, U3>((T1, T2, T3) value, Func<T1, U1> mapItem1, Func<T2, U2> mapItem2, Func<T3, U3> mapItem3)
        {
            return (mapItem1(value.Item1), mapItem2(value.Item2), mapItem3(value.Item3));
        }
        public virtual Tuple<U1, U2, U3> Transform<T1, U1, T2, U2, T3, U3>(Tuple<T1, T2, T3> value, Func<T1, U1> mapItem1, Func<T2, U2> mapItem2, Func<T3, U3> mapItem3)
        {
            return Tuple.Create(mapItem1(value.Item1), mapItem2(value.Item2), mapItem3(value.Item3));
        }
        public virtual List<U> Transform<T, U>(IEnumerable<T> value, Func<T, U> f)
        {
            return value.Select(f).ToList();
        }
        public virtual Dictionary<K2, V2> Transform<K, V, K2, V2>(IDictionary<K, V> value, Func<K, K2> mapKey, Func<V, V2> mapValue)
        {
            return value.ToDictionary(kv => mapKey(kv.Key), kv => mapValue(kv.Value));
        }
    }
}