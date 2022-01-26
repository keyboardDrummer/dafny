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
    public class ModuleDefinition : INamedRegion, IAttributeBearingDeclaration
    {
        public readonly IToken Tok;
        public IToken BodyStartTok = Token.NoToken;
        public IToken BodyEndTok = Token.NoToken;
        public readonly string DafnyName; // The (not-qualified) name as seen in Dafny source code
        public readonly string Name; // (Last segment of the) module name
        public string FullDafnyName
        {
            get
            {
                if (EnclosingModule == null)
                {
                    return "";
                }

                string n = EnclosingModule.FullDafnyName;
                return (n.Length == 0 ? n : (n + ".")) + DafnyName;
            }
        }
        public string FullName
        {
            get
            {
                if (EnclosingModule == null || EnclosingModule.IsDefaultModule)
                {
                    return Name;
                }
                else
                {
                    return EnclosingModule.FullName + "." + Name;
                }
            }
        }
        public readonly List<IToken> PrefixIds; // The qualified module name, except the last segment when a
                                                // nested module declaration is outside its enclosing module
        IToken IRegion.BodyStartTok { get { return BodyStartTok; } }
        IToken IRegion.BodyEndTok { get { return BodyEndTok; } }
        string INamedRegion.Name { get { return Name; } }
        public ModuleDefinition EnclosingModule;  // readonly, except can be changed by resolver for prefix-named modules when the real parent is discovered
        public readonly Attributes Attributes;
        public ModuleQualifiedId RefinementQId; // full qualified ID of the refinement parent, null if no refinement base
        public bool SuccessfullyResolved;  // set to true upon successful resolution; modules that import an unsuccessfully resolved module are not themselves resolved

        public List<Include> Includes;

        public readonly List<TopLevelDecl> TopLevelDecls;  // filled in by the parser; readonly after that, except for the addition of prefix-named modules, which happens in the resolver
        public readonly List<Tuple<List<IToken>, LiteralModuleDecl>> PrefixNamedModules = new List<Tuple<List<IToken>, LiteralModuleDecl>>();  // filled in by the parser; emptied by the resolver
        public readonly Graph<ICallable> CallGraph = new Graph<ICallable>();  // filled in during resolution
        public int Height;  // height in the topological sorting of modules; filled in during resolution
        public readonly bool IsAbstract;
        public readonly bool IsFacade; // True iff this module represents a module facade (that is, an abstract interface)
        public readonly bool IsBuiltinName; // true if this is something like _System that shouldn't have it's name mangled.
        public readonly bool IsToBeVerified;
        public readonly bool IsToBeCompiled;

        public int? ResolvedHash { get; set; }

        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(cce.NonNullElements(TopLevelDecls));
            Contract.Invariant(CallGraph != null);
        }

        public ModuleDefinition(IToken tok, string name, List<IToken> prefixIds, bool isAbstract, bool isFacade,
          ModuleQualifiedId refinementQId, ModuleDefinition enclosingModule, Attributes attributes, bool isBuiltinName,
          bool isToBeVerified, bool isToBeCompiled, List<TopLevelDecl> TopLevelDecls = null)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            this.Tok = tok;
            this.DafnyName = tok.val;
            this.Name = name;
            this.PrefixIds = prefixIds;
            this.Attributes = attributes;
            this.EnclosingModule = enclosingModule;
            this.RefinementQId = refinementQId;
            this.IsAbstract = isAbstract;
            this.IsFacade = isFacade;
            this.Includes = new List<Include>();
            this.IsBuiltinName = isBuiltinName;
            this.IsToBeVerified = isToBeVerified;
            this.IsToBeCompiled = isToBeCompiled;
            this.TopLevelDecls = TopLevelDecls ?? new List<TopLevelDecl>();
        }

        VisibilityScope visibilityScope;

        public VisibilityScope VisibilityScope
        {
            get
            {
                if (visibilityScope == null)
                {
                    visibilityScope = new VisibilityScope(this.CompileName);
                }
                return visibilityScope;
            }
        }

        public virtual bool IsDefaultModule
        {
            get
            {
                return false;
            }
        }
        string compileName;
        public string CompileName
        {
            get
            {
                if (compileName == null)
                {
                    var externArgs = DafnyOptions.O.DisallowExterns ? null : Attributes.FindExpressions(this.Attributes, "extern");
                    if (externArgs != null && 1 <= externArgs.Count && externArgs[0] is StringLiteralExpr)
                    {
                        compileName = (string)((StringLiteralExpr)externArgs[0]).Value;
                    }
                    else if (IsBuiltinName || externArgs != null)
                    {
                        compileName = Name;
                    }
                    else
                    {
                        if (EnclosingModule != null && EnclosingModule.Name != "_module")
                        {
                            // Include all names in the module tree path, to disambiguate when compiling
                            // a flat list of modules.
                            // Use an "underscore-escaped" character as a module name separator, since
                            // underscores are already used as escape characters in CompilerizeName()
                            compileName = EnclosingModule.CompileName + "_m" + NonglobalVariable.CompilerizeName(Name);
                        }
                        else
                        {
                            compileName = NonglobalVariable.CompilerizeName(Name);
                        }
                    }
                }
                return compileName;
            }
        }

        public string RefinementCompileName
        {
            get
            {
                return this.CompileName;
            }
        }

        /// <summary>
        /// Determines if "a" and "b" are in the same strongly connected component of the call graph, that is,
        /// if "a" and "b" are mutually recursive.
        /// Assumes that CallGraph has already been filled in for the modules containing "a" and "b".
        /// </summary>
        public static bool InSameSCC(ICallable a, ICallable b)
        {
            Contract.Requires(a != null);
            Contract.Requires(b != null);
            if (a is SpecialFunction || b is SpecialFunction) { return false; }
            var module = a.EnclosingModule;
            return module == b.EnclosingModule && module.CallGraph.GetSCCRepresentative(a) == module.CallGraph.GetSCCRepresentative(b);
        }

        /// <summary>
        /// Return the representative elements of the SCCs that contain any member declaration in a
        /// class in "declarations".
        /// Note, the representative element may in some cases be a Method, not necessarily a Function.
        /// </summary>
        public static IEnumerable<ICallable> AllFunctionSCCs(List<TopLevelDecl> declarations)
        {
            var set = new HashSet<ICallable>();
            foreach (var d in declarations)
            {
                var cl = d as TopLevelDeclWithMembers;
                if (cl != null)
                {
                    var module = cl.EnclosingModuleDefinition;
                    foreach (var member in cl.Members)
                    {
                        var fn = member as Function;
                        if (fn != null)
                        {
                            var repr = module.CallGraph.GetSCCRepresentative(fn);
                            set.Add(repr);
                        }
                    }
                }
            }
            return set;
        }

        public static IEnumerable<Function> AllFunctions(List<TopLevelDecl> declarations)
        {
            foreach (var d in declarations)
            {
                var cl = d as TopLevelDeclWithMembers;
                if (cl != null)
                {
                    foreach (var member in cl.Members)
                    {
                        var fn = member as Function;
                        if (fn != null)
                        {
                            yield return fn;
                        }
                    }
                }
            }
        }

        public static IEnumerable<Field> AllFields(List<TopLevelDecl> declarations)
        {
            foreach (var d in declarations)
            {
                var cl = d as TopLevelDeclWithMembers;
                if (cl != null)
                {
                    foreach (var member in cl.Members)
                    {
                        var fn = member as Field;
                        if (fn != null)
                        {
                            yield return fn;
                        }
                    }
                }
            }
        }

        public static IEnumerable<ClassDecl> AllClasses(List<TopLevelDecl> declarations)
        {
            foreach (var d in declarations)
            {
                if (d is ClassDecl cl)
                {
                    yield return cl;
                }
            }
        }

        public static IEnumerable<TopLevelDeclWithMembers> AllTypesWithMembers(List<TopLevelDecl> declarations)
        {
            foreach (var d in declarations)
            {
                if (d is TopLevelDeclWithMembers cl)
                {
                    yield return cl;
                }
            }
        }

        /// <summary>
        /// Yields all functions and methods that are members of some type in the given list of
        /// declarations.
        /// Note, an iterator declaration is a type, in this sense.
        /// Note, if the given list are the top-level declarations of a module, the yield will include
        /// greatest lemmas but not their associated prefix lemmas (which are tucked into the greatest lemma's
        /// .PrefixLemma field).
        /// </summary>
        public static IEnumerable<ICallable> AllCallables(List<TopLevelDecl> declarations)
        {
            foreach (var d in declarations)
            {
                var cl = d as TopLevelDeclWithMembers;
                if (cl != null)
                {
                    foreach (var member in cl.Members)
                    {
                        var clbl = member as ICallable;
                        if (clbl != null && !(member is ConstantField))
                        {
                            yield return clbl;
                            if (clbl is Function f && f.ByMethodDecl != null)
                            {
                                yield return f.ByMethodDecl;
                            }
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Yields all functions and methods that are members of some non-iterator type in the given
        /// list of declarations, as well as any IteratorDecl's in that list.
        /// </summary>
        public static IEnumerable<ICallable> AllItersAndCallables(List<TopLevelDecl> declarations)
        {
            foreach (var d in declarations)
            {
                if (d is IteratorDecl)
                {
                    var iter = (IteratorDecl)d;
                    yield return iter;
                }
                else if (d is TopLevelDeclWithMembers)
                {
                    var cl = (TopLevelDeclWithMembers)d;
                    foreach (var member in cl.Members)
                    {
                        var clbl = member as ICallable;
                        if (clbl != null)
                        {
                            yield return clbl;
                            if (clbl is Function f && f.ByMethodDecl != null)
                            {
                                yield return f.ByMethodDecl;
                            }
                        }
                    }
                }
            }
        }

        public static IEnumerable<IteratorDecl> AllIteratorDecls(List<TopLevelDecl> declarations)
        {
            foreach (var d in declarations)
            {
                var iter = d as IteratorDecl;
                if (iter != null)
                {
                    yield return iter;
                }
            }
        }

        /// <summary>
        /// Emits the declarations in "declarations", but for each such declaration that is a class with
        /// a corresponding non-null type, also emits that non-null type *after* the class declaration.
        /// </summary>
        public static IEnumerable<TopLevelDecl> AllDeclarationsAndNonNullTypeDecls(List<TopLevelDecl> declarations)
        {
            foreach (var d in declarations)
            {
                yield return d;
                var cl = d as ClassDecl;
                if (cl != null && cl.NonNullTypeDecl != null)
                {
                    yield return cl.NonNullTypeDecl;
                }
            }
        }

        public static IEnumerable<ExtremeLemma> AllExtremeLemmas(List<TopLevelDecl> declarations)
        {
            foreach (var d in declarations)
            {
                var cl = d as TopLevelDeclWithMembers;
                if (cl != null)
                {
                    foreach (var member in cl.Members)
                    {
                        var m = member as ExtremeLemma;
                        if (m != null)
                        {
                            yield return m;
                        }
                    }
                }
            }
        }

        public bool IsEssentiallyEmptyModuleBody()
        {
            foreach (var d in TopLevelDecls)
            {
                if (d is ModuleDecl)
                {
                    // modules don't count
                    continue;
                }
                else if (d is ClassDecl)
                {
                    var cl = (ClassDecl)d;
                    if (cl.Members.Count == 0)
                    {
                        // the class is empty, so it doesn't count
                        continue;
                    }
                }
                return false;
            }
            return true;
        }
    }

    public class TypeParameter : TopLevelDecl
    {
        public interface ParentType
        {
            string FullName { get; }
        }

        public override string WhatKind => "type parameter";

        ParentType parent;
        public ParentType Parent
        {
            get
            {
                return parent;
            }
            set
            {
                Contract.Requires(Parent == null);  // set it only once
                Contract.Requires(value != null);
                parent = value;
            }
        }

        public override string CompileName
        {
            get
            {
                if (compileName == null)
                {
                    var name = Name;
                    if (parent is MemberDecl && !name.StartsWith("_"))
                    {
                        // prepend "_" to type parameters of functions and methods, to ensure they don't clash with type parameters of the enclosing type
                        name = "_" + name;
                    }
                    compileName = NonglobalVariable.CompilerizeName(name);
                }
                return compileName;
            }
        }

        /// <summary>
        /// NonVariant_Strict     (default) - non-variant, no uses left of an arrow
        /// NonVariant_Permissive    !      - non-variant
        /// Covariant_Strict         +      - co-variant, no uses left of an arrow
        /// Covariant_Permissive     *      - co-variant
        /// Contravariant            -      - contra-variant
        /// </summary>
        public enum TPVarianceSyntax { NonVariant_Strict, NonVariant_Permissive, Covariant_Strict, Covariant_Permissive, Contravariance }
        public enum TPVariance { Co, Non, Contra }
        public static TPVariance Negate(TPVariance v)
        {
            switch (v)
            {
                case TPVariance.Co:
                    return TPVariance.Contra;
                case TPVariance.Contra:
                    return TPVariance.Co;
                default:
                    return v;
            }
        }
        public static int Direction(TPVariance v)
        {
            switch (v)
            {
                case TPVariance.Co:
                    return 1;
                case TPVariance.Contra:
                    return -1;
                default:
                    return 0;
            }
        }
        public TPVarianceSyntax VarianceSyntax;
        public TPVariance Variance
        {
            get
            {
                switch (VarianceSyntax)
                {
                    case TPVarianceSyntax.Covariant_Strict:
                    case TPVarianceSyntax.Covariant_Permissive:
                        return TPVariance.Co;
                    case TPVarianceSyntax.NonVariant_Strict:
                    case TPVarianceSyntax.NonVariant_Permissive:
                        return TPVariance.Non;
                    case TPVarianceSyntax.Contravariance:
                        return TPVariance.Contra;
                    default:
                        Contract.Assert(false);  // unexpected VarianceSyntax
                        throw new cce.UnreachableException();
                }
            }
        }
        public bool StrictVariance
        {
            get
            {
                switch (VarianceSyntax)
                {
                    case TPVarianceSyntax.Covariant_Strict:
                    case TPVarianceSyntax.NonVariant_Strict:
                        return true;
                    case TPVarianceSyntax.Covariant_Permissive:
                    case TPVarianceSyntax.NonVariant_Permissive:
                    case TPVarianceSyntax.Contravariance:
                        return false;
                    default:
                        Contract.Assert(false);  // unexpected VarianceSyntax
                        throw new cce.UnreachableException();
                }
            }
        }

        public enum EqualitySupportValue { Required, InferredRequired, Unspecified }
        public struct TypeParameterCharacteristics
        {
            public EqualitySupportValue EqualitySupport;  // the resolver may change this value from Unspecified to InferredRequired (for some signatures that may immediately imply that equality support is required)
            public Microsoft.Dafny.Type.AutoInitInfo AutoInit;
            public bool HasCompiledValue => AutoInit == Type.AutoInitInfo.CompilableValue;
            public bool IsNonempty => AutoInit != Type.AutoInitInfo.MaybeEmpty;
            public bool ContainsNoReferenceTypes;
            public TypeParameterCharacteristics(bool dummy)
            {
                EqualitySupport = EqualitySupportValue.Unspecified;
                AutoInit = Type.AutoInitInfo.MaybeEmpty;
                ContainsNoReferenceTypes = false;
            }
            public TypeParameterCharacteristics(EqualitySupportValue eqSupport, Microsoft.Dafny.Type.AutoInitInfo autoInit, bool containsNoReferenceTypes)
            {
                EqualitySupport = eqSupport;
                AutoInit = autoInit;
                ContainsNoReferenceTypes = containsNoReferenceTypes;
            }
        }
        public TypeParameterCharacteristics Characteristics;
        public bool SupportsEquality
        {
            get { return Characteristics.EqualitySupport != EqualitySupportValue.Unspecified; }
        }

        public bool NecessaryForEqualitySupportOfSurroundingInductiveDatatype = false;  // computed during resolution; relevant only when Parent denotes an IndDatatypeDecl

        public bool IsToplevelScope
        { // true if this type parameter is on a toplevel (ie. class C<T>), and false if it is on a member (ie. method m<T>(...))
            get { return parent is TopLevelDecl; }
        }
        public int PositionalIndex; // which type parameter this is (ie. in C<S, T, U>, S is 0, T is 1 and U is 2).

        public TypeParameter(IToken tok, string name, TPVarianceSyntax varianceS, TypeParameterCharacteristics characteristics)
          : base(tok, name, null, new List<TypeParameter>(), null, false)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Characteristics = characteristics;
            VarianceSyntax = varianceS;
        }

        public TypeParameter(IToken tok, string name, TPVarianceSyntax varianceS)
          : this(tok, name, varianceS, new TypeParameterCharacteristics(false))
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
        }

        public TypeParameter(IToken tok, string name, int positionalIndex, ParentType parent)
           : this(tok, name, TPVarianceSyntax.NonVariant_Strict)
        {
            PositionalIndex = positionalIndex;
            Parent = parent;
        }

        public override string FullName
        {
            get
            {
                // when debugging, print it all:
                return /* Parent.FullName + "." + */ Name;
            }
        }

        public static TypeParameterCharacteristics GetExplicitCharacteristics(TopLevelDecl d)
        {
            Contract.Requires(d != null);
            TypeParameterCharacteristics characteristics = new TypeParameterCharacteristics(false);
            if (d is OpaqueTypeDecl)
            {
                var dd = (OpaqueTypeDecl)d;
                characteristics = dd.Characteristics;
            }
            else if (d is TypeSynonymDecl)
            {
                var dd = (TypeSynonymDecl)d;
                characteristics = dd.Characteristics;
            }
            if (characteristics.EqualitySupport == EqualitySupportValue.InferredRequired)
            {
                return new TypeParameterCharacteristics(EqualitySupportValue.Unspecified, characteristics.AutoInit, characteristics.ContainsNoReferenceTypes);
            }
            else
            {
                return characteristics;
            }
        }
    }

    public class SelfType : NonProxyType
    {
        public TypeParameter TypeArg;
        public Type ResolvedType;
        public SelfType() : base()
        {
            TypeArg = new TypeParameter(Token.NoToken, "selfType", TypeParameter.TPVarianceSyntax.NonVariant_Strict);
        }

        [Pure]
        public override string TypeName(ModuleDefinition context, bool parseAble)
        {
            return "selftype";
        }
        public override bool Equals(Type that, bool keepConstraints = false)
        {
            return that.NormalizeExpand(keepConstraints) is SelfType;
        }
    }

    public abstract class TopLevelDecl : Declaration, Microsoft.Dafny.TypeParameter.ParentType
    {
        public abstract string WhatKind { get; }
        public readonly ModuleDefinition EnclosingModuleDefinition;
        public readonly List<TypeParameter> TypeArgs;
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(cce.NonNullElements(TypeArgs));
        }

        public TopLevelDecl(IToken tok, string name, ModuleDefinition enclosingModuleDefinition, List<TypeParameter> typeArgs, Attributes attributes, bool isRefining)
          : base(tok, name, attributes, isRefining)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(cce.NonNullElements(typeArgs));
            EnclosingModuleDefinition = enclosingModuleDefinition;
            TypeArgs = typeArgs;
        }

        public string FullDafnyName
        {
            get
            {
                if (Name == "_module")
                {
                    return "";
                }

                if (Name == "_default")
                {
                    return EnclosingModuleDefinition.FullDafnyName;
                }

                string n = EnclosingModuleDefinition.FullDafnyName;
                return (n.Length == 0 ? n : (n + ".")) + Name;
            }
        }
        public virtual string FullName
        {
            get
            {
                return EnclosingModuleDefinition.FullName + "." + Name;
            }
        }
        public string FullSanitizedName
        {
            get
            {
                return EnclosingModuleDefinition.CompileName + "." + CompileName;
            }
        }

        public string FullSanitizedRefinementName
        {
            get
            {
                return EnclosingModuleDefinition.RefinementCompileName + "." + CompileName;
            }
        }

        public string FullNameInContext(ModuleDefinition context)
        {
            if (EnclosingModuleDefinition == context)
            {
                return Name;
            }
            else
            {
                return EnclosingModuleDefinition.Name + "." + Name;
            }
        }
        public string FullCompileName
        {
            get
            {
                var externArgs = Attributes.FindExpressions(this.Attributes, "extern");
                if (!DafnyOptions.O.DisallowExterns && externArgs != null)
                {
                    if (externArgs.Count == 2 && externArgs[0] is StringLiteralExpr && externArgs[1] is StringLiteralExpr)
                    {
                        return externArgs[0].AsStringLiteral() + "." + externArgs[1].AsStringLiteral();
                    }
                }
                if (EnclosingModuleDefinition.IsDefaultModule && DafnyOptions.O.CompileTarget == DafnyOptions.CompilationTarget.Csharp)
                {
                    return Declaration.IdProtect(CompileName);
                }
                else
                {
                    return Declaration.IdProtect(EnclosingModuleDefinition.CompileName) + "." + Declaration.IdProtect(CompileName);
                }
            }
        }

        public TopLevelDecl ViewAsClass
        {
            get
            {
                if (this is NonNullTypeDecl)
                {
                    return ((NonNullTypeDecl)this).ClassDecl;
                }
                else
                {
                    return this;
                }
            }
        }

        /// <summary>
        /// Return the list of parent types of "this", where the type parameters
        /// of "this" have been instantiated by "typeArgs". For example, for a subset
        /// type, the return value is the RHS type, appropriately instantiated. As
        /// two other examples, given
        ///     class C<X> extends J<X, int>
        /// C.ParentTypes(real) = J<real, int>    // non-null types C and J
        /// C?.ParentTypes(real) = J?<real, int>  // possibly-null type C? and J?
        /// </summary>
        public virtual List<Type> ParentTypes(List<Type> typeArgs)
        {
            Contract.Requires(typeArgs != null);
            Contract.Requires(this.TypeArgs.Count == typeArgs.Count);
            return new List<Type>();
        }

        public bool AllowsAllocation => true;
    }

    public class ModuleSignature
    {
        public VisibilityScope VisibilityScope = null;
        public readonly Dictionary<string, TopLevelDecl> TopLevels = new Dictionary<string, TopLevelDecl>();
        public readonly Dictionary<string, ModuleExportDecl> ExportSets = new Dictionary<string, ModuleExportDecl>();
        public readonly Dictionary<string, Tuple<DatatypeCtor, bool>> Ctors = new Dictionary<string, Tuple<DatatypeCtor, bool>>();
        public readonly Dictionary<string, MemberDecl> StaticMembers = new Dictionary<string, MemberDecl>();
        public ModuleDefinition ModuleDef = null; // Note: this is null if this signature does not correspond to a specific definition (i.e.
                                                  // it is abstract). Otherwise, it points to that definition.
        public ModuleSignature CompileSignature = null; // This is the version of the signature that should be used at compile time.
        public ModuleSignature Refines = null;
        public bool IsAbstract = false;
        public ModuleSignature() { }
        public int? ResolvedHash { get; set; }

        // Qualified accesses follow module imports
        public bool FindImport(string name, out ModuleDecl decl)
        {
            TopLevelDecl top;
            if (TopLevels.TryGetValue(name, out top) && top is ModuleDecl)
            {
                decl = (ModuleDecl)top;
                return true;
            }
            else
            {
                decl = null;
                return false;
            }
        }
    }

    // Represents the exports of a module.
    public class ModuleExportDecl : ModuleDecl
    {
        public readonly bool IsDefault;
        public List<ExportSignature> Exports; // list of TopLevelDecl that are included in the export
        public List<IToken> Extends; // list of exports that are extended
        public readonly List<ModuleExportDecl> ExtendDecls = new List<ModuleExportDecl>(); // fill in by the resolver
        public readonly HashSet<Tuple<Declaration, bool>> ExportDecls = new HashSet<Tuple<Declaration, bool>>(); // fill in by the resolver
        public bool RevealAll; // only kept for initial rewriting, then discarded
        public bool ProvideAll;

        public readonly VisibilityScope ThisScope;
        public ModuleExportDecl(IToken tok, ModuleDefinition enclosingModuleDefinition,
          List<ExportSignature> exports, List<IToken> extends, bool provideAll, bool revealAll, bool isDefault, bool isRefining)
          : base(tok, (isDefault || tok.val == "export") ? enclosingModuleDefinition.Name : tok.val, enclosingModuleDefinition, false, isRefining)
        {
            Contract.Requires(exports != null);
            IsDefault = isDefault;
            Exports = exports;
            Extends = extends;
            ProvideAll = provideAll;
            RevealAll = revealAll;
            ThisScope = new VisibilityScope(this.FullCompileName);
        }

        public ModuleExportDecl(IToken tok, string name, ModuleDefinition parent,
          List<ExportSignature> exports, List<IToken> extends, bool provideAll, bool revealAll, bool isDefault, bool isRefining)
          : base(tok, name, parent, false, isRefining)
        {
            Contract.Requires(exports != null);
            IsDefault = isDefault;
            Exports = exports;
            Extends = extends;
            ProvideAll = provideAll;
            RevealAll = revealAll;
            ThisScope = new VisibilityScope(this.FullCompileName);
        }

        public void SetupDefaultSignature()
        {
            Contract.Requires(this.Signature == null);
            var sig = new ModuleSignature();
            sig.ModuleDef = this.EnclosingModuleDefinition;
            sig.IsAbstract = this.EnclosingModuleDefinition.IsAbstract;
            sig.VisibilityScope = new VisibilityScope();
            sig.VisibilityScope.Augment(ThisScope);
            this.Signature = sig;
        }

        public override object Dereference() { return this; }
        public override bool CanBeExported()
        {
            return false;
        }

    }

    public abstract class DatatypeDecl : TopLevelDeclWithMembers, RevealableTypeDecl, ICallable
    {
        public override bool CanBeRevealed() { return true; }
        public readonly List<DatatypeCtor> Ctors;
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(cce.NonNullElements(Ctors));
            Contract.Invariant(1 <= Ctors.Count);
        }

        public DatatypeDecl(IToken tok, string name, ModuleDefinition enclosingModuleDefinition, List<TypeParameter> typeArgs,
          [Captured] List<DatatypeCtor> ctors, List<MemberDecl> members, Attributes attributes, bool isRefining)
          : base(tok, name, enclosingModuleDefinition, typeArgs, members, attributes, isRefining)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(enclosingModuleDefinition != null);
            Contract.Requires(cce.NonNullElements(typeArgs));
            Contract.Requires(cce.NonNullElements(ctors));
            Contract.Requires(cce.NonNullElements(members));
            Contract.Requires(1 <= ctors.Count);
            Ctors = ctors;
            this.NewSelfSynonym();
        }
        public bool HasFinitePossibleValues
        {
            get
            {
                return (TypeArgs.Count == 0 && Ctors.TrueForAll(ctr => ctr.Formals.Count == 0));
            }
        }

        public bool IsRecordType
        {
            get { return this is IndDatatypeDecl && Ctors.Count == 1; }
        }

        TopLevelDecl RevealableTypeDecl.AsTopLevelDecl { get { return this; } }

        bool ICodeContext.IsGhost { get { return true; } }
        List<TypeParameter> ICodeContext.TypeArgs { get { return TypeArgs; } }
        List<Formal> ICodeContext.Ins { get { return new List<Formal>(); } }
        ModuleDefinition ICodeContext.EnclosingModule { get { return EnclosingModuleDefinition; } }
        bool ICodeContext.MustReverify { get { return false; } }
        bool ICodeContext.AllowsNontermination { get { return false; } }
        IToken ICallable.Tok { get { return Tok; } }
        string ICallable.NameRelativeToModule { get { return Name; } }
        Specification<Expression> ICallable.Decreases
        {
            get
            {
                // The resolver checks that a NewtypeDecl sits in its own SSC in the call graph.  Therefore,
                // the question of what its Decreases clause is should never arise.
                throw new cce.UnreachableException();
            }
        }
        bool ICallable.InferredDecreases
        {
            get { throw new cce.UnreachableException(); }  // see comment above about ICallable.Decreases
            set { throw new cce.UnreachableException(); }  // see comment above about ICallable.Decreases
        }

        public abstract DatatypeCtor GetGroundingCtor();
    }

    public class DatatypeCtor : Declaration, Microsoft.Dafny.TypeParameter.ParentType
    {
        public readonly List<Formal> Formals;
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(cce.NonNullElements(Formals));
            Contract.Invariant(Destructors != null);
            Contract.Invariant(
              Destructors.Count == 0 || // this is until resolution
              Destructors.Count == Formals.Count);  // after resolution
        }

        // TODO: One could imagine having a precondition on datatype constructors
        public DatatypeDecl EnclosingDatatype;  // filled in during resolution
        public SpecialField QueryField;  // filled in during resolution
        public List<DatatypeDestructor> Destructors = new List<DatatypeDestructor>();  // contents filled in during resolution; includes both implicit (not mentionable in source) and explicit destructors

        public DatatypeCtor(IToken tok, string name, [Captured] List<Formal> formals, Attributes attributes)
          : base(tok, name, attributes, false)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(cce.NonNullElements(formals));
            this.Formals = formals;
        }

        public string FullName
        {
            get
            {
                Contract.Ensures(Contract.Result<string>() != null);
                Contract.Assume(EnclosingDatatype != null);

                return "#" + EnclosingDatatype.FullName + "." + Name;
            }
        }
    }

    public class TupleTypeDecl : IndDatatypeDecl
    {
        public readonly List<bool> ArgumentGhostness;

        public int Dims => ArgumentGhostness.Count;

        public int NonGhostDims => ArgumentGhostness.Count(x => !x);

        /// <summary>
        /// Construct a resolved built-in tuple type with "dim" arguments.  "systemModule" is expected to be the _System module.
        /// </summary>
        public TupleTypeDecl(List<bool> argumentGhostness, ModuleDefinition enclosingModuleDefinition, Attributes attributes)
          : this(enclosingModuleDefinition, CreateCovariantTypeParameters(argumentGhostness.Count), argumentGhostness, attributes)
        {
            Contract.Requires(0 <= argumentGhostness.Count);
            Contract.Requires(enclosingModuleDefinition != null);

            // Resolve the type parameters here
            Contract.Assert(TypeArgs.Count == Dims);
            for (var i = 0; i < Dims; i++)
            {
                var tp = TypeArgs[i];
                tp.Parent = this;
                tp.PositionalIndex = i;
            }
        }

        private TupleTypeDecl(ModuleDefinition systemModule, List<TypeParameter> typeArgs, List<bool> argumentGhostness, Attributes attributes)
          : base(Token.NoToken, BuiltIns.TupleTypeName(argumentGhostness), systemModule, typeArgs, CreateConstructors(typeArgs, argumentGhostness), new List<MemberDecl>(), attributes, false)
        {
            Contract.Requires(systemModule != null);
            Contract.Requires(typeArgs != null);
            ArgumentGhostness = argumentGhostness;
            foreach (var ctor in Ctors)
            {
                ctor.EnclosingDatatype = this;  // resolve here
                GroundingCtor = ctor;
                TypeParametersUsedInConstructionByGroundingCtor = new bool[typeArgs.Count];
                for (int i = 0; i < typeArgs.Count; i++)
                {
                    TypeParametersUsedInConstructionByGroundingCtor[i] = !argumentGhostness[i];
                }
            }
            this.EqualitySupport = argumentGhostness.Contains(true) ? ES.Never : ES.ConsultTypeArguments;
        }
        private static List<TypeParameter> CreateCovariantTypeParameters(int dims)
        {
            Contract.Requires(0 <= dims);
            var ts = new List<TypeParameter>();
            for (int i = 0; i < dims; i++)
            {
                var tp = new TypeParameter(Token.NoToken, "T" + i, TypeParameter.TPVarianceSyntax.Covariant_Strict);
                tp.NecessaryForEqualitySupportOfSurroundingInductiveDatatype = true;
                ts.Add(tp);
            }
            return ts;
        }
        private static List<DatatypeCtor> CreateConstructors(List<TypeParameter> typeArgs, List<bool> argumentGhostness)
        {
            Contract.Requires(typeArgs != null);
            var formals = new List<Formal>();
            var nonGhostArgs = 0;
            for (int i = 0; i < typeArgs.Count; i++)
            {
                string compileName;
                if (argumentGhostness[i])
                {
                    // This name is irrelevant, since it won't be used in compilation. Give it a strange name
                    // that would alert us of any bug that nevertheless tries to access this name.
                    compileName = "this * is * never * used * " + i.ToString();
                }
                else
                {
                    compileName = nonGhostArgs.ToString();
                    nonGhostArgs++;
                }
                var tp = typeArgs[i];
                var f = new Formal(Token.NoToken, i.ToString(), new UserDefinedType(Token.NoToken, tp), true, argumentGhostness[i], null, nameForCompilation: compileName);
                formals.Add(f);
            }
            string ctorName = BuiltIns.TupleTypeCtorName(typeArgs.Count);
            var ctor = new DatatypeCtor(Token.NoToken, ctorName, formals, null);
            return new List<DatatypeCtor>() { ctor };
        }

        public override string CompileName
        {
            get
            {
                return "Tuple" + BuiltIns.ArgumentGhostnessToString(ArgumentGhostness);
            }
        }
    }

    public class IndDatatypeDecl : DatatypeDecl, RevealableTypeDecl
    {
        public override string WhatKind { get { return "datatype"; } }
        public DatatypeCtor GroundingCtor;  // set during resolution

        public override DatatypeCtor GetGroundingCtor()
        {
            return GroundingCtor;
        }

        public bool[] TypeParametersUsedInConstructionByGroundingCtor;  // set during resolution; has same length as the number of type arguments

        public enum ES { NotYetComputed, Never, ConsultTypeArguments }
        public ES EqualitySupport = ES.NotYetComputed;

        public IndDatatypeDecl(IToken tok, string name, ModuleDefinition enclosingModuleDefinition, List<TypeParameter> typeArgs,
          [Captured] List<DatatypeCtor> ctors, List<MemberDecl> members, Attributes attributes, bool isRefining)
          : base(tok, name, enclosingModuleDefinition, typeArgs, ctors, members, attributes, isRefining)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(enclosingModuleDefinition != null);
            Contract.Requires(cce.NonNullElements(typeArgs));
            Contract.Requires(cce.NonNullElements(ctors));
            Contract.Requires(cce.NonNullElements(members));
            Contract.Requires(1 <= ctors.Count);
        }
    }

    public class CoDatatypeDecl : DatatypeDecl
    {
        public override string WhatKind { get { return "codatatype"; } }
        public CoDatatypeDecl SscRepr;  // filled in during resolution

        public CoDatatypeDecl(IToken tok, string name, ModuleDefinition enclosingModuleDefinition, List<TypeParameter> typeArgs,
          [Captured] List<DatatypeCtor> ctors, List<MemberDecl> members, Attributes attributes, bool isRefining)
          : base(tok, name, enclosingModuleDefinition, typeArgs, ctors, members, attributes, isRefining)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(enclosingModuleDefinition != null);
            Contract.Requires(cce.NonNullElements(typeArgs));
            Contract.Requires(cce.NonNullElements(ctors));
            Contract.Requires(cce.NonNullElements(members));
            Contract.Requires(1 <= ctors.Count);
        }

        public override DatatypeCtor GetGroundingCtor()
        {
            return Ctors[0];
        }
    }

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
        public SpecialField(IToken tok, string name, ID specialId, object idParam, bool isGhost, bool isMutable, bool isUserMutable, Type type, Attributes attributes)
          : this(tok, name, specialId, idParam, false, isGhost, isMutable, isUserMutable, type, attributes)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(!isUserMutable || isMutable);
            Contract.Requires(type != null);
        }

        public SpecialField(IToken tok, string name, ID specialId, object idParam, bool hasStaticKeyword, bool isGhost, bool isMutable, bool isUserMutable, Type type, Attributes attributes)
          : base(tok, name, hasStaticKeyword, isGhost, isMutable, isUserMutable, type, attributes)
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

        public DatatypeDestructor(IToken tok, DatatypeCtor enclosingCtor, Formal correspondingFormal, string name, string compiledName, bool isGhost, Type type, Attributes attributes)
          : base(tok, name, SpecialField.ID.UseIdParam, compiledName, isGhost, false, false, type, attributes)
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
        public ConstantField(IToken tok, string name, Expression/*?*/ rhs, bool hasStaticKeyword, bool isGhost, Type type, Attributes attributes)
          : base(tok, name, SpecialField.ID.UseIdParam, NonglobalVariable.CompilerizeName(name), hasStaticKeyword, isGhost, false, false, type, attributes)
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

        public MemberDecl(IToken tok, string name, bool hasStaticKeyword, bool isGhost, Attributes attributes, bool isRefining)
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

        public Field(IToken tok, string name, bool isGhost, Type type, Attributes attributes)
          : this(tok, name, false, isGhost, true, true, type, attributes)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(type != null);
        }

        public Field(IToken tok, string name, bool hasStaticKeyword, bool isGhost, bool isMutable, bool isUserMutable, Type type, Attributes attributes)
          : base(tok, name, hasStaticKeyword, isGhost, attributes, false)
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

    public class FrameExpression
    {
        public readonly IToken Tok;
        public readonly Expression E;  // may be a WildcardExpr
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(E != null);
            Contract.Invariant(!(E is WildcardExpr) || (FieldName == null && Field == null));
        }

        public readonly string FieldName;
        public Field Field;  // filled in during resolution (but is null if FieldName is)

        /// <summary>
        /// If a "fieldName" is given, then "tok" denotes its source location.  Otherwise, "tok"
        /// denotes the source location of "e".
        /// </summary>
        public FrameExpression(IToken tok, Expression e, string fieldName)
        {
            Contract.Requires(tok != null);
            Contract.Requires(e != null);
            Contract.Requires(!(e is WildcardExpr) || fieldName == null);
            this.Tok = tok;
            E = e;
            FieldName = fieldName;
        }
    }

    /// <summary>
    /// A CallStmt is always resolved.  It is typically produced as a resolved counterpart of the syntactic AST note ApplySuffix.
    /// </summary>
    public class CallStmt : Statement
    {
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(MethodSelect.Member is Method);
            Contract.Invariant(cce.NonNullElements(Lhs));
            Contract.Invariant(cce.NonNullElements(Args));
        }

        public readonly List<Expression> Lhs;
        public readonly MemberSelectExpr MethodSelect;
        public readonly ActualBindings Bindings;
        public List<Expression> Args => Bindings.Arguments;
        public Expression OriginalInitialLhs = null;

        public Expression Receiver { get { return MethodSelect.Obj; } }
        public Method Method { get { return (Method)MethodSelect.Member; } }

        public CallStmt(IToken tok, IToken endTok, List<Expression> lhs, MemberSelectExpr memSel, List<ActualBinding> args)
          : base(tok, endTok)
        {
            Contract.Requires(tok != null);
            Contract.Requires(endTok != null);
            Contract.Requires(cce.NonNullElements(lhs));
            Contract.Requires(memSel != null);
            Contract.Requires(memSel.Member is Method);
            Contract.Requires(cce.NonNullElements(args));

            this.Lhs = lhs;
            this.MethodSelect = memSel;
            this.Bindings = new ActualBindings(args);
        }

        /// <summary>
        /// This constructor is intended to be used when constructing a resolved CallStmt. The "args" are expected
        /// to be already resolved, and are all given positionally.
        /// </summary>
        public CallStmt(IToken tok, IToken endTok, List<Expression> lhs, MemberSelectExpr memSel, List<Expression> args)
          : this(tok, endTok, lhs, memSel, args.ConvertAll(e => new ActualBinding(null, e)))
        {
            Bindings.AcceptArgumentExpressionsAsExactParameterList();
        }

        public override IEnumerable<Expression> NonSpecificationSubExpressions
        {
            get
            {
                foreach (var e in base.NonSpecificationSubExpressions) { yield return e; }
                foreach (var ee in Lhs)
                {
                    yield return ee;
                }
                yield return MethodSelect;
                foreach (var ee in Args)
                {
                    yield return ee;
                }
            }
        }
    }

    public class MemberSelectExpr : Expression
    {
        public readonly Expression Obj;
        public readonly string MemberName;
        public MemberDecl Member;    // filled in by resolution, will be a Field or Function
        public Label /*?*/ AtLabel;  // determined by resolution; non-null for a two-state selection

        /// <summary>
        /// TypeApplication_AtEnclosingClass is the list of type arguments used to instantiate the type that
        /// declares Member (which is some supertype of the receiver type).
        /// </summary>
        public List<Type> TypeApplication_AtEnclosingClass;  // filled in during resolution

        /// <summary>
        ///  TypeApplication_JustMember is the list of type arguments used to instantiate the type parameters
        /// of Member.
        /// </summary>
        public List<Type> TypeApplication_JustMember;  // filled in during resolution

        /// <summary>
        /// Returns a mapping from formal type parameters to actual type arguments. For example, given
        ///     trait T<A> {
        ///       function F<X>(): bv8 { ... }
        ///     }
        ///     class C<B, D> extends T<map<B, D>> { }
        /// and MemberSelectExpr o.F<int> where o has type C<real, bool>, the type map returned is
        ///     A -> map<real, bool>
        ///     X -> int
        /// To also include B and D in the mapping, use TypeArgumentSubstitutionsWithParents instead.
        /// </summary>
        public Dictionary<TypeParameter, Type> TypeArgumentSubstitutionsAtMemberDeclaration()
        {
            Contract.Requires(WasResolved());
            Contract.Ensures(Contract.Result<Dictionary<TypeParameter, Type>>() != null);

            var subst = new Dictionary<TypeParameter, Type>();

            // Add the mappings from the member's own type parameters
            if (Member is ICallable icallable)
            {
                Contract.Assert(TypeApplication_JustMember.Count == icallable.TypeArgs.Count);
                for (var i = 0; i < icallable.TypeArgs.Count; i++)
                {
                    subst.Add(icallable.TypeArgs[i], TypeApplication_JustMember[i]);
                }
            }
            else
            {
                Contract.Assert(TypeApplication_JustMember.Count == 0);
            }

            // Add the mappings from the enclosing class.
            TopLevelDecl cl = Member.EnclosingClass;
            // Expand the type down to its non-null type, if any
            if (cl != null)
            {
                Contract.Assert(cl.TypeArgs.Count == TypeApplication_AtEnclosingClass.Count);
                for (var i = 0; i < cl.TypeArgs.Count; i++)
                {
                    subst.Add(cl.TypeArgs[i], TypeApplication_AtEnclosingClass[i]);
                }
            }

            return subst;
        }

        /// <summary>
        /// Returns a mapping from formal type parameters to actual type arguments. For example, given
        ///     trait T<A> {
        ///       function F<X>(): bv8 { ... }
        ///     }
        ///     class C<B, D> extends T<map<B, D>> { }
        /// and MemberSelectExpr o.F<int> where o has type C<real, bool>, the type map returned is
        ///     A -> map<real, bool>
        ///     B -> real
        ///     D -> bool
        ///     X -> int
        /// NOTE: This method should be called only when all types have been fully and successfully
        /// resolved. During type inference, when there may still be some unresolved proxies, use
        /// TypeArgumentSubstitutionsAtMemberDeclaration instead.
        /// </summary>
        public Dictionary<TypeParameter, Type> TypeArgumentSubstitutionsWithParents()
        {
            Contract.Requires(WasResolved());
            Contract.Ensures(Contract.Result<Dictionary<TypeParameter, Type>>() != null);

            return TypeArgumentSubstitutionsWithParentsAux(Obj.Type, Member, TypeApplication_JustMember);
        }

        public static Dictionary<TypeParameter, Type> TypeArgumentSubstitutionsWithParentsAux(Type receiverType, MemberDecl member, List<Type> typeApplicationMember)
        {
            Contract.Requires(receiverType != null);
            Contract.Requires(member != null);
            Contract.Requires(typeApplicationMember != null);
            Contract.Ensures(Contract.Result<Dictionary<TypeParameter, Type>>() != null);

            var subst = new Dictionary<TypeParameter, Type>();

            // Add the mappings from the member's own type parameters
            if (member is ICallable)
            {
                // Make sure to include the member's type parameters all the way up the inheritance chain
                for (var ancestor = member; ancestor != null; ancestor = ancestor.OverriddenMember)
                {
                    var icallable = (ICallable)ancestor;
                    Contract.Assert(typeApplicationMember.Count == icallable.TypeArgs.Count);
                    for (var i = 0; i < icallable.TypeArgs.Count; i++)
                    {
                        subst.Add(icallable.TypeArgs[i], typeApplicationMember[i]);
                    }
                }
            }
            else
            {
                Contract.Assert(typeApplicationMember.Count == 0);
            }

            // Add the mappings from the receiver's type "cl"
            var udt = receiverType.NormalizeExpand() as UserDefinedType;
            if (udt != null)
            {
                if (udt.ResolvedClass is InternalTypeSynonymDecl isyn)
                {
                    udt = isyn.RhsWithArgumentIgnoringScope(udt.TypeArgs) as UserDefinedType;
                }
                if (udt.ResolvedClass is NonNullTypeDecl nntd)
                {
                    udt = nntd.RhsWithArgumentIgnoringScope(udt.TypeArgs) as UserDefinedType;
                }
            }
            var cl = udt?.ResolvedClass;

            if (cl != null)
            {
                Contract.Assert(cl.TypeArgs.Count == udt.TypeArgs.Count);
                for (var i = 0; i < cl.TypeArgs.Count; i++)
                {
                    subst.Add(cl.TypeArgs[i], udt.TypeArgs[i]);
                }

                // Add in the mappings from parent types' formal type parameters to types
                if (cl is TopLevelDeclWithMembers cls)
                {
                    foreach (var entry in cls.ParentFormalTypeParametersToActuals)
                    {
                        var v = Resolver.SubstType(entry.Value, subst);
                        subst.Add(entry.Key, v);
                    }
                }
            }

            return subst;
        }

        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(Obj != null);
            Contract.Invariant(MemberName != null);
            Contract.Invariant((Member != null) == (TypeApplication_AtEnclosingClass != null));  // TypeApplication_* are set whenever Member is set
            Contract.Invariant((Member != null) == (TypeApplication_JustMember != null));  // TypeApplication_* are set whenever Member is set
        }

        public MemberSelectExpr(IToken tok, Expression obj, string memberName)
          : base(tok)
        {
            Contract.Requires(tok != null);
            Contract.Requires(obj != null);
            Contract.Requires(memberName != null);
            this.Obj = obj;
            this.MemberName = memberName;
        }

        /// <summary>
        /// Returns a resolved MemberSelectExpr for a field.
        /// </summary>
        public MemberSelectExpr(IToken tok, Expression obj, Field field)
          : this(tok, obj, field.Name)
        {
            Contract.Requires(tok != null);
            Contract.Requires(obj != null);
            Contract.Requires(field != null);
            Contract.Requires(obj.Type != null);  // "obj" is required to be resolved

            this.Member = field;  // resolve here

            var receiverType = obj.Type.NormalizeExpand();
            this.TypeApplication_AtEnclosingClass = receiverType.TypeArgs;
            this.TypeApplication_JustMember = new List<Type>();
            this.ResolvedOutparameterTypes = new List<Type>();

            var typeMap = new Dictionary<TypeParameter, Type>();
            if (receiverType is UserDefinedType udt)
            {
                var cl = udt.ResolvedClass as TopLevelDeclWithMembers;
                Contract.Assert(cl != null);
                Contract.Assert(cl.TypeArgs.Count == TypeApplication_AtEnclosingClass.Count);
                for (var i = 0; i < cl.TypeArgs.Count; i++)
                {
                    typeMap.Add(cl.TypeArgs[i], TypeApplication_AtEnclosingClass[i]);
                }
                foreach (var entry in cl.ParentFormalTypeParametersToActuals)
                {
                    var v = Resolver.SubstType(entry.Value, typeMap);
                    typeMap.Add(entry.Key, v);
                }
            }
            else if (field.EnclosingClass == null)
            {
                // leave typeMap as the empty substitution
            }
            else
            {
                Contract.Assert(field.EnclosingClass.TypeArgs.Count == TypeApplication_AtEnclosingClass.Count);
                for (var i = 0; i < field.EnclosingClass.TypeArgs.Count; i++)
                {
                    typeMap.Add(field.EnclosingClass.TypeArgs[i], TypeApplication_AtEnclosingClass[i]);
                }
            }
            this.Type = Resolver.SubstType(field.Type, typeMap);  // resolve here
        }

        public void MemberSelectCase(Action<Field> fieldK, Action<Function> functionK)
        {
            MemberSelectCase<bool>(
              f =>
              {
                  fieldK(f);
                  return true;
              },
              f =>
              {
                  functionK(f);
                  return true;
              });
        }

        public A MemberSelectCase<A>(Func<Field, A> fieldK, Func<Function, A> functionK)
        {
            var field = Member as Field;
            var function = Member as Function;
            if (field != null)
            {
                return fieldK(field);
            }
            else
            {
                Contract.Assert(function != null);
                return functionK(function);
            }
        }

        public override IEnumerable<Expression> SubExpressions
        {
            get { yield return Obj; }
        }

        public override IEnumerable<Type> ComponentTypes => Util.Concat(TypeApplication_AtEnclosingClass, TypeApplication_JustMember);

        public List<Type> ResolvedOutparameterTypes; // filled in during resolution
    }

    /// <summary>
    /// A TypeRhs represents one of five things, each having to do with allocating something in the heap:
    ///  * new T[EE]
    ///    This allocates an array of objects of type T (where EE is a list of expression)
    ///  * new T[EE] (elementInit)
    ///    This is like the previous, but uses "elementInit" to initialize the elements of the new array.
    ///  * new T[E] [EE]
    ///    This is like the first one, but uses the elements displayed in the list EE as the initial
    ///    elements of the array.  Only a 1-dimensional array may be used in this case.  The size denoted
    ///    by E must equal the length of EE.
    ///  * new C
    ///    This allocates an object of type C
    ///  * new C.Init(EE)
    ///    This allocates an object of type C and then invokes the method/constructor Init on it
    /// There are three ways to construct a TypeRhs syntactically:
    ///  * TypeRhs(T, EE, initExpr)
    ///      -- represents "new T[EE]" (with "elementInit" being "null") and "new T[EE] (elementInit)"
    ///  * TypeRhs(T, E, EE)
    ///      -- represents "new T[E] [EE]"
    ///  * TypeRhs(C)
    ///      -- represents new C
    ///  * TypeRhs(Path, EE)
    ///    Here, Path may either be of the form C.Init
    ///      -- represents new C.Init(EE)
    ///    or all of Path denotes a type
    ///      -- represents new C._ctor(EE), where _ctor is the anonymous constructor for class C
    /// </summary>
    public class TypeRhs : AssignmentRhs
    {
        /// <summary>
        /// If ArrayDimensions != null, then the TypeRhs represents "new EType[ArrayDimensions]",
        ///     ElementInit is non-null to represent "new EType[ArrayDimensions] (elementInit)",
        ///     InitDisplay is non-null to represent "new EType[ArrayDimensions] [InitDisplay]",
        ///     and Arguments, Path, and InitCall are all null.
        /// If ArrayDimentions == null && Arguments == null, then the TypeRhs represents "new EType"
        ///     and ElementInit, Path, and InitCall are all null.
        /// If Arguments != null, then the TypeRhs represents "new Path(Arguments)"
        ///     and EType and InitCall is filled in by resolution, and ArrayDimensions == null and ElementInit == null.
        /// If OptionalNameComponent == null and Arguments != null, then the TypeRHS has not been resolved yet;
        ///   resolution will either produce an error or will chop off the last part of "EType" and move it to
        ///   OptionalNameComponent, after which the case above applies.
        /// </summary>
        public Type EType;  // in the case of Arguments != null, EType is filled in during resolution
        public readonly List<Expression> ArrayDimensions;
        public readonly Expression ElementInit;
        public readonly List<Expression> InitDisplay;
        public readonly ActualBindings/*?*/ Bindings;
        public List<Expression> Arguments
        {
            get
            {
                Contract.Requires(Bindings != null);
                return Bindings.Arguments;
            }
        }

        public Type Path;
        public CallStmt InitCall;  // may be null (and is definitely null for arrays), may be filled in during resolution
        public Type Type;  // filled in during resolution
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(EType != null || Bindings != null);
            Contract.Invariant(ElementInit == null || InitDisplay == null);
            Contract.Invariant(InitDisplay == null || ArrayDimensions.Count == 1);
            Contract.Invariant(ArrayDimensions == null || (Bindings == null && Path == null && InitCall == null && 1 <= ArrayDimensions.Count));
            Contract.Invariant(Bindings == null || (Path != null && ArrayDimensions == null && ElementInit == null && InitDisplay == null));
            Contract.Invariant(!(ArrayDimensions == null && Bindings == null) || (Path == null && InitCall == null && ElementInit == null && InitDisplay == null));
        }

        public TypeRhs(IToken tok, Type type, List<Expression> arrayDimensions, Expression elementInit)
          : base(tok)
        {
            Contract.Requires(tok != null);
            Contract.Requires(type != null);
            Contract.Requires(arrayDimensions != null && 1 <= arrayDimensions.Count);
            EType = type;
            ArrayDimensions = arrayDimensions;
            ElementInit = elementInit;
        }
        public TypeRhs(IToken tok, Type type, Expression dim, List<Expression> initDisplay)
          : base(tok)
        {
            Contract.Requires(tok != null);
            Contract.Requires(type != null);
            Contract.Requires(dim != null);
            Contract.Requires(initDisplay != null);
            EType = type;
            ArrayDimensions = new List<Expression> { dim };
            InitDisplay = initDisplay;
        }
        public TypeRhs(IToken tok, Type type)
          : base(tok)
        {
            Contract.Requires(tok != null);
            Contract.Requires(type != null);
            EType = type;
        }
        public TypeRhs(IToken tok, Type path, List<ActualBinding> arguments)
          : base(tok)
        {
            Contract.Requires(tok != null);
            Contract.Requires(path != null);
            Contract.Requires(arguments != null);
            Path = path;
            Bindings = new ActualBindings(arguments);
        }
        public override bool CanAffectPreviouslyKnownExpressions
        {
            get
            {
                if (InitCall != null)
                {
                    foreach (var mod in InitCall.Method.Mod.Expressions)
                    {
                        if (!(mod.E is ThisExpr))
                        {
                            return true;
                        }
                    }
                }
                return false;
            }
        }

        public override IEnumerable<Expression> SubExpressions
        {
            get
            {
                if (ArrayDimensions != null)
                {
                    foreach (var e in ArrayDimensions)
                    {
                        yield return e;
                    }
                    if (ElementInit != null)
                    {
                        yield return ElementInit;
                    }
                    if (InitDisplay != null)
                    {
                        foreach (var e in InitDisplay)
                        {
                            yield return e;
                        }
                    }
                }
            }
        }
        public override IEnumerable<Statement> SubStatements
        {
            get
            {
                if (InitCall != null)
                {
                    yield return InitCall;
                }
            }
        }
    }
    public class DatatypeBoundedPool : BoundedPool
    {
        public readonly DatatypeDecl Decl;

        public DatatypeBoundedPool(DatatypeDecl d)
        {
            Contract.Requires(d != null);
            Decl = d;
        }
        public override PoolVirtues Virtues => PoolVirtues.Finite | PoolVirtues.Enumerable | PoolVirtues.IndependentOfAlloc | PoolVirtues.IndependentOfAlloc_or_ExplicitAlloc;
        public override int Preference() => 8;
    }

    /// <summary>
    /// A CasePattern is either a BoundVar or a datatype constructor with optional arguments.
    /// Lexically, the CasePattern starts with an identifier.  If it continues with an open paren (as
    /// indicated by Arguments being non-null), then the CasePattern is a datatype constructor.  If
    /// it continues with a colon (which is indicated by Var.Type not being a proxy type), then it is
    /// a BoundVar.  But if it ends with just the identifier, then resolution is required to figure out
    /// which it is; in this case, Var is non-null, because this is the only place where Var.IsGhost
    /// is recorded by the parser.
    /// </summary>
    public class CasePattern<VT> where VT : IVariable
    {
        public readonly IToken Tok;
        public readonly string Id;
        // After successful resolution, exactly one of the following two fields is non-null.
        public DatatypeCtor Ctor;  // finalized by resolution (null if the pattern is a bound variable)
        public VT Var;  // finalized by resolution (null if the pattern is a constructor)  Invariant:  Var != null ==> Arguments == null
        public List<CasePattern<VT>> Arguments;

        public Expression Expr;  // an r-value version of the CasePattern; filled in by resolution

        public void MakeAConstructor()
        {
            this.Arguments = new List<CasePattern<VT>>();
        }

        public CasePattern(IToken tok, string id, [Captured] List<CasePattern<VT>> arguments)
        {
            Contract.Requires(tok != null);
            Contract.Requires(id != null);
            this.Tok = tok;
            Id = id;
            Arguments = arguments;
        }

        public CasePattern(IToken tok, VT bv)
        {
            Contract.Requires(tok != null);
            Contract.Requires(bv != null);
            this.Tok = tok;
            Id = bv.Name;
            Var = bv;
        }

        /// <summary>
        /// Sets the Expr field.  Assumes the CasePattern and its arguments to have been successfully resolved, except for assigning
        /// to Expr.
        /// </summary>
        public void AssembleExpr(List<Type> dtvTypeArgs)
        {
            Contract.Requires(Var != null || dtvTypeArgs != null);
            if (Var != null)
            {
                Contract.Assert(this.Id == this.Var.Name);
                this.Expr = new IdentifierExpr(this.Tok, this.Var);
            }
            else
            {
                var dtValue = new DatatypeValue(this.Tok, this.Ctor.EnclosingDatatype.Name, this.Id,
                  this.Arguments == null ? new List<Expression>() : this.Arguments.ConvertAll(arg => arg.Expr));
                dtValue.Ctor = this.Ctor;  // resolve here
                dtValue.InferredTypeArgs.AddRange(dtvTypeArgs);  // resolve here
                dtValue.Type = new UserDefinedType(this.Tok, this.Ctor.EnclosingDatatype.Name, this.Ctor.EnclosingDatatype, dtvTypeArgs);
                this.Expr = dtValue;
            }
        }

        public IEnumerable<VT> Vars
        {
            get
            {
                if (Var != null)
                {
                    yield return Var;
                }
                else
                {
                    if (Arguments != null)
                    {
                        foreach (var arg in Arguments)
                        {
                            foreach (var bv in arg.Vars)
                            {
                                yield return bv;
                            }
                        }
                    }
                }
            }
        }
    }

    public class VarDeclPattern : Statement
    {
        public readonly CasePattern<LocalVariable> LHS;
        public readonly Expression RHS;
        public bool HasGhostModifier;

        public VarDeclPattern(IToken tok, IToken endTok, CasePattern<LocalVariable> lhs, Expression rhs, bool hasGhostModifier = true)
          : base(tok, endTok)
        {
            LHS = lhs;
            RHS = rhs;
            HasGhostModifier = hasGhostModifier;
        }

        public override IEnumerable<Expression> NonSpecificationSubExpressions
        {
            get
            {
                foreach (var e in base.NonSpecificationSubExpressions)
                {
                    yield return e;
                }
                yield return RHS;
            }
        }

        public IEnumerable<LocalVariable> LocalVars
        {
            get
            {
                foreach (var bv in LHS.Vars)
                {
                    yield return bv;
                }
            }
        }
    }

    /// <summary>
    /// This class is really just a WhileStmt, except that it serves the purpose of remembering if the object was created as the result of a refinement
    /// merge.
    /// </summary>
    public class RefinedWhileStmt : WhileStmt
    {
        public RefinedWhileStmt(IToken tok, IToken endTok, Expression guard,
                                List<AttributedExpression> invariants, Specification<Expression> decreases, Specification<FrameExpression> mod,
                                BlockStmt body)
          : base(tok, endTok, guard, invariants, decreases, mod, body)
        {
            Contract.Requires(tok != null);
            Contract.Requires(endTok != null);
            Contract.Requires(body != null);
        }
    }

    public class WhileStmt : OneBodyLoopStmt
    {
        public readonly Expression/*?*/ Guard;

        public class LoopBodySurrogate
        {
            public readonly List<IVariable> LocalLoopTargets;
            public readonly bool UsesHeap;

            public LoopBodySurrogate(List<IVariable> localLoopTargets, bool usesHeap)
            {
                LocalLoopTargets = localLoopTargets;
                UsesHeap = usesHeap;
            }
        }

        public WhileStmt(IToken tok, IToken endTok, Expression guard,
                         List<AttributedExpression> invariants, Specification<Expression> decreases, Specification<FrameExpression> mod,
                         BlockStmt body)
          : base(tok, endTok, invariants, decreases, mod, body, null)
        {
            Contract.Requires(tok != null);
            Contract.Requires(endTok != null);
            this.Guard = guard;
        }

        public WhileStmt(IToken tok, IToken endTok, Expression guard,
                     List<AttributedExpression> invariants, Specification<Expression> decreases, Specification<FrameExpression> mod,
                     BlockStmt body, Attributes attributes)
          : base(tok, endTok, invariants, decreases, mod, body, attributes)
        {
            Contract.Requires(tok != null);
            Contract.Requires(endTok != null);
            this.Guard = guard;
        }

        public override IEnumerable<Statement> SubStatements
        {
            get
            {
                if (Body != null)
                {
                    yield return Body;
                }
            }
        }
        public override IEnumerable<Expression> NonSpecificationSubExpressions
        {
            get
            {
                foreach (var e in base.NonSpecificationSubExpressions) { yield return e; }
                if (Guard != null)
                {
                    yield return Guard;
                }
            }
        }
    }

    public abstract class OneBodyLoopStmt : LoopStmt
    {
        public readonly BlockStmt/*?*/ Body;
        public Microsoft.Dafny.WhileStmt.LoopBodySurrogate BodySurrogate;  // set by Resolver; remains null unless Body==null

        public OneBodyLoopStmt(IToken tok, IToken endTok,
          List<AttributedExpression> invariants, Specification<Expression> decreases, Specification<FrameExpression> mod,
          BlockStmt /*?*/ body, Attributes/*?*/ attributes)
          : base(tok, endTok, invariants, decreases, mod, attributes)
        {
            Body = body;
        }

        public override IEnumerable<Statement> SubStatements
        {
            get
            {
                if (Body != null)
                {
                    yield return Body;
                }
            }
        }
    }

    public class ForLoopStmt : OneBodyLoopStmt
    {
        public readonly BoundVar LoopIndex;
        public readonly Expression Start;
        public readonly Expression/*?*/ End;
        public readonly bool GoingUp;

        public ForLoopStmt(IToken tok, IToken endTok, BoundVar loopIndex, Expression start, Expression/*?*/ end, bool goingUp,
          List<AttributedExpression> invariants, Specification<Expression> decreases, Specification<FrameExpression> mod,
          BlockStmt /*?*/ body, Attributes attributes)
          : base(tok, endTok, invariants, decreases, mod, body, attributes)
        {
            Contract.Requires(tok != null);
            Contract.Requires(endTok != null);
            Contract.Requires(loopIndex != null);
            Contract.Requires(start != null);
            Contract.Requires(invariants != null);
            Contract.Requires(decreases != null);
            Contract.Requires(mod != null);
            LoopIndex = loopIndex;
            Start = start;
            End = end;
            GoingUp = goingUp;
        }

        public override IEnumerable<Expression> NonSpecificationSubExpressions
        {
            get
            {
                foreach (var e in base.NonSpecificationSubExpressions) { yield return e; }
                yield return Start;
                if (End != null)
                {
                    yield return End;
                }
            }
        }
    }

    public abstract class LoopStmt : Statement
    {
        public readonly List<AttributedExpression> Invariants;
        public readonly Specification<Expression> Decreases;
        public bool InferredDecreases;  // filled in by resolution; says that no explicit "decreases" clause was given and an attempt was made to find one automatically (which may or may not have produced anything)
        public readonly Specification<FrameExpression> Mod;
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(cce.NonNullElements(Invariants));
            Contract.Invariant(Decreases != null);
            Contract.Invariant(Mod != null);
        }
        public LoopStmt(IToken tok, IToken endTok, List<AttributedExpression> invariants, Specification<Expression> decreases, Specification<FrameExpression> mod)
        : base(tok, endTok)
        {
            Contract.Requires(tok != null);
            Contract.Requires(endTok != null);
            Contract.Requires(cce.NonNullElements(invariants));
            Contract.Requires(decreases != null);
            Contract.Requires(mod != null);

            this.Invariants = invariants;
            this.Decreases = decreases;
            this.Mod = mod;
        }
        public LoopStmt(IToken tok, IToken endTok, List<AttributedExpression> invariants, Specification<Expression> decreases, Specification<FrameExpression> mod, Attributes attributes)
           : base(tok, endTok, attributes)
        {
            Contract.Requires(tok != null);
            Contract.Requires(endTok != null);
            Contract.Requires(cce.NonNullElements(invariants));
            Contract.Requires(decreases != null);
            Contract.Requires(mod != null);

            this.Invariants = invariants;
            this.Decreases = decreases;
            this.Mod = mod;
        }
        public IEnumerable<Expression> LoopSpecificationExpressions
        {
            get
            {
                foreach (var mfe in Invariants)
                {
                    foreach (var e in Attributes.SubExpressions(mfe.Attributes)) { yield return e; }
                    yield return mfe.E;
                }
                foreach (var e in Attributes.SubExpressions(Decreases.Attributes)) { yield return e; }
                if (Decreases.Expressions != null)
                {
                    foreach (var e in Decreases.Expressions)
                    {
                        yield return e;
                    }
                }
                foreach (var e in Attributes.SubExpressions(Mod.Attributes)) { yield return e; }
                if (Mod.Expressions != null)
                {
                    foreach (var fe in Mod.Expressions)
                    {
                        yield return fe.E;
                    }
                }
            }
        }

        public override IEnumerable<Expression> NonSpecificationSubExpressions
        {
            get
            {
                foreach (var e in base.NonSpecificationSubExpressions)
                {
                    yield return e;
                }
            }
        }

        public override IEnumerable<Expression> SpecificationSubExpressions
        {
            get
            {
                foreach (var e in LoopSpecificationExpressions)
                {
                    yield return e;
                }
            }
        }
    }

    public class AlternativeLoopStmt : LoopStmt
    {
        public readonly bool UsesOptionalBraces;
        public readonly List<GuardedAlternative> Alternatives;
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(Alternatives != null);
        }
        public AlternativeLoopStmt(IToken tok, IToken endTok,
                                   List<AttributedExpression> invariants, Specification<Expression> decreases, Specification<FrameExpression> mod,
                                   List<GuardedAlternative> alternatives, bool usesOptionalBraces)
          : base(tok, endTok, invariants, decreases, mod)
        {
            Contract.Requires(tok != null);
            Contract.Requires(endTok != null);
            Contract.Requires(alternatives != null);
            this.Alternatives = alternatives;
            this.UsesOptionalBraces = usesOptionalBraces;
        }
        public AlternativeLoopStmt(IToken tok, IToken endTok,
                       List<AttributedExpression> invariants, Specification<Expression> decreases, Specification<FrameExpression> mod,
                       List<GuardedAlternative> alternatives, bool usesOptionalBraces, Attributes attributes)
          : base(tok, endTok, invariants, decreases, mod, attributes)
        {
            Contract.Requires(tok != null);
            Contract.Requires(endTok != null);
            Contract.Requires(alternatives != null);
            this.Alternatives = alternatives;
            this.UsesOptionalBraces = usesOptionalBraces;
        }
        public override IEnumerable<Statement> SubStatements
        {
            get
            {
                foreach (var alt in Alternatives)
                {
                    foreach (var s in alt.Body)
                    {
                        yield return s;
                    }
                }
            }
        }
        public override IEnumerable<Expression> NonSpecificationSubExpressions
        {
            get
            {
                foreach (var e in base.NonSpecificationSubExpressions) { yield return e; }
                foreach (var alt in Alternatives)
                {
                    yield return alt.Guard;
                }
            }
        }
    }

    public class ModifyStmt : Statement
    {
        public readonly Specification<FrameExpression> Mod;
        public readonly BlockStmt Body;

        public ModifyStmt(IToken tok, IToken endTok, Specification<FrameExpression> mod, BlockStmt body)
          : base(tok, endTok)
        {
            Contract.Requires(tok != null);
            Contract.Requires(endTok != null);
            Contract.Requires(mod != null);
            Mod = mod;
            Body = body;
        }

        public ModifyStmt(IToken tok, IToken endTok, List<FrameExpression> baseMod, Attributes attributes, BlockStmt body)
          : this(tok, endTok, new Specification<FrameExpression>(baseMod, attributes), body)
        {
        }

        public override IEnumerable<Statement> SubStatements
        {
            get
            {
                if (Body != null)
                {
                    yield return Body;
                }
            }
        }
        public override IEnumerable<Expression> SpecificationSubExpressions
        {
            get
            {
                foreach (var e in base.SpecificationSubExpressions) { yield return e; }
                foreach (var e in Attributes.SubExpressions(Mod.Attributes)) { yield return e; }
                foreach (var fe in Mod.Expressions)
                {
                    yield return fe.E;
                }
            }
        }
    }

    public class MatchStmt : Statement
    {
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(Source != null);
            Contract.Invariant(cce.NonNullElements(Cases));
            Contract.Invariant(cce.NonNullElements(MissingCases));
        }

        private Expression source;
        private List<MatchCaseStmt> cases;
        public readonly MatchingContext Context;
        public readonly List<DatatypeCtor> MissingCases = new List<DatatypeCtor>();  // filled in during resolution
        public readonly bool UsesOptionalBraces;
        public MatchStmt OrigUnresolved;  // the resolver makes this clone of the MatchStmt before it starts desugaring it
        public MatchStmt(IToken tok, IToken endTok, Expression source, [Captured] List<MatchCaseStmt> cases, bool usesOptionalBraces, MatchingContext context = null)
          : base(tok, endTok)
        {
            Contract.Requires(tok != null);
            Contract.Requires(endTok != null);
            Contract.Requires(source != null);
            Contract.Requires(cce.NonNullElements(cases));
            this.source = source;
            this.cases = cases;
            this.UsesOptionalBraces = usesOptionalBraces;
            this.Context = context is null ? new HoleCtx() : context;
        }
        public MatchStmt(IToken tok, IToken endTok, Expression source, [Captured] List<MatchCaseStmt> cases, bool usesOptionalBraces, Attributes attributes, MatchingContext context = null)
          : base(tok, endTok, attributes)
        {
            Contract.Requires(tok != null);
            Contract.Requires(endTok != null);
            Contract.Requires(source != null);
            Contract.Requires(cce.NonNullElements(cases));
            this.source = source;
            this.cases = cases;
            this.UsesOptionalBraces = usesOptionalBraces;
            this.Context = context is null ? new HoleCtx() : context;
        }

        public Expression Source
        {
            get { return source; }
        }

        public List<MatchCaseStmt> Cases
        {
            get { return cases; }
        }

        // should only be used in desugar in resolve to change the cases of the matchexpr
        public void UpdateSource(Expression source)
        {
            this.source = source;
        }

        public void UpdateCases(List<MatchCaseStmt> cases)
        {
            this.cases = cases;
        }

        public override IEnumerable<Statement> SubStatements
        {
            get
            {
                foreach (var kase in cases)
                {
                    foreach (var s in kase.Body)
                    {
                        yield return s;
                    }
                }
            }
        }
        public override IEnumerable<Expression> NonSpecificationSubExpressions
        {
            get
            {
                foreach (var e in base.NonSpecificationSubExpressions) { yield return e; }
                yield return Source;
            }
        }
    }

    public class Method : MemberDecl, Microsoft.Dafny.TypeParameter.ParentType, IMethodCodeContext
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

    public class Function : MemberDecl, Microsoft.Dafny.TypeParameter.ParentType, ICallable
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
        public Function(IToken tok, string name, bool hasStaticKeyword, bool isGhost,
          List<TypeParameter> typeArgs, List<Formal> formals, Formal result, Type resultType,
          List<AttributedExpression> req, List<FrameExpression> reads, List<AttributedExpression> ens, Specification<Expression> decreases,
          Expression/*?*/ body, IToken/*?*/ byMethodTok, BlockStmt/*?*/ byMethodBody,
          Attributes attributes, IToken/*?*/ signatureEllipsis)
          : base(tok, name, hasStaticKeyword, isGhost, attributes, signatureEllipsis != null)
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

    public class FunctionCallExpr : Expression
    {
        public readonly string Name;
        public readonly Expression Receiver;
        public readonly IToken OpenParen;  // can be null if Args.Count == 0
        public readonly Label/*?*/ AtLabel;
        public readonly ActualBindings Bindings;
        public List<Expression> Args => Bindings.Arguments;
        public List<Type> TypeApplication_AtEnclosingClass;  // filled in during resolution
        public List<Type> TypeApplication_JustFunction;  // filled in during resolution
        public bool IsByMethodCall; // filled in during resolution

        /// <summary>
        /// Return a mapping from each type parameter of the function and its enclosing class to actual type arguments.
        /// This method should only be called on fully and successfully resolved FunctionCallExpr's.
        /// </summary>
        public Dictionary<TypeParameter, Type> GetTypeArgumentSubstitutions()
        {
            var typeMap = new Dictionary<TypeParameter, Type>();
            Util.AddToDict(typeMap, Function.EnclosingClass.TypeArgs, TypeApplication_AtEnclosingClass);
            Util.AddToDict(typeMap, Function.TypeArgs, TypeApplication_JustFunction);
            return typeMap;
        }

        /// <summary>
        /// Returns a mapping from formal type parameters to actual type arguments. For example, given
        ///     trait T<A> {
        ///       function F<X>(): bv8 { ... }
        ///     }
        ///     class C<B, D> extends T<map<B, D>> { }
        /// and FunctionCallExpr o.F<int>(args) where o has type C<real, bool>, the type map returned is
        ///     A -> map<real, bool>
        ///     B -> real
        ///     D -> bool
        ///     X -> int
        /// NOTE: This method should be called only when all types have been fully and successfully
        /// resolved.
        /// </summary>
        public Dictionary<TypeParameter, Type> TypeArgumentSubstitutionsWithParents()
        {
            Contract.Requires(WasResolved());
            Contract.Ensures(Contract.Result<Dictionary<TypeParameter, Type>>() != null);

            return MemberSelectExpr.TypeArgumentSubstitutionsWithParentsAux(Receiver.Type, Function, TypeApplication_JustFunction);
        }

        public enum CoCallResolution
        {
            No,
            Yes,
            NoBecauseFunctionHasSideEffects,
            NoBecauseFunctionHasPostcondition,
            NoBecauseRecursiveCallsAreNotAllowedInThisContext,
            NoBecauseIsNotGuarded,
            NoBecauseRecursiveCallsInDestructiveContext
        }
        public CoCallResolution CoCall = CoCallResolution.No;  // indicates whether or not the call is a co-recursive call; filled in by resolution
        public string CoCallHint = null;  // possible additional hint that can be used in verifier error message, filled in by resolver

        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(Name != null);
            Contract.Invariant(Receiver != null);
            Contract.Invariant(cce.NonNullElements(Args));
            Contract.Invariant(
              Function == null || TypeApplication_AtEnclosingClass == null ||
              Function.EnclosingClass.TypeArgs.Count == TypeApplication_AtEnclosingClass.Count);
            Contract.Invariant(
              Function == null || TypeApplication_JustFunction == null ||
              Function.TypeArgs.Count == TypeApplication_JustFunction.Count);
        }

        public Function Function;  // filled in by resolution

        public FunctionCallExpr(IToken tok, string fn, Expression receiver, IToken openParen, [Captured] List<ActualBinding> args, Label/*?*/ atLabel = null)
          : this(tok, fn, receiver, openParen, new ActualBindings(args), atLabel)
        {
            Contract.Requires(tok != null);
            Contract.Requires(fn != null);
            Contract.Requires(receiver != null);
            Contract.Requires(cce.NonNullElements(args));
            Contract.Requires(openParen != null || args.Count == 0);
            Contract.Ensures(type == null);
        }

        public FunctionCallExpr(IToken tok, string fn, Expression receiver, IToken openParen, [Captured] ActualBindings bindings, Label/*?*/ atLabel = null)
          : base(tok)
        {
            Contract.Requires(tok != null);
            Contract.Requires(fn != null);
            Contract.Requires(receiver != null);
            Contract.Requires(bindings != null);
            Contract.Requires(openParen != null);
            Contract.Ensures(type == null);

            this.Name = fn;
            this.Receiver = receiver;
            this.OpenParen = openParen;
            this.AtLabel = atLabel;
            this.Bindings = bindings;
        }

        /// <summary>
        /// This constructor is intended to be used when constructing a resolved FunctionCallExpr. The "args" are expected
        /// to be already resolved, and are all given positionally.
        /// </summary>
        public FunctionCallExpr(IToken tok, string fn, Expression receiver, IToken openParen, [Captured] List<Expression> args,
          Label /*?*/ atLabel = null)
          : this(tok, fn, receiver, openParen, args.ConvertAll(e => new ActualBinding(null, e)), atLabel)
        {
            Bindings.AcceptArgumentExpressionsAsExactParameterList();
        }

        public override IEnumerable<Expression> SubExpressions
        {
            get
            {
                yield return Receiver;
                foreach (var e in Args)
                {
                    yield return e;
                }
            }
        }

        public override IEnumerable<Type> ComponentTypes => Util.Concat(TypeApplication_AtEnclosingClass, TypeApplication_JustFunction);
    }

    public class SpecialFunction : Function, ICodeContext, ICallable
    {
        public readonly ModuleDefinition Module;
        public SpecialFunction(IToken tok, string name, ModuleDefinition module, bool hasStaticKeyword, bool isGhost,
          List<TypeParameter> typeArgs, List<Formal> formals, Type resultType,
          List<AttributedExpression> req, List<FrameExpression> reads, List<AttributedExpression> ens, Specification<Expression> decreases,
          Expression body, Attributes attributes, IToken signatureEllipsis)
          : base(tok, name, hasStaticKeyword, isGhost, typeArgs, formals, null, resultType, req, reads, ens, decreases, body, null, null, attributes, signatureEllipsis)
        {
            Module = module;
        }
        ModuleDefinition ICodeContext.EnclosingModule { get { return this.Module; } }
        string ICallable.NameRelativeToModule { get { return Name; } }
    }

    public class Predicate : Function
    {
        public override string WhatKind { get { return "predicate"; } }
        public enum BodyOriginKind
        {
            OriginalOrInherited,  // this predicate definition is new (and the predicate may or may not have a body), or the predicate's body (whether or not it exists) is being inherited unmodified (from the previous refinement--it may be that the inherited body was itself an extension, for example)
            DelayedDefinition,  // this predicate declaration provides, for the first time, a body--the declaration refines a previously declared predicate, but the previous one had no body
            Extension  // this predicate extends the definition of a predicate with a body in a module being refined
        }
        public readonly BodyOriginKind BodyOrigin;
        public Predicate(IToken tok, string name, bool hasStaticKeyword, bool isGhost,
          List<TypeParameter> typeArgs, List<Formal> formals,
          List<AttributedExpression> req, List<FrameExpression> reads, List<AttributedExpression> ens, Specification<Expression> decreases,
          Expression body, BodyOriginKind bodyOrigin, IToken/*?*/ byMethodTok, BlockStmt/*?*/ byMethodBody, Attributes attributes, IToken signatureEllipsis)
          : base(tok, name, hasStaticKeyword, isGhost, typeArgs, formals, null, Type.Bool, req, reads, ens, decreases, body, byMethodTok, byMethodBody, attributes, signatureEllipsis)
        {
            Contract.Requires(bodyOrigin == Predicate.BodyOriginKind.OriginalOrInherited || body != null);
            BodyOrigin = bodyOrigin;
        }
    }

    public abstract class ExtremePredicate : Function
    {
        public enum KType { Unspecified, Nat, ORDINAL }
        public readonly KType TypeOfK;
        public bool KNat
        {
            get
            {
                return TypeOfK == KType.Nat;
            }
        }
        public readonly List<FunctionCallExpr> Uses = new List<FunctionCallExpr>();  // filled in during resolution, used by verifier
        public PrefixPredicate PrefixPredicate;  // filled in during resolution (name registration)

        public ExtremePredicate(IToken tok, string name, bool hasStaticKeyword, KType typeOfK,
          List<TypeParameter> typeArgs, List<Formal> formals,
          List<AttributedExpression> req, List<FrameExpression> reads, List<AttributedExpression> ens,
          Expression body, Attributes attributes, IToken signatureEllipsis)
          : base(tok, name, hasStaticKeyword, true, typeArgs, formals, null, Type.Bool,
                 req, reads, ens, new Specification<Expression>(new List<Expression>(), null), body, null, null, attributes, signatureEllipsis)
        {
            TypeOfK = typeOfK;
        }

        /// <summary>
        /// For the given call P(s), return P#[depth](s).  The resulting expression shares some of the subexpressions
        /// with 'fexp' (that is, what is returned is not necessarily a clone).
        /// </summary>
        public FunctionCallExpr CreatePrefixPredicateCall(FunctionCallExpr fexp, Expression depth)
        {
            Contract.Requires(fexp != null);
            Contract.Requires(fexp.Function == this);
            Contract.Requires(depth != null);
            Contract.Ensures(Contract.Result<FunctionCallExpr>() != null);

            var args = new List<Expression>() { depth };
            args.AddRange(fexp.Args);
            var prefixPredCall = new FunctionCallExpr(fexp.Tok, this.PrefixPredicate.Name, fexp.Receiver, fexp.OpenParen, args);
            prefixPredCall.Function = this.PrefixPredicate;  // resolve here
            prefixPredCall.TypeApplication_AtEnclosingClass = fexp.TypeApplication_AtEnclosingClass;  // resolve here
            prefixPredCall.TypeApplication_JustFunction = fexp.TypeApplication_JustFunction;  // resolve here
            prefixPredCall.Type = fexp.Type;  // resolve here
            prefixPredCall.CoCall = fexp.CoCall;  // resolve here
            return prefixPredCall;
        }
    }

    /// <summary>
    /// An PrefixPredicate is the inductive unrolling P# implicitly declared for every extreme predicate P.
    /// </summary>
    public class PrefixPredicate : Function
    {
        public override string WhatKind { get { return "prefix predicate"; } }
        public readonly Formal K;
        public readonly ExtremePredicate ExtremePred;
        public PrefixPredicate(IToken tok, string name, bool hasStaticKeyword,
          List<TypeParameter> typeArgs, Formal k, List<Formal> formals,
          List<AttributedExpression> req, List<FrameExpression> reads, List<AttributedExpression> ens, Specification<Expression> decreases,
          Expression body, Attributes attributes, ExtremePredicate extremePred)
          : base(tok, name, hasStaticKeyword, true, typeArgs, formals, null, Type.Bool, req, reads, ens, decreases, body, null, null, attributes, null)
        {
            Contract.Requires(k != null);
            Contract.Requires(extremePred != null);
            Contract.Requires(formals != null && 1 <= formals.Count && formals[0] == k);
            K = k;
            ExtremePred = extremePred;
        }
    }

    public class LeastPredicate : ExtremePredicate
    {
        public override string WhatKind { get { return "least predicate"; } }
        public LeastPredicate(IToken tok, string name, bool hasStaticKeyword, KType typeOfK,
          List<TypeParameter> typeArgs, List<Formal> formals,
          List<AttributedExpression> req, List<FrameExpression> reads, List<AttributedExpression> ens,
          Expression body, Attributes attributes, IToken signatureEllipsis)
          : base(tok, name, hasStaticKeyword, typeOfK, typeArgs, formals,
                 req, reads, ens, body, attributes, signatureEllipsis)
        {
        }
    }

    public class GreatestPredicate : ExtremePredicate
    {
        public override string WhatKind { get { return "greatest predicate"; } }
        public GreatestPredicate(IToken tok, string name, bool hasStaticKeyword, KType typeOfK,
          List<TypeParameter> typeArgs, List<Formal> formals,
          List<AttributedExpression> req, List<FrameExpression> reads, List<AttributedExpression> ens,
          Expression body, Attributes attributes, IToken signatureEllipsis)
          : base(tok, name, hasStaticKeyword, typeOfK, typeArgs, formals,
                 req, reads, ens, body, attributes, signatureEllipsis)
        {
        }
    }

    public class TwoStatePredicate : TwoStateFunction
    {
        public override string WhatKind { get { return "twostate predicate"; } }
        public TwoStatePredicate(IToken tok, string name, bool hasStaticKeyword,
                         List<TypeParameter> typeArgs, List<Formal> formals,
                         List<AttributedExpression> req, List<FrameExpression> reads, List<AttributedExpression> ens, Specification<Expression> decreases,
                         Expression body, Attributes attributes, IToken signatureEllipsis)
          : base(tok, name, hasStaticKeyword, typeArgs, formals, null, Type.Bool, req, reads, ens, decreases, body, attributes, signatureEllipsis)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(typeArgs != null);
            Contract.Requires(formals != null);
            Contract.Requires(req != null);
            Contract.Requires(reads != null);
            Contract.Requires(ens != null);
            Contract.Requires(decreases != null);
        }
    }

    public class TwoStateFunction : Function
    {
        public override string WhatKind { get { return "twostate function"; } }
        public TwoStateFunction(IToken tok, string name, bool hasStaticKeyword,
                         List<TypeParameter> typeArgs, List<Formal> formals, Formal result, Type resultType,
                         List<AttributedExpression> req, List<FrameExpression> reads, List<AttributedExpression> ens, Specification<Expression> decreases,
                         Expression body, Attributes attributes, IToken signatureEllipsis)
          : base(tok, name, hasStaticKeyword, true, typeArgs, formals, result, resultType, req, reads, ens, decreases, body, null, null, attributes, signatureEllipsis)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(typeArgs != null);
            Contract.Requires(formals != null);
            Contract.Requires(resultType != null);
            Contract.Requires(req != null);
            Contract.Requires(reads != null);
            Contract.Requires(ens != null);
            Contract.Requires(decreases != null);
        }
        public override bool ReadsHeap { get { return true; } }
    }
    // Represents module X { ... }
    public class LiteralModuleDecl : ModuleDecl
    {
        public readonly ModuleDefinition ModuleDef;
        public ModuleSignature DefaultExport;  // the default export set of the module. fill in by the resolver.

        private ModuleSignature emptySignature;
        public override ModuleSignature AccessibleSignature(bool ignoreExports)
        {
            if (ignoreExports)
            {
                return Signature;
            }
            return this.AccessibleSignature();
        }
        public override ModuleSignature AccessibleSignature()
        {
            if (DefaultExport == null)
            {
                if (emptySignature == null)
                {
                    emptySignature = new ModuleSignature();
                }
                return emptySignature;
            }
            return DefaultExport;
        }

        public LiteralModuleDecl(ModuleDefinition moduleDef, ModuleDefinition enclosingModuleDefinition)
          : base(moduleDef.Tok, moduleDef.Name, enclosingModuleDefinition, false, false)
        {
            ModuleDef = moduleDef;
        }
        public override object Dereference() { return ModuleDef; }
    }

    // Represents a submodule declaration at module level scope
    abstract public class ModuleDecl : TopLevelDecl
    {
        public override string WhatKind { get { return "module"; } }
        public ModuleSignature Signature; // filled in by resolution, in topological order.
        public virtual ModuleSignature AccessibleSignature(bool ignoreExports)
        {
            Contract.Requires(Signature != null);
            return Signature;
        }
        public virtual ModuleSignature AccessibleSignature()
        {
            Contract.Requires(Signature != null);
            return Signature;
        }
        public int Height;

        public readonly bool Opened;

        public ModuleDecl(IToken tok, string name, ModuleDefinition enclosingModuleDefinition, bool opened, bool isRefining)
          : base(tok, name, enclosingModuleDefinition, new List<TypeParameter>(), null, isRefining)
        {
            Height = -1;
            Signature = null;
            Opened = opened;
        }
        public abstract object Dereference();

        public int? ResolvedHash { get; set; }
    }

    public class ModuleQualifiedId
    {
        public readonly List<IToken> Path; // Path != null && Path.Count > 0

        public ModuleQualifiedId(List<IToken> path)
        {
            Contract.Assert(path != null && path.Count > 0);
            this.Path = path; // note that the list is aliased -- not to be modified after construction
        }

        // Creates a clone, including a copy of the list;
        // if the argument is true, resolution information is included
        public ModuleQualifiedId Clone(bool includeResInfo)
        {
            List<IToken> newlist = new List<IToken>(Path);
            ModuleQualifiedId cl = new ModuleQualifiedId(newlist);
            if (includeResInfo)
            {
                cl.Root = this.Root;
                cl.Decl = this.Decl;
                cl.Def = this.Def;
                cl.Sig = this.Sig;
                Contract.Assert(this.Def == this.Sig.ModuleDef);
            }
            return cl;
        }

        public string rootName()
        {
            return Path[0].val;
        }

        public IToken rootToken()
        {
            return Path[0];
        }

        override public string ToString()
        {
            string s = Path[0].val;
            for (int i = 1; i < Path.Count; ++i)
            {
                s = s + "." + Path[i].val;
            }

            return s;
        }

        public void SetRoot(ModuleDecl m)
        {
            this.Root = m;
        }

        public void Set(ModuleDecl m)
        {
            if (m == null)
            {
                this.Decl = null;
                this.Def = null;
                this.Sig = null;
            }
            else
            {
                this.Decl = m;
                this.Def = ((LiteralModuleDecl)m).ModuleDef;
                this.Sig = m.Signature;
            }
        }

        // The following are filled in during resolution
        // Note that the root (first path segment) is resolved initially,
        // as it is needed to determine dependency relationships.
        // Then later the rest of the path is resolved, at which point all of the
        // ids in the path have been fully resolved
        // Note also that the resolution of the root depends on the syntactice location
        // of the qualified id; the resolution of subsequent ids depends on the
        // default export set of the previous id.
        public ModuleDecl Root; // the module corresponding to Path[0].val
        public ModuleDecl Decl; // the module corresponding to the full path
        public ModuleDefinition Def; // the module definition corresponding to the full path
        public ModuleSignature Sig; // the module signature corresponding to the full path
    }
    // Represents "module name = path;", where name is an identifier and path is a possibly qualified name.
    public class AliasModuleDecl : ModuleDecl
    {
        public readonly ModuleQualifiedId TargetQId; // generated by the parser, this is looked up
        public readonly List<IToken> Exports; // list of exports sets
        public bool ShadowsLiteralModule;  // initialized early during Resolution (and used not long after that); true for "import opened A = A" where "A" is a literal module in the same scope

        public AliasModuleDecl(ModuleQualifiedId targetQId, IToken tok, ModuleDefinition enclosingModuleDefinition, bool opened, List<IToken> exports)
          : base(tok, tok.val, enclosingModuleDefinition, opened, false)
        {
            Contract.Requires(targetQId != null && targetQId.Path.Count > 0);
            Contract.Requires(exports != null);
            Contract.Requires(exports.Count == 0 || targetQId.Path.Count == 1);
            TargetQId = targetQId;
            Exports = exports;
        }
        public override object Dereference() { return Signature.ModuleDef; }
    }

    // Represents "module name as path [ = compilePath];", where name is a identifier and path is a possibly qualified name.
    // Used to be called ModuleFacadeDecl -- renamed to be like LiteralModuleDecl, AliasModuleDecl
    public class AbstractModuleDecl : ModuleDecl
    {
        public readonly ModuleQualifiedId QId;
        public readonly List<IToken> Exports; // list of exports sets
        public ModuleDecl CompileRoot;
        public ModuleSignature OriginalSignature;

        public AbstractModuleDecl(ModuleQualifiedId qId, IToken tok, ModuleDefinition enclosingModuleDefinition, bool opened, List<IToken> exports)
          : base(tok, tok.val, enclosingModuleDefinition, opened, false)
        {
            Contract.Requires(qId != null && qId.Path.Count > 0);
            Contract.Requires(exports != null);

            QId = qId;
            Exports = exports;
        }
        public override object Dereference() { return this; }
    }

    public class TraitDecl : ClassDecl
    {
        public override string WhatKind { get { return "trait"; } }
        public bool IsParent { set; get; }
        public TraitDecl(IToken tok, string name, ModuleDefinition enclosingModuleDefinition,
          List<TypeParameter> typeArgs, [Captured] List<MemberDecl> members, Attributes attributes, bool isRefining, List<Type>/*?*/ parentTraits)
          : base(tok, name, enclosingModuleDefinition, typeArgs, members, attributes, isRefining, parentTraits) { }
    }
    public class InheritanceInformationClass
    {
        private readonly Dictionary<TraitDecl, List<(Type, List<TraitDecl> /*via this parent path*/)>> info = new Dictionary<TraitDecl, List<(Type, List<TraitDecl>)>>();

        /// <summary>
        /// Returns a subset of the trait's ParentTraits, but not repeating any head type.
        /// Assumes the declaration has been successfully resolved.
        /// </summary>
        public List<Type> UniqueParentTraits()
        {
            return info.ToList().ConvertAll(entry => entry.Value[0].Item1);
        }

        public void Record(TraitDecl traitHead, UserDefinedType parentType)
        {
            Contract.Requires(traitHead != null);
            Contract.Requires(parentType != null);
            Contract.Requires(parentType.ResolvedClass is NonNullTypeDecl nntd && nntd.ViewAsClass == traitHead);

            if (!info.TryGetValue(traitHead, out var list))
            {
                list = new List<(Type, List<TraitDecl>)>();
                info.Add(traitHead, list);
            }
            list.Add((parentType, new List<TraitDecl>()));
        }

        public void Extend(TraitDecl parent, InheritanceInformationClass parentInfo, Dictionary<TypeParameter, Type> typeMap)
        {
            Contract.Requires(parent != null);
            Contract.Requires(parentInfo != null);
            Contract.Requires(typeMap != null);

            foreach (var entry in parentInfo.info)
            {
                var traitHead = entry.Key;
                if (!info.TryGetValue(traitHead, out var list))
                {
                    list = new List<(Type, List<TraitDecl>)>();
                    info.Add(traitHead, list);
                }
                foreach (var pair in entry.Value)
                {
                    var ty = Resolver.SubstType(pair.Item1, typeMap);
                    // prepend the path with "parent"
                    var parentPath = new List<TraitDecl>() { parent };
                    parentPath.AddRange(pair.Item2);
                    list.Add((ty, parentPath));
                }
            }
        }

        public IEnumerable<List<(Type, List<TraitDecl>)>> GetTypeInstantiationGroups()
        {
            foreach (var pair in info.Values)
            {
                yield return pair;
            }
        }
    }

    public class NonNullTypeDecl : SubsetTypeDecl
    {
        public override string WhatKind { get { return "non-null type"; } }
        public readonly ClassDecl ClassDecl;
        /// <summary>
        /// The public constructor is NonNullTypeDecl(ClassDecl cl). The rest is pretty crazy: There are stages of "this"-constructor calls
        /// in order to build values that depend on previously computed parameters.
        /// </summary>
        public NonNullTypeDecl(ClassDecl classDecl)
          : this(classDecl, classDecl.TypeArgs.ConvertAll(tp => new TypeParameter(tp.Tok, tp.Name, tp.VarianceSyntax, tp.Characteristics)))
        {
            Contract.Requires(classDecl != null);
        }

        private NonNullTypeDecl(ClassDecl classDecl, List<TypeParameter> tps)
          : this(classDecl, tps,
          new BoundVar(classDecl.Tok, "c", new UserDefinedType(classDecl.Tok, classDecl.Name + "?", tps.Count == 0 ? null : tps.ConvertAll(tp => (Type)new UserDefinedType(tp)))))
        {
            Contract.Requires(classDecl != null);
            Contract.Requires(tps != null);
        }

        private NonNullTypeDecl(ClassDecl cl, List<TypeParameter> tps, BoundVar boundVar)
          : base(cl.Tok, cl.Name, new TypeParameter.TypeParameterCharacteristics(), tps, cl.EnclosingModuleDefinition, boundVar,
          new BinaryExpr(cl.Tok, BinaryExpr.Opcode.Neq, new IdentifierExpr(cl.Tok, boundVar), new LiteralExpr(cl.Tok)),
          SubsetTypeDecl.WKind.Special, null, BuiltIns.AxiomAttribute())
        {
            Contract.Requires(cl != null);
            Contract.Requires(tps != null);
            Contract.Requires(boundVar != null);
            ClassDecl = cl;
        }

        public override List<Type> ParentTypes(List<Type> typeArgs)
        {
            List<Type> result = new List<Type>(base.ParentTypes(typeArgs));

            foreach (var rhsParentType in ClassDecl.ParentTypes(typeArgs))
            {
                var rhsParentUdt = (UserDefinedType)rhsParentType; // all parent types of .Class are expected to be possibly-null class types
                Contract.Assert(rhsParentUdt.ResolvedClass is ClassDecl);
                result.Add(UserDefinedType.CreateNonNullType(rhsParentUdt));
            }

            return result;
        }
    }

    public class ClassDecl : TopLevelDeclWithMembers, RevealableTypeDecl
    {
        public override string WhatKind { get { return "class"; } }
        public override bool CanBeRevealed() { return true; }
        public bool HasConstructor;  // filled in (early) during resolution; true iff there exists a member that is a Constructor
        public readonly NonNullTypeDecl NonNullTypeDecl;
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(cce.NonNullElements(Members));
            Contract.Invariant(ParentTraits != null);
        }

        public ClassDecl(IToken tok, string name, ModuleDefinition enclosingModuleDefinition,
          List<TypeParameter> typeArgs, [Captured] List<MemberDecl> members, Attributes attributes, bool isRefining, List<Type>/*?*/ parentTraits)
          : base(tok, name, enclosingModuleDefinition, typeArgs, members, attributes, isRefining, parentTraits)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(enclosingModuleDefinition != null);
            Contract.Requires(cce.NonNullElements(typeArgs));
            Contract.Requires(cce.NonNullElements(members));
            Contract.Assume(!(this is ArrowTypeDecl));  // this is also a precondition, really, but "this" cannot be mentioned in Requires of a constructor; ArrowTypeDecl should use the next constructor
            if (!IsDefaultClass)
            {
                NonNullTypeDecl = new NonNullTypeDecl(this);
            }
            this.NewSelfSynonym();
        }
        /// <summary>
        /// The following constructor is supposed to be called by the ArrowTypeDecl subtype, in order to avoid
        /// the call to this.NewSelfSynonym() (because NewSelfSynonym() depends on the .Arity field having been
        /// set, which it hasn't during the base call of the ArrowTypeDecl constructor). Instead, the ArrowTypeDecl
        /// constructor will do that call.
        /// </summary>
        protected ClassDecl(IToken tok, string name, ModuleDefinition module,
          List<TypeParameter> typeArgs, [Captured] List<MemberDecl> members, Attributes attributes, bool isRefining)
          : base(tok, name, module, typeArgs, members, attributes, isRefining, null)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(module != null);
            Contract.Requires(cce.NonNullElements(typeArgs));
            Contract.Requires(cce.NonNullElements(members));
            Contract.Assume(this is ArrowTypeDecl);  // this is also a precondition, really, but "this" cannot be mentioned in Requires of a constructor
        }
        public virtual bool IsDefaultClass
        {
            get
            {
                return false;
            }
        }

        public bool IsObjectTrait
        {
            get => Name == "object";
        }

        internal bool HeadDerivesFrom(TopLevelDecl b)
        {
            Contract.Requires(b != null);
            return this == b || this.ParentTraitHeads.Exists(tr => tr.HeadDerivesFrom(b));
        }

        public List<Type> NonNullTraitsWithArgument(List<Type> typeArgs)
        {
            Contract.Requires(typeArgs != null);
            Contract.Requires(typeArgs.Count == TypeArgs.Count);

            // Instantiate with the actual type arguments
            if (typeArgs.Count == 0)
            {
                // this optimization seems worthwhile
                return ParentTraits;
            }
            else
            {
                var subst = Resolver.TypeSubstitutionMap(TypeArgs, typeArgs);
                return ParentTraits.ConvertAll(traitType => Resolver.SubstType(traitType, subst));
            }
        }

        public List<Type> PossiblyNullTraitsWithArgument(List<Type> typeArgs)
        {
            Contract.Requires(typeArgs != null);
            Contract.Requires(typeArgs.Count == TypeArgs.Count);
            // Instantiate with the actual type arguments
            var subst = Resolver.TypeSubstitutionMap(TypeArgs, typeArgs);
            return ParentTraits.ConvertAll(traitType => (Type)UserDefinedType.CreateNullableType((UserDefinedType)Resolver.SubstType(traitType, subst)));
        }

        public override List<Type> ParentTypes(List<Type> typeArgs)
        {
            return PossiblyNullTraitsWithArgument(typeArgs);
        }

        TopLevelDecl RevealableTypeDecl.AsTopLevelDecl { get => this; }
    }

    public class DefaultClassDecl : ClassDecl
    {
        public DefaultClassDecl(ModuleDefinition enclosingModuleDefinition, [Captured] List<MemberDecl> members)
          : base(Token.NoToken, "_default", enclosingModuleDefinition, new List<TypeParameter>(), members, null, false, null)
        {
            Contract.Requires(enclosingModuleDefinition != null);
            Contract.Requires(cce.NonNullElements(members));
        }
        public override bool IsDefaultClass
        {
            get
            {
                return true;
            }
        }
    }

    public class ArrayClassDecl : ClassDecl
    {
        public override string WhatKind { get { return "array type"; } }
        public readonly int Dims;
        public ArrayClassDecl(int dims, ModuleDefinition enclosingModuleDefinition, Attributes attributes)
        : base(Token.NoToken, BuiltIns.ArrayClassName(dims), enclosingModuleDefinition,
          new List<TypeParameter>(new TypeParameter[] { new TypeParameter(Token.NoToken, "arg", TypeParameter.TPVarianceSyntax.NonVariant_Strict) }),
          new List<MemberDecl>(), attributes, false, null)
        {
            Contract.Requires(1 <= dims);
            Contract.Requires(enclosingModuleDefinition != null);

            Dims = dims;
            // Resolve type parameter
            Contract.Assert(TypeArgs.Count == 1);
            var tp = TypeArgs[0];
            tp.Parent = this;
            tp.PositionalIndex = 0;
        }
    }

    public class ArrowTypeDecl : ClassDecl
    {
        public override string WhatKind { get { return "function type"; } }
        public readonly int Arity;
        public readonly Function Requires;
        public readonly Function Reads;

        public ArrowTypeDecl(List<TypeParameter> typeArgs, Function requires, Function reads, ModuleDefinition enclosingModuleDefinition, Attributes attributes)
          : base(Token.NoToken, ArrowType.ArrowTypeName(typeArgs.Count - 1), enclosingModuleDefinition, typeArgs,
                 new List<MemberDecl> { requires, reads }, attributes, false)
        {
            Contract.Requires(typeArgs != null && 1 <= typeArgs.Count);
            Contract.Requires(requires != null);
            Contract.Requires(reads != null);
            Contract.Requires(enclosingModuleDefinition != null);
            Arity = typeArgs.Count - 1;
            Requires = requires;
            Reads = reads;
            Requires.EnclosingClass = this;
            Reads.EnclosingClass = this;
            this.NewSelfSynonym();
        }
    }

    public class IteratorDecl : ClassDecl, IMethodCodeContext
    {
        public override string WhatKind { get { return "iterator"; } }
        public readonly List<Formal> Ins;
        public readonly List<Formal> Outs;
        public readonly Specification<FrameExpression> Reads;
        public readonly Specification<FrameExpression> Modifies;
        public readonly Specification<Expression> Decreases;
        public readonly List<AttributedExpression> Requires;
        public readonly List<AttributedExpression> Ensures;
        public readonly List<AttributedExpression> YieldRequires;
        public readonly List<AttributedExpression> YieldEnsures;
        public readonly BlockStmt Body;
        public bool SignatureIsOmitted { get { return SignatureEllipsis != null; } }
        public readonly IToken SignatureEllipsis;
        public readonly List<Field> OutsFields;
        public readonly List<Field> OutsHistoryFields;  // these are the 'xs' variables
        public readonly List<Field> DecreasesFields;  // filled in during resolution
        public SpecialField Member_Modifies;  // filled in during resolution
        public SpecialField Member_Reads;  // filled in during resolution
        public SpecialField Member_New;  // filled in during resolution
        public Constructor Member_Init;  // created during registration phase of resolution; its specification is filled in during resolution
        public Predicate Member_Valid;  // created during registration phase of resolution; its specification is filled in during resolution
        public Method Member_MoveNext;  // created during registration phase of resolution; its specification is filled in during resolution
        public readonly LocalVariable YieldCountVariable;

        public IteratorDecl(IToken tok, string name, ModuleDefinition enclosingModuleDefinition, List<TypeParameter> typeArgs,
                            List<Formal> ins, List<Formal> outs,
                            Specification<FrameExpression> reads, Specification<FrameExpression> modifies, Specification<Expression> decreases,
                            List<AttributedExpression> requires,
                            List<AttributedExpression> ensures,
                            List<AttributedExpression> yieldRequires,
                            List<AttributedExpression> yieldEnsures,
                            BlockStmt body, Attributes attributes, IToken signatureEllipsis)
          : base(tok, name, enclosingModuleDefinition, typeArgs, new List<MemberDecl>(), attributes, signatureEllipsis != null, null)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(enclosingModuleDefinition != null);
            Contract.Requires(typeArgs != null);
            Contract.Requires(ins != null);
            Contract.Requires(outs != null);
            Contract.Requires(reads != null);
            Contract.Requires(modifies != null);
            Contract.Requires(decreases != null);
            Contract.Requires(requires != null);
            Contract.Requires(ensures != null);
            Contract.Requires(yieldRequires != null);
            Contract.Requires(yieldEnsures != null);
            Ins = ins;
            Outs = outs;
            Reads = reads;
            Modifies = modifies;
            Decreases = decreases;
            Requires = requires;
            Ensures = ensures;
            YieldRequires = yieldRequires;
            YieldEnsures = yieldEnsures;
            Body = body;
            SignatureEllipsis = signatureEllipsis;

            OutsFields = new List<Field>();
            OutsHistoryFields = new List<Field>();
            DecreasesFields = new List<Field>();

            YieldCountVariable = new LocalVariable(tok, tok, "_yieldCount", new EverIncreasingType(), true);
            YieldCountVariable.type = YieldCountVariable.OptionalType;  // resolve YieldCountVariable here
        }

        /// <summary>
        /// Returns the non-null expressions of this declaration proper (that is, do not include the expressions of substatements).
        /// Does not include the generated class members.
        /// </summary>
        public virtual IEnumerable<Expression> SubExpressions
        {
            get
            {
                foreach (var e in Attributes.SubExpressions(Attributes))
                {
                    yield return e;
                }
                foreach (var e in Attributes.SubExpressions(Reads.Attributes))
                {
                    yield return e;
                }
                foreach (var e in Reads.Expressions)
                {
                    yield return e.E;
                }
                foreach (var e in Attributes.SubExpressions(Modifies.Attributes))
                {
                    yield return e;
                }
                foreach (var e in Modifies.Expressions)
                {
                    yield return e.E;
                }
                foreach (var e in Attributes.SubExpressions(Decreases.Attributes))
                {
                    yield return e;
                }
                foreach (var e in Decreases.Expressions)
                {
                    yield return e;
                }
                foreach (var e in Requires)
                {
                    yield return e.E;
                }
                foreach (var e in Ensures)
                {
                    yield return e.E;
                }
                foreach (var e in YieldRequires)
                {
                    yield return e.E;
                }
                foreach (var e in YieldEnsures)
                {
                    yield return e.E;
                }
            }
        }

        /// <summary>
        /// This Dafny type exists only for the purpose of giving the yield-count variable a type, so
        /// that the type can be recognized during translation of Dafny into Boogie.  It represents
        /// an integer component in a "decreases" clause whose order is (\lambda x,y :: x GREATER y),
        /// not the usual (\lambda x,y :: x LESS y AND 0 ATMOST y).
        /// </summary>
        public class EverIncreasingType : BasicType
        {
            [Pure]
            public override string TypeName(ModuleDefinition context, bool parseAble)
            {
                Contract.Assert(parseAble == false);

                return "_increasingInt";
            }
            public override bool Equals(Type that, bool keepConstraints = false)
            {
                return that.NormalizeExpand(keepConstraints) is EverIncreasingType;
            }
        }

        bool ICodeContext.IsGhost { get { return false; } }
        List<TypeParameter> ICodeContext.TypeArgs { get { return this.TypeArgs; } }
        List<Formal> ICodeContext.Ins { get { return this.Ins; } }
        List<Formal> IMethodCodeContext.Outs { get { return this.Outs; } }
        Specification<FrameExpression> IMethodCodeContext.Modifies { get { return this.Modifies; } }
        IToken ICallable.Tok { get { return this.Tok; } }
        string ICallable.NameRelativeToModule { get { return this.Name; } }
        Specification<Expression> ICallable.Decreases { get { return this.Decreases; } }
        bool _inferredDecr;
        bool ICallable.InferredDecreases
        {
            set { _inferredDecr = value; }
            get { return _inferredDecr; }
        }

        ModuleDefinition ICodeContext.EnclosingModule { get { return this.EnclosingModuleDefinition; } }
        bool ICodeContext.MustReverify { get { return false; } }
        public bool AllowsNontermination
        {
            get
            {
                return Contract.Exists(Decreases.Expressions, e => e is WildcardExpr);
            }
        }
    }

    public abstract class TopLevelDeclWithMembers : TopLevelDecl
    {
        public readonly List<MemberDecl> Members;

        // The following fields keep track of parent traits
        public readonly List<MemberDecl> InheritedMembers = new List<MemberDecl>();  // these are instance members declared in parent traits
        public readonly List<Type> ParentTraits;  // these are the types that are parsed after the keyword 'extends'; note, for a successfully resolved program, these are UserDefinedType's where .ResolvedClas is NonNullTypeDecl
        public readonly Dictionary<TypeParameter, Type> ParentFormalTypeParametersToActuals = new Dictionary<TypeParameter, Type>();  // maps parent traits' type parameters to actuals

        /// <summary>
        /// TraitParentHeads contains the head of each distinct trait parent. It is initialized during resolution.
        /// </summary>
        public readonly List<TraitDecl> ParentTraitHeads = new List<TraitDecl>();

        public InheritanceInformationClass ParentTypeInformation;  // filled in during resolution
        public class InheritanceInformationClass
        {
            private readonly Dictionary<TraitDecl, List<(Type, List<TraitDecl> /*via this parent path*/)>> info = new Dictionary<TraitDecl, List<(Type, List<TraitDecl>)>>();

            /// <summary>
            /// Returns a subset of the trait's ParentTraits, but not repeating any head type.
            /// Assumes the declaration has been successfully resolved.
            /// </summary>
            public List<Type> UniqueParentTraits()
            {
                return info.ToList().ConvertAll(entry => entry.Value[0].Item1);
            }

            public void Record(TraitDecl traitHead, UserDefinedType parentType)
            {
                Contract.Requires(traitHead != null);
                Contract.Requires(parentType != null);
                Contract.Requires(parentType.ResolvedClass is NonNullTypeDecl nntd && nntd.ViewAsClass == traitHead);

                if (!info.TryGetValue(traitHead, out var list))
                {
                    list = new List<(Type, List<TraitDecl>)>();
                    info.Add(traitHead, list);
                }
                list.Add((parentType, new List<TraitDecl>()));
            }

            public void Extend(TraitDecl parent, InheritanceInformationClass parentInfo, Dictionary<TypeParameter, Type> typeMap)
            {
                Contract.Requires(parent != null);
                Contract.Requires(parentInfo != null);
                Contract.Requires(typeMap != null);

                foreach (var entry in parentInfo.info)
                {
                    var traitHead = entry.Key;
                    if (!info.TryGetValue(traitHead, out var list))
                    {
                        list = new List<(Type, List<TraitDecl>)>();
                        info.Add(traitHead, list);
                    }
                    foreach (var pair in entry.Value)
                    {
                        var ty = Resolver.SubstType(pair.Item1, typeMap);
                        // prepend the path with "parent"
                        var parentPath = new List<TraitDecl>() { parent };
                        parentPath.AddRange(pair.Item2);
                        list.Add((ty, parentPath));
                    }
                }
            }

            public IEnumerable<List<(Type, List<TraitDecl>)>> GetTypeInstantiationGroups()
            {
                foreach (var pair in info.Values)
                {
                    yield return pair;
                }
            }
        }

        public TopLevelDeclWithMembers(IToken tok, string name, ModuleDefinition enclosingModuleDefinition, List<TypeParameter> typeArgs, List<MemberDecl> members, Attributes attributes, bool isRefining, List<Type>/*?*/ traits = null)
          : base(tok, name, enclosingModuleDefinition, typeArgs, attributes, isRefining)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(cce.NonNullElements(typeArgs));
            Contract.Requires(cce.NonNullElements(members));
            Members = members;
            ParentTraits = traits ?? new List<Type>();
        }

        public static List<UserDefinedType> CommonTraits(TopLevelDeclWithMembers a, TopLevelDeclWithMembers b)
        {
            Contract.Requires(a != null);
            Contract.Requires(b != null);
            var aa = a.TraitAncestors();
            var bb = b.TraitAncestors();
            aa.IntersectWith(bb);
            var types = new List<UserDefinedType>();
            foreach (var t in aa)
            {
                var typeArgs = t.TypeArgs.ConvertAll(tp => a.ParentFormalTypeParametersToActuals[tp]);
                var u = new UserDefinedType(t.Tok, t.Name + "?", t, typeArgs);
                types.Add(u);
            }
            return types;
        }

        /// <summary>
        /// Returns the set of transitive parent traits (not including "this" itself).
        /// This method assumes the .ParentTraits fields have been checked for various cycle restrictions.
        /// </summary>
        public ISet<TraitDecl> TraitAncestors()
        {
            var s = new HashSet<TraitDecl>();
            AddTraitAncestors(s);
            return s;
        }
        /// <summary>
        /// Adds to "s" the transitive parent traits (not including "this" itself).
        /// This method assumes the .ParentTraits fields have been checked for various cycle restrictions.
        /// </summary>
        private void AddTraitAncestors(ISet<TraitDecl> s)
        {
            Contract.Requires(s != null);
            foreach (var parent in ParentTraits)
            {
                var udt = (UserDefinedType)parent;  // in a successfully resolved program, we expect all .ParentTraits to be a UserDefinedType
                var nntd = (NonNullTypeDecl)udt.ResolvedClass;  // we expect the trait type to be the non-null version of the trait type
                var tr = (TraitDecl)nntd.ClassDecl;
                s.Add(tr);
                tr.AddTraitAncestors(s);
            }
        }
    }

    public class OpaqueTypeDecl : TopLevelDeclWithMembers, Microsoft.Dafny.TypeParameter.ParentType, RevealableTypeDecl
    {
        public override string WhatKind { get { return "opaque type"; } }
        public override bool CanBeRevealed() { return true; }
        public readonly Microsoft.Dafny.TypeParameter.TypeParameterCharacteristics Characteristics;
        public bool SupportsEquality
        {
            get { return Characteristics.EqualitySupport != TypeParameter.EqualitySupportValue.Unspecified; }
        }

        public OpaqueTypeDecl(IToken tok, string name, ModuleDefinition enclosingModuleDefinition, Microsoft.Dafny.TypeParameter.TypeParameterCharacteristics characteristics, List<TypeParameter> typeArgs, List<MemberDecl> members, Attributes attributes, bool isRefining)
          : base(tok, name, enclosingModuleDefinition, typeArgs, members, attributes, isRefining)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(enclosingModuleDefinition != null);
            Contract.Requires(typeArgs != null);
            Characteristics = characteristics;
            this.NewSelfSynonym();
        }

        public TopLevelDecl AsTopLevelDecl
        {
            get { return this; }
        }
    }

    public class NewtypeDecl : TopLevelDeclWithMembers, RevealableTypeDecl, RedirectingTypeDecl
    {
        public override string WhatKind { get { return "newtype"; } }
        public override bool CanBeRevealed() { return true; }
        public readonly Type BaseType;
        public readonly BoundVar Var;  // can be null (if non-null, then object.ReferenceEquals(Var.Type, BaseType))
        public readonly Expression Constraint;  // is null iff Var is
        public readonly Microsoft.Dafny.SubsetTypeDecl.WKind WitnessKind = SubsetTypeDecl.WKind.CompiledZero;
        public readonly Expression/*?*/ Witness;  // non-null iff WitnessKind is Compiled or Ghost
        public NativeType NativeType; // non-null for fixed-size representations (otherwise, use BigIntegers for integers)
        public NewtypeDecl(IToken tok, string name, ModuleDefinition enclosingModuleDefinition, Type baseType, List<MemberDecl> members, Attributes attributes, bool isRefining)
          : base(tok, name, enclosingModuleDefinition, new List<TypeParameter>(), members, attributes, isRefining)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(enclosingModuleDefinition != null);
            Contract.Requires(baseType != null);
            Contract.Requires(members != null);
            BaseType = baseType;
        }
        public NewtypeDecl(IToken tok, string name, ModuleDefinition module, BoundVar bv, Expression constraint, Microsoft.Dafny.SubsetTypeDecl.WKind witnessKind, Expression witness, List<MemberDecl> members, Attributes attributes, bool isRefining)
          : base(tok, name, module, new List<TypeParameter>(), members, attributes, isRefining)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(module != null);
            Contract.Requires(bv != null && bv.Type != null);
            Contract.Requires((witnessKind == SubsetTypeDecl.WKind.Compiled || witnessKind == SubsetTypeDecl.WKind.Ghost) == (witness != null));
            Contract.Requires(members != null);
            BaseType = bv.Type;
            Var = bv;
            Constraint = constraint;
            Witness = witness;
            WitnessKind = witnessKind;
            this.NewSelfSynonym();
        }

        TopLevelDecl RevealableTypeDecl.AsTopLevelDecl { get { return this; } }
        public Microsoft.Dafny.TypeParameter.EqualitySupportValue EqualitySupport
        {
            get
            {
                if (this.BaseType.SupportsEquality)
                {
                    return TypeParameter.EqualitySupportValue.Required;
                }
                else
                {
                    return TypeParameter.EqualitySupportValue.Unspecified;
                }
            }
        }

        string RedirectingTypeDecl.Name { get { return Name; } }
        IToken RedirectingTypeDecl.tok { get { return Tok; } }
        Attributes RedirectingTypeDecl.Attributes { get { return Attributes; } }
        ModuleDefinition RedirectingTypeDecl.Module { get { return EnclosingModuleDefinition; } }
        BoundVar RedirectingTypeDecl.Var { get { return Var; } }
        Expression RedirectingTypeDecl.Constraint { get { return Constraint; } }
        Microsoft.Dafny.SubsetTypeDecl.WKind RedirectingTypeDecl.WitnessKind { get { return WitnessKind; } }
        Expression RedirectingTypeDecl.Witness { get { return Witness; } }
        FreshIdGenerator RedirectingTypeDecl.IdGenerator { get { return IdGenerator; } }

        bool ICodeContext.IsGhost
        {
            get { throw new NotSupportedException(); }  // if .IsGhost is needed, the object should always be wrapped in an CodeContextWrapper
        }
        List<TypeParameter> ICodeContext.TypeArgs { get { return new List<TypeParameter>(); } }
        List<Formal> ICodeContext.Ins { get { return new List<Formal>(); } }
        ModuleDefinition ICodeContext.EnclosingModule { get { return EnclosingModuleDefinition; } }
        bool ICodeContext.MustReverify { get { return false; } }
        bool ICodeContext.AllowsNontermination { get { return false; } }
        IToken ICallable.Tok { get { return Tok; } }
        string ICallable.NameRelativeToModule { get { return Name; } }
        Specification<Expression> ICallable.Decreases
        {
            get
            {
                // The resolver checks that a NewtypeDecl sits in its own SSC in the call graph.  Therefore,
                // the question of what its Decreases clause is should never arise.
                throw new cce.UnreachableException();
            }
        }
        bool ICallable.InferredDecreases
        {
            get { throw new cce.UnreachableException(); }  // see comment above about ICallable.Decreases
            set { throw new cce.UnreachableException(); }  // see comment above about ICallable.Decreases
        }
    }

    /// <summary>
    /// The "ValuetypeDecl" class models the built-in value types (like bool, int, set, and seq.
    /// Its primary function is to hold the formal type parameters and built-in members of these types.
    /// </summary>
    public class ValuetypeDecl : TopLevelDecl
    {
        public override string WhatKind { get { return Name; } }
        public readonly Dictionary<string, MemberDecl> Members = new Dictionary<string, MemberDecl>();
        public readonly Func<Type, bool> TypeTester;
        public readonly Func<List<Type>, Type>/*?*/ TypeCreator;

        public ValuetypeDecl(string name, ModuleDefinition enclosingModuleDefinition, int typeParameterCount, Func<Type, bool> typeTester, Func<List<Type>, Type>/*?*/ typeCreator)
          : base(Token.NoToken, name, enclosingModuleDefinition, new List<TypeParameter>(), null, false)
        {
            Contract.Requires(name != null);
            Contract.Requires(enclosingModuleDefinition != null);
            Contract.Requires(0 <= typeParameterCount);
            Contract.Requires(typeTester != null);
            // fill in the type parameters
            this.TypeParameterCount = typeParameterCount;
            for (int i = 0; i < typeParameterCount; i++)
            {
                TypeArgs.Add(new TypeParameter(Token.NoToken, ((char)('T' + i)).ToString(), i, this));
            }
            this.TypeTester = typeTester;
            this.TypeCreator = typeCreator;
        }

        public int TypeParameterCount { get; private set; }

        public bool IsThisType(Type t)
        {
            Contract.Assert(t != null);
            return TypeTester(t);
        }

        public Type CreateType(List<Type> typeArgs)
        {
            Contract.Requires(typeArgs != null);
            Contract.Requires(typeArgs.Count == TypeArgs.Count);
            Contract.Assume(TypeCreator != null);  // can only call CreateType for a ValuetypeDecl with a type creator (this is up to the caller to ensure)
            return TypeCreator(typeArgs);
        }
    }


    public class SubsetTypeDecl : TypeSynonymDecl, RedirectingTypeDecl
    {
        public override string WhatKind { get { return "subset type"; } }
        public readonly BoundVar BoundVar;
        public readonly Expression Constraint;
        public enum WKind { CompiledZero, Compiled, Ghost, OptOut, Special }
        public readonly Microsoft.Dafny.SubsetTypeDecl.WKind WitnessKind;
        public readonly Expression/*?*/ Witness;  // non-null iff WitnessKind is Compiled or Ghost
        public bool ConstraintIsCompilable; // Will be filled in by the Resolver
        public bool CheckedIfConstraintIsCompilable = false; // Set to true lazily by the Resolver when the Resolver fills in "ConstraintIsCompilable".
        public SubsetTypeDecl(IToken tok, string name, Microsoft.Dafny.TypeParameter.TypeParameterCharacteristics characteristics, List<TypeParameter> typeArgs, ModuleDefinition enclosingModuleDefinition,
          BoundVar boundVar, Expression constraint, WKind witnessKind, Expression witness,
          Attributes attributes)
          : base(tok, name, characteristics, typeArgs, enclosingModuleDefinition, boundVar.Type, attributes)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(typeArgs != null);
            Contract.Requires(enclosingModuleDefinition != null);
            Contract.Requires(boundVar != null && boundVar.Type != null);
            Contract.Requires(constraint != null);
            Contract.Requires((witnessKind == WKind.Compiled || witnessKind == WKind.Ghost) == (witness != null));
            BoundVar = boundVar;
            Constraint = constraint;
            Witness = witness;
            WitnessKind = witnessKind;
        }
        BoundVar RedirectingTypeDecl.Var { get { return BoundVar; } }
        Expression RedirectingTypeDecl.Constraint { get { return Constraint; } }
        WKind RedirectingTypeDecl.WitnessKind { get { return WitnessKind; } }
        Expression RedirectingTypeDecl.Witness { get { return Witness; } }

        public override List<Type> ParentTypes(List<Type> typeArgs)
        {
            return new List<Type> { RhsWithArgument(typeArgs) };
        }
    }

    public class TypeSynonymDecl : TypeSynonymDeclBase, RedirectingTypeDecl, RevealableTypeDecl
    {
        public TypeSynonymDecl(IToken tok, string name, Microsoft.Dafny.TypeParameter.TypeParameterCharacteristics characteristics, List<TypeParameter> typeArgs, ModuleDefinition enclosingModuleDefinition, Type rhs, Attributes attributes)
          : base(tok, name, characteristics, typeArgs, enclosingModuleDefinition, rhs, attributes)
        {
            this.NewSelfSynonym();
        }
        TopLevelDecl RevealableTypeDecl.AsTopLevelDecl { get { return this; } }
    }

    public abstract class TypeSynonymDeclBase : TopLevelDecl, RedirectingTypeDecl
    {
        public override string WhatKind { get { return "type synonym"; } }
        public Microsoft.Dafny.TypeParameter.TypeParameterCharacteristics Characteristics;  // the resolver may change the .EqualitySupport component of this value from Unspecified to InferredRequired (for some signatures that may immediately imply that equality support is required)
        public bool SupportsEquality
        {
            get { return Characteristics.EqualitySupport != TypeParameter.EqualitySupportValue.Unspecified; }
        }
        public readonly Type Rhs;
        public TypeSynonymDeclBase(IToken tok, string name, Microsoft.Dafny.TypeParameter.TypeParameterCharacteristics characteristics,
          List<TypeParameter> typeArgs, ModuleDefinition enclosingModuleDefinition, Type rhs, Attributes attributes)
          : base(tok, name, enclosingModuleDefinition, typeArgs, attributes, false)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(typeArgs != null);
            Contract.Requires(enclosingModuleDefinition != null);
            Contract.Requires(rhs != null);
            Characteristics = characteristics;
            Rhs = rhs;
        }
        /// <summary>
        /// Return .Rhs instantiated with "typeArgs", but only look at the part of .Rhs that is in scope.
        /// </summary>
        public Type RhsWithArgument(List<Type> typeArgs)
        {
            Contract.Requires(typeArgs != null);
            Contract.Requires(typeArgs.Count == TypeArgs.Count);
            var scope = Type.GetScope();
            var rtd = Rhs.AsRevealableType;
            if (rtd != null)
            {
                Contract.Assume(rtd.AsTopLevelDecl.IsVisibleInScope(scope));
                if (!rtd.IsRevealedInScope(scope))
                {
                    // type is actually hidden in this scope
                    return rtd.SelfSynonym(typeArgs);
                }
            }
            return RhsWithArgumentIgnoringScope(typeArgs);
        }
        /// <summary>
        /// Returns the declared .Rhs but with formal type arguments replaced by the given actuals.
        /// </summary>
        public Type RhsWithArgumentIgnoringScope(List<Type> typeArgs)
        {
            Contract.Requires(typeArgs != null);
            Contract.Requires(typeArgs.Count == TypeArgs.Count);
            // Instantiate with the actual type arguments
            if (typeArgs.Count == 0)
            {
                // this optimization seems worthwhile
                return Rhs;
            }
            else
            {
                var subst = Resolver.TypeSubstitutionMap(TypeArgs, typeArgs);
                return Resolver.SubstType(Rhs, subst);
            }
        }

        string RedirectingTypeDecl.Name { get { return Name; } }
        IToken RedirectingTypeDecl.tok { get { return Tok; } }
        Attributes RedirectingTypeDecl.Attributes { get { return Attributes; } }
        ModuleDefinition RedirectingTypeDecl.Module { get { return EnclosingModuleDefinition; } }
        BoundVar RedirectingTypeDecl.Var { get { return null; } }
        Expression RedirectingTypeDecl.Constraint { get { return null; } }
        Microsoft.Dafny.SubsetTypeDecl.WKind RedirectingTypeDecl.WitnessKind { get { return SubsetTypeDecl.WKind.CompiledZero; } }
        Expression RedirectingTypeDecl.Witness { get { return null; } }
        FreshIdGenerator RedirectingTypeDecl.IdGenerator { get { return IdGenerator; } }

        bool ICodeContext.IsGhost
        {
            get { throw new NotSupportedException(); }  // if .IsGhost is needed, the object should always be wrapped in an CodeContextWrapper
        }
        List<TypeParameter> ICodeContext.TypeArgs { get { return TypeArgs; } }
        List<Formal> ICodeContext.Ins { get { return new List<Formal>(); } }
        ModuleDefinition ICodeContext.EnclosingModule { get { return EnclosingModuleDefinition; } }
        bool ICodeContext.MustReverify { get { return false; } }
        bool ICodeContext.AllowsNontermination { get { return false; } }
        IToken ICallable.Tok { get { return Tok; } }
        string ICallable.NameRelativeToModule { get { return Name; } }
        Specification<Expression> ICallable.Decreases
        {
            get
            {
                // The resolver checks that a NewtypeDecl sits in its own SSC in the call graph.  Therefore,
                // the question of what its Decreases clause is should never arise.
                throw new cce.UnreachableException();
            }
        }
        bool ICallable.InferredDecreases
        {
            get { throw new cce.UnreachableException(); }  // see comment above about ICallable.Decreases
            set { throw new cce.UnreachableException(); }  // see comment above about ICallable.Decreases
        }
        public override bool CanBeRevealed()
        {
            return true;
        }
    }

    public class InternalTypeSynonymDecl : TypeSynonymDeclBase, RedirectingTypeDecl
    {
        public InternalTypeSynonymDecl(IToken tok, string name, Microsoft.Dafny.TypeParameter.TypeParameterCharacteristics characteristics,
          List<TypeParameter> typeArgs, ModuleDefinition enclosingModuleDefinition, Type rhs, Attributes attributes)
          : base(tok, name, characteristics, typeArgs, enclosingModuleDefinition, rhs, attributes)
        {
        }
    }

    public class ArrowType : UserDefinedType
    {
        public List<Type> Args
        {
            get { return TypeArgs.GetRange(0, Arity); }
        }

        public Type Result
        {
            get { return TypeArgs[Arity]; }
        }

        public int Arity
        {
            get { return TypeArgs.Count - 1; }
        }

        /// <summary>
        /// Constructs a(n unresolved) arrow type.
        /// </summary>
        public ArrowType(IToken tok, List<Type> args, Type result)
          : base(tok, ArrowTypeName(args.Count), Util.Snoc(args, result))
        {
            Contract.Requires(tok != null);
            Contract.Requires(args != null);
            Contract.Requires(result != null);
        }
        /// <summary>
        /// Constructs and returns a resolved arrow type.
        /// </summary>
        public ArrowType(IToken tok, ArrowTypeDecl atd, List<Type> typeArgsAndResult)
          : base(tok, ArrowTypeName(atd.Arity), atd, typeArgsAndResult)
        {
            Contract.Requires(tok != null);
            Contract.Requires(atd != null);
            Contract.Requires(typeArgsAndResult != null);
            Contract.Requires(typeArgsAndResult.Count == atd.Arity + 1);
        }
        /// <summary>
        /// Constructs and returns a resolved arrow type.
        /// </summary>
        public ArrowType(IToken tok, ArrowTypeDecl atd, List<Type> typeArgs, Type result)
          : this(tok, atd, Util.Snoc(typeArgs, result))
        {
            Contract.Requires(tok != null);
            Contract.Requires(atd != null);
            Contract.Requires(typeArgs != null);
            Contract.Requires(typeArgs.Count == atd.Arity);
            Contract.Requires(result != null);
        }

        public const string Arrow_FullCompileName = "Func";  // this is the same for all arities

        public static string ArrowTypeName(int arity)
        {
            return "_#Func" + arity;
        }

        [Pure]
        public static bool IsArrowTypeName(string s)
        {
            return s.StartsWith("_#Func");
        }

        public static string PartialArrowTypeName(int arity)
        {
            return "_#PartialFunc" + arity;
        }

        [Pure]
        public static bool IsPartialArrowTypeName(string s)
        {
            return s.StartsWith("_#PartialFunc");
        }

        public static string TotalArrowTypeName(int arity)
        {
            return "_#TotalFunc" + arity;
        }

        [Pure]
        public static bool IsTotalArrowTypeName(string s)
        {
            return s.StartsWith("_#TotalFunc");
        }

        public const string ANY_ARROW = "~>";
        public const string PARTIAL_ARROW = "-->";
        public const string TOTAL_ARROW = "->";

        public override string TypeName(ModuleDefinition context, bool parseAble)
        {
            return PrettyArrowTypeName(ANY_ARROW, Args, Result, context, parseAble);
        }

        /// <summary>
        /// Pretty prints an arrow type.  If "result" is null, then all arguments, including the result type are expected in "typeArgs".
        /// If "result" is non-null, then only the in-arguments are in "typeArgs".
        /// </summary>
        public static string PrettyArrowTypeName(string arrow, List<Type> typeArgs, Type result, ModuleDefinition context, bool parseAble)
        {
            Contract.Requires(arrow != null);
            Contract.Requires(typeArgs != null);
            Contract.Requires(result != null || 1 <= typeArgs.Count);

            int arity = result == null ? typeArgs.Count - 1 : typeArgs.Count;
            var domainNeedsParens = false;
            if (arity != 1)
            {
                // 0 or 2-or-more arguments:  need parentheses
                domainNeedsParens = true;
            }
            else if (typeArgs[0].IsBuiltinArrowType)
            {
                // arrows are right associative, so we need parentheses around the domain type
                domainNeedsParens = true;
            }
            else
            {
                // if the domain type consists of a single tuple type, then an extra set of parentheses is needed
                // Note, we do NOT call .AsDatatype or .AsIndDatatype here, because those calls will do a NormalizeExpand().  Instead, we do the check manually.
                var udt = typeArgs[0].Normalize() as UserDefinedType;  // note, we do Normalize(), not NormalizeExpand(), since the TypeName will use any synonym
                if (udt != null && ((udt.FullName != null && BuiltIns.IsTupleTypeName(udt.FullName)) || udt.ResolvedClass is TupleTypeDecl))
                {
                    domainNeedsParens = true;
                }
            }
            string s = "";
            if (domainNeedsParens) { s += "("; }
            s += Util.Comma(typeArgs.Take(arity), arg => arg.TypeName(context, parseAble));
            if (domainNeedsParens) { s += ")"; }
            s += " " + arrow + " ";
            s += (result ?? typeArgs.Last()).TypeName(context, parseAble);
            return s;
        }

        public override bool SupportsEquality
        {
            get
            {
                return false;
            }
        }
    }

    public class UserDefinedType : NonProxyType
    {
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(tok != null);
            Contract.Invariant(Name != null);
            Contract.Invariant(cce.NonNullElements(TypeArgs));
            Contract.Invariant(NamePath is NameSegment || NamePath is ExprDotName);
            Contract.Invariant(!ArrowType.IsArrowTypeName(Name) || this is ArrowType);
        }

        public readonly Expression NamePath;  // either NameSegment or ExprDotName (with the inner expression satisfying this same constraint)
        public readonly IToken tok;  // token of the Name
        public readonly string Name;
        [Rep]

        public string FullName
        {
            get
            {
                if (ResolvedClass?.EnclosingModuleDefinition?.IsDefaultModule == false)
                {
                    return ResolvedClass.EnclosingModuleDefinition.Name + "." + Name;
                }
                else
                {
                    return Name;
                }
            }
        }

        string compileName;
        public string CompileName
        {
            get
            {
                if (compileName == null)
                {
                    compileName = ResolvedClass.CompileName;
                }
                return compileName;
            }
        }
        public string FullCompanionCompileName
        {
            get
            {
                Contract.Requires(ResolvedClass is TraitDecl || (ResolvedClass is NonNullTypeDecl nntd && nntd.ClassDecl is TraitDecl));
                var m = ResolvedClass.EnclosingModuleDefinition;
                var s = m.IsDefaultModule ? "" : m.CompileName + ".";
                return s + "_Companion_" + ResolvedClass.CompileName;
            }
        }

        public TopLevelDecl ResolvedClass;  // filled in by resolution, if Name denotes a class/datatype/iterator and TypeArgs match the type parameters of that class/datatype/iterator

        public UserDefinedType(IToken tok, string name, List<Type> optTypeArgs)
          : this(tok, new NameSegment(tok, name, optTypeArgs))
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(optTypeArgs == null || optTypeArgs.Count > 0);  // this is what it means to be syntactically optional
        }

        public UserDefinedType(IToken tok, Expression namePath)
        {
            Contract.Requires(tok != null);
            Contract.Requires(namePath is NameSegment || namePath is ExprDotName);
            this.tok = tok;
            if (namePath is NameSegment)
            {
                var n = (NameSegment)namePath;
                this.Name = n.Name;
                this.TypeArgs = n.OptTypeArguments;
            }
            else
            {
                var n = (ExprDotName)namePath;
                this.Name = n.SuffixName;
                this.TypeArgs = n.OptTypeArguments;
            }
            if (this.TypeArgs == null)
            {
                this.TypeArgs = new List<Type>();  // TODO: is this really the thing to do?
            }
            this.NamePath = namePath;
        }

        /// <summary>
        /// Constructs a Type (in particular, a UserDefinedType) from a TopLevelDecl denoting a type declaration.  If
        /// the given declaration takes type parameters, these are filled as references to the formal type parameters
        /// themselves.  (Usually, this method is called when the type parameters in the result don't matter, other
        /// than that they need to be filled in, so as to make a properly resolved UserDefinedType.)
        /// If "typeArgs" is non-null, then its type parameters are used in constructing the returned type.
        /// If "typeArgs" is null, then the formal type parameters of "cd" are used.
        /// </summary>
        public static UserDefinedType FromTopLevelDecl(IToken tok, TopLevelDecl cd, List<TypeParameter> typeArgs = null)
        {
            Contract.Requires(tok != null);
            Contract.Requires(cd != null);
            Contract.Assert((cd is ArrowTypeDecl) == ArrowType.IsArrowTypeName(cd.Name));
            var args = (typeArgs ?? cd.TypeArgs).ConvertAll(tp => (Type)new UserDefinedType(tp));
            if (cd is ArrowTypeDecl)
            {
                return new ArrowType(tok, (ArrowTypeDecl)cd, args);
            }
            else if (cd is ClassDecl && !(cd is DefaultClassDecl))
            {
                return new UserDefinedType(tok, cd.Name + "?", cd, args);
            }
            else
            {
                return new UserDefinedType(tok, cd.Name, cd, args);
            }
        }

        public static UserDefinedType FromTopLevelDeclWithAllBooleanTypeParameters(TopLevelDecl cd)
        {
            Contract.Requires(cd != null);
            Contract.Requires(!(cd is ArrowTypeDecl));

            var typeArgs = cd.TypeArgs.ConvertAll(tp => (Type)Type.Bool);
            return new UserDefinedType(cd.Tok, cd.Name, cd, typeArgs);
        }

        /// <summary>
        /// If "member" is non-null, then:
        ///   Return the upcast of "receiverType" that has base type "member.EnclosingClass".
        ///   Assumes that "receiverType" normalizes to a UserDefinedFunction with a .ResolveClass that is a subtype
        ///   of "member.EnclosingClass".
        /// Otherwise:
        ///   Return "receiverType" (expanded).
        /// </summary>
        public static Type UpcastToMemberEnclosingType(Type receiverType, MemberDecl/*?*/ member)
        {
            Contract.Requires(receiverType != null);
            if (member != null && member.EnclosingClass != null && !(member.EnclosingClass is ValuetypeDecl))
            {
                return receiverType.AsParentType(member.EnclosingClass);
            }
            return receiverType.NormalizeExpandKeepConstraints();
        }

        /// <summary>
        /// This constructor constructs a resolved class/datatype/iterator/subset-type/newtype type
        /// </summary>
        public UserDefinedType(IToken tok, string name, TopLevelDecl cd, [Captured] List<Type> typeArgs, Expression/*?*/ namePath = null)
        {
            Contract.Requires(tok != null);
            Contract.Requires(name != null);
            Contract.Requires(cd != null);
            Contract.Requires(cce.NonNullElements(typeArgs));
            Contract.Requires(cd.TypeArgs.Count == typeArgs.Count);
            Contract.Requires(namePath == null || namePath is NameSegment || namePath is ExprDotName);
            // The following is almost a precondition. In a few places, the source program names a class, not a type,
            // and in then name==cd.Name for a ClassDecl.
            //Contract.Requires(!(cd is ClassDecl) || cd is DefaultClassDecl || cd is ArrowTypeDecl || name == cd.Name + "?");
            Contract.Requires(!(cd is ArrowTypeDecl) || name == cd.Name);
            Contract.Requires(!(cd is DefaultClassDecl) || name == cd.Name);
            this.tok = tok;
            this.Name = name;
            this.ResolvedClass = cd;
            this.TypeArgs = typeArgs;
            if (namePath == null)
            {
                var ns = new NameSegment(tok, name, typeArgs.Count == 0 ? null : typeArgs);
                var r = new Resolver_IdentifierExpr(tok, cd, typeArgs);
                ns.ResolvedExpression = r;
                ns.Type = r.Type;
                this.NamePath = ns;
            }
            else
            {
                this.NamePath = namePath;
            }
        }

        public static UserDefinedType CreateNonNullType(UserDefinedType udtNullableType)
        {
            Contract.Requires(udtNullableType != null);
            Contract.Requires(udtNullableType.ResolvedClass is ClassDecl);
            var cl = (ClassDecl)udtNullableType.ResolvedClass;
            return new UserDefinedType(udtNullableType.tok, cl.NonNullTypeDecl.Name, cl.NonNullTypeDecl, udtNullableType.TypeArgs);
        }

        public static UserDefinedType CreateNullableType(UserDefinedType udtNonNullType)
        {
            Contract.Requires(udtNonNullType != null);
            Contract.Requires(udtNonNullType.ResolvedClass is NonNullTypeDecl);
            var nntd = (NonNullTypeDecl)udtNonNullType.ResolvedClass;
            return new UserDefinedType(udtNonNullType.tok, nntd.ClassDecl.Name + "?", nntd.ClassDecl, udtNonNullType.TypeArgs);
        }

        /// <summary>
        /// This constructor constructs a resolved type parameter
        /// </summary>
        public UserDefinedType(TypeParameter tp)
          : this(tp.Tok, tp)
        {
            Contract.Requires(tp != null);
        }

        /// <summary>
        /// This constructor constructs a resolved type parameter (but shouldn't be called if "tp" denotes
        /// the .TheType of an opaque type -- use the (OpaqueType_AsParameter, OpaqueTypeDecl, List(Type))
        /// constructor for that).
        /// </summary>
        public UserDefinedType(IToken tok, TypeParameter tp)
        {
            Contract.Requires(tok != null);
            Contract.Requires(tp != null);
            this.tok = tok;
            this.Name = tp.Name;
            this.TypeArgs = new List<Type>();
            this.ResolvedClass = tp;
            var ns = new NameSegment(tok, tp.Name, null);
            var r = new Resolver_IdentifierExpr(tok, tp);
            ns.ResolvedExpression = r;
            ns.Type = r.Type;
            this.NamePath = ns;
        }

        public override bool Equals(Type that, bool keepConstraints = false)
        {
            var i = NormalizeExpand(keepConstraints);
            if (i is UserDefinedType)
            {
                var ii = (UserDefinedType)i;
                var t = that.NormalizeExpand(keepConstraints) as UserDefinedType;
                if (t == null || ii.ResolvedClass != t.ResolvedClass || ii.TypeArgs.Count != t.TypeArgs.Count)
                {
                    return false;
                }
                else
                {
                    for (int j = 0; j < ii.TypeArgs.Count; j++)
                    {
                        if (!ii.TypeArgs[j].Equals(t.TypeArgs[j], keepConstraints))
                        {
                            return false;
                        }
                    }
                    return true;
                }
            }
            else
            {
                // TODO?: return i.Equals(that.NormalizeExpand());
                return i.Equals(that, keepConstraints);
            }
        }

        /// <summary>
        /// If type denotes a resolved class type, then return that class type.
        /// Otherwise, return null.
        /// </summary>
        public static UserDefinedType DenotesClass(Type type)
        {
            Contract.Requires(type != null);
            Contract.Ensures(Contract.Result<UserDefinedType>() == null || Contract.Result<UserDefinedType>().ResolvedClass is ClassDecl);
            type = type.NormalizeExpand();
            UserDefinedType ct = type as UserDefinedType;
            if (ct != null && ct.ResolvedClass is ClassDecl)
            {
                return ct;
            }
            else
            {
                return null;
            }
        }

        public static Type ArrayElementType(Type type)
        {
            Contract.Requires(type != null);
            Contract.Requires(type.IsArrayType);
            Contract.Ensures(Contract.Result<Type>() != null);

            UserDefinedType udt = DenotesClass(type);
            Contract.Assert(udt != null);
            Contract.Assert(udt.TypeArgs.Count == 1);  // holds true of all array types
            return udt.TypeArgs[0];
        }

        [Pure]
        public override string TypeName(ModuleDefinition context, bool parseAble)
        {
            Contract.Ensures(Contract.Result<string>() != null);
            if (BuiltIns.IsTupleTypeName(Name))
            {
                // Unfortunately, ResolveClass may be null, so Name is all we have.  Reverse-engineer the string name.
                IEnumerable<bool> argumentGhostness = BuiltIns.ArgumentGhostnessFromString(Name, TypeArgs.Count);
                return "(" + Util.Comma(System.Linq.Enumerable.Zip(TypeArgs, argumentGhostness),
                  (ty_u) => Resolver.GhostPrefix(ty_u.Item2) + ty_u.Item1.TypeName(context, parseAble)) + ")";
            }
            else if (ArrowType.IsPartialArrowTypeName(Name))
            {
                return ArrowType.PrettyArrowTypeName(ArrowType.PARTIAL_ARROW, TypeArgs, null, context, parseAble);
            }
            else if (ArrowType.IsTotalArrowTypeName(Name))
            {
                return ArrowType.PrettyArrowTypeName(ArrowType.TOTAL_ARROW, TypeArgs, null, context, parseAble);
            }
            else
            {
#if TEST_TYPE_SYNONYM_TRANSPARENCY
        if (Name == "type#synonym#transparency#test" && ResolvedClass is TypeSynonymDecl) {
          return ((TypeSynonymDecl)ResolvedClass).Rhs.TypeName(context);
        }
#endif
                var s = Printer.ExprToString(NamePath);
                if (ResolvedClass != null)
                {
                    var optionalTypeArgs = NamePath is NameSegment ? ((NameSegment)NamePath).OptTypeArguments : ((ExprDotName)NamePath).OptTypeArguments;
                    if (optionalTypeArgs == null && TypeArgs != null && TypeArgs.Count != 0)
                    {
                        s += this.TypeArgsToString(context, parseAble);
                    }
                }
                return s;
            }
        }

        public override bool SupportsEquality
        {
            get
            {
                if (ResolvedClass is ClassDecl || ResolvedClass is NewtypeDecl)
                {
                    return ResolvedClass.IsRevealedInScope(Type.GetScope());
                }
                else if (ResolvedClass is CoDatatypeDecl)
                {
                    return false;
                }
                else if (ResolvedClass is IndDatatypeDecl)
                {
                    var dt = (IndDatatypeDecl)ResolvedClass;
                    Contract.Assume(dt.EqualitySupport != IndDatatypeDecl.ES.NotYetComputed);
                    if (!dt.IsRevealedInScope(Type.GetScope()))
                    {
                        return false;
                    }
                    if (dt.EqualitySupport == IndDatatypeDecl.ES.Never)
                    {
                        return false;
                    }
                    Contract.Assert(dt.TypeArgs.Count == TypeArgs.Count);
                    var i = 0;
                    foreach (var tp in dt.TypeArgs)
                    {
                        if (tp.NecessaryForEqualitySupportOfSurroundingInductiveDatatype && !TypeArgs[i].SupportsEquality)
                        {
                            return false;
                        }
                        i++;
                    }
                    return true;
                }
                else if (ResolvedClass is TypeSynonymDeclBase)
                {
                    var t = (TypeSynonymDeclBase)ResolvedClass;
                    if (t.SupportsEquality)
                    {
                        return true;
                    }
                    else if (t.IsRevealedInScope(Type.GetScope()))
                    {
                        return t.RhsWithArgument(TypeArgs).SupportsEquality;
                    }
                    else
                    {
                        return false;
                    }
                }
                else if (ResolvedClass is TypeParameter)
                {
                    return ((TypeParameter)ResolvedClass).SupportsEquality;
                }
                else if (ResolvedClass is OpaqueTypeDecl)
                {
                    return ((OpaqueTypeDecl)ResolvedClass).SupportsEquality;
                }
                Contract.Assume(false);  // the SupportsEquality getter requires the Type to have been successfully resolved
                return true;
            }
        }

        public override bool MayInvolveReferences
        {
            get
            {
                if (ResolvedClass is ClassDecl)
                {
                    return true;
                }
                else if (ResolvedClass is NewtypeDecl)
                {
                    return false;
                }
                else if (ResolvedClass is DatatypeDecl)
                {
                    var dt = (DatatypeDecl)ResolvedClass;
                    if (!dt.IsRevealedInScope(Type.GetScope()))
                    {
                        return true;
                    }
                    Contract.Assert(dt.TypeArgs.Count == TypeArgs.Count);
                    return TypeArgs.TrueForAll(ta => ta.MayInvolveReferences);
                }
                else if (ResolvedClass is TypeSynonymDeclBase)
                {
                    var t = (TypeSynonymDeclBase)ResolvedClass;
                    // (Note, if type parameters/opaque types could have a may-involve-references characteristic, then it would be consulted here)
                    if (t.IsRevealedInScope(Type.GetScope()))
                    {
                        return t.RhsWithArgument(TypeArgs).MayInvolveReferences;
                    }
                    else
                    {
                        return true;
                    }
                }
                else if (ResolvedClass is TypeParameter || ResolvedClass is OpaqueTypeDecl)
                {
                    // (Note, if type parameters/opaque types could have a may-involve-references characteristic, then it would be consulted here)
                    return true;
                }
                Contract.Assume(false);  // the MayInvolveReferences getter requires the Type to have been successfully resolved
                return true;
            }
        }

        public override List<Type> ParentTypes()
        {
            return ResolvedClass != null ? ResolvedClass.ParentTypes(TypeArgs) : base.ParentTypes();
        }

        public override bool IsSubtypeOf(Type super, bool ignoreTypeArguments, bool ignoreNullity)
        {
            super = super.NormalizeExpandKeepConstraints();

            // Specifically handle object as the implicit supertype of classes and traits.
            // "object?" is handled by Builtins rather than the Type hierarchy, so unfortunately
            // it can't be returned in ParentTypes().
            if (super.IsObjectQ)
            {
                return IsRefType;
            }
            else if (super.IsObject)
            {
                return ignoreNullity ? IsRefType : IsNonNullRefType;
            }

            return base.IsSubtypeOf(super, ignoreTypeArguments, ignoreNullity);
        }
    }

    /// <summary>
    /// This proxy stands for any type, but it originates from an instantiated type parameter.
    /// </summary>
    public class ParamTypeProxy : TypeProxy
    {
        public TypeParameter orig;
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(orig != null);
        }

        public ParamTypeProxy(TypeParameter orig)
        {
            Contract.Requires(orig != null);
            this.orig = orig;
        }
    }

    public class DatatypeValue : Expression
    {
        public readonly string DatatypeName;
        public readonly string MemberName;
        public readonly ActualBindings Bindings;
        public List<Expression> Arguments => Bindings.Arguments;
        public DatatypeCtor Ctor;  // filled in by resolution
        public List<Type> InferredTypeArgs = new List<Type>();  // filled in by resolution
        public bool IsCoCall;  // filled in by resolution
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(DatatypeName != null);
            Contract.Invariant(MemberName != null);
            Contract.Invariant(cce.NonNullElements(Arguments));
            Contract.Invariant(cce.NonNullElements(InferredTypeArgs));
            Contract.Invariant(Ctor == null || InferredTypeArgs.Count == Ctor.EnclosingDatatype.TypeArgs.Count);
        }

        public DatatypeValue(IToken tok, string datatypeName, string memberName, [Captured] List<ActualBinding> arguments)
          : base(tok)
        {
            Contract.Requires(cce.NonNullElements(arguments));
            Contract.Requires(tok != null);
            Contract.Requires(datatypeName != null);
            Contract.Requires(memberName != null);
            this.DatatypeName = datatypeName;
            this.MemberName = memberName;
            this.Bindings = new ActualBindings(arguments);
        }

        /// <summary>
        /// This constructor is intended to be used when constructing a resolved DatatypeValue. The "args" are expected
        /// to be already resolved, and are all given positionally.
        /// </summary>
        public DatatypeValue(IToken tok, string datatypeName, string memberName, List<Expression> arguments)
          : this(tok, datatypeName, memberName, arguments.ConvertAll(e => new ActualBinding(null, e)))
        {
            Bindings.AcceptArgumentExpressionsAsExactParameterList();
        }

        public override IEnumerable<Expression> SubExpressions
        {
            get { return Arguments; }
        }
    }

    /// <summary>
    /// This class is used only inside the resolver itself. It gets hung in the AST in uncompleted name segments.
    /// </summary>
    class Resolver_IdentifierExpr : Expression
    {
        public readonly TopLevelDecl Decl;
        public readonly List<Type> TypeArgs;
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(Decl != null);
            Contract.Invariant(TypeArgs != null);
            Contract.Invariant(TypeArgs.Count == Decl.TypeArgs.Count);
            Contract.Invariant(Type is ResolverType_Module || Type is ResolverType_Type);
        }

        public abstract class ResolverType : Type
        {
        }
        public class ResolverType_Module : ResolverType
        {
            [Pure]
            public override string TypeName(ModuleDefinition context, bool parseAble)
            {
                Contract.Assert(parseAble == false);
                return "#module";
            }
            public override bool Equals(Type that, bool keepConstraints = false)
            {
                return that.NormalizeExpand(keepConstraints) is ResolverType_Module;
            }
        }
        public class ResolverType_Type : ResolverType
        {
            [Pure]
            public override string TypeName(ModuleDefinition context, bool parseAble)
            {
                Contract.Assert(parseAble == false);
                return "#type";
            }
            public override bool Equals(Type that, bool keepConstraints = false)
            {
                return that.NormalizeExpand(keepConstraints) is ResolverType_Type;
            }
        }

        public Resolver_IdentifierExpr(IToken tok, object decl, List<Type> typeArgs)
          : base(tok)
        {
            Contract.Requires(tok != null);
            Contract.Requires(decl != null);
            Decl = (TopLevelDecl)decl;
            TypeArgs = typeArgs;
            Type = decl is ModuleDecl ? (Type)new ResolverType_Module() : new ResolverType_Type();
        }
        public Resolver_IdentifierExpr(IToken tok, TypeParameter tp)
          : this(tok, tp, new List<Type>())
        {
            Contract.Requires(tok != null);
            Contract.Requires(tp != null);
        }
    }

    public class UnchangedExpr : Expression
    {
        public readonly List<FrameExpression> Frame;
        public readonly string/*?*/ At;
        public Label/*?*/ AtLabel;  // filled in during resolution; after that, At==null iff AtLabel==null
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(Frame != null);
        }

        public UnchangedExpr(IToken tok, List<FrameExpression> frame, string/*?*/ at)
          : base(tok)
        {
            Contract.Requires(tok != null);
            Contract.Requires(frame != null);
            this.Frame = frame;
            this.At = at;
        }

        public override IEnumerable<Expression> SubExpressions
        {
            get
            {
                foreach (var fe in Frame)
                {
                    yield return fe.E;
                }
            }
        }
    }

    public class LetExpr : Expression, IAttributeBearingDeclaration, IBoundVarsBearingExpression
    {
        public readonly List<CasePattern<BoundVar>> LHSs;
        public readonly List<Expression> RHSs;
        public readonly Expression Body;
        public readonly bool Exact;  // Exact==true means a regular let expression; Exact==false means an assign-such-that expression
        public readonly Attributes Attributes;
        public List<Microsoft.Dafny.ComprehensionExpr.BoundedPool> Constraint_Bounds;  // initialized and filled in by resolver; null for Exact=true and for when expression is in a ghost context
                                                                                       // invariant Constraint_Bounds == null || Constraint_Bounds.Count == BoundVars.Count;
        private Expression translationDesugaring;  // filled in during translation, lazily; to be accessed only via Translation.LetDesugaring; always null when Exact==true
        private Translator lastTranslatorUsed; // avoid clashing desugaring between translators

        public IToken BodyStartTok = Token.NoToken;
        public IToken BodyEndTok = Token.NoToken;
        IToken IRegion.BodyStartTok { get { return BodyStartTok; } }
        IToken IRegion.BodyEndTok { get { return BodyEndTok; } }

        public void setTranslationDesugaring(Translator trans, Expression expr)
        {
            lastTranslatorUsed = trans;
            translationDesugaring = expr;
        }

        public Expression getTranslationDesugaring(Translator trans)
        {
            if (lastTranslatorUsed == trans)
            {
                return translationDesugaring;
            }
            else
            {
                return null;
            }
        }

        public LetExpr(IToken tok, List<CasePattern<BoundVar>> lhss, List<Expression> rhss, Expression body, bool exact, Attributes attributes = null)
          : base(tok)
        {
            LHSs = lhss;
            RHSs = rhss;
            Body = body;
            Exact = exact;
            Attributes = attributes;
        }
        public override IEnumerable<Expression> SubExpressions
        {
            get
            {
                foreach (var e in Attributes.SubExpressions(Attributes))
                {
                    yield return e;
                }
                foreach (var rhs in RHSs)
                {
                    yield return rhs;
                }
                yield return Body;
            }
        }

        public override IEnumerable<Type> ComponentTypes => BoundVars.Select(bv => bv.Type);

        public IEnumerable<BoundVar> BoundVars
        {
            get
            {
                foreach (var lhs in LHSs)
                {
                    foreach (var bv in lhs.Vars)
                    {
                        yield return bv;
                    }
                }
            }
        }

        public IEnumerable<BoundVar> AllBoundVars => BoundVars;
    }

    public class ForallExpr : QuantifierExpr
    {
        public override string WhatKind => "forall expression";
        protected override Microsoft.Dafny.BinaryExpr.ResolvedOpcode SplitResolvedOp { get { return BinaryExpr.ResolvedOpcode.And; } }

        public ForallExpr(IToken tok, IToken endTok, List<BoundVar> bvars, Expression range, Expression term, Attributes attributes)
          : this(tok, endTok, new List<TypeParameter>(), bvars, range, term, attributes)
        {
            Contract.Requires(cce.NonNullElements(bvars));
            Contract.Requires(tok != null);
            Contract.Requires(term != null);
        }
        public ForallExpr(IToken tok, IToken endTok, List<TypeParameter> tvars, List<BoundVar> bvars, Expression range, Expression term, Attributes attributes)
          : base(tok, endTok, tvars, bvars, range, term, attributes)
        {
            Contract.Requires(cce.NonNullElements(bvars));
            Contract.Requires(tok != null);
            Contract.Requires(term != null);
        }
        public override Expression LogicalBody(bool bypassSplitQuantifier = false)
        {
            if (Range == null)
            {
                return Term;
            }
            var body = new BinaryExpr(Term.Tok, BinaryExpr.Opcode.Imp, Range, Term);
            body.ResolvedOp = BinaryExpr.ResolvedOpcode.Imp;
            body.Type = Term.Type;
            return body;
        }
    }

    public abstract class QuantifierExpr : ComprehensionExpr, Microsoft.Dafny.TypeParameter.ParentType
    {
        public override string WhatKind => "quantifier";

        private readonly int UniqueId;
        public List<TypeParameter> TypeArgs;
        private static int currentQuantId = -1;

        protected virtual Microsoft.Dafny.BinaryExpr.ResolvedOpcode SplitResolvedOp { get { return BinaryExpr.ResolvedOpcode.Or; } }

        private Expression SplitQuantifierToExpression()
        {
            Contract.Requires(SplitQuantifier != null && SplitQuantifier.Any());
            Expression accumulator = SplitQuantifier[0];
            for (int tid = 1; tid < SplitQuantifier.Count; tid++)
            {
                accumulator = new BinaryExpr(Term.Tok, SplitResolvedOp, accumulator, SplitQuantifier[tid]);
            }
            return accumulator;
        }

        private List<Expression> _SplitQuantifier;
        public List<Expression> SplitQuantifier
        {
            get
            {
                return _SplitQuantifier;
            }
            set
            {
                Contract.Assert(!value.Contains(this)); // don't let it put into its own split quantifiers.
                _SplitQuantifier = value;
                SplitQuantifierExpression = SplitQuantifierToExpression();
            }
        }

        internal Expression SplitQuantifierExpression { get; private set; }

        static int FreshQuantId()
        {
            return System.Threading.Interlocked.Increment(ref currentQuantId);
        }

        public string FullName
        {
            get
            {
                return "q$" + UniqueId;
            }
        }

        public String Refresh(string prefix, FreshIdGenerator idGen)
        {
            return idGen.FreshId(prefix);
        }

        public TypeParameter Refresh(TypeParameter p, FreshIdGenerator idGen)
        {
            var cp = new TypeParameter(p.Tok, idGen.FreshId(p.Name + "#"), p.VarianceSyntax, p.Characteristics);
            cp.Parent = this;
            return cp;
        }
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            var _scratch = true;
            Contract.Invariant(Attributes.ContainsBool(Attributes, "typeQuantifier", ref _scratch) || TypeArgs.Count == 0);
        }
        public QuantifierExpr(IToken tok, IToken endTok, List<TypeParameter> tvars, List<BoundVar> bvars, Expression range, Expression term, Attributes attributes)
          : base(tok, endTok, bvars, range, term, attributes)
        {
            Contract.Requires(tok != null);
            Contract.Requires(cce.NonNullElements(bvars));
            Contract.Requires(term != null);
            this.TypeArgs = tvars;
            this.UniqueId = FreshQuantId();
        }

        public virtual Expression LogicalBody(bool bypassSplitQuantifier = false)
        {
            // Don't call this on a quantifier with a Split clause: it's not a real quantifier. The only exception is the Compiler.
            Contract.Requires(bypassSplitQuantifier || SplitQuantifier == null);
            throw new cce.UnreachableException(); // This body is just here for the "Requires" clause
        }

        public override IEnumerable<Expression> SubExpressions
        {
            get
            {
                if (SplitQuantifier == null)
                {
                    foreach (var e in base.SubExpressions)
                    {
                        yield return e;
                    }
                }
                else
                {
                    foreach (var e in Attributes.SubExpressions(Attributes))
                    {
                        yield return e;
                    }
                    foreach (var e in SplitQuantifier)
                    {
                        yield return e;
                    }
                }
            }
        }
    }

    public class ExistsExpr : QuantifierExpr
    {
        public override string WhatKind => "exists expression";
        protected override Microsoft.Dafny.BinaryExpr.ResolvedOpcode SplitResolvedOp { get { return BinaryExpr.ResolvedOpcode.Or; } }

        public ExistsExpr(IToken tok, IToken endTok, List<BoundVar> bvars, Expression range, Expression term, Attributes attributes)
          : this(tok, endTok, new List<TypeParameter>(), bvars, range, term, attributes)
        {
            Contract.Requires(cce.NonNullElements(bvars));
            Contract.Requires(tok != null);
            Contract.Requires(term != null);
        }
        public ExistsExpr(IToken tok, IToken endTok, List<TypeParameter> tvars, List<BoundVar> bvars, Expression range, Expression term, Attributes attributes)
          : base(tok, endTok, tvars, bvars, range, term, attributes)
        {
            Contract.Requires(cce.NonNullElements(bvars));
            Contract.Requires(tok != null);
            Contract.Requires(term != null);
        }
        public override Expression LogicalBody(bool bypassSplitQuantifier = false)
        {
            if (Range == null)
            {
                return Term;
            }
            var body = new BinaryExpr(Term.Tok, BinaryExpr.Opcode.And, Range, Term);
            body.ResolvedOp = BinaryExpr.ResolvedOpcode.And;
            body.Type = Term.Type;
            return body;
        }
    }

    public class LambdaExpr : ComprehensionExpr
    {
        public override string WhatKind => "lambda";

        public readonly List<FrameExpression> Reads;

        public LambdaExpr(IToken tok, IToken endTok, List<BoundVar> bvars, Expression requires, List<FrameExpression> reads, Expression body)
          : base(tok, endTok, bvars, requires, body, null)
        {
            Contract.Requires(reads != null);
            Reads = reads;
        }

        // Synonym
        public Expression Body
        {
            get
            {
                return Term;
            }
        }

        public override IEnumerable<Expression> SubExpressions
        {
            get
            {
                yield return Term;
                if (Range != null)
                {
                    yield return Range;
                }
                foreach (var read in Reads)
                {
                    yield return read.E;
                }
            }
        }

    }

    public class MatchExpr : Expression
    {  // a MatchExpr is an "extended expression" and is only allowed in certain places
        private Expression source;
        private List<MatchCaseExpr> cases;
        public readonly MatchingContext Context;
        public readonly List<DatatypeCtor> MissingCases = new List<DatatypeCtor>();  // filled in during resolution
        public readonly bool UsesOptionalBraces;
        public MatchExpr OrigUnresolved;  // the resolver makes this clone of the MatchExpr before it starts desugaring it

        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(Source != null);
            Contract.Invariant(cce.NonNullElements(Cases));
            Contract.Invariant(cce.NonNullElements(MissingCases));
        }

        public MatchExpr(IToken tok, Expression source, [Captured] List<MatchCaseExpr> cases, bool usesOptionalBraces, MatchingContext context = null)
          : base(tok)
        {
            Contract.Requires(tok != null);
            Contract.Requires(source != null);
            Contract.Requires(cce.NonNullElements(cases));
            this.source = source;
            this.cases = cases;
            this.UsesOptionalBraces = usesOptionalBraces;
            this.Context = context is null ? new HoleCtx() : context;
        }

        public Expression Source
        {
            get { return source; }
        }

        public List<MatchCaseExpr> Cases
        {
            get { return cases; }
        }

        // should only be used in desugar in resolve to change the source and cases of the matchexpr
        public void UpdateSource(Expression source)
        {
            this.source = source;
        }

        public void UpdateCases(List<MatchCaseExpr> cases)
        {
            this.cases = cases;
        }

        public override IEnumerable<Expression> SubExpressions
        {
            get
            {
                yield return Source;
                foreach (var mc in cases)
                {
                    yield return mc.Body;
                }
            }
        }

        public override IEnumerable<Type> ComponentTypes
        {
            get
            {
                foreach (var mc in cases)
                {
                    foreach (var bv in mc.Arguments)
                    {
                        yield return bv.Type;
                    }
                }
            }
        }
    }

    public class LetOrFailExpr : ConcreteSyntaxExpression
    {
        public readonly CasePattern<BoundVar>/*?*/ Lhs; // null means void-error handling: ":- E; F", non-null means "var pat :- E; F"
        public readonly Expression Rhs;
        public readonly Expression Body;

        public LetOrFailExpr(IToken tok, CasePattern<BoundVar>/*?*/ lhs, Expression rhs, Expression body) : base(tok)
        {
            Lhs = lhs;
            Rhs = rhs;
            Body = body;
        }
    }

    public class DatatypeUpdateExpr : ConcreteSyntaxExpression
    {
        public readonly Expression Root;
        public readonly List<Tuple<IToken, string, Expression>> Updates;
        public List<DatatypeCtor> LegalSourceConstructors;  // filled in by resolution
        public DatatypeUpdateExpr(IToken tok, Expression root, List<Tuple<IToken, string, Expression>> updates)
          : base(tok)
        {
            Contract.Requires(tok != null);
            Contract.Requires(root != null);
            Contract.Requires(updates != null);
            Contract.Requires(updates.Count != 0);
            Root = root;
            Updates = updates;
        }

        public override IEnumerable<Expression> SubExpressions
        {
            get
            {
                if (ResolvedExpression == null)
                {
                    yield return Root;
                    foreach (var update in Updates)
                    {
                        yield return update.Item3;
                    }
                }
                else
                {
                    foreach (var e in base.SubExpressions)
                    {
                        yield return e;
                    }
                }
            }
        }
    }

    /// <summary>
    /// When an actual parameter is omitted for a formal with a default value, the positional resolved
    /// version of the actual parameter will have a DefaultValueExpression value. This has three
    /// advantages:
    /// * It allows the entire module to be resolved before any substitutions take place.
    /// * It gives a good place to check for default-value expressions that would give rise to an
    ///   infinite expansion.
    /// * It preserves the pre-substitution form, which gives compilers a chance to avoid re-evaluation
    ///   of actual parameters used in other default-valued expressions.
    ///
    /// Note. Since DefaultValueExpression is a wrapper around another expression and can in several
    /// places be expanded according to its ResolvedExpression, it is convenient to make DefaultValueExpression
    /// inherit from ConcreteSyntaxExpression. However, there are some places in the code where
    /// one then needs to pay attention to DefaultValueExpression's. Such places would be more
    /// conspicuous if DefaultValueExpression were not an Expression at all. At the time of this
    /// writing, a change to a separate type has shown to be more hassle than the need for special
    /// attention to DefaultValueExpression's in some places.
    /// </summary>
    public class DefaultValueExpression : ConcreteSyntaxExpression
    {
        public readonly Formal Formal;
        public readonly Expression Receiver;
        public readonly Dictionary<IVariable, Expression> SubstMap;
        public readonly Dictionary<TypeParameter, Type> TypeMap;

        public DefaultValueExpression(IToken tok, Formal formal,
          Expression/*?*/ receiver, Dictionary<IVariable, Expression> substMap, Dictionary<TypeParameter, Type> typeMap)
          : base(tok)
        {
            Contract.Requires(tok != null);
            Contract.Requires(formal != null);
            Contract.Requires(formal.DefaultValue != null);
            Contract.Requires(substMap != null);
            Contract.Requires(typeMap != null);
            Formal = formal;
            Receiver = receiver;
            SubstMap = substMap;
            TypeMap = typeMap;
            Type = Resolver.SubstType(formal.Type, typeMap);
        }
    }

    public class DefaultModuleDecl : ModuleDefinition
    {
        public DefaultModuleDecl()
          : base(Token.NoToken, "_module", new List<IToken>(), false, false, null, null, null, true, true, true)
        {
        }
        public override bool IsDefaultModule
        {
            get
            {
                return true;
            }
        }
    }

    public class BuiltIns
    {
        public readonly ModuleDefinition SystemModule = new ModuleDefinition(Token.NoToken, "_System", new List<IToken>(), false, false, null, null, null, true, true, true);
        readonly Dictionary<int, ClassDecl> arrayTypeDecls = new Dictionary<int, ClassDecl>();
        public readonly Dictionary<int, ArrowTypeDecl> ArrowTypeDecls = new Dictionary<int, ArrowTypeDecl>();
        public readonly Dictionary<int, SubsetTypeDecl> PartialArrowTypeDecls = new Dictionary<int, SubsetTypeDecl>();  // same keys as arrowTypeDecl
        public readonly Dictionary<int, SubsetTypeDecl> TotalArrowTypeDecls = new Dictionary<int, SubsetTypeDecl>();  // same keys as arrowTypeDecl
        readonly Dictionary<List<bool>, TupleTypeDecl> tupleTypeDecls = new Dictionary<List<bool>, TupleTypeDecl>(new Dafny.IEnumerableComparer<bool>());
        public readonly ISet<int> Bitwidths = new HashSet<int>();
        public SpecialField ORDINAL_Offset;  // filled in by the resolver, used by the translator

        public readonly SubsetTypeDecl NatDecl;
        public UserDefinedType Nat() { return new UserDefinedType(Token.NoToken, "nat", NatDecl, new List<Type>()); }
        public readonly TraitDecl ObjectDecl;
        public UserDefinedType ObjectQ()
        {
            Contract.Assume(ObjectDecl != null);
            return new UserDefinedType(Token.NoToken, "object?", null) { ResolvedClass = ObjectDecl };
        }

        public BuiltIns()
        {
            SystemModule.Height = -1;  // the system module doesn't get a height assigned later, so we set it here to something below everything else
                                       // create type synonym 'string'
            var str = new TypeSynonymDecl(Token.NoToken, "string",
              new TypeParameter.TypeParameterCharacteristics(TypeParameter.EqualitySupportValue.InferredRequired, Type.AutoInitInfo.CompilableValue, false),
              new List<TypeParameter>(), SystemModule, new SeqType(new CharType()), null);
            SystemModule.TopLevelDecls.Add(str);
            // create subset type 'nat'
            var bvNat = new BoundVar(Token.NoToken, "x", Type.Int);
            var natConstraint = Expression.CreateAtMost(Expression.CreateIntLiteral(Token.NoToken, 0), Expression.CreateIdentExpr(bvNat));
            var ax = AxiomAttribute();
            NatDecl = new SubsetTypeDecl(Token.NoToken, "nat",
              new TypeParameter.TypeParameterCharacteristics(TypeParameter.EqualitySupportValue.InferredRequired, Type.AutoInitInfo.CompilableValue, false),
              new List<TypeParameter>(), SystemModule, bvNat, natConstraint, SubsetTypeDecl.WKind.CompiledZero, null, ax);
            SystemModule.TopLevelDecls.Add(NatDecl);
            // create trait 'object'
            ObjectDecl = new TraitDecl(Token.NoToken, "object", SystemModule, new List<TypeParameter>(), new List<MemberDecl>(), DontCompile(), false, null);
            SystemModule.TopLevelDecls.Add(ObjectDecl);
            // add one-dimensional arrays, since they may arise during type checking
            // Arrays of other dimensions may be added during parsing as the parser detects the need for these
            UserDefinedType tmp = ArrayType(1, Type.Int, true);
            // Arrow types of other dimensions may be added during parsing as the parser detects the need for these.  For the 0-arity
            // arrow type, the resolver adds a Valid() predicate for iterators, whose corresponding arrow type is conveniently created here.
            CreateArrowTypeDecl(0);
            // Note, in addition to these types, the _System module contains tuple types.  These tuple types are added to SystemModule
            // by the parser as the parser detects the need for these.
        }

        private Attributes DontCompile()
        {
            var flse = Expression.CreateBoolLiteral(Token.NoToken, false);
            return new Attributes("compile", new List<Expression>() { flse }, null);
        }

        public static Attributes AxiomAttribute()
        {
            return new Attributes("axiom", new List<Expression>(), null);
        }

        public UserDefinedType ArrayType(int dims, Type arg, bool allowCreationOfNewClass)
        {
            Contract.Requires(1 <= dims);
            Contract.Requires(arg != null);
            return ArrayType(Token.NoToken, dims, new List<Type>() { arg }, allowCreationOfNewClass);
        }
        public UserDefinedType ArrayType(IToken tok, int dims, List<Type> optTypeArgs, bool allowCreationOfNewClass, bool useClassNameType = false)
        {
            Contract.Requires(tok != null);
            Contract.Requires(1 <= dims);
            Contract.Requires(optTypeArgs == null || optTypeArgs.Count > 0);  // ideally, it is 1, but more will generate an error later, and null means it will be filled in automatically
            Contract.Ensures(Contract.Result<UserDefinedType>() != null);

            var arrayName = ArrayClassName(dims);
            if (useClassNameType)
            {
                arrayName = arrayName + "?";
            }
            if (allowCreationOfNewClass && !arrayTypeDecls.ContainsKey(dims))
            {
                ArrayClassDecl arrayClass = new ArrayClassDecl(dims, SystemModule, DontCompile());
                for (int d = 0; d < dims; d++)
                {
                    string name = dims == 1 ? "Length" : "Length" + d;
                    Field len = new SpecialField(Token.NoToken, name, SpecialField.ID.ArrayLength, dims == 1 ? null : (object)d, false, false, false, Type.Int, null);
                    len.EnclosingClass = arrayClass;  // resolve here
                    arrayClass.Members.Add(len);
                }
                arrayTypeDecls.Add(dims, arrayClass);
                SystemModule.TopLevelDecls.Add(arrayClass);
                CreateArrowTypeDecl(dims);  // also create an arrow type with this arity, since it may be used in an initializing expression for the array
            }
            UserDefinedType udt = new UserDefinedType(tok, arrayName, optTypeArgs);
            return udt;
        }

        public static string ArrayClassName(int dims)
        {
            Contract.Requires(1 <= dims);
            if (dims == 1)
            {
                return "array";
            }
            else
            {
                return "array" + dims;
            }
        }

        /// <summary>
        /// Idempotently add an arrow type with arity 'arity' to the system module, and along
        /// with this arrow type, the two built-in subset types based on the arrow type.
        /// </summary>
        public void CreateArrowTypeDecl(int arity)
        {
            Contract.Requires(0 <= arity);
            if (!ArrowTypeDecls.ContainsKey(arity))
            {
                IToken tok = Token.NoToken;
                var tps = Util.Map(Enumerable.Range(0, arity + 1), x => x < arity ?
                  new TypeParameter(tok, "T" + x, TypeParameter.TPVarianceSyntax.Contravariance) :
                  new TypeParameter(tok, "R", TypeParameter.TPVarianceSyntax.Covariant_Strict));
                var tys = tps.ConvertAll(tp => (Type)(new UserDefinedType(tp)));
                var args = Util.Map(Enumerable.Range(0, arity), i => new Formal(tok, "x" + i, tys[i], true, false, null));
                var argExprs = args.ConvertAll(a =>
                      (Expression)new IdentifierExpr(tok, a.Name) { Var = a, Type = a.Type });
                var readsIS = new FunctionCallExpr(tok, "reads", new ImplicitThisExpr(tok), tok, argExprs)
                {
                    Type = new SetType(true, ObjectQ()),
                };
                var readsFrame = new List<FrameExpression> { new FrameExpression(tok, readsIS, null) };
                var req = new Function(tok, "requires", false, true,
                  new List<TypeParameter>(), args, null, Type.Bool,
                  new List<AttributedExpression>(), readsFrame, new List<AttributedExpression>(),
                  new Specification<Expression>(new List<Expression>(), null),
                  null, null, null, null, null);
                var reads = new Function(tok, "reads", false, true,
                  new List<TypeParameter>(), args, null, new SetType(true, ObjectQ()),
                  new List<AttributedExpression>(), readsFrame, new List<AttributedExpression>(),
                  new Specification<Expression>(new List<Expression>(), null),
                  null, null, null, null, null);
                readsIS.Function = reads;  // just so we can really claim the member declarations are resolved
                readsIS.TypeApplication_AtEnclosingClass = tys;  // ditto
                readsIS.TypeApplication_JustFunction = new List<Type>();  // ditto
                var arrowDecl = new ArrowTypeDecl(tps, req, reads, SystemModule, DontCompile());
                ArrowTypeDecls.Add(arity, arrowDecl);
                SystemModule.TopLevelDecls.Add(arrowDecl);

                // declaration of read-effect-free arrow-type, aka heap-independent arrow-type, aka partial-function arrow-type
                tps = Util.Map(Enumerable.Range(0, arity + 1), x => x < arity ?
                  new TypeParameter(tok, "T" + x, TypeParameter.TPVarianceSyntax.Contravariance) :
                  new TypeParameter(tok, "R", TypeParameter.TPVarianceSyntax.Covariant_Strict));
                tys = tps.ConvertAll(tp => (Type)(new UserDefinedType(tp)));
                var id = new BoundVar(tok, "f", new ArrowType(tok, arrowDecl, tys));
                var partialArrow = new SubsetTypeDecl(tok, ArrowType.PartialArrowTypeName(arity),
                  new TypeParameter.TypeParameterCharacteristics(false), tps, SystemModule,
                  id, ArrowSubtypeConstraint(tok, tok, id, reads, tps, false), SubsetTypeDecl.WKind.Special, null, DontCompile());
                PartialArrowTypeDecls.Add(arity, partialArrow);
                SystemModule.TopLevelDecls.Add(partialArrow);

                // declaration of total arrow-type

                tps = Util.Map(Enumerable.Range(0, arity + 1), x => x < arity ?
                  new TypeParameter(tok, "T" + x, TypeParameter.TPVarianceSyntax.Contravariance) :
                  new TypeParameter(tok, "R", TypeParameter.TPVarianceSyntax.Covariant_Strict));
                tys = tps.ConvertAll(tp => (Type)(new UserDefinedType(tp)));
                id = new BoundVar(tok, "f", new UserDefinedType(tok, partialArrow.Name, partialArrow, tys));
                var totalArrow = new SubsetTypeDecl(tok, ArrowType.TotalArrowTypeName(arity),
                  new TypeParameter.TypeParameterCharacteristics(false), tps, SystemModule,
                  id, ArrowSubtypeConstraint(tok, tok, id, req, tps, true), SubsetTypeDecl.WKind.Special, null, DontCompile());
                TotalArrowTypeDecls.Add(arity, totalArrow);
                SystemModule.TopLevelDecls.Add(totalArrow);
            }
        }

        /// <summary>
        /// Returns an expression that is the constraint of:
        /// the built-in partial-arrow type (if "!total", in which case "member" is expected to denote the "reads" member), or
        /// the built-in total-arrow type (if "total", in which case "member" is expected to denote the "requires" member).
        /// The given "id" is expected to be already resolved.
        /// </summary>
        private Expression ArrowSubtypeConstraint(IToken tok, IToken endTok, BoundVar id, Function member, List<TypeParameter> tps, bool total)
        {
            Contract.Requires(tok != null);
            Contract.Requires(endTok != null);
            Contract.Requires(id != null);
            Contract.Requires(member != null);
            Contract.Requires(tps != null && 1 <= tps.Count);
            var f = new IdentifierExpr(tok, id);
            // forall x0,x1,x2 :: f.reads(x0,x1,x2) == {}
            // OR
            // forall x0,x1,x2 :: f.requires(x0,x1,x2)
            var bvs = new List<BoundVar>();
            var args = new List<Expression>();
            var bounds = new List<Microsoft.Dafny.ComprehensionExpr.BoundedPool>();
            for (int i = 0; i < tps.Count - 1; i++)
            {
                var bv = new BoundVar(tok, "x" + i, new UserDefinedType(tps[i]));
                bvs.Add(bv);
                args.Add(new IdentifierExpr(tok, bv));
                bounds.Add(new ComprehensionExpr.SpecialAllocIndependenceAllocatedBoundedPool());
            }
            var fn = new MemberSelectExpr(tok, f, member.Name)
            {
                Member = member,
                TypeApplication_AtEnclosingClass = f.Type.TypeArgs,
                TypeApplication_JustMember = new List<Type>(),
                Type = GetTypeOfFunction(member, tps.ConvertAll(tp => (Type)new UserDefinedType(tp)), new List<Type>())
            };
            Expression body = new ApplyExpr(tok, fn, args);
            body.Type = member.ResultType;  // resolve here
            if (!total)
            {
                Expression emptySet = new SetDisplayExpr(tok, true, new List<Expression>());
                emptySet.Type = member.ResultType;  // resolve here
                body = Expression.CreateEq(body, emptySet, member.ResultType);
            }
            if (tps.Count > 1)
            {
                body = new ForallExpr(tok, endTok, bvs, null, body, null) { Type = Type.Bool, Bounds = bounds };
            }
            return body;
        }

        Type GetTypeOfFunction(Function f, List<Type> typeArgumentsClass, List<Type> typeArgumentsMember)
        {
            Contract.Requires(f != null);
            Contract.Requires(f.EnclosingClass != null);
            Contract.Requires(typeArgumentsClass != null);
            Contract.Requires(typeArgumentsMember != null);
            Contract.Requires(typeArgumentsClass.Count == f.EnclosingClass.TypeArgs.Count);
            Contract.Requires(typeArgumentsMember.Count == f.TypeArgs.Count);

            var atd = ArrowTypeDecls[f.Formals.Count];

            var formals = Util.Concat(f.EnclosingClass.TypeArgs, f.TypeArgs);
            var actuals = Util.Concat(typeArgumentsClass, typeArgumentsMember);
            var typeMap = Resolver.TypeSubstitutionMap(formals, actuals);
            return new ArrowType(f.Tok, atd, f.Formals.ConvertAll(arg => Resolver.SubstType(arg.Type, typeMap)), Resolver.SubstType(f.ResultType, typeMap));
        }

        public TupleTypeDecl TupleType(IToken tok, int dims, bool allowCreationOfNewType, List<bool> argumentGhostness = null)
        {
            Contract.Requires(tok != null);
            Contract.Requires(0 <= dims);
            Contract.Requires(argumentGhostness == null || argumentGhostness.Count == dims);
            Contract.Ensures(Contract.Result<TupleTypeDecl>() != null);

            TupleTypeDecl tt;
            argumentGhostness = argumentGhostness ?? new bool[dims].Select(_ => false).ToList();
            if (!tupleTypeDecls.TryGetValue(argumentGhostness, out tt))
            {
                Contract.Assume(allowCreationOfNewType);  // the parser should ensure that all needed tuple types exist by the time of resolution
                                                          // tuple#2 is already defined in DafnyRuntime.cs
                var attributes = dims == 2 && !argumentGhostness.Contains(true) ? DontCompile() : null;
                tt = new TupleTypeDecl(argumentGhostness, SystemModule, attributes);
                tupleTypeDecls.Add(argumentGhostness, tt);
                SystemModule.TopLevelDecls.Add(tt);
            }
            return tt;
        }

        public static char IsGhostToChar(bool isGhost)
        {
            return isGhost ? 'G' : 'O';
        }

        public static bool IsGhostFromChar(char c)
        {
            Contract.Requires(c == 'G' || c == 'O');
            return c == 'G';
        }

        public static string ArgumentGhostnessToString(List<bool> argumentGhostness)
        {
            return argumentGhostness.Count + (!argumentGhostness.Contains(true)
              ? "" : String.Concat(argumentGhostness.Select(IsGhostToChar)));
        }

        public static IEnumerable<bool> ArgumentGhostnessFromString(string s, int count)
        {
            List<bool> argumentGhostness = new bool[count].ToList();
            if (System.Char.IsDigit(s[s.Length - 1]))
            {
                return argumentGhostness.Select(_ => false);
            }
            else
            {
                return argumentGhostness.Select((_, i) => IsGhostFromChar(s[s.Length - count + i]));
            }
        }

        public static string TupleTypeName(List<bool> argumentGhostness)
        {
            return "_tuple#" + ArgumentGhostnessToString(argumentGhostness);
        }

        public static bool IsTupleTypeName(string s)
        {
            Contract.Requires(s != null);
            return s.StartsWith("_tuple#");
        }
        public const string TupleTypeCtorNamePrefix = "_#Make";  // the printer wants this name prefix to be uniquely recognizable

        public static string TupleTypeCtorName(int dims)
        {
            Contract.Assert(0 <= dims);
            return TupleTypeCtorNamePrefix + dims;
        }
    }
    public class Program
    {
        [ContractInvariantMethod]
        void ObjectInvariant()
        {
            Contract.Invariant(FullName != null);
            Contract.Invariant(DefaultModule != null);
        }

        public readonly string FullName;
        public Dictionary<ModuleDefinition, ModuleSignature> ModuleSigs; // filled in during resolution.
                                                                         // Resolution essentially flattens the module hierarchy, for
                                                                         // purposes of translation and compilation.
        public List<ModuleDefinition> CompileModules; // filled in during resolution.
                                                      // Contains the definitions to be used for compilation.

        public Method MainMethod; // Method to be used as main if compiled
        public readonly ModuleDecl DefaultModule;
        public readonly ModuleDefinition DefaultModuleDef;
        public readonly BuiltIns BuiltIns;
        public readonly ErrorReporter Reporter;

        public Program(string name, [Captured] ModuleDecl defaultModule, [Captured] BuiltIns builtIns, ErrorReporter reporter)
        {
            Contract.Requires(name != null);
            Contract.Requires(defaultModule != null);
            Contract.Requires(defaultModule is LiteralModuleDecl);
            Contract.Requires(reporter != null);
            FullName = name;
            DefaultModule = defaultModule;
            DefaultModuleDef = (DefaultModuleDecl)((LiteralModuleDecl)defaultModule).ModuleDef;
            BuiltIns = builtIns;
            this.Reporter = reporter;
            ModuleSigs = new Dictionary<ModuleDefinition, ModuleSignature>();
            CompileModules = new List<ModuleDefinition>();
        }

        //Set appropriate visibilty before presenting module
        public IEnumerable<ModuleDefinition> Modules()
        {

            foreach (var msig in ModuleSigs)
            {
                Type.PushScope(msig.Value.VisibilityScope);
                yield return msig.Key;
                Type.PopScope(msig.Value.VisibilityScope);
            }

        }

        public IEnumerable<ModuleDefinition> RawModules()
        {
            return ModuleSigs.Keys;
        }

        public string Name
        {
            get
            {
                try
                {
                    return System.IO.Path.GetFileName(FullName);
                }
                catch (ArgumentException)
                {
                    return FullName;
                }
            }
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

        public virtual Microsoft.Dafny.V2.UserDefinedType TransformUnion(Microsoft.Dafny.UserDefinedType value) =>
        value switch
        {
            Microsoft.Dafny.ArrowType subType => Transform(subType),
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
            Microsoft.Dafny.ParamTypeProxy subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.V2.TopLevelDecl TransformUnion(Microsoft.Dafny.TopLevelDecl value) =>
        value switch
        {
            Microsoft.Dafny.TypeParameter subType => Transform(subType),
            Microsoft.Dafny.ModuleDecl subType => TransformUnion(subType),
            Microsoft.Dafny.TopLevelDeclWithMembers subType => TransformUnion(subType),
            Microsoft.Dafny.ValuetypeDecl subType => Transform(subType),
            Microsoft.Dafny.TypeSynonymDeclBase subType => TransformUnion(subType),
        };

        public virtual Microsoft.Dafny.V2.ModuleDecl TransformUnion(Microsoft.Dafny.ModuleDecl value) =>
        value switch
        {
            Microsoft.Dafny.LiteralModuleDecl subType => Transform(subType),
            Microsoft.Dafny.AliasModuleDecl subType => Transform(subType),
            Microsoft.Dafny.AbstractModuleDecl subType => Transform(subType),
            Microsoft.Dafny.ModuleExportDecl subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.V2.ModuleDefinition TransformUnion(Microsoft.Dafny.ModuleDefinition value) =>
        value switch
        {
            Microsoft.Dafny.DefaultModuleDecl subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.Declaration TransformUnion(Microsoft.Dafny.Declaration value) =>
        value switch
        {
            Microsoft.Dafny.TopLevelDecl subType => TransformUnion(subType),
            Microsoft.Dafny.DatatypeCtor subType => Transform(subType),
            Microsoft.Dafny.MemberDecl subType => TransformUnion(subType),
        };

        public virtual Microsoft.Dafny.V2.ClassDecl TransformUnion(Microsoft.Dafny.ClassDecl value) =>
        value switch
        {
            Microsoft.Dafny.TraitDecl subType => Transform(subType),
            Microsoft.Dafny.DefaultClassDecl subType => Transform(subType),
            Microsoft.Dafny.ArrayClassDecl subType => Transform(subType),
            Microsoft.Dafny.ArrowTypeDecl subType => Transform(subType),
            Microsoft.Dafny.IteratorDecl subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.V2.TopLevelDeclWithMembers TransformUnion(Microsoft.Dafny.TopLevelDeclWithMembers value) =>
        value switch
        {
            Microsoft.Dafny.ClassDecl subType => TransformUnion(subType),
            Microsoft.Dafny.DatatypeDecl subType => TransformUnion(subType),
            Microsoft.Dafny.OpaqueTypeDecl subType => Transform(subType),
            Microsoft.Dafny.NewtypeDecl subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.V2.DatatypeDecl TransformUnion(Microsoft.Dafny.DatatypeDecl value) =>
        value switch
        {
            Microsoft.Dafny.IndDatatypeDecl subType => TransformUnion(subType),
            Microsoft.Dafny.CoDatatypeDecl subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.V2.IndDatatypeDecl TransformUnion(Microsoft.Dafny.IndDatatypeDecl value) =>
        value switch
        {
            Microsoft.Dafny.TupleTypeDecl subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.V2.MemberDecl TransformUnion(Microsoft.Dafny.MemberDecl value) =>
        value switch
        {
            Microsoft.Dafny.Field subType => TransformUnion(subType),
            Microsoft.Dafny.Function subType => TransformUnion(subType),
            Microsoft.Dafny.Method subType => TransformUnion(subType),
        };

        public virtual Microsoft.Dafny.V2.Function TransformUnion(Microsoft.Dafny.Function value) =>
        value switch
        {
            Microsoft.Dafny.SpecialFunction subType => Transform(subType),
            Microsoft.Dafny.Predicate subType => Transform(subType),
            Microsoft.Dafny.PrefixPredicate subType => Transform(subType),
            Microsoft.Dafny.ExtremePredicate subType => TransformUnion(subType),
            Microsoft.Dafny.TwoStateFunction subType => TransformUnion(subType),
        };

        public virtual Microsoft.Dafny.V2.Field TransformUnion(Microsoft.Dafny.Field value) =>
        value switch
        {
            Microsoft.Dafny.SpecialField subType => TransformUnion(subType),
        };

        public virtual Microsoft.Dafny.V2.SpecialField TransformUnion(Microsoft.Dafny.SpecialField value) =>
        value switch
        {
            Microsoft.Dafny.DatatypeDestructor subType => Transform(subType),
            Microsoft.Dafny.ConstantField subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.V2.TypeSynonymDeclBase TransformUnion(Microsoft.Dafny.TypeSynonymDeclBase value) =>
        value switch
        {
            Microsoft.Dafny.TypeSynonymDecl subType => TransformUnion(subType),
            Microsoft.Dafny.InternalTypeSynonymDecl subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.V2.TypeSynonymDecl TransformUnion(Microsoft.Dafny.TypeSynonymDecl value) =>
        value switch
        {
            Microsoft.Dafny.SubsetTypeDecl subType => TransformUnion(subType),
        };

        public virtual Microsoft.Dafny.V2.SubsetTypeDecl TransformUnion(Microsoft.Dafny.SubsetTypeDecl value) =>
        value switch
        {
            Microsoft.Dafny.NonNullTypeDecl subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.Formal TransformUnion(Microsoft.Dafny.Formal value) =>
        value switch
        {
            Microsoft.Dafny.ImplicitFormal subType => subType,
        };

        public virtual Microsoft.Dafny.V2.ExtremePredicate TransformUnion(Microsoft.Dafny.ExtremePredicate value) =>
        value switch
        {
            Microsoft.Dafny.LeastPredicate subType => Transform(subType),
            Microsoft.Dafny.GreatestPredicate subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.V2.TwoStateFunction TransformUnion(Microsoft.Dafny.TwoStateFunction value) =>
        value switch
        {
            Microsoft.Dafny.TwoStatePredicate subType => Transform(subType),
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

        public virtual Microsoft.Dafny.V2.LoopStmt TransformUnion(Microsoft.Dafny.LoopStmt value) =>
        value switch
        {
            Microsoft.Dafny.OneBodyLoopStmt subType => TransformUnion(subType),
            Microsoft.Dafny.AlternativeLoopStmt subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.V2.OneBodyLoopStmt TransformUnion(Microsoft.Dafny.OneBodyLoopStmt value) =>
        value switch
        {
            Microsoft.Dafny.WhileStmt subType => TransformUnion(subType),
            Microsoft.Dafny.ForLoopStmt subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.V2.WhileStmt TransformUnion(Microsoft.Dafny.WhileStmt value) =>
        value switch
        {
            Microsoft.Dafny.RefinedWhileStmt subType => Transform(subType),
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

        public virtual Microsoft.Dafny.V2.QuantifierExpr TransformUnion(Microsoft.Dafny.QuantifierExpr value) =>
        value switch
        {
            Microsoft.Dafny.ForallExpr subType => Transform(subType),
            Microsoft.Dafny.ExistsExpr subType => Transform(subType),
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

        public virtual Microsoft.Dafny.ComprehensionExpr.CollectionBoundedPool TransformUnion(Microsoft.Dafny.ComprehensionExpr.CollectionBoundedPool value) =>
        value switch
        {
            Microsoft.Dafny.ComprehensionExpr.SetBoundedPool subType => Transform(subType),
            Microsoft.Dafny.ComprehensionExpr.MultiSetBoundedPool subType => Transform(subType),
            Microsoft.Dafny.ComprehensionExpr.MapBoundedPool subType => Transform(subType),
            Microsoft.Dafny.ComprehensionExpr.SeqBoundedPool subType => Transform(subType),
        };

        public virtual Microsoft.Dafny.V2.LetExpr TransformUnion(Microsoft.Dafny.LetExpr value) =>
        value switch
        {

        };

        public virtual Microsoft.Dafny.V2.ModuleDefinition Transform(Microsoft.Dafny.ModuleDefinition value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.Attributes Transform(Microsoft.Dafny.Attributes value)
        {
            return new Microsoft.Dafny.Attributes(value.Name, value.Args, value.Prev);
        }

        public virtual Microsoft.Dafny.V2.TypeParameter Transform(Microsoft.Dafny.TypeParameter value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.SelfType Transform(Microsoft.Dafny.SelfType value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.MapType Transform(Microsoft.Dafny.MapType value)
        {
            return new Microsoft.Dafny.MapType(value.Finite, value.Domain, value.Range);
        }

        public virtual Microsoft.Dafny.V2.ModuleSignature Transform(Microsoft.Dafny.ModuleSignature value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.ModuleExportDecl Transform(Microsoft.Dafny.ModuleExportDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.ExportSignature Transform(Microsoft.Dafny.ExportSignature value)
        {
            return new Microsoft.Dafny.ExportSignature(value.PrefixTok, value.Prefix, value.IdTok, value.Id, value.Opaque);
        }

        public virtual Microsoft.Dafny.Formal Transform(Microsoft.Dafny.Formal value)
        {
            return new Microsoft.Dafny.Formal(value.Tok, value.Name, value.Type, value.InParam, value.IsGhost, value.DefaultValue, value.IsOld, value.IsNameOnly, value.NameForCompilation);
        }

        public virtual Microsoft.Dafny.V2.DatatypeCtor Transform(Microsoft.Dafny.DatatypeCtor value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.TupleTypeDecl Transform(Microsoft.Dafny.TupleTypeDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.IndDatatypeDecl Transform(Microsoft.Dafny.IndDatatypeDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.CoDatatypeDecl Transform(Microsoft.Dafny.CoDatatypeDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.SpecialField Transform(Microsoft.Dafny.SpecialField value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.DatatypeDestructor Transform(Microsoft.Dafny.DatatypeDestructor value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.ConstantField Transform(Microsoft.Dafny.ConstantField value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.Field Transform(Microsoft.Dafny.Field value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.AttributedExpression Transform(Microsoft.Dafny.AttributedExpression value)
        {
            return new Microsoft.Dafny.AttributedExpression(value.E);
        }

        public virtual Microsoft.Dafny.V2.FrameExpression Transform(Microsoft.Dafny.FrameExpression value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.Specification<T> Transform(Microsoft.Dafny.Specification<T> value)
        {
            return new Microsoft.Dafny.Specification<T>(value.Exprs, value.Attributes);
        }

        public virtual Microsoft.Dafny.BlockStmt Transform(Microsoft.Dafny.BlockStmt value)
        {
            return new Microsoft.Dafny.BlockStmt(value.Tok, value.EndTok, value.Body);
        }

        public virtual Microsoft.Dafny.AssertStmt Transform(Microsoft.Dafny.AssertStmt value)
        {
            return new Microsoft.Dafny.AssertStmt(value.Tok, value.EndTok, value.Expr, value.Proof, value.Label, value.Attributes);
        }

        public virtual Microsoft.Dafny.ExpectStmt Transform(Microsoft.Dafny.ExpectStmt value)
        {
            return new Microsoft.Dafny.ExpectStmt(value.Tok, value.EndTok, value.Expr, value.Message, value.Attributes);
        }

        public virtual Microsoft.Dafny.PrintStmt Transform(Microsoft.Dafny.PrintStmt value)
        {
            return new Microsoft.Dafny.PrintStmt(value.Tok, value.EndTok, value.Args);
        }

        public virtual Microsoft.Dafny.RevealStmt Transform(Microsoft.Dafny.RevealStmt value)
        {
            return new Microsoft.Dafny.RevealStmt(value.Tok, value.EndTok, value.Exprs);
        }

        public virtual Microsoft.Dafny.BreakStmt Transform(Microsoft.Dafny.BreakStmt value)
        {
            return new Microsoft.Dafny.BreakStmt(value.Tok, value.EndTok, value.TargetLabel);
        }

        public virtual Microsoft.Dafny.ExprRhs Transform(Microsoft.Dafny.ExprRhs value)
        {
            return new Microsoft.Dafny.ExprRhs(value.Expr, value.Attributes);
        }

        public virtual Microsoft.Dafny.ActualBindings Transform(Microsoft.Dafny.ActualBindings value)
        {
            return new Microsoft.Dafny.ActualBindings(value.ArgumentBindings);
        }

        public virtual Microsoft.Dafny.ActualBinding Transform(Microsoft.Dafny.ActualBinding value)
        {
            return new Microsoft.Dafny.ActualBinding(value.FormalParameterName, value.Actual, value.IsGhost);
        }

        public virtual Microsoft.Dafny.V2.CallStmt Transform(Microsoft.Dafny.CallStmt value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.MemberSelectExpr Transform(Microsoft.Dafny.MemberSelectExpr value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.TypeRhs Transform(Microsoft.Dafny.TypeRhs value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.UpdateStmt Transform(Microsoft.Dafny.UpdateStmt value)
        {
            return new Microsoft.Dafny.UpdateStmt(value.Tok, value.EndTok, value.Lhss, value.Rhss, value.Mutate);
        }

        public virtual Microsoft.Dafny.LocalVariable Transform(Microsoft.Dafny.LocalVariable value)
        {
            return new Microsoft.Dafny.LocalVariable(value.Tok, value.EndTok, value.Name, value.Type, value.IsGhost);
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.ExactBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.ExactBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.ExactBoundedPool(value.E);
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.AllocFreeBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.AllocFreeBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.AllocFreeBoundedPool(value.T);
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.IntBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.IntBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.IntBoundedPool(value.LowerBound, value.UpperBound);
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.SetBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.SetBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.SetBoundedPool(value.Set, value.BvType, value.CollectionElementType, value.IsFiniteCollection);
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.MultiSetBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.MultiSetBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.MultiSetBoundedPool(value.Multiset, value.BvType, value.CollectionElementType);
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.MapBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.MapBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.MapBoundedPool(value.Map, value.BvType, value.CollectionElementType, value.IsFiniteCollection);
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.SeqBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.SeqBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.SeqBoundedPool(value.Seq, value.BvType, value.CollectionElementType);
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.SubSetBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.SubSetBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.SubSetBoundedPool(value.Set, value.IsFiniteCollection);
        }

        public virtual Microsoft.Dafny.ComprehensionExpr.SuperSetBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.SuperSetBoundedPool value)
        {
            return new Microsoft.Dafny.ComprehensionExpr.SuperSetBoundedPool(value.Set);
        }

        public virtual Microsoft.Dafny.V2.DatatypeBoundedPool Transform(Microsoft.Dafny.ComprehensionExpr.DatatypeBoundedPool value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.AssignSuchThatStmt Transform(Microsoft.Dafny.AssignSuchThatStmt value)
        {
            return new Microsoft.Dafny.AssignSuchThatStmt(value.Tok, value.EndTok, value.Lhss, value.Expr, value.AssumeToken, value.Attributes);
        }

        public virtual Microsoft.Dafny.AssignOrReturnStmt Transform(Microsoft.Dafny.AssignOrReturnStmt value)
        {
            return new Microsoft.Dafny.AssignOrReturnStmt(value.Tok, value.EndTok, value.Lhss, value.Rhs, value.KeywordToken, value.Rhss);
        }

        public virtual Microsoft.Dafny.VarDeclStmt Transform(Microsoft.Dafny.VarDeclStmt value)
        {
            return new Microsoft.Dafny.VarDeclStmt(value.Tok, value.EndTok, value.Locals, value.Update);
        }

        public virtual Microsoft.Dafny.V2.CasePattern<VT> Transform(Microsoft.Dafny.CasePattern<VT> value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.VarDeclPattern Transform(Microsoft.Dafny.VarDeclPattern value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.AssignStmt Transform(Microsoft.Dafny.AssignStmt value)
        {
            return new Microsoft.Dafny.AssignStmt(value.Tok, value.EndTok, value.Lhs, value.Rhs);
        }

        public virtual Microsoft.Dafny.IfStmt Transform(Microsoft.Dafny.IfStmt value)
        {
            return new Microsoft.Dafny.IfStmt(value.Tok, value.EndTok, value.IsBindingGuard, value.Guard, value.Thn, value.Els);
        }

        public virtual Microsoft.Dafny.GuardedAlternative Transform(Microsoft.Dafny.GuardedAlternative value)
        {
            return new Microsoft.Dafny.GuardedAlternative(value.Tok, value.IsBindingGuard, value.Guard, value.Body);
        }

        public virtual Microsoft.Dafny.AlternativeStmt Transform(Microsoft.Dafny.AlternativeStmt value)
        {
            return new Microsoft.Dafny.AlternativeStmt(value.Tok, value.EndTok, value.Alternatives, value.UsesOptionalBraces);
        }

        public virtual Microsoft.Dafny.V2.RefinedWhileStmt Transform(Microsoft.Dafny.RefinedWhileStmt value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.WhileStmt Transform(Microsoft.Dafny.WhileStmt value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.ForLoopStmt Transform(Microsoft.Dafny.ForLoopStmt value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.AlternativeLoopStmt Transform(Microsoft.Dafny.AlternativeLoopStmt value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.ForallStmt Transform(Microsoft.Dafny.ForallStmt value)
        {
            return new Microsoft.Dafny.ForallStmt(value.Tok, value.EndTok, value.BoundVars, value.Attributes, value.Range, value.Ens, value.Body);
        }

        public virtual Microsoft.Dafny.V2.ModifyStmt Transform(Microsoft.Dafny.ModifyStmt value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.CalcStmt.TernaryCalcOp Transform(Microsoft.Dafny.CalcStmt.TernaryCalcOp value)
        {
            return new Microsoft.Dafny.CalcStmt.TernaryCalcOp(value.Idx);
        }

        public virtual Microsoft.Dafny.CalcStmt Transform(Microsoft.Dafny.CalcStmt value)
        {
            return new Microsoft.Dafny.CalcStmt(value.Tok, value.EndTok, value.UserSuppliedOp, value.Lines, value.Hints, value.StepOps, value.Attributes);
        }

        public virtual Microsoft.Dafny.MatchCaseStmt Transform(Microsoft.Dafny.MatchCaseStmt value)
        {
            return new Microsoft.Dafny.MatchCaseStmt(value.Tok, value.Ctor, value.Arguments, value.Body, value.Attributes);
        }

        public virtual Microsoft.Dafny.LiteralExpr Transform(Microsoft.Dafny.LiteralExpr value)
        {
            return new Microsoft.Dafny.LiteralExpr(value.Tok);
        }

        public virtual Microsoft.Dafny.StaticReceiverExpr Transform(Microsoft.Dafny.StaticReceiverExpr value)
        {
            return new Microsoft.Dafny.StaticReceiverExpr(value.Tok, value.T, value.IsImplicit);
        }

        public virtual Microsoft.Dafny.LitCtx Transform(Microsoft.Dafny.LitCtx value)
        {
            return new Microsoft.Dafny.LitCtx(value.Lit);
        }

        public virtual Microsoft.Dafny.V2.MatchStmt Transform(Microsoft.Dafny.MatchStmt value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.SkeletonStatement Transform(Microsoft.Dafny.SkeletonStatement value)
        {
            return new Microsoft.Dafny.SkeletonStatement(value.Tok, value.EndTok);
        }

        public virtual Microsoft.Dafny.NestedMatchCaseStmt Transform(Microsoft.Dafny.NestedMatchCaseStmt value)
        {
            return new Microsoft.Dafny.NestedMatchCaseStmt(value.Tok, value.Pat, value.Body);
        }

        public virtual Microsoft.Dafny.NestedMatchStmt Transform(Microsoft.Dafny.NestedMatchStmt value)
        {
            return new Microsoft.Dafny.NestedMatchStmt(value.Tok, value.EndTok, value.Source, value.Cases, value.UsesOptionalBraces, value.Attributes);
        }

        public virtual Microsoft.Dafny.DividedBlockStmt Transform(Microsoft.Dafny.DividedBlockStmt value)
        {
            return new Microsoft.Dafny.DividedBlockStmt(value.Tok, value.EndTok, value.BodyInit, value.SeparatorTok, value.BodyProper);
        }

        public virtual Microsoft.Dafny.V2.Method Transform(Microsoft.Dafny.Method value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
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

        public virtual Microsoft.Dafny.V2.Function Transform(Microsoft.Dafny.Function value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.FunctionCallExpr Transform(Microsoft.Dafny.FunctionCallExpr value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.SpecialFunction Transform(Microsoft.Dafny.SpecialFunction value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.Predicate Transform(Microsoft.Dafny.Predicate value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.PrefixPredicate Transform(Microsoft.Dafny.PrefixPredicate value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.LeastPredicate Transform(Microsoft.Dafny.LeastPredicate value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.GreatestPredicate Transform(Microsoft.Dafny.GreatestPredicate value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.TwoStatePredicate Transform(Microsoft.Dafny.TwoStatePredicate value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.TwoStateFunction Transform(Microsoft.Dafny.TwoStateFunction value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.LiteralModuleDecl Transform(Microsoft.Dafny.LiteralModuleDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.ModuleQualifiedId Transform(Microsoft.Dafny.ModuleQualifiedId value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.AliasModuleDecl Transform(Microsoft.Dafny.AliasModuleDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.AbstractModuleDecl Transform(Microsoft.Dafny.AbstractModuleDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.TraitDecl Transform(Microsoft.Dafny.TraitDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.InheritanceInformationClass Transform(Microsoft.Dafny.TopLevelDeclWithMembers.InheritanceInformationClass value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.NonNullTypeDecl Transform(Microsoft.Dafny.NonNullTypeDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.ClassDecl Transform(Microsoft.Dafny.ClassDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.DefaultClassDecl Transform(Microsoft.Dafny.DefaultClassDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.ArrayClassDecl Transform(Microsoft.Dafny.ArrayClassDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.ArrowTypeDecl Transform(Microsoft.Dafny.ArrowTypeDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.IteratorDecl Transform(Microsoft.Dafny.IteratorDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.OpaqueTypeDecl Transform(Microsoft.Dafny.OpaqueTypeDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.NewtypeDecl Transform(Microsoft.Dafny.NewtypeDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.ValuetypeDecl Transform(Microsoft.Dafny.ValuetypeDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.SubsetTypeDecl Transform(Microsoft.Dafny.SubsetTypeDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.TypeSynonymDecl Transform(Microsoft.Dafny.TypeSynonymDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.InternalTypeSynonymDecl Transform(Microsoft.Dafny.InternalTypeSynonymDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.ArrowType Transform(Microsoft.Dafny.ArrowType value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.UserDefinedType Transform(Microsoft.Dafny.UserDefinedType value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.ParamTypeProxy Transform(Microsoft.Dafny.ParamTypeProxy value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.DatatypeValue Transform(Microsoft.Dafny.DatatypeValue value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.Resolver_IdentifierExpr Transform(Microsoft.Dafny.Resolver_IdentifierExpr value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.ExpressionPair Transform(Microsoft.Dafny.ExpressionPair value)
        {
            return new Microsoft.Dafny.ExpressionPair(value.A, value.B);
        }

        public virtual Microsoft.Dafny.MapDisplayExpr Transform(Microsoft.Dafny.MapDisplayExpr value)
        {
            return new Microsoft.Dafny.MapDisplayExpr(value.Tok, value.Finite, value.Elements);
        }

        public virtual Microsoft.Dafny.SeqSelectExpr Transform(Microsoft.Dafny.SeqSelectExpr value)
        {
            return new Microsoft.Dafny.SeqSelectExpr(value.Tok, value.SelectOne, value.Seq, value.E0, value.E1);
        }

        public virtual Microsoft.Dafny.MultiSelectExpr Transform(Microsoft.Dafny.MultiSelectExpr value)
        {
            return new Microsoft.Dafny.MultiSelectExpr(value.Tok, value.Array, value.Indices);
        }

        public virtual Microsoft.Dafny.SeqUpdateExpr Transform(Microsoft.Dafny.SeqUpdateExpr value)
        {
            return new Microsoft.Dafny.SeqUpdateExpr(value.Tok, value.Seq, value.Index, value.Val);
        }

        public virtual Microsoft.Dafny.ApplyExpr Transform(Microsoft.Dafny.ApplyExpr value)
        {
            return new Microsoft.Dafny.ApplyExpr(value.Tok, value.Fn, value.Args);
        }

        public virtual Microsoft.Dafny.SeqConstructionExpr Transform(Microsoft.Dafny.SeqConstructionExpr value)
        {
            return new Microsoft.Dafny.SeqConstructionExpr(value.Tok, value.ElementType, value.Length, value.Initializer);
        }

        public virtual Microsoft.Dafny.MultiSetFormingExpr Transform(Microsoft.Dafny.MultiSetFormingExpr value)
        {
            return new Microsoft.Dafny.MultiSetFormingExpr(value.Tok, value.Expr);
        }

        public virtual Microsoft.Dafny.OldExpr Transform(Microsoft.Dafny.OldExpr value)
        {
            return new Microsoft.Dafny.OldExpr(value.Tok, value.Expr, value.At);
        }

        public virtual Microsoft.Dafny.V2.UnchangedExpr Transform(Microsoft.Dafny.UnchangedExpr value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.BinaryExpr Transform(Microsoft.Dafny.BinaryExpr value)
        {
            return new Microsoft.Dafny.BinaryExpr(value.Tok, value.Op, value.E0, value.E1);
        }

        public virtual Microsoft.Dafny.TernaryExpr Transform(Microsoft.Dafny.TernaryExpr value)
        {
            return new Microsoft.Dafny.TernaryExpr(value.Tok, value.Op, value.E0, value.E1, value.E2);
        }

        public virtual Microsoft.Dafny.V2.LetExpr Transform(Microsoft.Dafny.LetExpr value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.ForallExpr Transform(Microsoft.Dafny.ForallExpr value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.ExistsExpr Transform(Microsoft.Dafny.ExistsExpr value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.MapComprehension Transform(Microsoft.Dafny.MapComprehension value)
        {
            return new Microsoft.Dafny.MapComprehension(value.Tok, value.EndTok, value.Finite, value.Bvars, value.Range, value.TermLeft, value.TermRight, value.Attributes);
        }

        public virtual Microsoft.Dafny.V2.LambdaExpr Transform(Microsoft.Dafny.LambdaExpr value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.StmtExpr Transform(Microsoft.Dafny.StmtExpr value)
        {
            return new Microsoft.Dafny.StmtExpr(value.Tok, value.Stmt, value.Expr);
        }

        public virtual Microsoft.Dafny.ITEExpr Transform(Microsoft.Dafny.ITEExpr value)
        {
            return new Microsoft.Dafny.ITEExpr(value.Tok, value.IsBindingGuard, value.Test, value.Thn, value.Els);
        }

        public virtual Microsoft.Dafny.MatchCaseExpr Transform(Microsoft.Dafny.MatchCaseExpr value)
        {
            return new Microsoft.Dafny.MatchCaseExpr(value.Tok, value.Ctor, value.Arguments, value.Body, value.Attributes);
        }

        public virtual Microsoft.Dafny.V2.MatchExpr Transform(Microsoft.Dafny.MatchExpr value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.BoxingCastExpr Transform(Microsoft.Dafny.BoxingCastExpr value)
        {
            return new Microsoft.Dafny.BoxingCastExpr(value.E, value.FromType, value.ToType);
        }

        public virtual Microsoft.Dafny.UnboxingCastExpr Transform(Microsoft.Dafny.UnboxingCastExpr value)
        {
            return new Microsoft.Dafny.UnboxingCastExpr(value.E, value.FromType, value.ToType);
        }

        public virtual Microsoft.Dafny.V2.LetOrFailExpr Transform(Microsoft.Dafny.LetOrFailExpr value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.NestedMatchCaseExpr Transform(Microsoft.Dafny.NestedMatchCaseExpr value)
        {
            return new Microsoft.Dafny.NestedMatchCaseExpr(value.Tok, value.Pat, value.Body, value.Attributes);
        }

        public virtual Microsoft.Dafny.NestedMatchExpr Transform(Microsoft.Dafny.NestedMatchExpr value)
        {
            return new Microsoft.Dafny.NestedMatchExpr(value.Tok, value.Source, value.Cases, value.UsesOptionalBraces, value.Attributes);
        }

        public virtual Microsoft.Dafny.TypeExpr Transform(Microsoft.Dafny.TypeExpr value)
        {
            return new Microsoft.Dafny.TypeExpr(value.Tok, value.E, value.T);
        }

        public virtual Microsoft.Dafny.ParensExpression Transform(Microsoft.Dafny.ParensExpression value)
        {
            return new Microsoft.Dafny.ParensExpression(value.Tok, value.E);
        }

        public virtual Microsoft.Dafny.V2.DatatypeUpdateExpr Transform(Microsoft.Dafny.DatatypeUpdateExpr value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.DefaultValueExpression Transform(Microsoft.Dafny.DefaultValueExpression value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.NegationExpression Transform(Microsoft.Dafny.NegationExpression value)
        {
            return new Microsoft.Dafny.NegationExpression(value.Tok, value.E);
        }

        public virtual Microsoft.Dafny.ChainingExpression Transform(Microsoft.Dafny.ChainingExpression value)
        {
            return new Microsoft.Dafny.ChainingExpression(value.Tok, value.Operands, value.Operators, value.OperatorLocs, value.PrefixLimits);
        }

        public virtual Microsoft.Dafny.ExprDotName Transform(Microsoft.Dafny.ExprDotName value)
        {
            return new Microsoft.Dafny.ExprDotName(value.Tok, value.Obj, value.SuffixName, value.OptTypeArguments);
        }

        public virtual Microsoft.Dafny.ApplySuffix Transform(Microsoft.Dafny.ApplySuffix value)
        {
            return new Microsoft.Dafny.ApplySuffix(value.Tok, value.AtLabel, value.Lhs, value.Args);
        }

        public virtual Microsoft.Dafny.NameSegment Transform(Microsoft.Dafny.NameSegment value)
        {
            return new Microsoft.Dafny.NameSegment(value.Tok, value.Name, value.OptTypeArguments);
        }

        public virtual Microsoft.Dafny.V2.DefaultModuleDecl Transform(Microsoft.Dafny.DefaultModuleDecl value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.BuiltIns Transform(Microsoft.Dafny.BuiltIns value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }

        public virtual Microsoft.Dafny.V2.Program Transform(Microsoft.Dafny.Program value)
        {
            throw new NotImplementedException("Mutated types do not have a default Transform implementation.");
        }
    }
}