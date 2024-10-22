using System;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.Linq;

namespace Microsoft.Dafny;

public abstract class QuantifierExpr : ComprehensionExpr, TypeParameter.ParentType {
  public override string WhatKind => "quantifier";

  private readonly int UniqueId;
  private static int currentQuantId = -1;

  protected virtual BinaryExpr.ResolvedOpcode SplitResolvedOp => BinaryExpr.ResolvedOpcode.Or;

  private Expression SplitQuantifierToExpression() {
    Contract.Requires(SplitQuantifier != null && SplitQuantifier.Any());
    Expression accumulator = SplitQuantifier[0];
    for (int tid = 1; tid < SplitQuantifier.Count; tid++) {
      accumulator = new BinaryExpr(Term.tok, SplitResolvedOp, accumulator, SplitQuantifier[tid]);
    }
    return accumulator;
  }

  private List<Expression> _SplitQuantifier;
  public List<Expression> SplitQuantifier {
    get {
      return _SplitQuantifier;
    }
    set {
      Contract.Assert(!value.Contains(this)); // don't let it put into its own split quantifiers.
      _SplitQuantifier = value;
      SplitQuantifierExpression = SplitQuantifierToExpression();
    }
  }

  internal Expression SplitQuantifierExpression { get; private set; }

  static int FreshQuantId() {
    return System.Threading.Interlocked.Increment(ref currentQuantId);
  }

  public string FullName {
    get {
      return "q$" + UniqueId;
    }
  }

  public String Refresh(string prefix, FreshIdGenerator idGen) {
    return idGen.FreshId(prefix);
  }

  public QuantifierExpr(IToken tok, RangeToken rangeToken, List<BoundVar> bvars, Expression range, Expression term, Attributes attrs)
    : base(tok, rangeToken, bvars, range, term, attrs) {
    Contract.Requires(tok != null);
    Contract.Requires(cce.NonNullElements(bvars));
    Contract.Requires(term != null);
    UniqueId = FreshQuantId();
  }

  protected QuantifierExpr(Cloner cloner, QuantifierExpr original) : base(cloner, original) {
    if (cloner.CloneResolvedFields) {
      if (original.SplitQuantifier != null) {
        SplitQuantifier = original.SplitQuantifier.Select(cloner.CloneExpr).ToList();
      }
    }
    UniqueId = FreshQuantId();
  }

  public virtual Expression LogicalBody(bool bypassSplitQuantifier = false) {
    // Don't call this on a quantifier with a Split clause: it's not a real quantifier. The only exception is the Compiler.
    Contract.Requires(bypassSplitQuantifier || SplitQuantifier == null);
    throw new cce.UnreachableException(); // This body is just here for the "Requires" clause
  }

  public override IEnumerable<INode> PreResolveChildren => base.SubExpressions;
  public IEnumerable<Expression> PreResolveSubExpressions => base.SubExpressions;

  public override IEnumerable<Expression> SubExpressions {
    get {
      foreach (var e in base.SubExpressions) {
        yield return e;
      }
      foreach (var e in Attributes.SubExpressions(Attributes)) {
        yield return e;
      }

      if (SplitQuantifier != null) {
        foreach (var e in SplitQuantifier) {
          yield return e;
        }
      }
    }
  }

  public IEnumerable<Expression> SplitOrSubExpressions {
    get {
      foreach (var e in Attributes.SubExpressions(Attributes)) {
        yield return e;
      }
      if (SplitQuantifier == null) {
        foreach (var e in base.SubExpressions) {
          yield return e;
        }
      } else {
        foreach (var e in SplitQuantifier) {
          yield return e;
        }
      }
    }
  }

  public void OldResolve(ModuleResolver resolver, ResolutionContext context) {
    Contract.Assert(SplitQuantifier == null); // No split quantifiers during resolution
    resolver.Scope.PushMarker();
    foreach (BoundVar v in BoundVars) {
      resolver.ScopePushAndReport(resolver.Scope, v, "bound-variable");
      var option = new ModuleResolver.ResolveTypeOption(ResolveTypeOptionEnum.InferTypeProxies);
      resolver.ResolveType(v.tok, v.Type, context, option, null);
    }
    if (Range != null) {
      resolver.ResolveExpression(Range, context);
      Contract.Assert(Range.Type != null);  // follows from postcondition of ResolveExpression
      resolver.ConstrainTypeExprBool(Range, "range of quantifier must be of type bool (instead got {0})");
    }
    resolver.ResolveExpression(Term, context);
    Contract.Assert(Term.Type != null);  // follows from postcondition of ResolveExpression
    resolver.ConstrainTypeExprBool(Term, "body of quantifier must be of type bool (instead got {0})");
    // Since the body is more likely to infer the types of the bound variables, resolve it
    // first (above) and only then resolve the attributes (below).
    resolver.ResolveAttributes(this, context);
    resolver.Scope.PopMarker();
    Type = Type.Bool;
  }

  public void NewResolve(PreTypeResolver resolver, ResolutionContext resolutionContext) {
    if (resolutionContext.CodeContext is Function enclosingFunction) {
      enclosingFunction.ContainsQuantifier = true;
    }
    Contract.Assert(SplitQuantifier == null); // No split quantifiers during resolution
    resolver.Scope.PushMarker();
    foreach (var v in BoundVars) {
      resolver.resolver.ResolveType(v.tok, v.Type, resolutionContext, ResolveTypeOptionEnum.InferTypeProxies, null);
      resolver.ScopePushAndReport(v, "bound-variable", true);
    }
    if (Range != null) {
      resolver.ResolveExpression(Range, resolutionContext);
      resolver.ConstrainTypeExprBool(Range, "range of quantifier must be of type bool (instead got {0})");
    }
    resolver.ResolveExpression(Term, resolutionContext);
    resolver.ConstrainTypeExprBool(Term, "body of quantifier must be of type bool (instead got {0})");
    // Since the body is more likely to infer the types of the bound variables, resolve it
    // first (above) and only then resolve the attributes (below).
    resolver.ResolveAttributes(this, resolutionContext, false);
    resolver.Scope.PopMarker();
    PreType = resolver.ConstrainResultToBoolFamilyOperator(tok, WhatKind);
  }
}
