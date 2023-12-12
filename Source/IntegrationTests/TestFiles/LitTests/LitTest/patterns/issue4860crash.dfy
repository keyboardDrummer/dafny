// RUN: %verify "%s" > "%t"
// RUN: %diff "%s.expect" "%t"

datatype Expr =
    | Const(value: int)
    | Var(name: string)
    | Add(left: Expr, right: Expr)
    | Mult(left: Expr, right: Expr)

function Eval(expr: Expr, env: map<string, int>): int
{
    match expr
        case Const(value) => value
        case Var(name) => if name in env then env[name] else 0
        case Add(left, right) => Eval(left, env) + Eval(right, env)
        case Mult(left, right) => Eval(left, env) * Eval(right, env)
}

function Optimize(expr: Expr): Expr
{
    match expr
        case Const(value) => Const(value)
        case Var(name) => Var(name)
        case Add(left, right) =>
            match (Optimize(left), Optimize(right))
                case (Const(lv), Const(rv)) => Const(lv + rv)
                case (Const(0), rv) => rv
                case (lv, Const(0)) => lv
                case (lv, rv) => Add(lv, rv)
        case Mult(left, right) =>
            match (Optimize(left), Optimize(right))
                case (Const(lv), Const(rv)) => Const(lv * rv)
                case (Const(0), _) => Const(0)
                case (_, Const(0)) => Const(0)
                case (Const(1), rv) => rv
                case (lv, Const(1)) => lv
                case (lv, rv) => Mult(lv, rv)
}

lemma Lemma_OptimizerPreservesSemantics(expr: Expr, env: map<string, int>)
    ensures Eval(expr, env) == Eval(Optimize(expr), env)
{
    match expr
        case Const(value) =>
            // For constants, the optimized expression is the same as the original.
        case Var(name) =>
            // For variables, the optimized expression is the same as the original.
        case Add(left, right) =>
            // Inductive step for addition.
            Lemma_OptimizerPreservesSemantics(left, env);
            Lemma_OptimizerPreservesSemantics(right, env);
        case Mult(left, right) =>
            // Inductive step for multiplication.
            Lemma_OptimizerPreservesSemantics(left, env);
            Lemma_OptimizerPreservesSemantics(right, env);
}