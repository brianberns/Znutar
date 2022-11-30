namespace Znutar

type Literal =
    | IntLiteral of int
    | BoolLiteral of bool

type BinaryOperator =
    | Plus | Minus | Times | Equals

module BinaryOperator =

    let unparse = function
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
        | Equals -> "="

[<System.Diagnostics.DebuggerDisplay("{Unparse()}")>]
type Expression =
    | VariableExpr of Identifier
    | ApplicationExpr of Application
    | LambdaExpr of LambdaAbstraction
    | LetExpr of LetBinding
    | LiteralExpr of Literal
    | IfExpr of If
    | FixExpr of Expression
    | BinaryOperationExpr of BinaryOperation
    | AnnotationExpr of Annotation

    with
    member expr.Unparse() =
        match expr with
            | VariableExpr ident ->
                Identifier.unparse ident
            | ApplicationExpr app ->
                $"({app.Function.Unparse()} {app.Argument.Unparse()})"
            | LambdaExpr lam ->
                $"(fun {Identifier.unparse lam.Identifier} -> \
                    {lam.Body.Unparse()})"
            | LetExpr letb ->
                $"(let {Identifier.unparse letb.Identifier} = \
                    {letb.Argument.Unparse()} in \
                    {letb.Body.Unparse()})"
            | LiteralExpr (IntLiteral n) -> string n
            | LiteralExpr (BoolLiteral b) ->
                if b then "true" else "false"
            | IfExpr iff ->
                $"(if {iff.Condition.Unparse()} \
                    then {iff.TrueBranch.Unparse()} \
                    else {iff.FalseBranch.Unparse()})"
            | FixExpr expr ->
                $"(fix {expr.Unparse()})"
            | BinaryOperationExpr bop ->
                $"({bop.Left.Unparse()} \
                    {BinaryOperator.unparse bop.Operator} \
                    {bop.Right.Unparse()})"
            | AnnotationExpr ann ->
                $"({ann.Expression.Unparse()} : {ann.Type.Unparse()})"

/// fun arg
and Application =
    {
        Function : Expression
        Argument : Expression
    }

/// fun ident -> body
and LambdaAbstraction =
    {
        Identifier : Identifier
        Body : Expression
    }

/// let ident = arg in body
and LetBinding =
    {
        Identifier : Identifier
        Argument : Expression
        Body : Expression
    }

/// if cond then t-branch else f-branch
and If =
    {
        Condition : Expression
        TrueBranch : Expression
        FalseBranch : Expression
    }

/// left op right
and BinaryOperation =
    {
        Operator : BinaryOperator
        Left : Expression
        Right : Expression
    }

/// expr : type
and Annotation =
    {
        Expression : Expression
        Type : Type
    }

module Expression =

    let rec unparse (expr : Expression) =
        expr.Unparse()

/// Top-level declaration.
/// E.g. let add x y = x + y => let add = \x -> \y -> x + y
type Declaration =
    {
        Identifier : Identifier
        Body : Expression
    }

module Declaration =

    let unparse decl =
        let ident = Identifier.unparse decl.Identifier
        let body = Expression.unparse decl.Body
        $"decl {ident} = {body};"

type Program =
    {
        Declarations : List<Declaration>
        Main : Expression
    }

module Program =

    let unparse program =
        let decls =
            program.Declarations
                |> Seq.map Declaration.unparse
                |> String.concat "\n\n"
        let main = Expression.unparse program.Main
        $"{decls}\n\n{main}"
