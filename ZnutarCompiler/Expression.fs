﻿namespace Znutar

/// A literal value, such as 1 or true.
type Literal =
    | IntLiteral of int
    | BoolLiteral of bool

/// Binary operator.
type BinaryOperator =
    | Plus | Minus | Times
    | Equals | GreaterThan | LessThan
    | Divide | Modulo

module BinaryOperator =

    /// Unparses the given operator.
    let unparse = function
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
        | Equals -> "="
        | GreaterThan -> ">"
        | LessThan -> "<"
        | Divide -> "/"
        | Modulo -> "%"

[<System.Diagnostics.DebuggerDisplay("{Unparse()}")>]
type Expression =
    | VariableExpr of Identifier
    | ApplicationExpr of Application
    | LambdaExpr of LambdaAbstraction
    | LetExpr of LetBinding
    | LiteralExpr of Literal
    | IfExpr of If
    | BinaryOperationExpr of BinaryOperation
    | AnnotationExpr of Annotation
    | MemberAccessExpr of MemberAccess

    with

    /// Unparses the given expression.
    member expr.Unparse() =
        match expr with
            | VariableExpr ident ->
                ident.Name
            | ApplicationExpr app ->
                $"({app.Function.Unparse()} {app.Argument.Unparse()})"
            | LambdaExpr lam ->
                $"(fun {lam.Identifier.Name} -> \
                    {lam.Body.Unparse()})"
            | LetExpr letb ->
                let sRec = if letb.Recursive then "rec " else ""
                $"(let {sRec}{letb.Identifier.Name} = \
                    {letb.Argument.Unparse()} in \
                    {letb.Body.Unparse()})"
            | LiteralExpr (IntLiteral n) -> string n
            | LiteralExpr (BoolLiteral b) ->
                if b then "true" else "false"
            | IfExpr iff ->
                $"(if {iff.Condition.Unparse()} \
                    then {iff.TrueBranch.Unparse()} \
                    else {iff.FalseBranch.Unparse()})"
            | BinaryOperationExpr bop ->
                $"({bop.Left.Unparse()} \
                    {BinaryOperator.unparse bop.Operator} \
                    {bop.Right.Unparse()})"
            | AnnotationExpr ann ->
                $"({ann.Expression.Unparse()} : {ann.Type.Unparse()})"
            | MemberAccessExpr ma ->
                $"{ma.Expression.Unparse()}.{ma.Identifier.Name}"

/// func arg
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

/// let [rec] ident = arg in body
and LetBinding =
    {
        Recursive : bool
        Identifier : Identifier
        Argument : Expression
        Body : Expression
    }

/// if cond then true-branch else false-branch
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

/// expr.ident
and MemberAccess =
    {
        Expression : Expression
        Identifier : Identifier
    }

module Expression =

    /// Unparses the given expression.
    let rec unparse (expr : Expression) =
        expr.Unparse()
