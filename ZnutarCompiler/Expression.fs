namespace Znutar

/// A literal value, such as 1 or true.
type Literal =

    /// E.g. true.
    | BoolLiteral of bool

    /// E.g. 1.
    | IntLiteral of int

    /// E.g. "Hello world".
    | StringLiteral of char[]   // prevent null

    /// E.g. ().
    | UnitLiteral

module Literal =

    open System

    /// Unparses a literal.
    let unparse = function
        | BoolLiteral b ->
            if b then "true" else "false"
        | IntLiteral n -> string n
        | StringLiteral chars ->
            let str =
                chars
                    |> Seq.map (function
                        | '"' -> "\\\""   // backslash, quote
                        | '\\' -> "\\\\"  // backslash, backslash
                        | c ->
                            if Char.IsControl(c) then
                                let str = (int c).ToString("x4")
                                $"\u{str}"
                            else string c)
                    |> String.concat ""
            $"\"{str}\""
        | UnitLiteral -> "()"

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

/// Abstract syntax tree for an expression.
[<System.Diagnostics.DebuggerDisplay("{Unparse()}")>]
type Expression =
    | IdentifierExpr of Identifier
    | ApplicationExpr of Application
    | LambdaExpr of LambdaAbstraction
    | LetExpr of LetBinding
    | LiteralExpr of Literal
    | IfExpr of If
    | BinaryOperationExpr of BinaryOperation
    | AnnotationExpr of Annotation
    | MemberAccessExpr of MemberAccess
    | TupleExpr of MultiItemList<Expression>

    /// Unparses the given expression.
    member expr.Unparse() =
        match expr with
            | IdentifierExpr ident ->
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
            | LiteralExpr literal -> Literal.unparse literal
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
            | TupleExpr exprs ->
                let str =
                    exprs
                        |> Seq.map (fun expr -> expr.Unparse())
                        |> String.concat ", "
                $"({str})"

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
