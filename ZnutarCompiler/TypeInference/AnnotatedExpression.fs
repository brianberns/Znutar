namespace Znutar.TypeInference

open Znutar

/// An expression annotated with its inferred type.
[<System.Diagnostics.DebuggerDisplay("{Unparse()}")>]
type AnnotatedExpression =
    | AnnotatedIdentifierExpr of AnnotatedIdentifier
    | AnnotatedApplicationExpr of AnnotatedApplication
    | AnnotatedLambdaExpr of AnnotatedLambdaAbstraction
    | AnnotatedLetExpr of AnnotatedLetBinding
    | AnnotatedLiteralExpr of Literal
    | AnnotatedIfExpr of AnnotatedIf
    | AnnotatedBinaryOperationExpr of AnnotatedBinaryOperation
    | AnnotatedMemberAccessExpr of AnnotatedMemberAccess
    | AnnotatedTupleExpr of AnnotatedTuple

    /// Result type.
    member annex.Type =
        match annex with
            | AnnotatedIdentifierExpr ai -> ai.Type
            | AnnotatedApplicationExpr app -> app.Type
            | AnnotatedLambdaExpr lam -> lam.Type
            | AnnotatedLetExpr letb -> letb.Type
            | AnnotatedLiteralExpr (IntLiteral _) -> Type.int
            | AnnotatedLiteralExpr (BoolLiteral _) -> Type.bool
            | AnnotatedLiteralExpr (StringLiteral _) -> Type.string
            | AnnotatedLiteralExpr UnitLiteral -> Type.unit
            | AnnotatedIfExpr iff -> iff.Type
            | AnnotatedBinaryOperationExpr bop -> bop.Type
            | AnnotatedMemberAccessExpr ma -> ma.Type
            | AnnotatedTupleExpr tuple -> tuple.Type

    member annex.Unparse() =
        match annex with
            | AnnotatedIdentifierExpr ai ->
                ai.Identifier.Name
            | AnnotatedApplicationExpr app ->
                $"({app.Function.Unparse()} {app.Argument.Unparse()})"
            | AnnotatedLambdaExpr lam ->
                $"(fun {lam.Identifier.Name} -> \
                    {lam.Body.Unparse()})"
            | AnnotatedLetExpr letb ->
                $"(let {letb.Identifier.Name} = \
                    {letb.Argument.Unparse()} in \
                    {letb.Body.Unparse()})"
            | AnnotatedLiteralExpr literal -> Literal.unparse literal
            | AnnotatedIfExpr iff ->
                $"(if {iff.Condition.Unparse()} \
                    then {iff.TrueBranch.Unparse()} \
                    else {iff.FalseBranch.Unparse()})"
            | AnnotatedBinaryOperationExpr bop ->
                $"({bop.Left.Unparse()} \
                    {BinaryOperator.unparse bop.Operator} \
                    {bop.Right.Unparse()})"
            | AnnotatedMemberAccessExpr ma ->
                $"({ma.MemberAccess.Expression.Unparse()}).{ma.MemberAccess.Identifier.Name}"
            | AnnotatedTupleExpr tuple ->
                let str =
                    tuple.Expressions
                        |> Seq.map (fun expr -> expr.Unparse())
                        |> String.concat ", "
                $"({str})"

/// x
and AnnotatedIdentifier =
    {
        Identifier : Identifier

        /// Result type. E.g. (x : int).
        Type : Type
    }

/// func arg
and AnnotatedApplication =
    {
        Function : AnnotatedExpression
        Argument : AnnotatedExpression

        /// Result type. E.g. ((f x) : int).
        Type : Type
    }

/// fun ident -> body
and AnnotatedLambdaAbstraction =
    {
        Identifier : Identifier
        Body : AnnotatedExpression

        /// Result type. E.g. ((fun x -> x + 1) : int -> int).
        Type : Type
    }

/// let ident = arg in body
and AnnotatedLetBinding =
    {
        Identifier : Identifier

        /// E.g. <'a, 'b>('a -> 'b -> 'a).
        Scheme : Scheme

        Argument : AnnotatedExpression
        Body : AnnotatedExpression

        /// Result type. E.g. ((let x = 1 in x + 1) : int).
        Type : Type
    }

/// if cond then true-branch else false-branch
and AnnotatedIf =
    {
        Condition : AnnotatedExpression
        TrueBranch : AnnotatedExpression
        FalseBranch : AnnotatedExpression

        /// Result type. E.g. ((if flag then 1 else 0) : int).
        Type : Type
    }

/// left op right
and AnnotatedBinaryOperation =
    {
        Operator : BinaryOperator
        Left : AnnotatedExpression
        Right : AnnotatedExpression

        /// Result type. E.g. ((1 = 1) : bool).
        Type : Type
    }

/// expr.ident
and AnnotatedMemberAccess =
    {
        // Expression : AnnotatedExpression
        // Identifier : Identifier
        MemberAccess : MemberAccess   // to-do: sub-expressions should be annotated

        /// Result type. E.g. (expr.ident : int).
        Type : Type
        IsConstructor : bool
    }

/// a, b
and AnnotatedTuple =
    {
        Expressions : MultiItemList<AnnotatedExpression>

        /// Result type. E.g. int * string.
        Type : Type
    }

module AnnotatedExpression =

    let rec unparse (annex : AnnotatedExpression) =
        annex.Unparse()

    let rec apply subst annex =

        let loop = apply subst
        let tapply = Substitution.Type.apply subst
        let sapply = Substitution.Scheme.apply subst

        match annex with
            | AnnotatedIdentifierExpr ai ->
                AnnotatedIdentifierExpr { ai with Type = tapply ai.Type }
            | AnnotatedApplicationExpr app ->
                AnnotatedApplicationExpr {
                    Function = loop app.Function
                    Argument = loop app.Argument
                    Type = tapply app.Type
                }
            | AnnotatedLambdaExpr lam ->
                AnnotatedLambdaExpr {
                    lam with
                        Body = loop lam.Body
                        Type = tapply lam.Type
                }
            | AnnotatedLetExpr letb ->
                AnnotatedLetExpr {
                    letb with
                        Scheme = sapply letb.Scheme
                        Argument = loop letb.Argument
                        Body = loop letb.Body
                        Type = tapply letb.Type
                }
            | AnnotatedLiteralExpr _ -> annex
            | AnnotatedIfExpr iff ->
                AnnotatedIfExpr {
                    Condition = loop iff.Condition
                    TrueBranch = loop iff.TrueBranch
                    FalseBranch = loop iff.FalseBranch
                    Type = tapply iff.Type
                }
            | AnnotatedBinaryOperationExpr bop ->
                AnnotatedBinaryOperationExpr {
                    bop with
                        Left = loop bop.Left
                        Right = loop bop.Right
                        Type = tapply bop.Type
                }
            | AnnotatedMemberAccessExpr ma ->
                AnnotatedMemberAccessExpr {
                    ma with
                        Type = tapply ma.Type
                }
            | AnnotatedTupleExpr tuple ->
                AnnotatedTupleExpr {
                    Expressions =
                        MultiItemList.map loop tuple.Expressions
                    Type = tapply tuple.Type
                }
