﻿namespace Znutar.TypeInference

open Znutar

/// An expression annotated with its inferred type.
[<System.Diagnostics.DebuggerDisplay("{Unparse()}")>]
type AnnotatedExpression =
    | IdentifierExpr of AnnotatedIdentifier
    | ApplicationExpr of AnnotatedApplication
    | LambdaExpr of AnnotatedLambdaAbstraction
    | LetExpr of AnnotatedLetBinding
    | LiteralExpr of Literal
    | IfExpr of AnnotatedIf
    | BinaryOperationExpr of AnnotatedBinaryOperation
    | MemberAccessExpr of AnnotatedMemberAccess

    with

    /// Result type.
    member annex.Type =
        match annex with
            | IdentifierExpr ai -> ai.Type
            | ApplicationExpr app -> app.Type
            | LambdaExpr lam -> lam.Type
            | LetExpr letb -> letb.Type
            | LiteralExpr (IntLiteral n) -> Type.int
            | LiteralExpr (BoolLiteral b) -> Type.bool
            | IfExpr iff -> iff.Type
            | BinaryOperationExpr bop -> bop.Type
            | MemberAccessExpr ma -> ma.Type

    member annex.Unparse() =
        match annex with
            | IdentifierExpr ai ->
                ai.Identifier.Name
            | ApplicationExpr app ->
                $"({app.Function.Unparse()} {app.Argument.Unparse()})"
            | LambdaExpr lam ->
                $"(fun {lam.Identifier.Name} -> \
                    {lam.Body.Unparse()})"
            | LetExpr letb ->
                $"(let {letb.Identifier.Name} = \
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
            | MemberAccessExpr ma ->
                $"({ma.Expression}).{ma.Identifier}"

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
        Expression : AnnotatedExpression
        Identifier : Identifier

        /// Result type. E.g. (expr.ident : int).
        Type : Type
    }

module AnnotatedExpression =

    let rec unparse (annex : AnnotatedExpression) =
        annex.Unparse()

/// Top-level declaration.
/// E.g. let add x y = x + y ^=> let add = \x -> \y -> x + y
type AnnotatedDeclaration =
    {
        Identifier : Identifier
        Body : AnnotatedExpression
    }

module AnnotatedDeclaration =

    let unparse decl =
        let ident = decl.Identifier.Name
        let body = AnnotatedExpression.unparse decl.Body
        $"decl {ident} = {body};"

type AnnotatedProgram =
    {
        Declarations : List<AnnotatedDeclaration>
        Main : AnnotatedExpression
    }

module AnnotatedProgram =

    let unparse program =
        let decls =
            program.Declarations
                |> Seq.map AnnotatedDeclaration.unparse
                |> String.concat "\n\n"
        let main = AnnotatedExpression.unparse program.Main
        $"{decls}\n\n{main}"
