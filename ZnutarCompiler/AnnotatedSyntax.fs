namespace Znutar

[<System.Diagnostics.DebuggerDisplay("{Unparse()}")>]
type AnnotatedExpression =
    | VariableExpr of AnnotatedVariable
    | ApplicationExpr of AnnotatedApplication
    | LambdaExpr of AnnotatedLambdaAbstraction
    | LetExpr of AnnotatedLetBinding
    | LiteralExpr of Literal
    | IfExpr of AnnotatedIf
    | FixExpr of AnnotatedFix
    | BinaryOperationExpr of AnnotatedBinaryOperation
    | FunctionExpr of Function

    with
    member annex.Type =
        match annex with
            | VariableExpr var -> var.Type
            | ApplicationExpr app -> app.Type
            | LambdaExpr lam -> lam.Type
            | LetExpr letb -> letb.Type
            | LiteralExpr (IntLiteral n) -> Type.int
            | LiteralExpr (BoolLiteral b) -> Type.bool
            | IfExpr iff -> iff.Type
            | FixExpr fix -> fix.Type
            | BinaryOperationExpr bop -> bop.Type

    member annex.Unparse() =
        match annex with
            | VariableExpr var ->
                var.Identifier.Name
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
            | FixExpr fix ->
                $"(fix {fix.Expression.Unparse()})"
            | BinaryOperationExpr bop ->
                $"({bop.Left.Unparse()} \
                    {BinaryOperator.unparse bop.Operator} \
                    {bop.Right.Unparse()})"

/// x
and AnnotatedVariable =
    {
        Identifier : Identifier
        Type : Type
    }

/// func arg
and AnnotatedApplication =
    {
        Function : AnnotatedExpression
        Argument : AnnotatedExpression
        Type : Type
    }

/// fun ident -> body
and AnnotatedLambdaAbstraction =
    {
        Identifier : Identifier
        Body : AnnotatedExpression
        Type : Type
    }

/// let ident = arg in body
and AnnotatedLetBinding =
    {
        Identifier : Identifier
        Argument : AnnotatedExpression
        Body : AnnotatedExpression
        Type : Type
    }

/// if cond then true-branch else false-branch
and AnnotatedIf =
    {
        Condition : AnnotatedExpression
        TrueBranch : AnnotatedExpression
        FalseBranch : AnnotatedExpression
        Type : Type
    }

and AnnotatedFix =
    {
        Expression : AnnotatedExpression
        Type : Type
    }

/// left op right
and AnnotatedBinaryOperation =
    {
        Operator : BinaryOperator
        Left : AnnotatedExpression
        Right : AnnotatedExpression
        Type : Type
    }

/// let const x y = x in next
and Function =
    {
        /// E.g. "const"
        Identifier : Identifier

        /// E.g. [x; y]
        Arguments : List<Identifier>

        /// E.g. x
        FunctionBody : AnnotatedExpression

        /// E.g. <'a, 'b>('a -> 'b -> 'a)
        Scheme : Scheme

        /// E.g. next
        ExpressionBody : AnnotatedExpression
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
