namespace Znutar

type Identifier = string

type Literal =
    | IntLiteral of int
    | BoolLiteral of bool

type BinaryOperator =
    | Plus | Minus | Times | Equals

type Variable = string

type Expression =
    | VariableExpr of Variable
    | ApplicationExpr of Application
    | LambdaExpr of LambdaAbstraction
    | LetExpr of LetBinding
    | LiteralExpr of Literal
    | IfExpr of If
    | FixExpr of Expression
    | BinaryOperationExpr of BinaryOperation

and Application =
    {
        Function : Expression
        Argument : Expression
    }

and LambdaAbstraction =
    {
        Identifier : Identifier
        Body : Expression
    }

and LetBinding =
    {
        Identifier : Identifier
        Argument : Expression
        Body : Expression
    }

and If =
    {
        Condition : Expression
        TrueBranch : Expression
        FalseBranch : Expression
    }

and BinaryOperation =
    {
        Operator : BinaryOperator
        Left : Expression
        Right : Expression
    }

/// Top-level declaration.
/// E.g. let add x y = x + y => let add = \x -> \y -> x + y
type Declaration =
    {
        Identifier : Identifier
        Body : Expression
    }

type Program =
    {
        Declarations : List<Declaration>
        Main : Expression
    }
