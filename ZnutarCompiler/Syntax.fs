namespace Znutar

type Identifier = string

type Literal =
    | IntLiteral of int
    | BoolLiteral of bool

type BinaryOperator =
    | Add | Sub | Mul | Eql

type Variable = string

type Expression =
    | VariableExpr of Variable
    | ApplicationExpr of Application
    | LambdaExpr of LambdaAbstraction
    | LetExpr of LetBinding
    | LiteralExpr of Literal
    | IfExpr of If
    | FixExpr of Expression
    | BinaryOperationExpr of BinaryOperator * Expression * Expression

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

// let add x y = x + y; => let add = \x -> \y -> x + y;
type Declaration = Identifier * Expression

type Program = Program of List<Declaration> * Expression
