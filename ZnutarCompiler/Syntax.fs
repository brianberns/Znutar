namespace Znutar

type Identifier = string

type Literal =
    | IntLiteral of int
    | BoolLiteral of bool

type Binop =
    | Add | Sub | Mul | Eql

type Variable = string

type Expression =
    | VariableExpr of Variable
    | ApplicationExpr of Expression * Expression
    | LambdaExpr of Identifier * Expression
    | LetExpr of Identifier * Expression * Expression
    | LiteralExpr of Literal
    | IfExpr of Expression * Expression * Expression
    | FixExpr of Expression
    | BinaryOperationExpr of Binop * Expression * Expression

// let add x y = x + y; => let add = \x -> \y -> x + y;
type Function = Identifier * Expression

type Program = Program of List<Function> * Expression
