namespace Znutar

type Value =
    | IntValue of int
    | BoolValue of bool
    | ClosureValue of Closure
    
and Closure =
    {
        Lambda : LambdaAbstraction
        Environment : TermEnvironment
    }

and TermEnvironment = Map<Identifier, Value>

module Eval =

    let private evalBinaryOperation op xValue yValue =
        match op, xValue, yValue with
            | Plus, IntValue x, IntValue y -> Ok (IntValue (x + y))
            | Minus, IntValue x, IntValue y -> Ok (IntValue (x - y))
            | Times, IntValue x, IntValue y -> Ok (IntValue (x * y))
            | Equals, IntValue x, IntValue y -> Ok (BoolValue (x = y))
            | _ -> Error $"Invalid operation: {xValue} {op} {yValue}"

    let rec evalExpr env expr =
        result {
            match expr with
                | LiteralExpr (IntLiteral n) ->
                    return IntValue n
                | LiteralExpr (BoolLiteral b) ->
                    return BoolValue b
                | VariableExpr var ->
                    match Map.tryFind var env with
                        | Some value ->
                            return value
                        | None ->
                            return! Error $"No such variable: {var}"
                | BinaryOperationExpr bop ->
                    let! leftVal = evalExpr env bop.Left
                    let! rightVal = evalExpr env bop.Right
                    return! evalBinaryOperation
                        bop.Operator
                        leftVal
                        rightVal
                | LambdaExpr lam ->
                    return ClosureValue {
                        Lambda = lam
                        Environment = env
                    }
        }
