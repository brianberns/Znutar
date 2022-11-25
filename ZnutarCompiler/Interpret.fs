﻿namespace Znutar

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

module Interpret =

    let private evalBinOp op xValue yValue =
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
                    return! evalBinOp
                        bop.Operator
                        leftVal
                        rightVal
                | LambdaExpr lam ->
                    return ClosureValue {
                        Lambda = lam
                        Environment = env
                    }
                | ApplicationExpr app ->
                    match! evalExpr env app.Function with
                        | ClosureValue clo ->
                            let! argVal = evalExpr env app.Argument
                            let env = Map.add clo.Lambda.Identifier argVal clo.Environment
                            return! evalExpr env clo.Lambda.Body
                        | _ -> return! Error $"Invalid application: {app}"
                | LetExpr letb ->
                    let! exprVal = evalExpr env letb.Argument
                    let env' = Map.add letb.Identifier exprVal env
                    return! evalExpr env' letb.Body
                | IfExpr iff ->
                    let! condVal = evalExpr env iff.Condition
                    match condVal with
                        | BoolValue true ->
                            return! evalExpr env iff.TrueBranch
                        | BoolValue false ->
                            return! evalExpr env iff.FalseBranch
                        | _ ->
                            return! Error $"Invalid condition value: {condVal}"
                | FixExpr fix ->
                    let expr =
                        ApplicationExpr {
                            Function = expr
                            Argument = FixExpr expr
                        }
                    return! evalExpr env expr
        }

    let private evalDecl env decl =
        result {
            let! declVal = evalExpr env decl.Body
            return Map.add decl.Identifier declVal env
        }

    let evalProgram program =
        result {
            let! env =
                Result.foldM
                    evalDecl
                    Map.empty
                    program.Declarations
            return! evalExpr env program.Main
        }