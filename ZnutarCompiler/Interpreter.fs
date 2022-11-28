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

type InvalidBinaryOperation =
    InvalidBinaryOperation of BinaryOperator * Value * Value
    with interface ICompilerError

module InvalidBinaryOperation =

    let error op leftVal rightVal =
        (op, leftVal, rightVal)
            |> InvalidBinaryOperation
            |> cerror

type UnboundVariable = UnboundVariable of Identifier
    with interface ICompilerError

type InvalidApplication =
    InvalidApplication of Application
    with interface ICompilerError

type InvalidConditionValue =
    InvalidConditionValue of Value
    with interface ICompilerError

module Interpreter =

    let private evalBinOp op leftVal rightVal =
        match op, leftVal, rightVal with
            | Plus, IntValue x, IntValue y -> Ok (IntValue (x + y))
            | Minus, IntValue x, IntValue y -> Ok (IntValue (x - y))
            | Times, IntValue x, IntValue y -> Ok (IntValue (x * y))
            | Equals, IntValue x, IntValue y -> Ok (BoolValue (x = y))
            | _ -> InvalidBinaryOperation.error op leftVal rightVal

    let rec evalExpr env expr =
        result {
            match expr with

                | LiteralExpr (IntLiteral n) -> return IntValue n
                | LiteralExpr (BoolLiteral b) -> return BoolValue b

                | VariableExpr ident ->
                    match Map.tryFind ident env with
                        | Some value ->
                            return value
                        | None ->
                            return! cerror (UnboundVariable ident)

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
                            let cloEnv =
                                Map.add
                                    clo.Lambda.Identifier
                                    argVal clo.Environment
                            return! evalExpr cloEnv clo.Lambda.Body
                        | _ -> return! cerror (InvalidApplication app)

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
                            return! cerror (InvalidConditionValue condVal)

                    // fix f = \x -> f (fix f) x
                    // https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_functional_implementation
                | FixExpr f ->
                    let lamExpr =
                        let ident = Identifier.create "$x"
                        LambdaExpr {
                            Identifier = ident
                            Body =
                                ApplicationExpr {
                                    Function =
                                        ApplicationExpr {
                                            Function = f
                                            Argument = FixExpr f
                                        }
                                    Argument = VariableExpr ident
                                }
                        }
                    return! evalExpr env lamExpr
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
