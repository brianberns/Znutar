namespace Znutar

module TypeInference =

    open Substitution

    let mutable count = 0

    let private fresh _ =
        count <- count + 1
        Identifier.create $"tv{count}" |> TypeVariable

    let private instantiate scheme =
        let types = List.map fresh scheme.TypeVariables
        let subst = Map (List.zip scheme.TypeVariables types)
        Type.apply subst scheme.Type

    let private generalize env typ =
        let tvs =
            Set.toList (
                Type.freeTypeVariables typ
                    - TypeEnvironment.freeTypeVariables env)
        Scheme.create tvs typ

    module private TypeEnvironment =

        let instantiate var (env : TypeEnvironment) =
            match Map.tryFind var env with
                | None ->
                    cerror (UnboundVariable var)
                | Some scheme ->
                    Ok (instantiate scheme)

    let private binOpMap =
        Map [
            Plus, Type.int => Type.int => Type.int
            Minus, Type.int => Type.int => Type.int
            Times, Type.int => Type.int => Type.int
            Equals, Type.int => Type.int => Type.bool   // to-do: make polymorphic
        ]

    let rec inferExpr env = function
        | VariableExpr var -> inferVar env var
        | LambdaExpr lam -> inferLambda env lam
        | ApplicationExpr app -> inferApplication env app
        | LetExpr letb -> inferLet env letb
        | IfExpr iff -> inferIf env iff
        | FixExpr expr -> inferFix env expr
        | BinaryOperationExpr bop -> inferBinOp env bop
        | LiteralExpr (IntLiteral _) ->
            Ok (Substitution.empty, Type.int)
        | LiteralExpr (BoolLiteral _) ->
            Ok (Substitution.empty, Type.bool)

    and private inferVar env var =
        result {
            let! typ = TypeEnvironment.instantiate var env
            return Substitution.empty, typ
        }

    and private inferLambda env lam =
        result {
            let freshType = fresh ()
            let env' =
                let scheme = Scheme.create [] freshType
                TypeEnvironment.add lam.Identifier scheme env
            let! bodySubst, bodyType = inferExpr env' lam.Body
            let freshType' = Type.apply bodySubst freshType
            return bodySubst, freshType' => bodyType
        }

    and private inferApplication env app =
        result {
            let freshType = fresh ()
            let! funSubst, funType = inferExpr env app.Function
            let! argSubst, argType =
                let env' = TypeEnvironment.apply funSubst env
                inferExpr env' app.Argument
            let! appSubst =
                let funType' = Type.apply argSubst funType
                unify funType' (argType => freshType)
            return
                funSubst ++ argSubst ++ appSubst,
                Type.apply appSubst freshType
        }

    and private inferLet env letb =
        result {
            let! argSubst, argType = inferExpr env letb.Argument
            let env' = TypeEnvironment.apply argSubst env
            let argType' = generalize env' argType
            let! bodySubst, bodyType =
                let env'' =
                    TypeEnvironment.add letb.Identifier argType' env'
                inferExpr env'' letb.Body
            return argSubst ++ bodySubst, bodyType
        }

    and private inferIf env iff =
        result {
            let! condSubst, condType = inferExpr env iff.Condition
            let! trueSubst, trueType = inferExpr env iff.TrueBranch
            let! falseSubst, falseType = inferExpr env iff.FalseBranch
            let! condSubst' = unify condType Type.bool
            let! branchSubst = unify trueType falseType
            return
                condSubst ++ trueSubst ++ falseSubst
                    ++ condSubst' ++ branchSubst,
                Type.apply branchSubst trueType
        }

    and private inferFix env expr =
        result {
            let! exprSubst, exprType = inferExpr env expr
            let freshType = fresh ()
            let! arrowSubst =
                unify (freshType => freshType) exprType
            return
                arrowSubst,
                Type.apply exprSubst freshType
        }

    and private inferBinOp env bop =
        result {
            let! leftSubst, leftType = inferExpr env bop.Left
            let! rightSubst, rightType = inferExpr env bop.Right
            let freshType = fresh ()
            let! arrowSubst =
                unify
                    (leftType => rightType => freshType)
                    binOpMap[bop.Operator]
            return
                leftSubst ++ rightSubst ++ arrowSubst,
                Type.apply arrowSubst freshType
        }

    let private inferDecl env decl =
        result {
            let! subst, typ = inferExpr env decl.Body
            let scheme =
                Type.apply subst typ
                    |> generalize TypeEnvironment.empty
            return TypeEnvironment.add decl.Identifier scheme env
        }

    let inferProgram program =
        result {
            let! env =
                Result.foldM
                    inferDecl
                    TypeEnvironment.empty
                    program.Declarations
            let! _subst, typ = inferExpr env program.Main
            return env, typ
        }
