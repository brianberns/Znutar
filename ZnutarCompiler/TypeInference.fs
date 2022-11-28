namespace Znutar

module TypeInference =

    open Substitution

    let mutable count = 0

    let fresh _ =
        count <- count + 1
        Identifier.create $"tv{count}" |> TypeVariable

    let instantiate scheme =
        let types = List.map fresh scheme.TypeVariables
        let subst = Map (List.zip scheme.TypeVariables types)
        Type.apply subst scheme.Type

    let generalize env typ =
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

    let rec infer env expr =
        result {
            match expr with

                | VariableExpr var ->
                    let! typ = TypeEnvironment.instantiate var env
                    return Substitution.empty, typ

                | LambdaExpr lam ->
                    let freshType = fresh ()
                    let env' =
                        let scheme = Scheme.create [] freshType
                        TypeEnvironment.add lam.Identifier scheme env
                    let! bodySubst, bodyType = infer env' lam.Body
                    let freshType' = Type.apply bodySubst freshType
                    return bodySubst, freshType' => bodyType

                | ApplicationExpr app ->
                    let freshType = fresh ()
                    let! funSubst, funType = infer env app.Function
                    let! argSubst, argType =
                        let env' = TypeEnvironment.apply funSubst env
                        infer env' app.Argument
                    let! appSubst =
                        let funType' = Type.apply argSubst funType
                        unify funType' (argType => freshType)
                    return
                        funSubst ++ argSubst ++ appSubst,
                        Type.apply appSubst freshType

                | LetExpr letb ->
                    let! argSubst, argType = infer env letb.Argument
                    let env' = TypeEnvironment.apply argSubst env
                    let argType' = generalize env' argType
                    let! bodySubst, bodyType =
                        let env'' =
                            TypeEnvironment.add letb.Identifier argType' env'
                        infer env'' letb.Body
                    return argSubst ++ bodySubst, bodyType

                | IfExpr iff ->
                    let! condSubst, condType = infer env iff.Condition
                    let! trueSubst, trueType = infer env iff.TrueBranch
                    let! falseSubst, falseType = infer env iff.FalseBranch
                    let! condSubst' = unify condType Type.bool
                    let! branchSubst = unify trueType falseType
                    return
                        condSubst ++ trueSubst ++ falseSubst ++ condSubst' ++ branchSubst,
                        Type.apply branchSubst trueType

                | FixExpr expr ->
                    let! exprSubst, exprType = infer env expr
                    let freshType = fresh ()
                    let! arrowSubst =
                        unify (freshType => freshType) exprType
                    return
                        arrowSubst,
                        Type.apply exprSubst freshType

                | BinaryOperationExpr bop ->
                    let! leftSubst, leftType = infer env bop.Left
                    let! rightSubst, rightType = infer env bop.Right
                    let freshType = fresh ()
                    let! arrowSubst =
                        unify
                            (leftType => rightType => freshType)
                            binOpMap[bop.Operator]
                    return
                        leftSubst ++ rightSubst ++ arrowSubst,
                        Type.apply arrowSubst freshType

                | LiteralExpr (IntLiteral _) ->
                    return Substitution.empty, Type.int
                | LiteralExpr (BoolLiteral _) ->
                    return Substitution.empty, Type.bool
        }

    let private closeOver sub ty =
        generalize TypeEnvironment.empty (Type.apply sub ty)

    let private inferExpr env ex =
        result {
            let! sub, ty = infer env ex
            return closeOver sub ty
        }

    let rec inferTop env = function
        | [] -> Ok env
        | decl :: xs ->
            result {
                let! sc = inferExpr env decl.Body
                let env' = TypeEnvironment.add decl.Identifier sc env
                return! inferTop env' xs
            }
