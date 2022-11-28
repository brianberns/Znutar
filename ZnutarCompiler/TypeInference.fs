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
                    let scheme = Scheme.create [] freshType
                    let env' = TypeEnvironment.add lam.Identifier scheme env
                    let! subst1, type1 = infer env' lam.Body
                    return subst1, Type.apply subst1 freshType => type1
                | ApplicationExpr app ->
                    let freshType = fresh ()
                    let! subst1, type1 = infer env app.Function
                    let! subst2, type2 = infer (TypeEnvironment.apply subst1 env) app.Argument
                    let! subst3 = unify (Type.apply subst2 type1) (type2 => freshType)
                    return subst3 ++ subst2 ++ subst1, Type.apply subst3 freshType
                | LetExpr letb ->
                    let! subst1, type1 = infer env letb.Argument
                    let env' = TypeEnvironment.apply subst1 env
                    let type1' = generalize env' type1
                    let! subst2, type2 =
                        infer (TypeEnvironment.add letb.Identifier type1' env') letb.Body
                    return subst1 ++ subst2, type2
                | IfExpr iff ->
                    let! subst1, type1 = infer env iff.Condition
                    let! subst2, type2 = infer env iff.TrueBranch
                    let! subst3, type3 = infer env iff.FalseBranch
                    let! subst4 = unify type1 Type.bool
                    let! subst5 = unify type2 type3
                    return subst5 ++ subst4 ++ subst3 ++ subst2 ++ subst1, Type.apply subst5 type2
                | FixExpr fix ->
                    let! subst1, typ = infer env fix
                    let freshType = fresh ()
                    let! subst2 = unify (freshType => freshType) typ
                    return subst2, Type.apply subst1 freshType
                | BinaryOperationExpr bop ->
                    let! subst1, type1 = infer env bop.Left
                    let! subst2, type2 = infer env bop.Right
                    let freshType = fresh ()
                    let! subst3 = unify (type1 => type2 => freshType) binOpMap[bop.Operator]
                    return subst1 ++ subst2 ++ subst3, Type.apply subst3 freshType
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
