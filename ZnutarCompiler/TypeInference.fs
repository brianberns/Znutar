namespace Znutar

module TypeInference =

    open Substitution

    let mutable count = 0

    let fresh _ =
        count <- count + 1
        Identifier.create $"tv{count}" |> TypeVariable

    let instantiate scheme =
        let tvs = List.map fresh scheme.TypeVariables
        let subst = Map (List.zip scheme.TypeVariables tvs)
        Type.apply subst scheme.Type

    let generalize env typ =
        let tvs =
            Set.toList (
                Type.freeTypeVariables typ
                    - TypeEnvironment.freeTypeVariables env)
        {
            TypeVariables = tvs
            Type = typ
        }

    let private lookupEnv (env : TypeEnvironment) x =
        result {
            match Map.tryFind x env with
                | None ->
                    return! cerror (UnboundVariable x)
                | Some s ->
                    let t = instantiate s
                    return Substitution.empty, t
        }

    let private ops =
        Map [
            Plus, Type.int => Type.int => Type.int
            Minus, Type.int => Type.int => Type.int
            Times, Type.int => Type.int => Type.int
            Equals, Type.int => Type.int => Type.bool   // to-do: make polymorphic
        ]

    let rec infer env ex =
        result {
            match ex with
                | VariableExpr x ->
                    return! lookupEnv env x
                | LambdaExpr lam ->
                    let tv = fresh ()
                    let scheme = { TypeVariables = []; Type = tv }
                    let env' = TypeEnvironment.add lam.Identifier scheme env
                    let! s1, t1 = infer env' lam.Body
                    return s1, Type.apply s1 tv => t1
                | ApplicationExpr app ->
                    let tv = fresh ()
                    let! s1, t1 = infer env app.Function
                    let! s2, t2 = infer (TypeEnvironment.apply s1 env) app.Argument
                    let! s3 = unify (Type.apply s2 t1) (t2 => tv)
                    return s3 ++ s2 ++ s1, Type.apply s3 tv
                | LetExpr letb ->
                    let! s1, t1 = infer env letb.Argument
                    let env' = TypeEnvironment.apply s1 env
                    let t' = generalize env' t1
                    let! s2, t2 =
                        infer (TypeEnvironment.add letb.Identifier t' env') letb.Body
                    return s1 ++ s2, t2
                | IfExpr iff ->
                    let! s1, t1 = infer env iff.Condition
                    let! s2, t2 = infer env iff.TrueBranch
                    let! s3, t3 = infer env iff.FalseBranch
                    let! s4 = unify t1 Type.bool
                    let! s5 = unify t2 t3
                    return s5 ++ s4 ++ s3 ++ s2 ++ s1, Type.apply s5 t2
                | FixExpr fix ->
                    let! s1, t = infer env fix
                    let tv = fresh ()
                    let! s2 = unify (tv => tv) t
                    return s2, Type.apply s1 tv
                | BinaryOperationExpr bop ->
                    let! s1, t1 = infer env bop.Left
                    let! s2, t2 = infer env bop.Right
                    let tv = fresh ()
                    let! s3 = unify (t1 => t2 => tv) ops[bop.Operator]
                    return s1 ++ s2 ++ s3, Type.apply s3 tv
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
