namespace Znutar.TypeInference

open Znutar

/// Substitute types for type variables.
type Substitution = Map<TypeVariable, Type>

type UnificationFailure =
    UnificationFailure of Type * Type
    with interface ICompilerError

module Substitution =

    let empty : Substitution = Map.empty

    let toString (subst : Substitution) =
        subst
            |> Map.toSeq
            |> Seq.map (fun (tv, typ) ->
                $"{TypeVariable.unparse tv} <- {Type.unparse typ}")
            |> String.concat ", "

    module Map =

        /// Left-biased union of two maps.
        // https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html#v:union
        let union map1 map2 =
            Seq.append (Map.toSeq map2) (Map.toSeq map1)
                |> Map.ofSeq

    module Type =

        /// Applies the given substitution to the given type.
        let rec apply (subst : Substitution) = function
            | TypeConstant ident -> TypeConstant ident
            | TypeVariable tv as typ ->
                subst
                    |> Map.tryFind tv
                    |> Option.map (apply subst)   // https://github.com/sdiehl/write-you-a-haskell/issues/116
                    |> Option.defaultValue typ
            | TypeArrow (type1, type2) ->
                apply subst type1 ^=> apply subst type2

    /// Composition of substitutions.
    let compose (subst1 : Substitution) (subst2 : Substitution) : Substitution =
        subst2
            |> Map.map (fun _ value ->
                Type.apply subst1 value)
            |> Map.union subst1

    /// Composition of substitutions.
    let inline (++) subst1 subst2 = compose subst1 subst2

    /// Attempts to find a substitution that unifies the given types.
    let rec unify type1 type2 =

        /// Does the given type variable occur free in the given type?
        let occurs tv typ =
            Set.contains tv (Type.freeTypeVariables typ)

        match type1, type2 with
            | TypeArrow (left1, right1), TypeArrow (left2, right2) ->
                result {
                    let! subst1 = unify left1 left2
                    let! subst2 =
                        unify
                            (Type.apply subst1 right1)
                            (Type.apply subst1 right2)
                    return subst1 ++ subst2
                }
            | TypeVariable tv, typ
            | typ, TypeVariable tv ->
                if typ = TypeVariable tv then
                    Ok empty
                elif occurs tv typ then
                    cerror (UnificationFailure (type1, type2))
                else
                    Ok (Map [tv, typ])
            | (TypeConstant ident1), (TypeConstant ident2)
                when ident1 = ident2 ->
                Ok empty
            | _ -> cerror (UnificationFailure (type1, type2))

    module Expression =

        let rec apply subst = function
            | VariableExpr _
            | LiteralExpr _ as expr -> expr
            | ApplicationExpr app ->
                ApplicationExpr {
                    Function = apply subst app.Function
                    Argument = apply subst app.Argument
                }
            | LambdaExpr lam ->
                LambdaExpr {
                    lam with Body = apply subst lam.Body }
            | LetExpr letb ->
                LetExpr {
                    letb with
                        Argument = apply subst letb.Argument
                        Body= apply subst letb.Body
                }
            | IfExpr iff ->
                IfExpr {
                    Condition = apply subst iff.Condition
                    TrueBranch = apply subst iff.TrueBranch
                    FalseBranch = apply subst iff.FalseBranch
                }
            | BinaryOperationExpr bop ->
                BinaryOperationExpr {
                    bop with
                        Left = apply subst bop.Left
                        Right = apply subst bop.Right
                }
            | AnnotationExpr ann ->
                AnnotationExpr {
                    Type = Type.apply subst ann.Type
                    Expression = apply subst ann.Expression
                }
            | MemberAccessExpr ma ->
                MemberAccessExpr {
                    ma with
                        Expression = apply subst ma.Expression
                }

    module Scheme =

        /// Applies the given substitution to the given scheme.
        let apply (subst : Substitution) scheme =
            let subst' : Substitution =
                List.foldBack Map.remove scheme.TypeVariables subst
            {
                scheme with
                    Type = Type.apply subst' scheme.Type
            }

    module TypeEnvironment =

        /// Applies the given substitution to the given environment.
        let apply subst (env : TypeEnvironment) : TypeEnvironment =
            Map.map (fun _ value ->
                Scheme.apply subst value) env
