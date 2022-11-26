namespace Znutar

type Substitution = Map<TypeVariable, Type>

type UnificationFailure =
    UnificationFailure of Type * Type
    with interface ICompilerError

type InfiniteType =
    InfiniteType of TypeVariable * Type
    with interface ICompilerError

[<AutoOpen>]
module Arrow =

    let (=>) t1 t2 = TypeArrow (t1, t2)

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
                    |> Option.defaultValue typ
            | TypeArrow (type1, type2) ->
                apply subst type1 => apply subst type2

        /// Free type variables in the given type.
        let rec freeTypeVariables = function
            | TypeConstant _ -> Set.empty
            | TypeVariable tv -> set [tv]
            | TypeArrow (type1, type2) ->
                freeTypeVariables type1 + freeTypeVariables type2

    /// Composition of substitutions.
    let compose (subst1 : Substitution) (subst2 : Substitution) : Substitution =
        assert(Set.intersect
            (set subst1.Keys)
            (set subst2.Keys) = Set.empty)
        subst2
            |> Map.map (fun _ value ->
                Type.apply subst1 value)
            |> Map.union subst1

    /// Composition of substitutions.
    let inline (++) subst1 subst2 = compose subst1 subst2

    /// Attempts to find a substitution that unifies the given types.
    let rec unify type1 type2 =

        let occursCheck tv typ =
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
                elif occursCheck tv typ then
                    cerror (InfiniteType (tv, typ))
                else
                    Ok (Map [tv, typ])
            | (TypeConstant ident1), (TypeConstant ident2)
                when ident1 = ident2 ->
                Ok empty
            | _ -> cerror (UnificationFailure (type1, type2))

    module Scheme =

        /// Applies the given substitution to the given scheme.
        let apply (subst : Substitution) scheme =
            let subst' : Substitution =
                List.foldBack Map.remove scheme.TypeVariables subst
            {
                scheme with
                    Type = Type.apply subst' scheme.Type
            }

        /// Free type variables in the given scheme.
        let freeTypeVariables scheme =
            Type.freeTypeVariables scheme.Type
                - set scheme.TypeVariables

    module TypeEnv =

        /// Applies the given substitution to the given environment.
        let apply subst (env : TypeEnvironment) : TypeEnvironment =
            Map.map (fun _ value ->
                Scheme.apply subst value) env

        /// Free type variables in the given environment.
        let freeTypeVariables (env : TypeEnvironment) =
            Seq.collect
                Scheme.freeTypeVariables
                (Map.values env)
                |> set
