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

    module private Map =

        let union map1 map2 =
            Seq.append (Map.toSeq map2) (Map.toSeq map1)   // left-biased
                |> Map.ofSeq

    module Type =

        let rec apply (subst : Substitution) = function
            | TypeConstant ident -> TypeConstant ident
            | TypeVariable tv as typ ->
                subst
                    |> Map.tryFind tv
                    |> Option.defaultValue typ
            | TypeArrow (type1, type2) ->
                apply subst type1 => apply subst type2

        let rec freeTypeVariables = function
            | TypeConstant _ -> Set.empty
            | TypeVariable tv -> set [tv]
            | TypeArrow (type1, type2) ->
                freeTypeVariables type1 + freeTypeVariables type2

    let compose (subst1 : Substitution) (subst2 : Substitution) : Substitution =
        subst2
            |> Map.map (fun _ value ->
                Type.apply subst1 value)
            |> Map.union subst1

    let (++) = compose

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
                    error (InfiniteType (tv, typ))
                else
                    Ok (Map [tv, typ])
            | (TypeConstant ident1), (TypeConstant ident2)
                when ident1 = ident2 ->
                Ok empty
            | _ -> error (UnificationFailure (type1, type2))

    module Scheme =

        let apply (subst : Substitution) scheme =
            let subst' : Substitution =
                List.foldBack Map.remove scheme.TypeVariables subst
            {
                scheme with
                    Type = Type.apply subst' scheme.Type
            }

        let freeTypeVariables scheme =
            Type.freeTypeVariables scheme.Type
                - set scheme.TypeVariables

    module TypeEnv =

        let apply subst (env : TypeEnvironment) : TypeEnvironment =
            Map.map (fun _ value ->
                Scheme.apply subst value) env

        let freeTypeVariables (env : TypeEnvironment) =
            Seq.collect
                Scheme.freeTypeVariables
                (Map.values env)
                |> set
