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

        let map' f =
            Map.map (fun _ v -> f v)

        let union m1 m2 =
            Seq.append (Map.toSeq m2) (Map.toSeq m1)   // left-biased
                |> Map.ofSeq

    module Type =

        let rec apply (s : Substitution) = function
            | TypeConstant a -> TypeConstant a
            | TypeVariable a as t ->
                s
                    |> Map.tryFind a
                    |> Option.defaultValue t
            | TypeArrow (t1, t2) ->
                apply s t1 => apply s t2

        let rec freeTypeVariables = function
            | TypeConstant _ -> Set.empty
            | TypeVariable a -> set [a]
            | TypeArrow (t1, t2) -> freeTypeVariables t1 + freeTypeVariables t2

    let compose (s1 : Substitution) (s2 : Substitution) : Substitution =
        Map.map' (Type.apply s1) s2
            |> Map.union s1

    let (++) = compose

    let rec unify t1 t2 =

        let occursCheck a t =
            Set.contains a (Type.freeTypeVariables t)

        let bind a t =
            if t = TypeVariable a then
                Ok empty
            elif occursCheck a t then
                error (InfiniteType (a, t))
            else
                Ok (Map [a, t])

        match t1, t2 with
            | TypeArrow (l, r), TypeArrow (l', r') ->
                result {
                    let! s1 = unify l l'
                    let! s2 =
                        unify (Type.apply s1 r) (Type.apply s1 r')
                    return s1 ++ s2
                }
            | TypeVariable a, t
            | t, TypeVariable a ->
                bind a t
            | (TypeConstant a), (TypeConstant b) when a = b ->
                Ok empty
            | _ -> error (UnificationFailure (t1, t2))

    module Scheme =

        let apply (s : Substitution) scheme =
            let s' : Substitution =
                List.foldBack Map.remove scheme.TypeVariables s
            {
                scheme with
                    Type = Type.apply s' scheme.Type
            }

        let freeTypeVariables scheme =
            Type.freeTypeVariables scheme.Type
                - set scheme.TypeVariables

    module TypeEnv =

        let apply s (env : TypeEnvironment) : TypeEnvironment =
            Map.map' (Scheme.apply s) env

        let freeTypeVariables (env : TypeEnvironment) =
            Seq.collect
                Scheme.freeTypeVariables
                (Map.values env)
                |> set
