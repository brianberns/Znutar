namespace Znutar

/// Type variable substitution.
/// E.g. ['x <- Int; 'y <- Bool].
type Substitution =
    List<TypeVariable * Type>

type UnificationFailure =
    UnificationFailure of Type * Type
    with interface ICompilerError

[<AutoOpen>]
module Arrow =

    let (=>) t1 t2 = TypeArrow (t1, t2)

module Substitution =

    let empty : Substitution = List.empty

    let toString (subst : Substitution) =
        subst
            |> List.toSeq
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

        let substitute fromTv toType inType =

            let rec loop = function
                | TypeVariable tv as typ ->
                    if tv = fromTv then toType
                    else typ
                | TypeArrow (type1, type2) ->
                    loop type1 => loop type2
                | typ -> typ

            loop inType

        let apply (subst : Substitution) typ =
            (typ, subst)
                ||> List.fold (fun acc (fromIdent, toType) ->
                    substitute fromIdent toType acc)

        /// Free type variables in the given type.
        let rec freeTypeVariables = function
            | TypeConstant _ -> Set.empty
            | TypeVariable tv -> set [tv]
            | TypeArrow (type1, type2) ->
                freeTypeVariables type1 + freeTypeVariables type2

    let apply (subst : Substitution) (inSubst : Substitution) =
        (inSubst, subst)
            ||> List.fold (fun acc (fromIdent, toType) ->
                List.map (fun (ident, typ) ->
                    let typ' = Type.substitute fromIdent toType typ
                    ident, typ') acc)

    /// Composition of substitutions.
    let compose (subst1 : Substitution) subst2 : Substitution =
        subst1 @ apply subst1 subst2

    /// Composition of substitutions.
    let inline (++) subst1 subst2 = compose subst1 subst2

    let private occurs ident typ =
        typ
            |> Type.freeTypeVariables
            |> Set.contains ident

    /// Finds a substitution that unifies the given types.
    let unify type1 type2 =

        let rec loop type1 type2 =
            result {
                match type1, type2 with

                    | TypeConstant ident1, TypeConstant ident2
                        when ident1 = ident2 ->
                        return empty

                    | TypeVariable ident1, TypeVariable ident2   // avoid occurs check
                        when ident1 = ident2 ->
                        return empty

                    | TypeVariable ident, _
                        when type2 |> occurs ident |> not ->
                        return [ ident, type2 ]

                    | _, TypeVariable ident
                        when type1 |> occurs ident |> not ->
                        return [ ident, type1 ]

                    | TypeArrow (inpType1, outType1), TypeArrow (inpType2, outType2) ->
                        let! subst1 = loop inpType1 inpType2
                        let! subst2 =
                            loop
                                (Type.apply subst1 outType1)
                                (Type.apply subst1 outType2)
                        return subst1 ++ subst2

                    | _ ->
                        return! cerror (UnificationFailure (type1, type2))
            }

        loop type1 type2

    module Scheme =

        let substitute fromIdent toType scheme =
            if List.contains fromIdent scheme.TypeVariables then
                scheme
            else {
                scheme with
                    Type =
                        Type.substitute
                            fromIdent
                            toType
                            scheme.Type
            }

        let apply (subst : Substitution) scheme =
            (scheme, subst)
                ||> List.fold (fun acc (fromIdent, toType) ->
                    substitute fromIdent toType acc)

        /// Free type variables in the given scheme.
        let freeTypeVariables scheme =
            Type.freeTypeVariables scheme.Type
                - set scheme.TypeVariables

    module TypeEnvironment =

        /// Applies the given substitution to the given environment.
        let apply (subst : Substitution) (env : TypeEnvironment) : TypeEnvironment =
            (env, subst)
                ||> List.fold (fun acc (fromTv, toType) ->
                    Map.map (fun _ typ ->
                        Scheme.substitute fromTv toType typ) acc)

        /// Free type variables in the given environment.
        let freeTypeVariables (env : TypeEnvironment) =
            Seq.collect
                Scheme.freeTypeVariables
                (Map.values env)
                |> set
