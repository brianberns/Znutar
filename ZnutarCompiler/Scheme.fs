namespace Znutar

/// E.g. <'a>('a -> 'a).
type Scheme =
    {
        TypeVariables : List<TypeVariable>
        Type : Type
    }

module Scheme =

    let create typeVars typ =
        {
            TypeVariables = typeVars
            Type = typ
        }

    /// Free type variables in the given scheme.
    let freeTypeVariables scheme =
        Type.freeTypeVariables scheme.Type
            - set scheme.TypeVariables

type TypeEnvironment = Map<Identifier, Scheme>

module TypeEnvironment =

    let empty : TypeEnvironment = Map.empty

    let add ident scheme (env : TypeEnvironment) : TypeEnvironment =
        env |> Map.add ident scheme

    let tryFind ident (env : TypeEnvironment) =
        match Map.tryFind ident env with
            | Some scheme ->
                Ok scheme
            | None ->
                cerror (UnboundVariable ident)

    /// Free type variables in the given environment.
    let freeTypeVariables (env : TypeEnvironment) =
        Seq.collect
            Scheme.freeTypeVariables
            (Map.values env)
            |> set
