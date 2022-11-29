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
