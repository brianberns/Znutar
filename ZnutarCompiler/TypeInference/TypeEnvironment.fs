namespace Znutar.TypeInference

open Znutar

type private TypeEnvironment = Map<Identifier, Scheme>

module private TypeEnvironment =

    let empty : TypeEnvironment = Map.empty

    let add ident scheme (env : TypeEnvironment) : TypeEnvironment =
        env |> Map.add ident scheme

    let tryFind ident (env : TypeEnvironment) =
        match Map.tryFind ident env with
            | Some scheme ->
                Ok scheme
            | None ->
                Error (UnboundIdentifier ident)

    /// Free type variables in the given environment.
    let freeTypeVariables (env : TypeEnvironment) =
        Seq.collect
            Scheme.freeTypeVariables
            (Map.values env)
            |> set
