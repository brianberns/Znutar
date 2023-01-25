namespace Znutar.TypeInference

open Znutar

/// Tracks schemes by name.
/// E.g. "const" is mapped to <'a, 'b>('a -> 'b -> 'a).
type private TypeEnvironment = Map<Identifier, Scheme>

module private TypeEnvironment =

    /// Empty type environment.
    let empty : TypeEnvironment = Map.empty

    /// Adds the given scheme with the given identifier to
    /// the given environment.
    let add ident scheme (env : TypeEnvironment) : TypeEnvironment =
        env |> Map.add ident scheme

    /// Tries to find a scheme by name in the given environment.
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
