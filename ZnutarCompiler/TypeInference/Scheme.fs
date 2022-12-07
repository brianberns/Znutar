namespace Znutar

/// Generalized type signature of a function.
/// E.g. const has scheme: <'a, 'b>('a -> 'b -> 'a).
[<System.Diagnostics.DebuggerDisplay("{Unparse()}")>]
type Scheme =
    {
        TypeVariables : List<TypeVariable>
        Type : Type
    }

    with
    member scheme.Unparse() =
        let typeVars =
            if scheme.TypeVariables.IsEmpty then ""
            else
                scheme.TypeVariables
                    |> Seq.map TypeVariable.unparse
                    |> String.concat ", "
                    |> sprintf "<%s>"
        $"{typeVars}{Type.unparse scheme.Type}"

module Scheme =

    let create typeVars typ =
        {
            TypeVariables = typeVars
            Type = typ
        }

    let unparse (scheme : Scheme) =
        scheme.Unparse()

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
