namespace Znutar.TypeInference

open Znutar

/// Generalized type signature of a function.
/// E.g. const has scheme: <'a, 'b>('a -> 'b -> 'a).
[<System.Diagnostics.DebuggerDisplay("{Unparse()}")>]
type Scheme =
    {
        TypeVariables : List<TypeVariable>
        Type : Type
    }

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
    /// E.g. 'b is free in <'a>('a -> 'b).
    let freeTypeVariables scheme =
        Type.freeTypeVariables scheme.Type
            - set scheme.TypeVariables
