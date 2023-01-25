namespace Znutar.TypeInference

open Znutar

/// Generalized type signature of a function.
/// E.g. const has scheme: <'a, 'b>('a -> 'b -> 'a).
[<System.Diagnostics.DebuggerDisplay("{Unparse()}")>]
type Scheme =
    {
        /// Type variables bound by this scheme.
        TypeVariables : List<TypeVariable>

        /// Type defined by this scheme.
        Type : Type
    }

    /// Unparses the given scheme.
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

    /// Creates a scheme.
    let create typeVars typ =
        {
            TypeVariables = typeVars
            Type = typ
        }

    /// Unparses the given scheme.
    let unparse (scheme : Scheme) =
        scheme.Unparse()

    /// Free type variables in the given scheme.
    /// E.g. 'b is free in <'a>('a -> 'b).
    let freeTypeVariables scheme =
        Type.freeTypeVariables scheme.Type
            - set scheme.TypeVariables
