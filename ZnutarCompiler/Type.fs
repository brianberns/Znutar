﻿namespace Znutar

type Identifier =
    {
        Name : string
    }

module Identifier =

    let create name =
        { Name = name }

    let unparse ident =
        ident.Name

type TypeVariable = Identifier

module TypeVariable =

    let unparse tv =
        $"'{tv.Name}"   // apostrophe is implicit

/// The type of a value or function.
[<System.Diagnostics.DebuggerDisplay("{Unparse()}")>]
type Type =

    /// Type constant. E.g. "Int", "Bool".
    | TypeConstant of Identifier

    /// Type variable. E.g. "'a".
    | TypeVariable of TypeVariable

    /// Function type. E.g. "'a -> Int".
    | TypeArrow of Type * Type

    with
    member typ.Unparse() =
        match typ with
            | TypeConstant ident -> ident.Name
            | TypeVariable tv -> TypeVariable.unparse tv
            | TypeArrow (inpType, outType) ->
                $"({inpType.Unparse()} -> {outType.Unparse()})"

    static member (=>) (type1, type2) =
        TypeArrow (type1, type2)

module Type =

    let unparse (typ : Type) =
        typ.Unparse()

    let int = TypeConstant (Identifier.create "int")
    let bool = TypeConstant (Identifier.create "bool")
