namespace Znutar

/// Name of a value or type.
type Identifier =
    {
        Name : string
    }

module Identifier =

    let create name =
        { Name = name }

/// A type variable, such as 'a in 'a -> int.
type TypeVariable = Identifier

module TypeVariable =

    let unparse tv =
        $"'{tv.Name}"   // apostrophe is implicit

/// The type of a value or function.
[<System.Diagnostics.DebuggerDisplay("{Unparse()}")>]
type Type =

    /// Type constant. E.g. "int", "bool".
    | TypeConstant of Identifier

    /// Type variable. E.g. "'a".
    | TypeVariable of TypeVariable

    /// Function type. E.g. "'a -> int".
    | TypeArrow of Type * Type

    with
    member typ.Unparse() =
        match typ with
            | TypeConstant ident -> ident.Name
            | TypeVariable tv -> TypeVariable.unparse tv
            | TypeArrow (inpType, outType) ->
                $"({inpType.Unparse()} -> {outType.Unparse()})"

    /// Constructs a type arrow.
    static member (=>) (type1, type2) =
        TypeArrow (type1, type2)

module Type =

    let unparse (typ : Type) =
        typ.Unparse()

    let int = TypeConstant (Identifier.create "int")
    let bool = TypeConstant (Identifier.create "bool")

    /// Free type variables in the given type.
    let rec freeTypeVariables = function
        | TypeConstant _ -> Set.empty
        | TypeVariable tv -> Set.singleton tv
        | TypeArrow (type1, type2) ->
            freeTypeVariables type1 + freeTypeVariables type2
