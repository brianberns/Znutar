namespace Znutar

/// Name of a value or type.
type Identifier =
    {
        Name : string
    }

module Identifier =

    /// Creates an identifier with the given name.
    let create name =
        { Name = name }

/// A type variable, such as 'a in 'a -> int.
type TypeVariable = Identifier

module TypeVariable =

    /// Creates a type variable with the given name.
    let create (name : string) : TypeVariable =
        assert(name.Contains('\'') |> not)   // apostrophe is implicit
        { Name = name }

    /// Unparses the given type variable.
    let unparse (tv : TypeVariable) =
        $"'{tv.Name}"                        // apostrophe is implicit

/// The type of a value or function.
[<System.Diagnostics.DebuggerDisplay("{Unparse()}")>]
type Type =

    /// Type constant. E.g. int, bool.
    | TypeConstant of Identifier

    /// Type variable. E.g. 'a.
    | TypeVariable of TypeVariable

    /// Function type. E.g. 'a -> int.
    | TypeArrow of Type * Type

    with

    /// Unparses the given type.
    member typ.Unparse() =
        match typ with
            | TypeConstant ident -> ident.Name
            | TypeVariable tv -> TypeVariable.unparse tv
            | TypeArrow (inpType, outType) ->
                $"({inpType.Unparse()} -> {outType.Unparse()})"

    /// Constructs a type arrow. Right associative.
    // https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/#operator-precedence
    static member (^=>) (inpType, outType) =
        TypeArrow (inpType, outType)

module Type =

    /// Unparses the given type.
    let unparse (typ : Type) =
        typ.Unparse()

    /// Creates a type variable with the given name.
    let variable = Identifier.create >> TypeVariable

    /// Creates a type constant with the given name.
    let constant = Identifier.create >> TypeConstant

    /// Primitive integer type.
    let int = constant "int"

    /// Primitive Boolean type.
    let bool = constant "bool"

    /// Free type variables in the given type.
    let rec freeTypeVariables = function
        | TypeConstant _ -> Set.empty
        | TypeVariable tv -> Set.singleton tv
        | TypeArrow (type1, type2) ->
            freeTypeVariables type1 + freeTypeVariables type2
