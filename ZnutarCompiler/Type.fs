namespace Znutar

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

module Type =

    let unparse (typ : Type) =
        typ.Unparse()

    let int = TypeConstant { Name = "Int" }
    let bool = TypeConstant { Name = "Bool" }

/// E.g. <'a>('a -> 'a).
type Scheme =
    {
        TypeVariables : List<TypeVariable>
        Type : Type
    }

type TypeEnvironment = Map<Variable, Scheme>

module TypeEnvironment =

    let empty : TypeEnvironment = Map.empty
