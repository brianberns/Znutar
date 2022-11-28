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

    static member (=>) (type1, type2) =
        TypeArrow (type1, type2)

module Type =

    let unparse (typ : Type) =
        typ.Unparse()

    let int = TypeConstant (Identifier.create "Int")
    let bool = TypeConstant (Identifier.create "Bool")

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
