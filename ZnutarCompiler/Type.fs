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

    let create name : TypeVariable =
        { Name = name }

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

    /// Tupled types. E.g. "int * 'a".
    | TypeTuple of type1 : Type * type2 : Type * otherTypes : List<Type>

    with
    member typ.Unparse() =
        match typ with
            | TypeConstant ident -> ident.Name
            | TypeVariable tv -> TypeVariable.unparse tv
            | TypeArrow (inpType, outType) ->
                $"({inpType.Unparse()} -> {outType.Unparse()})"
            | TypeTuple (type1, type2, types) ->
                let str =
                    seq { yield type1; yield type2; yield! types }
                        |> Seq.map (fun typ -> typ.Unparse())
                        |> String.concat " * "
                $"({str})"

    /// Constructs a type arrow. Right associative.
    // https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/#operator-precedence
    static member (^=>) (type1, type2) =
        TypeArrow (type1, type2)

module Type =

    let unparse (typ : Type) =
        typ.Unparse()

    let variable = Identifier.create >> TypeVariable
    let constant = Identifier.create >> TypeConstant

    let int = constant "int"
    let bool = constant "bool"

    /// Free type variables in the given type.
    let rec freeTypeVariables = function
        | TypeConstant _ -> Set.empty
        | TypeVariable tv -> Set.singleton tv
        | TypeArrow (type1, type2) ->
            freeTypeVariables type1 + freeTypeVariables type2
        | TypeTuple (type1, type2, types) ->
            seq { yield type1; yield type2; yield! types }
                |> Seq.map freeTypeVariables
                |> Set.unionMany
