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
    | TypeTuple of TypeTuple

    with
    member typ.Unparse() =
        match typ with
            | TypeConstant ident -> ident.Name
            | TypeVariable tv -> TypeVariable.unparse tv
            | TypeArrow (inpType, outType) ->
                $"({inpType.Unparse()} -> {outType.Unparse()})"
            | TypeTuple tuple ->
                let str =
                    tuple.Types
                        |> Seq.map (fun (typ : Type) ->
                            typ.Unparse())
                        |> String.concat " * "
                $"({str})"

    /// Constructs a type arrow. Right associative.
    // https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/#operator-precedence
    static member (^=>) (type1, type2) =
        TypeArrow (type1, type2)

/// Tupled types. E.g. "int * 'a".
and TypeTuple =
    {
        Type1 : Type
        Type2 : Type
        Types3N : List<Type>
    }

    with
    member this.Types =
        seq {
            yield this.Type1
            yield this.Type2
            yield! this.Types3N
        }

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
        | TypeTuple tuple ->
            tuple.Types
                |> Seq.map freeTypeVariables
                |> Set.unionMany

module TypeTuple =

    let map f tuple =
        {
            Type1 = f tuple.Type1
            Type2 = f tuple.Type2
            Types3N = List.map f tuple.Types3N
        }
