namespace Znutar

open System.Reflection

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
        assert(name.StartsWith("'") |> not)   // apostrophe is implicit
        { Name = name }

    /// Unparses the given type variable.
    let unparse (tv : TypeVariable) =
        $"'{tv.Name}"                         // apostrophe is implicit

/// A qualified identifier. E.g. "System.Guid".
type QualifiedIdentifier = NonEmptyList<Identifier>

module QualifiedIdentifier =

    /// Creates a qualified identifier from the given full type name.
    let create (fullName : string) : QualifiedIdentifier =
        let idents =
            fullName.Split('.')
                |> Seq.map Identifier.create
                |> Seq.toList
        match idents with
            | [] -> failwith "Empty name"
            | ident :: idents ->
                NonEmptyList.create ident idents

    /// Converts the given qualified identifier to a string.
    let toString (qi : QualifiedIdentifier) =
        qi
            |> Seq.map (fun ident -> ident.Name)
            |> String.concat "."

/// The type of a value or function.
[<System.Diagnostics.DebuggerDisplay("{Unparse()}")>]
type Type =

    /// Type constant. E.g. int, bool, System.Guid.
    | TypeConstant of QualifiedIdentifier

    /// Type variable. E.g. 'a.
    | TypeVariable of TypeVariable

    /// Function type. E.g. 'a -> int.
    | TypeArrow of Type * Type

    /// Tuple type. E.g. int * 'a * bool.
    | TypeTuple of MultiItemList<Type>

    /// Unparses the given type.
    member typ.Unparse() =
        match typ with
            | TypeConstant qi -> QualifiedIdentifier.toString qi
            | TypeVariable tv -> TypeVariable.unparse tv
            | TypeArrow (inpType, outType) ->
                $"({inpType.Unparse()} -> {outType.Unparse()})"
            | TypeTuple types ->
                let sTypes =
                    types
                        |> Seq.map (fun typ -> typ.Unparse())
                        |> String.concat " * "
                $"({sTypes})"

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
    let constant = QualifiedIdentifier.create >> TypeConstant

    /// Primitive Boolean type.
    let bool = constant "bool"

    /// Primitive integer type.
    let int = constant "int"

    /// Primitive string type.
    let string = constant "string"

    /// Primitive unit type.
    let unit = constant "unit"

    /// Free type variables in the given type. (Note: *All*
    /// the type variables in a type are free. They only get
    /// bound in a scheme.)
    let rec freeTypeVariables = function
        | TypeConstant _ -> Set.empty
        | TypeVariable tv -> Set.singleton tv
        | TypeArrow (type1, type2) ->
            freeTypeVariables type1 + freeTypeVariables type2
        | TypeTuple types ->
            types
                |> Seq.map freeTypeVariables
                |> Set.unionMany

    let private ofDotnetType (dotnetType : System.Type) =
        if dotnetType = typeof<System.Boolean> then bool
        elif dotnetType = typeof<System.Int32> then int
        elif dotnetType = typeof<System.String> then string
        elif dotnetType = typeof<System.Void> then unit   // convert void to unit
        else constant dotnetType.FullName

    let ofMethod (method : MethodInfo) =
        assert(not method.IsGenericMethod)
        let inpType =
            let inpTypes =
                method.GetParameters()
                    |> Seq.map (fun parm ->
                        ofDotnetType parm.ParameterType)
                    |> Seq.toList
            match inpTypes with
                | [] -> unit   // convert void to unit
                | [typ] -> typ
                | type0 :: type1 :: rest ->
                    MultiItemList.create type0 type1 rest
                        |> TypeTuple
        let outType = ofDotnetType method.ReturnType
        inpType ^=> outType
