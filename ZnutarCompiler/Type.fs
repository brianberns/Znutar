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

/// The type of a value or function.
[<System.Diagnostics.DebuggerDisplay("{Unparse()}")>]
type Type =

    /// Type constant. E.g. int, bool.
    | TypeConstant of Identifier

    /// Type variable. E.g. 'a.
    | TypeVariable of TypeVariable

    /// Function type. E.g. 'a -> int.
    | TypeArrow of Type * Type

    /// Tuple type. e.g. int * 'a * bool.
    | TypeTuple of MultiItemList<Type>

    /// Unparses the given type.
    member typ.Unparse() =
        match typ with
            | TypeConstant ident -> ident.Name
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
    let constant = Identifier.create >> TypeConstant

    /// Primitive Boolean type.
    let bool = constant "bool"

    /// Primitive integer type.
    let int = constant "int"

    /// Primitive string type.
    let string = constant "string"

    /// Primitive unit type.
    let unit = constant "unit"

    /// Primitive void type. (Not the same as unit.)
    let ``void`` = constant "void"

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

    let private ofSystemType (sysType : System.Type) =
        if sysType = typeof<System.Boolean> then bool
        elif sysType = typeof<System.Int32> then int
        elif sysType = typeof<System.String> then string
        elif sysType = typeof<System.Void> then ``void``
        else
            constant sysType.Name

    let ofMethod (method : MethodInfo) =
        assert(not method.IsGenericMethod)
        let inpType =
            let inpTypes =
                method.GetParameters()
                    |> Seq.map (fun parm ->
                        ofSystemType parm.ParameterType)
                    |> Seq.toList
            match inpTypes with
                | [] -> unit
                | [typ] -> typ
                | type0 :: type1 :: rest ->
                    MultiItemList.create type0 type1 rest
                        |> TypeTuple
        let outType =
            method.ReturnType
                |> ofSystemType
        inpType ^=> outType
