﻿namespace Znutar

open System
open System.Reflection

/// Name of a value or type.
type Identifier =
    {
        Name : string
    }

module Identifier =

    /// Creates an identifier with the given name.
    let create name =
        assert(String.IsNullOrEmpty(name) |> not)   // to-do: improve validation
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

    /// Splits the given string into at least one substring.
    let private nonEmptySplit (separator : char) (str : string) =
        assert(isNull str |> not)
        let substrs = str.Split(separator)
        assert(substrs.Length >= 0)
        NonEmptyList.create
            substrs[0]
            (Seq.toList substrs[1..])

    /// Creates a qualified identifier from the given full name.
    let parse (fullName : string) : QualifiedIdentifier =
        fullName
            |> nonEmptySplit '.'
            |> NonEmptyList.map Identifier.create

    /// Converts the given qualified identifier to a string.
    let unparse (qi : QualifiedIdentifier) =
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
            | TypeConstant qi -> QualifiedIdentifier.unparse qi
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
    let constant = QualifiedIdentifier.parse >> TypeConstant

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

    /// Map from .NET types to Znutar types.
    let private dotnetTypeDict =
        dict [   // can't use F# Map because System.Type doesn't implement IComparable
            typeof<Boolean>, bool
            typeof<Int32>, int
            typeof<String>, string
            typeof<Void>, unit   // convert void to unit
        ]

    /// Creates a Znutar type from the given .NET type.
    let private ofDotnetType dotnetType =
        match dotnetTypeDict.TryGetValue(dotnetType) with
            | true, typ -> typ
            | false, _ -> constant dotnetType.FullName

    /// Creates a Znutar type for the signature of the given method.
    let private getMethodBaseSignature returnType (method : MethodBase) =
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
        let outType = ofDotnetType returnType
        inpType ^=> outType

    /// Creates a Znutar type for the signature of the given method.
    let getMethodSignature (method : MethodInfo) =
        getMethodBaseSignature
            method.ReturnType
            method

    /// Creates a Znutar type for the signature of the given property.
    let getPropertySignature (property : PropertyInfo) =
        assert(not property.GetMethod.IsGenericMethod)
        ofDotnetType property.PropertyType

    /// Creates a Znutar type for the signature of the given constructor.
    let getConstructorSignature (constructor : ConstructorInfo) =
        getMethodBaseSignature
            constructor.DeclaringType
            constructor

   // to-do: allow explicit references to .NET name of Znutar type (e.g. "(Console.WriteLine : System.String -> unit)")
