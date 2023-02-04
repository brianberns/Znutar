﻿namespace Znutar.TypeInference

open System.Reflection
open Znutar

/// Tracks schemes by name.
/// E.g. "const" is mapped to <'a, 'b>('a -> 'b -> 'a).
type private FunctionTypeEnvironment = Map<Identifier, Scheme>

module private FunctionTypeEnvironment =

    /// Empty type environment.
    let empty : FunctionTypeEnvironment = Map.empty

    /// Adds the given scheme with the given identifier to
    /// the given environment.
    let add ident scheme (env : FunctionTypeEnvironment) : FunctionTypeEnvironment =
        env |> Map.add ident scheme

    /// Tries to find a scheme by name in the given environment.
    let tryFind ident (env : FunctionTypeEnvironment) =
        match Map.tryFind ident env with
            | Some scheme ->
                Ok scheme
            | None ->
                Error (UnboundIdentifier ident)

    /// Free type variables in the given environment.
    let freeTypeVariables (env : FunctionTypeEnvironment) =
        Seq.collect
            Scheme.freeTypeVariables
            (Map.values env)
            |> set

type private MemberTypeEnvironment =
    {
        Schemes : List<Scheme>
        Children : Map<Identifier, MemberTypeEnvironment>
    }

module private MemberTypeEnvironment =

    let empty =
        {
            Schemes = List.empty
            Children = Map.empty
        }

    let private addMember path (mem : #MemberInfo) env getSig add =
        match path with
            | [] ->
                let scheme =
                    Scheme.create [] (getSig mem)
                { env with Schemes = scheme :: env.Schemes }
            | ident :: tail ->
                let child =
                    env.Children
                        |> Map.tryFind ident
                        |> Option.defaultValue empty
                let child' = add tail mem child
                { env with
                    Children = Map.add ident child' env.Children }

    let rec addMethod path method env =
        addMember path method env
            Type.getMethodSignature
            addMethod

    let rec addProperty path property env =
        addMember path property env
            Type.getPropertySignature
            addProperty

    let create assemblies =
        let pairs =
            [|
                for (assembly : Assembly) in assemblies do
                    for typ in assembly.ExportedTypes do
                        if not typ.IsGenericType then
                            let namespaceParts = typ.Namespace.Split('.')

                            for method in typ.GetMethods() do
                                if method.IsStatic && not method.IsGenericMethod then
                                    let path =
                                        [
                                            yield! namespaceParts
                                            yield typ.Name
                                            yield method.Name
                                        ] |> List.map Identifier.create
                                    yield path, Choice1Of2 method

                            for property in typ.GetProperties() do
                                if property.GetMethod.IsStatic && not property.GetMethod.IsGenericMethod then
                                    let path =
                                        [
                                            yield! namespaceParts
                                            yield typ.Name
                                            yield property.Name
                                        ] |> List.map Identifier.create
                                    yield path, Choice2Of2 property
            |]
        (empty, pairs)
            ||> Seq.fold (fun tree (path, choice) ->
                match choice with
                    | Choice1Of2 method ->
                        addMethod path method tree
                    | Choice2Of2 property ->
                        addProperty path property tree)

    let rec tryFind path env =
        match path with
            | [] -> env.Schemes
            | ident :: tail ->
                env.Children
                    |> Map.tryFind ident
                    |> Option.map (tryFind tail)
                    |> Option.defaultValue List.empty

type TypeEnvironment =
    private {
        FuncTypeEnv : FunctionTypeEnvironment
        MemberTypeEnv : MemberTypeEnvironment
    }

module TypeEnvironment =

    let create assemblies =
        {
            FuncTypeEnv = FunctionTypeEnvironment.empty
            MemberTypeEnv = MemberTypeEnvironment.create assemblies
        }

    /// Adds the given scheme with the given identifier to
    /// the given environment.
    let add ident scheme env =
        { env with
            FuncTypeEnv =
                FunctionTypeEnvironment.add
                    ident
                    scheme
                    env.FuncTypeEnv }

    let tryFindFunc ident env =
        FunctionTypeEnvironment.tryFind ident env.FuncTypeEnv

    let tryFindMember path env =
        MemberTypeEnvironment.tryFind path env.MemberTypeEnv

    let freeTypeVariables env =
        FunctionTypeEnvironment.freeTypeVariables env.FuncTypeEnv
