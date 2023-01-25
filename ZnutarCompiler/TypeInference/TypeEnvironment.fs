﻿namespace Znutar.TypeInference

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

type private MethodTypeEnvironmentNode =
    {
        Schemes : Scheme[]
        Children : Map<Identifier, MethodTypeEnvironmentNode>
    }

type private MethodTypeEnvironment =
    Map<Identifier, MethodTypeEnvironmentNode>

module private MethodTypeEnvironment =

    let create assemblies =
        {
            Schemes = Array.empty
            Children = Map.empty
        }

type TypeEnvironment =
    private {
        FuncTypeEnv : FunctionTypeEnvironment
        MethodTypeEnv : MethodTypeEnvironment
    }

module TypeEnvironment =

    let create (assemblies : System.Reflection.Assembly[]) =
        {
            FuncTypeEnv = FunctionTypeEnvironment.empty
            MethodTypeEnv = Map.empty
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

    let freeTypeVariables env =
        FunctionTypeEnvironment.freeTypeVariables env.FuncTypeEnv
