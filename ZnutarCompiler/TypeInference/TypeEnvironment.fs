namespace Znutar.TypeInference

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

type MemberScheme =
    {
        Scheme : Scheme
        IsConstructor : bool
    }

module MemberScheme =

    let create typeVars typ isConstructor =
        {
            Scheme = Scheme.create typeVars typ
            IsConstructor = isConstructor
        }

type private MemberTypeEnvironment =
    {
        Schemes : List<MemberScheme>
        Children : Map<Identifier, MemberTypeEnvironment>
    }

module private MemberTypeEnvironment =

    let empty =
        {
            Schemes = List.empty
            Children = Map.empty
        }

    // to-do: use QualifiedIdentifier type for path?
    let private addMember path (mem : #MemberInfo) env getSig add =
        match path with
            | [] ->
                let scheme =
                    MemberScheme.create
                        []
                        (getSig mem)
                        (mem.MemberType = MemberTypes.Constructor)
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

    let rec addConstructor path constructor env =
        addMember path constructor env
            Type.getConstructorSignature
            addConstructor

    let create assemblies =
        let pairs =
            [|
                for (assembly : Assembly) in assemblies do
                    for typ in assembly.ExportedTypes do
                        if not typ.IsGenericType then

                            let namespaceParts = typ.Namespace.Split('.')
                            let getPath (mem : MemberInfo) =
                                [
                                    yield! namespaceParts
                                    yield typ.Name
                                    if mem.MemberType <> MemberTypes.Constructor then
                                        yield mem.Name
                                ] |> List.map Identifier.create

                            for method in typ.GetMethods() do
                                if method.IsStatic && not method.IsGenericMethod then
                                    yield getPath method, Choice1Of3 method

                            for property in typ.GetProperties() do
                                let method = property.GetMethod
                                if method.IsStatic && not method.IsGenericMethod then
                                    yield getPath property, Choice2Of3 property

                            for constructor in typ.GetConstructors() do
                                yield getPath constructor, Choice3Of3 constructor
            |]
        (empty, pairs)
            ||> Seq.fold (fun tree (path, choice) ->
                match choice with
                    | Choice1Of3 method ->
                        addMethod path method tree
                    | Choice2Of3 property ->
                        addProperty path property tree
                    | Choice3Of3 constructor ->
                        addConstructor path constructor tree)

    let rec tryFind path env =
        match path with
            | [] -> env.Schemes
            | ident :: tail ->
                env.Children
                    |> Map.tryFind ident
                    |> Option.map (tryFind tail)
                    |> Option.defaultValue List.empty

    /// Tries to find the given static member access in the given
    /// environment. E.g. System.Console.WriteLine.
    let tryFindStatic ma env =

        let rec loop ma =
            option {
                let! env', idents =
                    match ma.Expression with

                            // e.g. find System.Console
                        | MemberAccessExpr ma' -> loop ma'

                            // e.g. find System
                        | IdentifierExpr ident ->
                            option {
                                let! env' = Map.tryFind ident env.Children
                                return env', NonEmptyList.singleton ident
                            }

                            // not a static member access
                        | _ -> None

                    // e.g. find WriteLine in System.Console's environment
                let! env'' = Map.tryFind ma.Identifier env'.Children
                let idents' = NonEmptyList.cons ma.Identifier idents
                return env'', idents'
            }

        option {
            let! env', idents = loop ma
            let qi : QualifiedIdentifier = NonEmptyList.rev idents
            return env'.Schemes, qi
        }

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

    let tryFindStaticMember ma env =
        MemberTypeEnvironment.tryFindStatic ma env.MemberTypeEnv

    let freeTypeVariables env =
        FunctionTypeEnvironment.freeTypeVariables env.FuncTypeEnv
