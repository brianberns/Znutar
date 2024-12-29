namespace Znutar.TypeInference

open System.Reflection
open Znutar

/// Tracks functional schemes by name. E.g. "const" is mapped
/// to <'a, 'b>('a -> 'b -> 'a).
type private FunctionalTypeEnvironment = Map<Identifier, Scheme>

module private FunctionalTypeEnvironment =

    /// Empty type environment.
    let empty : FunctionalTypeEnvironment = Map.empty

    /// Adds the given scheme with the given identifier to
    /// the given environment.
    let add ident scheme (env : FunctionalTypeEnvironment) : FunctionalTypeEnvironment =
        env |> Map.add ident scheme

    /// Tries to find a scheme by name in the given environment.
    let tryFind ident (env : FunctionalTypeEnvironment) =
        match Map.tryFind ident env with
            | Some scheme ->
                Ok scheme
            | None ->
                Error (UnboundIdentifier ident)

    /// Free type variables in the given environment.
    let freeTypeVariables (env : FunctionalTypeEnvironment) =
        Seq.collect
            Scheme.freeTypeVariables
            (Map.values env)
            |> set

/// The scheme of a member.
type MemberScheme =
    {
        /// Underlying scheme.
        Scheme : Scheme

        /// Is this member a constructor?
        IsConstructor : bool
    }

module MemberScheme =

    /// Creates a member scheme.
    let create typeVars typ isConstructor =
        {
            Scheme = Scheme.create typeVars typ
            IsConstructor = isConstructor
        }

/// Tracks member schemes by name, hierarchically.
type private MemberTypeEnvironment =
    {
        /// Member schemes at this level of the hierarchy.
        Schemes : List<MemberScheme>

        /// Child environments by name. (E.g. Console under System.)
        Children : Map<Identifier, MemberTypeEnvironment>
    }

module private MemberTypeEnvironment =

    /// Empty member type environment.
    let empty =
        {
            Schemes = List.empty
            Children = Map.empty
        }

    /// Adds a member to the given environment.
    let private addMember path (mem : #MemberInfo) typ add env =
        match path with

                // add member scheme at this level
            | [] ->
                let scheme =
                    let isConstructor =
                        (mem.MemberType = MemberTypes.Constructor)
                    MemberScheme.create [] typ isConstructor   // to-do: allow type variables
                { env with Schemes = scheme :: env.Schemes }

                // add to child environment
            | ident :: tail ->
                let child =
                    env.Children
                        |> Map.tryFind ident
                        |> Option.defaultValue empty
                let child' = add tail mem child
                { env with
                    Children = Map.add ident child' env.Children }

    /// Adds a method to the given environment.
    let rec private addMethod path method env =
        addMember path method
            (Type.getMethodSignature method)
            addMethod
            env

    /// Adds a property to the given environment.
    let rec private addProperty path property env =
        addMember path property
            (Type.getPropertySignature property)
            addProperty
            env

    /// Adds a constructor to the given environment.
    let rec private addConstructor path constructor env =
        addMember path constructor
            (Type.getConstructorSignature constructor)
            addConstructor
            env

    /// E.g. Parameter beginEndCallback of ObjectiveCMarshal.Initialize.
    let hasValidInterface (method : MethodInfo) =
        let parms =
            method.GetParameters()
                |> Seq.forall (fun parm ->
                    not (isNull parm.ParameterType.FullName))
        parms && not (isNull method.ReturnType.FullName)

    /// Creates a member type environment from the given assemblies.
    let create assemblies =
        let pairs =
            [|
                for (assembly : Assembly) in assemblies do
                    for typ in assembly.ExportedTypes do
                        if not typ.IsGenericType then

                                // type's path (e.g. [System; Console])
                            let typePath = QualifiedIdentifier.parse typ.FullName

                            /// Gets member's full path, including member name.
                            /// E.g. [System; Console; WriteLine].
                            let getFullPath (mem : MemberInfo) =
                                if mem.MemberType = MemberTypes.Constructor then
                                    typePath
                                else
                                    let ident =
                                        Identifier.create mem.Name
                                    NonEmptyList.append
                                        typePath
                                        (NonEmptyList.singleton ident)

                            for method in typ.GetMethods() do
                                if not method.IsGenericMethod
                                    && hasValidInterface method then
                                    yield getFullPath method, Choice1Of3 method

                            for property in typ.GetProperties() do
                                let method = property.GetMethod
                                if not method.IsGenericMethod then
                                    yield getFullPath property, Choice2Of3 property

                            for constructor in typ.GetConstructors() do
                                yield getFullPath constructor, Choice3Of3 constructor
            |]
        (empty, pairs)
            ||> Seq.fold (fun tree (path, choice) ->
                let path' = NonEmptyList.toList path
                match choice with
                    | Choice1Of3 method ->
                        addMethod path' method tree
                    | Choice2Of3 property ->
                        addProperty path' property tree
                    | Choice3Of3 constructor ->
                        addConstructor path' constructor tree)

    /// Tries to convert a member access to an identifier path.
    /// E.g. System.Console.WriteLine -> [ System; Console; WriteLine ]
    let private tryGetPath (ma : MemberAccess) =

        let rec loop acc expr : Option<QualifiedIdentifier> =
            option {
                match expr with
                    | IdentifierExpr ident ->
                        return NonEmptyList.cons ident acc
                    | MemberAccessExpr ma' ->
                        let acc' = NonEmptyList.cons ma'.Identifier acc
                        return! loop acc' ma'.Expression
                    | _ ->
                        return! None
            }

        let init = NonEmptyList.singleton ma.Identifier
        loop init ma.Expression

    /// Tries to navigate the given path in the given environment.
    let private tryNavigatePath (path : QualifiedIdentifier) env =
        let idents = NonEmptyList.toList path
        (env, idents)
            ||> Option.foldM (fun env' ident ->
                Map.tryFind ident env'.Children)

    /// Tries to find the given static member access in the given
    /// environment. E.g. System.Console.WriteLine.
    let tryFindStatic ma env =
        option {
            let! path = tryGetPath ma
            let! env' = tryNavigatePath path env
            return env'.Schemes, path
        }

    /// Tries to find the given instance member access in the given
    /// environment. E.g. dt.Year.
    let tryFindInstance typ ident env =
        match typ with
            | TypeConstant qi ->
                option {
                    let path =
                        NonEmptyList.append qi
                            (NonEmptyList.singleton ident)
                    let! env' = tryNavigatePath path env
                    return env'.Schemes
                }
            | _ -> None

/// Tracks function and member schemes by name.
type TypeEnvironment =
    private {

        /// Functional type environment.
        FuncTypeEnv : FunctionalTypeEnvironment

        /// Member type environment.
        MemberTypeEnv : MemberTypeEnvironment
    }

module TypeEnvironment =

    /// Creates a type environment from the given assemblies.
    let create assemblies =
        {
            FuncTypeEnv = FunctionalTypeEnvironment.empty
            MemberTypeEnv = MemberTypeEnvironment.create assemblies
        }

    /// Adds the given functional scheme with the given identifier
    /// to the given environment.
    let addFunctional ident scheme env =
        { env with
            FuncTypeEnv =
                FunctionalTypeEnvironment.add
                    ident
                    scheme
                    env.FuncTypeEnv }

    /// Free type variables in the given environment.
    let freeTypeVariables env =
        FunctionalTypeEnvironment.freeTypeVariables env.FuncTypeEnv

    /// Tries to find the given functional identifier in the given
    /// environment.
    let tryFindFunctional ident env =
        FunctionalTypeEnvironment.tryFind ident env.FuncTypeEnv

    /// Tries to find a static member in the given environment.
    let tryFindStaticMember ma env =
        MemberTypeEnvironment.tryFindStatic ma env.MemberTypeEnv

    /// Tries to find an instance member in the given environment.
    let tryFindInstanceMember typ ident env =
        MemberTypeEnvironment.tryFindInstance typ ident env.MemberTypeEnv
