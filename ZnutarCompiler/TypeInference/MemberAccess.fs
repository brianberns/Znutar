namespace Znutar.TypeInference

open Znutar
open Znutar.TypeInference
open Substitution

module private Type =

    /// Count of type variables created.
    let mutable private count = 0

    /// Creates a fresh type variable with the given prefix.
    let createFreshTypeVariable (prefix : string) =
        count <- count + 1
        Type.variable $"{prefix}{count}"

module private MemberAccess =

    /// Infers the type of a static member access.
    /// E.g. System.Console.WriteLine.
    let private inferStaticMemberAccessWith env ma tryResolve =
        match TypeEnvironment.tryFindStaticMember ma env with

                // no such member (e.g. System.Xyzzy)
            | None

                // incomplete member access has no type (e.g. System.Diagnostics)
            | Some ([], _) -> Error (UnboundIdentifier ma.Identifier)

                // try to resolve overload
            | Some (schemes, path) ->
                match tryResolve schemes with
                    | Some (subst : Substitution, scheme : MemberScheme) ->
                        let annex =
                            AnnotatedStaticMemberAccessExpr {
                                Path = path
                                Type = scheme.Scheme.Type   // to-do: instantiate type?
                                IsConstructor = scheme.IsConstructor
                            }
                        Ok (subst, annex)
                    | None ->
                        Error (UnresolvedMethodOverload ma)

    let private inferInstanceMemberAccessWith env typ ident tryResolve =
        Error (InternalError "oops")

    /// Infers the type of the given member access using the
    /// given scheme resolver.
    let inferMemberAccessWith inferExpr env (ma : MemberAccess) tryResolve =

        let tryResolve' subst schemes =   // to-do: apply given substitution to each scheme?
            tryResolve schemes
                |> Option.map (fun (subst', scheme) ->
                    subst ++ subst', scheme)

            // determine static vs. instance member access
        match inferExpr env ma.Expression with

                // instance member (e.g. dt.AddYears)
            | Ok (exprSubst, exprAnnex : AnnotatedExpression) ->
                inferInstanceMemberAccessWith
                    env
                    exprAnnex.Type
                    ma.Identifier
                    (tryResolve' exprSubst)

                // possible static member (e.g. System.Console)
            | Error (_ : CompilerError) ->
                inferStaticMemberAccessWith
                    env
                    ma
                    (tryResolve' Substitution.empty)

    /// Infers the type of applying the given argument to the given
    /// member access. E.g. System.Console.WriteLine("Hello world").
    let inferMemberApplication inferExpr env ma (arg : Expression) =
        result {
                // infer the input type (e.g. "Hello world" : string)
            let! (argSubst : Substitution), (argAnnex : AnnotatedExpression) =
                inferExpr env arg

                // infer the member access type (e.g. WriteLine : string -> void)
            let! maSubst, maAnnex =
                let arrowType =
                    argAnnex.Type ^=> Type.createFreshTypeVariable "ma"
                inferStaticMemberAccessWith env ma (
                    Seq.tryPick (fun scheme ->
                        match Substitution.unify scheme.Scheme.Type arrowType with
                            | Ok subst -> Some (subst, scheme)
                            | Error _ -> None))

                // gather results
            let! typ =
                match maAnnex.Type with
                    | TypeArrow (_, outType) -> Ok outType
                    | _ ->
                        Error
                            (InternalError
                                $"Unexpected member access type: {maAnnex.Type.Unparse()}")
            let annex =
                AnnotatedApplicationExpr {
                    Function = maAnnex
                    Argument = argAnnex
                    Type = typ
                }
            return
                argSubst ++ maSubst,
                annex
        }
