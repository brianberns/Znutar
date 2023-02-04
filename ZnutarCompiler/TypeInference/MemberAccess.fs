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

    /// Converts a member access to an identifier path.
    /// E.g. System.Console.WriteLine -> [ System; Console; WriteLine ]
    let private getPath (ma : MemberAccess) =

        let rec loop acc expr =
            result {
                match expr with
                    | Expression.IdentifierExpr ident ->
                        return ident :: acc
                    | Expression.MemberAccessExpr ma ->
                        return! loop (ma.Identifier :: acc) ma.Expression
                    | _ ->
                        return! Error (
                            InternalError
                                $"Unexpected expression type: {expr.Unparse()}")
            }

        loop [ma.Identifier] ma.Expression

    /// Infers the type of a member access.
    /// E.g. System.Console.WriteLine.
    let inferMemberAccess env ma tryResolve =
        result {
            let! path = getPath ma
            match TypeEnvironment.tryFindMember path env with

                    // no such member
                | [] -> return! Error (UnboundIdentifier ma.Identifier)

                    // try to resolve overload
                | schemes ->
                    match tryResolve schemes with
                        | Some (subst : Substitution, scheme : Scheme) ->
                            let annex =
                                MemberAccessExpr {
                                    MemberAccess = ma
                                    Type = scheme.Type   // to-do: instantiate type?
                                }
                            return subst, annex
                        | None ->
                            return! Error (UnresolvedMethodOverload ma)
        }

    /// Infers the type of a member access with the given signature.
    /// E.g. System.Console.WriteLine : string -> void.
    let inferMemberAccessTyped env ma typ =
        inferMemberAccess env ma (
            Seq.tryPick (fun scheme ->
                match Substitution.unify scheme.Type typ with
                    | Ok subst -> Some (subst, scheme)
                    | Error _ -> None))

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
                inferMemberAccessTyped env ma arrowType

                // gather results
            let! typ =
                match maAnnex.Type with
                    | TypeArrow (_, outType) -> Ok outType
                    | _ ->
                        Error
                            (InternalError
                                $"Unexpected member access type: {maAnnex.Type.Unparse()}")
            let annex =
                ApplicationExpr {
                    Function = maAnnex
                    Argument = argAnnex
                    Type = typ
                }
            return
                argSubst ++ maSubst,
                annex
        }
