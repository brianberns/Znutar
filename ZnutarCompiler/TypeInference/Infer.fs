namespace Znutar.TypeInference

open Znutar
open Znutar.TypeInference
open Substitution

/// Type inference.
// See p. 91 of https://web.archive.org/web/20220524212025/http://dev.stephendiehl.com/fun/WYAH.pdf
module Infer =   // to-do: replace with constraint-based inference

    /// Creates a fresh type from the given scheme.
    let private instantiate scheme =
        let subst =
            (Substitution.empty, scheme.TypeVariables)
                ||> List.fold (fun acc tv ->
                    let typ = Type.createFreshTypeVariable tv.Name
                    acc |> Map.add tv typ)
        Type.apply subst scheme.Type

    /// Creates a scheme for the given type.
    let private generalize env typ =
        let tvs =
            Set.toList (
                Type.freeTypeVariables typ
                    - TypeEnvironment.freeTypeVariables env)
        Scheme.create tvs typ

    /// Binary operator type signatures. Equality is polymorphic,
    /// so its type is created fresh for each use.
    let private getBinaryOperatorType = function
        | Plus | Minus | Times | Divide | Modulo ->
            Type.int ^=> Type.int ^=> Type.int
        | LessThan | GreaterThan ->
            Type.int ^=> Type.int ^=> Type.bool
        | Equals ->
            let tv = Type.createFreshTypeVariable "eq"
            tv ^=> tv ^=> Type.bool

    module private rec Expression =

        /// Infers and annotates the type of the given expression.
        /// * Substitution used to infer the type
        /// * Equivalent expression fully annotated with inferred types
        let infer env = function
            | IdentifierExpr ident -> inferIdent env ident
            | LambdaExpr lam -> inferLambda env lam
            | ApplicationExpr app -> inferApplication env app
            | LetExpr letb -> inferLet env letb
            | IfExpr iff -> inferIf env iff
            | BinaryOperationExpr bop ->
                inferBinaryOperation env bop
            | LiteralExpr lit ->
                Ok (Substitution.empty, AnnotatedLiteralExpr lit)
            | AnnotationExpr ann -> inferAnnotation env ann
            | MemberAccessExpr ma -> inferMemberAccess env ma
            | TupleExpr exprs -> inferTuple env exprs

        /// Infers the types of the given expressions in order, threading
        /// each resulting substitution into the inference of the next.
        /// Answers the combined substitution, the annotated expressions,
        /// and their types (with the combined substitution applied).
        let private inferMany env exprs =
            result {
                let! subst, annexesRev =
                    ((Substitution.empty, []), exprs)
                        ||> Result.foldM (fun (subst, annexes) expr ->
                            result {
                                let env' = TypeEnvironment.apply subst env
                                let! subst', annex = infer env' expr
                                return subst' ++ subst, annex :: annexes
                            })
                let annexes = List.rev annexesRev
                let types =
                    annexes
                        |> List.map (fun (annex : AnnotatedExpression) ->
                            Type.apply subst annex.Type)
                return subst, annexes, types
            }

        /// Infers the type of a identifier by looking it up in the
        /// given environment.
        let private inferIdent env ident =
            result {
                let! scheme =
                    TypeEnvironment.tryFindFunctional ident env
                let annex =
                    AnnotatedIdentifierExpr {
                        Identifier = ident
                        Type = instantiate scheme
                    }
                return Substitution.empty, annex
            }

        /// Infers the type of a lambda abstraction.
        let private inferLambda env lam =
            result {
                    // create an input type
                let identType =
                    Type.createFreshTypeVariable lam.Identifier.Name
                let env' =
                    let scheme = Scheme.create [] identType
                    TypeEnvironment.addFunctional lam.Identifier scheme env

                    // infer the output type using the input type
                let! bodySubst, bodyAnnex =
                    infer env' lam.Body

                    // gather results
                let typ =
                    let identType' = Type.apply bodySubst identType
                    identType' ^=> bodyAnnex.Type
                let annex =
                    AnnotatedLambdaExpr {
                        Identifier = lam.Identifier
                        Body = bodyAnnex
                        Type = typ
                    }
                return bodySubst, annex
            }

        /// Infers the type of a functional or member application.
        let private inferApplication env app =
            match app.Function with
                | MemberAccessExpr ma ->
                    MemberAccess.inferMemberApplication
                        infer env ma app.Argument
                | _ ->
                    inferFunctionalApplication env app

        /// Infers the type of a functional application.
        let private inferFunctionalApplication env app =
            result {
                    // infer the function and input types
                let! subst, annexes, types =
                    inferMany env [ app.Function; app.Argument ]
                let funAnnex, argAnnex, funType, argType =
                    match annexes, types with
                        | [ funAnnex; argAnnex ], [ funType; argType ] ->
                            funAnnex, argAnnex, funType, argType
                        | _ -> failwith "Unexpected"

                    // unify (input ^=> output) with function type
                let outType = Type.createFreshTypeVariable "app"
                let! appSubst =
                    unify funType (argType ^=> outType)

                    // gather results
                let annex =
                    AnnotatedApplicationExpr {
                        Function = funAnnex
                        Argument = argAnnex
                        Type = Type.apply appSubst outType
                    }
                return appSubst ++ subst, annex
            }

        /// Infers the type of a let binding.
        let private inferLet env letb =
            result {
                    // infer argument type, using a monomorphic placeholder
                    // for the identifier in case of recursion
                    // e.g. let rec f = arg_refers_to_f in ...
                let! argSubst, argAnnex =
                    if letb.Recursive then
                        result {
                            let placeholder = Type.createFreshTypeVariable "rec"
                            let env' =
                                TypeEnvironment.addFunctional   // to-do: allow mutual recursion
                                    letb.Identifier
                                    (Scheme.create [] placeholder)
                                    env
                            let! argSubst, argAnnex = infer env' letb.Argument

                                // placeholder must match actual argument type
                            let! recSubst =
                                unify
                                    (Type.apply argSubst placeholder)
                                    argAnnex.Type
                            return recSubst ++ argSubst, argAnnex
                        }
                    else
                        infer env letb.Argument
                let argType = Type.apply argSubst argAnnex.Type
                let env' = TypeEnvironment.apply argSubst env

                    // generalize argument ("let polymorphism"), but only
                    // if it's a function ("value restriction")
                    // e.g. let id = fun x -> x in ...
                let scheme =
                    match letb.Argument with
                        | LambdaExpr _ -> generalize env' argType
                        | _ -> Scheme.create [] argType

                    // infer body type using argument type
                let! bodySubst, bodyAnnex =
                    let env'' =
                        TypeEnvironment.addFunctional
                            letb.Identifier scheme env'
                    infer env'' letb.Body

                    // gather result
                let annex =
                    AnnotatedLetExpr {
                        Recursive = letb.Recursive
                        Identifier = letb.Identifier
                        Scheme = scheme
                        Argument = argAnnex
                        Body = bodyAnnex
                        Type = bodyAnnex.Type
                    }
                return bodySubst ++ argSubst, annex
            }

        /// Infers the type of an if.
        let private inferIf env iff =
            result {
                    // infer sub-expression types
                let! subst, annexes, types =
                    inferMany env [
                        iff.Condition
                        iff.TrueBranch
                        iff.FalseBranch
                    ]
                let (condAnnex, trueAnnex, falseAnnex), (condType, trueType, falseType) =
                    match annexes, types with
                        | [ condAnnex; trueAnnex; falseAnnex ], [ condType; trueType; falseType ] ->
                            (condAnnex, trueAnnex, falseAnnex), (condType, trueType, falseType)
                        | _ -> failwith "Unexpected"

                    // condition must be Boolean
                let! condSubst = unify condType Type.bool

                    // branches must match
                let! branchSubst =
                    unify
                        (Type.apply condSubst trueType)
                        (Type.apply condSubst falseType)
                let subst' = branchSubst ++ condSubst ++ subst

                    // gather results
                let annex =
                    AnnotatedIfExpr {
                        Condition = condAnnex
                        TrueBranch = trueAnnex
                        FalseBranch = falseAnnex
                        Type = Type.apply subst' trueType
                    }
                return subst', annex
            }

        /// Infers the type of a binary operation.
        let private inferBinaryOperation env bop =
            result {
                    // infer sub-expression types
                let! subst, annexes, types =
                    inferMany env [ bop.Left; bop.Right ]
                let leftAnnex, rightAnnex, leftType, rightType =
                    match annexes, types with
                        | [ leftAnnex; rightAnnex ], [ leftType; rightType ] ->
                            leftAnnex, rightAnnex, leftType, rightType
                        | _ -> failwith "Unexpected"

                    // must match expected signature
                let resultType = Type.createFreshTypeVariable "bop"
                let! arrowSubst =
                    unify
                        (leftType ^=> rightType ^=> resultType)
                        (getBinaryOperatorType bop.Operator)

                    // gather results
                let annex =
                    AnnotatedBinaryOperationExpr {
                        Operator = bop.Operator
                        Left = leftAnnex
                        Right = rightAnnex
                        Type = Type.apply arrowSubst resultType
                    }
                return arrowSubst ++ subst, annex
            }

        /// Infers the type of a member access.
        let private inferMemberAccessWith =
            MemberAccess.inferMemberAccessWith infer

        /// Infers the type of an annotation.
        let private inferAnnotation env ann =
            match ann.Expression with

                    // pick member that matches the annotation type
                | MemberAccessExpr ma ->
                    MemberAccess.tryResolveUnify ann.Type
                        |> inferMemberAccessWith env ma

                    // must be a functional annotation
                | _ -> inferFunctionalAnnotation env ann

        /// Infers the type of a functional annotation.
        let private inferFunctionalAnnotation env ann =
            result {
                    // infer underlying sub-expression type
                let! exprSubst, exprAnnex =
                    infer env ann.Expression

                    // must be compatible with annotated type
                let! typeSubst = unify exprAnnex.Type ann.Type

                    // gather results
                let annex =
                    AnnotatedExpression.apply typeSubst exprAnnex
                return
                    typeSubst ++ exprSubst,
                    annex
            }

        /// Infers the type of a member access.
        let private inferMemberAccess env ma =
            inferMemberAccessWith env ma MemberAccess.tryResolveOne

        /// Infers the type of a tuple.
        let private inferTuple env exprs =
            result {
                let! subst, annexes, types =
                    inferMany env (List.ofSeq exprs)
                let annex =
                    match annexes, types with
                        | annex1 :: annex2 :: annexes,
                          type1 :: type2 :: types ->
                            AnnotatedTupleExpr {
                                Expressions =
                                    MultiItemList.create annex1 annex2 annexes
                                Type =
                                    MultiItemList.create type1 type2 types
                                        |> TypeTuple
                            }
                        | _ -> failwith "Unexpected"
                return subst, annex
            }

    /// Infers the type of the given expression.
    let inferExpression assemblies expr =
        let env = TypeEnvironment.create assemblies
        expr
            |> Expression.infer env
            |> Result.map (fun (subst, annex) ->

                    // make inner annotations consistent with the final result
                AnnotatedExpression.apply subst annex)
