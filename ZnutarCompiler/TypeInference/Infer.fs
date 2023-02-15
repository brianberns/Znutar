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

    /// Binary operator type signatures.
    let private binOpMap =
        Map [
            Plus, Type.int ^=> Type.int ^=> Type.int
            Minus, Type.int ^=> Type.int ^=> Type.int
            Times, Type.int ^=> Type.int ^=> Type.int
            Equals, Type.int ^=> Type.int ^=> Type.bool   // to-do: make polymorphic
            LessThan, Type.int ^=> Type.int ^=> Type.bool
            GreaterThan, Type.int ^=> Type.int ^=> Type.bool
            Divide, Type.int ^=> Type.int ^=> Type.int
            Modulo, Type.int ^=> Type.int ^=> Type.int
        ]

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
                    // infer the function type (must be an arrow)
                let! funSubst, funAnnex = infer env app.Function

                    // infer the input type
                let! argSubst, argAnnex =
                    let env' = TypeEnvironment.apply funSubst env
                    infer env' app.Argument

                    // unify (input ^=> output) with function type
                let outType = Type.createFreshTypeVariable "app"
                let! appSubst =
                    let funType = Type.apply argSubst funAnnex.Type
                    unify funType (argAnnex.Type ^=> outType)

                    // gather results
                let subst = funSubst ++ argSubst ++ appSubst
                let typ = Type.apply appSubst outType
                let annex =
                    AnnotatedApplicationExpr {
                        Function = funAnnex
                        Argument = argAnnex
                        Type = typ
                    }
                return subst, annex
            }

        /// Infers the type of a let binding.
        let private inferLet env letb =
            result {
                    // add placeholder argument type in case of recursion?
                    // e.g. let rec f = arg_refers_to_f in ...
                let env' =
                    if letb.Recursive then
                        let scheme =
                            Type.createFreshTypeVariable "arg"
                                |> generalize env
                        TypeEnvironment.addFunctional
                            letb.Identifier scheme env   // to-do: allow mutual recursion
                    else env

                    // infer actual argument type
                let! argSubst, argAnnex =
                    infer env' letb.Argument
                let env'' = TypeEnvironment.apply argSubst env'

                    // generalize argument ("let polymorphism")
                    // e.g. let id = fun x -> x in ...
                let scheme = generalize env'' argAnnex.Type

                    // infer body type using argument type
                let! bodySubst, bodyAnnex =
                    let env''' =
                        TypeEnvironment.addFunctional
                            letb.Identifier scheme env''
                    infer env''' letb.Body

                    // gather result
                let annex =
                    AnnotatedLetExpr {
                        Identifier = letb.Identifier
                        Scheme = scheme
                        Argument = argAnnex
                        Body = bodyAnnex
                        Type = bodyAnnex.Type
                    }
                return argSubst ++ bodySubst, annex
            }

        /// Infers the type of an if.
        let private inferIf env iff =
            result {
                    // infer sub-expression types
                let! condSubst, condAnnex =
                    infer env iff.Condition
                let! trueSubst, trueAnnex =
                    infer env iff.TrueBranch
                let! falseSubst, falseAnnex =
                    infer env iff.FalseBranch

                    // condition must be Boolean
                let! condSubst' = unify condAnnex.Type Type.bool

                    // branches must match
                let! branchSubst = unify trueAnnex.Type falseAnnex.Type
                let typ = Type.apply branchSubst trueAnnex.Type

                    // gather results
                let annex =
                    AnnotatedIfExpr {
                        Condition = condAnnex
                        TrueBranch = trueAnnex
                        FalseBranch = falseAnnex
                        Type = typ
                    }
                return
                    condSubst ++ trueSubst ++ falseSubst
                        ++ condSubst' ++ branchSubst,
                    annex
            }

        /// Infers the type of a binary operation.
        let private inferBinaryOperation env bop =
            result {
                    // infer sub-expression types
                let! leftSubst, leftAnnex =
                    infer env bop.Left
                let! rightSubst, rightAnnex =
                    infer env bop.Right
                let resultType = Type.createFreshTypeVariable "bop"

                    // must match expected scheme
                let! arrowSubst =
                    unify
                        (leftAnnex.Type ^=> rightAnnex.Type ^=> resultType)
                        binOpMap[bop.Operator]
                let typ = Type.apply arrowSubst resultType

                    // gather results
                let annex =
                    AnnotatedBinaryOperationExpr {
                        Operator = bop.Operator
                        Left = leftAnnex
                        Right = rightAnnex
                        Type = typ
                    }
                return
                    leftSubst ++ rightSubst ++ arrowSubst,
                    annex
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
                    exprSubst ++ typeSubst,
                    annex
            }

        /// Infers the type of a member access.
        let private inferMemberAccess env ma =
            inferMemberAccessWith env ma MemberAccess.tryResolveOne

        /// Infers the type of a tuple.
        let private inferTuple env exprs =
            result {

                    // infer first item's type
                let! subst1, annex1 =
                    infer env exprs.Head1

                    // infer second item's type
                let! subst2, annex2 =
                    infer env exprs.Head2

                    // infer subsequent items' types
                let! pairs =
                    exprs.Tail
                        |> Result.traverse (infer env)
                let restSubsts, restAnnexs = List.unzip pairs

                    // gather results
                let subst =
                    subst1 :: subst2 :: restSubsts
                        |> List.reduce Substitution.compose
                let annex =
                    let annexs =
                        MultiItemList.create annex1 annex2 restAnnexs
                    let typ =
                        annexs
                            |> MultiItemList.map (fun annex -> annex.Type)
                            |> TypeTuple
                            |> Type.apply subst
                    AnnotatedTupleExpr {
                        Expressions = annexs
                        Type = typ
                    }
                return subst, annex
            }

    /// Infers the type of the given expression.
    let inferExpression assemblies expr =
        let env = TypeEnvironment.create assemblies
        expr
            |> Expression.infer env
            |> Result.map snd
