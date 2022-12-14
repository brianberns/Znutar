namespace Znutar.TypeInference

open Znutar
open Znutar.TypeInference

/// Type inference.
// See p. 91 of https://web.archive.org/web/20220524212025/http://dev.stephendiehl.com/fun/WYAH.pdf
module Infer =   // to-do: replace with constraint-based inference

    open Substitution

    let mutable private count = 0

    let private createFreshTypeVariable (prefix : string) =
        count <- count + 1
        Type.variable $"{prefix}{count}"

    /// Creates a fresh type from the given scheme.
    let private instantiate scheme =
        let subst =
            (Substitution.empty, scheme.TypeVariables)
                ||> List.fold (fun acc tv ->
                    let typ = createFreshTypeVariable tv.Name
                    acc |> Map.add tv typ)
        Type.apply subst scheme.Type

    /// Creates a scheme for the given type.
    let private generalize env typ =
        let tvs =
            Set.toList (
                Type.freeTypeVariables typ
                    - TypeEnvironment.freeTypeVariables env)
        Scheme.create tvs typ

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
            | Expression.IdentifierExpr ident -> inferIdent env ident
            | Expression.LambdaExpr lam -> inferLambda env lam
            | Expression.ApplicationExpr app -> inferApplication env app
            | Expression.LetExpr letb -> inferLet env letb
            | Expression.IfExpr iff -> inferIf env iff
            | Expression.BinaryOperationExpr bop ->
                inferBinaryOperation env bop
            | Expression.LiteralExpr lit ->
                Ok (Substitution.empty, LiteralExpr lit)
            | AnnotationExpr ann -> inferAnnotation env ann
            | Expression.MemberAccessExpr ma ->
                inferMemberAccess env ma
            | Expression.TupleExpr exprs ->
                inferTuple env exprs

        /// Infers the type of a identifier by looking it up in the
        /// given environment.
        let private inferIdent env ident =
            result {
                let! scheme = TypeEnvironment.tryFind ident env
                let annex =
                    IdentifierExpr {
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
                    createFreshTypeVariable lam.Identifier.Name
                let env' =
                    let scheme = Scheme.create [] identType
                    TypeEnvironment.add lam.Identifier scheme env

                    // infer the output type using the input type
                let! bodySubst, bodyAnnex =
                    infer env' lam.Body

                    // gather results
                let typ =
                    let identType' = Type.apply bodySubst identType
                    identType' ^=> bodyAnnex.Type
                let annex =
                    LambdaExpr {
                        Identifier = lam.Identifier
                        Body = bodyAnnex
                        Type = typ
                    }
                return bodySubst, annex
            }

        /// Infers the type of a function application.
        let private inferApplication env app =
            result {
                    // infer the function type (must be an arrow)
                let! funSubst, funAnnex =
                    infer env app.Function

                    // infer the input type
                let! argSubst, argAnnex =
                    let env' = TypeEnvironment.apply funSubst env
                    infer env' app.Argument

                    // unify (input ^=> output) with function type
                let outType = createFreshTypeVariable "app"
                let! appSubst =
                    let funType = Type.apply argSubst funAnnex.Type
                    unify funType (argAnnex.Type ^=> outType)

                    // gather results
                let subst = funSubst ++ argSubst ++ appSubst
                let typ = Type.apply appSubst outType
                let annex =
                    ApplicationExpr {
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
                            createFreshTypeVariable "arg"
                                |> generalize env
                        TypeEnvironment.add
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
                        TypeEnvironment.add
                            letb.Identifier scheme env''
                    infer env''' letb.Body

                    // gather result
                let annex =
                    LetExpr {
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
                    IfExpr {
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
                let resultType = createFreshTypeVariable "bop"

                    // must match expected scheme
                let! arrowSubst =
                    unify
                        (leftAnnex.Type ^=> rightAnnex.Type ^=> resultType)
                        binOpMap[bop.Operator]
                let typ = Type.apply arrowSubst resultType

                    // gather results
                let annex =
                    BinaryOperationExpr {
                        Operator = bop.Operator
                        Left = leftAnnex
                        Right = rightAnnex
                        Type = typ
                    }
                return
                    leftSubst ++ rightSubst ++ arrowSubst,
                    annex
            }

        /// Checks the type of an annotation.
        let private inferAnnotation env ann =
            result {
                    // infer acutal sub-expression type
                let! exprSubst, exprAnnex =
                    infer env ann.Expression

                    // must match annotated type
                let! typeSubst = unify exprAnnex.Type ann.Type

                    // gather results
                return
                    exprSubst ++ typeSubst,
                    exprAnnex
            }

        /// Infers the type of a member access.
        let private inferMemberAccess env ma =
            Error (InternalError "oops")

        /// Infers the type of a tuple.
        let private inferTuple env exprs =
            result {

                    // infer first item's type
                let! subst1, annex1 =
                    infer env exprs.Item1

                    // infer second item's type
                let! subst2, annex2 =
                    infer env exprs.Item2

                    // infer subsequent items' types
                let! pairs =
                    exprs.Rest
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
                    TupleExpr {
                        Expressions = annexs
                        Type = typ
                    }
                return subst, annex
            }

    /// Infers the type of the given expression.
    let inferExpression refAssemblies expr =
        expr
            |> Expression.infer TypeEnvironment.empty
            |> Result.map snd
