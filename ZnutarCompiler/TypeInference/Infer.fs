namespace Znutar.TypeInference

open Znutar
open Znutar.TypeInference

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
        ]

    module private rec Expression =

        /// Infers and annotates the type of the given expression.
        /// * Substitution used to infer the type
        /// * Equivalent expression fully annotated with inferred types
        let infer env = function
            | Expression.VariableExpr ident -> inferVariable env ident
            | Expression.LambdaExpr lam -> inferLambda env lam
            | Expression.ApplicationExpr app -> inferApplication env app
            | Expression.LetExpr letb -> inferLet env letb
            | Expression.IfExpr iff -> inferIf env iff
            | Expression.BinaryOperationExpr bop ->
                inferBinaryOperation env bop
            | Expression.LiteralExpr lit ->
                Ok (Substitution.empty, LiteralExpr lit)
            | AnnotationExpr ann -> inferAnnotation env ann

        /// Infers the type of a variable by looking it up in the
        /// given environment.
        let private inferVariable env ident =
            result {
                let! scheme = TypeEnvironment.tryFind ident env
                let annex =
                    VariableExpr {
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
                    // e.g. let f = arg_refers_to_f
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

        let private inferIf env iff =
            result {
                let! condSubst, condAnnex =
                    infer env iff.Condition
                let! trueSubst, trueAnnex =
                    infer env iff.TrueBranch
                let! falseSubst, falseAnnex =
                    infer env iff.FalseBranch
                let! condSubst' = unify condAnnex.Type Type.bool
                let! branchSubst = unify trueAnnex.Type falseAnnex.Type
                let typ = Type.apply branchSubst trueAnnex.Type
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

        let private inferBinaryOperation env bop =
            result {
                let! leftSubst, leftAnnex =
                    infer env bop.Left
                let! rightSubst, rightAnnex =
                    infer env bop.Right
                let resultType = createFreshTypeVariable "bop"
                let! arrowSubst =
                    unify
                        (leftAnnex.Type ^=> rightAnnex.Type ^=> resultType)
                        binOpMap[bop.Operator]
                let typ = Type.apply arrowSubst resultType
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

        let private inferAnnotation env ann =
            result {
                let! exprSubst, exprAnnex =
                    infer env ann.Expression
                let! typeSubst = unify exprAnnex.Type ann.Type
                return
                    exprSubst ++ typeSubst,
                    exprAnnex
            }

    let inferExpression expr =
        expr
            |> Expression.infer TypeEnvironment.empty
            |> Result.map snd
