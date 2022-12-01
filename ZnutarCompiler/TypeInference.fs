namespace Znutar

module TypeInference =   // to-do: replace with constraint-based inference

    open Substitution

    let mutable private count = 0

    let private createFreshTypeVariable (prefix : string) =
        count <- count + 1
        Type.variable $"{prefix}{count}"

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

    /// Annotates the given expression with the given type.
    let private annotate typ = function

            // annotation not needed
        | LiteralExpr (IntLiteral _) as expr ->
            assert(typ = Type.int)
            expr
        | LiteralExpr (BoolLiteral _) as expr ->
            assert(typ = Type.bool)
            expr
        | AnnotationExpr inner as expr ->
            assert(inner.Type = typ)
            expr

            // create annotation
        | expr ->
            AnnotationExpr {
                Expression = expr
                Type = typ
            }

    /// Infers the type of the given expression. Returns:
    /// * Substitution used to infer the type
    /// * The inferred type
    /// * Equivalent expression fully annotated with inferred types
    let rec inferExpression env expr =
        result {
                // infer expression type
            let! subst, typ, expr' =
                match expr with
                    | VariableExpr ident -> inferVariable env ident
                    | LambdaExpr lam -> inferLambda env lam
                    | ApplicationExpr app -> inferApplication env app
                    | LetExpr letb -> inferLet env letb
                    | IfExpr iff -> inferIf env iff
                    | FixExpr expr -> inferFix env expr
                    | BinaryOperationExpr bop ->
                        inferBinaryOperation env bop
                    | LiteralExpr (IntLiteral _) ->
                        Ok (Substitution.empty, Type.int, expr)
                    | LiteralExpr (BoolLiteral _) ->
                        Ok (Substitution.empty, Type.bool, expr)
                    | AnnotationExpr ann -> inferAnnotation env ann

                // annotate expression with inferred type
            let expr'' =
                expr'
                    |> Expression.apply subst   // to-do: apply final substitution once at the end, instead of every step
                    |> annotate typ
            return subst, typ, expr''
        }

    /// Infers the type of a variable by looking it up in the
    /// given environment.
    and private inferVariable env ident =
        result {
            let! scheme = TypeEnvironment.tryFind ident env
            let typ = instantiate scheme
            let expr = VariableExpr ident
            return Substitution.empty, typ, expr
        }

    /// Infers the type of a lambda abstraction.
    and private inferLambda env lam =
        result {
                // create an input type
            let identType =
                createFreshTypeVariable lam.Identifier.Name
            let env' =
                let scheme = Scheme.create [] identType
                TypeEnvironment.add lam.Identifier scheme env

                // infer the output type using the input type
            let! bodySubst, bodyType, bodyExpr =
                inferExpression env' lam.Body

                // gather results
            let typ =
                let identType' = Type.apply bodySubst identType
                identType' ^=> bodyType
            let expr =
                LambdaExpr { lam with Body = bodyExpr }
            return bodySubst, typ, expr
        }

    /// Infers the type of a function application.
    and private inferApplication env app =
        result {
                // infer the function type (must be an arrow)
            let! funSubst, funType, funExpr =
                inferExpression env app.Function

                // infer the input type
            let! argSubst, argType, argExpr =
                let env' = TypeEnvironment.apply funSubst env
                inferExpression env' app.Argument

                // unify (input ^=> output) with function type
            let outType = createFreshTypeVariable "app"
            let! appSubst =
                let funType' = Type.apply argSubst funType
                unify funType' (argType ^=> outType)

                // gather results
            let subst = funSubst ++ argSubst ++ appSubst
            let typ = Type.apply appSubst outType
            let expr =
                ApplicationExpr {
                    Function = funExpr
                    Argument = argExpr
                }
            return subst, typ, expr
        }

    /// Infers the type of a let binding.
    and private inferLet env letb =
        result {
                // infer argument type
            let! argSubst, argType, argExpr =
                inferExpression env letb.Argument
            let env' = TypeEnvironment.apply argSubst env

                // generalize argument ("let polymorphism")
                // e.g. let id arg = arg
            let argType' = generalize env' argType

                // infer body type using argument type
            let! bodySubst, bodyType, bodyExpr =
                let env'' =
                    TypeEnvironment.add
                        letb.Identifier argType' env'
                inferExpression env'' letb.Body

                // gather result
            let expr =
                LetExpr {
                    letb with
                        Argument = argExpr
                        Body = bodyExpr
                }
            return
                argSubst ++ bodySubst,
                bodyType, expr
        }

    and private inferIf env iff =
        result {
            let! condSubst, condType, condExpr =
                inferExpression env iff.Condition
            let! trueSubst, trueType, trueExpr =
                inferExpression env iff.TrueBranch
            let! falseSubst, falseType, falseExpr =
                inferExpression env iff.FalseBranch
            let! condSubst' = unify condType Type.bool
            let! branchSubst = unify trueType falseType
            let typ = Type.apply branchSubst trueType
            let expr =
                IfExpr {
                    Condition = condExpr
                    TrueBranch = trueExpr
                    FalseBranch = falseExpr
                }
            return
                condSubst ++ trueSubst ++ falseSubst
                    ++ condSubst' ++ branchSubst,
                typ, expr
        }

    and private inferFix env expr =
        result {
            let! exprSubst, exprType, exprExpr =
                inferExpression env expr
            let freshType = createFreshTypeVariable "fix"
            let! arrowSubst =
                unify (freshType ^=> freshType) exprType
            let typ = Type.apply exprSubst freshType
            let expr = FixExpr exprExpr
            return arrowSubst, typ, expr
        }

    and private inferBinaryOperation env bop =
        result {
            let! leftSubst, leftType, leftExpr =
                inferExpression env bop.Left
            let! rightSubst, rightType, rightExpr =
                inferExpression env bop.Right
            let freshType = createFreshTypeVariable "bop"
            let! arrowSubst =
                unify
                    (leftType ^=> rightType ^=> freshType)
                    binOpMap[bop.Operator]
            let typ = Type.apply arrowSubst freshType
            let expr =
                BinaryOperationExpr {
                    bop with
                        Left = leftExpr
                        Right = rightExpr
                }
            return
                leftSubst ++ rightSubst ++ arrowSubst,
                typ, expr
        }

    and private inferAnnotation env ann =
        result {
            let! exprSubst, exprType, exprExpr =
                inferExpression env ann.Expression
            let! typeSubst = unify exprType ann.Type
            let typ = Type.apply typeSubst exprType
            return
                exprSubst ++ typeSubst,
                typ, exprExpr
        }

    let inferDeclaration env decl =
        result {
            let! subst, typ, body = inferExpression env decl.Body
            let scheme =
                Type.apply subst typ
                    |> generalize TypeEnvironment.empty
            let env' =
                TypeEnvironment.add
                    decl.Identifier scheme env
            let decl' = { decl with Body = body }
            return env', decl'
        }

    let inferProgram program =
        result {
            count <- 0   // reset for deterministic result
            let! env, decls =
                ((TypeEnvironment.empty, []), program.Declarations)
                    ||> Result.foldM (fun (accEnv, accDecls) decl ->
                        result {
                            let! accEnv', decl' =
                                inferDeclaration accEnv decl
                            return accEnv', (decl' :: accDecls)
                        })
            let! _subst, typ, main =
                inferExpression env program.Main
            let program' =
                {
                    Declarations = List.rev decls
                    Main = main
                }
            return env, typ, program'
        }
