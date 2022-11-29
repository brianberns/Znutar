namespace Znutar

module TypeInference =

    open Substitution

    let mutable private count = 0

    let private createFreshTypeVariable (prefix : string) =
        count <- count + 1
        Identifier.create $"{prefix}{count}"
            |> TypeVariable

    let private instantiate scheme =
        let subst =
            (Substitution.empty, scheme.TypeVariables)
                ||> List.fold (fun acc tv ->
                    let typ = createFreshTypeVariable tv.Name
                    acc |> Map.add tv typ)
        Type.apply subst scheme.Type

    let private generalize env typ =
        let tvs =
            Set.toList (
                Type.freeTypeVariables typ
                    - TypeEnvironment.freeTypeVariables env)
        Scheme.create tvs typ

    let private binOpMap =
        Map [
            Plus, Type.int => Type.int => Type.int
            Minus, Type.int => Type.int => Type.int
            Times, Type.int => Type.int => Type.int
            Equals, Type.int => Type.int => Type.bool   // to-do: make polymorphic
        ]

    let rec inferExpression env expr =
        result {
            let! subst, typ, expr' =
                match expr with
                    | VariableExpr ident -> inferVariable env ident
                    | LambdaExpr lam -> inferLambda env lam
                    | ApplicationExpr app -> inferApplication env app
                    | LetExpr letb -> inferLet env letb
                    | IfExpr iff -> inferIf env iff
                    | FixExpr expr -> inferFix env expr
                    | BinaryOperationExpr bop -> inferBinaryOperation env bop
                    | LiteralExpr (IntLiteral _) ->
                        Ok (Substitution.empty, Type.int, expr)
                    | LiteralExpr (BoolLiteral _) ->
                        Ok (Substitution.empty, Type.bool, expr)
                    | AnnotationExpr ann -> inferAnnotation env ann
            let expr'' =
                AnnotationExpr {
                    Expression = expr'
                    Type = typ
                }
            return subst, typ, expr''
        }

    and private inferVariable env ident =
        result {
            let! scheme = TypeEnvironment.tryFind ident env
            let typ = instantiate scheme
            let expr = VariableExpr ident
            return Substitution.empty, typ, expr
        }

    and private inferLambda env lam =
        result {
            let freshType =
                createFreshTypeVariable lam.Identifier.Name
            let env' =
                let scheme = Scheme.create [] freshType
                TypeEnvironment.add lam.Identifier scheme env
            let! bodySubst, bodyType, bodyExpr =
                inferExpression env' lam.Body
            let freshType' = Type.apply bodySubst freshType
            let typ = freshType' => bodyType
            let expr =
                LambdaExpr { lam with Body = bodyExpr }
            return bodySubst, typ, expr
        }

    and private inferApplication env app =
        result {
            let freshType =
                createFreshTypeVariable "app"
            let! funSubst, funType, funExpr =
                inferExpression env app.Function
            let! argSubst, argType, argExpr =
                let env' = TypeEnvironment.apply funSubst env
                inferExpression env' app.Argument
            let! appSubst =
                let funType' = Type.apply argSubst funType
                unify funType' (argType => freshType)
            let typ = Type.apply appSubst freshType
            let expr =
                ApplicationExpr {
                    Function = funExpr
                    Argument = argExpr
                }
            return
                funSubst ++ argSubst ++ appSubst,
                typ,
                expr
        }

    and private inferLet env letb =
        result {
            let! argSubst, argType, argExpr =
                inferExpression env letb.Argument
            let env' = TypeEnvironment.apply argSubst env
            let argType' = generalize env' argType
            let! bodySubst, bodyType, bodyExpr =
                let env'' =
                    TypeEnvironment.add
                        letb.Identifier argType' env'
                inferExpression env'' letb.Body
            let expr =
                LetExpr {
                    letb with
                        Argument = argExpr
                        Body = bodyExpr
                }
            return
                argSubst ++ bodySubst,
                bodyType,
                expr
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
                typ,
                expr
        }

    and private inferFix env expr =
        result {
            let! exprSubst, exprType, exprExpr =
                inferExpression env expr
            let freshType = createFreshTypeVariable "fix"
            let! arrowSubst =
                unify (freshType => freshType) exprType
            let typ = Type.apply exprSubst freshType
            let expr = FixExpr exprExpr
            return
                arrowSubst,
                typ,
                expr
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
                    (leftType => rightType => freshType)
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
                typ,
                expr
        }

    and private inferAnnotation env ann =
        result {
            let! exprSubst, exprType, exprExpr =
                inferExpression env ann.Expression
            let! typeSubst = unify exprType ann.Type
            let typ = Type.apply typeSubst exprType
            return
                exprSubst ++ typeSubst,
                typ,
                exprExpr
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
                            let! accEnv', decl' = inferDeclaration accEnv decl
                            return accEnv', (decl' :: accDecls)
                        })
            let! _subst, typ, main =
                inferExpression env program.Main
            return env, typ
        }
