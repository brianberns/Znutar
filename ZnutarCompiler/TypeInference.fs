namespace Znutar

module TypeInference =   // to-do: replace with constraint-based inference

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

    /// From: let const = fun x -> fun y -> x in body
    /// To:   let const (x, y) = x in body
    let tryConvert (scheme : Scheme) (letb : AnnotatedLetBinding) =

        let rec gatherLambdas = function
            | LambdaExpr lam ->
                lam :: gatherLambdas lam.Body
            | _ -> []

        let lams = gatherLambdas letb.Argument
        if lams.Length = 0 then None
        else
            Some {
                Identifier = letb.Identifier
                Arguments =
                    lams
                        |> List.map (fun lam -> lam.Identifier)
                Body =
                    let lam = List.last lams
                    lam.Body
                Scheme = scheme
            }

    /// Infers and annotates the type of the given expression.
    /// * Substitution used to infer the type
    /// * Equivalent expression fully annotated with inferred types
    let rec inferExpression env = function
        | Expression.VariableExpr ident -> inferVariable env ident
        | Expression.LambdaExpr lam -> inferLambda env lam
        | Expression.ApplicationExpr app -> inferApplication env app
        | Expression.LetExpr letb -> inferLet env letb
        | Expression.IfExpr iff -> inferIf env iff
        | Expression.FixExpr expr -> inferFix env expr
        | Expression.BinaryOperationExpr bop ->
            inferBinaryOperation env bop
        | Expression.LiteralExpr lit ->
            Ok (Substitution.empty, LiteralExpr lit)
        | AnnotationExpr ann -> inferAnnotation env ann

    /// Infers the type of a variable by looking it up in the
    /// given environment.
    and private inferVariable env ident =
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
    and private inferLambda env lam =
        result {
                // create an input type
            let identType =
                createFreshTypeVariable lam.Identifier.Name
            let env' =
                let scheme = Scheme.create [] identType
                TypeEnvironment.add lam.Identifier scheme env

                // infer the output type using the input type
            let! bodySubst, bodyAnnex =
                inferExpression env' lam.Body

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
    and private inferApplication env app =
        result {
                // infer the function type (must be an arrow)
            let! funSubst, funAnnex =
                inferExpression env app.Function

                // infer the input type
            let! argSubst, argAnnex =
                let env' = TypeEnvironment.apply funSubst env
                inferExpression env' app.Argument

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
    and private inferLet env letb =
        result {
                // infer argument type
            let! argSubst, argAnnex =
                inferExpression env letb.Argument
            let env' = TypeEnvironment.apply argSubst env

                // generalize argument ("let polymorphism")
                // e.g. let id = fun x -> x in ...
            let scheme = generalize env' argAnnex.Type

                // infer body type using argument type
            let! bodySubst, bodyAnnex =
                let env'' =
                    TypeEnvironment.add
                        letb.Identifier scheme env'
                inferExpression env'' letb.Body

                // gather result
            let annex =
                let letb' =
                    {
                        Identifier = letb.Identifier
                        Argument = argAnnex
                        Body = bodyAnnex
                        Type = bodyAnnex.Type
                    }
                letb'
                    |> tryConvert scheme
                    |> Option.map FunctionExpr
                    |> Option.defaultValue (
                        LetExpr letb')
            return argSubst ++ bodySubst, annex
        }

    and private inferIf env iff =
        result {
            let! condSubst, condAnnex =
                inferExpression env iff.Condition
            let! trueSubst, trueAnnex =
                inferExpression env iff.TrueBranch
            let! falseSubst, falseAnnex =
                inferExpression env iff.FalseBranch
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

    and private inferFix env expr =
        result {
            let! exprSubst, exprAnnex =
                inferExpression env expr
            let freshType = createFreshTypeVariable "fix"
            let! arrowSubst =
                unify (freshType ^=> freshType) exprAnnex.Type
            let typ = Type.apply exprSubst freshType
            let annex =
                FixExpr {
                    Expression = exprAnnex
                    Type = exprAnnex.Type
                }
            return arrowSubst, annex
        }

    and private inferBinaryOperation env bop =
        result {
            let! leftSubst, leftAnnex =
                inferExpression env bop.Left
            let! rightSubst, rightAnnex =
                inferExpression env bop.Right
            let freshType = createFreshTypeVariable "bop"
            let! arrowSubst =
                unify
                    (leftAnnex.Type ^=> rightAnnex.Type ^=> freshType)
                    binOpMap[bop.Operator]
            let typ = Type.apply arrowSubst freshType
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

    and private inferAnnotation env ann =
        result {
            let! exprSubst, exprAnnex =
                inferExpression env ann.Expression
            let! typeSubst = unify exprAnnex.Type ann.Type
            return
                exprSubst ++ typeSubst,
                exprAnnex
        }

    let inferDeclaration env (decl : Declaration) =
        count <- 0   // reset for deterministic result
        result {
            let! subst, annBody = inferExpression env decl.Body
            let scheme =
                Type.apply subst annBody.Type
                    |> generalize TypeEnvironment.empty
            let env' =
                TypeEnvironment.add
                    decl.Identifier scheme env
            let annDecl =
                {
                    Identifier = decl.Identifier
                    Body = annBody
                }
            return env', annDecl
        }

    let inferProgram (program : Program) =
        count <- 0   // reset for deterministic result
        result {
            let! env, annDecls =
                ((TypeEnvironment.empty, []), program.Declarations)
                    ||> Result.foldM (fun (accEnv, accDecls) decl ->
                        result {
                            let! accEnv', decl' =
                                inferDeclaration accEnv decl
                            return accEnv', (decl' :: accDecls)
                        })
            let! _subst, annMain =
                inferExpression env program.Main
            let annProgram =
                {
                    Declarations = List.rev annDecls
                    Main = annMain
                }
            return env, annProgram
        }
