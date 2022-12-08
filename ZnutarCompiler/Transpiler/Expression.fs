namespace Znutar.Transpiler

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Znutar
open Znutar.Parser
open Znutar.TypeInference

module Expression =

    let private transpileLiteral (lit : Literal) =
        let node =
            match lit with
                | IntLiteral n ->
                    LiteralExpression(
                        SyntaxKind.NumericLiteralExpression,
                        Literal(n))
                        :> Syntax.ExpressionSyntax
                | BoolLiteral b ->
                    let kind =
                        if b then SyntaxKind.TrueLiteralExpression
                        else SyntaxKind.FalseLiteralExpression
                    LiteralExpression(kind)
        Ok ([], node)

    let rec private transpileExpr = function
        | VariableExpr var -> transpileIdentifier var.Identifier
        // | ApplicationExpr app -> transpileApplication venv app
        | LetExpr letb -> transpileLet letb
        // | IfExpr iff -> transpileIf venv iff
        // | FixExpr expr -> transpileFix venv expr
        | BinaryOperationExpr bop -> transpileBinaryOperation bop
        | LiteralExpr lit -> transpileLiteral lit
        // | LambdaExpr lam -> cerror (Unsupported "Unannotated lambda")

    and private transpileIdentifier (ident : Identifier) =
        Ok ([], IdentifierName(ident.Name))

    and private transpileLet letb =
        match Function.tryCreate letb with
            | Some func -> transpileFunction func
            | None -> transpileLetRaw letb

    and private transpileLetRaw letb =
        result {
            let typeNode = Type.transpile letb.Argument.Type
            let! argStmtNodes, argExprNode = transpileExpr letb.Argument   // argStmtNodes: int x = 1, argExprNode: 2 * x
            let! bodyStmtNodes, bodyExprNode = transpileExpr letb.Body     // bodyStmtNodes: int z = 3, bodyExprNode: y + z
            let stmtNode : Syntax.StatementSyntax =                      // stmtNode: int y = 2 * x
                LocalDeclarationStatement(
                    VariableDeclaration(typeNode)
                        .WithVariables(
                            SingletonSeparatedList(
                                VariableDeclarator(
                                    Identifier(letb.Identifier.Name))
                                    .WithInitializer(
                                        EqualsValueClause(
                                            argExprNode)))))
            let stmtNodes =
                [
                    yield! argStmtNodes
                    yield stmtNode
                    yield! bodyStmtNodes
                ]
            return stmtNodes, bodyExprNode
        }

    and private transpileFunction (func : Function) =

        let rec gatherTypes = function
            | TypeArrow (inpType, outType) ->
                inpType :: gatherTypes outType
            | typ -> [typ]

        result {
            let types = gatherTypes func.Scheme.Type
            let! argTypes, returnType =
                match List.rev types with
                    | [] | [_] ->
                        cerror (
                            Unsupported $"Invalid function type: \
                                {func.Scheme.Type.Unparse()}")
                    | returnType :: argTypesRev ->
                        Ok (List.rev argTypesRev, returnType)
            if argTypes.Length = func.Arguments.Length then
                let argPairs = List.zip func.Arguments argTypes
                let returnTypeNode = Type.transpile returnType
                let typeParmNodes =
                    func.Scheme.TypeVariables
                        |> List.map (fun tv ->
                            TypeParameter(Identifier(tv.Name)))
                let parmNodes =
                    argPairs
                        |> List.map (fun (ident, typ) ->
                            Parameter(
                                Identifier(ident.Name))
                                .WithType(Type.transpile typ))
                let! bodyStmtNodes, bodyExprNode = transpileExpr func.FunctionBody
                let! nextStmtNodes, nextExprNode = transpileExpr func.ExpressionBody
                let bodyStmtNodes' =
                    [|
                        yield! bodyStmtNodes
                        yield ReturnStatement(bodyExprNode)
                    |]
                let funcNode =
                    LocalFunctionStatement(
                        returnType = returnTypeNode,
                        identifier = func.Identifier.Name)
                        .WithTypeParameterList(
                            TypeParameterList(
                                Syntax.separatedList typeParmNodes))
                        .WithParameterList(
                            ParameterList(
                                Syntax.separatedList parmNodes))
                        .WithBody(
                            Block(bodyStmtNodes'))
                let stmtNodes =
                    [
                        yield funcNode :> Syntax.StatementSyntax
                        yield! nextStmtNodes
                    ]
                return stmtNodes, nextExprNode
            else
                return! cerror (Unsupported "Function arity mismatch")
        }

    and private transpileBinaryOperation bop =
        let kind =
            match bop.Operator with
                | Plus -> SyntaxKind.AddExpression
                | Minus -> SyntaxKind.SubtractExpression
                | Times -> SyntaxKind.MultiplyExpression
                | Equals -> SyntaxKind.EqualsExpression
        result {
            let! leftStmtNodes, leftExprNode = transpileExpr bop.Left
            let! rightStmtNodes, rightExprNode = transpileExpr bop.Right
            let stmtNodes =
                [
                    yield! leftStmtNodes
                    yield! rightStmtNodes
                ]
            let exprNode =
                BinaryExpression(
                    kind,
                    leftExprNode,
                    rightExprNode)
            return stmtNodes, exprNode
        }

    (*
    /// E.g. ((System.Func<int, int>)(x => x + 1))
    and transpileLambda venv typ (lam : LambdaAbstraction) =
        result {
            let venv' =
                let identNode : Syntax.ExpressionSyntax =
                    IdentifierName(lam.Identifier.Name)
                Map.add lam.Identifier identNode venv
            let! bodyNode, _ = transpile venv' lam.Body
            let node =
                ParenthesizedExpression(
                    CastExpression(
                        Type.transpile typ,
                        ParenthesizedExpression(
                            SimpleLambdaExpression(
                                Parameter(
                                    Identifier(lam.Identifier.Name)))
                                .WithExpressionBody(bodyNode))))
            return node, venv
        }

    and transpileApplication venv (app : Application) =
        result {

            let! funcNode =
                match transpile venv app.Function with
                    | Ok (node, _) -> Ok node
                    | Error cerr ->
                        match cerr with
                            | :? UnboundVariable as (UnboundVariable ident) ->
                                result {
                                    let! instType =
                                        match app.Function with
                                            | AnnotationExpr { Type = typ; Expression = _ } ->
                                                Ok typ
                                            | expr -> cerror (Unsupported <| expr.Unparse())
                                    let! scheme = TypeEnvironment.tryFind ident tenv
                                    let! subst = Substitution.unify scheme.Type instType
                                    let typeArgNodes =
                                        scheme.TypeVariables
                                            |> Seq.map (fun tv ->
                                                Type.transpile subst[tv])
                                    return InvocationExpression(
                                        GenericName(   // to-do: use plain IdentifierName(ident.Name) for non-generic calls?
                                            Identifier(ident.Name))
                                            .WithTypeArgumentList(
                                                TypeArgumentList(
                                                    Syntax.separatedList typeArgNodes)))
                                }
                            | _ -> Error cerr

            let! argNode, _ = transpile venv app.Argument

            let node =
                InvocationExpression(funcNode)
                    .WithArgumentList(
                        ArgumentList(
                            SingletonSeparatedList(
                                Argument(argNode))))

            return node, venv
        }

    and transpileIf venv (iff : If) =
        result {

            let! condNode, _ = transpile venv iff.Condition
            let! trueNode, _ = transpile venv iff.TrueBranch
            let! falseNode, _ = transpile venv iff.FalseBranch

            let node =
                ConditionalExpression(
                    condNode, trueNode, falseNode)

            return node, venv
        }

    and transpileFix venv expr =
        result {

            let! exprNode, _ = transpile venv expr

            let node =
                InvocationExpression(
                    IdentifierName("Fix"))
                    .WithArgumentList(
                        ArgumentList(
                            SingletonSeparatedList(
                                Argument(exprNode))))

            return node, venv
        }
*)

    let transpile expr =
        result {
            let! mainStmtNodes, mainExprNode =
                transpileExpr expr

            let typeNode = Type.transpile expr.Type
            let stmts =
                [|
                    yield! mainStmtNodes
                    yield ReturnStatement(mainExprNode)
                |]
            return MethodDeclaration(
                returnType = typeNode,
                identifier = "main")
                .AddModifiers(
                    Token(SyntaxKind.StaticKeyword))
                .WithBody(
                    Block(stmts))
        }
