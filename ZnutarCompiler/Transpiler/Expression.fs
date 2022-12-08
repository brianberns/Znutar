namespace Znutar.Transpiler

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Znutar
open Znutar.Parser
open Znutar.TypeInference

module Expression =

    let private transpileIdentifier ident =
        let node : Syntax.ExpressionSyntax =
            IdentifierName(ident.Name)
        Ok ([], node)

    let private transpileLiteral lit =
        let node : Syntax.ExpressionSyntax =
            match lit with
                | IntLiteral n ->
                    LiteralExpression(
                        SyntaxKind.NumericLiteralExpression,
                        Literal(n))
                | BoolLiteral b ->
                    let kind =
                        if b then SyntaxKind.TrueLiteralExpression
                        else SyntaxKind.FalseLiteralExpression
                    LiteralExpression(kind)
        Ok ([], node)

    let rec private transpileExpr = function
        | VariableExpr var -> transpileIdentifier var.Identifier
        | ApplicationExpr app -> transpileApplication app
        | LetExpr letb -> transpileLet letb
        | IfExpr iff -> transpileIf iff
        // | FixExpr expr -> transpileFix venv expr
        | BinaryOperationExpr bop -> transpileBinaryOperation bop
        | LiteralExpr lit -> transpileLiteral lit
        // | LambdaExpr lam -> cerror (Unsupported "Unannotated lambda")

    and private transpileApplication app =
        app
            |> FunctionCall.create
            |> FunctionCall.transpile transpileExpr

    and private transpileLet letb =
        match Function.tryCreate letb with
            | Some func -> transpileFunction func
            | None -> transpileLetRaw letb

    (*
        From:
            let y =
                let x = 1
                in 2 * x
            in
            let z = 3 in
            y + z

        To:
            int x = 1;
            int y = 2 * x;
            int z = 3;
            return y + z;
     *)
    and private transpileLetRaw letb =
        result {
            let typeNode = Type.transpile letb.Argument.Type
            let! argStmtNodes, argExprNode = transpileExpr letb.Argument   // argStmtNodes: int x = 1, argExprNode: 2 * x
            let! bodyStmtNodes, bodyExprNode = transpileExpr letb.Body     // bodyStmtNodes: int z = 3, bodyExprNode: y + z
            let stmtNode : Syntax.StatementSyntax =                        // stmtNode: int y = 2 * x
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

    and private transpileFunction func =
        Function.transpile transpileExpr func

    and transpileIf iff =
        result {

            let! condStmtNodes, condExprNode = transpileExpr iff.Condition
            let! trueStmtNodes, trueExprNode = transpileExpr iff.TrueBranch
            let! falseStmtNodes, falseExprNode = transpileExpr iff.FalseBranch

            let ifStmtNodes =
                condStmtNodes @ trueStmtNodes @ falseStmtNodes
            let ifExprNode =
                ConditionalExpression(
                    condExprNode, trueExprNode, falseExprNode)
            return ifStmtNodes, ifExprNode
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
