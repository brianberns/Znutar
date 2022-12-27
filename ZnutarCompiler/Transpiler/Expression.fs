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

    let rec transpile = function
        | IdentifierExpr ai -> transpileIdentifier ai.Identifier
        | ApplicationExpr app -> transpileApplication app
        | LetExpr letb -> transpileLet letb
        | IfExpr iff -> transpileIf iff
        | BinaryOperationExpr bop -> transpileBinaryOperation bop
        | LiteralExpr lit -> transpileLiteral lit
        | LambdaExpr lam -> transpileLambda lam
        | MemberAccessExpr ma -> transpileMemberAccess ma

    and private transpileApplication app =
        app
            |> FunctionCall.create
            |> FunctionCall.transpile transpile

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
            let! argStmtNodes, argExprNode = transpile letb.Argument   // argStmtNodes: int x = 1, argExprNode: 2 * x
            let! bodyStmtNodes, bodyExprNode = transpile letb.Body     // bodyStmtNodes: int z = 3, bodyExprNode: y + z
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
        Function.transpile transpile func

    and transpileIf iff =
        result {

            let! condStmtNodes, condExprNode = transpile iff.Condition
            let! trueStmtNodes, trueExprNode = transpile iff.TrueBranch
            let! falseStmtNodes, falseExprNode = transpile iff.FalseBranch

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
                | GreaterThan -> SyntaxKind.GreaterThanExpression
                | LessThan -> SyntaxKind.LessThanExpression
                | Divide -> SyntaxKind.DivideExpression
                | Modulo -> SyntaxKind.ModuloExpression
        result {
            let! leftStmtNodes, leftExprNode = transpile bop.Left
            let! rightStmtNodes, rightExprNode = transpile bop.Right
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

    /// Converts an anonymous lambda to a named lambda, then
    /// transpiles it into a function.
    /// From: fun x -> x
    /// To:   let lambda = (fun x -> x) in lambda
    and private transpileLambda lam =
        let ident = Identifier.create "anonymous"
        let scheme =
            let typeVars =
                Type.freeTypeVariables lam.Type
                    |> Set.toList
            {
                TypeVariables = typeVars
                Type = lam.Type
            }
        let annIdent =
            {
                Identifier = ident
                Type = lam.Type
            }
        let expr =
            LetExpr {
                Identifier = ident
                Scheme = scheme
                Argument = LambdaExpr lam
                Body = IdentifierExpr annIdent
                Type = lam.Type
            }
        transpile expr

    and private transpileMemberAccess ma =
        result {
            return! Error { new ICompilerError }
        }
