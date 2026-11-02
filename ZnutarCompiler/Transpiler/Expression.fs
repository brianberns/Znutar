namespace Znutar.Transpiler

open System
open System.Collections.Generic

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Znutar
open Znutar.TypeInference

/// Transpilation context.
type Context =
    {
        /// Generates unique names.
        Names : NameGenerator

        /// Scheme and arity of each known local function, by
        /// (unique) name.
        Functions : Dictionary<Identifier, Scheme * int>
    }

module Context =

    /// Creates a context.
    let create names =
        {
            Names = names
            Functions = Dictionary()
        }

module rec Expression =

    /// Unit literal.
    let private unitNode : Syntax.ExpressionSyntax =
        MemberAccessExpression(
            SyntaxKind.SimpleMemberAccessExpression,
            Type.transpile Type.unit,
            IdentifierName("Value"))

    /// Parenthesizes the given node.
    let private paren (node : Syntax.ExpressionSyntax) =
        ParenthesizedExpression(node) :> Syntax.ExpressionSyntax

    /// Casts the given node to the given type. E.g. ((T)(node)).
    let private cast typ node =
        CastExpression(Type.transpile typ, paren node)
            |> paren

    /// Invokes the given function node with the given arguments.
    let private invoke (funcNode : Syntax.ExpressionSyntax) argNodes =
        InvocationExpression(funcNode)
            .WithArgumentList(
                ArgumentList(
                    argNodes
                        |> Seq.map Argument
                        |> Syntax.separatedList))
            :> Syntax.ExpressionSyntax

    /// Declares a local variable. E.g. int x = 1;
    let private declare (name : string) typ exprNode =
        LocalDeclarationStatement(
            VariableDeclaration(Type.transpile typ)
                .WithVariables(
                    SingletonSeparatedList(
                        VariableDeclarator(Identifier(name))
                            .WithInitializer(
                                EqualsValueClause(exprNode)))))
            :> Syntax.StatementSyntax

    /// Creates a block that executes the given statements and then
    /// returns the given expression.
    let private block stmtNodes exprNode =
        Block([|
            yield! stmtNodes
            yield ReturnStatement(exprNode) :> Syntax.StatementSyntax
        |])

    /// Creates a lambda with the given parameter.
    let private lambda (parmName : string) stmtNodes exprNode =
        let node = SimpleLambdaExpression(Parameter(Identifier(parmName)))
        match stmtNodes with
            | [] -> node.WithExpressionBody(exprNode)
            | _ -> node.WithBlock(block stmtNodes exprNode)

    /// Converts the given statements and expression of the given type
    /// into a single expression that can be evaluated lazily:
    /// ((System.Func<T>)(() => { stmts; return expr; }))()
    let private toExpression typ stmtNodes exprNode =
        match stmtNodes with
            | [] -> exprNode
            | _ ->
                let lambdaNode =
                    ParenthesizedLambdaExpression()
                        .WithBlock(block stmtNodes exprNode)
                let castNode =
                    CastExpression(
                        Type.funcNode [ Type.transpile typ ],
                        paren lambdaNode)
                invoke (paren castNode) []

    /// Transpiles an expression.
    let rec transpile ctx = function
        | AnnotatedIdentifierExpr ai -> transpileIdentifier ctx ai
        | AnnotatedApplicationExpr app -> transpileApplication ctx app
        | AnnotatedLetExpr letb -> transpileLet ctx letb
        | AnnotatedIfExpr iff -> transpileIf ctx iff
        | AnnotatedBinaryOperationExpr bop -> transpileBinaryOperation ctx bop
        | AnnotatedLiteralExpr lit -> transpileLiteral lit
        | AnnotatedLambdaExpr lam -> transpileLambda ctx lam
        | AnnotatedStaticMemberAccessExpr sma -> transpileStaticMemberAccess ctx sma
        | AnnotatedInstanceMemberAccessExpr ima -> transpileInstanceMemberAccess ctx ima
        | AnnotatedTupleExpr tuple -> transpileTuple ctx tuple

    /// Transpiles the given expressions in order.
    let private transpileMany ctx annexes =
        result {
            let! pairs = Result.traverse (transpile ctx) annexes
            let stmtNodeLists, exprNodes = List.unzip pairs
            return List.concat stmtNodeLists, exprNodes
        }

    /// Transpiles a literal.
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
                | StringLiteral chars ->
                    LiteralExpression(
                        SyntaxKind.StringLiteralExpression,
                        Literal(String(chars)))
                | UnitLiteral -> unitNode
        Ok ([], node)

    /// Transpiles an identifier. A reference to a known function
    /// that isn't being called is converted to a closure.
    let private transpileIdentifier ctx ai =
        match ctx.Functions.TryGetValue(ai.Identifier) with
            | true, func ->
                transpileFunctionCall ctx ai func []
            | false, _ ->
                let node : Syntax.ExpressionSyntax =
                    IdentifierName(ai.Identifier.Name)
                Ok ([], node)

    /// App (App f a) b -> f, [a; b].
    let private gatherArguments app =

        let rec loop (app : AnnotatedApplication) =
            match app.Function with
                | AnnotatedApplicationExpr app' ->
                    let expr, args = loop app'
                    expr, app.Argument :: args
                | expr -> expr, [app.Argument]

        let expr, argsRev = loop app
        expr, List.rev argsRev

    /// Transpiles an application.
    let private transpileApplication ctx app =
        match gatherArguments app with

                // call a known function directly (e.g. f(a, b))
            | AnnotatedIdentifierExpr ai, args
                when ctx.Functions.ContainsKey(ai.Identifier) ->
                transpileFunctionCall
                    ctx ai ctx.Functions[ai.Identifier] args

                // invoke a function value one argument at a time (e.g. f(a)(b))
            | funcAnnex, args ->
                result {
                    let! funcStmtNodes, funcExprNode = transpile ctx funcAnnex
                    let! argStmtNodes, argExprNodes = transpileMany ctx args
                    let exprNode =
                        (funcExprNode, argExprNodes)
                            ||> List.fold (fun acc argExprNode ->
                                invoke acc [ argExprNode ])
                    return funcStmtNodes @ argStmtNodes, exprNode
                }

    /// Transpiles a call to a known function with the given arguments,
    /// which might be more or fewer than the function's arity.
    (*
        Given:
            let add x y = x + y in ...
        Then:
            add 1 2     ->  add(1, 2)
            add 1       ->  int arg_1 = 1;
                            ((Func<int, int>)(p_1 => add(arg_1, p_1)))
            add         ->  ((Func<int, Func<int, int>>)(p_1 => p_2 => add(p_1, p_2)))
    *)
    let private transpileFunctionCall ctx ai (scheme, arity) args =
        result {
                // supply type arguments explicitly, since C# can't always infer them
            let! nameNode =
                match scheme.TypeVariables with
                    | [] ->
                        Ok (IdentifierName(ai.Identifier.Name) :> Syntax.SimpleNameSyntax)
                    | tvs ->
                        Substitution.unify scheme.Type ai.Type
                            |> Result.map (fun subst ->
                                let typeArgNodes =
                                    tvs
                                        |> List.map (fun tv ->
                                            TypeVariable tv
                                                |> Substitution.Type.apply subst
                                                |> Type.transpile)
                                GenericName(Identifier(ai.Identifier.Name))
                                    .WithTypeArgumentList(
                                        TypeArgumentList(
                                            Syntax.separatedList typeArgNodes))
                                    :> Syntax.SimpleNameSyntax)
            let! inpTypes, outType = Type.splitArrows arity ai.Type
            let! argStmtNodes, argExprNodes = transpileMany ctx args

            if args.Length >= arity then

                    // call the function, then invoke its result with any extra arguments
                let directArgNodes, extraArgNodes =
                    List.splitAt arity argExprNodes
                let exprNode =
                    (invoke nameNode directArgNodes, extraArgNodes)
                        ||> List.fold (fun acc argExprNode ->
                            invoke acc [ argExprNode ])
                return argStmtNodes, exprNode

            else
                    // evaluate supplied arguments once
                let suppliedTypes, missingTypes =
                    List.splitAt args.Length inpTypes
                let argNames =
                    suppliedTypes
                        |> List.map (fun _ -> ctx.Names.Fresh("arg"))
                let declNodes =
                    List.map3 declare argNames suppliedTypes argExprNodes

                    // create a closure for the missing arguments
                let parmNames =
                    missingTypes
                        |> List.map (fun _ -> ctx.Names.Fresh("p"))
                let callNode =
                    invoke nameNode [
                        for name in argNames @ parmNames do
                            IdentifierName(name) :> Syntax.ExpressionSyntax
                    ]
                let lambdaNode =
                    (parmNames, callNode)
                        ||> List.foldBack (fun parmName bodyNode ->
                            lambda parmName [] bodyNode)
                let closureType =
                    (missingTypes, outType)
                        ||> List.foldBack (^=>)
                return
                    argStmtNodes @ declNodes,
                    cast closureType lambdaNode
        }

    /// Transpiles a "let", possibly by turning it into a function.
    let private transpileLet ctx letb =
        match Function.tryCreate letb with
            | Some func ->
                ctx.Functions[func.Identifier] <-
                    (func.Scheme, func.Parameters.Length)
                Function.transpile (transpile ctx) func
            | None -> transpileLetRaw ctx letb

    /// Transpiles a "let".
    (*
        From:
            let y =
                let x = 1 in
                2 * x in
            let z = 3 in
            y + z

        To:
            int x = 1;
            int y = 2 * x;
            int z = 3;
            return y + z;
     *)
    let private transpileLetRaw ctx letb =
        result {
            let! argStmtNodes, argExprNode = transpile ctx letb.Argument   // argStmtNodes: int x = 1, argExprNode: 2 * x
            let! bodyStmtNodes, bodyExprNode = transpile ctx letb.Body     // bodyStmtNodes: int z = 3, bodyExprNode: y + z
            let stmtNode =                                                 // stmtNode: int y = 2 * x
                declare
                    letb.Identifier.Name
                    letb.Argument.Type
                    argExprNode
            let stmtNodes =
                [
                    yield! argStmtNodes
                    yield stmtNode
                    yield! bodyStmtNodes
                ]
            return stmtNodes, bodyExprNode
        }

    /// Transpiles an "if". Statements within a branch must only be
    /// executed if that branch is taken.
    let private transpileIf ctx iff =
        result {
            let! condStmtNodes, condExprNode = transpile ctx iff.Condition
            let! trueStmtNodes, trueExprNode = transpile ctx iff.TrueBranch
            let! falseStmtNodes, falseExprNode = transpile ctx iff.FalseBranch
            let exprNode =
                ConditionalExpression(
                    condExprNode,
                    toExpression iff.Type trueStmtNodes trueExprNode,
                    toExpression iff.Type falseStmtNodes falseExprNode)
            return condStmtNodes, paren exprNode
        }

    /// Transpiles a binary operation.
    let private transpileBinaryOperation ctx bop =
        result {
            let! stmtNodes, exprNodes = transpileMany ctx [ bop.Left; bop.Right ]
            let leftExprNode, rightExprNode =
                match exprNodes with
                    | [ leftExprNode; rightExprNode ] -> leftExprNode, rightExprNode
                    | _ -> failwith "Unexpected"
            let isPrimitive typ =
                typ = Type.int || typ = Type.bool || typ = Type.string
            let exprNode =
                match bop.Operator with

                        // e.g. System.Object.Equals(left, right)
                    | Equals when not (isPrimitive bop.Left.Type) ->
                        let equalsNode =
                            MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                QualifiedIdentifier.transpileName
                                    (QualifiedIdentifier.parse "System.Object"),
                                IdentifierName("Equals"))
                        invoke equalsNode [ leftExprNode; rightExprNode ]

                    | op ->
                        let kind =
                            match op with
                                | Plus -> SyntaxKind.AddExpression
                                | Minus -> SyntaxKind.SubtractExpression
                                | Times -> SyntaxKind.MultiplyExpression
                                | Equals -> SyntaxKind.EqualsExpression
                                | GreaterThan -> SyntaxKind.GreaterThanExpression
                                | LessThan -> SyntaxKind.LessThanExpression
                                | Divide -> SyntaxKind.DivideExpression
                                | Modulo -> SyntaxKind.ModuloExpression
                        BinaryExpression(kind, leftExprNode, rightExprNode)
                            |> paren
            return stmtNodes, exprNode
        }

    /// Transpiles an anonymous lambda to a C# lambda.
    /// From: fun x -> x + 1
    /// To:   ((System.Func<int, int>)(x => x + 1))
    let private transpileLambda ctx lam =
        result {
            let! bodyStmtNodes, bodyExprNode = transpile ctx lam.Body
            let lambdaNode =
                lambda lam.Identifier.Name bodyStmtNodes bodyExprNode
            return [], cast lam.Type lambdaNode
        }

    /// Transpiles a static member access.
    let private transpileStaticMemberAccessRaw sma =

        let exprNode =
            let init =
                AliasQualifiedName(
                    IdentifierName(Token(SyntaxKind.GlobalKeyword)),
                    IdentifierName(sma.Path.Head.Name))
                    :> Syntax.ExpressionSyntax
            (init, sma.Path.Tail)
                ||> List.fold (fun acc ident ->
                    MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        acc,
                        IdentifierName(ident.Name)))

        Ok (List.empty, exprNode)

    /// Transpiles an instance member access.
    let private transpileInstanceMemberAccessRaw ctx ima =
        result {
            let! stmtNodes, exprNode = transpile ctx ima.Expression
            let accessNode =
                MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    exprNode,
                    IdentifierName(ima.Identifier.Name))
            return stmtNodes, accessNode
        }

    /// Wraps a method or constructor in a lambda, so it can be used
    /// as a function value.
    (*
        constructor
            before:
                System.String
            after:
                ((System.Func<char[], string>)(x =>
                    new string(x)))

        string -> unit
            before:
                System.Console.WriteLine
            after:
                ((System.Func<string, Znutar.Runtime.Unit>)(x =>
                    {
                        System.Console.WriteLine(x);
                        return Znutar.Runtime.Unit.Value;
                    }))

        unit -> string
            before:
                System.Console.ReadLine
            after:
                ((System.Func<Znutar.Runtime.Unit, string>)(x =>
                    System.Console.ReadLine()))

        int * int -> int
            before:
                System.Math.Max
            after:
                ((System.Func<(int, int), int>)(x =>
                    System.Math.Max(x.Item1, x.Item2)))
    *)
    let private wrapMemberAccess ctx exprNode typ isConstructor =
        let ctx : Context = ctx
        match typ with
            | TypeArrow (inpType, outType) ->

                let parmName = ctx.Names.Fresh("x")

                    // gather arguments
                let argumentList =
                    match inpType with

                            // argument is a tuple?
                            // (e.g. x => new System.DateTime(x.Item1, x.Item2, x.Item3)))
                        | TypeTuple tuple ->
                            Seq.init tuple.Length (fun iArg ->
                                Argument(
                                    MemberAccessExpression(
                                        SyntaxKind.SimpleMemberAccessExpression,
                                        IdentifierName(parmName),
                                        IdentifierName($"Item{iArg + 1}"))))
                                |> Syntax.separatedList
                                |> ArgumentList

                            // ignore input unit?
                        | _ when inpType = Type.unit ->
                            ArgumentList()

                        | _ ->
                            ArgumentList(
                                SingletonSeparatedList(
                                    Argument(
                                        IdentifierName(parmName))))

                    // constructor invocation? (e.g. new String)
                let invocation : Syntax.ExpressionSyntax =
                    if isConstructor then
                        ObjectCreationExpression(Type.transpile outType)
                            .WithArgumentList(argumentList)
                    else
                        InvocationExpression(exprNode)
                            .WithArgumentList(argumentList)

                    // supply output unit?
                let lambdaNode =
                    if outType = Type.unit then
                        lambda parmName
                            [ ExpressionStatement(invocation) ]
                            unitNode
                    else
                        lambda parmName [] invocation

                cast typ lambdaNode
            | _ ->
                exprNode

    /// Transpiles a static member access.
    let private transpileStaticMemberAccess ctx sma =
        result {
                // transpile raw member access (e.g. Console.WriteLine)
            let! stmtNodes, exprNode = transpileStaticMemberAccessRaw sma

            let exprNode' =
                wrapMemberAccess ctx exprNode sma.Type sma.IsConstructor
            return stmtNodes, exprNode'
        }

    /// Transpiles an instance member access.
    let private transpileInstanceMemberAccess ctx ima =
        result {
                // transpile raw member access (e.g. dt.Year)
            let! stmtNodes, exprNode = transpileInstanceMemberAccessRaw ctx ima

            let exprNode' =
                wrapMemberAccess ctx exprNode ima.Type false
            return stmtNodes, exprNode'
        }

    /// Transpiles a tuple.
    let private transpileTuple ctx tuple =
        result {
            let! stmtNodes, exprNodes =
                transpileMany ctx (List.ofSeq tuple.Expressions)
            let exprNode =
                exprNodes
                    |> Seq.map Argument
                    |> Syntax.separatedList
                    |> TupleExpression
            return stmtNodes, exprNode
        }
