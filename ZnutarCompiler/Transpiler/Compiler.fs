namespace Znutar

open System.IO
open System.Reflection

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Basic.Reference.Assemblies

/// let const x y = x in next
type Function =
    {
        /// E.g. "const"
        Identifier : Identifier

        /// E.g. [x; y]
        Arguments : List<Identifier>

        /// E.g. x
        FunctionBody : AnnotatedExpression

        /// E.g. <'a, 'b>('a -> 'b -> 'a)
        Scheme : Scheme

        /// E.g. next
        ExpressionBody : AnnotatedExpression
    }

module Function =

    /// From: let const = fun x -> fun y -> x in body
    /// To:   let const (x, y) = x in body
    let tryCreate (letb : AnnotatedLetBinding) =

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
                FunctionBody =
                    let lam = List.last lams
                    lam.Body
                Scheme = letb.Scheme
                ExpressionBody = letb.Body
            }

type Unsupported =
    Unsupported of string
    with interface ICompilerError

module Compiler =

    let private compileWithMembers assemblyName memberNodes =

        let emitResult =

            let compilationUnit, mainTypeName =
                CompilationUnit.create assemblyName memberNodes
#if DEBUG
            printfn "%A" <| compilationUnit.NormalizeWhitespace()
#endif
            let compilation =
                let options =
                    CSharpCompilationOptions(OutputKind.ConsoleApplication)
                        .WithMainTypeName(mainTypeName)
                CSharpCompilation
                    .Create(assemblyName)
                    .WithReferences(
                        Net60.References.SystemRuntime,
                        Net60.References.SystemConsole)
                    .AddSyntaxTrees(compilationUnit.SyntaxTree)
                    .WithOptions(options)
            compilation.Emit($"{assemblyName}.dll")

        result {
            if emitResult.Success then
                let sourcePath =
                    Path.Combine(
                        Path.GetDirectoryName(
                            Assembly.GetExecutingAssembly().Location),
                        "App.runtimeconfig.json")
                File.Copy(
                    sourcePath,
                    $"{assemblyName}.runtimeconfig.json",
                    overwrite = true)
            else
                return! emitResult.Diagnostics
                    |> Seq.map string
                    |> String.concat "\n"
                    |> Unsupported
                    |> cerror
        }

    module private Type =

        let private predefinedTypeMap =
            Map [
                Type.int, SyntaxKind.IntKeyword
                Type.bool, SyntaxKind.BoolKeyword
            ]

        let rec compile = function
            | TypeConstant _ as typ ->
                let kind = predefinedTypeMap[typ]
                PredefinedType(Token(kind))
                    : Syntax.TypeSyntax
            | TypeVariable tv ->
                IdentifierName(tv.Name)
            | TypeArrow (inpType, outType) ->
                let inpNode = compile inpType
                let outNode = compile outType
                QualifiedName(
                    IdentifierName("System"),
                    GenericName(
                        Identifier("Func"))
                        .WithTypeArgumentList(
                            TypeArgumentList(
                                Syntax.separatedList(
                                    [inpNode; outNode]))))

    let private compileExpr expr =

        let compileLiteral (lit : Literal) =
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

        let rec compile = function
            | VariableExpr var -> compileIdentifier var.Identifier
            // | ApplicationExpr app -> compileApplication venv app
            | LetExpr letb -> compileLet letb
            // | IfExpr iff -> compileIf venv iff
            // | FixExpr expr -> compileFix venv expr
            | BinaryOperationExpr bop -> compileBinaryOperation bop
            | LiteralExpr lit -> compileLiteral lit
            // | LambdaExpr lam -> cerror (Unsupported "Unannotated lambda")

        and compileIdentifier (ident : Identifier) =
            Ok ([], IdentifierName(ident.Name))

        and compileLet letb =
            match Function.tryCreate letb with
                | Some func -> compileFunction func
                | None -> compileLetRaw letb

        and compileLetRaw letb =
            result {
                let typeNode = Type.compile letb.Argument.Type
                let! argStmtNodes, argExprNode = compile letb.Argument   // argStmtNodes: int x = 1, argExprNode: 2 * x
                let! bodyStmtNodes, bodyExprNode = compile letb.Body     // bodyStmtNodes: int z = 3, bodyExprNode: y + z
                let stmtNode : Syntax.StatementSyntax =                  // stmtNode: int y = 2 * x
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

        and compileFunction (func : Function) =

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
                    let returnTypeNode = Type.compile returnType
                    let typeParmNodes =
                        func.Scheme.TypeVariables
                            |> List.map (fun tv ->
                                TypeParameter(Identifier(tv.Name)))
                    let parmNodes =
                        argPairs
                            |> List.map (fun (ident, typ) ->
                                Parameter(
                                    Identifier(ident.Name))
                                    .WithType(Type.compile typ))
                    let! bodyStmtNodes, bodyExprNode = compile func.FunctionBody
                    let! nextStmtNodes, nextExprNode = compile func.ExpressionBody
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

        and compileBinaryOperation bop =
            let kind =
                match bop.Operator with
                    | Plus -> SyntaxKind.AddExpression
                    | Minus -> SyntaxKind.SubtractExpression
                    | Times -> SyntaxKind.MultiplyExpression
                    | Equals -> SyntaxKind.EqualsExpression
            result {
                let! leftStmtNodes, leftExprNode = compile bop.Left
                let! rightStmtNodes, rightExprNode = compile bop.Right
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
        and compileLambda venv typ (lam : LambdaAbstraction) =
            result {
                let venv' =
                    let identNode : Syntax.ExpressionSyntax =
                        IdentifierName(lam.Identifier.Name)
                    Map.add lam.Identifier identNode venv
                let! bodyNode, _ = compile venv' lam.Body
                let node =
                    ParenthesizedExpression(
                        CastExpression(
                            Type.compile typ,
                            ParenthesizedExpression(
                                SimpleLambdaExpression(
                                    Parameter(
                                        Identifier(lam.Identifier.Name)))
                                    .WithExpressionBody(bodyNode))))
                return node, venv
            }

        and compileApplication venv (app : Application) =
            result {

                let! funcNode =
                    match compile venv app.Function with
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
                                                    Type.compile subst[tv])
                                        return InvocationExpression(
                                            GenericName(   // to-do: use plain IdentifierName(ident.Name) for non-generic calls?
                                                Identifier(ident.Name))
                                                .WithTypeArgumentList(
                                                    TypeArgumentList(
                                                        Syntax.separatedList typeArgNodes)))
                                    }
                                | _ -> Error cerr

                let! argNode, _ = compile venv app.Argument

                let node =
                    InvocationExpression(funcNode)
                        .WithArgumentList(
                            ArgumentList(
                                SingletonSeparatedList(
                                    Argument(argNode))))

                return node, venv
            }

        and compileIf venv (iff : If) =
            result {

                let! condNode, _ = compile venv iff.Condition
                let! trueNode, _ = compile venv iff.TrueBranch
                let! falseNode, _ = compile venv iff.FalseBranch

                let node =
                    ConditionalExpression(
                        condNode, trueNode, falseNode)

                return node, venv
            }

        and compileFix venv expr =
            result {

                let! exprNode, _ = compile venv expr

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

        compile expr

    let private compileExpression annex =
        result {
            let! mainStmtNodes, mainExprNode =
                compileExpr annex

            let typeNode = Type.compile annex.Type
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

    let compile assemblyName text =
        result {
            let! expr = Parser.run Parser.parseExpression text
            let! subst, annex = TypeInference.inferExpression TypeEnvironment.empty expr
            let! methodNode = compileExpression annex
            do!
                compileWithMembers
                    assemblyName
                    [methodNode]
        }
