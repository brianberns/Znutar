﻿namespace Znutar

open System.IO
open System.Reflection

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Basic.Reference.Assemblies

module private Syntax =

    let numericLiteral (n : int) =
        LiteralExpression(
            SyntaxKind.NumericLiteralExpression,
            Literal(n))

    let boolLiteral flag =
        let kind =
            if flag then SyntaxKind.TrueLiteralExpression
            else SyntaxKind.FalseLiteralExpression
        LiteralExpression(kind)

    let by1 node kind =
        BinaryExpression(
            kind,
            node,
            numericLiteral 1)

    let isType node kind =
        BinaryExpression(
            SyntaxKind.IsExpression,
            node,
            PredefinedType(Token(kind)))

    let print node =
        InvocationExpression(IdentifierName("Print"))
            .WithArgumentList(
                ArgumentList(
                    SingletonSeparatedList(
                        Argument(node))))

    let not node =
        PrefixUnaryExpression(
            SyntaxKind.LogicalNotExpression,
            node)

type VariableEnvironment = Map<Identifier, Syntax.ExpressionSyntax>

module VariableEnvironment =

    let empty : VariableEnvironment =
        Map.empty

    let tryFind ident (env : VariableEnvironment) =
        match Map.tryFind ident env with
            | Some node -> Ok node
            | None -> cerror (UnboundVariable ident)

type Unsupported =
    Unsupported of string
    with interface ICompilerError

module Compiler =

    let private compileWithMembers assemblyName memberNodes mainNode =

        let emitResult =

            let compilationUnit, mainTypeName =
                CompilationUnit.create assemblyName memberNodes mainNode
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

    module private rec Expression =

        let getType = function
            | AnnotationExpr ann -> Ok ann.Type
            | expr ->
                cerror (Unsupported
                    $"Unannotated type: {expr.Unparse()}")

        let compile venv = function
            | VariableExpr ident -> compileIdentifier venv ident
            | LambdaExpr lam -> compileLambda venv lam
            | ApplicationExpr app -> compileApplication venv app
            | LetExpr letb -> compileLet venv letb
            | IfExpr iff -> compileIf venv iff
            | FixExpr expr -> compile venv expr
            | BinaryOperationExpr bop -> compileBinaryOperation venv bop
            | LiteralExpr lit -> compileLiteral venv lit
            | AnnotationExpr ann -> compile venv ann.Expression

        let private compileIdentifier venv ident =
            VariableEnvironment.tryFind ident venv
                |> Result.map (fun node -> node, venv)

        let private compileLambda venv (lam : LambdaAbstraction) =
            result {
                let venv' =
                    let identNode : Syntax.ExpressionSyntax =
                        IdentifierName(lam.Identifier.Name)
                    Map.add lam.Identifier identNode venv
                let! bodyNode, _ = compile venv' lam.Body
                let node =
                    SimpleLambdaExpression(
                        Parameter(
                            Identifier(lam.Identifier.Name)))
                        .WithExpressionBody(bodyNode)
                return node, venv
            }

        let private compileApplication venv (app : Application) =
            result {

                let! funcNode, _ = compile venv app.Function
                let! argNode, _ = compile venv app.Argument

                let node =
                    InvocationExpression(funcNode)
                        .WithArgumentList(
                            ArgumentList(
                                SingletonSeparatedList(
                                    Argument(argNode))))

                return node, venv
            }

        let private compileLiteral venv (lit : Literal) =
            let node =
                match lit with
                    | IntLiteral n ->
                        Syntax.numericLiteral n
                            :> Syntax.ExpressionSyntax
                    | BoolLiteral b ->
                        Syntax.boolLiteral b
            Ok (node, venv)

        let private compileLet venv (letb : LetBinding) =
            result {
                let! argNode, _ = compile venv letb.Argument
                let venv' = Map.add letb.Identifier argNode venv
                let! node, _ = compile venv' letb.Body
                return node, venv
            }

        let private compileBinaryOperation venv (bop : BinaryOperation) =
            let kind =
                match bop.Operator with
                    | Plus -> SyntaxKind.AddExpression
                    | Minus -> SyntaxKind.SubtractExpression
                    | Times -> SyntaxKind.MultiplyExpression
                    | Equals -> SyntaxKind.EqualsExpression
            result {
                let! leftNode, _ = compile venv bop.Left
                let! rightNode, _ = compile venv bop.Right
                let node =
                    BinaryExpression(
                        kind,
                        leftNode,
                        rightNode)
                return node, venv
            }

        let private compileIf venv (iff : If) =
            result {

                let! condNode, _ = compile venv iff.Condition
                let! trueNode, _ = compile venv iff.TrueBranch
                let! falseNode, _ = compile venv iff.FalseBranch

                let node =
                    ConditionalExpression(
                        condNode, trueNode, falseNode)

                return node, venv
            }

    type Syntax.MethodDeclarationSyntax with
        member node.MaybeWithTypeParameterList(
            typeParameterList : Syntax.TypeParameterListSyntax) =
            if typeParameterList.Parameters.Count > 0 then
                node.WithTypeParameterList(typeParameterList)
            else node

    module private Decl =

        let private predefinedTypeMap =
            Map [
                Type.int, SyntaxKind.IntKeyword
                Type.bool, SyntaxKind.BoolKeyword
            ]

        let rec private compileType = function
            | TypeConstant _ as typ ->
                let kind = predefinedTypeMap[typ]
                PredefinedType(Token(kind))
                    : Syntax.TypeSyntax
            | TypeVariable tv ->
                IdentifierName(tv.Name)
            | TypeArrow (inpType, outType) ->
                let inpNode = compileType inpType
                let outNode = compileType outType
                QualifiedName(
                    IdentifierName("System"),
                    GenericName(
                        Identifier("Func"))
                        .WithTypeArgumentList(
                            TypeArgumentList(
                                SeparatedList(
                                    [|
                                        SyntaxNodeOrToken.op_Implicit(inpNode)
                                        SyntaxNodeOrToken.op_Implicit(Token(SyntaxKind.CommaToken))
                                        SyntaxNodeOrToken.op_Implicit(outNode)
                                    |]))))

        let compile tenv (decl : Declaration) =
            result {
                let! scheme =
                    TypeEnvironment.tryFind decl.Identifier tenv
                let returnType = compileType scheme.Type
                let typeParmNodes =
                    scheme.TypeVariables
                        |> Seq.map (fun tv ->
                            TypeParameter(Identifier(tv.Name)))
                let! bodyNode, _ =
                    Expression.compile VariableEnvironment.empty decl.Body
                let methodNode =
                    MethodDeclaration(
                        returnType = returnType,
                        identifier = decl.Identifier.Name)
                        .AddModifiers(
                            Token(SyntaxKind.StaticKeyword))
                        .MaybeWithTypeParameterList(
                            TypeParameterList(SeparatedList(typeParmNodes)))
                        .WithBody(
                            Block(ReturnStatement(bodyNode)))
                return methodNode :> Syntax.MemberDeclarationSyntax
            }

    let private compileMemberAccess decl =
        result {
            return MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression,
                InvocationExpression(
                    GenericName(
                        Identifier(decl.Identifier.Name))
                        .WithTypeArgumentList(
                            TypeArgumentList(
                                SingletonSeparatedList(
                                    PredefinedType(
                                        Token(SyntaxKind.BoolKeyword)))))),
                IdentifierName("Invoke"))
        }

    let private compileProgam tenv program =
        result {
            let! declNodes =
                program.Declarations
                    |> Result.traverse (fun decl ->
                        Decl.compile tenv decl)
            let! venv =
                (VariableEnvironment.empty, program.Declarations)
                    ||> Result.foldM (fun acc decl ->
                        result {
                            let! node = compileMemberAccess decl
                            return acc |> Map.add decl.Identifier node
                        })
            let! mainNode, _ =
                program.Main
                    |> Expression.compile venv
            return declNodes, mainNode
        }

    let compile assemblyName text =
        result {
            let! program = Parser.run Parser.parseProgram text
            let! tenv, _, program' = TypeInference.inferProgram program
            let! declNodes, mainNode = compileProgam tenv program'
            do!
                compileWithMembers
                    assemblyName
                    declNodes
                    mainNode
        }
