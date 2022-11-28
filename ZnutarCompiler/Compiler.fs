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

    let private compileWithMembers assemblyName mainNode memberNodes =

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

        let compile venv = function
            | VariableExpr ident -> compileIdentifier venv ident
            // | LambdaExpr lam -> compileLambda env lam
            | ApplicationExpr app -> compileApplication venv app
            | LetExpr letb -> compileLet venv letb
            | IfExpr iff -> compileIf venv iff
            // | FixExpr expr -> compileFix env expr
            | BinaryOperationExpr bop -> compileBinaryOperation venv bop
            | LiteralExpr lit -> compileLiteral venv lit

        let private compileIdentifier venv ident =
            VariableEnvironment.tryFind ident venv
                |> Result.map (fun node -> node, venv)

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
                let! node, env' = compile venv letb.Body
                let env'' = Map.add letb.Identifier node env'
                return node, env''
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

        let private compileType typ =
            result {
                match typ with
                    | TypeConstant _ as typ ->
                        let kind = predefinedTypeMap[typ]
                        return (PredefinedType(Token(kind)) : Syntax.TypeSyntax)
                    | TypeVariable def ->
                        return IdentifierName(def.Name)
                    | _ -> return! cerror (Unsupported "Unexpected type")
            }

        let private compileParameter parm typ =
            result {
                let! typeNode = compileType typ
                return Parameter(
                    Identifier(parm.Name))
                        .WithType(typeNode)
            }

        let compile tenv decl =
            result {

                let! scheme =
                    TypeEnvironment.tryFind decl.Identifier tenv
                match scheme.Type with
                    | TypeArrow (inpType, outType) ->
                        let! returnType = compileType outType
                        let typeParmNodes =
                            scheme.TypeVariables
                                |> Seq.map (fun tv ->
                                    TypeParameter(Identifier(tv.Name)))
                        let! env =
                            (VariableEnvironment.empty, typedParms)
                                ||> Result.List.foldM (fun acc (parm, _) ->
                                    result {
                                        let node = IdentifierName(parm.Name)
                                        return! acc
                                            |> Env.tryAdd parm.Name node
                                    })
                        let! parmNodes =
                            typedParms
                                |> Result.List.traverse (fun (parm, typ) ->
                                    compileParameter parm typ)
                        let! bodyNode, _ = Expression.compile env decl.Body

                        return MethodDeclaration(
                            returnType = returnType,
                            identifier = decl.Identifier.Name)
                            .AddModifiers(
                                Token(SyntaxKind.StaticKeyword))
                            .MaybeWithTypeParameterList(
                                TypeParameterList(SeparatedList(typeParmNodes)))
                            .WithParameterList(
                                ParameterList(SeparatedList(parmNodes)))
                            .WithBody(
                                Block(ReturnStatement(bodyNode)))
                    | _ -> return! cerror (Unsupported "Not supported")
            }

    module private DeclGroup =

        let compile group =
            group.Decls
                |> Result.List.traverse Decl.compile

    module private Program =

        let compile program =
            result {
                let! declNodes =
                    program.DeclGroups
                        |> Result.List.traverse DeclGroup.compile
                        |> Result.map (Seq.concat >> Seq.toArray)
                let! mainNode, _ =
                    Expression.compile Env.empty program.Main
                return mainNode, declNodes
            }

    let compile assemblyName text =
        result {
            let! program = Parser.parse text
            let! program' = TypeInfer.annotate program
            do! TypeCheck.validate program'
            let! mainNode, methodNodes =
                Program.compile program'
            let memberNodes =
                methodNodes
                    |> Array.map (fun node ->
                        node :> Syntax.MemberDeclarationSyntax)
            do!
                Compiler.compileWithMembers
                    assemblyName
                    mainNode
                    memberNodes
        }
