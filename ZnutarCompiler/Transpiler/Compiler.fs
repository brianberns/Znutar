﻿namespace Znutar.Transpiler

open System.IO
open System.Reflection

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Basic.Reference.Assemblies

open Znutar
open Znutar.Parser
open Znutar.TypeInference

type InvalidProgramType = InvalidProgramType of Type
    with interface ICompilerError

module Compiler =

    /// Creates a method node for the given expression.
    let private transpile (expr : AnnotatedExpression) =
        result {
            let typeNode = Type.transpile expr.Type
            if typeNode.IsKind(SyntaxKind.PredefinedType) then

                let! mainStmtNodes, mainExprNode =
                    Expression.transpile expr

                let stmts =
                    [|
                        yield! mainStmtNodes
                        yield ReturnStatement(mainExprNode)
                    |]
                return MethodDeclaration(
                    returnType = typeNode,
                    identifier = "Expression")
                    .AddModifiers(
                        Token(SyntaxKind.StaticKeyword))
                    .WithBody(
                        Block(stmts))
            else
                return! cerror (InvalidProgramType expr.Type)
        }

    /// Compiles the given node into an assembly.
    let private compileNode assemblyName exprNode =

        let emitResult =

            let compilationUnit, mainTypeName =
                CompilationUnit.create assemblyName exprNode
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
                    |> InternalError
                    |> cerror
        }

    /// Compiles the given text into an an assembly.
    let compile assemblyName text =
        result {
                // parse the text
            let! expr = Parser.run Expression.parse text

                // infer types of the resulting syntax tree
            let! expr' = Infer.inferExpression expr

                // compile tree into an assembly
            let! exprNode = transpile expr'
            do! compileNode assemblyName exprNode
        }