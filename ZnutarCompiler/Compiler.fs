namespace Znutar

open System
open System.Reflection

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Basic.Reference.Assemblies

open Znutar.Parser
open Znutar.Runtime
open Znutar.Transpiler
open Znutar.TypeInference

module Compiler =

    /// Creates a method node for the given expression.
    let private transpile (expr : AnnotatedExpression) =
        result {
            let typeNode = Type.transpile expr.Type
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
        }

    /// Compiles the given node into an assembly.
    let private compileNode assemblyName outputPath exprNode =

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
                        Net80.References.SystemRuntime,
                        Net80.References.SystemConsole,
                        MetadataReference.CreateFromFile(
                            typeof<Unit>.Assembly.Location))
                    .AddSyntaxTrees(compilationUnit.SyntaxTree)
                    .WithOptions(options)
            compilation.Emit(outputPath : string)

        result {
            if not emitResult.Success then
                return! emitResult.Diagnostics
                    |> Seq.map string
                    |> String.concat "\n"
                    |> InternalError
                    |> Error
        }

    /// Compiles the given text into an an assembly.
    let compile (references : string[]) assemblyName outputPath text =
        result {
                // parse the text
            let! expr = Parser.run Expression.parse text

                // infer types of the resulting syntax tree
            let! expr' =
                let refAssemblies =
                    [|
                        yield typeof<obj>.Assembly
                        for ref in references do
                            yield Assembly.LoadFile(ref)
                    |]
                Infer.inferExpression refAssemblies expr

                // compile tree into an assembly
            let! exprNode = transpile expr'
            do! compileNode assemblyName outputPath exprNode
        }

    /// Compiles the given file into an an assembly.
    let compileFile references assemblyName outputPath (path : string) =
        use rdr = new System.IO.StreamReader(path)
        rdr.ReadToEnd()
            |> compile references assemblyName outputPath
