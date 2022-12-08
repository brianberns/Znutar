namespace Znutar.Transpiler

open System.IO
open System.Reflection

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

open Basic.Reference.Assemblies

open Znutar
open Znutar.Parser
open Znutar.TypeInference

module Transpiler =

    let private transpileWithMember assemblyName memberNode =

        let emitResult =

            let compilationUnit, mainTypeName =
                CompilationUnit.create assemblyName memberNode
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

    let transpile assemblyName text =
        result {
            let! expr = Parser.run Expression.parse text
            let! expr' = Infer.inferExpression expr
            let! methodNode = Expression.transpile expr'
            do!
                transpileWithMember
                    assemblyName
                    methodNode
        }
