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

/// Resolves assemblies from in-memory images by simple name.
type private ImageResolver(images : Map<string, byte[]>) =
    inherit MetadataAssemblyResolver()

    override _.Resolve(context, assemblyName) =
        match Map.tryFind assemblyName.Name images with
            | Some image -> context.LoadFromByteArray(image)
            | None -> null

module Compiler =

    /// Answers the images of the given reference assemblies by
    /// simple name, defaulting to .NET's reference assemblies if
    /// none are given. The same references are used to both type-
    /// check and compile a program.
    let private getReferenceImages (paths : string[]) =
        let images =
            if paths.Length = 0 then
                Net100.ReferenceInfos.All
                    |> Seq.map (fun info ->
                        IO.Path.GetFileNameWithoutExtension(info.FileName),
                        info.ImageBytes)
                    |> Map
            else
                paths
                    |> Seq.map (fun path ->
                        IO.Path.GetFileNameWithoutExtension(path),
                        IO.File.ReadAllBytes(path))
                    |> Map

            // Znutar runtime is always needed
        let runtimeAssembly = typeof<Unit>.Assembly
        let runtimeName = runtimeAssembly.GetName().Name
        if images.ContainsKey(runtimeName) then images
        else
            images.Add(
                runtimeName,
                IO.File.ReadAllBytes(runtimeAssembly.Location))

    /// Infers the types of the given expression using the given
    /// reference assemblies, which are loaded for inspection only.
    let private infer images expr =
        let coreAssemblyName =
            if Map.containsKey "System.Runtime" images then "System.Runtime"
            else null
        use context =
            new MetadataLoadContext(
                ImageResolver(images),
                coreAssemblyName)
        let assemblies =
            images.Keys
                |> Seq.map (fun name ->
                    context.LoadFromAssemblyName(name))
                |> Seq.toArray
        Infer.inferExpression assemblies expr

    /// Creates a method node for the given expression.
    let private transpile (expr : AnnotatedExpression) =
        result {
            let typeNode = Type.transpile expr.Type
            let! mainStmtNodes, mainExprNode =
                let expr', names = Rename.uniquify expr
                Expression.transpile (Context.create names) expr'
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
    let private compileNode images assemblyName outputPath isUnit exprNode =

        let emitResult =

            let compilationUnit, mainTypeName =
                CompilationUnit.create assemblyName isUnit exprNode
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
                        images
                            |> Map.values
                            |> Seq.map (fun (image : byte[]) ->
                                MetadataReference.CreateFromImage(image)
                                    :> MetadataReference))
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
            let images = getReferenceImages references
            let! expr' = infer images expr

                // compile tree into an assembly
            let! exprNode = transpile expr'
            do! compileNode
                    images
                    assemblyName
                    outputPath
                    (expr'.Type = Znutar.Type.unit)
                    exprNode
        }

    /// Compiles the given file into an an assembly.
    let compileFile references assemblyName outputPath (path : string) =
        use rdr = new System.IO.StreamReader(path)
        rdr.ReadToEnd()
            |> compile references assemblyName outputPath
