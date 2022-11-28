namespace Znutar

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

module Env =

    let empty : VariableEnvironment =
        Map.empty

    let tryFind ident (env : VariableEnvironment) =
        match Map.tryFind ident env with
            | Some node -> Ok node
            | None -> Error $"Unbound identifier: {ident.Name}"

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
                    |> Error
        }

    module private rec Expression =

        let compile env = function
            | VariableExpr ident -> compileIdentifier env ident
            // | LambdaExpr lam -> compileLambda env lam
            | ApplicationExpr app -> compileApplication env app
            | LetExpr letb -> compileLet env letb
            | IfExpr iff -> compileIf env iff
            // | FixExpr expr -> compileFix env expr
            | BinaryOperationExpr bop -> compileBinaryOperation env bop
            | LiteralExpr lit -> compileLiteral env lit

        let private compileIdentifier env ident =
            Env.tryFind ident env
                |> Result.map (fun node -> node, env)

        let private compileApplication env (app : Application) =
            result {

                let! funcNode, _ = compile env app.Function
                let! argNode, _ = compile env app.Argument

                let node =
                    InvocationExpression(funcNode)
                        .WithArgumentList(
                            ArgumentList(
                                SingletonSeparatedList(
                                    Argument(argNode))))

                return node, env
            }

        let private compileLiteral env (lit : Literal) =
            let node =
                match lit with
                    | IntLiteral n ->
                        Syntax.numericLiteral n
                            :> Syntax.ExpressionSyntax
                    | BoolLiteral b ->
                        Syntax.boolLiteral b
            Ok (node, env)

        let private compileLet env (letb : LetBinding) =
            result {
                let! node, env' = compile env letb.Body
                let env'' = Map.add letb.Identifier node env'
                return node, env''
            }

        let private compileBinaryOperation env (bop : BinaryOperation) =
            let kind =
                match bop.Operator with
                    | Plus -> SyntaxKind.AddExpression
                    | Minus -> SyntaxKind.SubtractExpression
                    | Times -> SyntaxKind.MultiplyExpression
                    | Equals -> SyntaxKind.EqualsExpression
            result {
                let! leftNode, _ = compile env bop.Left
                let! rightNode, _ = compile env bop.Right
                let node =
                    BinaryExpression(
                        kind,
                        leftNode,
                        rightNode)
                return node, env
            }

        let private compileIf env (iff : If) =
            result {

                let! condNode, _ = compile env iff.Condition
                let! trueNode, _ = compile env iff.TrueBranch
                let! falseNode, _ = compile env iff.FalseBranch

                let node =
                    ConditionalExpression(
                        condNode, trueNode, falseNode)

                return node, env
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
                    | _ -> return! Error "Unexpected type"
            }

        let private compileParameter parm typ =
            result {
                let! typeNode = compileType typ
                return Parameter(
                    Identifier(parm.Name))
                        .WithType(typeNode)
            }

        let compile decl =
            result {

                let! typedParms, outputType = Decl.getSignature decl
                let! returnType = compileType outputType
                let typeParmNodes =
                    decl.Scheme.TypeVariableIdents
                        |> Seq.map (fun tvIdent ->
                            TypeParameter(
                                Identifier(tvIdent.Name)))
                let! env =
                    (Env.empty, typedParms)
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
