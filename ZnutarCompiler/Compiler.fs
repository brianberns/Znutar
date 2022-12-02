namespace Znutar

open System.IO
open System.Reflection

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Basic.Reference.Assemblies

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

    module private rec Expression =

        let compile venv = function
            | VariableExpr ident -> compileIdentifier venv ident
            | ApplicationExpr app -> compileApplication venv app
            | LetExpr letb -> compileLet venv letb
            | IfExpr iff -> compileIf venv iff
            | FixExpr expr -> compileFix venv expr
            | BinaryOperationExpr bop -> compileBinaryOperation venv bop
            | LiteralExpr lit -> compileLiteral venv lit
            | AnnotationExpr ann ->
                match ann.Expression with
                    | LambdaExpr lam -> compileLambda venv ann.Type lam
                    | expr -> compile venv expr
            | LambdaExpr lam -> cerror (Unsupported "Unannotated lambda")

        let private compileIdentifier venv ident =
            VariableEnvironment.tryFind ident venv
                |> Result.map (fun node -> node, venv)

        /// E.g. ((System.Func<int, int>)(x => x + 1))
        let private compileLambda venv typ (lam : LambdaAbstraction) =
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
                        LiteralExpression(
                            SyntaxKind.NumericLiteralExpression,
                            Literal(n))
                            :> Syntax.ExpressionSyntax
                    | BoolLiteral b ->
                        let kind =
                            if b then SyntaxKind.TrueLiteralExpression
                            else SyntaxKind.FalseLiteralExpression
                        LiteralExpression(kind)
            Ok (node, venv)

        let private compileLet venv (letb : LetBinding) =
            result {
                let! argNode, _ = compile venv letb.Argument
                let! node, _ =
                    let venv' = Map.add letb.Identifier argNode venv
                    compile venv' letb.Body
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

        let private compileFix venv expr =
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

    type Syntax.MethodDeclarationSyntax with
        member node.MaybeWithTypeParameterList(
            typeParameterList : Syntax.TypeParameterListSyntax) =
            if typeParameterList.Parameters.Count > 0 then
                node.WithTypeParameterList(typeParameterList)
            else node

    module private Decl =

        let compile tenv (decl : Declaration) =
            result {

                    // find scheme of this declaration
                let! scheme =
                    TypeEnvironment.tryFind decl.Identifier tenv

                    // compile return type
                let returnTypeNode = Type.compile scheme.Type

                    // compile type parameters
                let typeParmNodes =
                    scheme.TypeVariables
                        |> Seq.map (fun tv ->
                            TypeParameter(Identifier(tv.Name)))

                    // compile body
                let! bodyNode, _ =
                    Expression.compile VariableEnvironment.empty decl.Body

                    // construct member
                return
                    MethodDeclaration(
                        returnType = returnTypeNode,
                        identifier = decl.Identifier.Name)
                        .AddModifiers(
                            Token(SyntaxKind.StaticKeyword))
                        .MaybeWithTypeParameterList(
                            TypeParameterList(SeparatedList(typeParmNodes)))
                        .WithBody(
                            Block(ReturnStatement(bodyNode)))
                        :> Syntax.MemberDeclarationSyntax
            }

    /// E.g. Member().Invoke
    let private compileMemberAccess decl =
        result {
            return MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression,
                InvocationExpression(
                    GenericName(
                        Identifier(decl.Identifier.Name))),
                IdentifierName("Invoke"))
        }

    let private compileProgam tenv program =
        result {

                // compile each declaration
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

                // compile main expression
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
