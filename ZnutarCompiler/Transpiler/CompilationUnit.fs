namespace Znutar.Transpiler

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

module CompilationUnit =

    (*
        static void Main()
        {
            System.Console.Write($node);
        }
    *)
    let private mainMethod =
        MethodDeclaration(
            returnType =
                PredefinedType(
                    Token(SyntaxKind.VoidKeyword)),
            identifier = "Main")
            .AddModifiers(
                Token(SyntaxKind.StaticKeyword))
            .WithBody(
                Block(
                    ExpressionStatement(
                        InvocationExpression(
                            MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    IdentifierName("System"),
                                    IdentifierName("Console")),
                                IdentifierName("Write")))
                            .WithArgumentList(
                                ArgumentList(
                                    SingletonSeparatedList(
                                        Argument(InvocationExpression(
                                            IdentifierName("main")))))))))

    let create assemblyName memberNode =
        let classNode =
            ClassDeclaration($"{assemblyName}Type")
                .AddModifiers(
                    Token(SyntaxKind.StaticKeyword))
                .AddMembers(
                    [|
                        mainMethod
                            :> Syntax.MemberDeclarationSyntax
                        memberNode
                    |])
        let namespaceNode =
            NamespaceDeclaration(
                IdentifierName(assemblyName : string))
                .AddMembers(classNode)
        let compilationUnit =
            CompilationUnit().AddMembers(namespaceNode)
        let mainTypeName =
            $"{namespaceNode.Name}.{classNode.Identifier}"
        compilationUnit, mainTypeName
