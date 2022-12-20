namespace Znutar.Transpiler

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

module CompilationUnit =

    /// Creates a Main method that writes the result of invoking the
    /// given expression method to the console.
    (*
        static void Main()
        {
            System.Console.Write($exprNode());
        }
    *)
    let private mainMethod (exprNode : Syntax.MethodDeclarationSyntax) =
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
                                            IdentifierName(exprNode.Identifier)))))))))

    /// Creates a compilation unit.
    let create assemblyName exprNode =
        let classNode =
            ClassDeclaration($"{assemblyName}Type")
                .AddModifiers(
                    Token(SyntaxKind.StaticKeyword))
                .AddMembers(
                    [|
                        mainMethod exprNode
                            :> Syntax.MemberDeclarationSyntax
                        exprNode
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
