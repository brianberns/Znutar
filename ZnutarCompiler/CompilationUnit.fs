namespace Znutar

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

module private Syntax =

    let separatedList<'t when 't :> SyntaxNode> nodes : SeparatedSyntaxList<'t> =
        let comma =
            SyntaxNodeOrToken.op_Implicit(
                Token(SyntaxKind.CommaToken))
        SeparatedList [|
            for iNode, (node : 't) in Seq.indexed nodes do
                if iNode > 0 then yield comma
                yield SyntaxNodeOrToken.op_Implicit(node)
        |]

module CompilationUnit =

    /// Z-combinator.
    (*
        static System.Func<A, B> Fix<A, B>(System.Func<System.Func<A, B>, System.Func<A, B>> f)
        {
            return (A x) => (f(Fix(f)))(x);
        }
    *)
    let private fixMethod =
        MethodDeclaration(
            QualifiedName(
                IdentifierName("System"),
                GenericName(
                    Identifier("Func"))
                    .WithTypeArgumentList(
                        TypeArgumentList(
                            Syntax.separatedList
                                [
                                    IdentifierName("A")
                                    IdentifierName("B")
                                ]))),
                Identifier("Fix"))
            .WithModifiers(
                TokenList(
                    Token(SyntaxKind.StaticKeyword)))
            .WithTypeParameterList(
                TypeParameterList(
                    Syntax.separatedList
                        [
                            TypeParameter(Identifier("A"))
                            TypeParameter(Identifier("B"))
                        ]))
            .WithParameterList(
                ParameterList(
                    SingletonSeparatedList(
                        Parameter(
                            Identifier("f"))
                            .WithType(
                                QualifiedName(
                                    IdentifierName("System"),
                                    GenericName(
                                        Identifier("Func"))
                                        .WithTypeArgumentList(
                                            TypeArgumentList(
                                                Syntax.separatedList
                                                    [
                                                        QualifiedName(
                                                            IdentifierName("System"),
                                                            GenericName(
                                                                Identifier("Func"))
                                                                .WithTypeArgumentList(
                                                                    TypeArgumentList(
                                                                        Syntax.separatedList
                                                                            [
                                                                                IdentifierName("A")
                                                                                IdentifierName("B")
                                                                            ])))
                                                        QualifiedName(
                                                            IdentifierName("System"),
                                                            GenericName(
                                                                Identifier("Func"))
                                                                .WithTypeArgumentList(
                                                                    TypeArgumentList(
                                                                        Syntax.separatedList
                                                                            [
                                                                                IdentifierName("A")
                                                                                IdentifierName("B")
                                                                            ])))
                                                    ])))))))
            .WithBody(
                Block(
                    SingletonList<Syntax.StatementSyntax>(
                        ReturnStatement(
                            ParenthesizedLambdaExpression()
                                .WithParameterList(
                                    ParameterList(
                                        SingletonSeparatedList(
                                            Parameter(
                                                Identifier("x"))
                                                .WithType(
                                                    IdentifierName("A")))))
                                .WithExpressionBody(
                                    InvocationExpression(
                                        ParenthesizedExpression(
                                            InvocationExpression(
                                                IdentifierName("f"))
                                                .WithArgumentList(
                                                    ArgumentList(
                                                        SingletonSeparatedList(
                                                            Argument(
                                                                InvocationExpression(
                                                                    IdentifierName("Fix"))
                                                                    .WithArgumentList(
                                                                        ArgumentList(
                                                                            SingletonSeparatedList(
                                                                                Argument(
                                                                                    IdentifierName("f")))))))))))
                                        .WithArgumentList(
                                            ArgumentList(
                                                SingletonSeparatedList(
                                                    Argument(
                                                        IdentifierName("x"))))))))))

    (*
        static void Main()
        {
            System.Console.Write($node);
        }
    *)
    /// our_code_starts_here
    let private mainMethod node =
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
                                        Argument(node)))))))

    let create assemblyName memberNodes mainNode =
        let classNode =
            ClassDeclaration($"{assemblyName}Type")
                .AddModifiers(
                    Token(SyntaxKind.StaticKeyword))
                .AddMembers(
                    [|
                        mainMethod mainNode
                            :> Syntax.MemberDeclarationSyntax
                        fixMethod
                        yield! memberNodes
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
