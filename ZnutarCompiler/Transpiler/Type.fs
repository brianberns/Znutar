namespace Znutar.Transpiler

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Znutar

module Syntax =

    let private comma =
        SyntaxNodeOrToken.op_Implicit(
            Token(SyntaxKind.CommaToken))

    /// Creates a comma-separated syntax list.
    let separatedList nodes =
        SeparatedList [|
            for iNode, (node : #SyntaxNode) in Seq.indexed nodes do
                if iNode > 0 then yield comma
                yield SyntaxNodeOrToken.op_Implicit(node)
        |]

    /// Converts the given expression node into a statement.
    let toStatement typ exprNode : Syntax.StatementSyntax =
        if typ = Type.``void`` then
            ExpressionStatement(exprNode)   // don't return a void value
        else
            ReturnStatement(exprNode)

module Type =

    let private primitiveTypeMap : Map<_, Syntax.TypeSyntax> =
        Map [
            Type.bool,
                PredefinedType(Token(SyntaxKind.BoolKeyword))
            Type.int,
                PredefinedType(Token(SyntaxKind.IntKeyword))
            Type.string,
                PredefinedType(Token(SyntaxKind.StringKeyword))
            Type.unit,
                QualifiedName(
                    QualifiedName(
                        IdentifierName("Znutar"),
                        IdentifierName("Runtime")),
                    IdentifierName("Unit"))
            Type.``void``,
                PredefinedType(Token(SyntaxKind.VoidKeyword))
        ]

    /// Transpiles the given type.
    let rec transpile = function
        | TypeConstant _ as typ ->
            primitiveTypeMap[typ]
        | TypeVariable tv ->
            IdentifierName(tv.Name)
        | TypeArrow (inpType, outType) ->
            let inpNode = transpile inpType
            let outNode = transpile outType
            if outType = Type.``void`` then
                QualifiedName(
                    IdentifierName("System"),
                    GenericName(
                        Identifier("Action"))
                        .WithTypeArgumentList(
                            TypeArgumentList(
                                SingletonSeparatedList(inpNode))))
            elif inpType = Type.``void`` then
                QualifiedName(
                    IdentifierName("System"),
                    GenericName(
                        Identifier("Func"))
                        .WithTypeArgumentList(
                            TypeArgumentList(
                                SingletonSeparatedList(outNode))))
            else
                QualifiedName(
                    IdentifierName("System"),
                    GenericName(
                        Identifier("Func"))
                        .WithTypeArgumentList(
                            TypeArgumentList(
                                Syntax.separatedList(
                                    [inpNode; outNode]))))
        | TypeTuple types ->
            let nodes =
                types
                    |> Seq.map (transpile >> TupleElement)
                    |> Syntax.separatedList
            TupleType(nodes)
