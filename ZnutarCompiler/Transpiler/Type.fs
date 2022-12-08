namespace Znutar.Transpiler

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Znutar

module private Syntax =

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

module Type =

    let private predefinedTypeMap =
        Map [
            Type.int, SyntaxKind.IntKeyword
            Type.bool, SyntaxKind.BoolKeyword
        ]

    /// Transpiles the given type.
    let rec transpile = function
        | TypeConstant _ as typ ->
            let kind = predefinedTypeMap[typ]
            PredefinedType(Token(kind))
                : Syntax.TypeSyntax
        | TypeVariable tv ->
            IdentifierName(tv.Name)
        | TypeArrow (inpType, outType) ->
            let inpNode = transpile inpType
            let outNode = transpile outType
            QualifiedName(
                IdentifierName("System"),
                GenericName(
                    Identifier("Func"))
                    .WithTypeArgumentList(
                        TypeArgumentList(
                            Syntax.separatedList(
                                [inpNode; outNode]))))
