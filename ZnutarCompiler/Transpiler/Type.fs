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

module Type =

    let private toTypeSyntax (qi : QualifiedIdentifier) =

        let rec loop = function
            | [] -> failwith "Empty qualified name"
            | [ident] ->
                IdentifierName(ident.Name)
                    :> Syntax.NameSyntax
            | ident :: idents ->
                QualifiedName(
                    loop idents,
                    IdentifierName(ident.Name))

        qi.Items
            |> Seq.toList
            |> List.rev
            |> loop
            :> Syntax.TypeSyntax

    let private primitiveTypeMap : Map<_, Syntax.TypeSyntax> =
        Map [
            Type.bool,
                PredefinedType(Token(SyntaxKind.BoolKeyword))
            Type.int,
                PredefinedType(Token(SyntaxKind.IntKeyword))
            Type.string,
                PredefinedType(Token(SyntaxKind.StringKeyword))
            Type.unit,
                typeof<Znutar.Runtime.Unit>.FullName
                    |> QualifiedIdentifier.split
                    |> toTypeSyntax
        ]

    /// Transpiles the given type.
    let rec transpile = function
        | TypeConstant qi as typ ->
            match Map.tryFind typ primitiveTypeMap with
                | Some node -> node
                | None -> toTypeSyntax qi
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
        | TypeTuple types ->
            let nodes =
                types
                    |> Seq.map (transpile >> TupleElement)
                    |> Syntax.separatedList
            TupleType(nodes)
