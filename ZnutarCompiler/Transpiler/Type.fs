namespace Znutar.Transpiler

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Znutar

module Syntax =

    /// Creates a comma-separated syntax list.
    let separatedList nodes =
        SeparatedList [|
            for iNode, (node : #SyntaxNode) in Seq.indexed nodes do
                if iNode > 0 then
                    yield SyntaxNodeOrToken.op_Implicit(
                        Token(SyntaxKind.CommaToken))
                yield SyntaxNodeOrToken.op_Implicit(node)
        |]

module QualifiedIdentifier =

    /// Transpiles the given qualified identifier.
    let transpile (qi : QualifiedIdentifier) =

        let rec loop = function
            | [] -> failwith "Empty qualified name"
            | [ident] ->
                IdentifierName(ident.Name)
                    :> Syntax.NameSyntax
            | ident :: idents ->
                QualifiedName(
                    loop idents,
                    IdentifierName(ident.Name))

        qi
            |> Seq.rev
            |> Seq.toList
            |> loop
            :> Syntax.TypeSyntax

module Type =

    /// Maps primitive types to syntax nodes.
    let private primitiveTypeMap =
        Map [
            Type.bool,
                PredefinedType(Token(SyntaxKind.BoolKeyword))
                    :> Syntax.TypeSyntax
            Type.int,
                PredefinedType(Token(SyntaxKind.IntKeyword))
            Type.string,
                PredefinedType(Token(SyntaxKind.StringKeyword))
            Type.unit,
                typeof<Znutar.Runtime.Unit>.FullName
                    |> QualifiedIdentifier.parse
                    |> QualifiedIdentifier.transpile
        ]

    /// Transpiles the given type.
    let rec transpile = function
        | TypeConstant qi as typ ->
            match Map.tryFind typ primitiveTypeMap with
                | Some node -> node
                | None -> QualifiedIdentifier.transpile qi
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
