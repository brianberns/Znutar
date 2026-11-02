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

    /// Transpiles the given qualified identifier to a fully qualified
    /// name, so it can't be shadowed by a local. E.g. global::System.Guid.
    let transpileName (qi : QualifiedIdentifier) =

        let rec loop = function
            | [] -> failwith "Empty qualified name"
            | [ident] ->
                AliasQualifiedName(
                    IdentifierName(Token(SyntaxKind.GlobalKeyword)),
                    IdentifierName(ident.Name))
                    :> Syntax.NameSyntax
            | ident :: idents ->
                QualifiedName(
                    loop idents,
                    IdentifierName(ident.Name))

        qi
            |> Seq.rev
            |> Seq.toList
            |> loop

    /// Transpiles the given qualified identifier.
    let transpile (qi : QualifiedIdentifier) =
        transpileName qi :> Syntax.TypeSyntax

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

    /// Creates a generic type name. E.g. System.Func<int, int>.
    let genericName (qi : QualifiedIdentifier) (typeArgNodes : seq<Syntax.TypeSyntax>) =
        let path, name =
            match List.rev (NonEmptyList.toList qi) with
                | name :: pathRev ->
                    List.rev pathRev, name
                | [] -> failwith "Empty qualified name"
        let genericNode =
            GenericName(Identifier(name.Name))
                .WithTypeArgumentList(
                    TypeArgumentList(
                        Syntax.separatedList typeArgNodes))
        match path with
            | head :: tail ->
                QualifiedName(
                    QualifiedIdentifier.transpileName (NonEmptyList.create head tail),
                    genericNode)
                    :> Syntax.NameSyntax
            | [] -> genericNode

    /// Creates a System.Func type with the given type arguments.
    let funcNode typeArgNodes =
        genericName (QualifiedIdentifier.parse "System.Func") typeArgNodes

    /// Splits the given function type into the given number of
    /// input types, plus an output type. E.g. 'a -> 'b -> 'c with
    /// count 1 -> ['a], 'b -> 'c.
    let rec splitArrows count typ =
        if count = 0 then Ok ([], typ)
        else
            match typ with
                | TypeArrow (inpType, outType) ->
                    splitArrows (count - 1) outType
                        |> Result.map (fun (inpTypes, outType') ->
                            inpType :: inpTypes, outType')
                | _ ->
                    Error (InternalError $"Not a function type: {typ.Unparse()}")

    /// Transpiles the given type.
    let rec transpile = function
        | TypeConstant qi as typ ->
            match Map.tryFind typ primitiveTypeMap with
                | Some node -> node
                | None -> QualifiedIdentifier.transpile qi
        | TypeVariable tv ->
            IdentifierName(tv.Name)
        | TypeArrow (inpType, outType) ->
            funcNode [ transpile inpType; transpile outType ]
        | TypeTuple types ->
            let nodes =
                types
                    |> Seq.map (transpile >> TupleElement)
                    |> Syntax.separatedList
            TupleType(nodes)
