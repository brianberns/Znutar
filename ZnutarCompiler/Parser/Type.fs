﻿namespace Znutar.Parser

open FParsec
open Znutar

/// Parses types. E.g. "'a -> int".
module Type =

    open Common

    let private parseType, private parseTypeRef =
        createParserForwardedToRef ()

    let private parseConstant =
        Identifier.parse
            |>> (fun ident ->
                NonEmptyList.create ident []   // to-do: support qualified identifiers
                    |> TypeConstant)

    let private parseVariableIdentifier =
        skipChar '\''
            >>. Identifier.parse   // don't include apostrophe

    let private parseVariable =
        parseVariableIdentifier
            |>> TypeVariable

    let private parseParenType =
        parseParens parseType

    let private parseSimpleType =
        choice [
            parseConstant
            parseVariable
            parseParenType
        ]

    /// Parses one or more simple types, folding them into a
    /// tuple if necessary. E.g. a * b * c.
    let private parseTupleType =

        let parseItem =
            parse {
                do! spaces .>> skipChar '*' >>. spaces
                return! parseSimpleType
            } |> attempt

        parse {
            let! typ = parseSimpleType
            match! many parseItem with
                | [] -> return typ
                | head :: items ->
                    return MultiItemList.create typ head items
                        |> TypeTuple
        }

    /// Parses a type arrow, if necessary.
    /// E.g. a * b -> c.
    let private parseTypeArrow =

        let create =
            parse {
                do! spaces >>. skipString "->" >>. spaces
                return (fun inpType outType ->
                    inpType ^=> outType)
            } |> attempt

        chainr1 parseTupleType create   // type arrow is right-associative

    do parseTypeRef.Value <- parseTypeArrow

    /// Parses a type. E.g. "'a -> int".
    let parse = parseType
