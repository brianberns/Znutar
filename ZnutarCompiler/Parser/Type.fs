namespace Znutar.Parser

open FParsec

open Znutar

module Type =

    open Common

    let private parseType, private parseTypeRef =
        createParserForwardedToRef ()

    let private parseConstant =
        Identifier.parse |>> TypeConstant

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

    let private parseTypeImpl =

        let create =
            parse {
                do! skipString "->" >>. spaces
                return (fun inpType outType ->
                    inpType ^=> outType)
            }

        chainr1   // type arrow is right-associative
            (parseSimpleType .>> spaces)
            (create .>> spaces)

    do parseTypeRef.Value <- parseTypeImpl

    let parse = parseType
