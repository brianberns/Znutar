namespace Znutar.Parser

open FParsec

open Znutar

module Type =

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
        Common.parseParens parseType

    let private parseSimpleType =
        choice [
            parseConstant
            parseVariable
            parseParenType
        ]

    let private parseTuple =
        parse {
            let! types =
                sepBy1 (parseSimpleType .>> spaces)
                    (skipChar '*' >>. spaces)
            match types with
                | [] -> return! fail "0-tuple"
                | [typ] -> return typ
                | type1 :: type2 :: types ->
                    return TypeTuple {
                        Type1 = type1
                        Type2 = type2
                        Types3N = types
                    }
        }

    let private parseTypeImpl =

        let create =
            parse {
                do! skipString "->" >>. spaces
                return (fun inpType outType ->
                    inpType ^=> outType)
            }

        chainr1   // type arrow is right-associative
            (parseTuple .>> spaces)
            (create .>> spaces)

    do parseTypeRef.Value <- parseTypeImpl

    let parse = parseType
