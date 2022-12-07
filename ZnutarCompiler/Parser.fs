namespace Znutar

open FParsec

type ParserError = ParserError of string
    with interface ICompilerError

module Parser =

#if DEBUG
    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            let safeResult = string reply.Result
            printfn "%A: Leaving %s (%A): %A" stream.Position label reply.Status safeResult
            reply
#endif

    let private parseBrackets cOpen cClose parser =
        parse {
            do! skipChar cOpen >>. spaces
            let! value = parser
            do! spaces >>. skipChar cClose
            return value
        }

    let private parseParens parser =
        parseBrackets '(' ')' parser

    let private parseAngles parser =
        parseBrackets '<' '>' parser

    let private keywords =
        set [
            "fun"
            "let"
            "in"
            "true"
            "false"
            "if"
            "then"
            "else"
            "fix"
        ]

    let private parseIdentifier : Parser<_, unit> =
        parse {
            let! name =
                identifier (IdentifierOptions ())      // to-do: refine
            if keywords |> Set.contains name |> not then
                return Identifier.create name
        } |> attempt

    module Type =

        let private parseType, private parseTypeRef =
            createParserForwardedToRef ()

        let private parseConstant =
            parseIdentifier |>> TypeConstant

        let private parseVariableIdentifier =
            skipChar '\''
                >>. parseIdentifier   // don't include apostrophe

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

        let parse = parseType

        do parseTypeRef.Value <- parseTypeImpl

    module private Expression =

        let private parseExpression, private parseExpressionRef =
            createParserForwardedToRef ()

        let private parseLambdaAbstraction =   // to-do: fold multiple arguments
            parse {
                do! skipString "fun" >>. spaces
                let! ident = parseIdentifier
                do! spaces >>. skipString "->" >>. spaces
                let! body = parseExpression
                return {
                    LambdaAbstraction.Identifier = ident
                    Body = body
                }
            }

        let private parseLetBinding =
            parse {
                do! skipString "let" >>. spaces
                let! ident = parseIdentifier
                do! spaces >>. skipChar '=' >>. spaces
                let! arg = parseExpression
                do! spaces >>. skipString "in" >>. spaces
                let! body = parseExpression
                return {
                    Identifier = ident
                    Argument = arg
                    Body = body
                }
            }

        let private parseLiteral =
            choice [
                pint32 |>> IntLiteral
                skipString "true" >>% BoolLiteral true
                skipString "false" >>% BoolLiteral false
            ]

        let private parseIf =
            parse {
                do! skipString "if" >>. spaces
                let! cond = parseExpression
                do! spaces >>. skipString "then" >>. spaces
                let! trueBranch = parseExpression
                do! spaces >>. skipString "else" >>. spaces
                let! falseBranch = parseExpression
                return {
                    Condition = cond
                    TrueBranch = trueBranch
                    FalseBranch = falseBranch
                }
            }

        let private parseFix =
            parse {
                do! skipString "fix" >>. spaces
                return! parseExpression
            }

        let private parseAnnotation =
            parse {
                let! expr = parseExpression
                do! spaces >>. skipChar ':' >>. spaces
                let! typ = Type.parse
                return {
                    Expression = expr
                    Type = typ
                }
            }
                |> parseParens
                |> attempt

        let private parseParenExpression =
            parseParens parseExpression

        let private parseSimpleExpr : Parser<_, _> =
            choice [
                parseIdentifier |>> VariableExpr
                parseLambdaAbstraction |>> LambdaExpr
                parseLetBinding |>> LetExpr
                parseLiteral |>> LiteralExpr
                parseIf |>> IfExpr
                parseFix |>> FixExpr
                parseAnnotation |>> AnnotationExpr
                parseParenExpression
            ]

        let private parseSimpleExprs =

            let gather = function
                | [] -> failwith "Unexpected"
                | [expr] -> expr
                | func :: exprs ->
                    (func, exprs)
                        ||> List.fold (fun func arg ->
                                ApplicationExpr {
                                    Function = func
                                    Argument = arg
                                })

            many1 (parseSimpleExpr .>> spaces)
                |>> gather

        let private parseExprImpl =

            let create (str, op) =
                parse {
                    do! skipString str >>. spaces
                    return (fun left right ->
                        BinaryOperationExpr {
                            Operator = op
                            Left = left
                            Right = right
                        })
                }

            let parseOp =
                [
                    "+", Plus
                    "-", Minus
                    "*", Times
                    "=", Equals
                ]
                    |> List.map create
                    |> choice

            chainl1
                (parseSimpleExprs .>> spaces)
                (parseOp .>> spaces)

        do parseExpressionRef.Value <- parseExprImpl

        let parse = parseExpression

    let parseExpression =
        spaces
            >>. Expression.parse
            .>> spaces

    /// Runs the given parser on the given text.
    let run parser text =
        let parser' = parser .>> eof
        match runParserOnString parser' () "" text with
            | Success (result, _, _) -> Result.Ok result
            | Failure (msg, _, _) -> cerror (ParserError msg)
