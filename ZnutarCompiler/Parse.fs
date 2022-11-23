﻿namespace Znutar

open FParsec

module Parse =

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

    let private parseKeyword =
        [
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
            |> List.map pstring
            |> choice

    let private parseIdentifier : Parser<_, unit> =
        notFollowedBy (lookAhead parseKeyword)
            >>. identifier (IdentifierOptions ())
            |>> Name

    let parseExpression, private parseExpressionRef =
        createParserForwardedToRef ()

    let private parseVariable : Parser<Variable, _> =
        parseIdentifier

    let private parseApplication =
        parse {
            let! func = parseExpression
            do! spaces
            let! arg = parseExpression
            return {
                Function = func
                Argument = arg
            }
        }

    let private parseLambdaAbstraction =
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

    let private parseParenExpression =
        parseParens parseExpression

    let private parseSimpleExpr : Parser<_, _> =
        choice [
            parseVariable |>> VariableExpr
            parseLambdaAbstraction |>> LambdaExpr
            parseLetBinding |>> LetExpr
            parseLiteral |>> LiteralExpr
            parseIf |>> IfExpr
            parseFix |>> FixExpr
            parseParenExpression
        ]

    let private parseSimpleExprs =

        let rec gather = function
            | [] -> failwith "Unexpected"
            | [expr] -> expr
            | func :: arg :: tail ->
                let expr =
                    ApplicationExpr {
                        Function = func
                        Argument = arg
                    }
                gather (expr :: tail)

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

    /// Runs the given parser on the given text.
    let run parser text =
        let parser' = parser .>> eof
        match runParserOnString parser' () "" text with
            | Success (result, _, _) -> Result.Ok result
            | Failure (msg, _, _) -> Result.Error msg
