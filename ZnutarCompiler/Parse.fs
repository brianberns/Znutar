namespace Znutar

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

    let private parseIdentifier : Parser<_, unit> =
        identifier (IdentifierOptions ())

    let private parseExpression, private parseExpressionRef =
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

    let private parseLambda =
        parse {
            let! ident = parseIdentifier
            do! spaces >>. skipString "=>" >>. spaces
            let! body = parseExpression
            return {
                LambdaAbstraction.Identifier = ident
                Body = body
            }
        }

    let private parseIf =
        parse {
            do! skipString "if" >>. spaces
            let! cond = parseExpression
            do! spaces
            let! trueBranch = parseExpression
            do! spaces >>. skipString "else" >>. spaces
            let! falseBranch = parseExpression
            return {
                Condition = cond
                TrueBranch = trueBranch
                FalseBranch = falseBranch
            }
        }

    let private parseSimpleExpr =
        choice [
            parseVariable |>> VariableExpr
            parseApplication |>> ApplicationExpr
            parseLambda |>> LambdaExpr
            parseIf |>> IfExpr
        ]

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
            (parseSimpleExpr .>> spaces)
            (parseOp .>> spaces)

    do parseExpressionRef.Value <- parseExprImpl
