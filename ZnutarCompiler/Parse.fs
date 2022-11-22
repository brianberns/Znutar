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

    let private identifier =
        identifier (IdentifierOptions ())

    let private parseExpr, private parseExprRef =
        createParserForwardedToRef ()

    let private parseVariable : Parser<Variable, _> =
        identifier .>> spaces

    let private parseIf =
        parse {
            do! skipString "if" >>. spaces
            let! cond = parseExpr
            do! spaces
            let! trueBranch = parseExpr
            do! spaces >>. skipString "else" >>. spaces
            let! falseBranch = parseExpr
            return {
                Condition = cond
                TrueBranch = trueBranch
                FalseBranch = falseBranch
            }
        }

    let private parseSimpleExpr =
        choice [
            parseVariable |>> VariableExpr
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
