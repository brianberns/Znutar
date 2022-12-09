namespace Znutar.Parser

open FParsec
open Znutar

module Expression =

    open Common

    let private parseExpression, private parseExpressionRef =
        createParserForwardedToRef ()

    let private parseLambdaAbstraction =   // to-do: fold multiple arguments
        parse {
            do! skipString "fun" >>. spaces
            let! ident = Identifier.parse
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
            let! recurisive =
                opt (skipString "rec" .>> spaces)
                    |>> Option.isSome
            let! ident = Identifier.parse
            do! spaces >>. skipChar '=' >>. spaces
            let! arg = parseExpression
            do! spaces >>. skipString "in" >>. spaces
            let! body = parseExpression
            return {
                Recursive = recurisive
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
            Identifier.parse |>> VariableExpr
            parseLambdaAbstraction |>> LambdaExpr
            parseLetBinding |>> LetExpr
            parseLiteral |>> LiteralExpr
            parseIf |>> IfExpr
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
