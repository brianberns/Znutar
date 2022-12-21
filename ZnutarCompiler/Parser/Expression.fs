namespace Znutar.Parser

open FParsec
open Znutar

module Expression =

    open Common

    /// Parses any expression.
    let private parseExpression, private parseExpressionRef =
        createParserForwardedToRef ()

    module private SimpleExpr =

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

        /// From: let f arg0 arg1 = binding in body
        /// To:   let f = fun arg0 -> fun arg1 -> binding in body
        let private parseLetBinding =
            parse {

                do! skipString "let" >>. spaces
                let! recursive =
                    opt (skipString "rec" .>> spaces)
                        |>> Option.isSome
                let! ident = Identifier.parse .>> spaces
                let! argIdents = many (Identifier.parse .>> spaces)
                do! skipChar '=' >>. spaces
                let! binding = parseExpression
                do! spaces >>. skipString "in" >>. spaces
                let! body = parseExpression

                let binding' =
                    (argIdents, binding)
                        ||> Seq.foldBack (fun argIdent acc ->
                            LambdaExpr {
                                Identifier = argIdent
                                Body = acc
                            })
                return {
                    Recursive = recursive
                    Identifier = ident
                    Argument = binding'
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

        /// E.g. (expr) -> expr.
        let private parseParenExpression =
            parseParens parseExpression

        /// Parses a simple (non-left-recursive) expression.
        let parse : Parser<_, _> =
            choice [
                Identifier.parse |>> VariableExpr
                parseLambdaAbstraction |>> LambdaExpr
                parseLetBinding |>> LetExpr
                parseLiteral |>> LiteralExpr
                parseIf |>> IfExpr
                parseAnnotation |>> AnnotationExpr
                parseParenExpression
            ]

    /// Parses a complex expression, which consists of a simple
    /// expression and its trailing member accesses (if any).
    let private parseComplexExpr =

        let parseAccess =
            skipChar '.'
                >>. spaces
                >>. Identifier.parse

        parse {
            let! expr = SimpleExpr.parse .>> spaces
            let! idents = many parseAccess
            return (expr, idents)
                ||> List.fold (fun acc ident ->
                    MemberAccessExpr {
                        Expression = acc
                        Identifier = ident
                    })
        }

    /// Parses one or more complex expressions, folding them
    /// together as function applications.
    /// E.g. a b c -> ((a b) c).
    let private parseComplexExprs =

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

        many1 (parseComplexExpr .>> spaces)
            |>> gather

    /// Parses any expression.
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
                ">", GreaterThan
                "<", LessThan
                "/", Divide
                "%", Modulo
            ]
                |> List.map create
                |> choice

        chainl1
            (parseComplexExprs .>> spaces)
            (parseOp .>> spaces)

    do parseExpressionRef.Value <- parseExprImpl

    let parse = parseExpression
