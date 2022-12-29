﻿namespace Znutar.Parser

open System
open System.Globalization

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

        /// \uHHHH
        let private parseHexEscapedChar =
            parse {
                do! skipString "\u"
                let! c0 = hex
                let! c1 = hex
                let! c2 = hex
                let! c3 = hex
                let n =
                    let str = String([| c0; c1; c2 ; c3 |])
                    Int32.Parse(str, NumberStyles.HexNumber)
                return Convert.ToChar(n)
            }

        let private quote = '"'

        let private parseEscapedChar =
            skipChar '\\'
                >>. choice [
                    pchar quote
                    pchar '\\'
                ]

        let private parseChar =
            choice [
                parseHexEscapedChar
                parseEscapedChar
                satisfy (fun c -> c <> quote)
            ]

        let private parseString =
            between
                (skipChar quote)
                (skipChar quote)
                (many parseChar)
                |>> List.toArray
                |> attempt

        let private parseLiteral =
            choice [
                skipString "true" >>% BoolLiteral true
                skipString "false" >>% BoolLiteral false

                pint32 |>> IntLiteral

                parseString |>> StringLiteral
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
                Identifier.parse |>> IdentifierExpr
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
            parse {
                do! spaces >>. skipChar '.' >>. spaces
                return! Identifier.parse
            } |> attempt

        parse {
            let! expr = SimpleExpr.parse
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

        let parseArgs =
            parse {
                do! spaces
                return! parseComplexExpr
            } |> attempt

        parse {
            let! expr = parseComplexExpr
            let! args = many parseArgs
            if args.IsEmpty then
                return expr
            else
                return (expr, args)
                    ||> List.fold (fun func arg ->
                            ApplicationExpr {
                                Function = func
                                Argument = arg
                            })
        }

    /// Parses any expression.
    let private parseExprImpl =

        let create (str, op) =
            parse {
                do! spaces >>. skipString str >>. spaces
                return (fun left right ->
                    BinaryOperationExpr {
                        Operator = op
                        Left = left
                        Right = right
                    })
            } |> attempt

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

        chainl1 parseComplexExprs parseOp

    do parseExpressionRef.Value <- parseExprImpl

    /// Parses an expression.
    let parse = parseExpression
