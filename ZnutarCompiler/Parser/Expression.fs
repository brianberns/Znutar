namespace Znutar.Parser

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

        module private Literal =

            let private parseBool =
                choice [
                    skipString "true" >>% true
                    skipString "false" >>% false
                ]

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

            let private parseUnit =
                skipString "()"

            let parse =
                choice [
                    parseBool |>> BoolLiteral
                    pint32 |>> IntLiteral
                    parseString |>> StringLiteral
                    parseUnit >>% UnitLiteral
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
                Literal.parse |>> LiteralExpr
                parseIf |>> IfExpr
                parseAnnotation |>> AnnotationExpr
                parseParenExpression
            ]

    /// Parses an expression that consists of a simple
    /// expression and its trailing member accesses (if any).
    let private parseMemberAccessExpr =

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

    /// Parses one or more expressions, folding them
    /// together as function applications (if necessary).
    /// E.g. a b c -> ((a b) c).
    let private parseApplicationExpr =

        let parseArgs =
            parse {
                do! spaces
                return! parseMemberAccessExpr
            } |> attempt

        parse {
            let! expr = parseMemberAccessExpr
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

    /// Parses a chain of binary operations (if any).
    let private parseBinaryOperationExpr =

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

        chainl1 parseApplicationExpr parseOp

    /// Parses a tuple.
    let private parseTupleExpr =

        let parseItem =
            parse {
                do! spaces .>> skipChar ',' >>. spaces
                return! parseBinaryOperationExpr
            } |> attempt

        parse {
            let! typ = parseBinaryOperationExpr
            match! many parseItem with
                | [] -> return typ
                | head :: items ->
                    return MultiItemList.create typ head items
                        |> TupleExpr
        }

    do parseExpressionRef.Value <- parseTupleExpr

    /// Parses an expression.
    let parse = parseExpression
