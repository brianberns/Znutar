namespace Znutar.Parser

open FParsec
open Znutar

type ParserError = ParserError of string
    with interface ICompilerError

module Common =

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

    let parseParens parser =
        parseBrackets '(' ')' parser

    let parseAngles parser =
        parseBrackets '<' '>' parser

module private Identifier =

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

    let parse : Parser<_, unit> =
        parse {
            let! name =
                identifier (IdentifierOptions ())      // to-do: refine
            if keywords |> Set.contains name |> not then
                return Identifier.create name
        } |> attempt

module Parser =

    /// Runs the given parser on the given text.
    let run parser text =
        let parser' =
            spaces
                >>. parser
                .>> spaces
                .>> eof
        match runParserOnString parser' () "" text with
            | Success (result, _, _) -> Result.Ok result
            | Failure (msg, _, _) -> cerror (ParserError msg)
