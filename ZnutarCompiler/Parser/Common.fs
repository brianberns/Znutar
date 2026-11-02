namespace Znutar.Parser

open System
open FParsec
open Znutar

/// Common parser functions.
module private Common =

#if DEBUG
    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            let safeResult = string reply.Result
            printfn "%A: Leaving %s (%A): %A" stream.Position label reply.Status safeResult
            reply
#endif

    let parseBrackets cOpen cClose parser =
        between
            (skipChar cOpen >>. spaces)
            (spaces >>. skipChar cClose)
            parser

    let parseParens parser =
        parseBrackets '(' ')' parser

    let parseAngles parser =
        parseBrackets '<' '>' parser

    /// Parses the given keyword, which must not be immediately
    /// followed by an identifier character. E.g. "in" but not "inx".
    let skipKeyword str : Parser<unit, _> =
        skipString str
            >>. notFollowedBy (satisfy (fun c -> Char.IsLetterOrDigit(c) || c = '_'))
            |> attempt

module private Identifier =

    let private keywords =
        set [
            "fun"
            "let"
            "rec"
            "in"
            "true"
            "false"
            "if"
            "then"
            "else"
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
            | Failure (msg, _, _) -> Result.Error (InvalidSyntax msg)
