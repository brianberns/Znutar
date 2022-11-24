namespace Znutar

open System

type ResultBuilder() =
    member _.Return(x) = Ok x
    member _.ReturnFrom(res : Result<_, _>) = res
    member _.Bind(res, f) = Result.bind f res
    member _.Zero() = Ok ()

[<AutoOpen>]
module ResultBuilder =

    /// Monadic result builder.
    let result = ResultBuilder()

module Result =

    // https://hoogle.haskell.org/?hoogle=foldM
    let foldM f state items =

        let rec loop state = function
            | item :: tail ->
                result {
                    let! state' = f state item
                    return! loop state' tail
                }
            | [] -> Ok state

        loop state items
