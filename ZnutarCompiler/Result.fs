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
