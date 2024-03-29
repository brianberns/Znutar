﻿namespace Znutar

open System

/// Monadic result builder.
type ResultBuilder() =
    member _.Return(value) = Ok value
    member _.ReturnFrom(res : Result<_, _>) = res
    member _.Bind(res, f) = Result.bind f res
    member _.Zero() = Ok ()
    member _.Combine(res, f) = Result.bind f res
    member _.Delay(f : unit -> Result<_, _>) = f
    member _.Run(f : unit -> Result<_, _>) = f ()

    member this.While(guard, body) =
        if not (guard ())
        then this.Zero()
        else this.Bind(body (), fun () ->
            this.While(guard, body))

    member this.TryWith(body, handler) =
        try this.ReturnFrom(body ())
        with e -> handler e

    member this.TryFinally(body, compensation) =
        try this.ReturnFrom(body ())
        finally compensation()

    member this.Using(disposable : #IDisposable, body) =
        let body' = fun () -> body disposable
        this.TryFinally(body', fun () ->
            match disposable with
                | null -> ()
                | disp -> disp.Dispose())

    member this.For(sequence : seq<_>, body) =
        this.Using(
            sequence.GetEnumerator(),
            fun enum ->
                this.While(enum.MoveNext,
                    this.Delay(fun () -> body enum.Current)))

[<AutoOpen>]
module ResultBuilder =

    /// Monadic result builder.
    let result = ResultBuilder()

module Result =

    /// Maps a list of items, with the possibility of error along
    /// the way.
    // https://stackoverflow.com/a/53029378/344223
    let traverse f items = 
        let folder head tail =
            result {
                let! h = f head
                let! t = tail
                return h :: t
            }
        let empty = result { return List.empty }
        List.foldBack folder items empty

    /// Converts a list of results into a list result.
    let sequence items =
        traverse id items

    // https://hoogle.haskell.org/?hoogle=foldM
    /// Monadic fold.
    let foldM f state items =

        let rec loop state = function
            | item :: tail ->
                result {
                    let! state' = f state item
                    return! loop state' tail
                }
            | [] -> Ok state

        loop state items
