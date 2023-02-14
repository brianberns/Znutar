namespace Znutar

open System

/// Monadic option builder.
type OptionBuilder() =
    member _.Return(value) = Some value
    member _.ReturnFrom(opt : Option<_>) = opt
    member _.Bind(opt, f) = Option.bind f opt
    member _.Zero() = None
    member _.Combine(opt, f) = Option.bind f opt
    member _.Delay(f : unit -> Option<_>) = f
    member _.Run(f : unit -> Option<_>) = f ()

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
module OptionBuilder =

    /// Monadic option builder.
    let option = OptionBuilder()

module Option =

    // https://hoogle.haskell.org/?hoogle=foldM
    /// Monadic fold.
    let foldM f state items =

        let rec loop state = function
            | item :: tail ->
                option {
                    let! state' = f state item
                    return! loop state' tail
                }
            | [] -> Some state

        loop state items
