namespace Znutar

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck

module Generator =

    let from<'t> = Arb.from<'t>.Generator   // is there a better way to get this?

module Identifier =

    let arb =
        Gen.elements ['a' .. 'z']       // limit to single character names for simplicity
            |> Gen.map (string >> Name)
            |> Arb.fromGen

type Arbitraries =
    static member Identifier() = Identifier.arb

[<TestClass>]
type FuzzTests() =

    let config =
        { Config.QuickThrowOnFailure with
            Arbitrary = [ typeof<Arbitraries> ]
            MaxTest = 1000
            Replay = Some (Random.StdGen (0, 0)) }

    [<TestMethod>]
    member _.ParseUnparseIsOriginal() =

        let parseUnparseIsOriginal expr =
            let unparsed = Expression.unparse expr
            let reparsed =
                Parse.run Parse.parseExpression unparsed
            let msg = sprintf "Text: %s\nResult: %A" unparsed reparsed
            reparsed = Ok expr |@ msg

        Check.One(config, parseUnparseIsOriginal)