namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck

open Znutar.Parser
open Znutar.TypeInference

module Gen =

    let from<'t> = Arb.from<'t>.Generator   // to-do: is there a better way to get this?

    let rec pick chooser gn =               // https://github.com/fscheck/FsCheck/issues/625
        gen {
            let! value = gn
            match chooser value with
                | Some v -> return v
                | None -> return! pick chooser gn
        }

module Identifier =

    let arb =
        Gen.elements ['a' .. 'z']       // limit to single character names for simplicity
            |> Gen.map (string >> Identifier.create)
            |> Arb.fromGen

module Unifiable =

    let arb =
        Gen.from<Type * Type>
            |> Gen.pick (fun (type1, type2) ->
                Substitution.unify type1 type2
                    |> Result.toOption
                    |> Option.map (fun subst ->
                        type1, type2, subst))
            |> Arb.fromGen

type Arbitraries =
    static member Identifier() = Identifier.arb
    static member Unifiable() = Unifiable.arb

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
                Parser.run Expression.parse unparsed
            let msg = sprintf "Text: %s\nResult: %A" unparsed reparsed
            reparsed = Ok expr |@ msg

        Check.One(config, parseUnparseIsOriginal)

    [<TestMethod>]
    member _.UnifyTypes() =

        let unify (type1 : Type, type2 : Type, subst : Substitution) =
            let type1' = Substitution.Type.apply subst type1
            let type2' = Substitution.Type.apply subst type2
            let msg =
                sprintf "\nType 1: %s\nType 2: %s\nSubstitution: %s\nType 1': %s\nType 2': %s"
                    (Type.unparse type1)
                    (Type.unparse type2)
                    (Substitution.unparse subst)
                    (Type.unparse type1')
                    (Type.unparse type2')
            type1' = type2' |@ msg

        Check.One(config, unify)
