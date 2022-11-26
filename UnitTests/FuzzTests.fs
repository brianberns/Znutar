namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck

module Generator =

    let from<'t> = Arb.from<'t>.Generator   // is there a better way to get this?

module Identifier =

    let arb =
        Gen.elements ['a' .. 'z']       // limit to single character names for simplicity
            |> Gen.map (fun c -> { Name = string c })
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

    let unify (type1 : Type) (type2 : Type) =
        match Substitution.unify type1 type2 with
            | Ok subst ->
                let type1' = Substitution.Type.apply subst type1
                let type2' = Substitution.Type.apply subst type2
                let msg =
                    sprintf "\nType 1: %s\nType 2: %s\nSubstitution: %s\nType 1': %s\nType 2': %s"
                        (Type.unparse type1)
                        (Type.unparse type2)
                        (Substitution.toString subst)
                        (Type.unparse type1')
                        (Type.unparse type2')
                type1' = type2' |@ msg
            | _ -> true |@ ""

    [<TestMethod>]
    member _.ParseUnparseIsOriginal() =

        let parseUnparseIsOriginal program =
            let unparsed = Program.unparse program
            let reparsed =
                Parser.run Parser.parseProgram unparsed
            let msg = sprintf "Text: %s\nResult: %A" unparsed reparsed
            reparsed = Ok program |@ msg

        Check.One(config, parseUnparseIsOriginal)

    [<TestMethod>]
    member _.UnifyTypes() =
        let config = { config with MaxTest = 10000 }
        Check.One(config, unify)

    [<TestMethod>]
    member _.UnifyTypeArrows() =

        let unifyArrows arrow1 arrow2 =
            unify (TypeArrow arrow1) (TypeArrow arrow2)

        let config = { config with MaxTest = 10000 }
        Check.One(config, unifyArrows)
