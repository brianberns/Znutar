namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting
open Znutar

[<TestClass>]
type UnifyTests() =

    let x : TypeVariable = { Name = "X" }
    let y : TypeVariable = { Name = "Y" }

    [<TestMethod>]
    member this.UnifySucceed1() =
        let t1 = TypeVariable x => Type.int
        let t2 = Type.bool => TypeVariable y
        let expected =
            Ok ([
                x, Type.bool
                y, Type.int
            ] : Substitution)
        let actual = Substitution.unify t1 t2
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.UnifySucceed2() =
        let t1 = TypeVariable x => TypeVariable x
        let t2 = Type.int => TypeVariable y
        let expected =
            Ok ([
                x, Type.int
                y, Type.int
            ] : Substitution)
        let actual = Substitution.unify t1 t2
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.UnifyFail1() =
        let t1 = Type.int => TypeVariable x
        let t2 = Type.bool => TypeVariable y
        let expected =
            cerror (UnificationFailure (Type.int, Type.bool))
        let actual = Substitution.unify t1 t2
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.UnifyFail2() =
        let t1 = TypeVariable x
        let t2 = TypeVariable x => TypeVariable y
        let expected = cerror (UnificationFailure (t1, t2))
        let actual = Substitution.unify t1 t2
        Assert.AreEqual(expected, actual)
