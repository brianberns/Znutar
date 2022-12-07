namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting
open Znutar

[<TestClass>]
type TypeInferenceTests() =

    [<TestMethod>]
    member _.Let() =
        let text = "let x = 2 in 3 * x"
        result {
            let! expr = Parser.run Parser.parseExpression text
            let expected = Type.int
            let! _, expr' =
                TypeInference.inferExpression
                    TypeEnvironment.empty expr
            let actual = expr'.Type
            Assert.AreEqual(expected, actual, actual.Unparse())
        } |> Assert.Ok

    [<TestMethod>]
    member _.Lambda() =
        let text = "fun x -> x + 1"
        result {
            let! expr = Parser.run Parser.parseExpression text
            let expected = Type.int ^=> Type.int
            let! _, expr' =
                TypeInference.inferExpression
                    TypeEnvironment.empty expr
            let actual = expr'.Type
            Assert.AreEqual(expected, actual, actual.Unparse())
        } |> Assert.Ok

    // https://courses.cs.cornell.edu/cs3110/2021sp/textbook/interp/letpoly.html
    [<TestMethod>]
    member _.Polymorphic() =
        let text =
            """
            let id = fun x -> x in
            let a = id 0 in
            id true
            """
        result {
            let! expr = Parser.run Parser.parseExpression text
            let expected = Type.bool
            let! _, expr' =
                TypeInference.inferExpression
                    TypeEnvironment.empty expr
            let actual = expr'.Type
            Assert.AreEqual(expected, actual, actual.Unparse())
        } |> Assert.Ok

    // https://courses.cs.cornell.edu/cs3110/2021sp/textbook/interp/reconstruction.html
    [<TestMethod>]
    member _.Arrows() =
        let text = "fun f -> fun x -> f (x + 1)"
        let sType = "(int -> 'a) -> int -> 'a"
        result {
            let! expr = Parser.run Parser.parseExpression text
            let! expected = Parser.run Parser.Type.parse sType
            let! _, expr' =
                TypeInference.inferExpression
                    TypeEnvironment.empty expr
            let actual = expr'.Type
            let! subst = Substitution.unify expected actual
            let (KeyValue(tv, typ)) = Seq.exactlyOne subst
            Assert.AreEqual("'a", TypeVariable.unparse tv)
            Assert.IsTrue(
                (match typ with
                    | TypeVariable _ -> true
                    | _ -> false),
                typ.Unparse())
        } |> Assert.Ok

    [<TestMethod>]
    member _.Const() =
        let text =
            """
            let const = fun x -> fun y -> x
            in const false 6
            """
        result {
            let! expr = Parser.run Parser.parseExpression text
            let expected = Type.bool
            let! _, expr' =
                TypeInference.inferExpression
                    TypeEnvironment.empty expr
            let actual = expr'.Type
            Assert.AreEqual(expected, actual, actual.Unparse())
        } |> Assert.Ok

    [<TestMethod>]
    member _.Fail() =
        let text = "false * 1"
        result {
            let! expr = Parser.run Parser.parseExpression text
            let expected =
                cerror (
                    UnificationFailure (Type.bool, Type.int))
            let actual =
                TypeInference.inferExpression
                    TypeEnvironment.empty expr
            Assert.AreEqual(expected, actual)
        } |> Assert.Ok
