namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting
open Znutar

[<TestClass>]
type InterpreterTests() =

    [<TestMethod>]
    member _.Subtraction() =

        result {
            let text = "3 - 2"
            let! expr = Parser.run Parser.Expression.parse text
            let! actual = Interpreter.eval expr
            Assert.AreEqual(IntValue 1, actual)
        } |> Assert.Ok

        result {
            let text = "3-2"   // to-do: support this
            let! expr = Parser.run Parser.Expression.parse text
            let! actual = Interpreter.eval expr
            Assert.AreEqual(IntValue 1, actual)
        } |> ignore // |> Assert.Ok

    [<TestMethod>]
    member _.Identity() =
        result {
            let text =
                """
                let id = fun x -> x in
                id true
                """
            let! expr = Parser.run Parser.Expression.parse text
            let! actual = Interpreter.eval expr
            Assert.AreEqual(BoolValue true, actual)
        } |> Assert.Ok

    [<TestMethod>]
    member _.Factorial() =
        result {
            let text =
                """
                let factorial = fix (fun fact -> fun n ->
                    if n = 0 then 1
                    else n * fact (n - 1)) in
                factorial 6
                """
            let! expr = Parser.run Parser.Expression.parse text
            let! actual = Interpreter.eval expr
            Assert.AreEqual(IntValue 720, actual)
        } |> Assert.Ok
