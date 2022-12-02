namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting
open Znutar

[<TestClass>]
type InterpreterTests() =

    [<TestMethod>]
    member _.Subtraction() =

        result {
            let text = "3 - 2"
            let! program = Parser.run Parser.parseProgram text
            let! actual = Interpreter.evalProgram program
            Assert.AreEqual(IntValue 1, actual)
        } |> Assert.Ok

        result {
            let text = "3-2"   // to-do: support this
            let! program = Parser.run Parser.parseProgram text
            let! actual = Interpreter.evalProgram program
            Assert.AreEqual(IntValue 1, actual)
        } |> ignore // |> Assert.Ok

    [<TestMethod>]
    member _.Identity() =
        result {
            let text =
                """
                decl id = fun x -> x;
                id true
                """
            let! program = Parser.run Parser.parseProgram text
            let! actual = Interpreter.evalProgram program
            Assert.AreEqual(BoolValue true, actual)
        } |> Assert.Ok

    [<TestMethod>]
    member _.Factorial() =
        result {
            let text =
                """
                decl factorial = fix (fun fact -> fun n ->
                    if n = 0 then 1
                    else n * fact (n - 1));

                factorial 6
                """
            let! program = Parser.run Parser.parseProgram text
            let! actual = Interpreter.evalProgram program
            Assert.AreEqual(IntValue 720, actual)
        } |> Assert.Ok
