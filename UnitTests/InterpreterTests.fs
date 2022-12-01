namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type InterpreterTests() =

    [<TestMethod>]
    member _.Subtraction() =

        let res =
            result {
                let text = "3 - 2"
                let! program = Parser.run Parser.parseProgram text
                return! Interpreter.evalProgram program
            }
        Assert.AreEqual(Ok (IntValue 1), res)

        let res =
            result {
                let text = "3-2"   // to-do: support this
                let! program = Parser.run Parser.parseProgram text
                return! Interpreter.evalProgram program
            }
        // Assert.AreEqual(Ok (IntValue 1), res)
        ()

    [<TestMethod>]
    member _.Factorial() =
        let res =
            result {
                let text =
                    """
                    decl factorial = fix (fun fact -> fun n ->
                        if n = 0 then 1
                        else n * fact (n - 1));

                    factorial 6
                    """
                let! program = Parser.run Parser.parseProgram text
                return! Interpreter.evalProgram program
            }
        Assert.AreEqual(Ok (IntValue 720), res)
