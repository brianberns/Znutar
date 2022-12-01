namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting

type Assert private () =

    // Improves error message for F# types (e.g. discriminated unions).
    static member AreEqual<'t when 't : equality>(expected : 't, actual : 't) =
        if actual <> expected then
            sprintf "\nExpected: %A.\nActual:   %A" expected actual
                |> Assert.Fail

    // Improves error message for F# types (e.g. discriminated unions).
    static member AreEqual<'t when 't : equality>(expected : 't, actual : 't, msg) =
        if actual <> expected then
            sprintf "%s\nExpected: %A.\nActual:   %A" msg expected actual
                |> Assert.Fail

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
