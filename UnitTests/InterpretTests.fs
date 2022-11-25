namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting

type Assert private () =

    // Improves error message for F# types (e.g. discriminated unions).
    static member AreEqual<'t when 't : equality>(expected : 't, actual : 't) =
        if actual <> expected then
            sprintf "\nExpected: %A.\nActual:   %A" expected actual
                |> Assert.Fail

[<TestClass>]
type InterpretTests() =

    [<TestMethod>]
    member _.Factorial() =
        let result =
            result {
                let text =
                    """
                    decl factorial = fix (fun fact -> fun n ->
                        if (n = 0) then 1
                        else (n * (fact (n - 1))));

                    factorial 6
                    """
                let! program = Parse.run Parse.parseProgram text
                return! Interpret.evalProgram program
            }
        Assert.AreEqual(Ok (IntValue 720), result)
