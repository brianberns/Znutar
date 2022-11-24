namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type InterpretTests() =

    [<TestMethod>]
    member _.Interpret() =

        (*
        let text =
            """
            let fact = fix (fun fact -> fun n ->
                if (n = 0) then 1
                else (n * (fact (n-1))));

            fact 6
            """
        let program = Parse.run Parse.parseProgram text
        *)
        ()
