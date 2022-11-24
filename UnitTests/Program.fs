namespace Znutar

module Program =
    result {
        let text =
            """
            decl fact = fix (fun fact -> fun n ->
                if (n = 0) then 1
                else (n * (fact (n-1))));

            fact 6
            """
        let! program = Parse.run Parse.parseProgram text
        return! Interpret.evalProgram program
    } |> printfn "%A"
