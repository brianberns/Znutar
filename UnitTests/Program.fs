namespace Znutar

module Program =
    result {
        let text =
            """
            decl factorial = fix (fun fact -> fun n ->
                if n = 0 then 1
                else n * fact (n - 1));

            factorial 6
            """
        let assemblyName = "Test"
        do! Compiler.compile assemblyName text
        return! Process.run assemblyName
    } |> printfn "%A"
