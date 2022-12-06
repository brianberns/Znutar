namespace Znutar

module Program =
    result {
        let text =
            """
            let id = fun x -> x in
            id true
            """
        let assemblyName = "Test"
        do! Compiler.compile assemblyName text
        return! Process.run assemblyName
    } |> printfn "%A"
