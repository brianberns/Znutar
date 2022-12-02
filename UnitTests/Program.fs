namespace Znutar

module Program =
    result {
        let text =
            """
            decl id = fun x -> x;
            id true
            """
        let assemblyName = "Test"
        do! Compiler.compile assemblyName text
        return! Process.run assemblyName
    } |> printfn "%A"
