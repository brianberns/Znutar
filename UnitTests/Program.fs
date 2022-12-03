namespace Znutar

module Program =
    result {
        let text =
            """
            decl const = fun x -> fun y -> x;
            const 5 6
            """
        let assemblyName = "Test"
        do! Compiler.compile assemblyName text
        return! Process.run assemblyName
    } |> printfn "%A"
