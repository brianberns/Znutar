namespace Znutar

module Program =
    result {
        let text =
            """
            let f x = x, 2 * x;
            f 3;
            """
        let assemblyName = "Test"
        do! Compiler.compile
                Array.empty
                assemblyName
                $"{assemblyName}.dll"
                text
        return! Process.run assemblyName
    } |> printfn "%A"
