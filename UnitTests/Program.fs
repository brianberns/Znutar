namespace Znutar

module Program =
    result {
        let text =
            """
            let y =
                let x = 1
                in 2 * x
            in
            let z = 3 in
            y + z
            """
        let assemblyName = "Test"
        do! Compiler.compile assemblyName text
        return! Process.run assemblyName
    } |> printfn "%A"
