namespace Znutar

module Program =
    result {
        let text =
            """
            let x =
                let a = 1;
                let b = 2;
                a + b;
            x * 2;
            """
        let assemblyName = "Test"
        do! Compiler.compile
                Array.empty
                assemblyName
                $"{assemblyName}.dll"
                text
        return! Process.run assemblyName
    } |> printfn "%A"
