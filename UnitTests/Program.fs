namespace Znutar

module Program =
    result {
        let text = "1 + 1"
        let assemblyName = "Test"
        do! Compiler.compile assemblyName $"{assemblyName}.dll" text
        return! Process.run assemblyName
    } |> printfn "%A"
