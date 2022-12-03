namespace Znutar

module Program =
    result {
        let text = "1"
        let assemblyName = "Test"
        do! Compiler.compile assemblyName text
        return! Process.run assemblyName
    } |> printfn "%A"
