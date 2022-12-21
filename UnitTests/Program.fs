namespace Znutar

module Program =
    result {
        let text = "System.Console.WriteLine"
        let assemblyName = "Test"
        do! Compiler.compile assemblyName $"{assemblyName}.dll" text
        return! Process.run assemblyName
    } |> printfn "%A"
