namespace Znutar

open System

module Program =
    result {
        let text =
            """
            System.DateTime(1, 1, 2023)
            """
        let assemblyName = "Test"
        do! Compiler.compile
                [| typeof<Console>.Assembly.Location |]
                assemblyName
                $"{assemblyName}.dll"
                text
        return! Process.run assemblyName
    } |> printfn "%A"
