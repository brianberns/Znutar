namespace Znutar

open System

module Program =
    result {
        let text =
            """
            let dt = System.DateTime(2023, 1, 1) in
            dt.Years
            """
        let assemblyName = "Test"
        do! Compiler.compile
                [| typeof<Console>.Assembly.Location |]
                assemblyName
                $"{assemblyName}.dll"
                text
        return! Process.run assemblyName
    } |> printfn "%A"
