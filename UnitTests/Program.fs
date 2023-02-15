namespace Znutar

open System

module Program =
    result {
        let text =
            """
            let list = System.Collections.ArrayList() in
            list.Add(System.Object())
            """
        let assemblyName = "Test"
        do! Compiler.compile
                [| typeof<Console>.Assembly.Location |]
                assemblyName
                $"{assemblyName}.dll"
                text
        return! Process.run assemblyName
    } |> printfn "%A"
