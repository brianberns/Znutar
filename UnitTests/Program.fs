namespace Znutar

open Znutar.Transpiler

module Program =
    result {
        let text =
            """
            fun x -> x + 1
            """
        let assemblyName = "Test"
        do! Transpiler.transpile assemblyName text
        return! Process.run assemblyName
    } |> printfn "%A"
