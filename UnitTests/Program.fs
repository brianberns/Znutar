namespace Znutar

open Znutar.Transpiler

module Program =
    result {
        let text =
            """
            (fun x -> fun y -> x) 1 true
            """
        let assemblyName = "Test"
        do! Transpiler.transpile assemblyName text
        return! Process.run assemblyName
    } |> printfn "%A"
