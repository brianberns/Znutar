namespace Znutar

open Znutar.Transpiler

module Program =
    result {
        let text =
            """
            let const = fun x -> fun y -> x in
            (const false) 0
            """
        let assemblyName = "Test"
        do! Transpiler.transpile assemblyName text
        return! Process.run assemblyName
    } |> printfn "%A"
