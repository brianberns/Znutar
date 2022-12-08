namespace Znutar

open Znutar.Transpile

module Program =
    result {
        let text =
            """
            let const = fun x -> fun y -> x in
            0
            """
        let assemblyName = "Test"
        do! Compiler.compile assemblyName text
        return! Process.run assemblyName
    } |> printfn "%A"
