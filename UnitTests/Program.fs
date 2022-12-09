namespace Znutar

open Znutar.Transpiler

module Program =
    result {
        let text =
            """
            let rec factorial = fun n ->
                if n = 0 then 1
                else n * factorial (n - 1) in
            factorial 6
            """
        let assemblyName = "Test"
        do! Transpiler.transpile assemblyName text
        return! Process.run assemblyName
    } |> printfn "%A"
