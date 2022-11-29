namespace Znutar

module Program =
    result {
        let text =
            """
            decl id = fun x -> x;
            id true
            """
        return! Compiler.compile "Test" text
    } |> printfn "%A"
