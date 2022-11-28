namespace Znutar

module Program =
    result {
        let text = "let x = 1 in x"
        let! expr = Parser.run Parser.Expression.parse text
        return! TypeInference.infer TypeEnvironment.empty expr
    } |> printfn "%A"
