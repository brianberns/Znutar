namespace Znutar

module Program =
    result {
        let text = "(0 : (u -> v))"
        let! expr = Parser.run Parser.Expression.parse text
        return! TypeInference.inferExpression TypeEnvironment.empty expr
    } |> printfn "%A"
