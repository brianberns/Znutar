namespace Znutar

module Program =
    result {
        let text = "(if 0 then true else e)"
        let! expr = Parser.run Parser.Expression.parse text
        return! TypeInference.inferExpression TypeEnvironment.empty expr
    } |> printfn "%A"
