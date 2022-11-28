namespace Znutar

module Program =
    result {
        let text = "let x = 1 in x"
        let! expr = Parser.run Parser.Expression.parse text
        return! TypeInference.inferExpr TypeEnvironment.empty expr
    } |> printfn "%A"
