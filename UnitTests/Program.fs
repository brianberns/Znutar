namespace Znutar

module Program =

    Parse.run Parse.parseExpression "z"
        |> printfn "%A"
