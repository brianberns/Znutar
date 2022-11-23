namespace Znutar

module Program =

    Parse.run Parse.parseExpression "if false then l else false"
        |> printfn "%A"
