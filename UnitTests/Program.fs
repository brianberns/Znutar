namespace Znutar

module Program =

    let text =
        """
        let v = false;
        0
        """

    Parse.run Parse.parseProgram text
        |> printfn "%A"
