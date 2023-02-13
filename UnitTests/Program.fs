namespace Znutar

open System

module Program =
    (*
    result {
        let text =
            """
            let dt = System.DateTime(2023, 1, 1) in
            dt.Years.ToString()
            """
        let assemblyName = "Test"
        do! Compiler.compile
                [| typeof<Console>.Assembly.Location |]
                assemblyName
                $"{assemblyName}.dll"
                text
        return! Process.run assemblyName
    } |> printfn "%A"
    *)

    open Znutar.TypeInference

    let ma0 =
        {
            Expression = IdentifierExpr (Identifier.create "System")
            Identifier = Identifier.create "Console"
        }
    let ma1 =
        {
            Expression = MemberAccessExpr ma0
            Identifier = Identifier.create "WriteLine"
        }

    TypeEnvironment.create [| typeof<Console>.Assembly |]
        |> TypeEnvironment.tryFindStaticMember ma1
        |> Option.map (fun (schemes, qi) -> schemes.Length, qi)
        |> printfn "%A"
