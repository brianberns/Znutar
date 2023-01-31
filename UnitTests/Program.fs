namespace Znutar

module Program =
    result {
        let text =
            """
            let f = (System.Console.Write : string -> void) in
            f("Hello world")
            """
        let assemblyName = "Test"
        do! Compiler.compile
                [| @"C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.2\ref\net7.0\System.Console.dll" |]
                assemblyName
                $"{assemblyName}.dll"
                text
        return! Process.run assemblyName
    } |> printfn "%A"
