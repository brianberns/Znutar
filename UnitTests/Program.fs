namespace Znutar

module Process =

    open System.Diagnostics

    let run assemblyName =
        try
            result {
                let psi =
                    ProcessStartInfo(
                        FileName = "dotnet",
                        Arguments = $"{assemblyName}.dll",
                        RedirectStandardOutput = true)
                use proc = new Process(StartInfo = psi)
                proc.Start() |> ignore
                return proc.StandardOutput
                    .ReadToEnd()
                    .Replace("\r", "")
            }
        with exn -> cerror (Unsupported exn.Message)

module Program =
    result {
        let text =
            """
            let plus1 = fun x -> x + 1
            in plus1 5
            """
        let assemblyName = "Test"
        do! Compiler.compile assemblyName text
        return! Process.run assemblyName
    } |> printfn "%A"
