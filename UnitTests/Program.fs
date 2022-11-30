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
            decl id = fun x -> x;
            id true
            """
        let assemblyName = "Test"
        do! Compiler.compile assemblyName text
        return! Process.run assemblyName
    } |> printfn "%A"
