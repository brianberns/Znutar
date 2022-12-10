namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting

type Assert private () =

    // Improves error message for F# types (e.g. discriminated unions).
    static member AreEqual<'t when 't : equality>(expected : 't, actual : 't) =
        if actual <> expected then
            sprintf "\nExpected: %A.\nActual:   %A" expected actual
                |> Assert.Fail

    // Improves error message for F# types (e.g. discriminated unions).
    static member AreEqual<'t when 't : equality>(expected : 't, actual : 't, msg) =
        if actual <> expected then
            sprintf "%s\nExpected: %A.\nActual:   %A" msg expected actual
                |> Assert.Fail

    static member Ok(result) =
        match result with
            | Ok () -> ()
            | Error err -> Assert.Fail(string err)

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
        with exn ->
            cerror (
                Znutar.Transpiler.InternalError exn.Message)
