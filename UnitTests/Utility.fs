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
    open System.IO
    open System.Reflection

    let run assemblyName =
        try
            result {

                let sourcePath =
                    Path.Combine(
                        Path.GetDirectoryName(
                            Assembly.GetExecutingAssembly().Location),
                        "App.runtimeconfig.json")   // MUST MATCH .NET VERSION
                File.Copy(
                    sourcePath,
                    $"{assemblyName}.runtimeconfig.json",
                    overwrite = true)

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
            Error (InternalError exn.Message)
