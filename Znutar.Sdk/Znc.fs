namespace Znutar.Sdk

open System.IO

open Microsoft.Build.Utilities
open Microsoft.Build.Framework

open Znutar.Transpiler

/// Znutar compiler task.
/// See https://learn.microsoft.com/en-us/visualstudio/msbuild/tutorial-custom-task-code-generation.
type Znc() =
    inherit Task()

    [<Required>]
    member val OutputAssembly = "" with get, set

    [<Required>]
    member val Sources = "" with get, set

    override this.Execute() =

        try
            let assemblyName =
                Path.GetFileNameWithoutExtension(this.Sources)
            let result =
                Compiler.compileFile
                    assemblyName
                    this.OutputAssembly
                    this.Sources
            match result with
                | Ok () -> ()
                | Error err ->
                    this.Log.LogError(string err)
        with exn ->
            this.Log.LogErrorFromException(exn)

        not this.Log.HasLoggedErrors
