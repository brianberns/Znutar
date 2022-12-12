namespace Znutar.MSBuild

open System.IO
open Microsoft.Build.Utilities
open Znutar.Transpiler

/// Znutar compiler task.
type Znc() =
    inherit Task()
    member val OutputAssembly = "" with get, set
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
                | Ok () -> true
                | Error err ->
                    this.Log.LogError(string err)
                    false
        with exn ->
            this.Log.LogError(exn.Message)
            false
