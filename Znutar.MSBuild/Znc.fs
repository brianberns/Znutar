namespace Znutar.MSBuild

open Microsoft.Build.Utilities
open Znutar.Transpiler

/// Znutar compiler task.
type Znc() =
    inherit Task()
    member val OutputAssembly = "" with get, set
    member val OutputRefAssembly = "" with get, set
    override this.Execute() =
        match Compiler.compile "Sample" this.OutputAssembly "1 + 1" with
            | Ok () -> true
            | Error err ->
                printfn "%A" err
                false
