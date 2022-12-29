namespace Znutar

module Program =
    let asm =
        System.Reflection.Assembly.LoadFrom(
            @"C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.1\ref\net7.0\System.Console.dll")
    let typ = asm.GetType("System.Console")
    let method = typ.GetMethod("WriteLine", [| typeof<string> |])
    Type.ofMethod method
        |> Type.unparse
        |> printfn "%s"
