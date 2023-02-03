namespace Znutar

module Program =
    result {
        let text =
            """
            System.Console.Write("Hello world")
            """
        let assemblyName = "Test"
        do! Compiler.compile
                [| @"C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\7.0.2\ref\net7.0\System.Console.dll" |]
                assemblyName
                $"{assemblyName}.dll"
                text
        return! Process.run assemblyName
    } |> printfn "%A"

(*
namespace Test
{
    static class TestType
    {
        static void Main()
        {
            Expression();
        }

        static Znutar.Runtime.Unit Expression()
        {
            return ((System.Func<string, Znutar.Runtime.Unit>)(x =>
            {
                System.Console.Write(x);
                return Znutar.Runtime.Unit.Value;
            }))("Hello world");
        }
    }
}
*)