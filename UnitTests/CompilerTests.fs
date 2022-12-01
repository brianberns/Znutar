namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting
open Znutar

[<TestClass>]
type CompilerTests() =

    let run text =
        let assemblyName = "Test"
        result {
            do! Compiler.compile assemblyName text
            return! Process.run assemblyName
        }

    [<TestMethod>]
    member _.Plus1() =
        let text =
            """
            decl plus1 = fun x -> x + 1;
            plus1 5
            """
        Assert.AreEqual(Ok "6", run text)
