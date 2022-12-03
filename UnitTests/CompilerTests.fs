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

    [<TestMethod>]
    member _.Factorial() =
        let text =
            """
            decl factorial = fix (fun fact -> fun n ->
                if n = 0 then 1
                else n * fact (n - 1));

            factorial 6
            """
        Assert.AreEqual(Ok "720", run text)

    [<TestMethod>]
    member _.Identity() =
        let text =
            """
            decl id = fun x -> x;
            id true
            """
        Assert.AreEqual(Ok "True", run text)

    [<TestMethod>]
    member _.Const() =
        let text =
            """
            decl const = fun x -> fun y -> x;
            const 5 6
            """
        Assert.AreEqual(Ok "True", run text)
