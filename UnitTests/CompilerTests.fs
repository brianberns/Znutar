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
    member _.Simple() =
        let text = "1"
        Assert.AreEqual(Ok "1", run text)

    [<TestMethod>]
    member _.Arithmetic() =
        let text =
            """
            let y =
                let x = 1
                in 2 * x
            in
            let z = 3 in
            y + z
            """
        Assert.AreEqual(Ok "5", run text)

    [<TestMethod>]
    member _.Plus1Decl() =
        let text =
            """
            decl plus1 = fun x -> x + 1;
            plus1 5
            """
        Assert.AreEqual(Ok "6", run text)

    [<TestMethod>]
    member _.Plus1Lambda() =
        let text =
            """
            let plus1 = fun x -> x + 1 in
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
    member _.IdentityDecl() =
        let text =
            """
            decl id = fun x -> x;
            id true
            """
        Assert.AreEqual(Ok "True", run text)

    [<TestMethod>]
    member _.IdentityLambda() =
        let text =
            """
            let id = fun x -> x in
            id true
            """
        Assert.AreEqual(Ok "True", run text)

    [<TestMethod>]
    member _.IdentityDecl2() =
        let text =
            """
            decl id = fun x -> x;

            let value = id 0 in
            if value = 0 then id true
            else id false
            """
        Assert.AreEqual(Ok "True", run text)

    [<TestMethod>]
    member _.Const() =
        let text =
            """
            decl const = fun x -> fun y -> x;
            const false 6
            """
        Assert.AreEqual(Ok "False", run text)
