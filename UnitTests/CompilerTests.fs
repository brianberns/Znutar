namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting

open Znutar

[<TestClass>]
type CompilerTests() =

    let run text =
        let assemblyName = "Test"
        result {
            do!
                Compiler.compile
                    Array.empty
                    assemblyName
                    $"{assemblyName}.dll"
                    text
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
                let x = 1 in
                2 * x in
            let z = 3 in
            y + z
            """
        Assert.AreEqual(Ok "5", run text)

    [<TestMethod>]
    member _.Plus1() =
        let text =
            """
            let plus1 x = x + 1 in
            plus1 5
            """
        Assert.AreEqual(Ok "6", run text)

    [<TestMethod>]
    member _.InfiniteRecursion() =
        let text = "let x = x in 0"
        Assert.IsTrue(run text |> Result.isError)

    [<TestMethod>]
    member _.Factorial() =
        let text =
            """
            let rec factorial n =
                if n = 0 then 1
                else n * factorial (n - 1) in
            factorial 6
            """
        Assert.AreEqual(Ok "720", run text)

    [<TestMethod>]
    member _.GreatestCommonDivisor() =
        let text =
            """
            let abs x =
                if x > 0 then x
                else 0 - x in
            
            let rec gcd a b =
                if b = 0 then abs a
                else gcd b (a % b) in
 
            gcd 400 600
            """
        Assert.AreEqual(Ok "200", run text)

    [<TestMethod>]
    member _.Identity() =
        let text =
            """
            let id x = x in
            id true
            """
        Assert.AreEqual(Ok "True", run text)

    [<TestMethod>]
    member _.Identity2() =
        let text =
            """
            let id x = x in
            let value = id 0 in
            if value = 0 then id true
            else id false
            """
        Assert.AreEqual(Ok "True", run text)

    [<TestMethod>]
    member _.Const() =
        let text =
            """
            let const x y = x in
            const false 6
            """
        Assert.AreEqual(Ok "False", run text)

    [<TestMethod>]
    member _.AnonymousLambda() =
        let text = "(fun x -> fun y -> x) 1 true"
        Assert.AreEqual(Ok "1", run text)

    [<TestMethod>]
    member _.AnonymousLambdaInvalid() =
        let text = "fun x -> fun y -> x"
        Assert.IsTrue(run text |> Result.isError)

    [<TestMethod>]
    member _.CSharpKeyword() =
        let text = "let class = 0 in class"
        Assert.AreEqual(Ok "0", run text)

    [<TestMethod>]
    member _.StringLiteral() =
        let text = "\"This is a string\""
        Assert.AreEqual(Ok "This is a string", run text)
