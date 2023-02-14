namespace Znutar

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Znutar

[<TestClass>]
type CompilerTests() =

    let run text =
        let assemblyName = "Test"
        result {
            do!
                Compiler.compile
                    [| typeof<Console>.Assembly.Location |]
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
        let text = "let rec f x = f (x : int) in f 0"
        match run text with
            | Error (InternalError _) -> ()   // to-do: improve this
            | res -> Assert.Fail(sprintf "%A" res)

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
        match run text with
            | Error (InternalError _) -> ()   // to-do: improve this
            | res -> Assert.Fail(sprintf "%A" res)

    [<TestMethod>]
    member _.CSharpKeyword() =
        let text = "let class = 0 in class"
        Assert.AreEqual(Ok "0", run text)

    [<TestMethod>]
    member _.StringLiteral() =
        let text = "\"This is a string\""
        Assert.AreEqual(Ok "This is a string", run text)

    [<TestMethod>]
    member _.Ignore() =
        let text =
            """
            let ignore x = () in
            ignore 3
            """
        Assert.AreEqual(Ok "", run text)

    [<TestMethod>]
    member _.Tuple() =
        let text =
            """
            let f x = x, 2 * x in
            f 3
            """
        Assert.AreEqual(Ok "(3, 6)", run text)

    /// string -> unit
    [<TestMethod>]
    member _.HelloWorld() =
        let text = "System.Console.Write(\"Hello world\")"
        Assert.AreEqual(Ok "Hello world", run text)

    [<TestMethod>]
    member _.Write() =
        let text =
            """
            let f x =
                System.Console.Write(x : int) in
            f 3
            """
        Assert.AreEqual(Ok "3", run text)

    [<TestMethod>]
    member _.VoidAssignment() =
        let text =
            """
            let x = System.Console.Write("Hello world") in
            x
            """
        Assert.AreEqual(Ok "Hello world", run text)

    [<TestMethod>]
    member _.AnnotatedMemberAccess() =
        let text =
            """
            let f = (System.Console.Write : string -> unit) in
            f("Hello world")
            """
        Assert.AreEqual(Ok "Hello world", run text)

    /// unit -> Guid
    [<TestMethod>]
    member _.NewGuid() =
        let text =
            """
            System.Guid.NewGuid()
            """
        match run text with
            | Ok str ->
                let flag, _ = Guid.TryParse(str)
                Assert.IsTrue(flag)
            | Error err -> Assert.Fail(string err)

    /// property
    [<TestMethod>]
    member _.Now() =
        let text =
            """
            System.DateTime.Now
            """
        match run text with
            | Ok str ->
                let flag, _ = DateTime.TryParse(str)
                Assert.IsTrue(flag)
            | Error err -> Assert.Fail(string err)

    /// unit -> unit
    [<TestMethod>]
    member _.Break() =
        let text =
            """
            System.Diagnostics.Debugger.Break()
            """
        Assert.AreEqual(Ok "", run text)

    /// unit -> obj
    [<TestMethod>]
    member _.Object() =
        let text =
            """
            System.Object()
            """
        Assert.AreEqual(Ok "System.Object", run text)

    /// (int * int * int) -> DateTime
    [<TestMethod>]
    member _.TupledMemberAccess() =
        let text =
            """
            System.DateTime(2023, 1, 1)
            """
        match run text with
            | Ok str ->
                let flag, dt = DateTime.TryParse(str)
                Assert.IsTrue(flag)
                Assert.AreEqual(DateTime(2023, 1, 1), dt)
            | Error err -> Assert.Fail(string err)

    [<TestMethod>]
    member _.IncompleteMemberAccess() =
        let text =
            """
            System.Diagnostics
            """
        Assert.AreEqual(
            Error (UnboundIdentifier (Identifier.create "Diagnostics")),
            run text)

    [<TestMethod>]
    member _.MissingMemberAccess() =
        let text =
            """
            System.Xyzzy
            """
        Assert.AreEqual(
            Error (UnboundIdentifier (Identifier.create "Xyzzy")),
            run text)
