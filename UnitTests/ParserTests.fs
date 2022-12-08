namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting

open Znutar
open Znutar.Parser

[<TestClass>]
type ParserTests() =

    [<TestMethod>]
    member _.TypeArrow() =
        result {
            let text = "'a -> 'b -> 'c"
            let expected =
                Type.variable "a"
                    ^=> Type.variable "b"
                        ^=> Type.variable "c"
            let expected' =
                Type.variable "a"
                    ^=> (Type.variable "b"
                        ^=> Type.variable "c")
            Assert.AreEqual(expected, expected')
            let! actual = Parser.run Type.parse text
            Assert.AreEqual(expected, actual, actual.Unparse())
        } |> Assert.Ok
