namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting

open Znutar
open Znutar.Parser

[<TestClass>]
type ParserTests() =

    [<TestMethod>]
    member _.Expression() =
        result {
            let text = "x + 3"
            let expected =
                BinaryOperationExpr {
                    Left = IdentifierExpr (Identifier.create "x")
                    Right = LiteralExpr (IntLiteral 3)
                    Operator = BinaryOperator.Plus
                }
            let! actual = Parser.run Expression.parse text
            Assert.AreEqual(expected, actual, actual.Unparse())
        } |> Assert.Ok

    [<TestMethod>]
    member _.MemberAccess() =
        result {
            let text = "(a b).c . d"
            let expected =
                MemberAccessExpr {
                    Expression =
                        MemberAccessExpr {
                            Expression =
                                ApplicationExpr {
                                    Function = IdentifierExpr (Identifier.create "a")
                                    Argument = IdentifierExpr (Identifier.create "b")
                                }
                            Identifier = Identifier.create "c"
                        }
                    Identifier = Identifier.create "d"
                }
            let! actual = Parser.run Expression.parse text
            Assert.AreEqual(expected, actual, actual.Unparse())
        } |> Assert.Ok

    [<TestMethod>]
    member _.SyntaxError() =
        let text = "invalid?"
        let actual = Parser.run Expression.parse text
        match actual with
            | Error (InvalidSyntax _) -> ()
            | _ -> Assert.Fail()

    [<TestMethod>]
    member _.InvalidString() =
        let text = "\"unterminated"
        let actual = Parser.run Expression.parse text
        match actual with
            | Error (InvalidSyntax _) -> ()
            | _ -> Assert.Fail()

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

    [<TestMethod>]
    member _.Tuple() =
        result {
            let text = "j * t*'o"
            let expected =
                MultiItemList.create
                    (Type.constant "j")
                    (Type.constant "t")
                    [Type.variable "o"]
                    |> TypeTuple
            let! actual = Parser.run Type.parse text
            Assert.AreEqual(expected, actual, actual.Unparse())
        } |> Assert.Ok
