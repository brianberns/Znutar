namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting
open Znutar

[<TestClass>]
type TypeInferenceTests() =

    let assertOk = function
        | Ok () -> ()
        | Error err -> Assert.Fail(string err)

    [<TestMethod>]
    member this.InferExpression1() =
        let text = "let x = 2 in 3 * x"
        result {
            let! expr = Parser.run Parser.Expression.parse text
            let expected = Type.int
            let! _, actual, _ =
                TypeInference.inferExpression
                    TypeEnvironment.empty expr
            Assert.AreEqual(expected, actual)
        } |> assertOk

    [<TestMethod>]
    member this.InferExpression2() =
        let text = "fun x -> x + 1"
        result {
            let! expr = Parser.run Parser.Expression.parse text
            let expected = Type.int => Type.int
            let! _, actual, _ =
                TypeInference.inferExpression
                    TypeEnvironment.empty expr
            Assert.AreEqual(expected, actual)
        } |> assertOk

    [<TestMethod>]
    member this.InferDeclaration1() =
        let text = "decl const = fun x -> fun y -> x;"
        result {
            let! decl = Parser.run Parser.parseDeclaration text
            let expected =
                let tvX = Identifier.create "x1"
                let tvY = Identifier.create "y2"
                let scheme =
                    Scheme.create
                        [tvX; tvY]
                        (TypeVariable tvX
                            => (TypeVariable tvY
                                => TypeVariable tvX))
                scheme
            let! env, _ =
                TypeInference.inferDeclaration
                    TypeEnvironment.empty decl
            let actual = env[Identifier.create "const"]
            Assert.AreEqual(expected, actual)
        } |> assertOk

    [<TestMethod>]
    member this.InferDeclaration2() =
        let text = "decl twice = fun x -> x + x;"
        result {
            let! decl = Parser.run Parser.parseDeclaration text
            let expected =
                Scheme.create [] (Type.int => Type.int)
            let! env, _ =
                TypeInference.inferDeclaration
                    TypeEnvironment.empty decl
            let actual = env[Identifier.create "twice"]
            Assert.AreEqual(expected, actual)
        } |> assertOk

    [<TestMethod>]
    member this.InferProgram() =
        let text =
            """
            decl id = fun x -> x;
            id true
            """
        result {
            let! program = Parser.run Parser.parseProgram text
            let expected =
                let tv = Identifier.create "x1"
                let scheme =
                    Scheme.create
                        [tv]
                        (TypeVariable tv => TypeVariable tv)
                scheme, Type.bool
            let! env, typ, _ =
                TypeInference.inferProgram program
            let actual = env[Identifier.create "id"], typ
            Assert.AreEqual(expected, actual)
        } |> assertOk

    [<TestMethod>]
    member this.InferFail() =
        let text = "false * 1"
        result {
            let! expr = Parser.run Parser.Expression.parse text
            let expected =
                cerror (
                    UnificationFailure (Type.bool, Type.int))
            let actual =
                TypeInference.inferExpression
                    TypeEnvironment.empty expr
            Assert.AreEqual(expected, actual)
        } |> assertOk
