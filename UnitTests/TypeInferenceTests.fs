namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting
open Znutar

[<TestClass>]
type TypeInferenceTests() =

    [<TestMethod>]
    member this.InferExpression() =
        let text = "let x = 2 in 3 * x"
        match Parser.run Parser.Expression.parse text with
            | Ok expr ->
                let expected =
                    Ok (Substitution.empty, Type.int)
                        |> Result.map snd
                let actual =
                    TypeInference.inferExpression
                        TypeEnvironment.empty expr
                        |> Result.map snd
                Assert.AreEqual(expected, actual)
            | Error err -> Assert.Fail(string err)

    [<TestMethod>]
    member this.InferDeclaration1() =
        let text = "decl const = fun x -> fun y -> x;"
        match Parser.run Parser.parseDeclaration text with
            | Ok decl ->
                let expected =
                    let tvX = Identifier.create "x1"
                    let tvY = Identifier.create "y2"
                    let scheme =
                        Scheme.create
                            [tvX; tvY]
                            (TypeVariable tvX
                                => (TypeVariable tvY
                                    => TypeVariable tvX))
                    Ok scheme
                let actual =
                    result {
                        let! env =
                            TypeInference.inferDeclaration
                                TypeEnvironment.empty decl
                        return env[Identifier.create "const"]
                    }
                Assert.AreEqual(expected, actual)
            | Error err -> Assert.Fail(string err)

    [<TestMethod>]
    member this.InferDeclaration2() =
        let text = "decl twice = fun x -> x + x;"
        match Parser.run Parser.parseDeclaration text with
            | Ok decl ->
                let expected =
                    let tv = Identifier.create "x1"
                    let scheme =
                        Scheme.create [] (Type.int => Type.int)
                    Ok scheme
                let actual =
                    result {
                        let! env =
                            TypeInference.inferDeclaration
                                TypeEnvironment.empty decl
                        return env[Identifier.create "twice"]
                    }
                Assert.AreEqual(expected, actual)
            | Error err -> Assert.Fail(string err)

    [<TestMethod>]
    member this.InferProgram() =
        let text =
            """
            decl id = fun x -> x;
            id true
            """
        match Parser.run Parser.parseProgram text with
            | Ok program ->
                let expected =
                    let tv = Identifier.create "x1"
                    let scheme =
                        Scheme.create
                            [tv]
                            (TypeVariable tv => TypeVariable tv)
                    Ok (scheme, Type.bool)
                let actual =
                    result {
                        let! env, typ =
                            TypeInference.inferProgram program
                        return env[Identifier.create "id"], typ
                    }
                Assert.AreEqual(expected, actual)
            | Error err -> Assert.Fail(string err)

    [<TestMethod>]
    member this.InferFail() =
        let text = "false * 1"
        match Parser.run Parser.Expression.parse text with
            | Ok expr ->
                let expected =
                    cerror (
                        UnificationFailure (Type.bool, Type.int))
                let actual =
                    TypeInference.inferExpression
                        TypeEnvironment.empty expr
                Assert.AreEqual(expected, actual)
            | Error err -> Assert.Fail(string err)
