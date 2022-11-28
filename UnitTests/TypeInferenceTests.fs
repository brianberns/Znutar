namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting
open Znutar

[<TestClass>]
type TypeInferenceTests() =

    [<TestMethod>]
    member this.InferExpr() =
        let text = "let x = 2 in 3 * x"
        match Parser.run Parser.Expression.parse text with
            | Ok expr ->
                let expected =
                    Ok (Substitution.empty, Type.int)
                        |> Result.map snd
                let actual =
                    TypeInference.infer TypeEnvironment.empty expr
                        |> Result.map snd
                Assert.AreEqual(expected, actual)
            | Error err -> Assert.Fail(string err)

    [<TestMethod>]
    member this.InferDecl() =
        let text = "decl id = fun x -> x;"
        match Parser.run Parser.parseDeclaration text with
            | Ok decl ->
                let expected =
                    let tv = Identifier.create "tv1"
                    let scheme =
                        Scheme.create
                            [tv]
                            (TypeVariable tv => TypeVariable tv)
                    Ok scheme
                let actual =
                    result {
                        let! env =
                            TypeInference.inferTop TypeEnvironment.empty [decl]
                        return env[decl.Identifier]
                    }
                Assert.AreEqual(expected, actual)
            | Error err -> Assert.Fail(string err)

    [<TestMethod>]
    member this.InferFail() =
        let text = "false * 1"
        match Parser.run Parser.Expression.parse text with
            | Ok expr ->
                let expected =
                    cerror (UnificationFailure (Type.bool, Type.int))
                let actual =
                    TypeInference.infer TypeEnvironment.empty expr
                Assert.AreEqual(expected, actual)
            | Error err -> Assert.Fail(string err)
