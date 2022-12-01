﻿namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting
open Znutar

[<TestClass>]
type TypeInferenceTests() =

    [<TestMethod>]
    member this.InferExpression1() =
        let text = "let x = 2 in 3 * x"
        result {
            let! expr = Parser.run Parser.Expression.parse text
            let expected = Type.int
            let! _, actual, expr' =
                TypeInference.inferExpression
                    TypeEnvironment.empty expr
            Assert.AreEqual(expected, actual, actual.Unparse())
            Assert.AreEqual(
                Set.empty,
                Expression.freeTypeVariables expr')
        } |> Assert.Ok

    [<TestMethod>]
    member this.InferExpression2() =
        let text = "fun x -> x + 1"
        result {
            let! expr = Parser.run Parser.Expression.parse text
            let expected = Type.int ^=> Type.int
            let! _, actual, expr' =
                TypeInference.inferExpression
                    TypeEnvironment.empty expr
            Assert.AreEqual(expected, actual, actual.Unparse())
            Assert.AreEqual(
                Set.empty,
                Expression.freeTypeVariables expr')
        } |> Assert.Ok

    // https://courses.cs.cornell.edu/cs3110/2021sp/textbook/interp/reconstruction.html
    [<TestMethod>]
    member this.InferExpression3() =
        let text = "fun f -> fun x -> f (x + 1)"
        (*let sType = "(int -> 'a) -> int -> 'a"*)   // to-do: support type normalization
        let sType = "(int -> 'a) -> (int -> 'a)"
        result {
            let! expr = Parser.run Parser.Expression.parse text
            let! expected = Parser.run Parser.Type.parse sType
            let! _, actual, _ =
                TypeInference.inferExpression
                    TypeEnvironment.empty expr
            let! subst = Substitution.unify expected actual
            let (KeyValue(tv, typ)) = Seq.exactlyOne subst
            Assert.AreEqual("'a", TypeVariable.unparse tv)
            Assert.IsTrue(
                (match typ with
                    | TypeVariable _ -> true
                    | _ -> false),
                typ.Unparse())
        } |> Assert.Ok

    [<TestMethod>]
    member this.InferDeclaration1() =
        let text = "decl const = fun x -> fun y -> x;"
        result {
            let! decl = Parser.run Parser.parseDeclaration text
            let expected =
                let tvX = TypeVariable.create "x1"
                let tvY = TypeVariable.create "y2"
                let scheme =
                    Scheme.create
                        [tvX; tvY]
                        (TypeVariable tvX
                            ^=> TypeVariable tvY
                                ^=> TypeVariable tvX)
                scheme
            let! env, _ =
                TypeInference.inferDeclaration
                    TypeEnvironment.empty decl
            let actual = env[Identifier.create "const"]
            Assert.AreEqual(expected, actual)
        } |> Assert.Ok

    [<TestMethod>]
    member this.InferDeclaration2() =
        let text = "decl twice = fun x -> x + x;"
        result {
            let! decl = Parser.run Parser.parseDeclaration text
            let expected =
                Scheme.create [] (Type.int ^=> Type.int)
            let! env, _ =
                TypeInference.inferDeclaration
                    TypeEnvironment.empty decl
            let actual = env[Identifier.create "twice"]
            Assert.AreEqual(expected, actual)
        } |> Assert.Ok

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
                let tv = TypeVariable.create "x1"
                let scheme =
                    Scheme.create
                        [tv]
                        (TypeVariable tv ^=> TypeVariable tv)
                scheme, Type.bool
            let! env, typ, _ =
                TypeInference.inferProgram program
            let actual = env[Identifier.create "id"], typ
            Assert.AreEqual(expected, actual)
        } |> Assert.Ok

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
        } |> Assert.Ok
