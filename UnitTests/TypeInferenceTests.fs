namespace Znutar

open Microsoft.VisualStudio.TestTools.UnitTesting

open Znutar
open Znutar.Parser
open Znutar.TypeInference

[<TestClass>]
type TypeInferenceTests() =

    let infer = Infer.inferExpression Array.empty

    [<TestMethod>]
    member _.Let() =
        let text = "let x = 2 in 3 * x"
        result {
            let! expr = Parser.run Expression.parse text
            let expected = Type.int
            let! expr' = infer expr
            let actual = expr'.Type
            Assert.AreEqual(expected, actual, actual.Unparse())
        } |> Assert.Ok

    [<TestMethod>]
    member _.Lambda() =
        let text = "fun x -> x + 1"
        result {
            let! expr = Parser.run Expression.parse text
            let expected = Type.int ^=> Type.int
            let! expr' = infer expr
            let actual = expr'.Type
            Assert.AreEqual(expected, actual, actual.Unparse())
        } |> Assert.Ok

    [<TestMethod>]
    member _.Annotation() =
        let text = "fun flag -> (flag : bool)"
        result {
            let! expr = Parser.run Expression.parse text
            let expected = Type.bool ^=> Type.bool
            let! expr' = infer expr
            let actual = expr'.Type
            Assert.AreEqual(expected, actual, actual.Unparse())
        } |> Assert.Ok

    // https://courses.cs.cornell.edu/cs3110/2021sp/textbook/interp/letpoly.html
    [<TestMethod>]
    member _.Polymorphic() =
        let text =
            """
            let id = fun x -> x in
            let a = id 0 in
            id true
            """
        result {
            let! expr = Parser.run Expression.parse text
            let expected = Type.bool
            let! expr' = infer expr
            let actual = expr'.Type
            Assert.AreEqual(expected, actual, actual.Unparse())
        } |> Assert.Ok

    // https://courses.cs.cornell.edu/cs3110/2021sp/textbook/interp/reconstruction.html
    [<TestMethod>]
    member _.Arrows() =
        let text = "fun f -> fun x -> f (x + 1)"
        let sType = "(int -> 'a) -> int -> 'a"
        result {
            let! expr = Parser.run Expression.parse text
            let! expected = Parser.run Type.parse sType
            let! expr' = infer expr
            let actual = expr'.Type
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
    member _.Const() =
        let text =
            """
            let const = fun x -> fun y -> x in
            const false 6
            """
        result {
            let! expr = Parser.run Expression.parse text
            let expected = Type.bool
            let! expr' = infer expr
            let actual = expr'.Type
            Assert.AreEqual(expected, actual, actual.Unparse())
        } |> Assert.Ok

    [<TestMethod>]
    member _.Fail() =
        let text = "false * 1"
        result {
            let! expr = Parser.run Expression.parse text
            let expected =
                Error (
                    UnificationFailure (Type.bool, Type.int))
            let actual = infer expr
            Assert.AreEqual(expected, actual)
        } |> Assert.Ok

    // https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf
    (*
    The definition for g is typed in an environment in which x has type
    'a, and the inferred scheme for g is <'b>('b -> 'a * 'b). This scheme
    is polymorphic in 'b, but not in 'a, because the latter is free in the
    type environment at the definition of g. This side condition, that g
    should be generalised only over variables that are not free in the
    type environment, is the only tricky point in the entire Hindley-
    Milner type system.
    *)
    [<TestMethod>]
    member _.Generalizable() =
        let text =
            """
            let f x =
                let g y = (x,y) in
                g in
            f 0
            """
        result {

                // overall type is 'a -> int * 'a
            let! expr = Parser.run Expression.parse text
            let expected =
                Type.variable "a" ^=>
                    TypeTuple (
                        MultiItemList.create
                            Type.int
                            (Type.variable "a")
                            [])
            let! expr' = infer expr
            let actual = expr'.Type
            let! subst = Substitution.unify expected actual
            Assert.AreEqual(1, subst.Count)

                // g has scheme <'b>('b -> 'a * 'b)
            match expr' with
                | LetExpr letF ->
                    match letF.Argument with
                        | LambdaExpr lam ->
                            match lam.Body with
                                | LetExpr letG ->
                                    let expected =
                                        Type.variable "b" ^=>
                                            TypeTuple (
                                                MultiItemList.create
                                                    (Type.variable "a")
                                                    (Type.variable "b")
                                                    [])
                                    let actual = letG.Scheme.Type
                                    let! subst = Substitution.unify expected actual
                                    Assert.AreEqual(2, subst.Count)
                                    Assert.AreEqual(
                                        [subst[TypeVariable.create "b"]],
                                        List.map TypeVariable letG.Scheme.TypeVariables)
                                | _ -> Assert.Fail()
                        | _ -> Assert.Fail()
                | _ -> Assert.Fail()

        } |> Assert.Ok

    // http://www.cs.rpi.edu/~milanova/csci4450/Lecture23.pdf, slide 10
    [<TestMethod>]
    member _.Ungeneralizable() =
        let text = "(fun f -> fun x -> let g = f in g x) (fun y -> y + 1) true"
        result {
            let! expr = Parser.run Expression.parse text
            let expected =
                Error (
                    UnificationFailure (Type.int, Type.bool))
            let actual = infer expr
            Assert.AreEqual(expected, actual)
        } |> Assert.Ok

    // https://cstheory.stackexchange.com/questions/39814/what-is-the-difference-between-system-f-and-hindley-milner-type-system
    [<TestMethod>]
    member _.Untypable() =
        result {

            let text = "let f = fun x -> x in fun x -> f f x"
            let! expr = Parser.run Expression.parse text
            let expected =
                Type.variable "a" ^=> Type.variable "a"
            let! expr' = infer expr
            let actual = expr'.Type
            let! subst = Substitution.unify expected actual
            Assert.AreEqual(1, subst.Count)

            let text = "(fun f -> fun x -> f f x) (fun x -> x)"
            let! expr = Parser.run Expression.parse text
            match infer expr with
                | Error (
                    UnificationFailure (
                        TypeVariable tv1,
                        TypeArrow (
                            TypeVariable tv2,
                            TypeVariable tv3))) as err ->   // can't unify 'a with 'a -> 'b
                    Assert.AreEqual(tv1, tv2)
                    Assert.AreNotEqual(tv1, tv3)
                | _ -> Assert.Fail()
        } |> Assert.Ok
