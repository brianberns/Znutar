namespace Znutar.Transpiler

open System.Collections.Generic

open Znutar
open Znutar.TypeInference

/// Generates C# names that are unique within a program. This is
/// necessary because C# doesn't allow a local to shadow another
/// local in the same method (e.g. let x = 1 in let x = x + 1 in x).
type NameGenerator(identNames : seq<string>, typeVarNames : seq<string>) =

    /// Names to avoid when generating a fresh name.
    let avoid = HashSet<string>(Seq.append identNames typeVarNames)

    /// Names that can't be used for a local (e.g. a C# type parameter).
    let reserved = HashSet<string>(typeVarNames)

    /// Names that have already been used for a local.
    let declared = HashSet<string>()

    /// Answers a fresh name that starts with the given prefix.
    member _.Fresh(prefix : string) =
        let rec loop n =
            let name = $"{prefix}_{n}"
            if avoid.Contains(name) || declared.Contains(name) then
                loop (n + 1)
            else name
        let name = loop 1
        declared.Add(name) |> ignore
        name

    /// Answers a unique identifier for a new binding of the given
    /// identifier. The first binding of a name keeps that name.
    member this.Declare(ident : Identifier) =
        if not (reserved.Contains(ident.Name))
            && declared.Add(ident.Name) then
            ident
        else
            Identifier.create (this.Fresh(ident.Name))

module Rename =

    /// Answers all identifier names and type variable names in the
    /// given expression.
    let rec private gatherNames (annex : AnnotatedExpression) =
        let typeVarNames =
            Type.freeTypeVariables annex.Type
                |> Seq.map (fun tv -> tv.Name)
                |> Seq.toList
        let identNames, typeVarNames' =
            match annex with
                | AnnotatedIdentifierExpr ai ->
                    [ ai.Identifier.Name ], []
                | AnnotatedLambdaExpr lam ->
                    let identNames, typeVarNames = gatherNames lam.Body
                    lam.Identifier.Name :: identNames, typeVarNames
                | AnnotatedLetExpr letb ->
                    let argIdentNames, argTypeVarNames = gatherNames letb.Argument
                    let bodyIdentNames, bodyTypeVarNames = gatherNames letb.Body
                    let schemeTypeVarNames =
                        letb.Scheme.TypeVariables
                            |> List.map (fun tv -> tv.Name)
                    letb.Identifier.Name :: argIdentNames @ bodyIdentNames,
                    schemeTypeVarNames @ argTypeVarNames @ bodyTypeVarNames
                | AnnotatedApplicationExpr app ->
                    gatherNamesMany [ app.Function; app.Argument ]
                | AnnotatedIfExpr iff ->
                    gatherNamesMany [ iff.Condition; iff.TrueBranch; iff.FalseBranch ]
                | AnnotatedBinaryOperationExpr bop ->
                    gatherNamesMany [ bop.Left; bop.Right ]
                | AnnotatedInstanceMemberAccessExpr ima ->
                    gatherNames ima.Expression
                | AnnotatedTupleExpr tuple ->
                    gatherNamesMany (List.ofSeq tuple.Expressions)
                | AnnotatedLiteralExpr _
                | AnnotatedStaticMemberAccessExpr _ -> [], []
        identNames, typeVarNames @ typeVarNames'

    /// Answers all identifier names and type variable names in the
    /// given expressions.
    and private gatherNamesMany annexes =
        let pairs = List.map gatherNames annexes
        List.collect fst pairs, List.collect snd pairs

    /// Gives every binding in the given expression a unique name.
    /// Answers the renamed expression and a generator for further
    /// unique names.
    let uniquify (annex : AnnotatedExpression) =

        let names =
            let identNames, typeVarNames = gatherNames annex
            NameGenerator(identNames, typeVarNames)

        let rec loop (env : Map<Identifier, Identifier>) (annex : AnnotatedExpression) =
            match annex with
                | AnnotatedIdentifierExpr ai ->
                    let ident =
                        env
                            |> Map.tryFind ai.Identifier
                            |> Option.defaultValue ai.Identifier
                    AnnotatedIdentifierExpr { ai with Identifier = ident }
                | AnnotatedLambdaExpr lam ->
                    let ident = names.Declare(lam.Identifier)
                    let env' = Map.add lam.Identifier ident env
                    AnnotatedLambdaExpr {
                        lam with
                            Identifier = ident
                            Body = loop env' lam.Body
                    }
                | AnnotatedLetExpr letb ->
                    let ident = names.Declare(letb.Identifier)
                    let env' = Map.add letb.Identifier ident env
                    AnnotatedLetExpr {
                        letb with
                            Identifier = ident
                            Argument =
                                loop
                                    (if letb.Recursive then env' else env)
                                    letb.Argument
                            Body = loop env' letb.Body
                    }
                | AnnotatedApplicationExpr app ->
                    AnnotatedApplicationExpr {
                        app with
                            Function = loop env app.Function
                            Argument = loop env app.Argument
                    }
                | AnnotatedIfExpr iff ->
                    AnnotatedIfExpr {
                        iff with
                            Condition = loop env iff.Condition
                            TrueBranch = loop env iff.TrueBranch
                            FalseBranch = loop env iff.FalseBranch
                    }
                | AnnotatedBinaryOperationExpr bop ->
                    AnnotatedBinaryOperationExpr {
                        bop with
                            Left = loop env bop.Left
                            Right = loop env bop.Right
                    }
                | AnnotatedInstanceMemberAccessExpr ima ->
                    AnnotatedInstanceMemberAccessExpr {
                        ima with
                            Expression = loop env ima.Expression
                    }
                | AnnotatedTupleExpr tuple ->
                    AnnotatedTupleExpr {
                        tuple with
                            Expressions =
                                MultiItemList.map (loop env) tuple.Expressions
                    }
                | AnnotatedLiteralExpr _
                | AnnotatedStaticMemberAccessExpr _ -> annex

        loop Map.empty annex, names
