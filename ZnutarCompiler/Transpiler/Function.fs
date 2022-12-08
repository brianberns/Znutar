﻿namespace Znutar.Transpiler

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Znutar
open Znutar.TypeInference

/// A function definition.
/// E.g. let const x y = x in next.
type Function =
    {
        /// E.g. "const"
        Identifier : Identifier

        /// E.g. [x; y]
        Parameters : List<Identifier>

        /// E.g. x
        FunctionBody : AnnotatedExpression

        /// E.g. <'a, 'b>('a -> 'b -> 'a)
        Scheme : Scheme

        /// E.g. next
        ExpressionBody : AnnotatedExpression
    }

module Function =

    /// Attempts to convert a let-bound lambda into a function
    /// definition. For example:
    /// * From: let const = fun x -> fun y -> x in next
    /// * To:   let const(x, y) = x in next
    let tryCreate letb =

        let rec gatherLambdas = function
            | LambdaExpr lam ->
                lam :: gatherLambdas lam.Body
            | _ -> []

        let lams = gatherLambdas letb.Argument
        if lams.Length = 0 then None
        else
            Some {
                Identifier = letb.Identifier
                Parameters =
                    lams
                        |> List.map (fun lam -> lam.Identifier)
                FunctionBody =
                    let lam = List.last lams
                    lam.Body
                Scheme = letb.Scheme
                ExpressionBody = letb.Body
            }

    let private createStatementNode
        returnTypeNode
        ident
        typeParmNodes
        parmNodes
        bodyStmtNodes =
        LocalFunctionStatement(
            returnType = returnTypeNode,
            identifier = ident.Name)
            .WithTypeParameterList(
                TypeParameterList(
                    Syntax.separatedList typeParmNodes))
            .WithParameterList(
                ParameterList(
                    Syntax.separatedList parmNodes))
            .WithBody(
                Block(bodyStmtNodes : Syntax.StatementSyntax[]))

    /// Transpiles the given function.
    let transpile transpileExpr func =

        let rec gatherTypes = function
            | TypeArrow (inpType, outType) ->
                inpType :: gatherTypes outType
            | typ -> [typ]

        result {

                // get function signature from scheme
            let! parmTypes, returnType =
                let types = gatherTypes func.Scheme.Type
                match List.rev types with
                    | [] | [_] ->
                        cerror (
                            Unsupported $"Invalid function type: \
                                {func.Scheme.Type.Unparse()}")
                    | returnType :: parmTypesRev ->
                        Ok (List.rev parmTypesRev, returnType)

            if parmTypes.Length = func.Parameters.Length then

                    // create parameters
                let parmNodes =
                    List.zip func.Parameters parmTypes
                        |> List.map (fun (ident, typ) ->
                            Parameter(
                                Identifier(ident.Name))
                                .WithType(Type.transpile typ))

                    // create type parameters
                let typeParmNodes =
                    func.Scheme.TypeVariables
                        |> List.map (fun tv ->
                            TypeParameter(Identifier(tv.Name)))

                    // transpile sub-expressions
                let!
                    (bodyStmtNodes : List<Syntax.StatementSyntax>),
                    (bodyExprNode : Syntax.ExpressionSyntax) =
                        transpileExpr func.FunctionBody
                let! nextStmtNodes, nextExprNode =
                    transpileExpr func.ExpressionBody

                    // create function
                let funcNode =
                    createStatementNode
                        (Type.transpile returnType)
                        func.Identifier
                        typeParmNodes
                        parmNodes
                        [|
                            yield! bodyStmtNodes
                            yield ReturnStatement(bodyExprNode)
                        |]

                    // gather results
                let stmtNodes =
                    [
                        yield funcNode :> Syntax.StatementSyntax
                        yield! nextStmtNodes
                    ]
                return stmtNodes, nextExprNode

            else
                return! cerror (Unsupported "Function arity mismatch")
        }
