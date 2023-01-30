namespace Znutar.Transpiler

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Znutar
open Znutar.TypeInference

/// Signature for a function that transpiles an expression
/// to Roslyn.
type ExpressionTranspiler =
    AnnotatedExpression ->
        CompilerResult<
            List<Syntax.StatementSyntax>
                * Syntax.ExpressionSyntax>

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

    /// Creates function signature from its scheme.
    let private getSignature func =

        /// E.g. 'a -> ('b -> 'a) => [ 'a; 'b; 'a ]
        let rec gatherTypes = function
            | TypeArrow (inpType, outType) ->
                inpType :: gatherTypes outType
            | typ -> [typ]

        let types = gatherTypes func.Scheme.Type
        match List.rev types with
            | [] | [_] ->
                Error (
                    InternalError $"Invalid function type: \
                        {func.Scheme.Type.Unparse()}")
            | returnType :: parmTypesRev ->
                Ok (List.rev parmTypesRev, returnType)

    /// Transpiles the given function.
    let transpile (transpileExpr : ExpressionTranspiler) func =
        result {
            let! parmTypes, returnType = getSignature func
            if parmTypes.Length = func.Parameters.Length then   // to-do: prevent earlier - this should be impossible

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
                let! bodyStmtNodes, bodyExprNode =
                    transpileExpr func.FunctionBody
                let! nextStmtNodes, nextExprNode =
                    transpileExpr func.ExpressionBody

                    // create function
                let funcNode =
                    LocalFunctionStatement(
                        returnType = Type.transpile returnType,
                        identifier = func.Identifier.Name)
                        .WithTypeParameterList(
                            TypeParameterList(
                                Syntax.separatedList typeParmNodes))
                        .WithParameterList(
                            ParameterList(
                                Syntax.separatedList parmNodes))
                        .WithBody(
                            Block([|
                                yield! bodyStmtNodes
                                yield ReturnStatement(bodyExprNode)
                            |]))

                    // gather results
                let stmtNodes =
                    [
                        yield funcNode :> Syntax.StatementSyntax
                        yield! nextStmtNodes
                    ]
                return stmtNodes, nextExprNode
            else
                return! Error (
                    InternalError "Function arity mismatch")
        }

/// Type that represents a function call (rather than an abstraction
/// application).
type FunctionCall =
    {
        /// Function being called.
        Function : AnnotatedExpression

        /// Arguments passed to function.
        Arguments : List<AnnotatedExpression>
    }

module FunctionCall =

    /// App (App f a) b -> f, [a; b].
    let private gatherArguments app =

        let rec loop (app : AnnotatedApplication) =
            match app.Function with
                | ApplicationExpr app' ->
                    let expr, args = loop app'
                    expr, app.Argument :: args
                | expr -> expr, [app.Argument]

        let expr, argsRev = loop app
        expr, List.rev argsRev

    /// Converts an abstraction application into a function call.
    /// E.g. (f a) b -> f(a, b).
    let create app =
        let expr, args = gatherArguments app
        {
            Function = expr
            Arguments = args
        }

    /// Transpiles the given function call using the given
    /// transpiler.
    let transpile (transpileExpr : ExpressionTranspiler) funcCall =
        result {

                // transpile function expression being called
            let! funcStmtNodes, funcExprNode =
                transpileExpr funcCall.Function

                // transpile argument expressions
            let! argStmtNodesRev, argExprNodesRev =
                (([], []), funcCall.Arguments)
                    ||> Result.foldM (fun (accStmtNodes, accExprNodes) arg ->
                        result {
                            let! stmtNodes, exprNode = transpileExpr arg
                            let accStmtNodes' =
                                (List.rev stmtNodes) @ accStmtNodes
                            let accExprNodes' =
                                exprNode :: accExprNodes
                            return accStmtNodes', accExprNodes'
                        })
            let argStmtNodes = List.rev argStmtNodesRev
            let argExprNodes = List.rev argExprNodesRev

                // gather results
            let callStmtNodes =
                funcStmtNodes @ argStmtNodes
            let callExprNode : Syntax.ExpressionSyntax =
                let argNodes =
                    argExprNodes
                        |> Seq.map Argument
                        |> Syntax.separatedList
                InvocationExpression(funcExprNode)
                    .WithArgumentList(
                        ArgumentList(argNodes))
            return callStmtNodes, callExprNode
        }
