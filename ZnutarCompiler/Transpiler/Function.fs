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
    let tryCreate (letb : AnnotatedLetBinding) =

        let rec gatherLambdas = function
            | AnnotatedLambdaExpr lam ->
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

    /// Transpiles the given function using the given transpiler.
    let transpile (transpileExpr : ExpressionTranspiler) func =
        result {
                // the return type might itself be a function (e.g. let f x = id)
            let! parmTypes, returnType =
                Type.splitArrows
                    func.Parameters.Length
                    func.Scheme.Type

                // create parameters
            let parmNodes =
                List.zip func.Parameters parmTypes
                    |> List.map (fun (ident, typ) ->
                        Parameter(
                            Identifier(ident.Name))
                            .WithType(Type.transpile typ))

                // transpile sub-expressions
            let! bodyStmtNodes, bodyExprNode =
                transpileExpr func.FunctionBody
            let! nextStmtNodes, nextExprNode =
                transpileExpr func.ExpressionBody

                // create function
            let funcNode =
                let funcNode =
                    LocalFunctionStatement(
                        returnType = Type.transpile returnType,
                        identifier = func.Identifier.Name)
                        .WithParameterList(
                            ParameterList(
                                Syntax.separatedList parmNodes))
                        .WithBody(
                            Block([|
                                yield! bodyStmtNodes
                                yield ReturnStatement(bodyExprNode)
                            |]))
                match func.Scheme.TypeVariables with
                    | [] -> funcNode
                    | tvs ->
                        let typeParmNodes =
                            tvs
                                |> List.map (fun tv ->
                                    TypeParameter(Identifier(tv.Name)))
                        funcNode.WithTypeParameterList(
                            TypeParameterList(
                                Syntax.separatedList typeParmNodes))

                // gather results
            let stmtNodes =
                [
                    yield funcNode :> Syntax.StatementSyntax
                    yield! nextStmtNodes
                ]
            return stmtNodes, nextExprNode
        }
