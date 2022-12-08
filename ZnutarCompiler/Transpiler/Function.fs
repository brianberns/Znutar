namespace Znutar

open Microsoft.CodeAnalysis.CSharp

/// let const x y = x in next
type Function =
    {
        /// E.g. "const"
        Identifier : Identifier

        /// E.g. [x; y]
        Arguments : List<Identifier>

        /// E.g. x
        FunctionBody : AnnotatedExpression

        /// E.g. <'a, 'b>('a -> 'b -> 'a)
        Scheme : Scheme

        /// E.g. next
        ExpressionBody : AnnotatedExpression
    }

module Function =

    /// From: let const = fun x -> fun y -> x in body
    /// To:   let const (x, y) = x in body
    let tryCreate (letb : AnnotatedLetBinding) =

        let rec gatherLambdas = function
            | LambdaExpr lam ->
                lam :: gatherLambdas lam.Body
            | _ -> []

        let lams = gatherLambdas letb.Argument
        if lams.Length = 0 then None
        else
            Some {
                Identifier = letb.Identifier
                Arguments =
                    lams
                        |> List.map (fun lam -> lam.Identifier)
                FunctionBody =
                    let lam = List.last lams
                    lam.Body
                Scheme = letb.Scheme
                ExpressionBody = letb.Body
            }
