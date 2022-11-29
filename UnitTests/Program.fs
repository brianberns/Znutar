namespace Znutar

module Program =
    result {
        let text =
            """
            decl factorial = fix (fun fact -> fun n ->
                if n = 0 then 1
                else n * fact (n - 1));

            factorial 6
            """
        let! program = Parser.run Parser.parseProgram text
        let! _, _, program' = TypeInference.inferProgram program
        let text' = program' |> Program.unparse
        let! program'' = Parser.run Parser.parseProgram text'
        return! Interpreter.evalProgram program''
    } |> printfn "%A"
