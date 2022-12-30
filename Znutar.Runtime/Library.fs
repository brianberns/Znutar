namespace Znutar.Runtime

type Unit private () =
    static member val Value = Unit()
    override _.ToString() = "()"
