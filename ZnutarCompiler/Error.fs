namespace Znutar

/// Standard compiler error type.
type CompilerError =
    | AmbiguousMethodOverload of MemberAccess
    | InternalError of message : string
    | InvalidSyntax of message : string
    | UnboundIdentifier of Identifier
    | UnificationFailure of Type * Type

/// Standard compiler result type.
type CompilerResult<'t> = Result<'t, CompilerError>
