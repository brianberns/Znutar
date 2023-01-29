namespace Znutar

/// Standard compiler error type.
type CompilerError =
    | InternalError of message : string
    | InvalidSyntax of message : string
    | UnboundIdentifier of Identifier
    | UnificationFailure of Type * Type
    | UnresolvedMethodOverload of MemberAccess

/// Standard compiler result type.
type CompilerResult<'t> = Result<'t, CompilerError>
