namespace Znutar

/// Standard compiler error type.
type CompilerError =
    | InternalError of message : string
    | InvalidProgramType of Type
    | InvalidSyntax of message : string
    | UnboundIdentifier of Identifier
    | UnificationFailure of Type * Type

/// Standard compiler result type.
type CompilerResult<'t> = Result<'t, CompilerError>
