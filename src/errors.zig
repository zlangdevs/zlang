pub const ReadFileError = error{
    FileNotFound,
    AccessDenied,
    OutOfMemory,
    InvalidPath,
    IOError,
};

pub const WriteFileError = error{
    AccessDenied,
    OutOfMemory,
    DiskFull,
    IOError,
};

pub const TokenizeError = error{
    LexerInitFailed,
    FileOpenFailed,
    OutOfMemory,
};

pub const SyntaxError = error{
    InvalidToken,
    UnexpectedCharacter,
    MissingBrace,
    InvalidExpression,
};

pub const ParseError = error{
    ParseFailed,
    LexerInitFailed,
    FileOpenFailed,
    OutOfMemory,
};

pub const GenericError = error{
    Unknown,
    NotImplemented,
};
