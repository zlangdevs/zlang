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

pub const SyntaxError = error{
    InvalidToken,
    UnexpectedCharacter,
    MissingBrace,
    InvalidExpression,
};

pub const GenericError = error{
    Unknown,
    NotImplemented,
};