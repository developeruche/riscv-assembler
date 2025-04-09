//! Defines the custom error types used throughout the assembler.

use thiserror::Error;

/// The main error type for the RISC-V assembler.
///
/// Encapsulates all possible errors that can occur during the assembly process,
/// including lexical, syntax, semantic, and encoding errors.
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum AssemblerError {
    #[error("Lexer error: {message} at line {line}, column {column}")]
    LexerError {
        message: String,
        line: usize,
        column: usize,
    },

    #[error("Parser error: Unexpected token '{found_token}' at line {line}, column {column}. {message}")]
    UnexpectedToken {
        found_token: String, // Use String to avoid lifetime issues with Token
        message: String,
        line: usize,
        column: usize,
    },

    #[error("Parser error: {message} at line {line}, column {column}")]
    SyntaxError {
        message: String,
        line: usize,
        column: usize,
    },

    #[error("Symbol error: Undefined symbol '{name}' referenced at line {line}")]
    SymbolNotFound {
        name: String,
        line: usize,
        // Optional: Add column if easily trackable during resolution
        // column: usize,
    },

    #[error("Symbol error: Symbol '{name}' redefined at line {line} (originally defined at line {original_line})")]
    SymbolRedefinition {
        name: String,
        line: usize,
        original_line: usize,
    },

    #[error("Encoding error: {message} for instruction at line {line}")]
    EncodingError { message: String, line: usize },

    #[error("Encoding error: Immediate value '{value}' out of range for instruction at line {line}. Range: [{min}, {max}]")]
    ImmediateOutOfRange {
        value: String, // Keep as string for flexibility (e.g., hex/dec display)
        min: i64,      // Use i64 to accommodate full range
        max: i64,
        line: usize,
    },

    #[error("Encoding error: Branch target offset {offset} out of range at line {line}")]
    BranchOffsetOutOfRange { offset: i64, line: usize },

     #[error("Encoding error: Jump target offset {offset} out of range at line {line}")]
    JumpOffsetOutOfRange { offset: i64, line: usize },

    #[error("Invalid argument '{value}' for directive '{directive}' at line {line}, column {column}: {message}")]
    InvalidDirectiveArg {
        directive: String,
        value: String,
        message: String,
        line: usize,
        column: usize,
    },
     #[error("Value error: '{value}' is invalid at line {line}, column {column}. {message}")]
    ValueError {
        value: String,
        message: String,
        line: usize,
        column: usize,
    },

    // Add more specific errors as needed, e.g., for specific instruction constraints
    #[error("Invalid register name '{name}' at line {line}, column {column}")]
    InvalidRegister {
        name: String,
        line: usize,
        column: usize,
    },

    #[error("Unsupported feature: {feature} at line {line}")]
    UnsupportedFeature {
        feature: String,
        line: usize,
        // column: usize, // Optional
    },

    #[error("I/O error: {0}")]
    IoError(String), // To wrap std::io::Error if doing file I/O directly
}

// Optional: Implement conversion from std::io::Error if needed
impl From<std::io::Error> for AssemblerError {
    fn from(err: std::io::Error) -> Self {
        AssemblerError::IoError(err.to_string())
    }
}