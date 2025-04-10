//! Defines the error types used throughout the assembler.
use std::fmt;

/// Represents the location (line and column) of an error in the source code.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SourceLocation {
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {}, column {}", self.line, self.col)
    }
}

/// The main error type for the assembler.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssemblerError {
    /// Error during the lexing (tokenization) phase.
    LexerError {
        message: String,
        loc: SourceLocation,
    },
    /// Error during the parsing phase.
    ParserError {
        message: String,
        loc: SourceLocation,
    },
    /// Error related to symbols (labels).
    SymbolError {
        message: String,
        loc: SourceLocation,
    }, // Loc might be where the symbol was used
    /// Error during instruction encoding.
    EncodingError {
        message: String,
        loc: SourceLocation,
    }, // Loc might be the instruction location
    /// General I/O error (e.g., reading source file).
    IoError(String), // Wrap std::io::Error if needed
    /// Represents multiple errors found during assembly.
    MultipleErrors(Vec<AssemblerError>),
    /// StdParseError
    StdParseError(String),
}

impl fmt::Display for AssemblerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AssemblerError::LexerError { message, loc } => {
                write!(f, "Lexer Error at {}: {}", loc, message)
            }
            AssemblerError::ParserError { message, loc } => {
                write!(f, "Parser Error at {}: {}", loc, message)
            }
            AssemblerError::SymbolError { message, loc } => {
                write!(f, "Symbol Error near {}: {}", loc, message)
            }
            AssemblerError::EncodingError { message, loc } => {
                write!(f, "Encoding Error near {}: {}", loc, message)
            }
            AssemblerError::IoError(msg) => write!(f, "I/O Error: {}", msg),
            AssemblerError::StdParseError(msg) => write!(f, "Parse Error: {}", msg),
            AssemblerError::MultipleErrors(errs) => {
                writeln!(f, "Multiple errors found:")?;
                for err in errs {
                    writeln!(f, "- {}", err)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for AssemblerError {}

// Helper to create errors with location
pub fn err_lex(message: impl Into<String>, line: usize, col: usize) -> AssemblerError {
    AssemblerError::LexerError {
        message: message.into(),
        loc: SourceLocation { line, col },
    }
}

pub fn err_parse(message: impl Into<String>, line: usize, col: usize) -> AssemblerError {
    AssemblerError::ParserError {
        message: message.into(),
        loc: SourceLocation { line, col },
    }
}

pub fn err_symbol(message: impl Into<String>, line: usize, col: usize) -> AssemblerError {
    AssemblerError::SymbolError {
        message: message.into(),
        loc: SourceLocation { line, col },
    }
}

pub fn err_encoding(message: impl Into<String>, line: usize, col: usize) -> AssemblerError {
    AssemblerError::EncodingError {
        message: message.into(),
        loc: SourceLocation { line, col },
    }
}

pub fn err_io(message: impl Into<String>) -> AssemblerError {
    AssemblerError::IoError(message.into())
}

pub fn err_multiple(errors: Vec<AssemblerError>) -> AssemblerError {
    AssemblerError::MultipleErrors(errors)
}
