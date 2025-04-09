//! Tokenizes RISC-V assembly source code.
use crate::error::{AssemblerError, SourceLocation, err_lex};

/// Represents a token recognized by the lexer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String, // The original text slice
    pub loc: SourceLocation,
}

/// Enum defining the different kinds of tokens.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // Keywords/Instructions (converted to specific Instruction enum later in parser)
    Instruction, // e.g., "add", "lw", "jal" - parser determines specific one
    Directive,   // e.g., ".word", ".text", ".globl"
    Identifier,  // Labels or other identifiers
    Register,    // e.g., "x0", "zero", "ra", "sp"
    Integer,     // e.g., 10, -5, 0xff
    Comma,       // ,
    Colon,       // : (often after labels)
    LParen,      // (
    RParen,      // )
    Newline,     // \n or \r\n
    Comment,     // Text after '#'
    EndOfFile,
}

/// Lexes the input string into a vector of Tokens.
///
/// Strips comments and whitespace (except newline).
/// Handles basic number formats (decimal, hex).
pub fn tokenize(source: &str) -> Result<Vec<Token>, AssemblerError> {
    let mut tokens = Vec::new();
    let mut line_num = 1;
    let mut col_num = 1;
    let mut chars = source.chars().peekable();

    while let Some(char) = chars.next() {
        let start_col = col_num;
        let loc = SourceLocation {
            line: line_num,
            col: start_col,
        };

        match char {
            // --- Whitespace and Comments ---
            ' ' | '\t' | '\r' => {
                // Ignore whitespace other than newline
                col_num += 1;
                continue;
            }
            '\n' => {
                tokens.push(Token {
                    kind: TokenKind::Newline,
                    text: "\n".to_string(),
                    loc,
                });
                line_num += 1;
                col_num = 1; // Reset column on new line
                continue; // Explicitly continue
            }
            '#' => {
                // Comment
                let mut text = String::new();
                text.push(char);
                while let Some(&c) = chars.peek() {
                    if c == '\n' {
                        break;
                    }
                    text.push(chars.next().unwrap()); // Consume char
                    col_num += 1;
                }
                // Optionally add Comment token, or just ignore
                // tokens.push(Token { kind: TokenKind::Comment, text, loc });
                col_num += 1; // Account for the '#'
                continue;
            }

            // --- Punctuation ---
            ',' => tokens.push(Token {
                kind: TokenKind::Comma,
                text: char.to_string(),
                loc,
            }),
            ':' => tokens.push(Token {
                kind: TokenKind::Colon,
                text: char.to_string(),
                loc,
            }),
            '(' => tokens.push(Token {
                kind: TokenKind::LParen,
                text: char.to_string(),
                loc,
            }),
            ')' => tokens.push(Token {
                kind: TokenKind::RParen,
                text: char.to_string(),
                loc,
            }),

            // --- Numbers (Decimal and Hex) ---
            '-' | '0'..='9' => {
                let mut text = String::new();
                text.push(char);
                let is_neg = char == '-';
                if is_neg {
                    // Ensure next char is a digit if it's just a minus sign
                    if !chars.peek().map_or(false, |c| c.is_ascii_digit()) {
                        return Err(err_lex("Expected digit after '-'", line_num, col_num + 1));
                    }
                    text.push(chars.next().unwrap()); // Consume the digit
                    col_num += 1;
                } else if char == '0' && chars.peek() == Some(&'x') {
                    // Hex prefix
                    text.push(chars.next().unwrap()); // Consume 'x'
                    col_num += 1;
                    while let Some(&c) = chars.peek() {
                        if c.is_ascii_hexdigit() {
                            text.push(chars.next().unwrap());
                            col_num += 1;
                        } else {
                            break;
                        }
                    }
                } else {
                    // Decimal
                    while let Some(&c) = chars.peek() {
                        if c.is_ascii_digit() {
                            text.push(chars.next().unwrap());
                            col_num += 1;
                        } else {
                            break;
                        }
                    }
                }
                tokens.push(Token {
                    kind: TokenKind::Integer,
                    text: text.clone(),
                    loc,
                });
                col_num += text.len() - 1; // Update col_num correctly after consuming multiple chars
                continue; // Already advanced col_num within the loop
            }

            // --- Identifiers, Directives, Instructions, Registers ---
            '.' => {
                // Potential directive
                let mut text = String::new();
                text.push(char);
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphanumeric() || c == '_' {
                        text.push(chars.next().unwrap());
                        col_num += 1;
                    } else {
                        break;
                    }
                }
                tokens.push(Token {
                    kind: TokenKind::Directive,
                    text: text.clone(),
                    loc,
                });
                col_num += text.len() - 1;
                continue;
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                // Identifier (label, instruction, register)
                let mut text = String::new();
                text.push(char);
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphanumeric() || c == '_' {
                        text.push(chars.next().unwrap());
                        col_num += 1;
                    } else {
                        break;
                    }
                }
                // Further classify in parser: Instruction, Register, or Identifier (Label)
                let kind = classify_identifier(&text);
                tokens.push(Token {
                    kind,
                    text: text.clone(),
                    loc,
                });
                col_num += text.len() - 1; // Adjust col_num
                continue; // Already advanced col_num
            }

            _ => {
                return Err(err_lex(
                    format!("Unexpected character: '{}'", char),
                    line_num,
                    start_col,
                ));
            }
        }
        // Increment column number *after* processing the character/token
        col_num += 1;
    }

    tokens.push(Token {
        kind: TokenKind::EndOfFile,
        text: "".to_string(),
        loc: SourceLocation {
            line: line_num,
            col: col_num,
        },
    });
    Ok(tokens)
}

/// Helper to make an initial guess for identifier kinds. Parser makes the final decision.
fn classify_identifier(s: &str) -> TokenKind {
    // Basic check - improve with actual ISA instruction/register names
    // Ideally, load known instructions/registers from isa module.
    match s {
        // Example registers (add more from ABI names)
        "zero" | "ra" | "sp" | "gp" | "tp" | "fp" | "s0" | "s1" | "s2" | "s3" | "s4" | "s5" |
        "s6" | "s7" | "s8" | "s9" | "s10" | "s11" | "a0" | "a1" | "a2" | "a3" | "a4" | "a5" |
        "a6" | "a7" | "t0" | "t1" | "t2" | "t3" | "t4" | "t5" | "t6" |
        "x0" | "x1" | "x2" | "x3" | "x4" | "x5" | "x6" | "x7" | "x8" | "x9" | "x10" |
        "x11" | "x12" | "x13" | "x14" | "x15" | "x16" | "x17" | "x18" | "x19" | "x20" |
        "x21" | "x22" | "x23" | "x24" | "x25" | "x26" | "x27" | "x28" | "x29" | "x30" | "x31" => TokenKind::Register,

        // Example instructions (add *all* RV32IM instructions)
        "add" | "sub" | "sll" | "slt" | "sltu" | "xor" | "srl" | "sra" | "or" | "and" | // R-type
        "addi" | "slti" | "sltiu" | "xori" | "ori" | "andi" | "slli" | "srli" | "srai" | // I-type (ALU)
        "lb" | "lh" | "lw" | "lbu" | "lhu" | // I-type (Load)
        "jalr" | // I-type (Jump)
        "sb" | "sh" | "sw" | // S-type
        "beq" | "bne" | "blt" | "bge" | "bltu" | "bgeu" | // B-type
        "lui" | "auipc" | // U-type
        "jal" | // J-type
        "mul" | "mulh" | "mulhsu" | "mulhu" | "div" | "divu" | "rem" | "remu" // M Extension
         => TokenKind::Instruction,

        // Default to identifier (likely a label)
        _ => TokenKind::Identifier,
    }
}

// --- Unit Tests ---
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_instruction() {
        let code = "loop: addi x1, x0, 5 # comment\n";
        let tokens = tokenize(code).unwrap();
        assert_eq!(tokens.len(), 10); // loop, :, Identifier(addi), Register(x1), Comma, Register(x0), Comma, Integer(5), Newline, EOF
        assert_eq!(tokens[0].kind, TokenKind::Identifier); // "loop" -> classified as Identifier initially
        assert_eq!(tokens[0].text, "loop");
        assert_eq!(tokens[1].kind, TokenKind::Colon);
        assert_eq!(tokens[2].kind, TokenKind::Instruction);
        assert_eq!(tokens[2].text, "addi");
        assert_eq!(tokens[3].kind, TokenKind::Register);
        assert_eq!(tokens[3].text, "x1");
        assert_eq!(tokens[4].kind, TokenKind::Comma);
        assert_eq!(tokens[5].kind, TokenKind::Register);
        assert_eq!(tokens[5].text, "x0");
        assert_eq!(tokens[6].kind, TokenKind::Comma);
        assert_eq!(tokens[7].kind, TokenKind::Integer);
        assert_eq!(tokens[7].text, "5");
        assert_eq!(tokens[8].kind, TokenKind::Newline);
        assert_eq!(tokens[9].kind, TokenKind::EndOfFile); // Added EOF token
    }

    #[test]
    fn test_hex_number() {
        let code = "li a0, 0xFF"; // li might be pseudo-instruction, handle in parser
        let tokens = tokenize(code).unwrap();
        assert!(
            tokens
                .iter()
                .any(|t| t.kind == TokenKind::Integer && t.text == "0xFF")
        );
    }

    #[test]
    fn test_directive() {
        let code = ".data\nmy_var: .word 123";
        let tokens = tokenize(code).unwrap();
        assert!(
            tokens
                .iter()
                .any(|t| t.kind == TokenKind::Directive && t.text == ".data")
        );
        assert!(
            tokens
                .iter()
                .any(|t| t.kind == TokenKind::Directive && t.text == ".word")
        );
    }

    #[test]
    fn test_comment_stripping() {
        let code = " add x1, x2, x3 # This should be ignored";
        let tokens = tokenize(code).unwrap();
        assert!(!tokens.iter().any(|t| t.kind == TokenKind::Comment));
        assert!(
            tokens
                .iter()
                .any(|t| t.kind == TokenKind::Instruction && t.text == "add")
        ); // Check instruction is present
        assert_eq!(tokens.last().unwrap().kind, TokenKind::EndOfFile); // Should end with EOF, not newline if no newline after comment
    }

    #[test]
    fn test_location() {
        let code = "line1\n line2: lw x1, 0(sp)";
        let tokens = tokenize(code).unwrap();
        let lw_token = tokens.iter().find(|t| t.text == "lw").unwrap();
        assert_eq!(lw_token.loc.line, 2);
        assert!(lw_token.loc.col > 1); // Should not be column 1
        let sp_token = tokens.iter().find(|t| t.text == "sp").unwrap();
        assert_eq!(sp_token.loc.line, 2);
    }
}
