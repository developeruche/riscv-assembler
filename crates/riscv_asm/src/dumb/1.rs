//! # RISVIM32 Assembler Library
//!
//! This crate provides the core components for assembling RISC-V RV32IM assembly
//! language source code into machine code.
//!
//! It includes modules for:
//! - Lexical analysis (`lexer`)
//! - Syntactic analysis (`parser`)
//! - Instruction Set Architecture definitions (`isa`)
//! - Symbol table management (`symbol`)
//! - Error handling (`error`)
//!
//! ## Example Usage (Conceptual)
//!
//! ```rust,ignore
//! use risvim32_assembler::{assemble, AssemblerError};
//!
//! const SOURCE_CODE: &str = r#"
//! .global _start
//!
//! _start:
//!     addi sp, sp, -16 # Allocate stack space
//!     li t0, 10
//! loop:
//!     addi t0, t0, -1
//!     bnez t0, loop
//!     addi a0, zero, 0 # Exit code 0
//!     # ecall # System call to exit (requires environment support)
//! "#;
//!
//! fn main() -> Result<(), AssemblerError> {
//!     let machine_code = assemble(SOURCE_CODE)?;
//!     println!("Assembled Machine Code ({} bytes):", machine_code.len());
//!     // Further processing: write to file, load into VM, etc.
//!     Ok(())
//! }
//! ```

// Declare modules
pub mod error;
pub mod isa;
pub mod lexer;
pub mod parser;
pub mod symbol;

// Re-export key types for easier use
pub use error::AssemblerError;
pub use lexer::{Lexer, Token, TokenKind};
pub use parser::{Directive, Instruction, ParsedLine, Parser};
pub use symbol::SymbolTable;

use std::collections::HashMap;

/// Represents the result of the first pass of assembly.
struct FirstPassResult {
    parsed_lines: Vec<ParsedLine>,
    symbol_table: SymbolTable,
    // Potentially other metadata like total size estimation
}

/// Represents the final assembled machine code.
pub struct AssemblyResult {
    pub machine_code: Vec<u8>,
    pub symbol_table: SymbolTable, // Return symbol table for debugging/linking
                                  // Add entry point address if applicable
                                  // pub entry_point: Option<u32>,
}

/// Assembles RV32IM assembly source code into machine code bytes.
///
/// This function performs a two-pass assembly process:
/// 1. **First Pass:** Lexes the source, parses instructions and directives,
///    and builds the symbol table (calculating label addresses).
/// 2. **Second Pass:** Resolves symbol references (e.g., labels in branch/jump
///    targets) and encodes instructions into their binary representation.
///
/// # Arguments
///
/// * `source`: A string slice containing the RV32IM assembly source code.
///
/// # Returns
///
/// * `Ok(AssemblyResult)` containing the generated machine code and symbol table.
/// * `Err(AssemblerError)` if any error occurs during lexing, parsing, or encoding.
pub fn assemble(source: &str) -> Result<AssemblyResult, AssemblerError> {
    // --- Pass 1: Lexing, Parsing, Symbol Table Construction ---
    let first_pass_result = run_first_pass(source)?;

    // --- Pass 2: Symbol Resolution and Encoding ---
    let machine_code = run_second_pass(
        &first_pass_result.parsed_lines,
        &first_pass_result.symbol_table,
    )?;

    Ok(AssemblyResult {
        machine_code,
        symbol_table: first_pass_result.symbol_table,
    })
}

/// Performs the first pass of assembly: lexing, parsing, and symbol definition.
fn run_first_pass(source: &str) -> Result<FirstPassResult, AssemblerError> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    let mut symbol_table = SymbolTable::new();
    let mut parsed_lines = Vec::new();
    let mut current_address: u32 = 0; // Assuming starting address is 0

    while let Some(line_result) = parser.next_line() {
        let line = line_result?; // Propagate parsing errors

        // --- Handle Labels ---
        if let Some(label) = &line.label {
            symbol_table.define(label.clone(), current_address, line.line_number)?;
        }

        // --- Handle Instructions and Directives ---
        match &line.kind {
            Some(parser::ParsedLineKind::Instruction(_)) => {
                // Most RV32IM instructions are 4 bytes
                // TODO: Handle compressed instructions (RV32C) if needed in the future
                parsed_lines.push(line);
                current_address += 4;
            }
            Some(parser::ParsedLineKind::Directive(directive)) => {
                // Handle directives that affect the address counter or define data
                match directive {
                    // Example: .word directive reserves 4 bytes
                    Directive::Word(_) => {
                        // TODO: Handle multiple values if directive allows
                        current_address += 4;
                    }
                    Directive::Byte(_) => {
                        // TODO: Handle multiple values
                        current_address += 1;
                    }
                    // Example: .align directive might change current_address
                    Directive::Align(alignment) => {
                        let align_val = *alignment as u32;
                        if align_val > 0 && align_val.is_power_of_two() {
                             let mask = align_val - 1;
                            if (current_address & mask) != 0 {
                                current_address = (current_address + align_val) & !mask;
                            }
                        } else {
                            // Return error for invalid alignment
                           return Err(AssemblerError::InvalidDirectiveArg {
                                directive: ".align".to_string(),
                                value: alignment.to_string(),
                                line: line.line_number,
                                column: line.column // Adjust if needed
                            });
                        }
                    }
                    // Directives like .global, .section usually don't advance the address
                    // counter directly in this pass but might store info.
                    Directive::Global(_) | Directive::Section(_) | Directive::Asciz(_) => {
                        // Handle Asciz address increment later if needed or during encoding.
                        // For now, just keep the parsed line.
                        // .asciz "str" -> adds len("str") + 1 bytes
                        if let Directive::Asciz(s) = directive {
                           current_address += s.len() as u32 + 1; // +1 for null terminator
                        }

                    }
                    // Add other directives like .space, .equ, etc.
                    // Directive::Equ(name, value) => {
                    //     symbol_table.define_constant(name, *value, line.line_number)?;
                    // }
                     _ => { /* Handle or ignore other directives */ }
                }
                 parsed_lines.push(line); // Store directives too for the second pass
            }
            None => {
                 // Line only contained a label or was empty/comment
                 parsed_lines.push(line);
            }
        }
    }

    // TODO: Potentially resolve `.equ` symbols or perform checks after first pass

    Ok(FirstPassResult {
        parsed_lines,
        symbol_table,
    })
}

/// Performs the second pass: resolves symbols and encodes instructions/data.
fn run_second_pass(
    parsed_lines: &[ParsedLine],
    symbol_table: &SymbolTable,
) -> Result<Vec<u8>, AssemblerError> {
    let mut machine_code = Vec::new();
    let mut current_address: u32 = 0; // Recalculate address based on first pass structure

    for line in parsed_lines {
         // Calculate current address *before* processing the line content, similar to pass 1
        let address_at_line_start = current_address;

        match &line.kind {
            Some(parser::ParsedLineKind::Instruction(instr)) => {
                let encoded_instr = isa::encode_instruction(instr, address_at_line_start, symbol_table)?;
                machine_code.extend_from_slice(&encoded_instr.to_le_bytes());
                current_address += 4; // Assuming 4-byte instructions
            }
            Some(parser::ParsedLineKind::Directive(directive)) => {
                match directive {
                    Directive::Word(expr) => {
                        // Resolve expression (might involve symbols)
                        let value = symbol_table.resolve_expression(expr, line.line_number)?;
                        machine_code.extend_from_slice(&value.to_le_bytes());
                        current_address += 4;
                    }
                     Directive::Byte(expr) => {
                        let value = symbol_table.resolve_expression(expr, line.line_number)?;
                        if value > 0xFF {
                             return Err(AssemblerError::ValueError{
                                 value: value.to_string(),
                                 message: "Value too large for .byte directive".to_string(),
                                 line: line.line_number,
                                 column: line.column, // Approximate
                             });
                        }
                        machine_code.push(value as u8);
                        current_address += 1;
                    }
                     Directive::Asciz(s) => {
                        machine_code.extend_from_slice(s.as_bytes());
                        machine_code.push(0); // Null terminator
                        current_address += s.len() as u32 + 1;
                    }
                     Directive::Align(alignment) => {
                        let align_val = *alignment as u32;
                         let mask = align_val - 1;
                        if (current_address & mask) != 0 {
                             let padding_needed = align_val - (current_address & mask);
                             for _ in 0..padding_needed {
                                machine_code.push(0); // Fill with NOPs (or zeros)
                             }
                            current_address += padding_needed;
                        }
                    }
                    // Other directives might not produce code directly (e.g., .global)
                    _ => { /* No code generation for this directive */ }
                }
            }
             None => {
                 // Only a label or comment/empty line, no code generated. Address doesn't advance here.
             }
        }
         // Post-condition check: Ensure address calculation matches between passes
        // This requires storing expected addresses from pass 1 if needed for validation.
    }

    Ok(machine_code)
}

// --- Unit Tests ---
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assemble_simple_add() {
        let source = "addi x1, x0, 42";
        let result = assemble(source);
        assert!(result.is_ok());
        let assembly_result = result.unwrap();
        // Expected encoding for addi x1, x0, 42 (imm=42=0x2a)
        // opcode=0x13, rd=1, funct3=0, rs1=0, imm=0x02a
        // 000000101010 00000 000 00001 0010011 -> 0x02A00093
        let expected_code: u32 = 0x02A00093;
        assert_eq!(assembly_result.machine_code.len(), 4);
        let actual_code = u32::from_le_bytes(assembly_result.machine_code.try_into().unwrap());
        assert_eq!(actual_code, expected_code);
    }

    #[test]
    fn test_assemble_label_branch() {
        let source = r#"
        _start:
            li t0, 5
        loop:
            addi t0, t0, -1
            bnez t0, loop
            j _start
        "#;
        // li t0, 5 expands to: addi t0, x0, 5 (0x00500293) at address 0
        // loop: (address 4)
        // addi t0, t0, -1 (0xfff28293) at address 4
        // bnez t0, loop => bne t0, x0, loop (offset = 4 - 8 = -4 = 0xffc)
        // imm[12|10:5] rs2 rs1 funct3 imm[4:1|11] opcode
        // imm = 1111 1111 1100 => imm[12]=1, imm[11]=1, imm[10:5]=111111, imm[4:1]=1100
        // 1 111111 00000 00101 001 1100 1 1100011
        // 11111110000000101001110011100011 -> 0xFE029CE3 at address 8
        // j _start => jal x0, _start (offset = 0 - 12 = -12 = 0xff4)
        // imm[20|10:1|11|19:12] rd opcode
        // imm = 1111 1111 0100 => imm[20]=1, imm[10:1]=1111111010, imm[11]=0, imm[19:12]=11111111
        // 1 1111111010 0 11111111 00000 1101111
        // 11111111010011111111000001101111 -> 0xFF4FF06F at address 12
        let result = assemble(source).expect("Assembly failed");
        let expected_bytes: Vec<u8> = [
            0x93, 0x02, 0x50, 0x00, // addi t0, x0, 5
            0x93, 0x82, 0xf2, 0xff, // addi t0, t0, -1
            0xe3, 0x9c, 0x02, 0xfe, // bnez t0, loop (target -4)
            0x6f, 0xf0, 0x4f, 0xff, // j _start (target -12)
        ]
        .into();

        assert_eq!(result.machine_code, expected_bytes);
        assert!(result.symbol_table.resolve("_start").is_ok());
        assert!(result.symbol_table.resolve("loop").is_ok());
        assert_eq!(result.symbol_table.resolve("_start").unwrap().address, 0);
        assert_eq!(result.symbol_table.resolve("loop").unwrap().address, 4);


    }

     #[test]
    fn test_data_directives() {
        let source = r#"
        data_section:
            .word 0xdeadbeef
            .byte 0x42
            .align 4 # Should add 3 padding bytes
            .asciz "Hi" # Adds 'H', 'i', '\0'
        next_label:
            nop
        "#;
         let result = assemble(source).expect("Assembly failed");
         let expected_bytes: Vec<u8> = vec![
             0xef, 0xbe, 0xad, 0xde, // .word 0xdeadbeef
             0x42,                 // .byte 0x42
             0x00, 0x00, 0x00,     // padding from .align 4
             b'H', b'i', 0x00,     // .asciz "Hi"
             0x13, 0x00, 0x00, 0x00, // nop (addi x0, x0, 0)
         ];
         assert_eq!(result.machine_code, expected_bytes);
         assert_eq!(result.symbol_table.resolve("data_section").unwrap().address, 0);
         assert_eq!(result.symbol_table.resolve("next_label").unwrap().address, 11); // 4+1+3+3 = 11
     }


    #[test]
    fn test_undefined_label() {
        let source = "j undefined_label";
        let result = assemble(source);
        assert!(result.is_err());
        match result.err().unwrap() {
            AssemblerError::SymbolNotFound { name, .. } => assert_eq!(name, "undefined_label"),
            e => panic!("Expected SymbolNotFound error, got {:?}", e),
        }
    }

     #[test]
    fn test_duplicate_label() {
        let source = r#"
        mylabel:
            nop
        mylabel:
            nop
        "#;
        let result = assemble(source);
        assert!(result.is_err());
        match result.err().unwrap() {
            AssemblerError::SymbolRedefinition { name, .. } => assert_eq!(name, "mylabel"),
            e => panic!("Expected SymbolRedefinition error, got {:?}", e),
        }
    }
}