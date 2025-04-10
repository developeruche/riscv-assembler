//! Parser for RISC-V assembly language.
//!
//! This module handles converting tokens from the lexer into structured IR
//! (Intermediate Representation) that can be processed by later stages.

use crate::error::{AssemblerError, err_parse};
use crate::isa::Register;
use crate::lexer::{Token, TokenKind};
use crate::symbol::SymbolTable;
use std::iter::Peekable;
use std::slice::Iter;

/// Represents an operand in a RISC-V instruction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    /// A register operand (e.g., x5, sp). Store the register number.
    Register(u8),
    /// An immediate integer value. Use i64 to accommodate potential wider ranges before final encoding.
    Immediate(i64),
    /// A symbol reference (label). Will be resolved to an address/value later.
    Symbol(String),
    /// Memory address operand (e.g., offset(base_register)).
    Memory { offset: i64, base: u8 }, // Base is register number
}

/// Represents a parsed instruction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedInstruction {
    pub mnemonic: String,
    pub operands: Vec<Operand>,
    /// The address assigned to this instruction during the first pass.
    pub address: u32,
    /// Original line number for error reporting.
    pub line_number: usize,
    /// Original column number for error reporting.
    pub column: usize,
}

/// Represents a parsed assembler directive.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    /// Define a byte (.byte)
    Byte(i64),
    /// Define a half-word (.half, .short)
    Half(i64),
    /// Define a word (.word)
    Word(i64),
    /// Define a string with null terminator (.string, .asciz)
    Asciz(String),
    /// Align to a power of 2 boundary (.align)
    Align(i64),
    /// Define a symbol's scope (.global, .globl)
    Global(String),
    /// Define a section (.text, .data, .rodata, .bss)
    Section(String),
    /// Skip bytes, filling with zeros (.space, .skip)
    Space(i64),
    /// Set location counter (.org)
    Org(i64),
    /// Set constant symbol value (.equ, .set)
    Equ(String, i64),
    /// Set constant symbol to PC (.equ, .set)
    EquPC(String),
    /// Define a zero-initialized array (.zero)
    Zero(i64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedDirective {
    pub directive: Directive,
    /// The address where this directive takes effect.
    pub address: u32,
    /// Original line number for error reporting.
    pub line_number: usize,
    /// Original column number for error reporting.
    pub column: usize,
}

/// Represents a parsed label definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedLabel {
    pub name: String,
    /// The address this label points to.
    pub address: u32,
    /// Original line number for error reporting.
    pub line_number: usize,
    /// Original column number for error reporting.
    pub column: usize,
}

/// Represents a single logical item parsed from a line of assembly.
/// This forms the Intermediate Representation (IR) passed to the next stage.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedItem {
    Instruction(ParsedInstruction),
    Directive(ParsedDirective),
    Label(ParsedLabel),
    Empty(usize), // Empty line or comment-only line with line number
}

/// Parser for RISC-V assembly language.
pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
    current_address: u32,
    current_section: String,
    /// Current line being parsed
    line: usize,
    /// Current column being parsed
    column: usize,
}

impl<'a> Parser<'a> {
    /// Creates a new parser from a series of tokens.
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens: tokens.iter().peekable(),
            current_address: 0,
            current_section: ".text".to_string(), // Default section
            line: 1,
            column: 1,
        }
    }

    /// Parses the entire token stream into a vector of `ParsedItem`s.
    pub fn parse_all(
        &mut self,
        symbol_table: &mut SymbolTable,
    ) -> Result<Vec<ParsedItem>, AssemblerError> {
        let mut parsed_items = Vec::new();
        let mut errors = Vec::new();

        // First pass: parse all items and build symbol table
        loop {
            // println!("Parsering line: {:?}", errors);
            match self.parse_line(symbol_table) {
                Ok(Some(item)) => {
                    // Track label addresses
                    if let ParsedItem::Label(ref label) = item {
                        if let Err(e) = symbol_table.define(
                            label.name.clone(),
                            label.address,
                            Some(label.line_number),
                        ) {
                            errors.push(e);
                        }
                    }
                    parsed_items.push(item);
                }
                Ok(None) => break, // End of input
                Err(e) => errors.push(e),
            }
        }

        // If we encountered any errors, return a MultipleErrors
        if !errors.is_empty() {
            return Err(AssemblerError::MultipleErrors(errors));
        }

        Ok(parsed_items)
    }

    /// Parses a single line of assembly.
    pub fn parse_line(
        &mut self,
        _symbol_table: &mut SymbolTable,
    ) -> Result<Option<ParsedItem>, AssemblerError> {
        // Skip any leading newlines
        while let Some(token) = self.peek_token() {
            if token.kind == TokenKind::Newline {
                self.consume_token();
                self.line += 1;
                self.column = 1;
            } else {
                break;
            }
        }

        // Check if we've reached the end of input
        if self.peek_token().is_none() || self.peek_token().unwrap().kind == TokenKind::EndOfFile {
            return Ok(None);
        }

        // Parse a label if present
        let label = if self.peek_token_is(TokenKind::Identifier)
            && self.peek_second_token_is(TokenKind::Colon)
        {
            // Capture values before advancing tokens
            let token = self.peek_token().unwrap();
            let name = token.text.clone();
            let line_number = token.loc.line;
            let column = token.loc.col;
            let address = self.current_address;

            // Now consume tokens
            self.consume_token(); // Consume identifier
            self.consume_token(); // Consume the colon

            Some(ParsedLabel {
                name,
                address,
                line_number,
                column,
            })
        } else {
            None
        };

        // Check what comes after the label (if anything)
        if self.peek_token_is(TokenKind::Newline) || self.peek_token_is(TokenKind::EndOfFile) {
            // Just a label on a line by itself
            if let Some(label) = label {
                return Ok(Some(ParsedItem::Label(label)));
            } else {
                return Ok(Some(ParsedItem::Empty(self.line)));
            }
        } else if self.peek_token_is(TokenKind::Directive) {
            // Parse a directive
            let directive = self.parse_directive()?;

            // Update current address based on directive
            self.update_address_for_directive(&directive.directive);

            if let Some(_) = label {
                // This doesn't work directly, need a way to return both
                // For now, just return the directive and rely on the label being in the symbol table
                return Ok(Some(ParsedItem::Directive(directive)));
            } else {
                return Ok(Some(ParsedItem::Directive(directive)));
            }
        } else if self.peek_token_is(TokenKind::Instruction) {
            // Parse an instruction
            let instruction = self.parse_instruction()?;

            // Update current address (most RISC-V instructions are 4 bytes)
            self.current_address += 4;

            if let Some(_) = label {
                // See comment above, same issue
                return Ok(Some(ParsedItem::Instruction(instruction)));
            } else {
                return Ok(Some(ParsedItem::Instruction(instruction)));
            }
        } else {
            let token = self.peek_token().unwrap();
            return Err(err_parse(
                format!(
                    "Unexpected token '{}', expected instruction or directive",
                    token.text
                ),
                token.loc.line,
                token.loc.col,
            ));
        }
    }

    /// Parses a directive (like .word, .text, etc.)
    fn parse_directive(&mut self) -> Result<ParsedDirective, AssemblerError> {
        let directive_token = self.consume_token().unwrap();
        let directive_name = directive_token.text.to_lowercase();

        let line = directive_token.loc.line;
        let column = directive_token.loc.col;

        let directive = match directive_name.as_str() {
            ".byte" => {
                let value = self.parse_expression()?;
                Directive::Byte(value)
            }
            ".half" | ".short" => {
                let value = self.parse_expression()?;
                Directive::Half(value)
            }
            ".word" => {
                let value = self.parse_expression()?;
                Directive::Word(value)
            }
            ".string" | ".asciz" | ".ascii" => {
                let string = self.parse_string()?;
                Directive::Asciz(string)
            }
            ".align" => {
                let value = self.parse_expression()?;
                Directive::Align(value)
            }
            ".global" | ".globl" => {
                let symbol = self.parse_identifier()?;
                Directive::Global(symbol)
            }
            ".text" | ".data" | ".rodata" | ".bss" => {
                self.current_section = directive_name.clone();
                Directive::Section(directive_name)
            }
            ".space" | ".skip" => {
                let value = self.parse_expression()?;
                Directive::Space(value)
            }
            ".org" => {
                let value = self.parse_expression()?;
                Directive::Org(value)
            }
            ".equ" | ".set" => {
                let symbol = self.parse_identifier()?;
                self.expect_token(TokenKind::Comma)?;

                // Check if we're setting to the current PC or an expression
                if self.peek_token_is(TokenKind::Identifier)
                    && self.peek_token().unwrap().text == "."
                {
                    self.consume_token(); // Consume the dot
                    Directive::EquPC(symbol)
                } else {
                    let value = self.parse_expression()?;
                    Directive::Equ(symbol, value)
                }
            }
            ".zero" => {
                let value = self.parse_expression()?;
                Directive::Zero(value)
            }
            _ => {
                return Err(err_parse(
                    format!("Unsupported directive: {}", directive_name),
                    line,
                    column,
                ));
            }
        };

        // Consume any remaining tokens on this line
        self.consume_to_end_of_line();

        Ok(ParsedDirective {
            directive,
            address: self.current_address,
            line_number: line,
            column,
        })
    }

    /// Updates the current address based on the directive
    fn update_address_for_directive(&mut self, directive: &Directive) {
        match directive {
            Directive::Byte(_) => self.current_address += 1,
            Directive::Half(_) => self.current_address += 2,
            Directive::Word(_) => self.current_address += 4,
            Directive::Asciz(s) => self.current_address += (s.len() + 1) as u32, // +1 for null terminator
            Directive::Align(n) => {
                let alignment = 1 << *n;
                let mask = alignment - 1;
                if (self.current_address & (mask as u32)) != 0 {
                    self.current_address = (self.current_address + (mask as u32)) & !(mask as u32);
                }
            }
            Directive::Space(n) | Directive::Zero(n) => self.current_address += *n as u32,
            Directive::Org(addr) => self.current_address = *addr as u32,
            _ => {} // Other directives don't affect the address
        }
    }

    /// Parses an instruction (mnemonic + operands)
    fn parse_instruction(&mut self) -> Result<ParsedInstruction, AssemblerError> {
        let mnemonic_token = self.consume_token().unwrap();
        let mnemonic = mnemonic_token.text.to_lowercase();

        let line = mnemonic_token.loc.line;
        let column = mnemonic_token.loc.col;

        // Parse operands
        let mut operands = Vec::new();

        // If the next token is a newline or end of file, this instruction has no operands
        if !self.peek_token_is(TokenKind::Newline) && !self.peek_token_is(TokenKind::EndOfFile) {
            // Parse the first operand
            operands.push(self.parse_operand()?);

            // Parse any additional operands separated by commas
            while self.peek_token_is(TokenKind::Comma) {
                let _ = self.consume_token(); // Consume the comma
                operands.push(self.parse_operand()?);
            }
        }

        // Consume any remaining tokens on this line
        self.consume_to_end_of_line();

        Ok(ParsedInstruction {
            mnemonic,
            operands,
            address: self.current_address,
            line_number: line,
            column,
        })
    }

    /// Parses an operand (register, immediate, label, or memory reference)
    fn parse_operand(&mut self) -> Result<Operand, AssemblerError> {
        if self.peek_token_is(TokenKind::Register) {
            // Register operand
            let reg_token = self.consume_token().unwrap();
            let reg_text = reg_token.text.clone();
            let line = reg_token.loc.line;
            let col = reg_token.loc.col;

            match Register::from_name(&reg_text) {
                Some(reg) => Ok(Operand::Register(reg.number())),
                None => Err(err_parse(
                    format!("Invalid register name: {}", reg_text),
                    line,
                    col,
                )),
            }
        } else if self.peek_token_is(TokenKind::Identifier) {
            // Symbol operand
            let symbol_token = self.consume_token().unwrap();
            Ok(Operand::Symbol(symbol_token.text.clone()))
        } else if self.peek_token_is(TokenKind::Integer)
            && self.peek_second_token_is(TokenKind::LParen)
        {
            // Memory operand with offset: 8(sp)
            let offset_token = self.consume_token().unwrap();
            let offset_text = offset_token.text.clone();
            let line = offset_token.loc.line;
            let col = offset_token.loc.col;

            let offset = self
                .parse_integer(&offset_text)
                .map_err(|e| err_parse(format!("Invalid offset: {}", e), line, col))?;

            self.expect_token(TokenKind::LParen)?;

            // Expect a register
            if !self.peek_token_is(TokenKind::Register) {
                let token = self.peek_token().unwrap();
                return Err(err_parse(
                    format!("Expected register, found '{}'", token.text),
                    token.loc.line,
                    token.loc.col,
                ));
            }

            let reg_token = self.consume_token().unwrap();
            let reg_text = reg_token.text.clone();
            let line = reg_token.loc.line;
            let col = reg_token.loc.col;

            let base = match Register::from_name(&reg_text) {
                Some(reg) => reg.number(),
                None => {
                    return Err(err_parse(
                        format!("Invalid register name: {}", reg_text),
                        line,
                        col,
                    ));
                }
            };

            self.expect_token(TokenKind::RParen)?;

            Ok(Operand::Memory { offset, base })
        } else if self.peek_token_is(TokenKind::Integer) {
            // Immediate operand
            let imm_token = self.consume_token().unwrap();
            let imm_text = imm_token.text.clone();
            let line = imm_token.loc.line;
            let col = imm_token.loc.col;

            self.parse_integer(&imm_text)
                .map(Operand::Immediate)
                .map_err(|e| err_parse(format!("Invalid integer: {}", e), line, col))
        } else if self.peek_token_is(TokenKind::LParen) {
            // Memory operand without offset: (sp)
            self.consume_token(); // Consume (

            if !self.peek_token_is(TokenKind::Register) {
                let token = self.peek_token().unwrap();
                return Err(err_parse(
                    format!("Expected register, found '{}'", token.text),
                    token.loc.line,
                    token.loc.col,
                ));
            }

            let reg_token = self.consume_token().unwrap();
            let reg_text = reg_token.text.clone();
            let line = reg_token.loc.line;
            let col = reg_token.loc.col;

            let base = match Register::from_name(&reg_text) {
                Some(reg) => reg.number(),
                None => {
                    return Err(err_parse(
                        format!("Invalid register name: {}", reg_text),
                        line,
                        col,
                    ));
                }
            };

            self.expect_token(TokenKind::RParen)?;

            Ok(Operand::Memory { offset: 0, base })
        } else {
            let token = self.peek_token().unwrap();
            Err(err_parse(
                format!("Expected operand, found '{}'", token.text),
                token.loc.line,
                token.loc.col,
            ))
        }
    }

    /// Parse a numerical expression (currently just a single integer)
    fn parse_expression(&mut self) -> Result<i64, AssemblerError> {
        if self.peek_token_is(TokenKind::Integer) {
            let token = self.consume_token().unwrap().clone();
            self.parse_integer(&token.text)
        } else if self.peek_token_is(TokenKind::Identifier) {
            // This is a symbol - during parsing we can't resolve it
            // We'll return 0 for now and let the symbol resolution phase handle it
            let _token = self.consume_token().unwrap();
            Ok(0)
        } else {
            let token = self.peek_token().unwrap();
            Err(err_parse(
                format!("Expected expression, found '{}'", token.text),
                token.loc.line,
                token.loc.col,
            ))
        }
    }

    /// Parses a string literal (for .asciz, etc.)
    fn parse_string(&mut self) -> Result<String, AssemblerError> {
        if let Some(token) = self.peek_token() {
            if token.text.starts_with('"') && token.text.ends_with('"') {
                let token = self.consume_token().unwrap();
                // Remove the quotes
                let string = token.text[1..token.text.len() - 1].to_string();
                Ok(string)
            } else {
                Err(err_parse(
                    format!("Expected string literal, found '{}'", token.text),
                    token.loc.line,
                    token.loc.col,
                ))
            }
        } else {
            Err(err_parse(
                "Unexpected end of input, expected string literal".to_string(),
                self.line,
                self.column,
            ))
        }
    }

    /// Parses an identifier (for symbol names)
    fn parse_identifier(&mut self) -> Result<String, AssemblerError> {
        if self.peek_token_is(TokenKind::Identifier) {
            let token = self.consume_token().unwrap();
            Ok(token.text.clone())
        } else {
            let token = self.peek_token().unwrap();
            Err(err_parse(
                format!("Expected identifier, found '{}'", token.text),
                token.loc.line,
                token.loc.col,
            ))
        }
    }

    /// Parses an integer literal, handling different bases
    fn parse_integer(&self, text: &str) -> Result<i64, AssemblerError> {
        if text.starts_with("0x") || text.starts_with("0X") {
            // Hexadecimal
            i64::from_str_radix(&text[2..], 16)
                .map_err(|e| AssemblerError::StdParseError(e.to_string()))
        } else if text.starts_with("0b") || text.starts_with("0B") {
            // Binary
            i64::from_str_radix(&text[2..], 2)
                .map_err(|e| AssemblerError::StdParseError(e.to_string()))
        } else if text.starts_with('0') && text.len() > 1 {
            // Octal
            i64::from_str_radix(&text[1..], 8)
                .map_err(|e| AssemblerError::StdParseError(e.to_string()))
        } else {
            // Decimal
            text.parse::<i64>()
                .map_err(|e| AssemblerError::StdParseError(e.to_string()))
        }
    }

    // Helper methods for token handling

    fn peek_token(&mut self) -> Option<&Token> {
        self.tokens.peek().copied()
    }

    fn peek_token_is(&mut self, kind: TokenKind) -> bool {
        self.peek_token().map_or(false, |t| t.kind == kind)
    }

    fn peek_second_token_is(&mut self, kind: TokenKind) -> bool {
        let mut iter = self.tokens.clone();
        iter.next(); // Skip first token
        iter.next().map_or(false, |t| t.kind == kind)
    }

    fn consume_token(&mut self) -> Option<&Token> {
        let token = self.tokens.next();
        if let Some(t) = token {
            self.column = t.loc.col + t.text.len();
        }
        token
    }

    /// Expect a token of a specific kind, returning an error if it's not found.
    fn expect_token(&mut self, expected_kind: TokenKind) -> Result<&Token, AssemblerError> {
        if let Some(token) = self.peek_token() {
            if token.kind == expected_kind {
                Ok(self.consume_token().unwrap())
            } else {
                Err(err_parse(
                    format!("Expected {:?}, found '{}'", expected_kind, token.text),
                    token.loc.line,
                    token.loc.col,
                ))
            }
        } else {
            Err(err_parse(
                format!("Unexpected end of input, expected {:?}", expected_kind),
                self.line,
                self.column,
            ))
        }
    }

    /// Consumes tokens until the end of the current line is reached.
    fn consume_to_end_of_line(&mut self) {
        while let Some(token) = self.peek_token() {
            if token.kind == TokenKind::Newline || token.kind == TokenKind::EndOfFile {
                if token.kind == TokenKind::Newline {
                    self.consume_token();
                    self.line += 1;
                    self.column = 1;
                }
                break;
            }
            self.consume_token();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        error::SourceLocation,
        lexer::{Token, TokenKind, tokenize},
    };

    fn create_token(kind: TokenKind, text: &str, line: usize, col: usize) -> Token {
        Token {
            kind,
            text: text.to_string(),
            loc: SourceLocation { line, col },
        }
    }

    #[test]
    fn test_consume_to_end_of_line() {
        let tokens_1 = vec![
            create_token(TokenKind::Identifier, "label", 1, 1),
            create_token(TokenKind::Colon, ":", 1, 6),
            create_token(TokenKind::Newline, "\n", 1, 7),
            create_token(TokenKind::EndOfFile, "", 2, 1),
        ];

        let mut parser_1 = Parser::new(&tokens_1);

        parser_1.consume_to_end_of_line();
        assert_eq!(parser_1.tokens.len(), 1);

        let tokens_2 = vec![
            create_token(TokenKind::Identifier, "label", 1, 1),
            create_token(TokenKind::Colon, ":", 1, 6),
            create_token(TokenKind::Newline, "\n", 1, 7),
        ];

        let mut parser_2 = Parser::new(&tokens_2);

        parser_2.consume_to_end_of_line();
        assert_eq!(parser_2.tokens.len(), 0);
    }

    #[test]
    fn test_expect_token() {
        let tokens = vec![
            create_token(TokenKind::Identifier, "label", 1, 1),
            create_token(TokenKind::Colon, ":", 1, 6),
            create_token(TokenKind::Newline, "\n", 1, 7),
        ];

        let mut parser = Parser::new(&tokens);

        parser.expect_token(TokenKind::Identifier).unwrap();
        parser.expect_token(TokenKind::Colon).unwrap();
        parser.expect_token(TokenKind::Newline).unwrap();

        assert!(parser.expect_token(TokenKind::EndOfFile).is_err());
    }

    #[test]
    fn test_consume_token() {
        let tokens = vec![
            create_token(TokenKind::Identifier, "label", 1, 1),
            create_token(TokenKind::Colon, ":", 1, 6),
            create_token(TokenKind::Newline, "\n", 1, 7),
        ];

        let mut parser = Parser::new(&tokens);

        let t1 = parser.consume_token().unwrap().clone();
        let t2 = parser.consume_token().unwrap().clone();
        let t3 = parser.consume_token().unwrap().clone();

        assert_eq!(t1, create_token(TokenKind::Identifier, "label", 1, 1));
        assert_eq!(t2, create_token(TokenKind::Colon, ":", 1, 6));
        assert_eq!(t3, create_token(TokenKind::Newline, "\n", 1, 7));

        assert!(parser.expect_token(TokenKind::EndOfFile).is_err());
    }

    #[test]
    fn test_peek_second_token_is() {
        let tokens = vec![
            create_token(TokenKind::Identifier, "label", 1, 1),
            create_token(TokenKind::Colon, ":", 1, 6),
            create_token(TokenKind::Newline, "\n", 1, 7),
        ];

        let mut parser = Parser::new(&tokens);

        let t = parser.peek_second_token_is(TokenKind::Colon);

        assert!(t);
    }

    #[test]
    fn test_peek_token_is_and_peek_token() {
        let tokens = vec![
            create_token(TokenKind::Identifier, "label", 1, 1),
            create_token(TokenKind::Colon, ":", 1, 6),
            create_token(TokenKind::Newline, "\n", 1, 7),
        ];

        let mut parser = Parser::new(&tokens);

        let t = parser.peek_token_is(TokenKind::Identifier);
        assert!(t);

        let t = parser.peek_token();
        assert_eq!(t, Some(&create_token(TokenKind::Identifier, "label", 1, 1)));
    }

    #[test]
    fn test_parse_integer() {
        let tokens = vec![];

        let parser = Parser::new(&tokens);

        let _10 = parser.parse_integer("10");
        let _neg5 = parser.parse_integer("-5");
        let _0xff = parser.parse_integer("0xff");

        assert_eq!(_10.unwrap(), 10);
        assert_eq!(_neg5.unwrap(), -5);
        assert_eq!(_0xff.unwrap(), 255);
    }

    #[test]
    fn test_parse_identifier() {
        let tokens = vec![create_token(TokenKind::Identifier, "label", 1, 1)];

        let mut parser = Parser::new(&tokens);

        let identifier = parser.parse_identifier();
        assert_eq!(identifier.unwrap(), "label");
    }

    #[test]
    fn test_parse_string() {
        let tokens = vec![
            create_token(TokenKind::Directive, ".word", 1, 1),
            create_token(TokenKind::Identifier, "\"hello\"", 1, 6),
        ];

        let mut parser = Parser::new(&tokens);
        let _ = parser.consume_token();
        let string = parser.parse_string();
        assert_eq!(string.unwrap(), "hello");
    }

    #[test]
    fn test_parse_expression() {
        let tokens = vec![create_token(TokenKind::Integer, "2", 1, 6)];

        let mut parser = Parser::new(&tokens);
        let expression = parser.parse_expression().unwrap();

        assert_eq!(expression, 2);
    }

    #[test]
    fn test_parse_operand() {
        let tokens_1 = vec![create_token(TokenKind::Register, "x1", 1, 6)];
        let mut parser = Parser::new(&tokens_1);
        let operand_1 = parser.parse_operand().unwrap();

        assert_eq!(operand_1, Operand::Register(1));

        let tokens_2 = vec![create_token(TokenKind::Integer, "1", 1, 6)];
        let mut parser = Parser::new(&tokens_2);
        let operand_2 = parser.parse_operand().unwrap();

        assert_eq!(operand_2, Operand::Immediate(1));

        let tokens_3 = vec![create_token(TokenKind::Identifier, "sys", 1, 6)];
        let mut parser = Parser::new(&tokens_3);
        let operand_3 = parser.parse_operand().unwrap();

        assert_eq!(operand_3, Operand::Symbol("sys".to_string()));

        let tokens_4 = vec![
            create_token(TokenKind::Integer, "8", 1, 6),
            create_token(TokenKind::LParen, "(", 1, 6),
            create_token(TokenKind::Register, "x1", 1, 6),
            create_token(TokenKind::RParen, ")", 1, 6),
        ];
        let mut parser = Parser::new(&tokens_4);
        let operand_4 = parser.parse_operand().unwrap();

        assert_eq!(operand_4, Operand::Memory { offset: 8, base: 1 });

        let tokens_5 = vec![
            create_token(TokenKind::LParen, "(", 1, 6),
            create_token(TokenKind::Register, "x1", 1, 6),
            create_token(TokenKind::RParen, ")", 1, 6),
        ];
        let mut parser = Parser::new(&tokens_5);
        let operand_5 = parser.parse_operand().unwrap();

        assert_eq!(operand_5, Operand::Memory { offset: 0, base: 1 });
    }

    #[test]
    fn test_parse_directive() {
        let tokens = vec![
            create_token(TokenKind::Directive, ".word", 1, 1),
            create_token(TokenKind::Integer, "2", 1, 6),
        ];

        let mut parser = Parser::new(&tokens);
        let directive = parser.parse_directive();
        assert_eq!(
            directive.unwrap(),
            ParsedDirective {
                directive: Directive::Word(2),
                address: 0,
                line_number: 1,
                column: 1
            }
        );
    }

    #[test]
    fn test_parse_simple_instruction() {
        // this also serve as the parse line test
        let tokens = vec![
            create_token(TokenKind::Instruction, "add", 1, 1),
            create_token(TokenKind::Register, "x1", 1, 5),
            create_token(TokenKind::Comma, ",", 1, 7),
            create_token(TokenKind::Register, "x2", 1, 9),
            create_token(TokenKind::Comma, ",", 1, 11),
            create_token(TokenKind::Register, "x3", 1, 13),
            create_token(TokenKind::Newline, "\n", 1, 15),
            create_token(TokenKind::EndOfFile, "", 2, 1),
        ];

        let mut parser = Parser::new(&tokens);
        let mut symbol_table = SymbolTable::new();

        let parsed = parser.parse_line(&mut symbol_table).unwrap().unwrap();

        match parsed {
            ParsedItem::Instruction(instr) => {
                assert_eq!(instr.mnemonic, "add");
                assert_eq!(instr.operands.len(), 3);
                assert_eq!(instr.operands[0], Operand::Register(1));
                assert_eq!(instr.operands[1], Operand::Register(2));
                assert_eq!(instr.operands[2], Operand::Register(3));
            }
            _ => panic!("Expected Instruction, got {:?}", parsed),
        }
    }

    #[test]
    fn test_parse_label() {
        let tokens = vec![
            create_token(TokenKind::Identifier, "loop", 1, 1),
            create_token(TokenKind::Colon, ":", 1, 5),
            create_token(TokenKind::Newline, "\n", 1, 6),
            create_token(TokenKind::EndOfFile, "", 2, 1),
        ];

        let mut parser = Parser::new(&tokens);
        let mut symbol_table = SymbolTable::new();

        let parsed = parser.parse_line(&mut symbol_table).unwrap().unwrap();

        match parsed {
            ParsedItem::Label(label) => {
                assert_eq!(label.name, "loop");
                assert_eq!(label.address, 0);
            }
            _ => panic!("Expected Label, got {:?}", parsed),
        }
    }

    #[test]
    fn test_parse_memory_operand() {
        let tokens = vec![
            create_token(TokenKind::Instruction, "lw", 1, 1),
            create_token(TokenKind::Register, "x1", 1, 4),
            create_token(TokenKind::Comma, ",", 1, 6),
            create_token(TokenKind::Integer, "8", 1, 8),
            create_token(TokenKind::LParen, "(", 1, 9),
            create_token(TokenKind::Register, "sp", 1, 10),
            create_token(TokenKind::RParen, ")", 1, 12),
            create_token(TokenKind::Newline, "\n", 1, 13),
            create_token(TokenKind::EndOfFile, "", 2, 1),
        ];

        let mut parser = Parser::new(&tokens);
        let mut symbol_table = SymbolTable::new();

        let parsed = parser.parse_line(&mut symbol_table).unwrap().unwrap();

        match parsed {
            ParsedItem::Instruction(instr) => {
                assert_eq!(instr.mnemonic, "lw");
                assert_eq!(instr.operands.len(), 2);
                assert_eq!(instr.operands[0], Operand::Register(1));
                assert_eq!(instr.operands[1], Operand::Memory { offset: 8, base: 2 }); // sp is x2
            }
            _ => panic!("Expected Instruction, got {:?}", parsed),
        }
    }

    #[test]
    fn test_parse_multiple_lines() {
        let source = r#"
                    .text
                    main:
                        addi sp, sp, -20
                        sw ra, 12(sp)
            
                        jal ra, function
            
                        lw ra, 12(sp)
                        addi sp, sp, 16
            
                    function:
                        addi a0, zero, 42
                    "#;

        let tokens = tokenize(source).unwrap();
        let mut parser = Parser::new(&tokens);
        let mut symbol_table = SymbolTable::new();

        let parsed = parser.parse_all(&mut symbol_table).unwrap();

        // Verify we parsed 11 items (1 section, 2 labels, 6 instructions)
        assert_eq!(parsed.len(), 9);

        // Verify symbol table has 2 labels: main and function
        assert_eq!(symbol_table.len(), 2);
        assert!(symbol_table.is_defined("main"));
        assert!(symbol_table.is_defined("function"));
    }

    #[test]
    fn test_parse_errors() {
        // Test invalid register name
        let tokens = vec![
            create_token(TokenKind::Instruction, "add", 1, 1),
            create_token(TokenKind::Register, "x99", 1, 5), // Invalid register (x99)
            create_token(TokenKind::Comma, ",", 1, 8),
            create_token(TokenKind::Register, "x2", 1, 10),
            create_token(TokenKind::Comma, ",", 1, 12),
            create_token(TokenKind::Register, "x3", 1, 14),
            create_token(TokenKind::Newline, "\n", 1, 16),
            create_token(TokenKind::EndOfFile, "", 2, 1),
        ];

        let mut parser = Parser::new(&tokens);
        let mut symbol_table = SymbolTable::new();

        let result = parser.parse_line(&mut symbol_table);
        assert!(result.is_err(), "Expected error for invalid register");

        // Test missing operand
        let tokens = vec![
            create_token(TokenKind::Instruction, "add", 1, 1),
            create_token(TokenKind::Register, "x1", 1, 5),
            create_token(TokenKind::Comma, ",", 1, 7),
            create_token(TokenKind::Newline, "\n", 1, 8), // Missing operands
            create_token(TokenKind::EndOfFile, "", 2, 1),
        ];

        let mut parser = Parser::new(&tokens);
        let mut symbol_table = SymbolTable::new();

        let result = parser.parse_line(&mut symbol_table);
        assert!(result.is_err(), "Expected error for missing operand");

        // Test invalid directive
        let tokens = vec![
            create_token(TokenKind::Directive, ".invalid", 1, 1), // Invalid directive
            create_token(TokenKind::Integer, "42", 1, 10),
            create_token(TokenKind::Newline, "\n", 1, 12),
            create_token(TokenKind::EndOfFile, "", 2, 1),
        ];

        let mut parser = Parser::new(&tokens);
        let mut symbol_table = SymbolTable::new();

        let result = parser.parse_line(&mut symbol_table);
        assert!(result.is_err(), "Expected error for invalid directive");

        // Test invalid memory operand
        let tokens = vec![
            create_token(TokenKind::Instruction, "lw", 1, 1),
            create_token(TokenKind::Register, "x1", 1, 4),
            create_token(TokenKind::Comma, ",", 1, 6),
            create_token(TokenKind::Integer, "8", 1, 8),
            create_token(TokenKind::LParen, "(", 1, 9),
            create_token(TokenKind::Identifier, "notareg", 1, 10), // Not a register
            create_token(TokenKind::RParen, ")", 1, 17),
            create_token(TokenKind::Newline, "\n", 1, 18),
            create_token(TokenKind::EndOfFile, "", 2, 1),
        ];

        let mut parser = Parser::new(&tokens);
        let mut symbol_table = SymbolTable::new();

        let result = parser.parse_line(&mut symbol_table);
        assert!(result.is_err(), "Expected error for invalid memory operand");
    }

    #[test]
    fn test_integration_with_lexer() {
        // Test complete integration from source to parsed items
        let source = r#"
            # This is a RISC-V assembly program
            .global main       # Make main visible externally

            .text
            main:              # Main entry point
                addi sp, sp, -16   # Allocate stack frame
                sw ra, 12(sp)     # Save return address
                
                lui a0, 0x42       # Load immediate
                jal ra, print     # Call print function
                
                lw ra, 12(sp)     # Restore return address
                addi sp, sp, 16   # Deallocate stack frame
            
            print:
                # Some function code...
                addi a0, a0, 1    # Increment argument
        "#;

        let tokens = tokenize(source).unwrap();
        let mut parser = Parser::new(&tokens);
        let mut symbol_table = SymbolTable::new();

        let parsed = parser.parse_all(&mut symbol_table).unwrap();

        // Verify we parsed expected number of items
        // - 1 global directive
        // - 1 text section
        // - 2 labels in text (main, print)
        // - 7 instructions in main
        // - 2 instructions in print
        // - 1 data section
        // - 2 labels in data (message, numbers)
        // - 2 data directives
        assert!(
            parsed.len() >= 11,
            "Expected at least 11 parsed items, got {}",
            parsed.len()
        );

        // Verify symbol table has main, print, message, and numbers labels
        assert!(symbol_table.is_defined("main"), "Symbol 'main' not found");
        assert!(symbol_table.is_defined("print"), "Symbol 'print' not found");

        // Verify the type of some specific items
        let mut found_main_label = false;
        let mut found_addi_sp = false;
        let mut found_global_main = false;

        for item in &parsed {
            match item {
                ParsedItem::Label(label) if label.name == "main" => {
                    found_main_label = true;
                }
                ParsedItem::Instruction(instr)
                    if instr.mnemonic == "addi"
                    && matches!(instr.operands[0], Operand::Register(2)) // sp is x2
                    && matches!(instr.operands[1], Operand::Register(2)) // sp is x2
                    && matches!(instr.operands[2], Operand::Immediate(-16)) =>
                {
                    found_addi_sp = true;
                }
                ParsedItem::Directive(dir) if matches!(dir.directive, Directive::Global(ref s) if s == "main") =>
                {
                    found_global_main = true;
                }
                _ => {}
            }
        }

        assert!(found_main_label, "Main label not found");
        assert!(found_addi_sp, "addi sp, sp, -16 instruction not found");
        assert!(found_global_main, ".global main directive not found");
    }

    #[test]
    fn test_address_tracking() {
        // Test that the parser correctly updates addresses
        let source = r#"
        .text
    start:
        addi x1, x0, 1    # 4 bytes
        addi x2, x0, 2    # 4 bytes
        .align 3          # Align to 8 bytes (might add padding)
    aligned:
        addi x3, x0, 3    # 4 bytes
        .word 0xdeadbeef  # 4 bytes
        .byte 0x42        # 1 byte
        "#;

        let tokens = tokenize(source).unwrap();
        let mut parser = Parser::new(&tokens);
        let mut symbol_table = SymbolTable::new();

        let parsed = parser.parse_all(&mut symbol_table).unwrap();

        // Get addresses from symbol table
        let start_addr = symbol_table.lookup("start").unwrap().address();
        let aligned_addr = symbol_table.lookup("aligned").unwrap().address();

        // Verify start address is 0
        assert_eq!(start_addr, 0, "start should be at address 0");

        // Verify aligned address is a multiple of 8 (2^3)
        assert_eq!(
            aligned_addr % 8,
            0,
            "aligned should be at an 8-byte boundary"
        );

        // Verify aligned is after start + 2 instructions (at least 8 bytes)
        assert!(
            aligned_addr >= start_addr + 8,
            "aligned should be at least 8 bytes after start"
        );

        // Find the .byte directive and get its address
        let mut byte_addr = 0;
        for item in &parsed {
            if let ParsedItem::Directive(dir) = item {
                if let Directive::Byte(_) = dir.directive {
                    byte_addr = dir.address;
                    break;
                }
            }
        }

        // Verify byte address is after aligned + instruction + word (at least 8 more bytes)
        assert!(
            byte_addr >= aligned_addr + 8,
            "byte directive should be at least 8 bytes after aligned"
        );
    }
}
