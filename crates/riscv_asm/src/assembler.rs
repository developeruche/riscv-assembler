//! Main assembler code
#![allow(unused_assignments)]
use crate::error::AssemblerError;
use crate::isa::{Instruction, Operand as IsaOperand, Register};
use crate::lexer::tokenize;
use crate::parser::{Directive, Operand as ParsedOperand, ParsedItem, Parser};
use crate::symbol::SymbolTable;

/// Represents the output of the assembler, containing machine code and metadata
pub struct AssemblyOutput {
    /// The machine code as a sequence of words (32-bit values)
    pub code: Vec<u32>,
    /// The final size in bytes
    pub size: usize,
    /// Starting address of the code
    pub start_address: u32,
}

/// Assembles RISC-V assembly code into machine code
pub fn assemble(source: &str) -> Result<AssemblyOutput, AssemblerError> {
    // Step 1: Tokenize the assembly code
    let tokens = tokenize(source)?;

    // Step 2: Parse tokens and build the symbol table
    let mut symbol_table = SymbolTable::new();
    let mut parser = Parser::new(&tokens);
    let parsed_items = parser.parse_all(&mut symbol_table)?;

    // Check for unresolved symbols
    let unresolved = symbol_table.check_unresolved();
    if !unresolved.is_empty() {
        let mut errors = Vec::new();
        for (name, lines) in unresolved {
            errors.push(AssemblerError::SymbolError {
                message: format!("Undefined symbol: {}", name),
                loc: crate::error::SourceLocation {
                    line: *lines.first().unwrap_or(&0),
                    col: 0,
                },
            });
        }
        if errors.len() == 1 {
            return Err(errors.remove(0));
        } else {
            return Err(AssemblerError::MultipleErrors(errors));
        }
    }

    // Step 3: Allocate memory based on parsed items
    let mut memory_map = MemoryMap::new();
    allocate_memory(&parsed_items, &mut memory_map)?;

    // Step 4: Generate machine code
    let output = generate_machine_code(&parsed_items, &symbol_table, &memory_map)?;
    Ok(output)
}

/// Represents a memory location during assembly
pub struct MemoryLocation {
    pub address: u32,
    pub size: usize, // in bytes
}

/// Maps parsed items to their allocated memory locations
struct MemoryMap {
    locations: Vec<(usize, MemoryLocation)>, // (item_index, location)
    current_address: u32,
}

impl MemoryMap {
    fn new() -> Self {
        MemoryMap {
            locations: Vec::new(),
            current_address: 0,
        }
    }

    ///When we call `map.allocate(0, 4, 4)`, we're saying:
    /// 1. "I need to store item #0"
    /// 2. "It requires 4 bytes of space"
    /// 3. "Its starting address must be divisible by 4"
    fn allocate(&mut self, item_index: usize, size: usize, align: usize) -> u32 {
        // Handle alignment if needed
        if align > 1 {
            let mask = align - 1;
            self.current_address = (self.current_address + mask as u32) & !(mask as u32);
        }

        let address = self.current_address;
        self.locations
            .push((item_index, MemoryLocation { address, size }));

        self.current_address += size as u32;
        address
    }

    fn get_address(&self, item_index: usize) -> Option<u32> {
        self.locations
            .iter()
            .find(|(idx, _)| *idx == item_index)
            .map(|(_, loc)| loc.address)
    }
}

/// First pass: allocate memory for all instructions and directives
fn allocate_memory(
    parsed_items: &[ParsedItem],
    memory_map: &mut MemoryMap,
) -> Result<(), AssemblerError> {
    for (i, item) in parsed_items.iter().enumerate() {
        match item {
            ParsedItem::Instruction(_) => {
                // All RISC-V instructions are 4 bytes
                memory_map.allocate(i, 4, 4); // Align instructions to 4 bytes
            }
            ParsedItem::Directive(dir) => match &dir.directive {
                Directive::Byte(_) => {
                    memory_map.allocate(i, 1, 1);
                }
                Directive::Half(_) => {
                    memory_map.allocate(i, 2, 2);
                }
                Directive::Word(_) => {
                    memory_map.allocate(i, 4, 4);
                }
                Directive::Asciz(s) => {
                    memory_map.allocate(i, s.len() + 1, 1); // +1 for null terminator
                }
                Directive::Align(n) => {
                    // Align to 2^n boundary
                    let alignment = 1 << *n;
                    memory_map.allocate(i, 0, alignment as usize); // Size 0 since we're just aligning
                }
                Directive::Space(n) | Directive::Zero(n) => {
                    memory_map.allocate(i, *n as usize, 1);
                }
                Directive::Org(addr) => {
                    // Set the current address explicitly
                    memory_map.current_address = *addr as u32;
                }
                // Other directives don't consume memory
                _ => {}
            },
            // Labels and empty lines don't consume memory
            _ => {}
        }
    }

    Ok(())
}

/// Second pass: generate machine code for all instructions and directives
fn generate_machine_code(
    parsed_items: &[ParsedItem],
    symbol_table: &SymbolTable,
    memory_map: &MemoryMap,
) -> Result<AssemblyOutput, AssemblerError> {
    // let mut code = Vec::new();
    let mut output_address = 0;
    let start_address = memory_map
        .locations
        .first()
        .map(|(_, loc)| loc.address)
        .unwrap_or(0);

    // Pre-allocate bytes for the entire program
    let total_size = memory_map.current_address as usize;
    let mut bytes = vec![0u8; total_size];

    for (i, item) in parsed_items.iter().enumerate() {
        if let Some(address) = memory_map.get_address(i) {
            output_address = address;

            match item {
                ParsedItem::Instruction(instr) => {
                    // Convert parsed instruction to ISA instruction
                    let isa_instr = convert_to_isa_instruction(instr, symbol_table)?;

                    // Encode the instruction
                    let loc = crate::error::SourceLocation {
                        line: instr.line_number,
                        col: instr.column,
                    };
                    let encoded = isa_instr.encode(symbol_table, output_address, &loc)?;

                    // Write to the output buffer
                    let offset = output_address as usize;
                    bytes[offset..offset + 4].copy_from_slice(&encoded.to_le_bytes());
                }
                ParsedItem::Directive(dir) => match &dir.directive {
                    Directive::Byte(val) => {
                        let offset = output_address as usize;
                        bytes[offset] = *val as u8;
                    }
                    Directive::Half(val) => {
                        let offset = output_address as usize;
                        bytes[offset..offset + 2].copy_from_slice(&(*val as u16).to_le_bytes());
                    }
                    Directive::Word(val) => {
                        let offset = output_address as usize;
                        bytes[offset..offset + 4].copy_from_slice(&(*val as u32).to_le_bytes());
                    }
                    Directive::Asciz(s) => {
                        let offset = output_address as usize;
                        // Copy the string bytes plus null terminator
                        for (i, b) in s.bytes().enumerate() {
                            bytes[offset + i] = b;
                        }
                        // Null terminator
                        bytes[offset + s.len()] = 0;
                    }
                    Directive::Space(n) => {
                        // Space already filled with zeros by our pre-allocation
                        let offset = output_address as usize;
                        for i in 0..*n as usize {
                            bytes[offset + i] = 0;
                        }
                    }
                    Directive::Zero(n) => {
                        // Same as Space, already zeroed
                        let offset = output_address as usize;
                        for i in 0..*n as usize {
                            bytes[offset + i] = 0;
                        }
                    }
                    // Other directives don't generate code
                    _ => {}
                },
                _ => {} // Labels and empty lines don't generate code
            }
        }
    }

    // Convert bytes to 32-bit words for the output
    let word_count = (bytes.len() + 3) / 4; // Ceiling division to include partial final word
    let mut words = Vec::with_capacity(word_count);

    for chunk in bytes.chunks(4) {
        let mut word = 0u32;
        for (i, &byte) in chunk.iter().enumerate() {
            word |= (byte as u32) << (i * 8);
        }
        words.push(word);
    }

    Ok(AssemblyOutput {
        code: words,
        size: bytes.len(),
        start_address,
    })
}

/// Convert a parsed instruction to an ISA instruction for encoding
fn convert_to_isa_instruction(
    instr: &crate::parser::ParsedInstruction,
    symbol_table: &SymbolTable,
) -> Result<Instruction, AssemblerError> {
    use crate::isa::Instruction::*;

    // Helper to convert a ParsedOperand to an IsaOperand
    let convert_operand = |op: &ParsedOperand| -> Result<IsaOperand, AssemblerError> {
        match op {
            ParsedOperand::Register(r) => Ok(IsaOperand::Register(Register::new(*r).unwrap())),
            ParsedOperand::Immediate(val) => Ok(IsaOperand::Immediate(*val)),
            ParsedOperand::Symbol(name) => {
                // Lookup the symbol
                if let Some(sym) = symbol_table.lookup(name) {
                    Ok(IsaOperand::Immediate(sym.address() as i64))
                } else {
                    Err(AssemblerError::SymbolError {
                        message: format!("Undefined symbol: {}", name),
                        loc: crate::error::SourceLocation {
                            line: instr.line_number,
                            col: instr.column,
                        },
                    })
                }
            }
            ParsedOperand::Memory {
                offset,
                base: _base,
            } => {
                // Memory operands typically need to be handled specially based on the instruction
                // but for now we'll just note this is incomplete
                Ok(IsaOperand::Immediate(*offset))
            }
        }
    };

    // Match based on mnemonic and operands
    match instr.mnemonic.as_str() {
        // R-type instructions
        "add" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Add { rd, rs1, rs2 })
        }

        "sub" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Sub { rd, rs1, rs2 })
        }

        "xor" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Xor { rd, rs1, rs2 })
        }

        "or" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Or { rd, rs1, rs2 })
        }

        "and" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(And { rd, rs1, rs2 })
        }

        "sll" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Sll { rd, rs1, rs2 })
        }

        "srl" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Srl { rd, rs1, rs2 })
        }

        "sra" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Sra { rd, rs1, rs2 })
        }

        "slt" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Slt { rd, rs1, rs2 })
        }

        "sltu" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Sltu { rd, rs1, rs2 })
        }

        // M-extension R-type instructions
        "mul" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Mul { rd, rs1, rs2 })
        }

        "mulh" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Mulh { rd, rs1, rs2 })
        }

        "mulhsu" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Mulhsu { rd, rs1, rs2 })
        }

        "mulhu" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Mulhu { rd, rs1, rs2 })
        }

        "div" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Div { rd, rs1, rs2 })
        }

        "divu" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Divu { rd, rs1, rs2 })
        }

        "rem" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Rem { rd, rs1, rs2 })
        }

        "remu" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[2])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 2, "register")),
            };
            Ok(Remu { rd, rs1, rs2 })
        }

        // I-type instructions
        "addi" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let imm = convert_operand(&instr.operands[2])?;
            Ok(Addi { rd, rs1, imm })
        }

        "xori" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let imm = convert_operand(&instr.operands[2])?;
            Ok(Xori { rd, rs1, imm })
        }

        "ori" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let imm = convert_operand(&instr.operands[2])?;
            Ok(Ori { rd, rs1, imm })
        }

        "andi" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let imm = convert_operand(&instr.operands[2])?;
            Ok(Andi { rd, rs1, imm })
        }

        "slli" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let shamt = convert_operand(&instr.operands[2])?;
            Ok(Slli { rd, rs1, shamt })
        }

        "srli" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let shamt = convert_operand(&instr.operands[2])?;
            Ok(Srli { rd, rs1, shamt })
        }

        "srai" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let shamt = convert_operand(&instr.operands[2])?;
            Ok(Srai { rd, rs1, shamt })
        }

        "slti" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let imm = convert_operand(&instr.operands[2])?;
            Ok(Slti { rd, rs1, imm })
        }

        "sltiu" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let imm = convert_operand(&instr.operands[2])?;
            Ok(Sltiu { rd, rs1, imm })
        }

        // Load instructions
        "lb" if instr.operands.len() == 2 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };

            match &instr.operands[1] {
                ParsedOperand::Memory { offset, base } => {
                    let rs1 = Register::new(*base).unwrap();
                    Ok(Lb {
                        rd,
                        rs1,
                        imm: IsaOperand::Immediate(*offset),
                    })
                }
                _ => Err(invalid_operand_error(instr, 1, "memory reference")),
            }
        }

        "lh" if instr.operands.len() == 2 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };

            match &instr.operands[1] {
                ParsedOperand::Memory { offset, base } => {
                    let rs1 = Register::new(*base).unwrap();
                    Ok(Lh {
                        rd,
                        rs1,
                        imm: IsaOperand::Immediate(*offset),
                    })
                }
                _ => Err(invalid_operand_error(instr, 1, "memory reference")),
            }
        }
        "lw" if instr.operands.len() == 2 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };

            // Handle memory operand
            match &instr.operands[1] {
                ParsedOperand::Memory { offset, base } => {
                    let rs1 = Register::new(*base).unwrap();
                    Ok(Lw {
                        rd,
                        rs1,
                        imm: IsaOperand::Immediate(*offset),
                    })
                }
                _ => Err(invalid_operand_error(instr, 1, "memory reference")),
            }
        }

        "lbu" if instr.operands.len() == 2 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };

            match &instr.operands[1] {
                ParsedOperand::Memory { offset, base } => {
                    let rs1 = Register::new(*base).unwrap();
                    Ok(Lbu {
                        rd,
                        rs1,
                        imm: IsaOperand::Immediate(*offset),
                    })
                }
                _ => Err(invalid_operand_error(instr, 1, "memory reference")),
            }
        }

        "lhu" if instr.operands.len() == 2 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };

            match &instr.operands[1] {
                ParsedOperand::Memory { offset, base } => {
                    let rs1 = Register::new(*base).unwrap();
                    Ok(Lhu {
                        rd,
                        rs1,
                        imm: IsaOperand::Immediate(*offset),
                    })
                }
                _ => Err(invalid_operand_error(instr, 1, "memory reference")),
            }
        }

        // Store instructions
        "sb" if instr.operands.len() == 2 => {
            let rs2 = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };

            match &instr.operands[1] {
                ParsedOperand::Memory { offset, base } => {
                    let rs1 = Register::new(*base).unwrap();
                    Ok(Sb {
                        rs1,
                        rs2,
                        imm: IsaOperand::Immediate(*offset),
                    })
                }
                _ => Err(invalid_operand_error(instr, 1, "memory reference")),
            }
        }

        "sh" if instr.operands.len() == 2 => {
            let rs2 = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };

            match &instr.operands[1] {
                ParsedOperand::Memory { offset, base } => {
                    let rs1 = Register::new(*base).unwrap();
                    Ok(Sh {
                        rs1,
                        rs2,
                        imm: IsaOperand::Immediate(*offset),
                    })
                }
                _ => Err(invalid_operand_error(instr, 1, "memory reference")),
            }
        }

        "sw" if instr.operands.len() == 2 => {
            let rs2 = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };

            // Handle memory operand
            match &instr.operands[1] {
                ParsedOperand::Memory { offset, base } => {
                    let rs1 = Register::new(*base).unwrap();
                    Ok(Sw {
                        rs1,
                        rs2,
                        imm: IsaOperand::Immediate(*offset),
                    })
                }
                _ => Err(invalid_operand_error(instr, 1, "memory reference")),
            }
        }

        // Branch instructions
        "beq" if instr.operands.len() == 3 => {
            let rs1 = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let target = convert_operand(&instr.operands[2])?;
            Ok(Beq { rs1, rs2, target })
        }

        "bne" if instr.operands.len() == 3 => {
            let rs1 = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let target = convert_operand(&instr.operands[2])?;
            Ok(Bne { rs1, rs2, target })
        }

        "blt" if instr.operands.len() == 3 => {
            let rs1 = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let target = convert_operand(&instr.operands[2])?;
            Ok(Blt { rs1, rs2, target })
        }

        "bge" if instr.operands.len() == 3 => {
            let rs1 = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let target = convert_operand(&instr.operands[2])?;
            Ok(Bge { rs1, rs2, target })
        }

        "bltu" if instr.operands.len() == 3 => {
            let rs1 = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let target = convert_operand(&instr.operands[2])?;
            Ok(Bltu { rs1, rs2, target })
        }

        "bgeu" if instr.operands.len() == 3 => {
            let rs1 = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs2 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let target = convert_operand(&instr.operands[2])?;
            Ok(Bgeu { rs1, rs2, target })
        }

        // Jump instructions
        "jal" if instr.operands.len() == 2 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let target = convert_operand(&instr.operands[1])?;
            Ok(Jal { rd, target })
        }

        // Special case for pseudo-instruction 'j label' -> 'jal x0, label'
        // TODO: Propagate this feature to the lexer
        "j" if instr.operands.len() == 1 => {
            let target = convert_operand(&instr.operands[0])?;
            Ok(Jal {
                rd: Register::new(0).unwrap(),
                target,
            })
        }

        "jalr" if instr.operands.len() == 3 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let rs1 = match convert_operand(&instr.operands[1])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 1, "register")),
            };
            let imm = convert_operand(&instr.operands[2])?;
            Ok(Jalr { rd, rs1, imm })
        }

        // Special case for pseudo-instruction 'ret' -> 'jalr x0, ra, 0'
        // TODO: Propagate this feature to the lexer
        "ret" if instr.operands.is_empty() => {
            Ok(Jalr {
                rd: Register::new(0).unwrap(),
                rs1: Register::new(1).unwrap(), // ra is x1
                imm: IsaOperand::Immediate(0),
            })
        }

        // U-type instructions
        "lui" if instr.operands.len() == 2 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let imm = convert_operand(&instr.operands[1])?;
            Ok(Lui { rd, imm })
        }

        "auipc" if instr.operands.len() == 2 => {
            let rd = match convert_operand(&instr.operands[0])? {
                IsaOperand::Register(r) => r,
                _ => return Err(invalid_operand_error(instr, 0, "register")),
            };
            let imm = convert_operand(&instr.operands[1])?;
            Ok(Auipc { rd, imm })
        }

        // Special instruction
        "ecall" if instr.operands.is_empty() => Ok(Ecall),

        // Add more instructions as needed...
        _ => Err(AssemblerError::EncodingError {
            message: format!("Unsupported instruction: {}", instr.mnemonic),
            loc: crate::error::SourceLocation {
                line: instr.line_number,
                col: instr.column,
            },
        }),
    }
}

fn invalid_operand_error(
    instr: &crate::parser::ParsedInstruction,
    operand_idx: usize,
    expected: &str,
) -> AssemblerError {
    AssemblerError::EncodingError {
        message: format!(
            "Invalid operand for {}: expected {}\n OperandIndex: {}",
            instr.mnemonic, expected, operand_idx
        ),
        loc: crate::error::SourceLocation {
            line: instr.line_number,
            col: instr.column,
        },
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{ParsedDirective, ParsedInstruction, ParsedLabel};

    use super::*;

    #[test]
    fn test_simple_assembly() {
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

        let result = assemble(source).unwrap();

        // We should have at least 4 words (3 instructions + 1 data word)
        assert!(result.code.len() >= 4);

        // Verify the first instruction (addi x1, x0, 1)
        // 0x00100093 = addi x1, x0, 1
        assert_eq!(result.code[0], 0x00100093);

        // Verify the second instruction (addi x2, x0, 2)
        // 0x00200113 = addi x2, x0, 2
        assert_eq!(result.code[1], 0x00200113);

        // The alignment might add padding

        // Verify the third instruction (addi x3, x0, 3)
        // This might not be at index 2 if there's padding from .align
        // We'd need to look up its address to be sure

        // Verify the .word directive
        // Check if 0xdeadbeef is in the output
        assert!(result.code.contains(&0xdeadbeef));

        // Verify the .byte directive
        // The byte 0x42 is somewhere in the last word
        // We would need to check the exact position based on alignment
    }

    #[test]
    fn test_memory_map() {
        let mut map = MemoryMap::new();

        // Test initial state
        assert_eq!(map.current_address, 0);
        assert_eq!(map.locations.len(), 0);

        // Test basic allocation
        let addr1 = map.allocate(0, 4, 4); // Allocate 4 bytes with 4-byte alignment
        assert_eq!(addr1, 0);
        assert_eq!(map.current_address, 4);
        assert_eq!(map.locations.len(), 1);

        // Test alignment
        let addr2 = map.allocate(1, 2, 8); // Allocate 2 bytes with 8-byte alignment
        // Current address is 4, aligning to 8 should give 8
        assert_eq!(addr2, 8);
        assert_eq!(map.current_address, 10);

        // Test get_address
        assert_eq!(map.get_address(0), Some(0));
        assert_eq!(map.get_address(1), Some(8));
        assert_eq!(map.get_address(2), None);

        // Test another allocation
        let addr3 = map.allocate(2, 6, 4); // Allocate 6 bytes with 4-byte alignment
        // Current address is 10, aligning to 4 should give 12
        assert_eq!(addr3, 12);
        assert_eq!(map.current_address, 18);
    }

    #[test]
    fn test_convert_to_isa_instruction() {
        let mut symbol_table = SymbolTable::new();
        symbol_table
            .define("loop".to_string(), 0x100, Some(5))
            .unwrap();

        // Test ADD instruction conversion
        let add_instr = ParsedInstruction {
            mnemonic: "add".to_string(),
            operands: vec![
                ParsedOperand::Register(1), // x1
                ParsedOperand::Register(2), // x2
                ParsedOperand::Register(3), // x3
            ],
            address: 0,
            line_number: 1,
            column: 1,
        };

        let result = convert_to_isa_instruction(&add_instr, &symbol_table).unwrap();
        if let crate::isa::Instruction::Add { rd, rs1, rs2 } = result {
            assert_eq!(rd.number(), 1);
            assert_eq!(rs1.number(), 2);
            assert_eq!(rs2.number(), 3);
        } else {
            panic!("Expected Add instruction");
        }

        // Test ADDI instruction with immediate
        let addi_instr = ParsedInstruction {
            mnemonic: "addi".to_string(),
            operands: vec![
                ParsedOperand::Register(5),   // x5
                ParsedOperand::Register(0),   // x0
                ParsedOperand::Immediate(42), // 42
            ],
            address: 0,
            line_number: 2,
            column: 1,
        };

        let result = convert_to_isa_instruction(&addi_instr, &symbol_table).unwrap();
        if let crate::isa::Instruction::Addi { rd, rs1, imm } = result {
            assert_eq!(rd.number(), 5);
            assert_eq!(rs1.number(), 0);
            assert!(matches!(imm, crate::isa::Operand::Immediate(42)));
        } else {
            panic!("Expected Addi instruction");
        }

        // Test LW instruction with memory operand
        let lw_instr = ParsedInstruction {
            mnemonic: "lw".to_string(),
            operands: vec![
                ParsedOperand::Register(10),                  // x10 (a0)
                ParsedOperand::Memory { offset: 8, base: 2 }, // 8(x2) (sp)
            ],
            address: 0,
            line_number: 3,
            column: 1,
        };

        let result = convert_to_isa_instruction(&lw_instr, &symbol_table).unwrap();
        if let crate::isa::Instruction::Lw { rd, rs1, imm } = result {
            assert_eq!(rd.number(), 10);
            assert_eq!(rs1.number(), 2);
            assert!(matches!(imm, crate::isa::Operand::Immediate(8)));
        } else {
            panic!("Expected Lw instruction");
        }

        // Test instruction with symbol reference
        let beq_instr = ParsedInstruction {
            mnemonic: "beq".to_string(),
            operands: vec![
                ParsedOperand::Register(4),           // x4
                ParsedOperand::Register(5),           // x5
                ParsedOperand::Symbol("loop".into()), // loop label
            ],
            address: 0,
            line_number: 4,
            column: 1,
        };

        let result = convert_to_isa_instruction(&beq_instr, &symbol_table).unwrap();
        if let crate::isa::Instruction::Beq { rs1, rs2, target } = result {
            assert_eq!(rs1.number(), 4);
            assert_eq!(rs2.number(), 5);
            assert!(matches!(target, crate::isa::Operand::Immediate(256))); // 0x100
        } else {
            panic!("Expected Beq instruction");
        }
    }

    #[test]
    fn test_allocate_memory() {
        let mut memory_map = MemoryMap::new();

        // Create a list of parsed items
        let parsed_items = vec![
            // An instruction (4 bytes)
            ParsedItem::Instruction(ParsedInstruction {
                mnemonic: "add".to_string(),
                operands: vec![
                    ParsedOperand::Register(1),
                    ParsedOperand::Register(2),
                    ParsedOperand::Register(3),
                ],
                address: 0,
                line_number: 1,
                column: 1,
            }),
            // A label (doesn't consume memory)
            ParsedItem::Label(ParsedLabel {
                name: "label1".to_string(),
                address: 4,
                line_number: 2,
                column: 1,
            }),
            // A byte directive (1 byte)
            ParsedItem::Directive(ParsedDirective {
                directive: Directive::Byte(42),
                address: 4,
                line_number: 3,
                column: 1,
            }),
            // A word directive (4 bytes)
            ParsedItem::Directive(ParsedDirective {
                directive: Directive::Word(0xdeadbeef),
                address: 5,
                line_number: 4,
                column: 1,
            }),
            // An align directive (aligns to 8-byte boundary)
            ParsedItem::Directive(ParsedDirective {
                directive: Directive::Align(3), // 2^3 = 8
                address: 9,
                line_number: 5,
                column: 1,
            }),
            // Another instruction after alignment
            ParsedItem::Instruction(ParsedInstruction {
                mnemonic: "addi".to_string(),
                operands: vec![
                    ParsedOperand::Register(5),
                    ParsedOperand::Register(0),
                    ParsedOperand::Immediate(10),
                ],
                address: 0,
                line_number: 6,
                column: 1,
            }),
        ];

        // Allocate memory for the items
        allocate_memory(&parsed_items, &mut memory_map).unwrap();

        // Verify memory allocations
        assert_eq!(memory_map.get_address(0), Some(0)); // First instruction at 0
        assert_eq!(memory_map.get_address(2), Some(4)); // Byte directive at 4
        assert_eq!(memory_map.get_address(3), Some(8)); // Word directive at 5
        assert_eq!(memory_map.get_address(4), Some(16)); // Align directive at 9

        // The last instruction should be at address 16 (after alignment to 8-byte boundary)
        // 9 -> 16 (aligned to 8) -> 16 + 4 = 20 (end)
        assert_eq!(memory_map.get_address(5), Some(16));

        // Final address should be 20
        assert_eq!(memory_map.current_address, 20);
    }

    #[test]
    fn test_generate_machine_code() {
        let mut symbol_table = SymbolTable::new();
        symbol_table
            .define("start".to_string(), 0, Some(1))
            .unwrap();

        let mut memory_map = MemoryMap::new();

        let parsed_items = vec![
            // Label
            ParsedItem::Label(ParsedLabel {
                name: "start".to_string(),
                address: 0,
                line_number: 1,
                column: 1,
            }),
            // addi x1, x0, 1
            ParsedItem::Instruction(ParsedInstruction {
                mnemonic: "addi".to_string(),
                operands: vec![
                    ParsedOperand::Register(1),
                    ParsedOperand::Register(0),
                    ParsedOperand::Immediate(1),
                ],
                address: 0,
                line_number: 2,
                column: 1,
            }),
            // .word 0xdeadbeef
            ParsedItem::Directive(ParsedDirective {
                directive: Directive::Word(0xdeadbeef),
                address: 4,
                line_number: 3,
                column: 1,
            }),
        ];

        // Allocate memory
        allocate_memory(&parsed_items, &mut memory_map).unwrap();

        // Generate machine code
        let output = generate_machine_code(&parsed_items, &symbol_table, &memory_map).unwrap();

        // Check the output
        assert_eq!(output.start_address, 0);
        assert_eq!(output.size, 8); // 4 bytes instruction + 4 bytes word
        assert_eq!(output.code.len(), 2);

        // Check that first word is the ADDI instruction
        // addi x1, x0, 1 => 0x00100093
        assert_eq!(output.code[0], 0x00100093);

        // Check that second word is 0xdeadbeef
        assert_eq!(output.code[1], 0xdeadbeef);
    }

    #[test]
    fn test_generate_machine_code_2() {
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

        let parsed_items = parser.parse_all(&mut symbol_table).unwrap();

        let mut memory_map = MemoryMap::new();
        allocate_memory(&parsed_items, &mut memory_map).unwrap();
        // Generate machine code
        let output = generate_machine_code(&parsed_items, &symbol_table, &memory_map).unwrap();

        // Check the output
        assert_eq!(output.start_address, 0);
        assert_eq!(output.size, 17); // 4 bytes instruction + 4 bytes word
        assert_eq!(output.code.len(), 5);

        // Check that first word is the ADDI instruction
        assert_eq!(output.code[0], 1048723);
        assert_eq!(output.code[1], 2097427);
    }

    #[test]
    fn test_assemble_1() {
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

        // assemble machine code
        let output = assemble(source).unwrap();

        // Check the output
        assert_eq!(output.start_address, 0);
        assert_eq!(output.size, 17); // 4 bytes instruction + 4 bytes word
        assert_eq!(output.code.len(), 5);

        // Check that first word is the ADDI instruction
        assert_eq!(output.code[0], 1048723);
        assert_eq!(output.code[1], 2097427);
    }

    #[test]
    #[ignore = "would be back to this after cli implemenation"]
    fn test_assemble_complete_program() {
        // A simple but complete RISC-V program that:
        // 1. Sets up registers
        // 2. Uses various instruction types
        // 3. Contains labels and branches
        // 4. Uses different directives
        let source = r#"
            # Test program with various RISC-V features

            .text

            # Program entry point
            main:
                # Stack setup
                addi sp, sp, -16       # Allocate stack frame
                sw ra, 12(sp)          # Save return address

                # Initialize registers
                addi a0, zero, 5       # Initialize a0 with 5
                addi a1, zero, 10      # Initialize a1 with 10

                # Test branch
                beq a0, a1, skip       # This branch should not be taken
                add a2, a0, a1         # a2 = a0 + a1 = 15

            skip:
                # Test jump and link
                jal ra, function       # Call function, store return address in ra

                # Cleanup and exit
                lw ra, 12(sp)          # Restore return address
                addi sp, sp, 16        # Deallocate stack frame

                # Test alignment directive
                .align 2               # Align to 4-byte boundary

            function:
                # Function that adds 1 to a0 and returns
                addi a0, a0, 1         # Increment a0
                jalr zero, ra, 0       # Return to caller

            # Data section
            .data
            .align 2
            value:
                .word 0xdeadbeef       # Test word directive
                .byte 0x42             # Test byte directive
        "#;

        // Assemble the program
        let result = assemble(source).expect("Assembly should succeed");

        // Check the basic properties of the output
        assert!(result.code.len() > 0, "Should generate machine code");
        assert_eq!(result.start_address, 0, "Program should start at address 0");

        println!("Generated code: {:?}", result.code);

        // Verify specific instructions in the output

        // addi sp, sp, -16 (first instruction) = 0xFF010113
        assert_eq!(result.code[0], 0xFF010113);

        // sw ra, 12(sp) (second instruction) = 0x00C12623
        assert_eq!(result.code[1], 0x00C12623);

        // addi a0, zero, 5 (third instruction) = 0x00500513
        assert_eq!(result.code[2], 0x00500513);

        // addi a1, zero, 10 (fourth instruction) = 0x00A00593
        assert_eq!(result.code[3], 0x00A00593);

        // We can also verify the total size is reasonable
        assert!(result.size > 40, "Program size should be at least 40 bytes");

        // Verify the size is consistent with the code length * 4
        assert_eq!(
            result.size,
            result.code.len() * 4,
            "Size should match code length * 4 (bytes per word)"
        );

        println!("Generated code: {:?}", result.code);
        println!("Program size: {} bytes", result.size);
    }

    #[test]
    #[ignore = "would be back to this after cli implemenation"]
    fn test_assemble_with_forward_references() {
        // Test that the assembler correctly handles forward references
        let source = r#"
            # Program using forward references

            .text
            start:
                addi t0, zero, 1       # Set t0 = 1
                beq t0, zero, end      # Forward branch to end (should not be taken)
                jal ra, middle         # Forward jump to middle

            loop:
                addi t0, t0, -1        # Decrement t0
                bge t0, zero, loop     # Loop until t0 < 0
                j end                  # Jump to end

            middle:
                addi t0, t0, 5         # t0 = t0 + 5 = 6
                jalr zero, ra, 0       # Return to caller

            end:
                addi a0, t0, 0         # Set return value in a0
        "#;

        // Assemble the program
        let result = assemble(source).expect("Assembly should succeed");

        // The program should have at least 7 instructions
        assert!(
            result.code.len() >= 7,
            "Should generate at least 7 instructions"
        );

        // First instruction: addi t0, zero, 1 = 0x00100293
        assert_eq!(result.code[0], 0x00100293);

        println!("Generated code with forward references: {:?}", result.code);
        println!("Program size: {} bytes", result.size);
    }

    #[test]
    #[ignore = "would be back to this after cli implemenation"]
    fn test_assemble_data_directives() {
        // Test that the assembler correctly handles data directives
        let source = r#"
            .data
            values:
                .word 0x12345678       # Test word
                .half 0xABCD           # Test half-word
                .byte 0x42             # Test byte

            .align 3                   # Align to 8-byte boundary
            string:
                .asciz "Hello, RISC-V" # Test string
        "#;

        // Assemble the program
        let result = assemble(source).expect("Assembly should succeed");

        // Verify the first word is our test value
        assert_eq!(result.code[0], 0x12345678);

        // Other data should also be in the output
        // We'd need to examine the exact memory layout to check everything

        println!("Generated data section: {:?}", result.code);
        println!("Data section size: {} bytes", result.size);
    }

    #[test]
    #[ignore = "would be back to this after cli implemenation"]
    fn test_assemble_error_handling() {
        // Test that the assembler correctly reports errors
        let invalid_source = r#"
            .text
            start:
                add x1, x2, x99        # Invalid register x99
        "#;

        // Assemble should return an error
        let result = assemble(invalid_source);
        assert!(
            result.is_err(),
            "Assembly should fail with invalid register"
        );

        // Test undefined symbol
        let undefined_symbol = r#"
            .text
            start:
                beq x0, x0, nonexistent_label  # Undefined label
        "#;

        let result = assemble(undefined_symbol);
        assert!(
            result.is_err(),
            "Assembly should fail with undefined symbol"
        );
    }
}
