//! RISC-V RV32IM Instruction Set Architecture definitions and encoding logic.

use crate::error::AssemblerError;
use crate::parser::{Instruction, Operand};
use crate::symbol::SymbolTable;
use std::collections::HashMap;

// --- Constants for Opcodes, Funct3, Funct7 ---
// Reference: RISC-V Unprivileged ISA Specification
mod opcodes {
    pub const LOAD: u32 = 0b0000011;
    pub const STORE: u32 = 0b0100011;
    pub const MADD: u32 = 0b1000011; // R4-type, not strictly RV32IM base
    pub const MSUB: u32 = 0b1000111; // R4-type
    pub const NMSUB: u32 = 0b1001011;// R4-type
    pub const NMADD: u32 = 0b1001111;// R4-type
    pub const OP_IMM: u32 = 0b0010011; // I-type (ALU immediate)
    pub const OP: u32 = 0b0110011;     // R-type (ALU register)
    pub const LUI: u32 = 0b0110111;    // U-type
    pub const AUIPC: u32 = 0b0010111;   // U-type
    pub const BRANCH: u32 = 0b1100011;  // B-type
    pub const JALR: u32 = 0b1100111;   // I-type (JALR)
    pub const JAL: u32 = 0b1101111;    // J-type
    pub const SYSTEM: u32 = 0b1110011; // I-type (ECALL/EBREAK)
    // Add AMO, FENCE if needed (RV32A, Zifencei)
}

mod funct3 {
    // OP_IMM
    pub const ADDI: u32 = 0b000;
    pub const SLTI: u32 = 0b010;
    pub const SLTIU: u32 = 0b011;
    pub const XORI: u32 = 0b100;
    pub const ORI: u32 = 0b110;
    pub const ANDI: u32 = 0b111;
    pub const SLLI: u32 = 0b001; // Requires funct7=0
    pub const SRLI: u32 = 0b101; // Requires funct7=0
    pub const SRAI: u32 = 0b101; // Requires funct7=0b0100000

    // OP
    pub const ADD_SUB: u32 = 0b000; // ADD (funct7=0), SUB (funct7=0b0100000)
    pub const SLL: u32 = 0b001;
    pub const SLT: u32 = 0b010;
    pub const SLTU: u32 = 0b011;
    pub const XOR: u32 = 0b100;
    pub const SRL_SRA: u32 = 0b101; // SRL (funct7=0), SRA (funct7=0b0100000)
    pub const OR: u32 = 0b110;
    pub const AND: u32 = 0b111;
    // M-extension
    pub const MUL: u32 = 0b000;
    pub const MULH: u32 = 0b001;
    pub const MULHSU: u32 = 0b010;
    pub const MULHU: u32 = 0b011;
    pub const DIV: u32 = 0b100;
    pub const DIVU: u32 = 0b101;
    pub const REM: u32 = 0b110;
    pub const REMU: u32 = 0b111;


    // LOAD
    pub const LB: u32 = 0b000;
    pub const LH: u32 = 0b001;
    pub const LW: u32 = 0b010;
    pub const LBU: u32 = 0b100;
    pub const LHU: u32 = 0b101;

    // STORE
    pub const SB: u32 = 0b000;
    pub const SH: u32 = 0b001;
    pub const SW: u32 = 0b010;

    // BRANCH
    pub const BEQ: u32 = 0b000;
    pub const BNE: u32 = 0b001;
    pub const BLT: u32 = 0b100;
    pub const BGE: u32 = 0b101;
    pub const BLTU: u32 = 0b110;
    pub const BGEU: u32 = 0b111;

    // JALR
    pub const JALR: u32 = 0b000;

    // SYSTEM
    pub const ECALL_EBREAK: u32 = 0b000;
}

mod funct7 {
    pub const ALT_OP: u32 = 0b0100000; // For SUB, SRA, SRAI
    pub const MULDIV: u32 = 0b0000001; // For M-extension instructions
    // ECALL/EBREAK specific funct12 values
    pub const ECALL: u32 = 0b000000000000;
    pub const EBREAK: u32 = 0b000000000001;

}

/// Represents a RISC-V register (x0-x31).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Register(u8);

impl Register {
    /// Creates a new Register if the number is valid (0-31).
    pub fn new(num: u8) -> Option<Self> {
        if num < 32 {
            Some(Register(num))
        } else {
            None
        }
    }

    /// Returns the register number.
    pub fn number(&self) -> u8 {
        self.0
    }

    /// Parses a register name (like "x10", "t0", "sp", "zero") into a Register.
    pub fn from_name(name: &str) -> Option<Self> {
        match name.to_lowercase().as_str() {
            "zero" | "x0" => Some(Register(0)),
            "ra" | "x1" => Some(Register(1)),
            "sp" | "x2" => Some(Register(2)),
            "gp" | "x3" => Some(Register(3)),
            "tp" | "x4" => Some(Register(4)),
            "t0" | "x5" => Some(Register(5)),
            "t1" | "x6" => Some(Register(6)),
            "t2" | "x7" => Some(Register(7)),
            "s0" | "fp" | "x8" => Some(Register(8)),
            "s1" | "x9" => Some(Register(9)),
            "a0" | "x10" => Some(Register(10)),
            "a1" | "x11" => Some(Register(11)),
            "a2" | "x12" => Some(Register(12)),
            "a3" | "x13" => Some(Register(13)),
            "a4" | "x14" => Some(Register(14)),
            "a5" | "x15" => Some(Register(15)),
            "a6" | "x16" => Some(Register(16)),
            "a7" | "x17" => Some(Register(17)),
            "s2" | "x18" => Some(Register(18)),
            "s3" | "x19" => Some(Register(19)),
            "s4" | "x20" => Some(Register(20)),
            "s5" | "x21" => Some(Register(21)),
            "s6" | "x22" => Some(Register(22)),
            "s7" | "x23" => Some(Register(23)),
            "s8" | "x24" => Some(Register(24)),
            "s9" | "x25" => Some(Register(25)),
            "s10" | "x26" => Some(Register(26)),
            "s11" | "x27" => Some(Register(27)),
            "t3" | "x28" => Some(Register(28)),
            "t4" | "x29" => Some(Register(29)),
            "t5" | "x30" => Some(Register(30)),
            "t6" | "x31" => Some(Register(31)),
            _ => {
                // Handle xN format directly
                if name.starts_with('x') && name.len() > 1 {
                    name[1..].parse::<u8>().ok().and_then(Register::new)
                } else {
                    None
                }
            }
        }
    }
}

/// Encodes a parsed instruction into its 32-bit binary representation.
///
/// # Arguments
/// * `instr`: The parsed instruction details.
/// * `current_address`: The address where this instruction will be placed.
/// * `symbol_table`: The symbol table for resolving labels.
///
/// # Returns
/// * `Ok(u32)`: The 32-bit encoded instruction.
/// * `Err(AssemblerError)`: If encoding fails (e.g., immediate out of range, invalid operand).
pub fn encode_instruction(
    instr: &Instruction,
    current_address: u32,
    symbol_table: &SymbolTable,
) -> Result<u32, AssemblerError> {
    let mnemonic = instr.mnemonic.to_lowercase();
    match mnemonic.as_str() {
        // --- R-Type Instructions (OP, M-ext) ---
        "add" | "sub" | "sll" | "slt" | "sltu" | "xor" | "srl" | "sra" | "or" | "and" |
        "mul" | "mulh" | "mulhsu" | "mulhu" | "div" | "divu" | "rem" | "remu" => {
            encode_r_type(instr, symbol_table)
        }

        // --- I-Type Instructions (OP_IMM, LOAD, JALR, SYSTEM) ---
        "addi" | "slti" | "sltiu" | "xori" | "ori" | "andi" | "slli" | "srli" | "srai" |
        "lb" | "lh" | "lw" | "lbu" | "lhu" |
        "jalr" |
        "ecall" | "ebreak" => {
            encode_i_type(instr, symbol_table)
        }

        // --- S-Type Instructions (STORE) ---
        "sb" | "sh" | "sw" => encode_s_type(instr, symbol_table),

        // --- B-Type Instructions (BRANCH) ---
        "beq" | "bne" | "blt" | "bge" | "bltu" | "bgeu" => {
            encode_b_type(instr, current_address, symbol_table)
        }

        // --- U-Type Instructions (LUI, AUIPC) ---
        "lui" | "auipc" => encode_u_type(instr, symbol_table),

        // --- J-Type Instructions (JAL) ---
        "jal" => encode_j_type(instr, current_address, symbol_table),

        // --- Pseudo-instructions ---
        "nop" => Ok(encode_i_type_fields(opcodes::OP_IMM, funct3::ADDI, 0, 0, 0)), // addi x0, x0, 0
        "li" => encode_li(instr, symbol_table), // Often expands to LUI + ADDI
        "mv" => encode_mv(instr, symbol_table), // mv rd, rs -> addi rd, rs, 0
        "j" => encode_j(instr, current_address, symbol_table), // j target -> jal x0, target
        "jr" => encode_jr(instr, symbol_table), // jr rs -> jalr x0, rs, 0
        "ret" => Ok(encode_i_type_fields(opcodes::JALR, funct3::JALR, 0, 1, 0)), // jalr x0, ra, 0
        "bnez" => encode_bnez(instr, current_address, symbol_table), // bnez rs, target -> bne rs, x0, target
        "beqz" => encode_beqz(instr, current_address, symbol_table), // beqz rs, target -> beq rs, x0, target
         // Add more pseudo-instructions as needed: call, tail, etc.

        _ => Err(AssemblerError::UnsupportedFeature {
            feature: format!("Instruction '{}'", instr.mnemonic),
            line: instr.line,
        }),
    }
}


// --- Encoding Helper Functions ---

fn get_register(operand: &Operand, line: usize) -> Result<u32, AssemblerError> {
    match operand {
        Operand::Register(name) => Register::from_name(name)
            .map(|r| r.number() as u32)
            .ok_or_else(|| AssemblerError::InvalidRegister {
                name: name.clone(),
                line,
                column: 0, // TODO: Improve column tracking
            }),
        _ => Err(AssemblerError::SyntaxError {
            message: "Expected register operand".to_string(),
            line,
            column: 0, // TODO: Improve column tracking
        }),
    }
}

fn get_immediate(operand: &Operand, symbol_table: &SymbolTable, line: usize) -> Result<i64, AssemblerError> {
     match operand {
        Operand::Immediate(val) => Ok(*val),
        Operand::Symbol(name) => {
            let symbol_info = symbol_table.resolve(name).map_err(|e| match e {
                // Remap symbol not found to include line number from instruction
                AssemblerError::SymbolNotFound { name, .. } => AssemblerError::SymbolNotFound { name, line },
                other => other,
            })?;
            // Treat symbol value as immediate (e.g., for li, lui, addi with label address)
             Ok(symbol_info.address as i64) // Use i64 for consistency
        },
        _ => Err(AssemblerError::SyntaxError {
            message: "Expected immediate or symbol operand".to_string(),
            line,
            column: 0, // TODO: Improve column tracking
        }),
    }
}


/// Check if immediate fits within `bits` signed bits.
fn check_immediate_range(value: i64, bits: u32, line: usize) -> Result<i32, AssemblerError> {
    let min = -(1i64 << (bits - 1));
    let max = (1i64 << (bits - 1)) - 1;
    if value >= min && value <= max {
        Ok(value as i32) // Cast to i32 for encoding
    } else {
        Err(AssemblerError::ImmediateOutOfRange {
            value: value.to_string(),
            min,
            max,
            line,
        })
    }
}
/// Check if immediate fits within `bits` unsigned bits.
fn check_immediate_range_unsigned(value: i64, bits: u32, line: usize) -> Result<u32, AssemblerError> {
    if value < 0 || value >= (1i64 << bits) {
         Err(AssemblerError::ImmediateOutOfRange {
             value: value.to_string(),
             min: 0,
             max: (1i64 << bits) -1,
             line,
         })
    } else {
        Ok(value as u32)
    }
}


fn encode_r_type_fields(opcode: u32, funct3: u32, funct7: u32, rd: u32, rs1: u32, rs2: u32) -> u32 {
    (funct7 << 25) | (rs2 << 20) | (rs1 << 15) | (funct3 << 12) | (rd << 7) | opcode
}

fn encode_i_type_fields(opcode: u32, funct3: u32, rd: u32, rs1: u32, imm: i32) -> u32 {
    let imm_u = imm as u32; // Cast immediate to unsigned for bit manipulation
    (imm_u << 20) | (rs1 << 15) | (funct3 << 12) | (rd << 7) | opcode
}

fn encode_s_type_fields(opcode: u32, funct3: u32, rs1: u32, rs2: u32, imm: i32) -> u32 {
    let imm_u = imm as u32;
    let imm11_5 = (imm_u >> 5) & 0b1111111; // imm[11:5]
    let imm4_0 = imm_u & 0b11111;          // imm[4:0]
    (imm11_5 << 25) | (rs2 << 20) | (rs1 << 15) | (funct3 << 12) | (imm4_0 << 7) | opcode
}

fn encode_b_type_fields(opcode: u32, funct3: u32, rs1: u32, rs2: u32, imm: i32) -> u32 {
     let imm_u = imm as u32;
    // imm[12 | 10:5] rs2 rs1 funct3 imm[4:1 | 11] opcode
    let imm12 = (imm_u >> 12) & 1;    // imm[12]
    let imm10_5 = (imm_u >> 5) & 0x3f;  // imm[10:5]
    let imm4_1 = (imm_u >> 1) & 0xf;    // imm[4:1]
    let imm11 = (imm_u >> 11) & 1;    // imm[11]

    (imm12 << 31) | (imm10_5 << 25) | (rs2 << 20) | (rs1 << 15) |
    (funct3 << 12) | (imm4_1 << 8) | (imm11 << 7) | opcode
}

fn encode_u_type_fields(opcode: u32, rd: u32, imm: i32) -> u32 {
    // Immediate is bits [31:12]
    let imm_u = (imm as u32) & 0xFFFFF000; // Mask to keep only upper 20 bits
    (imm_u) | (rd << 7) | opcode
}

fn encode_j_type_fields(opcode: u32, rd: u32, imm: i32) -> u32 {
    let imm_u = imm as u32;
    // imm[20 | 10:1 | 11 | 19:12] rd opcode
    let imm20 = (imm_u >> 20) & 1;      // imm[20]
    let imm10_1 = (imm_u >> 1) & 0x3ff;   // imm[10:1]
    let imm11 = (imm_u >> 11) & 1;      // imm[11]
    let imm19_12 = (imm_u >> 12) & 0xff;    // imm[19:12]

    (imm20 << 31) | (imm10_1 << 21) | (imm11 << 20) | (imm19_12 << 12) |
    (rd << 7) | opcode
}


// --- Specific Instruction Encoders ---

fn encode_r_type(instr: &Instruction, symbol_table: &SymbolTable) -> Result<u32, AssemblerError> {
    if instr.operands.len() != 3 {
        return Err(AssemblerError::SyntaxError { message: format!("Expected 3 operands for {}", instr.mnemonic), line: instr.line, column: 0 });
    }
    let rd = get_register(&instr.operands[0], instr.line)?;
    let rs1 = get_register(&instr.operands[1], instr.line)?;
    let rs2 = get_register(&instr.operands[2], instr.line)?;

    let (funct3, funct7, opcode) = match instr.mnemonic.to_lowercase().as_str() {
        "add" => (funct3::ADD_SUB, 0, opcodes::OP),
        "sub" => (funct3::ADD_SUB, funct7::ALT_OP, opcodes::OP),
        "sll" => (funct3::SLL, 0, opcodes::OP),
        "slt" => (funct3::SLT, 0, opcodes::OP),
        "sltu" => (funct3::SLTU, 0, opcodes::OP),
        "xor" => (funct3::XOR, 0, opcodes::OP),
        "srl" => (funct3::SRL_SRA, 0, opcodes::OP),
        "sra" => (funct3::SRL_SRA, funct7::ALT_OP, opcodes::OP),
        "or" => (funct3::OR, 0, opcodes::OP),
        "and" => (funct3::AND, 0, opcodes::OP),
        // M Extension
        "mul" => (funct3::MUL, funct7::MULDIV, opcodes::OP),
        "mulh" => (funct3::MULH, funct7::MULDIV, opcodes::OP),
        "mulhsu" => (funct3::MULHSU, funct7::MULDIV, opcodes::OP),
        "mulhu" => (funct3::MULHU, funct7::MULDIV, opcodes::OP),
        "div" => (funct3::DIV, funct7::MULDIV, opcodes::OP),
        "divu" => (funct3::DIVU, funct7::MULDIV, opcodes::OP),
        "rem" => (funct3::REM, funct7::MULDIV, opcodes::OP),
        "remu" => (funct3::REMU, funct7::MULDIV, opcodes::OP),

        _ => unreachable!("Handled by main match"), // Should not happen
    };

    Ok(encode_r_type_fields(opcode, funct3, funct7, rd, rs1, rs2))
}

fn encode_i_type(instr: &Instruction, symbol_table: &SymbolTable) -> Result<u32, AssemblerError> {
     let mnemonic = instr.mnemonic.to_lowercase();
    let (rd, rs1, imm_operand, opcode, expected_operands) = match mnemonic.as_str() {
        // ALU Immediate: rd, rs1, imm
        "addi" | "slti" | "sltiu" | "xori" | "ori" | "andi" | "slli" | "srli" | "srai" => {
            if instr.operands.len() != 3 { return Err(AssemblerError::SyntaxError { message: format!("Expected 3 operands (rd, rs1, imm) for {}", instr.mnemonic), line: instr.line, column: 0 }); }
            ( &instr.operands[0], &instr.operands[1], &instr.operands[2], opcodes::OP_IMM, 3)
        }
        // Load: rd, imm(rs1)
        "lb" | "lh" | "lw" | "lbu" | "lhu" => {
            if instr.operands.len() != 2 { return Err(AssemblerError::SyntaxError { message: format!("Expected 2 operands (rd, imm(rs1)) for {}", instr.mnemonic), line: instr.line, column: 0 }); }
            // Need special handling for imm(rs1) format
             let rd_op = &instr.operands