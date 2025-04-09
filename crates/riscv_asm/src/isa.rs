// //! Defines RISC-V (RV32IM) Instructions, registers, and encoding logic.

// use crate::error::{AssemblerError, SourceLocation};
// use crate::symbol::SymbolTable;
// use std::collections::HashMap;

// // --- Registers ---

// /// Represents a RISC-V general-purpose register.
// #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
// pub struct Register(u8); // Store the 5-bit register number

// impl Register {
//     /// Creates a register if the number is valid (0-31).
//     pub fn new(num: u8) -> Option<Self> {
//         if num < 32 { Some(Register(num)) } else { None }
//     }

//     /// Returns the 5-bit register number.
//     pub fn number(&self) -> u8 {
//         self.0
//     }

//     /// Parses a register name (e.g., "x5", "t0", "sp") into a Register.
//     pub fn from_name(name: &str) -> Option<Self> {
//         // Use a static map or match statement for ABI names
//         // This should be comprehensive
//         match name {
//             "zero" | "x0" => Some(Register(0)),
//             "ra"   | "x1" => Some(Register(1)),
//             "sp"   | "x2" => Some(Register(2)),
//             "gp"   | "x3" => Some(Register(3)),
//             "tp"   | "x4" => Some(Register(4)),
//             "t0"   | "x5" => Some(Register(5)),
//             "t1"   | "x6" => Some(Register(6)),
//             "t2"   | "x7" => Some(Register(7)),
//             "s0" | "fp" | "x8" => Some(Register(8)),
//             "s1"   | "x9" => Some(Register(9)),
//             "a0"   | "x10" => Some(Register(10)),
//             "a1"   | "x11" => Some(Register(11)),
//             "a2"   | "x12" => Some(Register(12)),
//             "a3"   | "x13" => Some(Register(13)),
//             "a4"   | "x14" => Some(Register(14)),
//             "a5"   | "x15" => Some(Register(15)),
//             "a6"   | "x16" => Some(Register(16)),
//             "a7"   | "x17" => Some(Register(17)),
//             "s2"   | "x18" => Some(Register(18)),
//             "s3"   | "x19" => Some(Register(19)),
//             "s4"   | "x20" => Some(Register(20)),
//             "s5"   | "x21" => Some(Register(21)),
//             "s6"   | "x22" => Some(Register(22)),
//             "s7"   | "x23" => Some(Register(23)),
//             "s8"   | "x24" => Some(Register(24)),
//             "s9"   | "x25" => Some(Register(25)),
//             "s10"  | "x26" => Some(Register(26)),
//             "s11"  | "x27" => Some(Register(27)),
//             "t3"   | "x28" => Some(Register(28)),
//             "t4"   | "x29" => Some(Register(29)),
//             "t5"   | "x30" => Some(Register(30)),
//             "t6"   | "x31" => Some(Register(31)),
//             _ => {
//                  // Try parsing 'x' followed by digits
//                  if name.starts_with('x') {
//                      name[1..].parse::<u8>().ok().and_then(Register::new)
//                  } else {
//                      None
//                  }
//             }
//         }
//     }
// }

// // --- Operands ---

// /// Represents different types of operands for instructions.
// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum Operand {
//     Register(Register),
//     Immediate(i64), // Use i64 to allow for range checks before casting
//     Label(String),  // Label name, resolved during Pass 2
// }

// // --- Instructions ---

// /// Enum representing parsed RISC-V instructions (RV32IM).
// /// Each variant holds the necessary operands.
// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum Instruction {
//     // R-Type (ALU) - funct7 | rs2 | rs1 | funct3 | rd | opcode
//     Add { rd: Register, rs1: Register, rs2: Register },
//     Sub { rd: Register, rs1: Register, rs2: Register },
//     Sll { rd: Register, rs1: Register, rs2: Register },
//     Slt { rd: Register, rs1: Register, rs2: Register },
//     Sltu{ rd: Register, rs1: Register, rs2: Register },
//     Xor { rd: Register, rs1: Register, rs2: Register },
//     Srl { rd: Register, rs1: Register, rs2: Register },
//     Sra { rd: Register, rs1: Register, rs2: Register },
//     Or  { rd: Register, rs1: Register, rs2: Register },
//     And { rd: Register, rs1: Register, rs2: Register },
//     // R-Type (M Extension)
//     Mul   { rd: Register, rs1: Register, rs2: Register },
//     Mulh  { rd: Register, rs1: Register, rs2: Register },
//     Mulhsu{ rd: Register, rs1: Register, rs2: Register },
//     Mulhu { rd: Register, rs1: Register, rs2: Register },
//     Div   { rd: Register, rs1: Register, rs2: Register },
//     Divu  { rd: Register, rs1: Register, rs2: Register },
//     Rem   { rd: Register, rs1: Register, rs2: Register },
//     Remu  { rd: Register, rs1: Register, rs2: Register },

//     // I-Type (ALU Immediate) - imm[11:0] | rs1 | funct3 | rd | opcode
//     Addi { rd: Register, rs1: Register, imm: Operand }, // imm can be Immediate or Label (low part)
//     Slti { rd: Register, rs1: Register, imm: Operand },
//     Sltiu{ rd: Register, rs1: Register, imm: Operand },
//     Xori { rd: Register, rs1: Register, imm: Operand },
//     Ori  { rd: Register, rs1: Register, imm: Operand },
//     Andi { rd: Register, rs1: Register, imm: Operand },
//     // I-Type (Shift Immediate) - funct7[11:5] | shamt[4:0] | rs1 | funct3 | rd | opcode
//     Slli { rd: Register, rs1: Register, shamt: Operand }, // shamt is Immediate (0-31)
//     Srli { rd: Register, rs1: Register, shamt: Operand },
//     Srai { rd: Register, rs1: Register, shamt: Operand },
//     // I-Type (Load) - imm[11:0] | rs1 | funct3 | rd | opcode
//     Lb   { rd: Register, rs1: Register, imm: Operand }, // Often imm(rs1) syntax
//     Lh   { rd: Register, rs1: Register, imm: Operand },
//     Lw   { rd: Register, rs1: Register, imm: Operand },
//     Lbu  { rd: Register, rs1: Register, imm: Operand },
//     Lhu  { rd: Register, rs1: Register, imm: Operand },
//     // I-Type (Jump) - imm[11:0] | rs1 | funct3 | rd | opcode
//     Jalr { rd: Register, rs1: Register, imm: Operand },

//     // S-Type (Store) - imm[11:5] | rs2 | rs1 | funct3 | imm[4:0] | opcode
//     Sb   { rs1: Register, rs2: Register, imm: Operand }, // Often imm(rs1) syntax
//     Sh   { rs1: Register, rs2: Register, imm: Operand },
//     Sw   { rs1: Register, rs2: Register, imm: Operand },

//     // B-Type (Branch) - imm[12|10:5] | rs2 | rs1 | funct3 | imm[4:1|11] | opcode
//     Beq  { rs1: Register, rs2: Register, target: Operand }, // Target is Label or Immediate offset
//     Bne  { rs1: Register, rs2: Register, target: Operand },
//     Blt  { rs1: Register, rs2: Register, target: Operand },
//     Bge  { rs1: Register, rs2: Register, target: Operand },
//     Bltu { rs1: Register, rs2: Register, target: Operand },
//     Bgeu { rs1: Register, rs2: Register, target: Operand },

//     // U-Type (Upper Immediate) - imm[31:12] | rd | opcode
//     Lui  { rd: Register, imm: Operand }, // Imm can be Immediate or Label (high part)
//     Auipc{ rd: Register, imm: Operand }, // Imm can be Immediate or Label (high part, PC-relative)

//     // J-Type (Jump) - imm[20|10:1|11|19:12] | rd | opcode
//     Jal  { rd: Register, target: Operand }, // Target is Label or Immediate offset
// }

// // --- Encoding Logic ---

// impl Instruction {
//     /// Encodes the instruction into its 32-bit binary representation.
//     /// Requires the symbol table and current program counter (PC) for resolving labels.
//     pub fn encode(
//         &self,
//         symbols: &SymbolTable,
//         current_pc: u32,
//         loc: &SourceLocation // Location for error reporting
//     ) -> Result<u32, AssemblerError> {
//         // Helper function to resolve label or immediate operand
//         let resolve_immediate = |op: &Operand, is_relative: bool, size_bits: u32| -> Result<i32, AssemblerError> {
//             match op {
//                 Operand::Immediate(imm) => {
//                      let max_val = (1i64 << (size_bits - 1)) - 1;
//                      let min_val = -(1i64 << (size_bits - 1));
//                      if *imm >= min_val && *imm <= max_val {
//                         Ok(*imm as i32)
//                      } else {
//                          Err(AssemblerError::EncodingError {
//                              message: format!("Immediate value {} out of range for {}-bit signed field", imm, size_bits),
//                              loc: loc.clone(),
//                          })
//                      }
//                 },
//                 Operand::Label(name) => {
//                     symbols.get(name)
//                         .map(|target_addr| {
//                             if is_relative {
//                                 // PC-relative offset calculation
//                                 let offset = (*target_addr as i64) - (current_pc as i64);
//                                 // Check offset range for branches (B/J types often have constraints)
//                                 // For now, cast and rely on bit extraction logic to handle encoding ranges
//                                 offset as i32
//                             } else {
//                                 // Absolute address (or part of it for LUI/AUIPC)
//                                 *target_addr as i32 // Cast needed, handle potential truncation later if needed
//                             }
//                         })
//                         .ok_or_else(|| AssemblerError::SymbolError {
//                             message: format!("Undefined label: {}", name),
//                             loc: loc.clone(), // Use instruction's location
//                         })
//                 }
//                 Operand::Register(_) => Err(AssemblerError::EncodingError{
//                     message: "Expected immediate or label, found register".to_string(),
//                     loc: loc.clone(),
//                 }),
//             }
//         };

//          // Helper function to resolve shamt operand (must be immediate 0-31)
//          let resolve_shamt = |op: &Operand| -> Result<u32, AssemblerError> {
//              match op {
//                  Operand::Immediate(imm) => {
//                      if *imm >= 0 && *imm < 32 {
//                          Ok(*imm as u32)
//                      } else {
//                          Err(AssemblerError::EncodingError {
//                              message: format!("Shift amount {} out of range [0, 31]", imm),
//                              loc: loc.clone(),
//                          })
//                      }
//                  },
//                  _ => Err(AssemblerError::EncodingError{
//                     message: "Expected immediate shift amount".to_string(),
//                     loc: loc.clone(),
//                  }),
//              }
//          };

//         // --- Define Opcodes and Funct3/Funct7 values ---
//         const OP_LUI: u32    = 0b0110111;
//         const OP_AUIPC: u32  = 0b0010111;
//         const OP_JAL: u32    = 0b1101111;
//         const OP_JALR: u32   = 0b1100111;
//         const OP_BRANCH: u32 = 0b1100011;
//         const OP_LOAD: u32   = 0b0000011;
//         const OP_STORE: u32  = 0b0100011;
//         const OP_IMM: u32    = 0b0010011; // Immediate ALU ops
//         const OP_REG: u32    = 0b0110011; // Register ALU ops
//         // M Extension uses OP_REG
//         const FUNCT7_MULDIV: u32 = 0b0000001;
//         const FUNCT7_SUB_SRA: u32 = 0b0100000; // Also used for SRAI
//         const FUNCT7_ZERO: u32   = 0b0000000; // Default for many

//         // --- Encoding based on instruction type ---
//         let encoding = match *self {
//             // --- R-Type ---
//             Instruction::Add { rd, rs1, rs2 } => encode_r(FUNCT7_ZERO, rs2, rs1, 0b000, rd, OP_REG),
//             Instruction::Sub { rd, rs1, rs2 } => encode_r(FUNCT7_SUB_SRA, rs2, rs1, 0b000, rd, OP_REG),
//             Instruction::Sll { rd, rs1, rs2 } => encode_r(FUNCT7_ZERO, rs2, rs1, 0b001, rd, OP_REG),
//             Instruction::Slt { rd, rs1, rs2 } => encode_r(FUNCT7_ZERO, rs2, rs1, 0b010, rd, OP_REG),
//             Instruction::Sltu{ rd, rs1, rs2 } => encode_r(FUNCT7_ZERO, rs2, rs1, 0b011, rd, OP_REG),
//             Instruction::Xor { rd, rs1, rs2 } => encode_r(FUNCT7_ZERO, rs2, rs1, 0b100, rd, OP_REG),
//             Instruction::Srl { rd, rs1, rs2 } => encode_r(FUNCT7_ZERO, rs2, rs1, 0b101, rd, OP_REG),
//             Instruction::Sra { rd, rs1, rs2 } => encode_r(FUNCT7_SUB_SRA, rs2, rs1, 0b101, rd, OP_REG),
//             Instruction::Or  { rd, rs1, rs2 } => encode_r(FUNCT7_ZERO, rs2, rs1, 0b110, rd, OP_REG),
//             Instruction::And { rd, rs1, rs2 } => encode_r(FUNCT7_ZERO, rs2, rs1, 0b111, rd, OP_REG),
//             // M Extension (R-Type)
//             Instruction::Mul   { rd, rs1, rs2 } => encode_r(FUNCT7_MULDIV, rs2, rs1, 0b000, rd, OP_REG),
//             Instruction::Mulh  { rd, rs1, rs2 } => encode_r(FUNCT7_MULDIV, rs2, rs1, 0b001, rd, OP_REG),
//             Instruction::Mulhsu{ rd, rs1, rs2 } => encode_r(FUNCT7_MULDIV, rs2, rs1, 0b010, rd, OP_REG),
//             Instruction::Mulhu { rd, rs1, rs2 } => encode_r(FUNCT7_MULDIV, rs2, rs1, 0b011, rd, OP_REG),
//             Instruction::Div   { rd, rs1, rs2 } => encode_r(FUNCT7_MULDIV, rs2, rs1, 0b100, rd, OP_REG),
//             Instruction::Divu  { rd, rs1, rs2 } => encode_r(FUNCT7_MULDIV, rs2, rs1, 0b101, rd, OP_REG),
//             Instruction::Rem   { rd, rs1, rs2 } => encode_r(FUNCT7_MULDIV, rs2, rs1, 0b110, rd, OP_REG),
//             Instruction::Remu  { rd, rs1, rs2 } => encode_r(FUNCT7_MULDIV, rs2, rs1, 0b111, rd, OP_REG),

//             // --- I-Type (ALU / Load / JALR) ---
//             Instruction::Addi { rd, rs1, ref imm } => encode_i(resolve_immediate(imm, false, 12)?, rs1, 0b000, rd, OP_IMM),
//             Instruction::Slti { rd, rs1, ref imm } => encode_i(resolve_immediate(imm, false, 12)?, rs1, 0b010, rd, OP_IMM),
//             Instruction::Sltiu{ rd, rs1, ref imm } => encode_i(resolve_immediate(imm, false, 12)?, rs1, 0b011, rd, OP_IMM), // Imm is signed 12-bit, but comparison is unsigned
//             Instruction::Xori { rd, rs1, ref imm } => encode_i(resolve_immediate(imm, false, 12)?, rs1, 0b100, rd, OP_IMM),
//             Instruction::Ori  { rd, rs1, ref imm } => encode_i(resolve_immediate(imm, false, 12)?, rs1, 0b110, rd, OP_IMM),
//             Instruction::Andi { rd, rs1, ref imm } => encode_i(resolve_immediate(imm, false, 12)?, rs1, 0b111, rd, OP_IMM),
//             Instruction::Lb   { rd, rs1, ref imm } => encode_i(resolve_immediate(imm, false, 12)?, rs1, 0b000, rd, OP_LOAD),
//             Instruction::Lh   { rd, rs1, ref imm } => encode_i(resolve_immediate(imm, false, 12)?, rs1, 0b001, rd, OP_LOAD),
//             Instruction::Lw   { rd, rs1, ref imm } => encode_i(resolve_immediate(imm, false, 12)?, rs1, 0b010, rd, OP_LOAD),
//             Instruction::Lbu  { rd, rs1, ref imm } => encode_i(resolve_immediate(imm, false, 12)?, rs1, 0b100, rd, OP_LOAD),
//             Instruction::Lhu  { rd, rs1, ref imm } => encode_i(resolve_immediate(imm, false, 12)?, rs1, 0b101, rd, OP_LOAD),
//             Instruction::Jalr { rd, rs1, ref imm } => encode_i(resolve_immediate(imm, false, 12)?, rs1, 0b000, rd, OP_JALR),
//             // I-Type (Shift) - special handling for shamt encoding
//             Instruction::Slli { rd, rs1, ref shamt } => encode_i_shamt(FUNCT7_ZERO, resolve_shamt(shamt)?, rs1, 0b001, rd, OP_IMM),
//             Instruction::Srli { rd, rs1, ref shamt } => encode_i_shamt(FUNCT7_ZERO, resolve_shamt(shamt)?, rs1, 0b101, rd, OP_IMM),
//             Instruction::Srai { rd, rs1, ref shamt } => encode_i_shamt(FUNCT7_SUB_SRA, resolve_shamt(shamt)?, rs1, 0b101, rd, OP_IMM),

//              // --- S-Type ---
//              Instruction::Sb { rs1, rs2, ref imm } => encode_s(resolve_immediate(imm, false, 12)?, rs2, rs1, 0b000, OP_STORE),
//              Instruction::Sh { rs1, rs2, ref imm } => encode_s(resolve_immediate(imm, false, 12)?, rs2, rs1, 0b001, OP_STORE),
//              Instruction::Sw { rs1, rs2, ref imm } => encode_s(resolve_immediate(imm, false, 12)?, rs2, rs1, 0b010, OP_STORE),

//              // --- B-Type ---
//              Instruction::Beq { rs1, rs2, ref target } => encode_b(resolve_immediate(target, true, 13)?, rs2, rs1, 0b000, OP_BRANCH),
//              Instruction::Bne { rs1, rs2, ref target } => encode_b(resolve_immediate(target, true, 13)?, rs2, rs1, 0b001, OP_BRANCH),
//              Instruction::Blt { rs1, rs2, ref target } => encode_b(resolve_immediate(target, true, 13)?, rs2, rs1, 0b100, OP_BRANCH),
//              Instruction::Bge { rs1, rs2, ref target } => encode_b(resolve_immediate(target, true, 13)?, rs2, rs1, 0b101, OP_BRANCH),
//              Instruction::Bltu{ rs1, rs2, ref target } => encode_b(resolve_immediate(target, true, 13)?, rs2, rs1, 0b110, OP_BRANCH),
//              Instruction::Bgeu{ rs1, rs2, ref target } => encode_b(resolve_immediate(target, true, 13)?, rs2, rs1, 0b111, OP_BRANCH),

//              // --- U-Type ---
//              Instruction::Lui  { rd, ref imm } => encode_u(resolve_immediate(imm, false, 20)?, rd, OP_LUI), // Imm needs upper 20 bits
//              Instruction::Auipc{ rd, ref imm } => encode_u(resolve_immediate(imm, true, 20)?, rd, OP_AUIPC), // Imm needs upper 20 bits of offset

//              // --- J-Type ---
//              Instruction::Jal { rd, ref target } => encode_j(resolve_immediate(target, true, 21)?, rd, OP_JAL),
//         };

//         Ok(encoding?) // Propagate potential errors from resolve_* and encode_* helpers
//     }
// }

// // --- Encoding Helper Functions ---
// // These functions perform the bit manipulation according to the RISC-V specification.

// #[inline]
// fn encode_r(funct7: u32, rs2: Register, rs1: Register, funct3: u32, rd: Register, opcode: u32) -> Result<u32, AssemblerError> {
//     Ok(((funct7 & 0x7F) << 25)  // 7 bits
//         | ((rs2.number() as u32 & 0x1F) << 20) // 5 bits
//         | ((rs1.number() as u32 & 0x1F) << 15) // 5 bits
//         | ((funct3 & 0x07) << 12) // 3 bits
//         | ((rd.number() as u32 & 0x1F) << 7)   // 5 bits
//         | (opcode & 0x7F))      // 7 bits
// }

// #[inline]
// fn encode_i(imm: i32, rs1: Register, funct3: u32, rd: Register, opcode: u32) -> Result<u32, AssemblerError> {
//      // Ensure immediate fits in 12 bits signed
//      if imm < -(1 << 11) || imm >= (1 << 11) {
//         return Err(AssemblerError::EncodingError {
//             message: format!("Immediate {} out of range for I-type instruction (-2048 to 2047)", imm),
//             loc: Default::default(), // TODO: Pass location down
//         });
//      }
//     Ok(((imm as u32 & 0xFFF) << 20)        // 12 bits (sign extension handled by cast if imm is neg)
//         | ((rs1.number() as u32 & 0x1F) << 15) // 5 bits
//         | ((funct3 & 0x07) << 12)           // 3 bits
//         | ((rd.number() as u32 & 0x1F) << 7)   // 5 bits
//         | (opcode & 0x7F))                  // 7 bits
// }

// #[inline]
// fn encode_i_shamt(funct7: u32, shamt: u32, rs1: Register, funct3: u32, rd: Register, opcode: u32) -> Result<u32, AssemblerError> {
//     // shamt is only 5 bits for RV32I
//      if shamt >= 32 {
//          return Err(AssemblerError::EncodingError {
//              message: format!("Shift amount {} must be < 32 for RV32I", shamt),
//              loc: Default::default(), // TODO: Pass location
//          });
//      }
//     Ok(((funct7 & 0x7F) << 25)  // Use funct7 field for SRAI/etc.
//         | ((shamt & 0x1F) << 20)                // 5 bits shamt
//         | ((rs1.number() as u32 & 0x1F) << 15) // 5 bits
//         | ((funct3 & 0x07) << 12)           // 3 bits
//         | ((rd.number() as u32 & 0x1F) << 7)   // 5 bits
//         | (opcode & 0x7F))                  // 7 bits
// }

// #[inline]
// fn encode_s(imm: i32, rs2: Register, rs1: Register, funct3: u32, opcode: u32) -> Result<u32, AssemblerError> {
//      // Ensure immediate fits in 12 bits signed
//      if imm < -(1 << 11) || imm >= (1 << 11) {
//         return Err(AssemblerError::EncodingError {
//             message: format!("Immediate {} out of range for S-type instruction (-2048 to 2047)", imm),
//             loc: Default::default(), // TODO: Pass location
//         });
//      }
//     let imm_u = imm as u32;
//     let imm11_5 = (imm_u >> 5) & 0x7F; // 7 bits
//     let imm4_0 = imm_u & 0x1F;         // 5 bits
//     Ok(((imm11_5) << 25)
//         | ((rs2.number() as u32 & 0x1F) << 20) // 5 bits
//         | ((rs1.number() as u32 & 0x1F) << 15) // 5 bits
//         | ((funct3 & 0x07) << 12)           // 3 bits
//         | ((imm4_0) << 7)                   // 5 bits
//         | (opcode & 0x7F))                  // 7 bits
// }

// #[inline]
// fn encode_b(imm: i32, rs2: Register, rs1: Register, funct3: u32, opcode: u32) -> Result<u32, AssemblerError> {
//      // B-type immediate is 13 bits signed, multiples of 2. LSB is always 0.
//      // Range: -4096 to +4094
//      if imm % 2 != 0 {
//          return Err(AssemblerError::EncodingError {
//              message: format!("Branch offset {} must be a multiple of 2", imm),
//              loc: Default::default(), // TODO: Pass location
//          });
//      }
//       if imm < -(1 << 12) || imm >= (1 << 12) {
//         return Err(AssemblerError::EncodingError {
//             message: format!("Branch offset {} out of range for B-type instruction (-4096 to 4094)", imm),
//             loc: Default::default(), // TODO: Pass location
//         });
//      }

//     let imm_u = imm as u32;
//     let imm12   = (imm_u >> 12) & 0x1; // bit 12
//     let imm10_5 = (imm_u >> 5) & 0x3F; // bits 10:5
//     let imm4_1  = (imm_u >> 1) & 0xF;  // bits 4:1
//     let imm11   = (imm_u >> 11) & 0x1; // bit 11

//     Ok(((imm12) << 31)                     // imm[12]
//         | ((imm10_5) << 25)                // imm[10:5]
//         | ((rs2.number() as u32 & 0x1F) << 20) // rs2
//         | ((rs1.number() as u32 & 0x1F) << 15) // rs1
//         | ((funct3 & 0x07) << 12)           // funct3
//         | ((imm4_1) << 8)                  // imm[4:1]
//         | ((imm11) << 7)                   // imm[11]
//         | (opcode & 0x7F))                 // opcode
// }

// #[inline]
// fn encode_u(imm: i32, rd: Register, opcode: u32) -> Result<u32, AssemblerError> {
//     // U-type immediate takes upper 20 bits. Check if lower 12 bits are zero if using absolute value?
//     // Or just mask? The spec says imm[31:12]. Let's just mask.
//     // For LUI, the immediate is used directly. For AUIPC, it's an offset.
//     // Range check on the value *before* shifting might be needed depending on assembler directives (%hi/%lo)
//     let imm_u = imm as u32;
//     Ok(((imm_u & 0xFFFFF000)) // imm[31:12] - already shifted correctly if imm is full 32-bit value/offset
//         | ((rd.number() as u32 & 0x1F) << 7)   // rd
//         | (opcode & 0x7F))                  // opcode
// }

// #[inline]
// fn encode_j(imm: i32, rd: Register, opcode: u32) -> Result<u32, AssemblerError> {
//     // J-type immediate is 21 bits signed, multiples of 2. LSB is always 0.
//     // Range: -1 MiB to +1 MiB (approx)
//       if imm % 2 != 0 {
//          return Err(AssemblerError::EncodingError {
//              message: format!("JAL offset {} must be a multiple of 2", imm),
//              loc: Default::default(), // TODO: Pass location
//          });
//      }
//       if imm < -(1 << 20) || imm >= (1 << 20) {
//         return Err(AssemblerError::EncodingError {
//             message: format!("JAL offset {} out of range for J-type instruction (-1048576 to 1048574)", imm),
//             loc: Default::default(), // TODO: Pass location
//         });
//      }

//     let imm_u = imm as u32;
//     let imm20   = (imm_u >> 20) & 0x1;  // bit 20
//     let imm10_1 = (imm_u >> 1) & 0x3FF; // bits 10:1
//     let imm11   = (imm_u >> 11) & 0x1;  // bit 11
//     let imm19_12= (imm_u >> 12) & 0xFF; // bits 19:12

//     Ok(((imm20) << 31)                     // imm[20]
//         | ((imm10_1) << 21)                // imm[10:1]
//         | ((imm11) << 20)                  // imm[11]
//         | ((imm19_12) << 12)               // imm[19:12]
//         | ((rd.number() as u32 & 0x1F) << 7) // rd
//         | (opcode & 0x7F))                 // opcode
// }

// // --- Unit Tests ---
// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::symbol::SymbolTable; // Needed for context

//     #[test]
//     fn test_register_parsing() {
//         assert_eq!(Register::from_name("x0").unwrap().number(), 0);
//         assert_eq!(Register::from_name("zero").unwrap().number(), 0);
//         assert_eq!(Register::from_name("sp").unwrap().number(), 2);
//         assert_eq!(Register::from_name("x31").unwrap().number(), 31);
//         assert_eq!(Register::from_name("t6").unwrap().number(), 31);
//         assert!(Register::from_name("x32").is_none());
//         assert!(Register::from_name("abc").is_none());
//     }

//     #[test]
//     fn test_encode_add() {
//         // add x3, x1, x2 => funct7=0, rs2=2, rs1=1, funct3=0, rd=3, opcode=0x33
//         // 0000000 00010 00001 000 00011 0110011
//         // 0x002081B3
//         let inst = Instruction::Add { rd: Register(3), rs1: Register(1), rs2: Register(2) };
//         let symbols = SymbolTable::new();
//         let encoded = inst.encode(&symbols, 0, &Default::default()).unwrap();
//         assert_eq!(encoded, 0x002081B3);
//     }

//      #[test]
//     fn test_encode_addi() {
//         // addi x5, x6, -10 => imm=-10 (0xFFFFFFF6), rs1=6, funct3=0, rd=5, opcode=0x13
//         // 111111110110 00110 000 00101 0010011
//         // 0xFF630293
//         let inst = Instruction::Addi { rd: Register(5), rs1: Register(6), imm: Operand::Immediate(-10) };
//         let symbols = SymbolTable::new();
//         let encoded = inst.encode(&symbols, 0, &Default::default()).unwrap();
//         assert_eq!(encoded, 0xFF630293);
//     }

//     #[test]
//     fn test_encode_lw() {
//         // lw x10, 16(x2) => imm=16 (0x10), rs1=2(sp), funct3=2, rd=10(a0), opcode=0x03
//         // 000000010000 00010 010 01010 0000011
//         // 0x01012503
//         let inst = Instruction::Lw { rd: Register(10), rs1: Register(2), imm: Operand::Immediate(16) };
//         let symbols = SymbolTable::new();
//         let encoded = inst.encode(&symbols, 0, &Default::default()).unwrap();
//         assert_eq!(encoded, 0x01012503);
//     }

//      #[test]
//     fn test_encode_sw() {
//         // sw x12, -20(x8) => imm=-20 (0xFFFFFFEC), rs2=12(a2), rs1=8(s0/fp), funct3=2, opcode=0x23
//         // imm[11:5] = 1111111 (-4 -> 0x7C)
//         // imm[4:0]  = 11100   (12 -> 0x1C)
//         // 1111111 01100 01000 010 11100 0100011
//         // 0xFECA8E23
//         let inst = Instruction::Sw { rs1: Register(8), rs2: Register(12), imm: Operand::Immediate(-20) };
//         let symbols = SymbolTable::new();
//         let encoded = inst.encode(&symbols, 0, &Default::default()).unwrap();
//         assert_eq!(encoded, 0xFECA8E23);
//     }

//     // #[test]
//     // fn test_encode_beq_forward() {
//     //     // beq x1, x2, target  ; assume PC=0x100, target=0x120 => offset = 0x20 (32)
//     //     // offset = 32 = 0b100000
//     //     // imm[12]=0, imm[10:5]=0b01000, imm[4:1]=0b0000, imm[11]=0
//     //     // 0 01000 00010 00001 000 0000 0 1100011
//     //     // 00100000 001000001000000001100011
//     //     // 0x04208063 -> Check calculation carefully. Let's use online encoder.
//     //     // Online encoder gives: 0x00208063 for beq x1, x2, +32
//     //     // Let's re-evaluate: imm=32=0x20
//     //     // imm[12]=0 (bit 12 is 0)
//     //     // imm[10:5]=0b010000 (bits 10..5 are 10000) -> No, bits 10:5 of 0x20 = 0b010000 -> correct = 0b000001 << 5? = 0x20?
//     //     // imm[4:1]=0b0000 (bits 4..1 are 0000)
//     //     // imm[11]=0 (bit 11 is 0)
//     //     // imm12=0; imm10_5=1; imm4_1=0; imm11=0;
//     //     // B-Format: imm[12|10:5] | rs2 | rs1 | funct3 | imm[4:1|11] | opcode
//     //     // imm[12]=0 << 31 -> 0
//     //     // imm[10:5]=1 << 25 -> 0x02000000
//     //     // rs2=2 << 20 -> 0x00200000
//     //     // rs1=1 << 15 -> 0x00008000
//     //     // funct3=0 << 12 -> 0
//     //     // imm[4:1]=0 << 8 -> 0
//     //     // imm[11]=0 << 7 ->
// }
