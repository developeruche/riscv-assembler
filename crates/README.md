# riscv_asm lib
A RISC-V (RV32IM) assembler library written in Rust.

## Overview

`riscv_asm` is a library for assembling RISC-V assembly language (RV32IM) into machine code. It provides a comprehensive set of features for parsing, analyzing, and encoding RISC-V assembly instructions and directives.

## Features

- Support for the entire RV32I base instruction set
- Support for the RV32M extension (multiplication and division)
- Handles common assembler directives (`.word`, `.byte`, `.align`, etc.)
- Two-pass assembly with symbol resolution
- Detailed error reporting with line and column information
- Memory address tracking and alignment

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
riscv_asm = "0.1.0"
```

### Example

```rust
use riscv_asm::assembler::assemble;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Simple RISC-V assembly program
    let source = r#"
        .text
    main:
        addi x1, x0, 10      # x1 = 10
        addi x2, x0, 20      # x2 = 20
        add x3, x1, x2       # x3 = x1 + x2 (30)
    "#;

    // Assemble the source code
    let output = assemble(source)?;
    
    println!("Generated {} bytes of machine code", output.size);
    println!("Starting address: 0x{:08x}", output.start_address);
    
    // Print each instruction
    for (i, word) in output.code.iter().enumerate() {
        println!("Instruction {}: 0x{:08x}", i, word);
    }
    
    Ok(())
}
```

## API Documentation

The library has several main components:

- `assembler`: The core assembling functionality
- `lexer`: Tokenizes assembly source code
- `parser`: Parses tokens into an intermediate representation
- `isa`: Defines the RISC-V instruction set architecture
- `symbol`: Manages symbol tables for label resolution
- `error`: Defines error types for detailed reporting

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.