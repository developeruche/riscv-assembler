# RISC-V Assembler

A Rust implementation of a RISC-V (RV32IM) assembler with both a library and CLI tool.

## Overview

This workspace contains:

1. A RISC-V assembler library (`riscv_asm`) that provides core functionality for assembling RISC-V code
2. A command-line interface (`cli`) for using the assembler with files

## Features

- Support for the complete RV32I base instruction set
- Support for the RV32M extension (multiplication and division instructions)
- Common assembler directives (`.word`, `.byte`, `.align`, etc.)
- Symbol table with forward reference resolution
- Multiple output formats (binary, Intel HEX, and text)
- Detailed error reporting

## Repository Structure

```
riscv-assembler/
├── crates/
│   └── riscv_asm/     # Core assembler library
│       ├── src/       # Library source code
│       └── Cargo.toml # Library manifest
├── bins/
│   └── cli/           # Command-line interface
│       ├── src/       # CLI source code
│       └── Cargo.toml # CLI manifest
└── Cargo.toml         # Workspace manifest
```

## Getting Started

### Prerequisites

- Rust toolchain (1.70.0 or newer)

### Building from Source

Clone this repository and build using Cargo:

```bash
git clone https://github.com/developeruche/riscv-assembler.git
cd riscv-assembler
cargo build --release
```

The CLI binary will be available at `target/release/cli`.

### Using the Library

To use the `riscv_asm` library in your own project, add it to your `Cargo.toml`:

```toml
[dependencies]
riscv_asm = { git = "https://github.com/developeruche/riscv-assembler.git" }
```

## Example Usage

### CLI Tool

```bash
# Assemble a file
cli input.s -o output.bin

# Generate Intel HEX format
cli input.s --format hex

# Show verbose output
cli input.s --verbose
```

### Library

```rust
use riscv_asm::assembler::assemble;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
        .text
        addi x1, x0, 42
    "#;

    let output = assemble(source)?;
    println!("Generated {} bytes of machine code", output.size);
    
    Ok(())
}
```

## Documentation

- For detailed library documentation, check the [library README](crates/riscv_asm/README.md)
- For CLI usage information, see the [CLI README](bins/cli/README.md)

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.