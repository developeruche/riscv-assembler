# RISC-V Assembler CLI

A command-line interface tool for the RISC-V assembler library.

## Overview

This CLI tool provides a convenient way to assemble RISC-V (RV32IM) assembly code from files and generate binary output. It leverages the `riscv_asm` library to perform the actual assembly process.

## Features

- Assemble RISC-V assembly files to machine code
- Multiple output formats:
  - Raw binary format
  - Intel HEX format
  - Text representation (with addresses and instruction hex)
- Automatic output file naming
- Verbose mode for detailed information
- Helpful error messages with context

## Installation

### From Crates.io

```
cargo install riscv-assembler-cli
```

### From Source

Clone the repository and build with Cargo:

```bash
git clone https://github.com/developeruche/riscv-assembler.git
cd riscv-assembler
cargo build --release
```

The binary will be available at `target/release/cli`.

## Usage

```
# Basic usage - outputs to input.bin
cli input.s

# Specify output file
cli input.s -o output.bin

# Choose output format
cli input.s --format hex

# Get verbose output
cli input.s --verbose

# Show help
cli --help
```

### Command-line Options

```
USAGE:
    cli [OPTIONS] <INPUT>

ARGS:
    <INPUT>    Input assembly file path

OPTIONS:
    -o, --output <OUTPUT>    Output file path (defaults to input name with appropriate extension)
    -f, --format <FORMAT>    Output format [default: binary] [possible values: binary, hex, text]
    -v, --verbose            Verbose output
    -h, --help               Print help information
    -V, --version            Print version information
```

### Examples

#### Assembling a Simple Program

Create a file named `example.s`:

```assembly
# Simple program that adds two numbers
.text
main:
    addi x1, x0, 10      # x1 = 10
    addi x2, x0, 20      # x2 = 20
    add  x3, x1, x2      # x3 = x1 + x2
```

Assemble it:

```bash
cli example.s -v
```

This will generate `example.bin` with the machine code, and print verbose information about the assembly process.

## License

This project is licensed under the MIT License - see the LICENSE file for details.