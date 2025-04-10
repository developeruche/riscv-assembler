//! RISC-V Assembler CLI Tool
//!
//! This binary provides a command-line interface to the RISC-V assembler library.
//! It takes an assembly file as input and produces a binary file with the machine code.
use std::path::PathBuf;
use std::process;

use anyhow::{Context, Result};
use clap::Parser;
use riscv_asm::{
    assembler::assemble,
    utils::{OutputFormat, read_file, write_output},
};

/// RISC-V Assembler CLI
#[derive(Parser, Debug)]
#[clap(author, version, about)]
struct Args {
    /// Input assembly file path
    #[clap(index = 1)]
    input: PathBuf,

    /// Output file path (defaults to input name with .bin extension)
    #[clap(short, long)]
    output: Option<PathBuf>,

    /// Output format
    #[clap(short, long, value_enum, default_value = "binary")]
    format: OutputFormat,

    /// Verbose output
    #[clap(short, long)]
    verbose: bool,
}

fn main() {
    if let Err(err) = run() {
        eprintln!("Error: {}", err);
        process::exit(1);
    }
}

fn run() -> Result<()> {
    // Parse command line arguments
    let args = Args::parse();

    // Read the input file
    let input_path = &args.input;
    let source = read_file(input_path)
        .with_context(|| format!("Failed to read input file: {}", input_path.display()))?;

    if args.verbose {
        println!("Assembling file: {}", input_path.display());
    }

    // Assemble the source code
    let assembled = assemble(&source).map_err(|e| anyhow::anyhow!("Assembly error: {}", e))?;

    println!("Assembled Code: {:?}", assembled.code);

    if args.verbose {
        println!(
            "Assembly successful. {} bytes of machine code generated.",
            assembled.size
        );
    }

    // Determine the output path
    let output_path = args.output.unwrap_or_else(|| {
        let mut output = input_path.clone();
        output.set_extension(match args.format {
            OutputFormat::Binary => "bin",
            OutputFormat::Hex => "hex",
            OutputFormat::Text => "txt",
        });
        output
    });

    // Write the assembled output
    write_output(&assembled, &output_path, args.format)
        .with_context(|| format!("Failed to write output to: {}", output_path.display()))?;

    if args.verbose {
        println!("Output written to: {}", output_path.display());
    }

    Ok(())
}
