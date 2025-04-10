use crate::OutputFormat;
use riscv_asm::assembler::AssemblyOutput;
use std::fs::File;
use std::io::{self, Read, Write};
use std::path::Path;

/// Read a file into a string
pub(crate) fn read_file(path: &Path) -> io::Result<String> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

/// Write the assembled output to a file in the specified format
pub(crate) fn write_output(
    assembled: &AssemblyOutput,
    path: &Path,
    format: OutputFormat,
) -> io::Result<()> {
    match format {
        OutputFormat::Binary => write_binary_output(assembled, path),
        OutputFormat::Hex => write_hex_output(assembled, path),
        OutputFormat::Text => write_text_output(assembled, path),
    }
}

/// Write raw binary output
pub(crate) fn write_binary_output(assembled: &AssemblyOutput, path: &Path) -> io::Result<()> {
    let mut file = File::create(path)?;

    // Convert each 32-bit word to bytes (little-endian)
    for word in &assembled.code {
        file.write_all(&word.to_le_bytes())?;
    }

    Ok(())
}

/// Write output in Intel HEX format
pub(crate) fn write_hex_output(assembled: &AssemblyOutput, path: &Path) -> io::Result<()> {
    let mut file = File::create(path)?;

    let mut address = assembled.start_address;

    // Write data records (16 bytes per line)
    for chunk in assembled.code.chunks(4) {
        // 4 words = 16 bytes
        let mut line = Vec::new();
        for word in chunk {
            line.extend_from_slice(&word.to_le_bytes());
        }

        // Only write non-empty lines
        if !line.is_empty() {
            // Calculate checksum and format the Intel HEX record
            let mut checksum = line.len() as u8; // Byte count
            checksum = checksum.wrapping_add((address >> 8) as u8); // Address high byte
            checksum = checksum.wrapping_add(address as u8); // Address low byte

            // Write record mark, byte count, address, and record type
            write!(file, ":{:02X}{:04X}00", line.len(), address)?;

            // Write data bytes
            for byte in &line {
                write!(file, "{:02X}", byte)?;
                checksum = checksum.wrapping_add(*byte);
            }

            // Write checksum (2's complement)
            write!(file, "{:02X}\n", (0u8).wrapping_sub(checksum))?;
        }

        // Update address
        address += (chunk.len() * 4) as u32;
    }

    // Write end of file record
    writeln!(file, ":00000001FF")?;

    Ok(())
}

/// Write output as text (one instruction per line)
pub(crate) fn write_text_output(assembled: &AssemblyOutput, path: &Path) -> io::Result<()> {
    let mut file = File::create(path)?;

    let mut address = assembled.start_address;

    for word in &assembled.code {
        writeln!(file, "{:08X}: {:08X}", address, word)?;
        address += 4; // Each instruction is 4 bytes
    }

    Ok(())
}
