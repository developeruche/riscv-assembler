use crate::assembler::AssemblyOutput;
use clap::ValueEnum;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Read, Write};
use std::path::Path;

/// Output format for the assembled binary
#[derive(Debug, Clone, Copy, ValueEnum)]
pub enum OutputFormat {
    /// Raw binary format (just the bytes)
    Binary,
    /// Intel HEX format
    Hex,
    /// Text format (one instruction per line in hex)
    Text,
}

/// Read assembled output from a file in the specified format
pub fn read_output(path: &Path, format: OutputFormat) -> io::Result<AssemblyOutput> {
    match format {
        OutputFormat::Binary => read_binary_output(path),
        OutputFormat::Hex => read_hex_output(path),
        OutputFormat::Text => read_text_output(path),
    }
}

/// Read raw binary output format
pub fn read_binary_output(path: &Path) -> io::Result<AssemblyOutput> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    // Check that buffer length is a multiple of 4 bytes (32-bit words)
    if buffer.len() % 4 != 0 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Binary file size is not a multiple of 4 bytes",
        ));
    }

    // Convert bytes to u32 words (little-endian)
    let mut code = Vec::with_capacity(buffer.len() / 4);
    for chunk in buffer.chunks(4) {
        let word = u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
        code.push(word);
    }

    Ok(AssemblyOutput {
        code,
        size: buffer.len(),
        start_address: 0, // Binary format doesn't preserve start address
    })
}

/// Read Intel HEX format output
pub fn read_hex_output(path: &Path) -> io::Result<AssemblyOutput> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let mut bytes = Vec::new();
    let mut start_address = None;

    for line_result in reader.lines() {
        let line = line_result?;
        if !line.starts_with(':') {
            continue; // Skip invalid lines
        }

        // Skip the colon
        let hex_data = &line[1..];

        // Check for end of file record
        if hex_data == "00000001FF" {
            break;
        }

        // Parse record
        let count = usize::from_str_radix(&hex_data[0..2], 16)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        let address = u32::from_str_radix(&hex_data[2..6], 16)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        // Record the start address from the first data record
        if start_address.is_none() {
            start_address = Some(address);
        }

        // Skip the record type (always 00 for data records)
        let data_start = 8;

        // Extract data bytes
        for i in 0..count {
            let pos = data_start + i * 2;
            if pos + 2 > hex_data.len() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Record too short for declared byte count",
                ));
            }

            let byte = u8::from_str_radix(&hex_data[pos..pos + 2], 16)
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
            bytes.push(byte);
        }

        // Note: We don't validate checksums for simplicity, but could add that
    }

    // Convert bytes to 32-bit words
    let mut code = Vec::with_capacity((bytes.len() + 3) / 4); // Ceiling division

    for chunk in bytes.chunks(4) {
        let mut word_bytes = [0u8; 4];
        for (i, &byte) in chunk.iter().enumerate() {
            word_bytes[i] = byte;
        }
        code.push(u32::from_le_bytes(word_bytes));
    }

    Ok(AssemblyOutput {
        code,
        size: bytes.len(),
        start_address: start_address.unwrap_or(0),
    })
}

/// Read text format output (one instruction per line)
pub fn read_text_output(path: &Path) -> io::Result<AssemblyOutput> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let mut code = Vec::new();
    let mut start_address = None;

    for line_result in reader.lines() {
        let line = line_result?;

        // Parse line in format "AAAAAAAA: WWWWWWWW"
        let parts: Vec<&str> = line.split(':').collect();
        if parts.len() != 2 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Invalid text format line: {}", line),
            ));
        }

        let address_str = parts[0].trim();
        let word_str = parts[1].trim();

        let address = u32::from_str_radix(address_str, 16)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        let word = u32::from_str_radix(word_str, 16)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        // Record the starting address from the first instruction
        if start_address.is_none() {
            start_address = Some(address);
        }

        code.push(word);
    }

    Ok(AssemblyOutput {
        code: code.clone(),
        size: code.len() * 4, // Each word is 4 bytes
        start_address: start_address.unwrap_or(0),
    })
}

/// Read a file into a string
pub fn read_file(path: &Path) -> io::Result<String> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

/// Write the assembled output to a file in the specified format
pub fn write_output(
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
pub fn write_binary_output(assembled: &AssemblyOutput, path: &Path) -> io::Result<()> {
    let mut file = File::create(path)?;

    // Convert each 32-bit word to bytes (little-endian)
    for word in &assembled.code {
        file.write_all(&word.to_le_bytes())?;
    }

    Ok(())
}

/// Write output in Intel HEX format
pub fn write_hex_output(assembled: &AssemblyOutput, path: &Path) -> io::Result<()> {
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
pub fn write_text_output(assembled: &AssemblyOutput, path: &Path) -> io::Result<()> {
    let mut file = File::create(path)?;

    let mut address = assembled.start_address;

    for word in &assembled.code {
        writeln!(file, "{:08X}: {:08X}", address, word)?;
        address += 4; // Each instruction is 4 bytes
    }

    Ok(())
}
