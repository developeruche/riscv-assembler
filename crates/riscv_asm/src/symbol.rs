//! Manages symbols (labels) and their corresponding addresses or values.
//!
//! The `SymbolTable` is crucial for resolving labels during the assembly process,
//! especially handling forward references (labels used before they are defined).
use std::collections::HashMap;

use crate::error::{AssemblerError, SourceLocation};

/// Represents a symbol defined in the assembly code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    /// The name of the symbol (label).
    name: String,
    /// The address or value associated with the symbol.
    /// Using u32 for RISVIM32 addresses.
    address: u32,
    /// Optional: Line number where the symbol was defined (for better errors handling).
    defined_at_line: Option<usize>,
}

impl Symbol {
    /// Creates a new symbol entry.
    pub fn new(name: String, address: u32, defined_at_line: Option<usize>) -> Self {
        Symbol {
            name,
            address,
            defined_at_line,
        }
    }

    /// Returns the name of the symbol.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the address of the symbol.
    pub fn address(&self) -> u32 {
        self.address
    }

    /// Returns the line number where the symbol was defined, if known.
    pub fn defined_at_line(&self) -> Option<usize> {
        self.defined_at_line
    }
}

/// Stores and manages symbols defined in the assembly source.
///
/// Typically used in a two-pass assembly process:
/// 1. Pass 1: Populate the table with label definitions and their addresses.
/// 2. Pass 2: Use the table to resolve symbol references in instruction operands.
#[derive(Debug, Default)]
pub struct SymbolTable {
    /// The core mapping from symbol names (String) to their definitions (Symbol).
    table: HashMap<String, Symbol>,
    /// Track unresolved symbols and where they were referenced (for Pass 2 or error reporting)
    unresolved: HashMap<String, Vec<usize>>, // Key: symbol name, Value: list of lines referencing it
}

impl SymbolTable {
    /// Creates a new, empty symbol table.
    pub fn new() -> Self {
        SymbolTable::default()
    }

    /// Defines a new symbol or updates an existing one.
    ///
    /// # Arguments
    /// * `name` - The name of the symbol (label).
    /// * `address` - The address associated with the symbol.
    /// * `line_number` - The line number where the symbol is defined (optional).
    ///
    /// # Returns
    /// * `Ok(())` - If the symbol was successfully defined.
    /// * `Err(SymbolError::Redefinition)` - If a symbol with the same name already exists.
    pub fn define(
        &mut self,
        name: String,
        address: u32,
        line_number: Option<usize>,
    ) -> Result<(), AssemblerError> {
        if self.table.contains_key(&name) {
            // Consider allowing redefinition if the address is the same?
            // For now, strict redefinition is an error.
            Err(AssemblerError::SymbolError {
                message: format!("Symbol '{}' already defined", name),
                loc: SourceLocation {
                    line: line_number.unwrap_or(0),
                    col: 0,
                },
            })
        } else {
            let symbol = Symbol::new(name.clone(), address, line_number);
            self.table.insert(name, symbol);
            Ok(())
        }
    }

    /// Looks up a symbol by its name.
    ///
    /// # Arguments
    /// * `name` - The name of the symbol to look up.
    ///
    /// # Returns
    /// * `Some(&Symbol)` - If the symbol is found.
    /// * `None` - If the symbol is not defined in the table.
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.table.get(name)
    }

    /// Checks if a symbol with the given name is defined.
    pub fn is_defined(&self, name: &str) -> bool {
        self.table.contains_key(name)
    }

    /// Returns an iterator over the symbols in the table.
    pub fn iter(&self) -> impl Iterator<Item = (&String, &Symbol)> {
        self.table.iter()
    }

    /// Returns the total number of defined symbols.
    pub fn len(&self) -> usize {
        self.table.len()
    }

    /// Checks if the symbol table is empty.
    pub fn is_empty(&self) -> bool {
        self.table.is_empty()
    }

    /// Records that an unresolved symbol was encountered at a specific location.
    /// Call this during Pass 1 when an operand uses a symbol not yet defined.
    pub fn add_unresolved_reference(&mut self, name: &str, line_number: usize) {
        self.unresolved
            .entry(name.to_string())
            .or_default()
            .push(line_number);
    }

    /// Checks for any symbols that were referenced but never defined after Pass 1.
    pub fn check_unresolved(&self) -> Vec<(&String, &Vec<usize>)> {
        self.unresolved
            .iter()
            .filter(|(name, _)| !self.table.contains_key(*name))
            .collect()
    }
}



#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_new_symbol_table() {
        let st = SymbolTable::new();
        assert!(st.is_empty());
        assert_eq!(st.len(), 0);
    }

    #[test]
    fn test_define_and_lookup() {
        let mut st = SymbolTable::new();
        let name = "loop_start".to_string();
        let address = 0x1000;

        assert!(st.define(name.clone(), address, Some(5)).is_ok());
        assert_eq!(st.len(), 1);
        assert!(st.is_defined(&name));

        let symbol = st.lookup(&name);
        assert!(symbol.is_some());
        let symbol = symbol.unwrap();
        assert_eq!(symbol.name(), name);
        assert_eq!(symbol.address(), address);
        assert_eq!(symbol.defined_at_line(), Some(5));
    }

    #[test]
    fn test_lookup_undefined() {
        let st = SymbolTable::new();
        assert!(st.lookup("undefined_label").is_none());
        assert!(!st.is_defined("undefined_label"));
    }

    #[test]
    fn test_redefinition_error() {
        let mut st = SymbolTable::new();
        let name = "label1".to_string();

        assert!(st.define(name.clone(), 0x200, Some(1)).is_ok());
        let result = st.define(name.clone(), 0x300, Some(10));

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap(),
            AssemblerError::SymbolError {
                message: format!("Symbol '{}' already defined", name),
                loc: SourceLocation { line: 10, col: 0 }
            }
        );
        assert_eq!(st.len(), 1); // Should still only contain the first definition

        // Verify the original definition is intact
        let symbol = st.lookup("label1").unwrap();
        assert_eq!(symbol.address(), 0x200);
        assert_eq!(symbol.defined_at_line(), Some(1));
    }

    #[test]
    fn test_iter() {
        let mut st = SymbolTable::new();
        st.define("lab_a".to_string(), 10, Some(1)).unwrap();
        st.define("lab_b".to_string(), 20, Some(2)).unwrap();

        let mut count = 0;
        let mut found_a = false;
        let mut found_b = false;
        for (name, symbol) in st.iter() {
            count += 1;
            if name == "lab_a" {
                assert_eq!(symbol.address(), 10);
                found_a = true;
            } else if name == "lab_b" {
                assert_eq!(symbol.address(), 20);
                found_b = true;
            }
        }
        assert_eq!(count, 2);
        assert!(found_a);
        assert!(found_b);
    }
}
