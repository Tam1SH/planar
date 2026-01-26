use std::collections::BTreeMap;

use crate::{
    linker::{linked_ast::LinkedModule, symbol_table::SymbolTable},
    source_registry::SourceRegistry,
};

#[derive(Debug)]
pub struct LinkedWorld {
    pub table: SymbolTable,
    pub modules: BTreeMap<String, LinkedModule>,
    pub registry: SourceRegistry,
}
