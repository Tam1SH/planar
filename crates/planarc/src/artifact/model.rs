use rkyv::{Archive, Deserialize, Serialize};
use std::collections::BTreeMap;

use crate::{
    linker::{
        ids::{SymbolId, SymbolKind},
        linked_ast::LinkedModule,
        symbol_table::SymbolTable,
    },
    spanned::{FileId, Location},
};

#[derive(Debug, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct Bundle {
    pub symbol_table: SymbolTable,
    pub modules: BTreeMap<String, LinkedModule>,
    pub wasm_modules: BTreeMap<String, Vec<u8>>,
    pub files: BTreeMap<FileId, String>,
    pub grammars: BTreeMap<String, GrammarMetadata>,
}

#[derive(Debug, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct GrammarMetadata {
    pub version: String,
}
