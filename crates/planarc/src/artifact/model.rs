use rkyv::{Archive, Deserialize, Serialize};
use std::collections::BTreeMap;

use crate::typechecker::typed_ast::TypedWorld;
use crate::{
    linker::{
        linked_ast::LinkedModule,
        meta::{SymbolId, SymbolKind},
        symbol_table::SymbolTable,
    },
    spanned::{FileId, Location},
};

#[derive(Debug, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct Bundle {
    pub world: TypedWorld,
    pub wasm_modules: BTreeMap<String, Vec<u8>>,
    pub files: BTreeMap<FileId, String>,
    pub grammars: BTreeMap<String, GrammarMetadata>,
}

#[derive(Debug, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct GrammarMetadata {
    pub version: String,
}
