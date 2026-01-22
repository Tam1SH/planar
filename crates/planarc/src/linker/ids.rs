use crate::spanned::Spanned;
use derive_more::Display;
use rkyv::{Archive, Deserialize, Serialize};

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub enum ResolvedId {
    Global(Spanned<SymbolId>),
    Local(Spanned<String>),
}

impl ResolvedId {
    pub fn symbol_id(&self) -> SymbolId {
        match self {
            ResolvedId::Global(s) => s.value,
            ResolvedId::Local(_) => panic!("Expected global symbol, found local 'it'"),
        }
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Archive,
    Serialize,
    Deserialize,
    Display,
    PartialOrd,
    Ord,
)]
#[rkyv(derive(Debug, Eq, PartialEq, Ord, PartialOrd))]
pub struct SymbolId(pub usize);

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq, Copy)]
#[rkyv(derive(Debug))]
pub enum SymbolKind {
    Fact,
    Type,
    Field,
    ExternFunction,
    Query,
    Node,
}
