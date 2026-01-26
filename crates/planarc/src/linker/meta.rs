use crate::spanned::{Location, Spanned};
use derive_more::Display;
use rkyv::{Archive, Deserialize, Serialize};
use std::fmt;
use std::fmt::Formatter;

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

impl SymbolId {
    pub const INVALID_ID: SymbolId = SymbolId(usize::MAX);
}

#[derive(Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
pub enum SymbolKind {
    Fact {
        fields: Vec<FieldMetadata>,
    },
    Type {
        base_type: Option<SymbolId>,
        fields: Vec<FieldMetadata>,
        is_primitive: bool,
    },
    ExternFunction {
        params: Vec<FunctionParam>,
        return_type: Option<SymbolId>,
    },
    Query {
        source: Spanned<String>,
        captures: Vec<Spanned<String>>,
    },
    Node,
    Edge {
        from: SymbolId,
        to: SymbolId,
    },
}

impl fmt::Debug for ArchivedSymbolKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let variant_name = match self {
            ArchivedSymbolKind::Fact { .. } => "Fact",
            ArchivedSymbolKind::Type { .. } => "Type",
            ArchivedSymbolKind::ExternFunction { .. } => "ExternFunction",
            ArchivedSymbolKind::Query { .. } => "Query",
            ArchivedSymbolKind::Node => "Node",
            ArchivedSymbolKind::Edge { .. } => "Edge",
        };
        f.write_str(variant_name)
    }
}
impl fmt::Debug for SymbolKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let variant_name = match self {
            SymbolKind::Fact { .. } => "Fact",
            SymbolKind::Type { .. } => "Type",
            SymbolKind::ExternFunction { .. } => "ExternFunction",
            SymbolKind::Query { .. } => "Query",
            SymbolKind::Node => "Node",
            SymbolKind::Edge { .. } => "Edge",
        };

        f.write_str(variant_name)
    }
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct FieldMetadata {
    pub name: String,
    pub type_id: SymbolId,
    pub attributes: Vec<String>,
    pub location: Location,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct FunctionParam {
    pub name: String,
    pub type_id: SymbolId,
    pub location: Location,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq, Copy)]
#[rkyv(derive(Debug))]
pub enum Visibility {
    Public,
    Package,
    ModulePrivate,
    Scoped(SymbolId),
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct SymbolMetadata {
    pub id: SymbolId,
    pub fqmn: String,
    pub kind: SymbolKind,
    pub location: Location,
    pub visibility: Visibility,
    pub package: String,
    pub module: String,
}

#[derive(Debug, Clone)]
pub struct PendingSymbol {
    pub name: String,
    pub kind: SymbolKind,
    pub loc: Location,
    pub visibility: Visibility,
    pub module_name: String,
    pub package_name: String,
}
