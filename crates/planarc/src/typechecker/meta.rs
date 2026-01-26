use rkyv::{Archive, Deserialize, Serialize};

use crate::linker::meta::SymbolId;

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[rkyv(derive(Debug, Eq, PartialEq))]
#[rkyv(
    serialize_bounds(
        __S: rkyv::ser::Writer + rkyv::ser::Allocator + rkyv::rancor::Fallible,
        <__S as rkyv::rancor::Fallible>::Error: rkyv::rancor::Source,
    ),
    deserialize_bounds(
        __D: rkyv::rancor::Fallible,
        <__D as rkyv::rancor::Fallible>::Error: rkyv::rancor::Source,
    ),
    bytecheck(bounds(
        __C: rkyv::validation::ArchiveContext,
        <__C as rkyv::rancor::Fallible>::Error: rkyv::rancor::Source,
    ))
)]
pub enum Type {
    Primitive(SymbolId),
    Fact(SymbolId),
    UserType(SymbolId),
    Node(SymbolId),
    List(#[rkyv(omit_bounds)] Box<Type>),
    Unknown,
}
