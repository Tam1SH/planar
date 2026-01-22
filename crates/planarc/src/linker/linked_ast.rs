use rkyv::{Archive, Deserialize, Serialize};

use crate::{
    linker::ids::{ResolvedId, SymbolId},
    spanned::{FileId, Spanned},
};

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct LinkedModule {
    pub file_id: FileId,
    pub facts: Vec<Spanned<LinkedFact>>,
    pub types: Vec<Spanned<LinkedType>>,
    pub externs: Vec<Spanned<LinkedExternDefinition>>,
    pub queries: Vec<Spanned<LinkedQuery>>,
    pub nodes: Vec<Spanned<LinkedNode>>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct LinkedNode {
    pub id: SymbolId,
    pub kind: String,
    pub statements: Vec<Spanned<LinkedNodeStatement>>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub enum LinkedNodeStatement {
    Match(LinkedMatchStatement),
    Query(LinkedQuery),
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct LinkedMatchStatement {
    pub query_ref: Spanned<LinkedMatchQueryReference>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub enum LinkedMatchQueryReference {
    Global(SymbolId),
    Raw(String),
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct LinkedQuery {
    pub id: SymbolId,
    pub name: String,
    pub grammar: String,
    pub query: String,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct LinkedExternDefinition {
    pub functions: Vec<Spanned<LinkedExternFunction>>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct LinkedExternFunction {
    pub id: SymbolId,
    pub name: String,
    pub args: Vec<Spanned<LinkedExternArgument>>,
    pub return_ty: Option<LinkedTypeReference>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct LinkedExternArgument {
    pub name: String,
    pub ty: LinkedTypeReference,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct LinkedFact {
    pub id: SymbolId,
    pub attributes: Vec<Spanned<LinkedAttribute>>,
    pub name: String,
    pub fields: Vec<Spanned<LinkedField>>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct LinkedType {
    pub id: SymbolId,
    pub name: String,
    pub attributes: Vec<Spanned<LinkedAttribute>>,
    pub definition: Spanned<LinkedTypeDefinition>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
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
pub struct LinkedTypeDefinition {
    pub base_type: Option<LinkedTypeReference>,
    #[rkyv(omit_bounds)]
    pub fields: Vec<Spanned<LinkedTypeField>>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
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
pub struct LinkedTypeField {
    pub name: String,
    pub definition: LinkedTypeDefinition,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct LinkedField {
    pub attributes: Vec<Spanned<LinkedAttribute>>,
    pub name: String,
    pub ty: LinkedTypeReference,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct LinkedAttribute {
    pub name: Spanned<String>,
    pub args: Vec<Spanned<LinkedExpression>>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
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
pub struct LinkedTypeReference {
    pub symbol: Spanned<ResolvedId>,
    #[rkyv(omit_bounds)]
    pub args: Vec<Spanned<LinkedTypeReference>>,
    pub refinement: Option<Spanned<LinkedExpression>>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
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
pub enum LinkedExpression {
    Identifier(ResolvedId),
    Number(String),
    StringLit(String),

    Binary {
        #[rkyv(omit_bounds)]
        left: Box<Spanned<LinkedExpression>>,
        operator: Spanned<ResolvedId>,
        #[rkyv(omit_bounds)]
        right: Box<Spanned<LinkedExpression>>,
    },

    PartialComparison {
        operator: Spanned<ResolvedId>,
        #[rkyv(omit_bounds)]
        right: Box<Spanned<LinkedExpression>>,
    },

    Call {
        #[rkyv(omit_bounds)]
        function: Box<Spanned<LinkedExpression>>,
        #[rkyv(omit_bounds)]
        args: Vec<Spanned<LinkedExpression>>,
    },

    InList(#[rkyv(omit_bounds)] Vec<Spanned<LinkedExpression>>),
    InRange {
        #[rkyv(omit_bounds)]
        start: Box<Spanned<LinkedExpression>>,
        #[rkyv(omit_bounds)]
        end: Option<Box<Spanned<LinkedExpression>>>,
    },
}
