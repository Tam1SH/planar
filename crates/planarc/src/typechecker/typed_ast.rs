use crate::linker::meta::SymbolId;
use crate::linker::symbol_table::SymbolTable;
use crate::spanned::{FileId, Spanned};
use rkyv::{Archive, Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedWorld {
    pub table: SymbolTable,
    pub modules: BTreeMap<String, TypedModule>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedModule {
    pub file_id: FileId,
    pub grammar: Option<Spanned<String>>,
    pub facts: Vec<Spanned<TypedFact>>,
    pub types: Vec<Spanned<TypedType>>,
    pub externs: Vec<Spanned<TypedExternDefinition>>,
    pub queries: Vec<Spanned<TypedQuery>>,
    pub nodes: Vec<Spanned<TypedNode>>,
    pub edges: Vec<Spanned<TypedEdge>>,
}

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
    I64,
    F64,
    Str,
    Bool,
    Fact(SymbolId),
    User(SymbolId),
    List(#[rkyv(omit_bounds)] Box<Type>),
    Node(SymbolId),
    Void,
    Unknown,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedEdge {
    pub id: SymbolId,
    pub name: String,
    pub from: SymbolId,
    pub to: SymbolId,
    pub relation: String,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub enum TypedMatchItem {
    Let(TypedLetBinding),
    Capture(TypedCapture),
    Emit(TypedEmitStatement),
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedEmitStatement {
    pub left: TypedEmittedFact,
    pub right: Option<TypedEmittedFact>,
    pub relation: Option<Spanned<SymbolId>>,
    pub direction: Option<TypedRelationDirection>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub enum TypedRelationDirection {
    Left,
    Right,
    Both,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedEmittedFact {
    pub fact_id: SymbolId,
    pub fields: Vec<TypedEmittedField>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedEmittedField {
    pub name: Spanned<String>,
    pub value: Spanned<TypedExpression>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedNode {
    pub id: SymbolId,
    pub kind: String,
    pub statements: Vec<TypedNodeStatement>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub enum TypedNodeStatement {
    Match(Spanned<TypedMatchStatement>),
    Query(Spanned<TypedQuery>),
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedMatchStatement {
    pub query_ref: Spanned<TypedMatchQueryReference>,
    pub body: Vec<Spanned<TypedMatchItem>>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedLetBinding {
    pub name: Spanned<String>,
    pub value: Spanned<TypedExpression>,
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
pub struct TypedCapture {
    pub name: Spanned<String>,
    #[rkyv(omit_bounds)]
    pub body: Vec<Spanned<TypedMatchItem>>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub enum TypedMatchQueryReference {
    Global(SymbolId),
    Raw {
        source: Spanned<String>,
        captures: Vec<Spanned<String>>,
    },
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedQuery {
    pub id: SymbolId,
    pub name: String,
    pub query: String,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedExternDefinition {
    pub functions: Vec<Spanned<TypedExternFunction>>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedExternFunction {
    pub id: SymbolId,
    pub name: String,
    pub args: Vec<Spanned<TypedExternArgument>>,
    pub return_ty: Option<TypedTypeReference>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedExternArgument {
    pub name: String,
    pub ty: TypedTypeReference,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedFact {
    pub id: SymbolId,
    pub attributes: Vec<Spanned<TypedAttribute>>,
    pub name: String,
    pub fields: Vec<Spanned<TypedField>>,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedType {
    pub id: SymbolId,
    pub name: String,
    pub attributes: Vec<Spanned<TypedAttribute>>,
    pub definition: Spanned<TypedTypeDefinition>,
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
pub struct TypedTypeDefinition {
    pub base_type: Option<TypedTypeReference>,
    #[rkyv(omit_bounds)]
    pub fields: Vec<Spanned<TypedTypeField>>,
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
pub struct TypedTypeField {
    pub name: String,
    pub definition: TypedTypeDefinition,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedField {
    pub attributes: Vec<Spanned<TypedAttribute>>,
    pub name: String,
    pub ty: TypedTypeReference,
}

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct TypedAttribute {
    pub name: Spanned<String>,
    pub args: Vec<Spanned<TypedExpression>>,
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
pub struct TypedTypeReference {
    pub ty: Type,
    pub symbol: Spanned<SymbolId>,
    #[rkyv(omit_bounds)]
    pub args: Vec<Spanned<TypedTypeReference>>,
    pub refinement: Option<Spanned<TypedExpression>>,
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
pub struct TypedExpression {
    pub ty: Type,
    #[rkyv(omit_bounds)]
    pub kind: TypedExpressionKind,
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
pub enum TypedExpressionKind {
    Identifier(SymbolId),
    LocalIdentifier(String),
    Number(String),
    StringLit(String),

    Binary {
        #[rkyv(omit_bounds)]
        left: Box<Spanned<TypedExpression>>,
        operator: SymbolId,
        #[rkyv(omit_bounds)]
        right: Box<Spanned<TypedExpression>>,
    },

    Call {
        #[rkyv(omit_bounds)]
        function: Box<Spanned<TypedExpression>>,
        #[rkyv(omit_bounds)]
        args: Vec<Spanned<TypedExpression>>,
    },

    InList(#[rkyv(omit_bounds)] Vec<Spanned<TypedExpression>>),
    InRange {
        #[rkyv(omit_bounds)]
        start: Box<Spanned<TypedExpression>>,
        #[rkyv(omit_bounds)]
        end: Option<Box<Spanned<TypedExpression>>>,
    },
}
