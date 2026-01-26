use crate::ast;
use crate::linker::meta::{ResolvedId, SymbolId};
use crate::spanned::{FileId, Spanned};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedModule {
    pub file_id: FileId,
    pub grammar: Option<Spanned<String>>,
    pub facts: Vec<Spanned<LinkedFact>>,
    pub types: Vec<Spanned<LinkedType>>,
    pub externs: Vec<Spanned<LinkedExternDefinition>>,
    pub queries: Vec<Spanned<LinkedQuery>>,
    pub nodes: Vec<Spanned<LinkedNode>>,
    pub edges: Vec<Spanned<LinkedEdge>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedEdge {
    pub id: SymbolId,
    pub name: String,
    pub from: SymbolId,
    pub to: SymbolId,
    pub relation: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LinkedMatchItem {
    Let(LinkedLetBinding),
    Capture(LinkedCapture),
    Emit(LinkedEmitStatement),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedEmitStatement {
    pub left: LinkedEmittedFact,
    pub right: Option<LinkedEmittedFact>,
    pub relation: Option<Spanned<SymbolId>>,
    pub direction: Option<RelationDirection>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RelationDirection {
    Left,  // <
    Right, // >
    Both,  // <>
}

impl From<ast::RelationDirection> for RelationDirection {
    fn from(ast_dir: ast::RelationDirection) -> Self {
        match ast_dir {
            ast::RelationDirection::Left => Self::Left,
            ast::RelationDirection::Right => Self::Right,
            ast::RelationDirection::Both => Self::Both,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedEmittedFact {
    pub fact_id: SymbolId,
    pub fields: Vec<LinkedEmittedField>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedEmittedField {
    pub name: Spanned<String>,
    pub value: Spanned<LinkedExpression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedNode {
    pub id: SymbolId,
    pub kind: String,
    pub statements: Vec<LinkedNodeStatement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LinkedNodeStatement {
    Match(Spanned<LinkedMatchStatement>),
    Query(Spanned<LinkedQuery>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedMatchStatement {
    pub query_ref: Spanned<LinkedMatchQueryReference>,
    pub body: Vec<Spanned<LinkedMatchItem>>,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedLetBinding {
    pub name: Spanned<String>,
    pub value: Spanned<LinkedExpression>,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedCapture {
    pub name: Spanned<String>,
    pub body: Vec<Spanned<LinkedMatchItem>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LinkedMatchQueryReference {
    Named(SymbolId),
    Raw {
        source: Spanned<String>,
        captures: Vec<Spanned<String>>,
    },
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedQuery {
    pub id: SymbolId,
    pub name: String,
    pub query: String,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedExternDefinition {
    pub functions: Vec<Spanned<LinkedExternFunction>>,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedExternFunction {
    pub id: SymbolId,
    pub name: String,
    pub args: Vec<Spanned<LinkedExternArgument>>,
    pub return_ty: Option<LinkedTypeReference>,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedExternArgument {
    pub name: String,
    pub ty: LinkedTypeReference,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedFact {
    pub id: SymbolId,
    pub attributes: Vec<Spanned<LinkedAttribute>>,
    pub name: String,
    pub fields: Vec<Spanned<LinkedField>>,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedType {
    pub id: SymbolId,
    pub name: String,
    pub attributes: Vec<Spanned<LinkedAttribute>>,
    pub definition: Spanned<LinkedTypeDefinition>,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedTypeDefinition {
    pub base_type: Option<LinkedTypeReference>,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedTypeField {
    pub name: String,
    pub definition: LinkedTypeDefinition,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedField {
    pub attributes: Vec<Spanned<LinkedAttribute>>,
    pub name: String,
    pub ty: LinkedTypeReference,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedAttribute {
    pub name: Spanned<String>,
    pub args: Vec<Spanned<LinkedExpression>>,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkedTypeReference {
    pub symbol: Spanned<ResolvedId>,
    pub args: Vec<Spanned<LinkedTypeReference>>,
    pub refinement: Option<Spanned<LinkedExpression>>,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LinkedExpression {
    Identifier(ResolvedId),
    Number(String),
    StringLit(String),

    Binary {
        left: Box<Spanned<LinkedExpression>>,
        operator: Spanned<ResolvedId>,
        right: Box<Spanned<LinkedExpression>>,
    },

    PartialComparison {
        operator: Spanned<ResolvedId>,
        right: Box<Spanned<LinkedExpression>>,
    },

    Call {
        function: Box<Spanned<LinkedExpression>>,
        args: Vec<Spanned<LinkedExpression>>,
    },

    InList(Vec<Spanned<LinkedExpression>>),
    InRange {
        start: Box<Spanned<LinkedExpression>>,
        end: Option<Box<Spanned<LinkedExpression>>>,
    },
}
