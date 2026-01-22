use crate::spanned::{FileId, Spanned};

#[derive(Debug, Clone, Default)]
pub enum Visibility {
    #[default]
    Private,
    Pub,
    Package,
}

#[derive(Debug, Clone, Default)]
pub struct Module {
    pub file_id: FileId,
    pub imports: Vec<Spanned<Import>>,
    pub facts: Vec<Spanned<FactDefinition>>,
    pub externs: Vec<Spanned<ExternDefinition>>,
    pub types: Vec<Spanned<TypeDeclaration>>,
    pub queries: Vec<Spanned<QueryDefinition>>,
    pub edges: Vec<Spanned<EdgeDefinition>>,
    pub nodes: Vec<Spanned<NodeDefinition>>,
}

#[derive(Debug, Clone)]
pub struct QueryDefinition {
    pub vis: Visibility,
    pub name: Spanned<String>,
    pub grammar: Spanned<String>,
    pub value: Spanned<String>,
}

#[derive(Debug, Clone)]
pub struct NodeDefinition {
    pub vis: Visibility,
    pub kind: Spanned<String>,
    pub statements: Vec<Spanned<NodeStatement>>,
}

#[derive(Debug, Clone)]
pub enum NodeStatement {
    Query(Spanned<QueryDefinition>),
    Match(Spanned<MatchStatement>),
}

#[derive(Debug, Clone)]
pub struct MatchStatement {
    pub query_ref: Spanned<MatchQueryReference>,
}

#[derive(Debug, Clone)]
pub struct Capture {
    //TODO: plug
    pub name: Spanned<String>,
}

#[derive(Debug, Clone)]
pub enum MatchQueryReference {
    Identifier(String),
    Raw(String),
}

#[derive(Debug, Clone)]
pub struct Import {
    pub fqmn: Spanned<String>,
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub vis: Visibility,
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: Spanned<String>,
    pub definition: Spanned<TypeDefinition>,
}

#[derive(Debug, Clone)]
pub struct EdgeDefinition {
    pub vis: Visibility,
    pub name: Spanned<String>,
    pub from: Spanned<String>,
    pub to: Spanned<String>,
    pub relation: Spanned<String>,
}

#[derive(Debug, Clone)]
pub struct TypeDefinition {
    pub base_type: Option<TypeAnnotation>,
    pub fields: Vec<Spanned<TypeField>>,
}

#[derive(Debug, Clone)]
pub struct TypeField {
    pub name: Spanned<String>,
    pub definition: Spanned<TypeDefinition>,
}

#[derive(Debug, Clone)]
pub struct TypeAnnotation {
    pub name: Spanned<String>,
    pub refinement: Option<Spanned<Expression>>,
    pub args: Vec<Spanned<TypeAnnotation>>,
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Spanned<String>,
}

#[derive(Debug, Clone)]
pub struct FactDefinition {
    pub attributes: Vec<Spanned<Attribute>>,
    pub vis: Visibility,
    pub name: Spanned<String>,
    pub fields: Vec<Spanned<FactField>>,
}

#[derive(Debug, Clone)]
pub struct FactField {
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: Spanned<String>,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(String),
    OperatorIdentifier(String),
    Number(String),
    StringLit(String),

    Binary {
        left: Box<Spanned<Expression>>,
        op: Spanned<String>,
        right: Box<Spanned<Expression>>,
    },

    Call {
        function: Box<Spanned<Expression>>,
        args: Vec<Spanned<Expression>>,
    },

    InList(Vec<Spanned<Expression>>),
    InRange {
        start: Box<Spanned<Expression>>,
        end: Option<Box<Spanned<Expression>>>,
    },
}

#[derive(Debug, Clone)]
pub struct ExternDefinition {
    pub vis: Visibility,
    pub attributes: Vec<Spanned<Attribute>>,
    pub functions: Vec<Spanned<ExternFunction>>,
}

#[derive(Debug, Clone)]
pub struct ExternFunction {
    pub name: Spanned<String>,
    pub args: Vec<Spanned<ExternArgument>>,
    pub return_type: Option<Spanned<TypeAnnotation>>,
}

#[derive(Debug, Clone)]
pub struct ExternArgument {
    pub name: Spanned<String>,
    pub ty: Spanned<TypeAnnotation>,
}
