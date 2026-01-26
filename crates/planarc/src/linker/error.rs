use miette::{Diagnostic, NamedSource, SourceSpan};
use std::{fmt, sync::Arc};
use thiserror::Error;

use crate::{
    error::ErrorCollection, impl_diagnostic_with_location, source_registry::MietteSource,
    spanned::Location,
};

#[derive(Clone, Debug, Error, Diagnostic)]
pub enum LinkerError {
    #[error("Undefined capture: '@{capture_name}'")]
    #[diagnostic(
        code(pdl::linker::undefined_capture),
        help(
            "The capture '@{capture_name}' must be present in the associated S-expression. \
             Check for typos and ensure it's not commented out in the query string."
        )
    )]
    UndefinedCapture {
        capture_name: String,

        #[source_code]
        src: MietteSource,

        #[label("'@{capture_name}' is not captured by the query")]
        span: SourceSpan,

        loc: Location,
    },
    #[error("Invalid capture block: identifier '{name}' cannot have a binding block")]
    #[diagnostic(
        code(pdl::linker::invalid_capture_block),
        help(
            "Only query captures (starting with '@') can define a lexical scope for binding \
             identifiers to generated facts. Plain variables must be assigned using '='."
        )
    )]
    InvalidCaptureBlock {
        name: String,
        #[source_code]
        src: MietteSource,
        #[label("plain identifier cannot open a binding block")]
        span: SourceSpan,
        loc: Location,
    },

    #[error("Access violation: symbol '{name}' is {reason}")]
    #[diagnostic(
        code(pdl::linker::access_violation),
        help("This symbol is private or internal. Use 'pub' to export it if needed.")
    )]
    AccessViolation {
        name: String,
        reason: String,
        #[source_code]
        src: MietteSource,
        #[label("access to this symbol is restricted")]
        span: SourceSpan,
        loc: Location,
    },

    #[error("Symbol collision: '{name}' is already defined")]
    #[diagnostic(code(pdl::linker::symbol_collision))]
    SymbolCollision {
        name: String,

        #[source_code]
        src: MietteSource,

        #[label("redefined here")]
        span: SourceSpan,

        #[related]
        related: Vec<PreviousDefinition>,
        loc: Location,
    },

    #[error("Unknown symbol: '{name}'")]
    #[diagnostic(code(pdl::linker::unknown_symbol))]
    UnknownSymbol {
        name: String,
        #[source_code]
        src: MietteSource,
        #[label("undeclared identifier")]
        span: SourceSpan,
        loc: Location,
        #[help]
        help: Option<String>,
    },

    #[error("Ambiguous reference: '{name}' could refer to multiple symbols")]
    #[diagnostic(code(pdl::linker::ambiguous_reference))]
    AmbiguousReference {
        name: String,

        #[source_code]
        src: MietteSource,

        #[label("ambiguous usage")]
        span: SourceSpan,

        #[related]
        candidates: Vec<AmbiguousCandidate>,
        loc: Location,
    },

    #[error("Invalid symbol kind: '{name}' is a {found}, but a {expected} was expected here")]
    #[diagnostic(
        code(pdl::linker::invalid_symbol_kind),
        help(
            "Check if you are using a variable where a definition name is required, or vice-versa."
        )
    )]
    InvalidSymbolKind {
        name: String,
        expected: String,
        found: String,

        #[source_code]
        src: MietteSource,

        #[label("expected {expected}, found {found}")]
        span: SourceSpan,
        loc: Location,
    },
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Found candidate in module '{module_name}'")]
pub struct AmbiguousCandidate {
    pub module_name: String,

    #[source_code]
    pub src: MietteSource,

    #[label("defined here")]
    pub span: SourceSpan,
    pub loc: Location,
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Originally defined here")]
pub struct PreviousDefinition {
    #[source_code]
    pub src: MietteSource,

    #[label("original definition")]
    pub span: SourceSpan,
    pub loc: Location,
}

impl_diagnostic_with_location!(LinkerError, {
    LinkerError::SymbolCollision,
    LinkerError::UnknownSymbol,
    LinkerError::AmbiguousReference,
    LinkerError::InvalidSymbolKind,
    LinkerError::AccessViolation,
    LinkerError::InvalidCaptureBlock,
    LinkerError::UndefinedCapture
});

pub type LinkerErrors = ErrorCollection<LinkerError>;

#[derive(Debug, Error, Diagnostic)]
pub enum GraphError {
    #[error("Module '{import}' not found")]
    #[diagnostic(
        code(pdl::dependencies::missing_module),
        help("Make sure the module exists in the source roots and is named correctly.")
    )]
    UnknownImport {
        #[source_code]
        src: MietteSource,
        #[label("this import")]
        span: SourceSpan,
        import: String,
        module: String,
        loc: Location,
    },

    #[error("Circular dependency detected involving '{root_module}'")]
    #[diagnostic(
        code(pdl::dependencies::circular_dependency),
        help("Break the cycle by extracting shared types into a separate module.")
    )]
    CircularDependency {
        root_module: String,
        #[related]
        cycle_path: Vec<CycleStep>,
    },

    #[error("Duplicate module definition: '{fqmn}'")]
    #[diagnostic(
        code(pdl::dependencies::duplicate_module),
        help(
            "The module '{fqmn}' is defined in multiple files:\n  1. {path1}\n  2. {path2}\nRename one of them or check your source roots."
        )
    )]
    DuplicateModule {
        fqmn: String,
        path1: String,
        path2: String,
    },
}

#[derive(Debug, Error, Diagnostic)]
#[error("...module '{module}' imports '{target}'")]
pub struct CycleStep {
    #[source_code]
    pub src: MietteSource,
    #[label("imports '{target}' here")]
    pub span: SourceSpan,
    pub loc: Location,
    pub module: String,
    pub target: String,
}
