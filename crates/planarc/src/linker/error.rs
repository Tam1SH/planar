use miette::{Diagnostic, NamedSource, SourceSpan};
use std::fmt;
use thiserror::Error;

use crate::{compiler::error::ErrorWithLocation, spanned::Location};

#[derive(Clone, Debug, Error, Diagnostic)]
pub enum LinkerError {
    #[error("Access violation: symbol '{name}' is {reason}")]
    #[diagnostic(
        code(pdl::linker::access_violation),
        help("This symbol is private or internal. Use 'pub' to export it if needed.")
    )]
    AccessViolation {
        name: String,
        reason: String,
        #[source_code]
        src: NamedSource<String>,
        #[label("access to this symbol is restricted")]
        span: SourceSpan,
        loc: Location,
    },

    #[error("Symbol collision: '{name}' is already defined")]
    #[diagnostic(code(pdl::linker::symbol_collision))]
    SymbolCollision {
        name: String,

        #[source_code]
        src: NamedSource<String>,

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
        src: NamedSource<String>,

        #[label("undeclared identifier")]
        span: SourceSpan,
        loc: Location,
    },

    #[error("Ambiguous reference: '{name}' could refer to multiple symbols")]
    #[diagnostic(code(pdl::linker::ambiguous_reference))]
    AmbiguousReference {
        name: String,

        #[source_code]
        src: NamedSource<String>,

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
        src: NamedSource<String>,

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
    pub src: NamedSource<String>,

    #[label("defined here")]
    pub span: SourceSpan,
    pub loc: Location,
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Originally defined here")]
pub struct PreviousDefinition {
    #[source_code]
    pub src: NamedSource<String>,

    #[label("original definition")]
    pub span: SourceSpan,
    pub loc: Location,
}

#[derive(Clone, Error, Diagnostic)]
#[error("Found {} linker errors", .0.len())]
pub struct LinkerErrors(#[related] pub Vec<LinkerError>);

impl ErrorWithLocation for LinkerError {
    fn location(&self) -> Location {
        match self {
            LinkerError::SymbolCollision { loc, .. } => *loc,
            LinkerError::UnknownSymbol { loc, .. } => *loc,
            LinkerError::AmbiguousReference { loc, .. } => *loc,
            LinkerError::InvalidSymbolKind { loc, .. } => *loc,
            LinkerError::AccessViolation { loc, .. } => *loc,
        }
    }
}

impl LinkerErrors {
    pub fn new(errors: Vec<LinkerError>) -> Self {
        Self(errors)
    }

    pub fn extend(&mut self, errors: LinkerErrors) {
        self.0.extend(errors.0);
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl fmt::Debug for LinkerErrors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&miette::Report::new(self.clone()), f)
    }
}

#[derive(Debug, Error, Diagnostic)]
pub enum GraphError {
    #[error("Module '{import}' not found")]
    #[diagnostic(
        code(pdl::dependencies::missing_module),
        help("Make sure the module exists in the source roots and is named correctly.")
    )]
    UnknownImport {
        #[source_code]
        src: NamedSource<String>,
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
    pub src: NamedSource<String>,
    #[label("imports '{target}' here")]
    pub span: SourceSpan,
    pub loc: Location,
    pub module: String,
    pub target: String,
}
