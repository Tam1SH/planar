use std::fmt;

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::{compiler::error::ErrorWithLocation, spanned::Location};

#[derive(Error, Clone, Debug, Diagnostic)]
pub enum ValidationError {
    // --- WIT Errors ---
    #[error("Type '{name}' is marked as wit-compatible but uses non-wit type '{used}'")]
    #[diagnostic(code(pdl::codegen::wit_incompatible))]
    WitIncompatibility {
        name: String,
        used: String,
        #[label("this type is not from std.wit.*")]
        span: SourceSpan,
        #[source_code]
        src: NamedSource<String>,
        loc: Location,
    },

    #[error("Refinements are not allowed in wit-compatible types")]
    #[diagnostic(code(pdl::codegen::wit_no_refinement))]
    WitRefinementDisallowed {
        #[label("refinement found here")]
        span: SourceSpan,
        #[source_code]
        src: NamedSource<String>,
        loc: Location,
    },

    // --- Query & Grammar Errors ---
    #[error(
        "Invalid grammar namespace '{namespace}'. Use 'grammars.' prefix for tree-sitter grammars"
    )]
    #[diagnostic(
        code(pdl::validator::invalid_grammar_namespace),
        help("Example: query myQuery: grammars.nginx = `...`")
    )]
    InvalidGrammarNamespace {
        namespace: String,
        #[label("namespace must start with 'grammars.'")]
        span: SourceSpan,
        #[source_code]
        src: NamedSource<String>,
        loc: Location,
    },

    #[error("Grammar '{name}' not found in the loaded dependencies")]
    #[diagnostic(
        code(pdl::validator::grammar_not_found),
        help(
            "Add `{name}` to the `grammars` block in `planar.kdl`.\nUse `{name}` for the official registry or `{name} path=\"...\"` for a local binary."
        )
    )]
    GrammarNotFound {
        name: String,
        #[label("no build artifact for grammar '{name}'")]
        span: SourceSpan,
        #[source_code]
        src: NamedSource<String>,
        loc: Location,
    },

    #[error("Tree-sitter query syntax error: {message}")]
    #[diagnostic(code(pdl::validator::query_syntax_error))]
    InvalidQuerySyntax {
        message: String,
        #[label("parsing of tree-sitter query failed")]
        span: SourceSpan,
        #[source_code]
        src: NamedSource<String>,
        loc: Location,
    },
}

#[derive(Clone, Error, Diagnostic)]
#[error("Found {} linker errors", .0.len())]
pub struct ValidationErrors(#[related] pub Vec<ValidationError>);

impl ErrorWithLocation for ValidationError {
    fn location(&self) -> Location {
        match &self {
            ValidationError::WitIncompatibility { loc, .. } => loc.clone(),
            ValidationError::WitRefinementDisallowed { loc, .. } => loc.clone(),
            ValidationError::InvalidGrammarNamespace { loc, .. } => loc.clone(),
            ValidationError::GrammarNotFound { loc, .. } => loc.clone(),
            ValidationError::InvalidQuerySyntax { loc, .. } => loc.clone(),
        }
    }
}

impl ValidationErrors {
    pub fn new(errors: Vec<ValidationError>) -> Self {
        Self(errors)
    }

    pub fn extend(&mut self, errors: ValidationErrors) {
        self.0.extend(errors.0);
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl fmt::Debug for ValidationErrors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&miette::Report::new(self.clone()), f)
    }
}
