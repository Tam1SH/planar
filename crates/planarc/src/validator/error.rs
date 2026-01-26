use std::fmt;

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::{
    error::{ErrorCollection, ErrorWithLocation},
    impl_diagnostic_with_location,
    source_registry::MietteSource,
    spanned::Location,
};

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
        src: MietteSource,
        loc: Location,
    },

    #[error("Refinements are not allowed in wit-compatible types")]
    #[diagnostic(code(pdl::codegen::wit_no_refinement))]
    WitRefinementDisallowed {
        #[label("refinement found here")]
        span: SourceSpan,
        #[source_code]
        src: MietteSource,
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
        src: MietteSource,
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
        src: MietteSource,
        loc: Location,
    },

    #[error("Tree-sitter query syntax error: {message}")]
    #[diagnostic(code(pdl::validator::query_syntax_error))]
    InvalidQuerySyntax {
        message: String,
        #[label("parsing of tree-sitter query failed")]
        span: SourceSpan,
        #[source_code]
        src: MietteSource,
        loc: Location,
    },
    #[error("Untyped query: no grammar specified for this file")]
    #[diagnostic(
        code(pdl::validator::untyped_query),
        help(
            "Add 'using grammars.<lang>' at the top of the file to enable S-expression validation."
        )
    )]
    UntypedQuery {
        #[source_code]
        src: MietteSource,
        #[label("this query requires a grammar")]
        span: SourceSpan,
        loc: Location,
    },
}

pub type ValidationErrors = ErrorCollection<ValidationError>;

impl_diagnostic_with_location!(ValidationError, {
    ValidationError::WitIncompatibility,
    ValidationError::WitRefinementDisallowed,
    ValidationError::InvalidGrammarNamespace,
    ValidationError::GrammarNotFound,
    ValidationError::InvalidQuerySyntax,
    ValidationError::UntypedQuery
});
