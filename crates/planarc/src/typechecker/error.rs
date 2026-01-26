use crate::error::ErrorCollection;
use crate::impl_diagnostic_with_location;
use crate::source_registry::MietteSource;
use crate::spanned::Location;
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Clone, Debug, Error, Diagnostic)]
pub enum TypeError {
    #[error("Unknown symbol or variable '{name}'")]
    #[diagnostic(
        code(pdl::type_check::unknown_symbol),
        help("Check if the variable is defined in a 'let' binding or available as a '@capture' in the current match.")
    )]
    UnknownSymbol {
        name: String,
        #[source_code]
        src: MietteSource,
        #[label("variable '{name}' not found in this scope")]
        span: SourceSpan,
        loc: Location,
    },

    #[error("Type mismatch: expected '{expected}', found '{found}'")]
    #[diagnostic(code(pdl::type_check::type_mismatch))]
    TypeMismatch {
        expected: String,
        found: String,
        #[source_code]
        src: MietteSource,
        #[label("expected {expected}, but this expression has type {found}")]
        span: SourceSpan,
        loc: Location,
    },

    #[error("Undefined field '{field_name}' for fact '{fact_name}'")]
    #[diagnostic(code(pdl::type_check::undefined_field))]
    UndefinedField {
        fact_name: String,
        field_name: String,
        #[source_code]
        src: MietteSource,
        #[label("field '{field_name}' not found in definition of '{fact_name}'")]
        span: SourceSpan,
        loc: Location,
    },

    #[error("Edge '{edge_name}' endpoint mismatch")]
    #[diagnostic(
        code(pdl::type_check::edge_endpoint_mismatch),
        help("The edge '{edge_name}' is defined as '{expected_from} -> {expected_to}'.")
    )]
    EdgeEndpointMismatch {
        edge_name: String,
        expected_from: String,
        expected_to: String,
        actual_from: String,
        actual_to: String,
        #[source_code]
        src: MietteSource,
        #[label("this emit doesn't match the edge definition")]
        span: SourceSpan,
        loc: Location,
    },

    #[error("Operator '{op}' is not defined for types '{left_ty}' and '{right_ty}'")]
    #[diagnostic(code(pdl::type_check::operator_undefined))]
    OperatorUndefined {
        op: String,
        left_ty: String,
        right_ty: String,
        #[source_code]
        src: MietteSource,
        #[label("no implementation for {left_ty} {op} {right_ty}")]
        span: SourceSpan,
        loc: Location,
    },

    #[error("'{name}' is not a function and cannot be called")]
    #[diagnostic(code(pdl::type_check::not_a_function))]
    NotAFunction {
        name: String,
        found_type: String,
        #[source_code]
        src: MietteSource,
        #[label("this has type {found_type}")]
        span: SourceSpan,
        loc: Location,
    },

    #[error("Function '{name}' expects {expected} arguments, but got {found}")]
    #[diagnostic(code(pdl::type_check::argument_count_mismatch))]
    ArgumentCountMismatch {
        name: String,
        expected: usize,
        found: usize,
        #[source_code]
        src: MietteSource,
        #[label("expected {expected} args")]
        span: SourceSpan,
        loc: Location,
    },
}

pub type TypeErrors = ErrorCollection<TypeError>;

impl_diagnostic_with_location!(TypeError, {
    TypeError::ArgumentCountMismatch,
    TypeError::NotAFunction,
    TypeError::OperatorUndefined,
    TypeError::EdgeEndpointMismatch,
    TypeError::UndefinedField,
    TypeError::TypeMismatch,
    TypeError::UnknownSymbol
});
