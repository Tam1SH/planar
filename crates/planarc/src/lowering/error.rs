use crate::{
    compiler::error::{DiagnosticWithLocation, ErrorWithLocation},
    lowering::ctx::Ctx,
    module_loader::Source,
    spanned::Location,
};
use miette::{Diagnostic, NamedSource, SourceSpan};
use std::fmt;
use thiserror::Error;
use type_sitter::{Node, UntypedNode};

#[derive(Clone, Debug, Error, Diagnostic)]
pub enum LoweringError {
    #[error("Unexpected syntax: expected {expected}, found {found}")]
    #[diagnostic(code(pdl::lowering::unexpected_syntax))]
    UnexpectedKind {
        #[source_code]
        src: NamedSource<String>,

        #[label("expected {expected}")]
        span: SourceSpan,

        loc: Location,

        expected: String,
        found: String,
    },

    #[error("Syntax error: {message}")]
    #[diagnostic(code(pdl::lowering::parse_error))]
    SyntaxError {
        #[source_code]
        src: NamedSource<String>,

        loc: Location,

        #[label("here")]
        span: SourceSpan,

        message: String,
    },
}

#[derive(Clone, Error, Diagnostic)]
#[error("Found {} lowering errors", .0.len())]
pub struct LoweringErrors(#[related] pub Vec<LoweringError>);

impl ErrorWithLocation for LoweringError {
    fn location(&self) -> Location {
        match &self {
            LoweringError::UnexpectedKind { loc, .. } => *loc,
            LoweringError::SyntaxError { loc, .. } => *loc,
        }
    }
}

impl LoweringErrors {
    pub fn new(errors: Vec<LoweringError>) -> Self {
        Self(errors)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn merge(&mut self, other: Self) {
        self.0.extend(other.0);
    }
}

impl fmt::Debug for LoweringErrors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&miette::Report::new(self.clone()), f)
    }
}

impl LoweringError {
    pub fn from_incorrect_kind(
        err: type_sitter::IncorrectKind<'_>,
        source: &Source,
        ctx: &Ctx<'_>,
    ) -> Self {
        let node = err.node;
        let range = node.range();

        Self::UnexpectedKind {
            src: NamedSource::new(source.origin.clone(), source.content.clone()),
            span: SourceSpan::new(range.start_byte.into(), range.end_byte - range.start_byte),
            expected: err.kind.to_string(),
            found: node.kind().to_string(),
            loc: ctx.location(&node),
        }
    }

    pub fn syntax_error(node: tree_sitter::Node, source: &Source, ctx: &Ctx<'_>) -> Self {
        let range = node.range();
        let bytes = source.content.as_bytes();
        let full_text = node.utf8_text(bytes).unwrap_or("");

        let message = if node.is_missing() {
            format!("expected '{}'", node.kind())
        } else {
            let display_text = if full_text.len() > 30 {
                format!("{}...", &full_text[..30].replace('\n', " "))
            } else {
                full_text.to_string()
            };

            if display_text.trim().is_empty() {
                format!("unexpected token '{}' (or end of file)", node.kind())
            } else {
                format!("unexpected token '{}'", display_text)
            }
        };

        Self::SyntaxError {
            src: NamedSource::new(source.origin.clone(), source.content.clone()),
            span: SourceSpan::new(range.start_byte.into(), range.end_byte - range.start_byte),
            message,
            loc: ctx.location(&UntypedNode::new(node)),
        }
    }

    pub fn collect_from_tree(root: tree_sitter::Node, source: &Source, ctx: &Ctx<'_>) -> Vec<Self> {
        let mut errors = Vec::new();
        let mut cursor = root.walk();
        recursive_find_errors(&mut cursor, source, &mut errors, ctx);
        errors
    }
}

fn recursive_find_errors(
    cursor: &mut tree_sitter::TreeCursor,
    source: &Source,
    errors: &mut Vec<LoweringError>,
    ctx: &Ctx<'_>,
) {
    let node = cursor.node();

    if node.is_missing() {
        errors.push(LoweringError::syntax_error(node, source, ctx));
        return;
    }

    if node.is_error() {
        if cursor.goto_first_child() {
            let mut found_child_error = false;

            loop {
                let child = cursor.node();
                if child.is_error() || child.is_missing() || child.has_error() {
                    let start_len = errors.len();
                    recursive_find_errors(cursor, source, errors, ctx);
                    if errors.len() > start_len {
                        found_child_error = true;
                    }
                }

                if !cursor.goto_next_sibling() {
                    break;
                }
            }
            cursor.goto_parent();

            if !found_child_error {
                errors.push(LoweringError::syntax_error(node, source, ctx));
            }
            return;
        } else {
            errors.push(LoweringError::syntax_error(node, source, ctx));
            return;
        }
    }

    if node.has_error() && cursor.goto_first_child() {
        loop {
            recursive_find_errors(cursor, source, errors, ctx);
            if !cursor.goto_next_sibling() {
                break;
            }
        }
        cursor.goto_parent();
    }
}
