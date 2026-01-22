use crate::lowering::ctx::Ctx;
use crate::lowering::edge::lower_edge_definition;
use crate::lowering::error::LoweringErrors;
use crate::lowering::extern_funcs::lower_extern_definition;
use crate::lowering::facts::lower_fact_definition;
use crate::lowering::import::lower_import;
use crate::lowering::node::lower_node_definition;
use crate::lowering::query::lower_query_definition;
use crate::lowering::type_decl::lower_type_declaration;
use crate::module_loader::Source;
use crate::pdl;
use crate::spanned::{FileId, Location, Span};
use crate::{ast::*, lowering::error::LoweringError};
use tree_sitter::TreeCursor;
use type_sitter::{HasChildren, Node};

pub fn lower_module(
    tree: type_sitter::Tree<pdl::SourceFile<'static>>,
    source: &Source,
    file_id: FileId,
) -> (Module, LoweringErrors) {
    let ctx = Ctx::new(source, file_id);

    let root = match tree.root_node() {
        Ok(r) => r,
        Err(e) => {
            return (
                Module::default(),
                LoweringErrors::new(vec![LoweringError::from_incorrect_kind(e, source, &ctx)]),
            );
        }
    };

    let mut errors = Vec::new();
    let mut imports = Vec::new();
    let mut facts = Vec::new();
    let mut types = Vec::new();
    let mut externs = Vec::new();
    let mut queries = Vec::new();
    let mut edges = Vec::new();
    let mut nodes = Vec::new();

    let mut cursor = root.walk();

    for child_res in root.children(&mut cursor) {
        use pdl::anon_unions::Anon314737192860065104467245150540293684006 as U;

        match child_res {
            Ok(child_union) => {
                if child_union.raw().has_error() {
                    errors.extend(LoweringError::collect_from_tree(
                        *child_union.raw(),
                        source,
                        &ctx,
                    ));
                }

                match child_union {
                    U::ImportDefinition(node) => match lower_import(&ctx, node) {
                        Ok(i) => imports.push(i),
                        Err(e) => errors.push(LoweringError::from_incorrect_kind(e, source, &ctx)),
                    },
                    U::FactDefinition(node) => match lower_fact_definition(&ctx, node) {
                        Ok(f) => facts.push(f),
                        Err(e) => errors.push(LoweringError::from_incorrect_kind(e, source, &ctx)),
                    },
                    U::TypeDeclaration(node) => match lower_type_declaration(&ctx, node) {
                        Ok(t) => types.push(t),
                        Err(e) => errors.push(LoweringError::from_incorrect_kind(e, source, &ctx)),
                    },
                    U::ExternDefinition(node) => match lower_extern_definition(&ctx, node) {
                        Ok(e) => externs.push(e),
                        Err(err) => {
                            errors.push(LoweringError::from_incorrect_kind(err, source, &ctx))
                        }
                    },
                    U::QueryDefinition(node) => match lower_query_definition(&ctx, node) {
                        Ok(e) => queries.push(e),
                        Err(err) => {
                            errors.push(LoweringError::from_incorrect_kind(err, source, &ctx))
                        }
                    },
                    U::EdgeDefinition(node) => match lower_edge_definition(&ctx, node) {
                        Ok(e) => edges.push(e),
                        Err(err) => {
                            errors.push(LoweringError::from_incorrect_kind(err, source, &ctx))
                        }
                    },
                    U::NodeDefinition(node) => match lower_node_definition(&ctx, node) {
                        Ok(e) => nodes.push(e),
                        Err(err) => {
                            errors.push(LoweringError::from_incorrect_kind(err, source, &ctx))
                        }
                    },
                }
            }
            Err(incorrect_kind) => {
                if incorrect_kind.node.has_error() || incorrect_kind.node.is_error() {
                    errors.extend(LoweringError::collect_from_tree(
                        *incorrect_kind.node.raw(),
                        source,
                        &ctx,
                    ));
                } else {
                    errors.push(LoweringError::from_incorrect_kind(
                        incorrect_kind,
                        source,
                        &ctx,
                    ));
                }
            }
        }
    }

    if root.raw().has_error() && errors.is_empty() {
        errors.extend(LoweringError::collect_from_tree(*root.raw(), source, &ctx));
    }

    (
        Module {
            imports,
            facts,
            externs,
            types,
            queries,
            file_id,
            edges,
            nodes,
        },
        LoweringErrors::new(errors),
    )
}
