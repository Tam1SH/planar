use crate::checked::Checked;
use crate::lowering::ctx::Ctx;
use crate::lowering::edge::lower_edge_definition;
use crate::lowering::error::LoweringErrors;
use crate::lowering::extern_funcs::lower_extern_definition;
use crate::lowering::facts::lower_fact_definition;
use crate::lowering::grammar::lower_grammar_declaration;
use crate::lowering::import::lower_import;
use crate::lowering::node::lower_node_definition;
use crate::lowering::query::lower_query_definition;
use crate::lowering::type_decl::lower_type_declaration;
use crate::module_loader::Source;
use crate::pdl;
use crate::source_registry::MietteSource;
use crate::spanned::{FileId, Location, Span};
use crate::{ast::*, lowering::error::LoweringError};
use tree_sitter::TreeCursor;
use type_sitter::{HasChildren, Node};

pub fn lower_module(
    tree: type_sitter::Tree<pdl::SourceFile<'static>>,
    source: MietteSource,
    file_id: FileId,
) -> Checked<Module, LoweringErrors> {
    let ctx = Ctx::new(source.clone(), file_id);

    let root = match tree.root_node() {
        Ok(r) => r,
        Err(e) => {
            return Checked::with_errors(
                Module::default(),
                LoweringErrors::new(vec![LoweringError::from_incorrect_kind(e, source, &ctx)]),
            );
        }
    };

    let mut mod_def = Module {
        file_id,
        ..Default::default()
    };
    let mut errors = LoweringErrors::default();

    macro_rules! process {
        ($node:expr, $lower_fn:expr, $target:expr) => {
            match $lower_fn(&ctx, $node) {
                Ok(item) => $target.push(item),
                Err(e) => errors.push(LoweringError::from_incorrect_kind(e, source.clone(), &ctx)),
            }
        };
        ($node:expr, $lower_fn:expr, set $target:expr) => {
            match $lower_fn(&ctx, $node) {
                Ok(item) => $target = Some(item),
                Err(e) => errors.push(LoweringError::from_incorrect_kind(e, source.clone(), &ctx)),
            }
        };
    }

    let mut cursor = root.walk();
    for child_res in root.children(&mut cursor) {
        use pdl::anon_unions::Anon331116562354213157727272504972515459572 as U;

        let raw_node = match &child_res {
            Ok(u) => *u.raw(),
            Err(e) => *e.node.raw(),
        };

        if raw_node.has_error() {
            errors.extend(LoweringError::collect_from_tree(
                raw_node,
                source.clone(),
                &ctx,
            ));
        }

        match child_res {
            Ok(u) => match u {
                U::GrammarDeclaration(n) => {
                    process!(n, lower_grammar_declaration, set mod_def.grammar)
                }
                U::ImportDefinition(n) => process!(n, lower_import, mod_def.imports),
                U::FactDefinition(n) => process!(n, lower_fact_definition, mod_def.facts),
                U::TypeDeclaration(n) => process!(n, lower_type_declaration, mod_def.types),
                U::ExternDefinition(n) => process!(n, lower_extern_definition, mod_def.externs),
                U::QueryDefinition(n) => process!(n, lower_query_definition, mod_def.queries),
                U::EdgeDefinition(n) => process!(n, lower_edge_definition, mod_def.edges),
                U::NodeDefinition(n) => process!(n, lower_node_definition, mod_def.nodes),
            },
            Err(e) if !raw_node.has_error() => {
                errors.push(LoweringError::from_incorrect_kind(e, source.clone(), &ctx));
            }
            _ => {}
        }
    }

    if root.raw().has_error() && errors.is_empty() {
        errors.extend(LoweringError::collect_from_tree(*root.raw(), source, &ctx));
    }

    Checked::with_errors(mod_def, errors)
}
