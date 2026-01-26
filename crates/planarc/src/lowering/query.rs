use crate::ast::{QueryDefinition, Visibility};
use crate::lowering::common::{extract_query_data, pub_vis_to_vis};
use crate::lowering::ctx::Ctx;
use crate::pdl;
use crate::spanned::{FileId, Location, Span, Spanned};
use tree_sitter::Parser;
use type_sitter::{IncorrectKind, Node, NodeResult, UntypedNode};

pub fn lower_query_definition<'a>(
    ctx: &Ctx,
    node: pdl::QueryDefinition<'a>,
) -> NodeResult<'a, Spanned<QueryDefinition>> {
    let name_node = node.name()?;
    let name = ctx.spanned(&name_node, ctx.text(&name_node));

    let value_node = node.value()?;

    let (content_text, captures) = extract_query_data(ctx, &value_node)?;

    let vis = if let Some(r#pub) = node.r#pub() {
        let r#pub = r#pub?;
        pub_vis_to_vis(r#pub)?
    } else {
        Visibility::Private
    };

    Ok(ctx.spanned(
        &node,
        QueryDefinition {
            name,
            value: content_text,
            vis,
            captures,
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_lower_snapshot;

    #[test]
    fn test_query_definition() {
        assert_lower_snapshot!(
            "query includePattern = `include (string)@path;`",
            as_query_definition,
            lower_query_definition
        );
    }

    #[test]
    fn test_empty_query() {
        assert_lower_snapshot!(
            "query empty = ` `",
            as_query_definition,
            lower_query_definition
        );
    }

    #[test]
    fn test_query_definition_with_captures() {
        assert_lower_snapshot!(
            "query find_stuff = `
                (function_item 
                    name: (identifier) @fn.name
                    body: (block) @fn.body
                ) ; @this_is_ignored
            `",
            as_query_definition,
            lower_query_definition
        );
    }

    #[test]
    fn test_query_no_captures() {
        assert_lower_snapshot!(
            "query empty = `(node)`",
            as_query_definition,
            lower_query_definition
        );
    }
}
