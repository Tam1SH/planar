use crate::ast::{QueryDefinition, Visibility};
use crate::lowering::common::pub_vis_to_vis;
use crate::lowering::ctx::Ctx;
use crate::pdl;
use crate::spanned::Spanned;
use type_sitter::{Node, NodeResult};

pub fn lower_query_definition<'a>(
    ctx: &Ctx<'a>,
    node: pdl::QueryDefinition<'a>,
) -> NodeResult<'a, Spanned<QueryDefinition>> {
    let name_node = node.name()?;
    let name = ctx.spanned(&name_node, ctx.text(&name_node));

    let grammar_node = node.grammar()?;
    let grammar = ctx.spanned(&grammar_node, ctx.text(&grammar_node));

    let value_node = node.value()?;
    let content_text = if let Some(content_res) = value_node.content() {
        ctx.text(&content_res?)
    } else {
        String::new()
    };

    let vis = if let Some(r#pub) = node.r#pub() {
        let r#pub = r#pub?;
        pub_vis_to_vis(r#pub)?
    } else {
        Visibility::Private
    };

    let value = ctx.spanned(&value_node, content_text);

    Ok(ctx.spanned(
        &node,
        QueryDefinition {
            name,
            grammar,
            value,
            vis,
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
            "query includePattern: grammars.nginx = `include (string)@path;` lines",
            as_query_definition,
            lower_query_definition
        );
    }

    #[test]
    fn test_empty_query() {
        assert_lower_snapshot!(
            "query empty: some.lang = ``",
            as_query_definition,
            lower_query_definition
        );
    }
}
