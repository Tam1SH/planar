use crate::ast::{EdgeDefinition, Visibility};
use crate::lowering::common::pub_vis_to_vis;
use crate::lowering::ctx::Ctx;
use crate::pdl;
use crate::spanned::Spanned;
use type_sitter::{Node, NodeResult};

pub fn lower_edge_definition<'a>(
    ctx: &Ctx<'a>,
    node: pdl::EdgeDefinition<'a>,
) -> NodeResult<'a, Spanned<EdgeDefinition>> {
    let name_node = node.name()?;
    let from_node = node.from()?;
    let to_node = node.to()?;
    let mut rel_node = None;

    let mut vis = Visibility::Private;

    let mut c = node.walk();
    for item in node.others(&mut c) {
        match item? {
            pdl::anon_unions::Pub_Relation::Pub(pub_vis) => {
                vis = pub_vis_to_vis(pub_vis)?;
            }
            pdl::anon_unions::Pub_Relation::Relation(relation) => rel_node = Some(relation),
        }
    }

    let name = ctx.spanned(&name_node, ctx.text(&name_node));
    let from = ctx.spanned(&from_node, ctx.text(&from_node));
    let to = ctx.spanned(&to_node, ctx.text(&to_node));
    let relation = ctx.spanned(
        &rel_node.expect("relation not found"),
        ctx.text(&rel_node.expect("relation not found")),
    );

    Ok(ctx.spanned(
        &node,
        EdgeDefinition {
            name,
            from,
            to,
            relation,
            vis,
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_lower_snapshot;

    #[test]
    fn test_edge_lowering() {
        assert_lower_snapshot!(
            "edge Friend = User -> User",
            as_edge_definition,
            lower_edge_definition
        );
    }
}
