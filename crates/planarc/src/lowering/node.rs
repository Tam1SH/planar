use crate::ast::{MatchQueryReference, MatchStatement, NodeDefinition, NodeStatement, Visibility};
use crate::lowering::common::pub_vis_to_vis;
use crate::lowering::ctx::Ctx;
use crate::lowering::query::lower_query_definition;
use crate::pdl;
use crate::spanned::Spanned;
use type_sitter::{HasChildren, Node, NodeResult};

pub fn lower_node_definition<'a>(
    ctx: &Ctx<'a>,
    node: pdl::NodeDefinition<'a>,
) -> NodeResult<'a, Spanned<NodeDefinition>> {
    let kind_node = node.kind()?;
    let kind = ctx.spanned(&kind_node, ctx.text(&kind_node));

    let mut statements = Vec::new();
    let mut vis = Visibility::Private;

    let mut cursor = node.walk();

    for child_res in node.others(&mut cursor) {
        use pdl::anon_unions::Block_Pub as Child;

        match child_res? {
            Child::Block(block_node) => {
                let mut block_cursor = block_node.walk();

                for stmt_res in block_node.children(&mut block_cursor) {
                    use pdl::anon_unions::MatchStmt_QueryDefinition as Stmt;
                    let stmt_node = stmt_res?;
                    match stmt_node {
                        Stmt::MatchStmt(m) => {
                            let s = lower_match_stmt(ctx, m)?;
                            statements.push(ctx.spanned(&m, NodeStatement::Match(s)));
                        }
                        Stmt::QueryDefinition(q) => {
                            let s = lower_query_definition(ctx, q)?;
                            statements.push(ctx.spanned(&q, NodeStatement::Query(s)));
                        }
                    }
                }
            }
            Child::Pub(pub_node) => {
                vis = pub_vis_to_vis(pub_node)?;
            }
        }
    }

    Ok(ctx.spanned(
        &node,
        NodeDefinition {
            kind,
            statements,
            vis,
        },
    ))
}

fn lower_match_stmt<'a>(
    ctx: &Ctx<'a>,
    node: pdl::MatchStmt<'a>,
) -> NodeResult<'a, Spanned<MatchStatement>> {
    let query_union = node.query()?;

    use pdl::anon_unions::Identifier_QueryLiteral as QueryRef;

    let query_ref = match query_union {
        QueryRef::Identifier(id) => {
            ctx.spanned(&id, MatchQueryReference::Identifier(ctx.text(&id)))
        }
        QueryRef::QueryLiteral(query_literal) => {
            let raw = if let Some(query_literal) = query_literal.content() {
                ctx.text(&query_literal?)
            } else {
                "".to_string()
            };

            ctx.spanned(&query_literal, MatchQueryReference::Raw(raw))
        }
    };

    let _match_block = node.match_block()?;

    Ok(ctx.spanned(&node, MatchStatement { query_ref }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_lower_snapshot;

    #[test]
    fn test_node_with_match() {
        assert_lower_snapshot!(
            "node IncludeDirective { match includePattern { } }",
            as_node_definition,
            lower_node_definition
        );
    }
}
