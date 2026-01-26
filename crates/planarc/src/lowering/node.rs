use crate::ast::{
    BlockStatement, Capture, EmitStatement, EmittedFact, EmittedFieldAssignment, Expression,
    LetBinding, MatchQueryReference, MatchStatement, NodeDefinition, NodeStatement,
    RelationDirection, Visibility,
};
use crate::lowering::common::{extract_query_data, pub_vis_to_vis};
use crate::lowering::ctx::Ctx;
use crate::lowering::query::lower_query_definition;
use crate::lowering::type_decl::{lower_expression_atom, lower_expression_list};
use crate::pdl;
use crate::spanned::Spanned;
use type_sitter::{HasChildren, IncorrectKind, Node, NodeResult};

pub fn lower_node_definition<'a>(
    ctx: &Ctx,
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
    ctx: &Ctx,
    node: pdl::MatchStmt<'a>,
) -> NodeResult<'a, Spanned<MatchStatement>> {
    let query_union = node.query()?;

    use pdl::anon_unions::Identifier_QueryLiteral as QueryRef;
    let query_ref = match query_union {
        QueryRef::Identifier(id) => {
            ctx.spanned(&id, MatchQueryReference::Identifier(ctx.text(&id)))
        }
        QueryRef::QueryLiteral(ql) => {
            let (value, captures) = extract_query_data(ctx, &ql)?;

            ctx.spanned(&ql, MatchQueryReference::Raw { value, captures })
        }
    };

    let mut statements = Vec::new();
    let match_block = node.match_block()?;
    let mut cursor = match_block.walk();

    for child_res in match_block.children(&mut cursor) {
        statements.push(lower_block_statement(ctx, child_res?)?);
    }

    Ok(ctx.spanned(
        &node,
        MatchStatement {
            query_ref,
            statements,
        },
    ))
}

fn lower_emit<'a>(ctx: &Ctx, node: pdl::Emit<'a>) -> NodeResult<'a, EmitStatement> {
    let left_node = node.left_fact()?;
    let left = lower_emitted_fact(ctx, left_node)?;

    let right = node
        .right_fact()
        .transpose()?
        .map(|rn| lower_emitted_fact(ctx, rn))
        .transpose()?;

    let mut relation = None;
    let mut direction = None;

    if let Some(rel_node) = node.relation().transpose()? {
        relation = Some(ctx.spanned(&rel_node.fqmn()?, ctx.text(&rel_node.fqmn()?)));

        let has_left = rel_node.left().is_some();
        let has_right = rel_node.right().is_some();

        direction = Some(match (has_left, has_right) {
            (true, true) => RelationDirection::Both,
            (true, false) => RelationDirection::Left,
            (false, true) => RelationDirection::Right,
            (false, false) => return Err(IncorrectKind::new::<pdl::Relation>(*rel_node.raw())),
        });
    }

    Ok(EmitStatement {
        left,
        right,
        relation,
        direction,
    })
}

fn lower_block_statement<'a>(
    ctx: &Ctx,
    node: pdl::anon_unions::Capture_Emit_LetBind<'a>,
) -> NodeResult<'a, Spanned<BlockStatement>> {
    use pdl::anon_unions::Capture_Emit_LetBind as Child;
    Ok(match node {
        Child::Capture(cap_node) => {
            let cap = lower_capture(ctx, cap_node)?;
            ctx.spanned(&cap_node, BlockStatement::Capture(cap))
        }
        Child::LetBind(let_node) => {
            let binding = lower_let_bind(ctx, let_node)?;
            ctx.spanned(&let_node, BlockStatement::Let(binding))
        }
        Child::Emit(emit_node) => {
            let emit = lower_emit(ctx, emit_node)?;
            ctx.spanned(&emit_node, BlockStatement::Emit(emit))
        }
    })
}

fn lower_capture<'a>(ctx: &Ctx, node: pdl::Capture<'a>) -> NodeResult<'a, Capture> {
    let mut name = None;

    let mut cursor = node.walk();
    let mut statements = Vec::new();

    for child_res in node.children(&mut cursor) {
        use pdl::anon_unions::CapIdentifier_CaptureBlock as CapChild;
        match child_res? {
            CapChild::CapIdentifier(id_node) => {
                name = Some(ctx.spanned(&id_node, ctx.text(&id_node)));
            }
            CapChild::CaptureBlock(block_node) => {
                let mut block_cursor = block_node.walk();

                for stmt_res in block_node.children(&mut block_cursor) {
                    statements.push(lower_block_statement(ctx, stmt_res?)?);
                }
            }
        }
    }

    Ok(Capture {
        name: name.unwrap_or_else(|| ctx.spanned(&node, "missing_name".to_string())),
        statements,
    })
}

fn lower_let_bind<'a>(ctx: &Ctx, node: pdl::LetBind<'a>) -> NodeResult<'a, LetBinding> {
    let id_node = node.identifier()?;
    let name = ctx.spanned(&id_node, ctx.text(&id_node));

    let mut expr_cursor = node.walk();
    let expr_node = node.expressions(&mut expr_cursor).next()
        .ok_or_else(|| IncorrectKind::new::<pdl::anon_unions::Fqmn_InExpression_It_Number_OperatorIdentifier_ParenthesizedExpression_String>(*node.raw()))??;

    let value = lower_expression_atom(ctx, expr_node)?;

    Ok(LetBinding { name, value })
}

fn lower_emitted_fact<'a>(
    ctx: &Ctx,
    node: pdl::EmmitedFact<'a>,
) -> NodeResult<'a, Spanned<EmittedFact>> {
    let mut type_name = None;
    let mut fields = Vec::new();
    let mut cursor = node.walk();

    for child_res in node.children(&mut cursor) {
        use pdl::anon_unions::EmmitedFactField_TypeIdentifier as FactChild;
        match child_res? {
            FactChild::TypeIdentifier(t) => {
                type_name = Some(ctx.spanned(&t, ctx.text(&t)));
            }
            FactChild::EmmitedFactField(f) => {
                fields.push(lower_emitted_field_assignment(ctx, f)?);
            }
        }
    }

    let type_name = type_name
        .ok_or_else(|| ::type_sitter::IncorrectKind::new::<pdl::TypeIdentifier>(*node.raw()))?;

    Ok(ctx.spanned(&node, EmittedFact { type_name, fields }))
}

fn lower_emitted_field_assignment<'a>(
    ctx: &Ctx,
    node: pdl::EmmitedFactField<'a>,
) -> NodeResult<'a, Spanned<EmittedFieldAssignment>> {
    let name = node.field().map(|f| ctx.spanned(&f, ctx.text(&f)))?;
    let value = node.value().map(|v| lower_expression_atom(ctx, v))??;

    Ok(ctx.spanned(&node, EmittedFieldAssignment { name, value }))
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
