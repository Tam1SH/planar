use type_sitter::{HasChild, HasChildren, Node, NodeResult};

use crate::{
    ast::{Attribute, Expression, Visibility},
    lowering::{
        ctx::Ctx,
        type_decl::{lower_expression_atom, lower_expression_list},
    },
    pdl::{self, Pub},
    spanned::Spanned,
};

pub fn lower_attribute<'a>(
    ctx: &Ctx<'a>,
    node: pdl::Attribute<'a>,
) -> NodeResult<'a, Spanned<Attribute>> {
    let name = &node.name()?;
    let name = ctx.spanned(name, ctx.text(name));

    let mut cursor = node.walk();

    Ok(ctx.spanned(&node, Attribute { name }))
}

pub fn lower_refinement_node<'a>(
    ctx: &Ctx<'a>,
    node: pdl::Refinement<'a>,
) -> NodeResult<'a, Spanned<Expression>> {
    let mut cursor = node.walk();
    let mut parts = Vec::new();

    parts.push(lower_expression_list(ctx, node.children(&mut cursor))?);

    if parts.is_empty() {
        return Ok(ctx.spanned(&node, Expression::Identifier("MISSING".to_string())));
    }

    if parts.len() == 1 {
        Ok(parts.pop().unwrap())
    } else {
        let mut items = parts.into_iter();
        let head = Box::new(items.next().unwrap());
        let args = items.collect();

        Ok(ctx.spanned(
            &node,
            Expression::Call {
                function: head,
                args,
            },
        ))
    }
}

pub fn pub_vis_to_vis<'a>(pub_vis: Pub<'a>) -> NodeResult<'a, Visibility> {
    if let Some(pkg) = pub_vis.pkg() {
        let _ = pkg?;
        Ok(Visibility::Package)
    } else {
        Ok(Visibility::Pub)
    }
}

pub fn lower_in_expression<'a>(
    ctx: &Ctx<'a>,
    node: pdl::InExpression<'a>,
) -> NodeResult<'a, Spanned<Expression>> {
    let child = node.child()?;

    match child {
        pdl::anon_unions::ListItems_Range::ListItems(list_node) => {
            let mut cursor = list_node.walk();
            let mut elements = Vec::new();

            for item_res in list_node.children(&mut cursor) {
                elements.push(lower_expression_atom(ctx, item_res?)?);
            }

            Ok(ctx.spanned(&node, Expression::InList(elements)))
        }
        pdl::anon_unions::ListItems_Range::Range(range_node) => {
            let start = Box::new(lower_expression_atom(ctx, range_node.start()?)?);
            let end = if let Some(end_node) = range_node.end() {
                Some(Box::new(lower_expression_atom(ctx, end_node?)?))
            } else {
                None
            };

            Ok(ctx.spanned(&node, Expression::InRange { start, end }))
        }
    }
}
