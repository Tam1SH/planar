use type_sitter::{HasChild, HasChildren, IncorrectKind, Node, NodeResult};

use crate::{
    ast::{Expression, TypeAnnotation, TypeDeclaration, TypeDefinition, TypeField, Visibility},
    lowering::{
        common::{lower_attribute, lower_in_expression, lower_refinement_node, pub_vis_to_vis},
        ctx::Ctx,
    },
    pdl,
    spanned::{Location, Span, Spanned},
};

type AtomUnion<'a> =
    pdl::anon_unions::Fqmn_InExpression_It_Number_OperatorIdentifier_ParenthesizedExpression_String<
        'a,
    >;

pub fn lower_type_declaration<'a>(
    ctx: &Ctx,
    node: pdl::TypeDeclaration<'a>,
) -> NodeResult<'a, Spanned<TypeDeclaration>> {
    let name_node = node.name()?;
    let name = ctx.spanned(&name_node, ctx.text(&name_node));

    let mut attributes = Vec::new();
    let mut c = node.walk();
    let mut vis = Visibility::Private;

    for item in node.others(&mut c) {
        match item? {
            pdl::anon_unions::Attribute_Pub::Attribute(attribute) => {
                attributes.push(lower_attribute(ctx, attribute)?)
            }
            pdl::anon_unions::Attribute_Pub::Pub(pub_vis) => vis = pub_vis_to_vis(pub_vis)?,
        }
    }

    let body_node = node.body()?;
    let definition = lower_type_definition(ctx, body_node)?;

    Ok(ctx.spanned(
        &node,
        TypeDeclaration {
            attributes,
            name,
            definition,
            vis,
        },
    ))
}

fn lower_type_definition<'a>(
    ctx: &Ctx,
    node: pdl::TypeDefinition<'a>,
) -> NodeResult<'a, Spanned<TypeDefinition>> {
    let base_type = if let Ok(ty_node_res) = node.r#type() {
        Some(lower_type_annotation(ctx, ty_node_res)?)
    } else {
        None
    };

    // let mut fields = Vec::new();
    // let mut field_cursor = node.walk();
    // for field_res in node.type_field_definitions(&mut field_cursor) {
    //     fields.push(lower_type_field_definition(ctx, field_res?)?);
    // }

    Ok(ctx.spanned(&node, TypeDefinition { base_type }))
}

// fn lower_type_field_definition<'a>(
//     ctx: &Ctx,
//     node: pdl::TypeFieldDefinition<'a>,
// ) -> NodeResult<'a, Spanned<TypeField>> {
//     let name = ctx.spanned(&node.name()?, ctx.text(&node.name()?));
//     let definition = lower_type_definition(ctx, node.r#type()?)?;

//     Ok(ctx.spanned(&node, TypeField { name, definition }))
// }

pub fn lower_type_annotation<'a>(
    ctx: &Ctx,
    node: pdl::TypeAnnotation<'a>,
) -> NodeResult<'a, TypeAnnotation> {
    use pdl::anon_unions::Refinement_TypeAnnotation_TypeApplication_TypeIdentifier as U;

    let mut name = None;
    let mut args = Vec::new();
    let mut refinement = None;

    let mut cursor = node.walk();

    for child_res in node.children(&mut cursor) {
        match child_res? {
            U::TypeIdentifier(n) => {
                let fqmn = n.fqmn()?;
                name = Some(ctx.spanned(&fqmn, ctx.text(&fqmn)));
            }
            U::TypeApplication(n) => {
                let constr = n.constructor()?;
                let fqmn = constr.fqmn()?;
                name = Some(ctx.spanned(&fqmn, ctx.text(&fqmn)));

                let mut arg_cursor = n.walk();
                for arg_u_res in n.arguments(&mut arg_cursor) {
                    let arg_u = arg_u_res?;

                    if let Some(ty_ann_node) = arg_u.as_type_annotation() {
                        args.push(
                            ctx.spanned(&ty_ann_node, lower_type_annotation(ctx, ty_ann_node)?),
                        );
                    }
                }
            }
            U::Refinement(n) => {
                refinement = Some(lower_refinement_node(ctx, n)?);
            }
            U::TypeAnnotation(n) => {
                return lower_type_annotation(ctx, n);
            }
        }
    }

    let name = name.ok_or_else(|| IncorrectKind::new::<pdl::TypeIdentifier>(*node.raw()))?;

    Ok(TypeAnnotation {
        name,
        args,
        refinement,
    })
}

pub fn lower_expression_atom<'a>(
    ctx: &Ctx,
    u: AtomUnion<'a>,
) -> NodeResult<'a, Spanned<Expression>> {
    match u {
        AtomUnion::It(n) => Ok(ctx.spanned(&n, Expression::It)),
        AtomUnion::Fqmn(n) => Ok(ctx.spanned(&n, Expression::Identifier(ctx.text(&n)))),
        AtomUnion::Number(n) => Ok(ctx.spanned(&n, Expression::Number(ctx.text(&n)))),
        AtomUnion::String(n) => Ok(ctx.spanned(&n, Expression::StringLit(ctx.text(&n)))),
        AtomUnion::OperatorIdentifier(n) => {
            Ok(ctx.spanned(&n, Expression::OperatorIdentifier(ctx.text(&n))))
        }
        AtomUnion::InExpression(n) => lower_in_expression(ctx, n),
        AtomUnion::ParenthesizedExpression(n) => {
            let mut cursor = n.walk();
            lower_expression_list(ctx, &n, n.children(&mut cursor))
        }
    }
}

pub fn lower_expression_list<'a, I, TNode>(
    ctx: &Ctx,
    parent: &TNode,
    nodes: I,
) -> NodeResult<'a, Spanned<Expression>>
where
    TNode: Node<'a>,
    I: Iterator<Item = NodeResult<'a, AtomUnion<'a>>>,
{
    let mut atoms = Vec::new();
    for node_res in nodes {
        let node = node_res?;

        let is_op = matches!(
            node,
            AtomUnion::OperatorIdentifier(_) | AtomUnion::InExpression(_)
        );
        let expr = lower_expression_atom(ctx, node)?;
        atoms.push((expr, is_op));
    }

    if atoms.is_empty() {
        return Err(IncorrectKind::new::<TNode>(*parent.raw()));
    }

    let mut grouped: Vec<(Spanned<Expression>, bool)> = Vec::new();
    let mut current_call: Vec<Spanned<Expression>> = Vec::new();

    for (expr, is_op) in atoms {
        if is_op {
            if !current_call.is_empty() {
                grouped.push((fold_call(ctx, parent, &mut current_call)?, false));
            }
            grouped.push((expr, true));
        } else {
            current_call.push(expr);
        }
    }
    if !current_call.is_empty() {
        grouped.push((fold_call(ctx, parent, &mut current_call)?, false));
    }

    let mut it = grouped.into_iter();
    let (mut result, _) = it
        .next()
        .ok_or_else(|| IncorrectKind::new::<TNode>(*parent.raw()))?;

    while let Some((op_expr, _)) = it.next() {
        let loc = op_expr.loc;

        let (op_name, right_part, final_loc) = match op_expr.value {
            Expression::InList(_) | Expression::InRange { .. } => {
                let loc = merge_locations(result.loc, op_expr.loc);
                ("in".to_string(), op_expr, loc)
            }

            Expression::OperatorIdentifier(op_str) => {
                let (right, _) = it
                    .next()
                    .ok_or_else(|| IncorrectKind::new::<TNode>(*parent.raw()))?;
                let loc = merge_locations(result.loc, right.loc);
                (op_str, right, loc)
            }

            Expression::StringLit(op_str) => {
                let (right, _) = it
                    .next()
                    .ok_or_else(|| IncorrectKind::new::<TNode>(*parent.raw()))?;
                let loc = merge_locations(result.loc, right.loc);
                (op_str, right, loc)
            }

            _ => return Err(IncorrectKind::new::<TNode>(*parent.raw())),
        };

        result = Spanned::new(
            Expression::Binary {
                left: Box::new(result),
                op: Spanned::new(op_name, loc),
                right: Box::new(right_part),
            },
            final_loc,
        );
    }

    Ok(result)
}

fn fold_call<'a, TNode>(
    ctx: &Ctx,
    parent: &TNode,
    parts: &mut Vec<Spanned<Expression>>,
) -> NodeResult<'a, Spanned<Expression>>
where
    TNode: Node<'a>,
{
    if parts.is_empty() {
        return Err(IncorrectKind::new::<TNode>(*parent.raw()));
    }

    if parts.len() == 1 {
        Ok(parts.pop().expect("len is 1"))
    } else {
        let mut it = parts.drain(..);
        let head = it.next().unwrap();
        let args: Vec<_> = it.collect();

        let end_loc = args.last().map(|a| a.loc).unwrap_or(head.loc);
        let loc = merge_locations(head.loc, end_loc);

        Ok(Spanned::new(
            Expression::Call {
                function: Box::new(head),
                args,
            },
            loc,
        ))
    }
}

fn merge_locations(a: Location, b: Location) -> Location {
    debug_assert_eq!(
        a.file_id, b.file_id,
        "Cannot merge locations from different files"
    );

    let (start_byte, line_start, col_start) = if a.span.start < b.span.start {
        (a.span.start, a.span.line, a.span.col)
    } else {
        (b.span.start, b.span.line, b.span.col)
    };

    let (end_byte, line_end, col_end) = if a.span.end > b.span.end {
        (a.span.end, a.span.line_end, a.span.col_end)
    } else {
        (b.span.end, b.span.line_end, b.span.col_end)
    };

    Location {
        file_id: a.file_id,
        span: Span {
            start: start_byte,
            end: end_byte,
            line: line_start,
            col: col_start,
            line_end,
            col_end,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_lower_snapshot;

    #[test]
    fn test_type_decl_with_refinement() {
        assert_lower_snapshot!(
            "type ParsableInt = String where std.is_int it",
            as_type_declaration,
            lower_type_declaration
        );
    }

    #[test]
    fn test_type_decl_with_nested_refinement() {
        assert_lower_snapshot!(
            "type PositiveList = List (Int where it > 0)",
            as_type_declaration,
            lower_type_declaration
        );
    }

    #[test]
    fn test_simple_type_decl() {
        assert_lower_snapshot!(
            "type UserID = Int",
            as_type_declaration,
            lower_type_declaration
        );
    }

    #[test]
    fn test_fold_in_refinement() {
        assert_lower_snapshot!(
            "type UserID = str where \"root\" / it",
            as_type_declaration,
            lower_type_declaration
        );
    }
}
