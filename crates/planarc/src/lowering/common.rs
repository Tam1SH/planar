use tracing::trace;
use type_sitter::{HasChild, HasChildren, IncorrectKind, Node, NodeResult};

use crate::{
    ast::{Attribute, Expression, Visibility},
    lowering::{
        ctx::Ctx,
        type_decl::{lower_expression_atom, lower_expression_list},
    },
    pdl::{self, Pub},
    spanned::{FileId, Location, Span, Spanned},
};

pub fn lower_attribute<'a>(
    ctx: &Ctx,
    node: pdl::Attribute<'a>,
) -> NodeResult<'a, Spanned<Attribute>> {
    let name = &node.name()?;
    let name = ctx.spanned(name, ctx.text(name));

    Ok(ctx.spanned(&node, Attribute { name }))
}

pub fn lower_refinement_node<'a>(
    ctx: &Ctx,
    node: pdl::Refinement<'a>,
) -> NodeResult<'a, Spanned<Expression>> {
    let mut cursor = node.walk();
    let mut parts = Vec::new();

    parts.push(lower_expression_list(
        ctx,
        &node,
        node.children(&mut cursor),
    )?);

    if parts.is_empty() {
        return Ok(ctx.spanned(&node, Expression::Identifier("MISSING".to_string())));
    }

    if parts.len() == 1 {
        Ok(parts.pop().expect("Verified length 1"))
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

pub fn pub_vis_to_vis(pub_vis: Pub) -> NodeResult<Visibility> {
    if let Some(pkg) = pub_vis.pkg() {
        let _ = pkg?;
        Ok(Visibility::Package)
    } else {
        Ok(Visibility::Pub)
    }
}

pub fn extract_query_data<'a>(
    ctx: &Ctx,
    content_node: &pdl::QueryLiteral<'a>,
) -> NodeResult<'a, (Spanned<String>, Vec<Spanned<String>>)> {
    let content_text = if let Some(content_res) = content_node.content() {
        ctx.text(&content_res?)
    } else {
        String::new()
    };

    let content_node = content_node
        .content()
        .transpose()?
        .ok_or_else(|| IncorrectKind::new::<pdl::QueryLiteral>(*content_node.raw()))?;

    let base_loc = ctx.location(&content_node);

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_tsquery::LANGUAGE.into())
        .map_err(|_| IncorrectKind::new::<pdl::QueryLiteral>(*content_node.raw()))?;

    let tree = parser
        .parse(&content_text, None)
        .ok_or_else(|| IncorrectKind::new::<pdl::QueryLiteral>(*content_node.raw()))?;

    let mut captures = Vec::new();
    let mut cursor = tree.walk();

    find_captures(
        &mut cursor,
        &content_text,
        base_loc,
        ctx.file_id,
        &mut captures,
    );

    let content_text = ctx.spanned(&content_node, content_text);

    Ok((content_text, captures))
}

fn map_sub_span(base_loc: Location, sub_range: tree_sitter::Range) -> Span {
    let start_byte = base_loc.span.start + sub_range.start_byte;
    let end_byte = base_loc.span.start + sub_range.end_byte;

    let line = base_loc.span.line + sub_range.start_point.row;
    let line_end = base_loc.span.line + sub_range.end_point.row;

    let col = if sub_range.start_point.row == 0 {
        base_loc.span.col + sub_range.start_point.column
    } else {
        sub_range.start_point.column + 1
    };

    let col_end = if sub_range.end_point.row == 0 {
        base_loc.span.col + sub_range.end_point.column
    } else {
        sub_range.end_point.column + 1
    };

    Span {
        start: start_byte,
        end: end_byte,
        line,
        col,
        line_end,
        col_end,
    }
}

fn find_captures(
    cursor: &mut tree_sitter::TreeCursor,
    source: &str,
    base_loc: Location,
    file_id: FileId,
    out: &mut Vec<Spanned<String>>,
) {
    let node = cursor.node();

    if node.kind() == "capture" {
        let raw_name = node.utf8_text(source.as_bytes()).unwrap_or("");
        let name = raw_name.to_string();

        out.push(Spanned {
            value: name,
            loc: Location {
                file_id,
                span: map_sub_span(base_loc, node.range()),
            },
        });
    }

    if cursor.goto_first_child() {
        loop {
            find_captures(cursor, source, base_loc, file_id, out);
            if !cursor.goto_next_sibling() {
                break;
            }
        }
        cursor.goto_parent();
    }
}

pub fn lower_in_expression<'a>(
    ctx: &Ctx,
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
