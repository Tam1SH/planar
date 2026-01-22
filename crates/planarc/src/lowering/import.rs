use type_sitter::NodeResult;

use crate::{ast::Import, lowering::ctx::Ctx, pdl, spanned::Spanned};

pub fn lower_import<'a>(
    ctx: &Ctx<'a>,
    node: pdl::ImportDefinition<'a>,
) -> NodeResult<'a, Spanned<Import>> {
    let fqmn_node = node.fqmn()?;
    let path = ctx.text(&fqmn_node);

    Ok(ctx.spanned(
        &node,
        Import {
            fqmn: ctx.spanned(&fqmn_node, path),
        },
    ))
}
