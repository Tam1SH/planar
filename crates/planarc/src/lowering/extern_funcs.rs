use crate::{
    ast::{ExternArgument, ExternDefinition, ExternFunction, Visibility},
    lowering::{
        common::{lower_attribute, pub_vis_to_vis},
        ctx::Ctx,
        type_decl::lower_type_annotation,
    },
    pdl,
    spanned::Spanned,
};
use type_sitter::{HasChildren, Node, NodeResult};

pub fn lower_extern_definition<'a>(
    ctx: &Ctx<'a>,
    node: pdl::ExternDefinition<'a>,
) -> NodeResult<'a, Spanned<ExternDefinition>> {
    let mut attributes = Vec::new();
    let mut cursor = node.walk();

    let vis = if let Some(r#pub) = node.r#pub() {
        let r#pub = r#pub?;
        pub_vis_to_vis(r#pub)?
    } else {
        Visibility::Private
    };

    for attr_res in node.attributess(&mut cursor) {
        let attr_node = attr_res?;
        attributes.push(lower_attribute(ctx, attr_node)?);
    }

    let block_node = node.block()?;
    let mut functions = Vec::new();

    let mut block_cursor = block_node.walk();
    for fn_node_res in block_node.extern_def_fns(&mut block_cursor) {
        let fn_node = fn_node_res?;
        functions.push(lower_extern_def_fn(ctx, fn_node)?);
    }

    Ok(ctx.spanned(
        &node,
        ExternDefinition {
            attributes,
            functions,
            vis,
        },
    ))
}

pub fn lower_extern_def_fn<'a>(
    ctx: &Ctx<'a>,
    node: pdl::ExternDefFn<'a>,
) -> NodeResult<'a, Spanned<ExternFunction>> {
    use pdl::anon_unions::ExternDefArg_ExternReturn_Identifier_OperatorIdentifier as Child;

    let mut name: Option<Spanned<String>> = None;
    let mut args = Vec::new();
    let mut return_type = None;

    let mut cursor = node.walk();

    for child_res in node.children(&mut cursor) {
        match child_res? {
            Child::Identifier(n) => {
                name = Some(ctx.spanned(&n, ctx.text(&n)));
            }

            Child::OperatorIdentifier(n) => {
                name = Some(ctx.spanned(&n, ctx.text(&n)));
            }

            Child::ExternDefArg(n) => {
                args.push(lower_extern_arg(ctx, n)?);
            }

            Child::ExternReturn(n) => {
                // let ty_node = n.type_annotation()?.name()?;
                // return_type = Some(ctx.spanned(&ty_node, ctx.text(&ty_node)));
            }
        }
    }

    let name = name.expect("Extern function must have a name or operator identifier");

    Ok(ctx.spanned(
        &node,
        ExternFunction {
            name,
            args,
            return_type,
        },
    ))
}

fn lower_extern_arg<'a>(
    ctx: &Ctx<'a>,
    node: pdl::ExternDefArg<'a>,
) -> NodeResult<'a, Spanned<ExternArgument>> {
    let name_node = node.arg()?;
    let ty_node = node.r#type()?;

    Ok(ctx.spanned(
        &node,
        ExternArgument {
            name: ctx.spanned(&name_node, ctx.text(&name_node)),
            ty: ctx.spanned(&ty_node, lower_type_annotation(ctx, ty_node)?),
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        assert_lower_snapshot, ast::ExternDefinition, lowering::ctx::Ctx, module_loader::Source,
        spanned::FileId,
    };
    use tree_sitter::Parser;

    #[test]
    fn test_simple_extern_module() {
        assert_lower_snapshot!(
            "extern std.lint { isPascalCase name: String -> Diagnostic? }",
            as_extern_definition,
            lower_extern_definition
        );
    }

    #[test]
    fn test_extern_with_multiple_fns() {
        assert_lower_snapshot!(
            "extern utils.core { validate input: String, mode: Int -> Result \n log msg: String }",
            as_extern_definition,
            lower_extern_definition
        );
    }

    #[test]
    fn test_extern_operator_overload() {
        assert_lower_snapshot!(
            "extern std.math { operator / left: builtin.str, right: builtin.str -> builtin.str }",
            as_extern_definition,
            lower_extern_definition
        );
    }
}
