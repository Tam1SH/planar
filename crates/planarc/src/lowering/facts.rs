use crate::lowering::common::{lower_attribute, pub_vis_to_vis};
use crate::lowering::ctx::Ctx;
use crate::lowering::type_decl::{lower_expression_atom, lower_type_annotation};
use crate::pdl;
use crate::{
    ast::*,
    spanned::{Span, Spanned},
};

use type_sitter::{HasChild, HasChildren, IncorrectKind, Node, NodeResult};

pub fn lower_fact_definition<'a>(
    ctx: &Ctx,
    node: pdl::FactDefinition<'a>,
) -> NodeResult<'a, Spanned<FactDefinition>> {
    use pdl::anon_unions::Attribute_FactFieldDefinition_Pub as U;

    let name = &node.name()?;
    let name = ctx.spanned(name, ctx.text(name));
    let mut attributes = Vec::new();
    let mut fields = Vec::new();
    let mut vis = Visibility::Private;

    let mut cursor = node.walk();
    for child_res in node.others(&mut cursor) {
        match child_res? {
            U::Attribute(n) => attributes.push(lower_attribute(ctx, n)?),
            U::FactFieldDefinition(n) => fields.push(lower_fact_field(ctx, n)?),
            U::Pub(pub_vis) => {
                vis = pub_vis_to_vis(pub_vis)?;
            }
        }
    }

    Ok(ctx.spanned(
        &node,
        FactDefinition {
            attributes,
            name,
            fields,
            vis,
        },
    ))
}

fn lower_fact_field<'a>(
    ctx: &Ctx,
    node: pdl::FactFieldDefinition<'a>,
) -> NodeResult<'a, Spanned<FactField>> {
    let mut attributes = Vec::new();
    let mut cursor = node.walk();

    for child_res in node.attributes(&mut cursor) {
        attributes.push(lower_attribute(ctx, child_res?)?);
    }

    let name_node = node.name()?;
    let name = ctx.spanned(&name_node, ctx.text(&name_node));

    let type_node = node.r#type()?;
    let ty = lower_type_annotation(ctx, type_node)?;

    Ok(ctx.spanned(
        &node,
        FactField {
            attributes,
            name,
            ty,
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_lower_snapshot;

    #[test]
    fn test_simple_fact() {
        assert_lower_snapshot!(
            r#"
            fact User { 
                #auto_id name: String 
                age: Int 
            }"#,
            as_fact_definition,
            lower_fact_definition
        );
    }

    #[test]
    fn test_fact_with_refinements() {
        assert_lower_snapshot!(
            r#"
            fact Product { 
                price: Float where it > 0.0 
                tags: List<String> 
                count: Int where it in [1..100] 
            }"#,
            as_fact_definition,
            lower_fact_definition
        );
    }

    #[test]
    fn test_fact_complex_types_and_generics() {
        assert_lower_snapshot!(
            r#"
            fact Container {
                count: Int where it in [1..100]
            }
        "#,
            as_fact_definition,
            lower_fact_definition
        );
    }
}
