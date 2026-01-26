use crate::lowering::ctx::Ctx;
use crate::pdl;
use crate::spanned::Spanned;
use type_sitter::{Node, NodeResult};

pub fn lower_grammar_declaration<'a>(
    ctx: &Ctx,
    node: pdl::GrammarDeclaration<'a>,
) -> NodeResult<'a, Spanned<String>> {
    let name_node = node.name()?;

    let text = ctx.text(&name_node);

    Ok(ctx.spanned(&node, text))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_lower_snapshot;

    #[test]
    fn test_grammar_declaration() {
        assert_lower_snapshot!(
            "using grammars.rust",
            as_grammar_declaration,
            lower_grammar_declaration
        );
    }
}
