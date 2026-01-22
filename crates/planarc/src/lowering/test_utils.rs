#[cfg(test)]
pub mod test_utils {
    use crate::lowering::ctx::Ctx;
    use crate::module_loader::Source;
    use crate::pdl;
    use crate::spanned::FileId;
    use tree_sitter::Parser;
    use type_sitter::HasChildren;

    pub fn get_parser() -> Parser {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_planardl::LANGUAGE.into())
            .unwrap();
        parser
    }

    #[macro_export]
    macro_rules! assert_lower_snapshot {
        ($code:expr, $as_method:ident, $lower_fn:ident) => {{
            use type_sitter::HasChildren;
            let mut parser = $crate::lowering::test_utils::test_utils::get_parser();
            let tree = parser.parse($code, None).expect("Failed to parse source");
            let typed_tree = type_sitter::Tree::<$crate::pdl::SourceFile>::wrap(tree);
            let root = typed_tree.root_node().expect("Failed to wrap root node");

            let source = $crate::module_loader::Source {
                content: $code.to_string(),
                origin: "test_input".to_string(),
            };
            let ctx = $crate::lowering::ctx::Ctx::new(&source, $crate::spanned::FileId(0));
            let mut cursor = typed_tree.walk();

            let pdl_node = root
                .children(&mut cursor)
                .find_map(|child_res| {
                    let child = child_res.expect("Error during tree iteration");
                    child.$as_method()
                })
                .expect("Target definition not found in test code!");

            let ast = $lower_fn(&ctx, pdl_node).expect("Lowering failed");
            insta::assert_debug_snapshot!(ast);
        }};
    }
}
