use anyhow::{Result, anyhow};
use std::path::PathBuf;

use crate::{module_loader::Source, pdl};

#[derive(Debug, Clone)]
pub struct CompilationUnit {
    pub source: Source,
    pub tree: type_sitter::Tree<pdl::SourceFile<'static>>,
}

impl CompilationUnit {
    pub fn new(source_data: Source) -> Result<Self> {
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_planardl::LANGUAGE.into())?;

        let tree = parser
            .parse(&source_data.content, None)
            .ok_or_else(|| anyhow!("Tree-sitter parse failed (internal error)"))?;

        let typed_tree = type_sitter::Tree::<pdl::SourceFile>::wrap(tree);

        Ok(Self {
            source: source_data,
            tree: typed_tree,
        })
    }
}
