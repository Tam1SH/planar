pub mod error;

use anyhow::{Context, Result};
use miette::Diagnostic;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::PathBuf;
use thiserror::Error;
use tracing::{debug, info, instrument, trace, warn};

use crate::compiler::error::CompilersError;
use crate::linker::dependency_graph::GraphBuilder;
use crate::linker::error::{LinkerError, LinkerErrors};
use crate::linker::linked_ast::LinkedModule;
use crate::linker::linker;
use crate::linker::symbol_table::SymbolTable;
use crate::lowering::error::LoweringErrors;
use crate::module_loader::{ModuleLoader, PackageRoot};
use crate::source_registry::SourceRegistry;
use crate::typechecker::TypeChecker;
use crate::typechecker::typed_ast::TypedWorld;
use crate::validator::error::ValidationErrors;
use crate::validator::grammar_registry::GrammarRegistry;
use crate::validator::query_validator::QueryValidator;
use crate::validator::wit_validator::WitValidator;
use crate::{DynamicLanguageLoader, typechecker};

pub struct CompilationResult {
    pub typed_world: TypedWorld,
    pub registry: SourceRegistry,
    pub errors: CompilersError,
    pub grammars: GrammarRegistry,
}

impl CompilationResult {
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

pub struct Compiler<L: ModuleLoader> {
    loader: L,
    prelude: Vec<String>,
}

impl<L: ModuleLoader + Sync> Compiler<L> {
    pub fn new(loader: L) -> Self {
        Self {
            loader,
            prelude: vec!["std".to_string()],
        }
    }

    pub fn with_prelude(mut self, prelude: Vec<String>) -> Self {
        self.prelude = prelude;
        self
    }

    #[instrument(
        skip(self, roots, paths),
        fields(
            root_count = roots.len(),
            grammar_count = paths.len()
        )
    )]
    pub fn compile(
        &self,
        roots: Vec<PackageRoot>,
        paths: BTreeMap<String, PathBuf>,
    ) -> miette::Result<CompilationResult> {
        // --- Phase 1: Lowering ---
        debug!("Phase 1: Lowering AST...");
        let builder = GraphBuilder::new(&self.loader);
        let (lowered_graph, lowering_errors) = builder.build(&roots)?;

        let registry = lowered_graph.registry.clone();

        // --- Phase 2: Linking ---
        debug!("Phase 2: Linking pipeline...");
        let (linked_world, linking_errors) =
            linker::link_to_world(self.prelude.clone(), lowered_graph);

        // --- Phase 3: Grammar Loading ---
        debug!("Phase 3: Loading Grammars...");
        let grammar_registry = GrammarRegistry::new_with_paths(
            Box::new(crate::loader::DynamicLanguageLoader::default()),
            paths,
        );

        // --- Phase 4: Validation (Queries) ---
        debug!("Phase 4: Validating Tree-Sitter queries...");
        let query_validator = QueryValidator {
            registry: &registry,
            grammars: &grammar_registry,
        };

        let mut validation_errors = Vec::new();
        for (name, module) in &linked_world.modules {
            let errs = query_validator.validate_module(module);
            validation_errors.extend(errs.0);
        }

        // --- Phase 5: Type Checking ---
        debug!("Phase 5: Type Checking (Finalizing TypedWorld)...");

        let (typed_world, type_errors) = typechecker::check_world(linked_world).into_parts();

        // --- Phase 6: Error Collection ---
        let mut all_errors = CompilersError::default();
        all_errors.absorb(lowering_errors);
        all_errors.absorb(linking_errors);
        all_errors.absorb(ValidationErrors::new(validation_errors));
        all_errors.absorb(type_errors);

        if all_errors.is_empty() {
            info!("Compilation successful");
        } else {
            warn!(
                error_count = all_errors.len(),
                "Compilation failed with errors"
            );
        }

        Ok(CompilationResult {
            typed_world,
            registry,
            errors: all_errors,
            grammars: grammar_registry,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::linker::meta::{ResolvedId, SymbolId};
    use crate::loader::MockLanguageLoader;
    use crate::module_loader::FsModuleLoader;
    use std::collections::HashMap;
    use std::fs;
    use tempfile::TempDir;

    fn compile(files: Vec<(&str, &str)>) -> CompilationResult {
        let temp = TempDir::new().expect("failed to create temp dir");
        let base_path = temp.path();

        let mut package_roots = HashMap::new();

        for (rel_path, content) in files {
            let full_path = base_path.join(rel_path);

            if let Some(parent) = full_path.parent() {
                fs::create_dir_all(parent).unwrap();
            }

            fs::write(&full_path, content).unwrap();

            let first_dir = std::path::Path::new(rel_path)
                .components()
                .next()
                .unwrap()
                .as_os_str()
                .to_string_lossy()
                .to_string();

            package_roots
                .entry(first_dir.clone())
                .or_insert_with(|| base_path.join(&first_dir));
        }

        let roots: Vec<_> = package_roots
            .into_iter()
            .map(|(name, path)| PackageRoot { name, path })
            .collect();

        let loader = FsModuleLoader;

        let compiler = Compiler::new(loader).with_prelude(vec![]);

        compiler
            .compile(roots, BTreeMap::new())
            .expect("Compilation infrastructure failed")
    }

    #[test]
    fn test_diamond_fs() {
        let res = compile(vec![
            (
                "A/main.pdl",
                "import B.lib\nimport C.lib\nfact Root { f: D.data.Item }",
            ),
            ("B/lib.pdl", "import D.data"),
            ("C/lib.pdl", "import D.data"),
            ("D/data.pdl", "pub type Item = builtin.str"),
        ]);

        assert!(!res.has_errors());
        assert!(res.typed_world.modules.contains_key("A.main"));
    }

    #[test]
    fn test_simple_import() {
        let res = compile(vec![
            ("pkg/util.pdl", "pub type ID = builtin.i64"),
            (
                "app/main.pdl",
                "import pkg.util\nfact User { id: pkg.util.ID }",
            ),
        ]);

        assert!(!res.has_errors());

        let main = &res.typed_world.modules["app.main"];
        let fact = &main.facts[0].value;
        let id = fact.fields[0].value.ty.symbol.value;
        let util = &res.typed_world.modules["pkg.util"];
        assert_eq!(id, util.types[0].value.id);
    }
}
