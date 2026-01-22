pub mod error;
use anyhow::{Context, Result};
use miette::Diagnostic;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::PathBuf;
use thiserror::Error;
use tracing::{debug, info, instrument, trace, warn};

use crate::DynamicLanguageLoader;
use crate::compiler::error::CompilersError;
use crate::linker::dependency_graph::GraphBuilder;
use crate::linker::error::{LinkerError, LinkerErrors};
use crate::linker::linked_ast::LinkedModule;
use crate::linker::linker::Linker;
use crate::linker::symbol_table::SymbolTable;
use crate::lowering::error::LoweringErrors;
use crate::module_loader::{ModuleLoader, PackageRoot};
use crate::source_registry::SourceRegistry;
use crate::validator::grammar_registry::GrammarRegistry;
use crate::validator::query_validator::QueryValidator;
use crate::validator::wit_validator::WitValidator;

pub struct CompilationResult {
    pub modules: BTreeMap<String, LinkedModule>,
    pub registry: SourceRegistry,
    pub symbol_table: SymbolTable,
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
        // 1. Discovery & Lowering
        debug!("Phase 1: Discovery. Scanning package roots and lowering AST...");
        let builder = GraphBuilder::new(&self.loader);
        let (lowered_graph, lowering_errors) = builder.build(&roots)?;
        trace!(errors = lowering_errors.0.len(), "Lowering finished");

        // 2. Linking
        debug!("Phase 2: Linking definitions and symbols...");
        let mut linker = Linker::new(self.prelude.clone());
        let definition_errors = linker.collect_definitions(&lowered_graph);
        debug!(
            symbols = linker.table.symbols.len(),
            "Symbol table populated"
        );

        let mut modules = BTreeMap::new();
        let mut linking_errors = LinkerErrors::new(vec![]);

        for (fqmn, module_ast) in &lowered_graph.modules {
            let _span = tracing::debug_span!("link_module", module = %fqmn).entered();
            let (linked_mod, mod_errors) =
                linker.link_module(fqmn, module_ast, &lowered_graph.registry);
            linking_errors.extend(mod_errors);
            modules.insert(fqmn.clone(), linked_mod);
        }

        // 3. Grammar Loading
        debug!("Phase 3: Initializing Grammar Registry...");
        for (name, path) in &paths {
            trace!(grammar = %name, path = ?path, "Registering grammar binary");
        }

        let grammar_registry =
            GrammarRegistry::new_with_paths(Box::new(DynamicLanguageLoader::default()), paths);

        // 4 Validation
        debug!("Phase 4: Validation (Wit & Queries)...");
        let wit_validator = WitValidator {
            table: &linker.table,
            registry: &lowered_graph.registry,
        };

        let query_validator = QueryValidator {
            registry: &lowered_graph.registry,
            grammars: &grammar_registry,
        };

        let mut validation_errors = Vec::new();

        for (fqmn, linked_mod) in &modules {
            let _span = tracing::debug_span!("validate_module", module = %fqmn).entered();

            let wit_errs = wit_validator.validate_module(linked_mod);
            validation_errors.extend(wit_errs.0);

            let query_errs = query_validator.validate_module(linked_mod);
            validation_errors.extend(query_errs.0);
        }

        // 5. Finalizing
        let mut all_errors = CompilersError::default();
        all_errors.extend(lowering_errors.0);
        all_errors.extend(definition_errors.0);
        all_errors.extend(linking_errors.0);
        all_errors.extend(validation_errors);

        if all_errors.is_empty() {
            info!(
                status = "success",
                modules = modules.len(),
                "Compilation finished successfully"
            );
        } else {
            warn!(
                status = "failed",
                error_count = all_errors.0.len(),
                "Compilation finished with errors"
            );
        }

        Ok(CompilationResult {
            modules,
            registry: lowered_graph.registry,
            errors: all_errors,
            symbol_table: linker.table,
            grammars: grammar_registry,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::linker::ids::ResolvedId;
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
        assert!(res.modules.contains_key("A.main"));
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

        let main = &res.modules["app.main"];
        let fact = &main.facts[0].value;
        if let ResolvedId::Global(id) = &fact.fields[0].value.ty.symbol.value {
            let util = &res.modules["pkg.util"];
            assert_eq!(id.value, util.types[0].value.id);
        } else {
            panic!("Symbol not resolved");
        }
    }
}
