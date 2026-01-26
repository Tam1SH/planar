use anyhow::{anyhow, Context, Result};
use derive_more::Display;
use miette::NamedSource;
use petgraph::algo::{is_cyclic_directed, toposort};
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::EdgeRef;
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, HashSet};
use std::fs;
use std::path::PathBuf;
use tracing::{debug, error, info, instrument, trace, trace_span, warn};
use type_sitter::{HasChildren, Node};

use crate::ast::Module;
use crate::checked::Checked;
use crate::linker::error::{CycleStep, GraphError};
use crate::linker::meta::SymbolId;
use crate::linker::symbol_table::SymbolTable;
use crate::lowering::error::{LoweringError, LoweringErrors};
use crate::lowering::module::lower_module;
use crate::manifest::ModuleResolver;
use crate::module_loader::{DiscoveredModule, ModuleLoader, PackageRoot};
use crate::pdl;
use crate::source_registry::{MietteSource, SourceRegistry};
use crate::spanned::{FileId, Location, Span, Spanned};
use crate::unit::CompilationUnit;
use crate::utils::TypeSitterResultExt;

#[derive(Debug, Clone, Display)]
pub enum Binding {
    Symbol(SymbolId),
    Namespace(String),
}

pub struct ModuleScope {
    pub bindings: BTreeMap<String, Binding>,
}

#[derive(Debug, Clone)]
pub struct LoweredGraph {
    pub modules: BTreeMap<String, Module>,
    pub dep_graph: DiGraph<String, Location>,
    pub registry: SourceRegistry,
}

pub struct GraphBuilder<'a, L: ModuleLoader> {
    loader: &'a L,
}

impl<'a, L: ModuleLoader + Sync> GraphBuilder<'a, L> {
    pub fn new(loader: &'a L) -> Self {
        Self { loader }
    }

    #[instrument(skip(self, roots), fields(roots_count = roots.len()))]
    fn discover_universe(
        &self,
        roots: &[PackageRoot],
    ) -> Result<BTreeMap<String, DiscoveredModule>> {
        info!("Starting module discovery phase");

        let mut universe = BTreeMap::new();

        for root in roots {
            let _span =
                trace_span!("scan_package", package = %root.name, root_path = ?root.path).entered();

            let modules = self
                .loader
                .scan(root)
                .with_context(|| format!("Failed to scan package root '{}'", root.name))?;

            debug!(count = modules.len(), "Scanned modules in package");

            for module in modules {
                let fqmn = module.fqmn.clone();
                let new_path = module.path.clone();

                match universe.entry(fqmn.clone()) {
                    Entry::Vacant(entry) => {
                        debug!(%fqmn, path = ?new_path, "Module discovered and registered");
                        entry.insert(module);
                    }
                    Entry::Occupied(entry) => {
                        let previous_module = entry.get();

                        error!(
                            %fqmn,
                            first_definition = ?previous_module.path,
                            second_definition = ?new_path,
                            "Duplicate module definition detected"
                        );

                        return Err(anyhow!(
                            "Duplicate module FQMN detected: '{}'.\n  -> First defined in: {:?}\n  -> Redefined in:    {:?}",
                            fqmn,
                            previous_module.path,
                            new_path
                        ));
                    }
                }
            }
        }

        info!(total_modules = universe.len(), "Discovery phase complete");
        Ok(universe)
    }

    #[instrument(skip(self, roots), fields(roots = ?roots))]
    pub fn build(&self, roots: &[PackageRoot]) -> miette::Result<(LoweredGraph, LoweringErrors)> {
        let universe = self
            .discover_universe(roots)
            .map_err(|e| miette::miette!(e))?;

        let universe_vec: Vec<_> = universe.into_iter().collect();

        let results: Vec<Result<_>> = universe_vec
            .into_par_iter()
            .enumerate()
            .map(|(idx, (fqmn, discovered))| {
                let file_id = FileId(idx);
                let _span = trace_span!("lower_module", module = %fqmn, ?file_id).entered();

                let unit = self
                    .loader
                    .load(&discovered.path)
                    .map_err(|e| anyhow!(e))
                    .and_then(CompilationUnit::new)?;

                let (module_ast, errors) =
                    lower_module(unit.tree, unit.source.clone(), file_id).into_parts();

                Ok((fqmn, file_id, unit.source, module_ast, errors))
            })
            .collect();

        let mut graph = DiGraph::new();
        let mut indices = BTreeMap::new();
        let mut modules = BTreeMap::new();
        let mut registry = SourceRegistry::default();
        let mut all_errors = LoweringErrors::new(vec![]);
        let mut source_lookup = BTreeMap::new();
        let mut pending_edges = Vec::new();

        for res in results {
            let (fqmn, file_id, source, ast, errors) = res.map_err(|e| miette::miette!(e))?;

            all_errors.merge(errors);
            source_lookup.insert(fqmn.clone(), source.clone());

            registry.add_with_id(source.clone(), file_id);

            let idx = graph.add_node(fqmn.clone());
            indices.insert(fqmn.clone(), idx);

            pending_edges.push((idx, fqmn.clone(), ast.imports.clone()));
            modules.insert(fqmn, ast);
        }

        for (src_idx, src_fqmn, imports) in pending_edges {
            for import in imports {
                let import_name = &import.value.fqmn.value;
                let location = import.value.fqmn.loc;

                if let Some(&target_idx) = indices.get(import_name) {
                    graph.add_edge(src_idx, target_idx, location);
                } else {
                    let src = source_lookup[&src_fqmn].clone();
                    return Err(GraphError::UnknownImport {
                        src,
                        span: location.into(),
                        import: import_name.clone(),
                        module: src_fqmn,
                        loc: location,
                    }
                    .into());
                }
            }
        }

        if let Err(cycle_err) = toposort(&graph, None) {
            let start_node = cycle_err.node_id();
            let cycle_steps = self.trace_cycle(&graph, start_node, &source_lookup)?;
            return Err(GraphError::CircularDependency {
                root_module: graph[start_node].clone(),
                cycle_path: cycle_steps,
            }
            .into());
        }

        Ok((
            LoweredGraph {
                modules,
                dep_graph: graph,
                registry,
            },
            all_errors,
        ))
    }

    fn trace_cycle(
        &self,
        graph: &DiGraph<String, Location>,
        start: NodeIndex,
        lookup: &BTreeMap<String, MietteSource>,
    ) -> miette::Result<Vec<CycleStep>> {
        let mut path = Vec::new();
        let mut visited = HashSet::new();
        let mut stack = HashSet::new();

        if self.dfs_cycle(graph, start, start, &mut visited, &mut stack, &mut path) {
            let mut steps = Vec::new();
            for (node_idx, loc) in path {
                let module_name = &graph[node_idx];
                let target_name = graph
                    .edges(node_idx)
                    .find(|e| e.weight() == &loc)
                    .map(|e| graph[e.target()].clone())
                    .unwrap_or_default();

                let src = lookup[module_name].clone();
                steps.push(CycleStep {
                    src,
                    span: loc.into(),
                    module: module_name.clone(),
                    target: target_name,
                    loc,
                });
            }
            Ok(steps)
        } else {
            Err(miette::miette!("Failed to trace cycle"))
        }
    }

    fn dfs_cycle(
        &self,
        graph: &DiGraph<String, Location>,
        current: NodeIndex,
        target: NodeIndex,
        visited: &mut HashSet<NodeIndex>,
        stack: &mut HashSet<NodeIndex>,
        path: &mut Vec<(NodeIndex, Location)>,
    ) -> bool {
        visited.insert(current);
        stack.insert(current);

        for edge in graph.edges(current) {
            let next_node = edge.target();
            let loc = *edge.weight();

            if next_node == target {
                path.push((current, loc));
                return true;
            }

            if (!visited.contains(&next_node) || stack.contains(&next_node))
                && !visited.contains(&next_node)
            {
                path.push((current, loc));
                if self.dfs_cycle(graph, next_node, target, visited, stack, path) {
                    return true;
                }
                path.pop();
            }
        }

        stack.remove(&current);
        false
    }

    fn extract_imports(
        &self,
        unit: &CompilationUnit,
        file_id: FileId,
    ) -> Result<Vec<Spanned<String>>> {
        let root = unit.tree.root_node().unwrap();
        let source_bytes = unit.source.inner().as_bytes();

        let mut imports = Vec::new();
        let mut cursor = root.walk();

        for child in root.children(&mut cursor) {
            use crate::pdl::anon_unions::Anon331116562354213157727272504972515459572 as NodeEnum;

            if let NodeEnum::ImportDefinition(imp) = child.static_err()? {
                let fqmn_node = imp.fqmn().static_err()?;

                let import_str = fqmn_node.raw().utf8_text(source_bytes)?.to_string();

                let range = fqmn_node.range();

                let span = Span::new(
                    range.start_byte,
                    range.end_byte,
                    range.start_point.row + 1,
                    range.start_point.column + 1,
                    range.end_point.row + 1,
                    range.end_point.column + 1,
                );

                let loc = Location::new(file_id, span);
                imports.push(Spanned::new(import_str, loc));
            }
        }

        Ok(imports)
    }

    fn format_cycle_error(
        &self,
        graph: &DiGraph<String, Location>,
        start_node: NodeIndex,
    ) -> String {
        let mut path = Vec::new();
        let mut visited = HashSet::new();
        let mut recursion_stack = HashSet::new();

        if self.find_cycle_dfs(
            graph,
            start_node,
            &mut visited,
            &mut recursion_stack,
            &mut path,
        ) {
            let steps: Vec<String> = path
                .iter()
                .map(|(node, loc)| {
                    let module_name = &graph[*node];

                    if let Some(l) = loc {
                        format!("{} (imported at {})", module_name, l)
                    } else {
                        module_name.to_string()
                    }
                })
                .collect();

            let last_node = path.last().unwrap().0;

            let first_node = path[0].0;

            return format!(
                "Circular dependency detected:\n  -> {}",
                steps.join("\n  -> ")
            );
        }

        format!(
            "Circular dependency detected involving module '{}' (trace failed)",
            graph[start_node]
        )
    }

    fn find_cycle_dfs(
        &self,
        graph: &DiGraph<String, Location>,
        curr: NodeIndex,
        visited: &mut HashSet<NodeIndex>,
        stack: &mut HashSet<NodeIndex>,
        path: &mut Vec<(NodeIndex, Option<Location>)>,
    ) -> bool {
        visited.insert(curr);
        stack.insert(curr);

        for edge in graph.edges(curr) {
            let target = edge.target();
            let location = edge.weight().clone();

            if stack.contains(&target) {
                path.push((curr, None));
                path.push((target, Some(location)));
                return true;
            }

            if !visited.contains(&target) {
                path.push((curr, Some(location)));
                if self.find_cycle_dfs(graph, target, visited, stack, path) {
                    return true;
                }
                path.pop();
            }
        }

        stack.remove(&curr);
        false
    }
}

#[cfg(test)]
mod tests {
    use crate::module_loader::FsModuleLoader;

    use super::*;
    use miette::miette;
    use std::{fs, sync::Once};
    use tempfile::tempdir;
    use tracing_subscriber::{fmt, EnvFilter};

    static INIT: Once = Once::new();

    fn init_test_logging() {
        INIT.call_once(|| {
            fmt()
                .with_env_filter(EnvFilter::new("planar=trace,linker=trace"))
                .with_test_writer()
                .init();
        });
    }

    #[test]
    fn test_complex_fs_structure() -> miette::Result<()> {
        init_test_logging();

        let root_dir = tempdir().map_err(|e| miette!(e))?;

        // /tmp/std/
        //   core.pdl
        //   io.pdl
        // /tmp/app/
        //   main.pdl

        let std_path = root_dir.path().join("std");
        let app_path = root_dir.path().join("app");
        fs::create_dir_all(&app_path).unwrap();
        fs::create_dir_all(&std_path).unwrap();

        fs::write(std_path.join("core.pdl"), "type Int = builtin.i64").unwrap();
        fs::write(
            std_path.join("io.pdl"),
            "import std.core\nfact File { size: std.core.Int }",
        )
        .unwrap();

        fs::write(
            app_path.join("main.pdl"),
            r#"
            import std.io
            import std.core
            fact Main { f: std.io.File }
        "#,
        )
        .unwrap();

        let roots = vec![
            PackageRoot {
                name: "std".into(),
                path: std_path,
            },
            PackageRoot {
                name: "app".into(),
                path: app_path,
            },
        ];

        let binding = FsModuleLoader;
        let builder = GraphBuilder::new(&binding);
        let (graph, _) = builder.build(&roots)?;

        assert_eq!(graph.modules.len(), 3);

        assert!(graph.modules.contains_key("std.core"));
        assert!(graph.modules.contains_key("std.io"));
        assert!(graph.modules.contains_key("app.main"));

        let main = &graph.modules["app.main"];
        let neighbors: Vec<_> = main
            .imports
            .iter()
            .map(|i| i.value.fqmn.value.as_str())
            .collect();

        assert!(neighbors.contains(&"std.io"));
        assert!(neighbors.contains(&"std.core"));

        Ok(())
    }

    #[test]
    fn test_missing_dependency() -> Result<()> {
        init_test_logging();
        let tmp = tempdir()?;
        let p_path = tmp.path().join("pkg");
        fs::create_dir(&p_path)?;

        fs::write(p_path.join("a.pdl"), "import pkg.b")?;

        let roots = vec![PackageRoot {
            name: "pkg".into(),
            path: p_path.clone(),
        }];

        let binding = FsModuleLoader;
        let builder = GraphBuilder::new(&binding);
        let res = builder.build(&roots);

        assert!(res.is_err());
        assert!(
            res.err()
                .unwrap()
                .to_string()
                .contains("Module 'pkg.b' not found")
        );

        Ok(())
    }

    #[test]
    fn test_cycle_fs() -> Result<()> {
        init_test_logging();
        let tmp = tempdir()?;
        let root = tmp.path().join("cycle");
        fs::create_dir(&root)?;

        fs::write(root.join("a.pdl"), "import cycle.b")?;
        fs::write(root.join("b.pdl"), "import cycle.a")?;

        let roots = vec![PackageRoot {
            name: "cycle".into(),
            path: root.clone(),
        }];

        let res = GraphBuilder::new(&FsModuleLoader).build(&roots);

        assert!(
            res.err()
                .unwrap()
                .to_string()
                .contains("Circular dependency detected involving 'cycle.b'")
        );

        Ok(())
    }
}
