use std::collections::BTreeMap;

use miette::{NamedSource, SourceSpan};
use rayon::iter::{IntoParallelIterator, IntoParallelRefIterator, ParallelIterator};
use tap::Pipe;
use tracing::{debug, debug_span, error, info, instrument, trace, warn};

use super::meta::PendingSymbol;
use crate::ast::{self, Expression};
use crate::checked::{Checked, CheckedIteratorExt};
use crate::error::ErrorCollection;
use crate::linker::ast_linker::AstLinker;
use crate::linker::dependency_graph::LoweredGraph;
use crate::linker::error::{AmbiguousCandidate, LinkerError, LinkerErrors, PreviousDefinition};
use crate::linker::linked_ast::*;
use crate::linker::linked_world::LinkedWorld;
use crate::linker::lookup::SymbolLookup;
use crate::linker::meta::{
    FieldMetadata, FunctionParam, ResolvedId, SymbolId, SymbolKind, Visibility,
};
use crate::linker::symbol_table::{SymbolTable, map_visibility};
use crate::source_registry::SourceRegistry;
use crate::spanned::{Location, Spanned};

struct CollectCtx<'a> {
    module_name: &'a str,
    package_name: &'a str,
    registry: &'a SourceRegistry,
}

#[instrument(skip(prelude, graph), fields(modules_count = graph.modules.len()))]
pub fn link_to_world(prelude: Vec<String>, graph: LoweredGraph) -> (LinkedWorld, LinkerErrors) {
    info!("Starting linking pipeline: Collect -> Register -> Link");

    (SymbolTable::with_builtins(), LinkerErrors::default())
        // === PHASE 1: COLLECT & REGISTER ===
        .pipe(|(mut table, mut errors)| {
            
            info!("Phase 1: Collecting and registering symbols");

            let (pending_batches, collect_errs): (Vec<_>, Vec<_>) = graph
                .modules
                .par_iter()
                .map(|(name, module)| {
                    
                    let _span = debug_span!("collect_module", module = %name).entered();

                    let pkg = name.split('.').next().unwrap_or(name);
                    let ctx = CollectCtx {
                        module_name: name,
                        package_name: pkg,
                        registry: &graph.registry,
                    };
                    
                    let result = collect_module_symbols(&ctx, module).into_parts();
                    debug!(symbols_count = result.0.len(), "Symbols collected");
                    result
                })
                .unzip();

            collect_errs.into_iter().for_each(|e| errors.merge(e));

            for ps in pending_batches.into_iter().flatten() {
                trace!(symbol = %ps.name, fqmn = %format!("{}.{}", ps.module_name, ps.name), "Registering symbol");
                if let Err(e) = table.register(ps, &graph.registry) {
                    errors.push(e);
                }
            }
            (table, errors)
        })
        // === PHASE 2: RESOLVE SIGNATURES ===
        .pipe(|(mut table, mut errors)| {
            info!("Phase 2: Resolving module signatures (facts, edges, externs)");
            let (updates_batches, sig_errs): (Vec<_>, Vec<_>) = graph
                .modules
                .par_iter()
                .map(|(name, module)| {
                    let _span = debug_span!("resolve_sig", module = %name).entered();
                    let pkg = name.split('.').next().unwrap_or(name).to_string();
                    let lookup = SymbolLookup {
                        table: &table,
                        registry: &graph.registry,
                        current_package: pkg,
                        current_module: name.clone(),
                        imports: module
                            .imports
                            .iter()
                            .map(|i| i.value.fqmn.value.clone())
                            .collect(),
                        prelude: &prelude,
                    };

                    let result = AstLinker::new(lookup)
                        .resolve_module_signatures(module)
                        .into_parts();
                    
                    debug!(updates = result.0.kinds.len(), "Signatures resolved");
                    result
                })
                .unzip();

            sig_errs.into_iter().for_each(|e| errors.merge(e));

            for batch in updates_batches.into_iter() {
                for (id, kind) in batch.kinds {
                    table.update_kind(id, kind);
                }
                for (id, vis) in batch.visibilities {
                    table.update_visibility(id, vis);
                }
            }
            (table, errors)
        })
        // === PHASE 3: LINK BODIES ===
        .pipe(|(table, mut errors)| {
            info!("Phase 3: Linking bodies (fact refinements, query sources, node statements)");
            let (linked_modules, link_errs): (BTreeMap<_, _>, Vec<_>) = graph
                .modules
                .par_iter()
                .map(|(name, module)| {

                    let _span = debug_span!("link_body", module = %name).entered();

                    let pkg = name.split('.').next().unwrap_or(name).to_string();
                    let lookup = SymbolLookup {
                        table: &table,
                        registry: &graph.registry,
                        current_package: pkg,
                        current_module: name.clone(),
                        imports: module
                            .imports
                            .iter()
                            .map(|i| i.value.fqmn.value.clone())
                            .collect(),
                        prelude: &prelude,
                    };

                    let linker = AstLinker::new(lookup);
                    let mut mod_errors = LinkerErrors::default();

                    debug!("Linking facts, types, and externs");
                    let linked = LinkedModule {
                        file_id: module.file_id,
                        grammar: module.grammar.clone(),
                        facts: linker
                            .link_vec(&module.facts, AstLinker::resolve_fact)
                            .sink(&mut mod_errors),
                        types: linker
                            .link_vec(&module.types, AstLinker::resolve_type_decl)
                            .sink(&mut mod_errors),
                        externs: linker
                            .link_vec(&module.externs, AstLinker::resolve_extern_definition)
                            .sink(&mut mod_errors),
                        queries: linker
                            .link_vec(&module.queries, AstLinker::resolve_query)
                            .sink(&mut mod_errors),
                        nodes: linker
                            .link_vec(&module.nodes, AstLinker::resolve_node)
                            .sink(&mut mod_errors),
                        edges: linker
                            .link_vec(&module.edges, AstLinker::resolve_edge)
                            .sink(&mut mod_errors),
                    };

                    debug!(
                        errors_count = mod_errors.0.len(),
                        "Module linking complete"
                    );
                    ((name.clone(), linked), mod_errors)
                })
                .unzip();

            link_errs.into_iter().for_each(|e| errors.merge(e));

            info!("Linking pipeline complete");
            (
                LinkedWorld {
                    table,
                    modules: linked_modules,
                    registry: graph.registry,
                },
                errors,
            )
        })
}

fn create_lookup<'a>(
    prelude: &'a Vec<String>,
    table: &'a SymbolTable,
    registry: &'a SourceRegistry,
    module_name: &str,
    module: &ast::Module,
) -> SymbolLookup<'a> {
    SymbolLookup {
        table,
        registry,
        current_package: module_name
            .split('.')
            .next()
            .unwrap_or(module_name)
            .to_string(),
        current_module: module_name.to_string(),
        imports: module
            .imports
            .iter()
            .map(|i| i.value.fqmn.value.clone())
            .collect(),
        prelude,
    }
}

fn collect_module_symbols(
    ctx: &CollectCtx,
    module: &ast::Module,
) -> Checked<Vec<PendingSymbol>, LinkerErrors> {
    let mut symbols = Vec::new();

    let pkg_name = ctx.package_name.to_string();
    let mod_name = ctx.module_name.to_string();

    for fact in &module.facts {
        symbols.push(PendingSymbol {
            name: fact.value.name.value.clone(),
            kind: SymbolKind::Fact { fields: vec![] },
            loc: fact.value.name.loc,
            visibility: map_visibility(&fact.value.vis, None),
            module_name: mod_name.clone(),
            package_name: pkg_name.clone(),
        });
    }

    for ty in &module.types {
        symbols.push(PendingSymbol {
            name: ty.value.name.value.clone(),
            kind: SymbolKind::Type {
                base_type: None,
                fields: vec![],
                is_primitive: false,
            },
            loc: ty.value.name.loc,
            visibility: map_visibility(&ty.value.vis, None),
            module_name: mod_name.clone(),
            package_name: pkg_name.clone(),
        });
    }

    for edge in &module.edges {
        symbols.push(PendingSymbol {
            name: edge.value.name.value.clone(),
            kind: SymbolKind::Edge {
                from: SymbolId::INVALID_ID, 
                to: SymbolId::INVALID_ID,
            },
            loc: edge.value.name.loc,
            visibility: map_visibility(&edge.value.vis, None),
            module_name: mod_name.clone(),
            package_name: pkg_name.clone(),
        });
    }

    for node in &module.nodes {
        let node_name = node.value.kind.value.clone();

        symbols.push(PendingSymbol {
            name: node_name.clone(),
            kind: SymbolKind::Node,
            loc: node.value.kind.loc,
            visibility: map_visibility(&node.value.vis, None),
            module_name: mod_name.clone(),
            package_name: pkg_name.clone(),
        });

        for stmt in &node.value.statements {
            if let ast::NodeStatement::Query(q) = &stmt.value {
                symbols.push(PendingSymbol {
                    name: format!("{}.{}", node_name, q.value.name.value),
                    kind: SymbolKind::Query {
                        captures: q.value.captures.clone(),
                        source: q.value.value.clone(),
                    },
                    loc: q.value.name.loc,
                    visibility: Visibility::Scoped(SymbolId::INVALID_ID),
                    module_name: mod_name.clone(),
                    package_name: pkg_name.clone(),
                });
            }
        }
    }

    for query in &module.queries {
        symbols.push(PendingSymbol {
            name: query.value.name.value.clone(),
            kind: SymbolKind::Query {
                captures: query.value.captures.clone(),
                source: query.value.value.clone(),
            },
            loc: query.value.name.loc,
            visibility: map_visibility(&query.value.vis, None),
            module_name: mod_name.clone(),
            package_name: pkg_name.clone(),
        });
    }

    for ext in &module.externs {
        let is_builtin = ext
            .value
            .attributes
            .iter()
            .any(|a| a.value.name.value == "builtin");

        let (effective_mod, vis) = if is_builtin {
            ("builtin".to_string(), Visibility::Public)
        } else {
            (mod_name.clone(), map_visibility(&ext.value.vis, None))
        };

        for func in &ext.value.functions {
            symbols.push(PendingSymbol {
                name: func.value.name.value.clone(),
                kind: SymbolKind::ExternFunction {
                    params: vec![],
                    return_type: None,
                },
                loc: func.value.name.loc,
                visibility: vis,
                module_name: effective_mod.clone(),
                package_name: pkg_name.clone(),
            });
        }
    }

    Checked::new(symbols)
}

#[cfg(test)]
pub mod tests {
    use std::collections::BTreeMap;
    use std::path::PathBuf;

    use super::*;
    use crate::linker::dependency_graph::{GraphBuilder, LoweredGraph};
    use crate::linker::error::LinkerError;
    use crate::linker::linked_ast::*;
    use crate::linker::meta::{ResolvedId, SymbolId, SymbolKind};
    use crate::module_loader::{InMemoryLoader, PackageRoot};

    pub fn setup_lowered_graph(files: &[(&str, &str)]) -> LoweredGraph {
        let mut mock_files = BTreeMap::new();
        let mut package_names = std::collections::HashSet::new();

        for (name, content) in files {
            mock_files.insert(name.to_string(), content.to_string());
            let root_pkg = name.split('.').next().unwrap_or(name);
            package_names.insert(root_pkg.to_string());
        }

        let loader = InMemoryLoader { files: mock_files };
        let builder = GraphBuilder::new(&loader);

        let roots: Vec<_> = package_names
            .into_iter()
            .map(|name| PackageRoot {
                name: name.clone(),
                path: PathBuf::from(format!("/virtual/{}", name)),
            })
            .collect();

        let (lowered, errors) = builder
            .build(&roots)
            .expect("Dependency graph build failed");

        assert!(errors.is_empty(), "Lowering failed: {:?}", errors);
        lowered
    }

    #[test]
    fn test_global_access_without_import() {
        let files = [
            ("lib", "pub type Shared = builtin.str"),
            ("main", "fact App { data: lib.Shared }"),
        ];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec![], lg);

        assert!(errors.is_empty(), "Linker errors: {:?}", errors);
        let fact = &world.modules["main"].facts[0].value;

        match &fact.fields[0].value.ty.symbol.value {
            ResolvedId::Global(id) => {
                let (expected, _) = world.table.resolve("lib.Shared").unwrap();
                assert_eq!(id.value, expected);
            }
            _ => panic!("Expected Global resolution"),
        }
    }

    #[test]
    fn test_import_as_alias() {
        let files = [
            ("std.math", "pub type PI = builtin.f64"),
            (
                "main",
                r#"
                import std.math
                fact Circle { value: math.PI }
            "#,
            ),
        ];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec![], lg);

        assert!(errors.is_empty(), "Errors: {:?}", errors);

        let fact = &world.modules["main"].facts[0].value;
        if let ResolvedId::Global(id) = &fact.fields[0].value.ty.symbol.value {
            let (expected, _) = world.table.resolve("std.math.PI").unwrap();
            assert_eq!(id.value, expected);
        } else {
            panic!("Should resolve via import alias");
        }
    }

    #[test]
    fn test_prelude_resolution() {
        let files = [
            ("std", "pub type Int = builtin.i64"),
            ("main", "fact Simple { f: Int }"),
        ];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec!["std".to_string()], lg);

        assert!(errors.is_empty(), "Errors: {:?}", errors);
        match &world.modules["main"].facts[0].value.fields[0]
            .value
            .ty
            .symbol
            .value
        {
            ResolvedId::Global(id) => {
                let (expected, _) = world.table.resolve("std.Int").unwrap();
                assert_eq!(id.value, expected);
            }
            _ => panic!("Prelude resolution failed"),
        }
    }

    #[test]
    fn test_shadowing_priority() {
        let files = [
            ("std", "pub type Val = builtin.i64"),
            (
                "main",
                r#"
                extern {
                    operator > left: builtin.str, right: builtin.str -> builtin.str
                }
                type Val = builtin.str
                
                type Test = builtin.str where it > 0 
            "#,
            ),
        ];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec!["std".to_string()], lg);

        assert!(errors.is_empty(), "Errors: {:?}", errors);

        let type_decl = &world.modules["main"].types[1].value.definition.value;
        let refinement = type_decl
            .base_type
            .as_ref()
            .unwrap()
            .refinement
            .as_ref()
            .unwrap();

        match &refinement.value {
            LinkedExpression::Binary { left, .. } => match &left.value {
                LinkedExpression::Identifier(ResolvedId::Local(name)) => {
                    assert_eq!(name.value, "it");
                }
                _ => panic!("Refinement variable should be Local, got {:?}", left.value),
            },
            _ => panic!("Expected binary expr"),
        }
    }

    #[test]
    fn test_ambiguity_with_aliases() {
        let files = [
            ("pkg.A", "pub type Item = builtin.str"),
            ("pkg.B", "pub type Item = builtin.i64"),
            (
                "app",
                r#"
                import pkg.A
                import pkg.B
                fact Conflict { f: Item }
            "#,
            ),
        ];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);

        assert!(
            errors
                .0
                .iter()
                .any(|e| matches!(e.as_ref(), LinkerError::AmbiguousReference { .. })),
            "Expected AmbiguousReference error, got: {:?}",
            errors
        );
    }

    #[test]
    fn test_nested_package_fqmn() {
        let files = [
            ("deep.nest.module", "pub type Value = builtin.bool"),
            ("main", "fact Test { f: deep.nest.module.Value }"),
        ];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);

        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }

    #[test]
    fn test_relative_import_suffix_matching() {
        let files = [
            ("some.deep.inner", "pub type Target = builtin.i64"),
            (
                "main",
                r#"
                import some.deep.inner
                type Alias = inner.Target 
            "#,
            ),
        ];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec![], lg);

        assert!(errors.is_empty(), "Errors: {:?}", errors);

        let type_alias = &world.modules["main"].types[0]
            .value
            .definition
            .value
            .base_type
            .as_ref()
            .unwrap();
        match &type_alias.symbol.value {
            ResolvedId::Global(id) => {
                let (expected, _) = world.table.resolve("some.deep.inner.Target").unwrap();
                assert_eq!(id.value, expected);
            }
            _ => panic!("Failed to resolve via suffix"),
        }
    }

    #[test]
    fn test_sibling_module_access_without_import() {
        let files = [
            ("auth.models", "pub type User = builtin.str"),
            ("auth.logic", "fact Check { u: models.User }"),
        ];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec![], lg);

        assert!(errors.is_empty(), "Errors: {:?}", errors);
        match world.modules["auth.logic"].facts[0].value.fields[0]
            .value
            .ty
            .symbol
            .value
        {
            ResolvedId::Global(_) => {} // OK
            _ => panic!("Sibling resolution failed"),
        }
    }

    #[test]
    fn test_extern_function_registration() {
        let files = [(
            "main",
            r#"
                extern {
                    isPascalCase name: str -> bool
                }
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec![], lg);

        let (id, _) = world
            .table
            .resolve("main.isPascalCase")
            .expect("Extern function should be registered in symbol table");

        assert!(errors.is_empty(), "Errors: {:?}", errors);

        let ext = &world.modules["main"].externs[0].value;

        assert_eq!(ext.functions[0].value.name, "isPascalCase");
        assert_eq!(ext.functions[0].value.id, id);
    }

    #[test]
    fn test_extern_type_resolution() {
        let files = [
            ("lib", "pub type MyType = builtin.i64"),
            (
                "main",
                r#"
                extern {
                    process val: lib.MyType -> builtin.bool
                }
            "#,
            ),
        ];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec![], lg);
        assert!(errors.is_empty(), "Errors: {:?}", errors);

        let ext_fn = &world.modules["main"].externs[0].value.functions[0].value;

        if let ResolvedId::Global(symbol_id) = &ext_fn.args[0].value.ty.symbol.value {
            let (expected_id, _) = world.table.resolve("lib.MyType").unwrap();
            assert_eq!(symbol_id.value, expected_id);
        } else {
            panic!("Extern argument type should be resolved to Global");
        }
    }

    #[test]
    fn test_call_to_extern_function() {
        let files = [
            (
                "std.utils",
                r#"
                pub extern {
                    check val: builtin.i64 -> builtin.bool
                }
            "#,
            ),
            (
                "main",
                r#"
                import std.utils
                fact Target {
                    age: builtin.i64 where check it
                }
            "#,
            ),
        ];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec![], lg);
        assert!(errors.is_empty(), "Errors: {:?}", errors);

        let fact = &world.modules["main"].facts[0].value;
        let refinement = fact.fields[0].value.ty.refinement.as_ref().unwrap();

        match &refinement.value {
            LinkedExpression::Call { function, .. } => {
                if let LinkedExpression::Identifier(resolved_id) = &function.value {
                    if let ResolvedId::Global(symbol_id) = resolved_id {
                        let (expected_id, _) = world.table.resolve("std.utils.check").unwrap();
                        assert_eq!(symbol_id.value, expected_id);
                    } else {
                        panic!("Call should resolve to Global symbol")
                    }
                } else {
                    panic!("Call should resolve to Global symbol");
                }
            }
            _ => panic!("Expected Call expression"),
        }
    }

    #[test]
    fn test_complex_nested_call_refinement() {
        let files = [
            (
                "std.math",
                r#"
                pub extern {
                    add a: builtin.i64 b: builtin.i64 -> builtin.i64
                    is_positive val: builtin.i64 -> builtin.bool
                }
            "#,
            ),
            (
                "main",
                r#"
                import std.math
                fact Physics {
                    velocity: builtin.i64 where std.math.is_positive (std.math.add it 10)
                }
            "#,
            ),
        ];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec![], lg);

        assert!(errors.is_empty(), "Linker errors: {:?}", errors);

        let fact = &world.modules["main"].facts[0].value;
        let refinement = fact.fields[0]
            .value
            .ty
            .refinement
            .as_ref()
            .expect("Refinement should exist");

        if let LinkedExpression::Call { function, args } = &refinement.value {
            // Check outer: is_positive
            let (expected_outer, _) = world.table.resolve("std.math.is_positive").unwrap();
            match &function.value {
                LinkedExpression::Identifier(ResolvedId::Global(id)) => {
                    assert_eq!(id.value, expected_outer)
                }
                _ => panic!("Expected global is_positive"),
            }

            if let LinkedExpression::Call {
                function: inner_func,
                args: inner_args,
            } = &args[0].value
            {
                let (expected_inner, _) = world.table.resolve("std.math.add").unwrap();
                match &inner_func.value {
                    LinkedExpression::Identifier(ResolvedId::Global(id)) => {
                        assert_eq!(id.value, expected_inner)
                    }
                    _ => panic!("Expected global add"),
                }
                assert_eq!(inner_args.len(), 2);
            } else {
                panic!("Expected nested call");
            }
        } else {
            panic!("Expected outer Call");
        }
    }

    #[test]
    fn test_extern_operator_resolution() {
        let files = [(
            "main",
            r#"
                extern {
                    operator / left: str, right: str -> str
                }
                
                type Path = str where "root" / it
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec![], lg);

        assert!(errors.is_empty(), "Linker errors: {:?}", errors);

        let (expected_id, _) = world.table.resolve("main./").unwrap();
        let type_decl = &world.modules["main"].types[0].value.definition.value;
        let refinement = type_decl
            .base_type
            .as_ref()
            .unwrap()
            .refinement
            .as_ref()
            .unwrap();

        match &refinement.value {
            LinkedExpression::Binary { operator, .. } => {
                if let ResolvedId::Global(id) = &operator.value {
                    assert_eq!(id.value, expected_id);
                } else {
                    panic!("Operator should be resolved to Global");
                }
            }
            _ => panic!("Expected LinkedExpression::Binary"),
        }
    }

    #[test]
    fn test_extern_collision_error() {
        let files = [(
            "main",
            r#"
                extern { fetch id: i64 -> str }
                extern { fetch name: str -> str }
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);

        assert!(!errors.is_empty(), "Should detect symbol collision");
        assert!(errors.0.iter().any(|e| matches!(e.as_ref(), LinkerError::SymbolCollision { name, .. } if name == "main.fetch")));
    }

    #[test]
    fn test_node_local_query_priority() {

        let files = [(
            "main",
            r#"
                query target = `(global_query)`
                
                node MyNode {
                    query target = `(local_query)`
                    match target { }
                }
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec![], lg);
        

        assert!(errors.is_empty(), "Errors: {:?}", errors);

        let node = &world.modules["main"].nodes[0].value;
        let match_stmt = match &node.statements[1] {
            LinkedNodeStatement::Match(m) => m,
            _ => panic!("Expected match statement"),
        };

        if let LinkedMatchQueryReference::Named(id) = &match_stmt.value.query_ref.value {
            let (expected_id, _) = world
                .table
                .resolve("main.MyNode.target")
                .expect("Local query not found");
            assert_eq!(id, &expected_id);
        } else {
            panic!("Expected global reference to local query");
        }
    }

    #[test]
    fn test_error_match_on_it_variable() {
        let files = [(
            "main",
            r#"
                node MyNode {
                    match it { }
                }
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);

        assert!(errors.0.iter().any(
            |e| matches!(e.as_ref(), LinkerError::UnknownSymbol { name, .. } if name == "it")
        ));
    }

    #[test]
    fn test_match_raw_query() {
        let files = [(
            "main",
            r#"
                node MyNode {
                    match `(node_pattern) @cap` { }
                }
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec![], lg);

        assert!(errors.is_empty());
        let node = &world.modules["main"].nodes[0].value;
        match &node.statements[0] {
            LinkedNodeStatement::Match(m) => match &m.value.query_ref.value {
                LinkedMatchQueryReference::Raw { source: s, .. } => assert_eq!(s.value, "(node_pattern) @cap"),
                _ => panic!("Expected Raw reference"),
            },
            _ => panic!("Expected match"),
        }
    }

    #[test]
    fn test_match_local_let_resolution() {
        let files = [(
            "main",
            r#"
                node MyNode {
                    match `(pattern) @cap` {
                        let x = 10
                        let y = x
                    }
                }
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec![], lg);

        assert!(errors.is_empty());
        let node = &world.modules["main"].nodes[0].value;
        let match_stmt = match &node.statements[0] {
            LinkedNodeStatement::Match(m) => m,
            _ => panic!("Expected match"),
        };
        let let_y = match &match_stmt.value.body[1].value {
            LinkedMatchItem::Let(l) => l,
            _ => panic!("Expected let"),
        };

        match &let_y.value.value {
            LinkedExpression::Identifier(ResolvedId::Local(id)) => assert_eq!(id.value, "x"),
            _ => panic!("Expected local resolution"),
        }
    }

    #[test]
    fn test_match_capture_resolution() {
        let files = [(
            "main",
            r#"
                node MyNode {
                    match `(pattern) @my_cap` {
                        let val = @my_cap
                    }
                }
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec![], lg);

        assert!(errors.is_empty());
        let node = &world.modules["main"].nodes[0].value;
        let match_stmt = match &node.statements[0] {
            LinkedNodeStatement::Match(m) => m,
            _ => panic!("Expected match"),
        };
        let let_val = match &match_stmt.value.body[0].value {
            LinkedMatchItem::Let(l) => l,
            _ => panic!("Expected let"),
        };
        match &let_val.value.value {
            LinkedExpression::Identifier(ResolvedId::Local(id)) => assert_eq!(id.value, "@my_cap"),
            _ => panic!("Expected @capture to resolve as Local"),
        }
    }

    #[test]
    fn test_match_capture_resolution_error() {
        let files = [(
            "main",
            r#"
                node MyNode {
                    match `(pattern) @my_cap` {
                        let val = @my_cap_wrong
                    }
                }
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);
        assert!(errors.0.iter().any(|e| matches!(e.as_ref(), LinkerError::UndefinedCapture { capture_name, .. } if capture_name == "@my_cap_wrong")));
    }

    #[test]
    fn test_match_shadowing() {
        let files = [
            ("lib", "pub type GlobalVar = builtin.i64"),
            (
                "main",
                r#"
                import lib
                node MyNode {
                    match `(node)` {
                        let GlobalVar = 42
                        let check = GlobalVar
                    }
                }
            "#,
            ),
        ];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec![], lg);

        assert!(errors.is_empty());
        let match_stmt = match &world.modules["main"].nodes[0].value.statements[0] {
            LinkedNodeStatement::Match(m) => m,
            _ => panic!("Expected match"),
        };
        let let_check = match &match_stmt.value.body[1].value {
            LinkedMatchItem::Let(l) => l,
            _ => panic!("Expected let"),
        };
        match &let_check.value.value {
            LinkedExpression::Identifier(ResolvedId::Local(id)) => {
                assert_eq!(id.value, "GlobalVar")
            }
            _ => panic!("Shadowing failed"),
        }
    }

    #[test]
    fn test_match_nested_capture_let() {
        let files = [(
            "main",
            r#"
                node MyNode {
                    match `(node)` {
                        @cap { let inner = 1 }
                    }
                }
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);
        assert!(errors.0.iter().any(|e| matches!(e.as_ref(), LinkerError::UndefinedCapture { capture_name, .. } if capture_name == "@cap")));
    }

    #[test]
    fn test_match_lexical_binding_scope() {
        let files = [(
            "main",
            r#"
                node MyNode {
                    match `(f (identifier) @n) @func` {
                        @func { let internal_id = @n }
                        let alias = internal_id
                    }
                }
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);
        assert!(errors.0.iter().any(|e| matches!(e.as_ref(), LinkerError::UnknownSymbol { name, .. } if name == "internal_id")));
    }

    #[test]
    fn test_match_it_disallowed() {
        let files = [(
            "main",
            r#"
                node MyNode {
                    match `(node)` { let x = it }
                }
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);
        assert!(errors.0.iter().any(
            |e| matches!(e.as_ref(), LinkerError::UnknownSymbol { name, .. } if name == "it")
        ));
    }

    #[test]
    fn test_match_order_dependency() {
        let files = [(
            "main",
            r#"
                node MyNode {
                    match `(node)` {
                        let y = x
                        let x = 1
                    }
                }
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);
        assert!(
            errors.0.iter().any(
                |e| matches!(e.as_ref(), LinkerError::UnknownSymbol { name, .. } if name == "x")
            )
        );
    }

    #[test]
    fn test_edge_invalid_endpoint_kind() {
        let files = [(
            "main",
            r#"
                query MyQuery = `(node)`
                fact MyFact { id: builtin.i64 }
                edge WrongEdge = MyQuery -> MyFact
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);

        assert!(errors.0.iter().any(|e| matches!(e.as_ref(), LinkerError::InvalidSymbolKind { found, .. } if found == "Query")));
    }

    #[test]
    fn test_emit_resolution_success() {
        let files = [(
            "main",
            r#"
                type String = builtin.str
                fact User { id: String }
                fact File { path: String }
                edge Owns = User -> File
                node Protect {
                    match `(u) @u` {
                        let p = "/root"
                        emit User { id: @u } -[Owns]-> File { path: p }
                    }
                }
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }

    #[test]
    fn test_emit_unknown_relation() {
        let files = [(
            "main",
            r#"
                fact A { id: i64 }
                node Test {
                    match `q` { emit A { id: 1 } <-[UnknownRel]-> A { id: 2 } }
                }
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);
        assert!(errors.0.iter().any(|e| matches!(e.as_ref(), LinkerError::UnknownSymbol { name, .. } if name == "UnknownRel")));
    }

    #[test]
    fn test_emit_order_dependency() {
        let files = [(
            "main",
            r#"
                fact User { id: str }
                edge Friend = User -> User
                node Test {
                    match `q` {
                        emit User { id: x } -[Friend]-> User { id: "admin" }
                        let x = "val"
                    }
                }
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);
        assert!(
            errors.0.iter().any(
                |e| matches!(e.as_ref(), LinkerError::UnknownSymbol { name, .. } if name == "x")
            )
        );
    }

    #[test]
    fn test_emit_with_capture_scoping() {
        let files = [(
            "main",
            r#"
                fact User { name: str }
                edge Rel = User -> User
                node Test {
                    match `q` {
                        @inner { let local_val = "ok" }
                        emit User { name: local_val } -[Rel]-> User { name: "sys" }
                    }
                }
            "#,
        )];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);
        assert!(errors.0.iter().any(
            |e| matches!(e.as_ref(), LinkerError::UnknownSymbol { name, .. } if name == "local_val")
        ));
    }

    #[test]
    fn test_emit_cross_module_resolution() {
        let files = [
            (
                "defs",
                "pub fact GlobalFact { id: i64 } \n pub edge GlobalRel = GlobalFact -> GlobalFact",
            ),
            (
                "main",
                "import defs \n node Test { match `q` { emit defs.GlobalFact { id: 1 } <-[defs.GlobalRel]-> defs.GlobalFact { id: 2 } } }",
            ),
        ];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }

    #[test]
    fn test_levenshtein_suggestion() {
        let files = [(
            "main",
            "fact MyFact {} \n node N { match `q` { emit MyFuct { }  } }",
        )];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);

        assert!(errors.0.iter().any(|e| {
            if let LinkerError::UnknownSymbol { name, help, .. } = e.as_ref() {
                name == "MyFuct" && help.as_ref().map(|h| h.contains("MyFact")).unwrap_or(false)
            } else {
                false
            }
        }));
    }


    #[test]
    fn test_full_pipeline_snapshot() {
        let files = [
            ("types", "pub type ID = builtin.i64"),
            (
                "models",
                r#"
                import types
                pub fact User { id: types.ID }
                pub fact Post { author_id: types.ID }
            "#,
            ),
            (
                "social",
                r#"
                import models
                pub edge Ownership = models.User -> models.Post
                
                node System {
                    match `(node)` {
                        emit models.User { id: 1 } -[Ownership]-> models.Post { author_id: 1 }
                        emit models.User { id: 1 }
                    }
                }
            "#,
            ),
        ];

        let lg = setup_lowered_graph(&files);
        let (world, errors) = link_to_world(vec!["std".to_string()], lg);

        assert!(errors.is_empty());
        insta::assert_debug_snapshot!("world_state", world);
    }

    #[test]
    fn test_linker_errors_snapshot() {
        let files = [
            (
                "main",
                r#"
                fact SameName {}
                fact SameName {}
                fact BadFact { field: UnknownType }
                node N { match `q` { emit other.PrivateFact {} } }
            "#,
            ),
            ("other", "fact PrivateFact {}"),
        ];

        let lg = setup_lowered_graph(&files);
        let (_, errors) = link_to_world(vec![], lg);

        insta::assert_debug_snapshot!("linker_errors", errors);
    }
}
