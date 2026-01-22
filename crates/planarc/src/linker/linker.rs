use crate::ast::{self, Expression};
use crate::linker::dependency_graph::LoweredGraph;
use crate::linker::error::{AmbiguousCandidate, LinkerError, LinkerErrors, PreviousDefinition};
use crate::linker::ids::{ResolvedId, SymbolId, SymbolKind};
use crate::linker::linked_ast::*;
use crate::linker::lookup::SymbolLookup;
use crate::linker::resolver::AstLinker;
use crate::linker::symbol_table::{SymbolMetadata, SymbolTable, Visibility, map_visibility};
use crate::source_registry::SourceRegistry;
use crate::spanned::{Location, Spanned};
use miette::{NamedSource, SourceSpan};
use tracing::{debug, error, info, instrument, trace, warn};

pub struct Linker {
    pub table: SymbolTable,
    pub prelude: Vec<String>,
}

struct CollectCtx<'a> {
    module_name: &'a str,
    package_name: &'a str,
    registry: &'a SourceRegistry,
}

impl Linker {
    pub fn new(prelude: Vec<String>) -> Self {
        Self {
            table: SymbolTable::with_builtins(),
            prelude,
        }
    }

    #[instrument(skip(self, graph), fields(modules_count = graph.modules.len()))]
    pub fn collect_definitions(&mut self, graph: &LoweredGraph) -> LinkerErrors {
        info!("Starting symbol collection phase");
        let mut errors = Vec::new();

        for (module_name, module) in &graph.modules {
            let package_name = module_name.split('.').next().unwrap_or(module_name);
            let ctx = CollectCtx {
                module_name,
                package_name,
                registry: &graph.registry,
            };

            self.collect_module_symbols(&ctx, module, &mut errors);
        }

        LinkerErrors(errors)
    }

    fn collect_module_symbols(
        &mut self,
        ctx: &CollectCtx,
        module: &ast::Module,
        errors: &mut Vec<LinkerError>,
    ) {
        // 1. Facts
        for fact in &module.facts {
            let vis = map_visibility(&fact.value.vis, None);
            self.try_register(
                ctx,
                &fact.value.name.value,
                SymbolKind::Fact,
                fact.value.name.loc,
                vis,
                errors,
            );
        }

        // 2. Types
        for ty in &module.types {
            let vis = map_visibility(&ty.value.vis, None);
            self.try_register(
                ctx,
                &ty.value.name.value,
                SymbolKind::Type,
                ty.value.name.loc,
                vis,
                errors,
            );
        }

        // 3. Nodes & Nested Queries
        for node in &module.nodes {
            let vis = map_visibility(&node.value.vis, None);
            if let Some(node_id) = self.try_register(
                ctx,
                &node.value.kind.value,
                SymbolKind::Node,
                node.value.kind.loc,
                vis,
                errors,
            ) {
                self.collect_node_statements(ctx, node_id, &node.value, errors);
            }
        }

        // 4. Global Queries
        for query in &module.queries {
            let vis = map_visibility(&query.value.vis, None);
            self.try_register(
                ctx,
                &query.value.name.value,
                SymbolKind::Query,
                query.loc,
                vis,
                errors,
            );
        }

        // 5. Externs
        for ext in &module.externs {
            let is_builtin = ext
                .value
                .attributes
                .iter()
                .any(|a| a.value.name.value == "builtin");
            let vis = if is_builtin {
                Visibility::Public
            } else {
                map_visibility(&ext.value.vis, None)
            };

            for func in &ext.value.functions {
                let prefix = if is_builtin {
                    "builtin"
                } else {
                    ctx.module_name
                };
                let fqmn = format!("{}.{}", prefix, func.value.name.value);

                if let Err(prev_loc) = self.table.insert(
                    &fqmn,
                    SymbolKind::ExternFunction,
                    func.value.name.loc,
                    vis,
                    ctx.package_name.to_string(),
                    ctx.module_name.to_string(),
                ) {
                    errors.push(self.create_collision_error(
                        &fqmn,
                        func.value.name.loc,
                        prev_loc,
                        ctx.registry,
                    ));
                }
            }
        }
    }

    fn collect_node_statements(
        &mut self,
        ctx: &CollectCtx,
        node_id: SymbolId,
        node: &ast::NodeDefinition,
        errors: &mut Vec<LinkerError>,
    ) {
        for stmt in &node.statements {
            if let ast::NodeStatement::Query(q) = &stmt.value {
                let name = format!("{}.{}", node.kind.value, q.value.name.value);
                self.try_register(
                    ctx,
                    &name,
                    SymbolKind::Query,
                    q.loc,
                    Visibility::Scoped(node_id),
                    errors,
                );
            }
        }
    }

    fn try_register(
        &mut self,
        ctx: &CollectCtx,
        name: &str,
        kind: SymbolKind,
        loc: Location,
        vis: Visibility,
        errors: &mut Vec<LinkerError>,
    ) -> Option<SymbolId> {
        let fqmn = format!("{}.{}", ctx.module_name, name);

        match self.table.insert(
            &fqmn,
            kind,
            loc,
            vis,
            ctx.package_name.to_string(),
            ctx.module_name.to_string(),
        ) {
            Ok(id) => Some(id),
            Err(prev_loc) => {
                errors.push(self.create_collision_error(&fqmn, loc, prev_loc, ctx.registry));
                None
            }
        }
    }

    #[instrument(skip(self, module, registry), fields(module = name))]
    pub fn link_module(
        &self,
        name: &str,
        module: &ast::Module,
        registry: &SourceRegistry,
    ) -> (LinkedModule, LinkerErrors) {
        let pkg = name.split('.').next().unwrap_or(name).to_string();
        let imports = module
            .imports
            .iter()
            .map(|i| i.value.fqmn.value.clone())
            .collect();

        let lookup = SymbolLookup {
            table: &self.table,
            registry,
            current_package: pkg,
            current_module: name.to_string(),
            imports,
            prelude: self.prelude.clone(),
            current_node_id: None,
        };

        let mut linker = AstLinker::new(lookup);

        let linked = LinkedModule {
            file_id: module.file_id,
            facts: linker.link_vec(&module.facts, AstLinker::resolve_fact),
            types: linker.link_vec(&module.types, AstLinker::resolve_type_decl),
            externs: linker.link_vec(&module.externs, AstLinker::resolve_extern_definition),
            queries: linker.link_vec(&module.queries, AstLinker::resolve_query),
            nodes: linker.link_vec(&module.nodes, AstLinker::resolve_node),
        };

        (linked, LinkerErrors(linker.errors))
    }

    fn create_collision_error(
        &self,
        name: &str,
        loc: Location,
        prev_loc: Location,
        reg: &SourceRegistry,
    ) -> LinkerError {
        let (src, span) = reg.get_source_and_span(loc);
        let (p_src, p_span) = reg.get_source_and_span(prev_loc);
        LinkerError::SymbolCollision {
            name: name.to_string(),
            src,
            span,
            loc,
            related: vec![PreviousDefinition {
                src: p_src,
                span: p_span,
                loc: prev_loc,
            }],
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use std::collections::BTreeMap;
    use std::path::PathBuf;
    use std::sync::Once;
    use tracing_subscriber::{EnvFilter, fmt};
    use tree_sitter::Parser;

    use crate::linker::dependency_graph::{GraphBuilder, LoweredGraph};
    use crate::linker::error::LinkerError;
    use crate::linker::ids::{ResolvedId, SymbolId, SymbolKind};
    use crate::linker::linked_ast::LinkedExpression;
    use crate::module_loader::{InMemoryLoader, PackageRoot, Source};
    use crate::spanned::Location;

    fn get_parser() -> Parser {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_planardl::LANGUAGE.into())
            .unwrap();
        parser
    }

    pub fn setup_project(files: &[(&str, &str)], _entry: &str) -> (LoweredGraph, Linker) {
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

        assert!(errors.is_empty(), "lower failed: {:?}", errors);

        let mut linker = Linker::new(vec!["std".to_string()]);
        let collect_errors = linker.collect_definitions(&lowered);

        assert!(
            collect_errors.is_empty(),
            "Collect definitions failed: {:?}",
            collect_errors
        );

        (lowered, linker)
    }

    #[test]
    fn test_global_access_without_import() {
        let files = [
            ("lib", "pub type Shared = builtin.str"),
            ("main", "fact App { data: lib.Shared }"),
        ];

        let (lg, linker) = setup_project(&files, "main");
        let (linked_mod, errors) = linker.link_module("main", &lg.modules["main"], &lg.registry);
        println!("{:?}", errors);
        assert!(errors.is_empty());
        let fact = &linked_mod.facts[0].value;

        match &fact.fields[0].value.ty.symbol.value {
            ResolvedId::Global(id) => {
                let (expected, _) = linker.table.resolve("lib.Shared").unwrap();
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

        let (lg, linker) = setup_project(&files, "main");
        let (linked_mod, errors) = linker.link_module("main", &lg.modules["main"], &lg.registry);

        assert!(errors.is_empty(), "Errors: {:?}", errors);

        let fact = &linked_mod.facts[0].value;
        if let ResolvedId::Global(id) = &fact.fields[0].value.ty.symbol.value {
            let (expected, _) = linker.table.resolve("std.math.PI").unwrap();
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

        let (lg, linker) = setup_project(&files, "main");
        let (linked_mod, errors) = linker.link_module("main", &lg.modules["main"], &lg.registry);

        assert!(errors.is_empty());
        match &linked_mod.facts[0].value.fields[0].value.ty.symbol.value {
            ResolvedId::Global(id) => {
                let (expected, _) = linker.table.resolve("std.Int").unwrap();
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

        let (lg, linker) = setup_project(&files, "main");
        let (linked_mod, errors) = linker.link_module("main", &lg.modules["main"], &lg.registry);

        println!("{:?}", &errors);
        assert!(errors.is_empty());

        let type_decl = &linked_mod.types[1].value.definition.value;
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
                "main",
                r#"
                import pkg.A
                import pkg.B
            "#,
            ),
            ("mod1", "pub type Thing = builtin.i64"),
            ("mod2", "pub type Thing = builtin.str"),
            (
                "app",
                r#"
                import mod1
                import mod2
                fact Conflict { f: Thing }
            "#,
            ),
        ];

        let (lg, linker) = setup_project(&files, "app");
        let (_, errors) = linker.link_module("app", &lg.modules["app"], &lg.registry);

        assert!(
            errors
                .0
                .iter()
                .any(|e| matches!(e, LinkerError::AmbiguousReference { .. })),
            "Errors found: {:?}",
            errors
        );
    }

    #[test]
    fn test_collision_internal() {
        let files = [(
            "main",
            r#"
                type X = builtin.str
                fact X {} 
            "#,
        )];

        let mut registry = SourceRegistry::default();
        let (file_id, src) = registry.add(Source {
            origin: "m.pdl".into(),
            content: files[0].1.into(),
        });

        let mut parser = get_parser();
        let tree = parser.parse(files[0].1, None).unwrap();
        let (module, _) =
            crate::lowering::module::lower_module(type_sitter::Tree::wrap(tree), src, file_id);

        let mut modules = BTreeMap::new();
        modules.insert("main".to_string(), module);
        let lg = LoweredGraph {
            modules,
            dep_graph: petgraph::graph::DiGraph::new(),
            registry,
        };

        let mut linker = Linker::new(vec![]);
        let errors = linker.collect_definitions(&lg);

        assert_eq!(errors.0.len(), 1);
        match &errors.0[0] {
            LinkerError::SymbolCollision { name, .. } => assert_eq!(name, "main.X"),
            _ => panic!("Expected collision error"),
        }
    }

    #[test]
    fn test_nested_package_fqmn() {
        let files = [
            ("deep.nest.module", "pub type Value = builtin.bool"),
            ("main", "fact Test { f: deep.nest.module.Value }"),
        ];

        let (lg, linker) = setup_project(&files, "main");
        let (_, errors) = linker.link_module("main", &lg.modules["main"], &lg.registry);

        assert!(errors.is_empty());
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

        let (lg, linker) = setup_project(&files, "main");
        let (linked_mod, errors) = linker.link_module("main", &lg.modules["main"], &lg.registry);

        assert!(errors.is_empty(), "Errors: {:?}", errors);

        let type_alias = &linked_mod.types[0]
            .value
            .definition
            .value
            .base_type
            .as_ref()
            .unwrap();
        match &type_alias.symbol.value {
            ResolvedId::Global(id) => {
                let (expected, _) = linker.table.resolve("some.deep.inner.Target").unwrap();
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

        let (lg, linker) = setup_project(&files, "auth.logic");
        let (linked_mod, errors) =
            linker.link_module("auth.logic", &lg.modules["auth.logic"], &lg.registry);

        assert!(errors.is_empty());
        match linked_mod.facts[0].value.fields[0].value.ty.symbol.value {
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

        let (lg, linker) = setup_project(&files, "main");

        let (id, _) = linker
            .table
            .resolve("main.isPascalCase")
            .expect("Extern function should be registered in symbol table");

        let (linked_mod, errors) = linker.link_module("main", &lg.modules["main"], &lg.registry);
        assert!(errors.is_empty(), "Errors: {:?}", errors);

        let ext = &linked_mod.externs[0].value;

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

        let (lg, linker) = setup_project(&files, "main");
        let (linked_mod, errors) = linker.link_module("main", &lg.modules["main"], &lg.registry);
        assert!(errors.is_empty());

        let ext_fn = &linked_mod.externs[0].value.functions[0].value;

        if let ResolvedId::Global(symbol_id) = &ext_fn.args[0].value.ty.symbol.value {
            let (expected_id, _) = linker.table.resolve("lib.MyType").unwrap();
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

        let (lg, linker) = setup_project(&files, "main");
        let (linked_mod, errors) = linker.link_module("main", &lg.modules["main"], &lg.registry);
        assert!(errors.is_empty(), "Errors: {:?}", errors);

        let type_decl = &linked_mod.facts[0].value;
        let refinement = type_decl.fields[0].value.ty.refinement.as_ref().unwrap();

        match &refinement.value {
            LinkedExpression::Call { function, .. } => {
                if let LinkedExpression::Identifier(resolved_id) = &function.value {
                    if let ResolvedId::Global(symbol_id) = resolved_id {
                        let (expected_id, _) = linker.table.resolve("std.utils.check").unwrap();
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

        let (lg, linker) = setup_project(&files, "main");
        let (linked_mod, errors) = linker.link_module("main", &lg.modules["main"], &lg.registry);

        assert!(errors.is_empty(), "Linker errors: {:?}", errors);

        let fact = &linked_mod.facts[0].value;
        let field = &fact.fields[0].value;
        let refinement = field
            .ty
            .refinement
            .as_ref()
            .expect("Refinement should exist");

        if let LinkedExpression::Call { function, args } = &refinement.value {
            assert_is_global_symbol(&linker, function, "std.math.is_positive");
            assert_eq!(args.len(), 1, "is_positive should have 1 argument");

            if let LinkedExpression::Call {
                function: inner_func,
                args: inner_args,
            } = &args[0].value
            {
                assert_is_global_symbol(&linker, inner_func, "std.math.add");
                assert_eq!(inner_args.len(), 2, "add should have 2 arguments");

                match &inner_args[0].value {
                    LinkedExpression::Identifier(ResolvedId::Local(id)) => {
                        assert_eq!(id.value, "it");
                    }
                    _ => panic!(
                        "First arg of 'add' should be local 'it', got {:?}",
                        inner_args[0].value
                    ),
                }

                match &inner_args[1].value {
                    LinkedExpression::Number(val) => {
                        assert_eq!(val, "10");
                    }
                    _ => panic!("Second arg of 'add' should be number '10'"),
                }
            } else {
                panic!(
                    "Expected nested Call to std.math.add, got {:?}",
                    args[0].value
                );
            }
        } else {
            panic!("Expected outer Call, got {:?}", refinement.value);
        }
    }

    fn assert_is_global_symbol(
        linker: &Linker,
        expr: &Spanned<LinkedExpression>,
        expected_fqmn: &str,
    ) {
        if let LinkedExpression::Identifier(ResolvedId::Global(symbol_id)) = &expr.value {
            let (expected_id, _) = linker
                .table
                .resolve(expected_fqmn)
                .unwrap_or_else(|| panic!("Could not resolve {} in global table", expected_fqmn));
            assert_eq!(
                symbol_id.value, expected_id,
                "Symbol mismatch for {}",
                expected_fqmn
            );
        } else {
            panic!(
                "Expected Global identifier for {}, got {:?}",
                expected_fqmn, expr.value
            );
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

        let (lg, linker) = setup_project(&files, "main");
        let (linked_mod, errors) = linker.link_module("main", &lg.modules["main"], &lg.registry);

        assert!(errors.is_empty(), "Linker errors: {:?}", errors);

        let (expected_id, _) = linker
            .table
            .resolve("main./")
            .expect("Operator '/' should be registered in main module");

        let type_decl = &linked_mod.types[0].value.definition.value;
        let refinement = type_decl
            .base_type
            .as_ref()
            .unwrap()
            .refinement
            .as_ref()
            .unwrap();

        match &refinement.value {
            LinkedExpression::Binary {
                left,
                operator,
                right,
            } => {
                if let ResolvedId::Global(id) = &operator.value {
                    assert_eq!(id.value, expected_id, "Operator ID mismatch");
                } else {
                    panic!(
                        "Operator should be resolved to Global, got {:?}",
                        operator.value
                    );
                }

                assert!(matches!(left.value, LinkedExpression::StringLit(_)));
                assert!(matches!(
                    right.value,
                    LinkedExpression::Identifier(ResolvedId::Local(_))
                ));
            }
            _ => panic!(
                "Expected LinkedExpression::Binary, got {:?}",
                refinement.value
            ),
        }
    }

    #[test]
    fn test_extern_collision_error() {
        let files = [(
            "main",
            r#"
                extern {
                    fetch id: i64 -> str
                }
                extern {
                    fetch name: str -> str
                }
            "#,
        )];

        let mut mock_files = std::collections::BTreeMap::new();
        for (name, content) in files {
            mock_files.insert(name.to_string(), content.to_string());
        }

        let loader = InMemoryLoader { files: mock_files };
        let builder = GraphBuilder::new(&loader);
        let roots = vec![PackageRoot {
            name: "main".into(),
            path: "/virtual/main".into(),
        }];
        let (lowered, _) = builder.build(&roots).unwrap();

        let mut linker = Linker::new(vec![]);
        let errors = linker.collect_definitions(&lowered);

        assert!(
            !errors.is_empty(),
            "Should detect symbol collision in extern"
        );
        assert!(errors.0.iter().any(
            |e| matches!(e, LinkerError::SymbolCollision { name, .. } if name == "main.fetch")
        ));
    }

    #[test]
    fn test_node_local_query_priority() {
        let files = [(
            "main",
            r#"
                query target: grammars.n = `(global_query)`
                
                node MyNode {
                    query target: grammars.n = `(local_query)`
                    match target { }
                }
            "#,
        )];

        let (lg, linker) = setup_project(&files, "main");
        let (linked_mod, errors) = linker.link_module("main", &lg.modules["main"], &lg.registry);

        assert!(errors.is_empty(), "Errors: {:?}", errors);

        let node = &linked_mod.nodes[0].value;
        let match_stmt = match &node.statements[1].value {
            LinkedNodeStatement::Match(m) => m,
            _ => panic!("Expected match statement"),
        };

        if let LinkedMatchQueryReference::Global(id) = &match_stmt.query_ref.value {
            let (expected_id, _) = linker
                .table
                .resolve("main.MyNode.target")
                .expect("Should resolve to local query");
            assert_eq!(
                id, &expected_id,
                "Match should point to local query, not global"
            );

            let global_id = linker.table.resolve("main.target").unwrap().0;
            assert_ne!(id, &global_id, "Match erroneously pointed to global query");
        } else {
            panic!("Expected global reference");
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

        let (lg, linker) = setup_project(&files, "main");
        let (_, errors) = linker.link_module("main", &lg.modules["main"], &lg.registry);

        assert!(!errors.is_empty(), "Should have failed");
        assert_eq!(errors.0[0].to_string(), "Unknown symbol: 'it'");
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

        let (lg, linker) = setup_project(&files, "main");
        let (linked_mod, errors) = linker.link_module("main", &lg.modules["main"], &lg.registry);

        assert!(errors.is_empty());

        let node = &linked_mod.nodes[0].value;
        match &node.statements[0].value {
            LinkedNodeStatement::Match(m) => match &m.query_ref.value {
                LinkedMatchQueryReference::Raw(s) => assert_eq!(s, "(node_pattern) @cap"),
                _ => panic!("Expected Raw query reference"),
            },
            _ => panic!("Expected match statement"),
        }
    }
}
