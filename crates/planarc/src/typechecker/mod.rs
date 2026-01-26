pub mod error;
pub mod meta;
pub mod typed_ast;

use std::collections::BTreeMap;

use tracing::{debug, info, instrument, trace, warn};

use crate::checked::Checked;
use crate::linker::linked_ast::*;
use crate::linker::linked_world::LinkedWorld;
use crate::linker::meta::{ResolvedId, SymbolId, SymbolKind};
use crate::linker::symbol_table::SymbolTable;
use crate::scope::ScopeStack;
use crate::source_registry::SourceRegistry;
use crate::spanned::{Location, Spanned, ToSpanned};
use crate::typechecker::error::TypeError;
use crate::typechecker::typed_ast::*;

use self::error::TypeErrors;

#[instrument(skip_all)]
pub fn check_world(world: LinkedWorld) -> Checked<TypedWorld, TypeErrors> {
    info!(
        modules_count = world.modules.len(),
        "Starting type checking pipeline"
    );

    let LinkedWorld {
        table,
        modules,
        registry,
    } = world;

    let mut checker = TypeChecker::new(&table, &registry);
    let mut errors = TypeErrors::default();
    let mut typed_modules = BTreeMap::new();

    for (name, module) in modules {
        let _span = tracing::debug_span!("module", name = %name).entered();
        let typed_mod = checker.check_module(module).sink(&mut errors);
        typed_modules.insert(name, typed_mod);
    }

    info!("Type checking complete. Errors: {}", errors.len());

    Checked::with_errors(
        TypedWorld {
            modules: typed_modules,
            table,
        },
        errors,
    )
}

pub struct TypeChecker<'a> {
    pub table: &'a SymbolTable,
    pub registry: &'a SourceRegistry,
    scopes: ScopeStack<SymbolId>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(table: &'a SymbolTable, registry: &'a SourceRegistry) -> Self {
        Self {
            table,
            registry,
            scopes: ScopeStack::new(),
        }
    }

    #[instrument(skip(self))]
    fn builtin(&self, name: &str) -> SymbolId {
        let fqmn = format!("builtin.{}", name);
        self.table
            .name_to_id
            .get(&fqmn)
            .cloned()
            .expect("Builtin missing")
    }

    #[instrument(skip_all)]
    fn check_module(&mut self, m: LinkedModule) -> Checked<TypedModule, TypeErrors> {
        let mut errors = TypeErrors::default();

        let typed = TypedModule {
            file_id: m.file_id,
            grammar: m.grammar,
            facts: m
                .facts
                .into_iter()
                .map(|f| self.map_spanned(f, Self::map_fact).sink(&mut errors))
                .collect(),
            types: m
                .types
                .into_iter()
                .map(|t| self.map_spanned(t, Self::map_type).sink(&mut errors))
                .collect(),
            externs: m
                .externs
                .into_iter()
                .map(|e| self.map_spanned(e, Self::map_extern_def).sink(&mut errors))
                .collect(),
            queries: m
                .queries
                .into_iter()
                .map(|q| self.map_spanned(q, Self::map_query).sink(&mut errors))
                .collect(),
            nodes: m
                .nodes
                .into_iter()
                .map(|n| self.map_spanned(n, Self::check_node).sink(&mut errors))
                .collect(),
            edges: m
                .edges
                .into_iter()
                .map(|e| self.map_spanned(e, Self::map_edge).sink(&mut errors))
                .collect(),
        };

        Checked::with_errors(typed, errors)
    }

    fn map_spanned<L, T>(
        &mut self,
        s: Spanned<L>,
        f: impl FnOnce(&mut Self, L) -> Checked<T, TypeErrors>,
    ) -> Checked<Spanned<T>, TypeErrors> {
        let loc = s.loc;
        f(self, s.value).map(|v| v.spanned(loc))
    }

    // --- Mappers ---

    fn map_fact(&mut self, f: LinkedFact) -> Checked<TypedFact, TypeErrors> {
        let mut errors = TypeErrors::default();
        let fact = TypedFact {
            id: f.id,
            name: f.name,
            attributes: f
                .attributes
                .into_iter()
                .map(|a| self.map_spanned(a, Self::map_attribute).sink(&mut errors))
                .collect(),
            fields: f
                .fields
                .into_iter()
                .map(|f| self.map_spanned(f, Self::map_field).sink(&mut errors))
                .collect(),
        };
        Checked::with_errors(fact, errors)
    }

    fn map_type(&mut self, t: LinkedType) -> Checked<TypedType, TypeErrors> {
        let mut errors = TypeErrors::default();
        let typed = TypedType {
            id: t.id,
            name: t.name,
            attributes: t
                .attributes
                .into_iter()
                .map(|a| self.map_spanned(a, Self::map_attribute).sink(&mut errors))
                .collect(),
            definition: self
                .map_spanned(t.definition, |this, v| this.map_type_definition(v, t.id))
                .sink(&mut errors),
        };
        Checked::with_errors(typed, errors)
    }

    fn map_type_definition(
        &mut self,
        d: LinkedTypeDefinition,
        owner_id: SymbolId,
    ) -> Checked<TypedTypeDefinition, TypeErrors> {
        let mut errors = TypeErrors::default();
        let def = TypedTypeDefinition {
            base_type: d
                .base_type
                .map(|v| self.map_type_reference(v, owner_id).sink(&mut errors)),
            fields: vec![],
        };
        Checked::with_errors(def, errors)
    }

    fn map_type_reference(
        &mut self,
        r: LinkedTypeReference,
        owner_id: SymbolId,
    ) -> Checked<TypedTypeReference, TypeErrors> {
        let mut errors = TypeErrors::default();
        let symbol_id = match &r.symbol.value {
            ResolvedId::Global(s) => s.value,
            _ => SymbolId::INVALID_ID,
        };

        let ty = self.id_to_type(symbol_id);

        let refinement = r.refinement.map(|expr| {
            self.scopes.push();
            self.scopes.define("it".into(), symbol_id);
            let checked = self.check_expression(expr.clone()).sink(&mut errors);
            self.scopes.pop();
            checked
        });

        let res = TypedTypeReference {
            ty,
            symbol: r.symbol.map(|s| match s {
                ResolvedId::Global(gs) => gs.value,
                _ => SymbolId::INVALID_ID,
            }),
            args: r
                .args
                .into_iter()
                .map(|a| {
                    self.map_spanned(a, |this, v| this.map_type_reference(v, owner_id))
                        .sink(&mut errors)
                })
                .collect(),
            refinement,
        };
        Checked::with_errors(res, errors)
    }

    fn map_field(&mut self, f: LinkedField) -> Checked<TypedField, TypeErrors> {
        let mut errors = TypeErrors::default();
        let res = TypedField {
            attributes: f
                .attributes
                .into_iter()
                .map(|a| self.map_spanned(a, Self::map_attribute).sink(&mut errors))
                .collect(),
            name: f.name,
            ty: self
                .map_type_reference(f.ty, SymbolId::INVALID_ID)
                .sink(&mut errors),
        };
        Checked::with_errors(res, errors)
    }

    fn map_attribute(&mut self, a: LinkedAttribute) -> Checked<TypedAttribute, TypeErrors> {
        let mut errors = TypeErrors::default();
        let res = TypedAttribute {
            name: a.name,
            args: a
                .args
                .into_iter()
                .map(|arg| self.check_expression(arg).sink(&mut errors))
                .collect(),
        };
        Checked::with_errors(res, errors)
    }

    fn map_extern_def(
        &mut self,
        e: LinkedExternDefinition,
    ) -> Checked<TypedExternDefinition, TypeErrors> {
        let mut errors = TypeErrors::default();
        let res = TypedExternDefinition {
            functions: e
                .functions
                .into_iter()
                .map(|f| self.map_spanned(f, Self::map_extern_fn).sink(&mut errors))
                .collect(),
        };
        Checked::with_errors(res, errors)
    }

    fn map_extern_fn(
        &mut self,
        v: LinkedExternFunction,
    ) -> Checked<TypedExternFunction, TypeErrors> {
        let mut errors = TypeErrors::default();
        let res = TypedExternFunction {
            id: v.id,
            name: v.name,
            args: v
                .args
                .into_iter()
                .map(|a| self.map_spanned(a, Self::map_extern_arg).sink(&mut errors))
                .collect(),
            return_ty: v.return_ty.map(|r| {
                self.map_type_reference(r, SymbolId::INVALID_ID)
                    .sink(&mut errors)
            }),
        };
        Checked::with_errors(res, errors)
    }

    fn map_extern_arg(
        &mut self,
        arg: LinkedExternArgument,
    ) -> Checked<TypedExternArgument, TypeErrors> {
        let mut errors = TypeErrors::default();
        let res = TypedExternArgument {
            name: arg.name,
            ty: self
                .map_type_reference(arg.ty, SymbolId::INVALID_ID)
                .sink(&mut errors),
        };
        Checked::with_errors(res, errors)
    }

    fn map_query(&mut self, q: LinkedQuery) -> Checked<TypedQuery, TypeErrors> {
        Checked::new(TypedQuery {
            id: q.id,
            name: q.name,
            query: q.query,
        })
    }

    fn map_edge(&mut self, e: LinkedEdge) -> Checked<TypedEdge, TypeErrors> {
        Checked::new(TypedEdge {
            id: e.id,
            name: e.name,
            from: e.from,
            to: e.to,
            relation: e.relation,
        })
    }

    // --- Nodes ---

    #[instrument(skip(self, node))]
    fn check_node(&mut self, node: LinkedNode) -> Checked<TypedNode, TypeErrors> {
        let mut errors = TypeErrors::default();
        let typed = TypedNode {
            id: node.id,
            kind: node.kind,
            statements: node
                .statements
                .into_iter()
                .map(|s| match s {
                    LinkedNodeStatement::Match(m) => {
                        TypedNodeStatement::Match(self.check_match(m).sink(&mut errors))
                    }
                    LinkedNodeStatement::Query(q) => {
                        TypedNodeStatement::Query(q.map(|v| TypedQuery {
                            id: v.id,
                            name: v.name,
                            query: v.query,
                        }))
                    }
                })
                .collect(),
        };
        Checked::with_errors(typed, errors)
    }

    #[instrument(skip(self, m))]
    fn check_match(
        &mut self,
        m: Spanned<LinkedMatchStatement>,
    ) -> Checked<Spanned<TypedMatchStatement>, TypeErrors> {
        let mut errors = TypeErrors::default();
        self.scopes.push();

        match &m.value.query_ref.value {
            LinkedMatchQueryReference::Named(id) => {
                if let Some(meta) = self.table.get_metadata_by_id(*id)
                    && let SymbolKind::Query { captures, .. } = &meta.kind
                {
                    for cap in captures {
                        self.scopes.define(cap.value.clone(), self.builtin("str"));
                    }
                }
            }
            LinkedMatchQueryReference::Raw { captures, .. } => {
                for cap in captures {
                    self.scopes.define(cap.value.clone(), self.builtin("str"));
                }
            }
        }

        let body = m
            .value
            .body
            .into_iter()
            .map(|item| {
                self.map_spanned(item.clone(), |this, val| match val {
                    LinkedMatchItem::Let(l) => {
                        let expr = this.check_expression(l.value).sink(&mut errors);
                        this.scopes
                            .define(l.name.value.clone(), expr.value.ty.to_id(this));
                        Checked::new(TypedMatchItem::Let(TypedLetBinding {
                            name: l.name,
                            value: expr,
                        }))
                    }
                    LinkedMatchItem::Capture(c) => {
                        this.scopes
                            .define(c.name.value.clone(), this.builtin("str"));
                        this.scopes.push();
                        let inner = c
                            .body
                            .into_iter()
                            .map(|i| {
                                i.clone().map(|_| {
                                    TypedMatchItem::Let(TypedLetBinding {
                                        name: "stub".to_string().spanned(i.loc),
                                        value: this.dummy_expr().spanned(i.loc),
                                    })
                                })
                            })
                            .collect();
                        this.scopes.pop();
                        Checked::new(TypedMatchItem::Capture(TypedCapture {
                            name: c.name,
                            body: inner,
                        }))
                    }
                    LinkedMatchItem::Emit(e) => Checked::new(TypedMatchItem::Emit(
                        this.check_emit(e, item.loc, &mut errors),
                    )),
                })
                .sink(&mut errors)
            })
            .collect();

        self.scopes.pop();

        let query_ref = match m.value.query_ref.value {
            LinkedMatchQueryReference::Named(id) => TypedMatchQueryReference::Global(id),
            LinkedMatchQueryReference::Raw { captures, source } => {
                TypedMatchQueryReference::Raw { captures, source }
            }
        }
        .spanned(m.value.query_ref.loc);

        Checked::with_errors(
            TypedMatchStatement { query_ref, body }.spanned(m.loc),
            errors,
        )
    }

    fn check_emit(
        &mut self,
        e: LinkedEmitStatement,
        loc: Location,
        errors: &mut TypeErrors,
    ) -> TypedEmitStatement {
        TypedEmitStatement {
            left: self.check_emitted_fact(e.left, loc, errors),
            right: e.right.map(|r| self.check_emitted_fact(r, loc, errors)),
            relation: e.relation,
            direction: e.direction.map(|d| match d {
                RelationDirection::Left => TypedRelationDirection::Left,
                RelationDirection::Right => TypedRelationDirection::Right,
                RelationDirection::Both => TypedRelationDirection::Both,
            }),
        }
    }

    fn check_emitted_fact(
        &mut self,
        ef: LinkedEmittedFact,
        loc: Location,
        errors: &mut TypeErrors,
    ) -> TypedEmittedFact {
        let meta = self
            .table
            .get_metadata_by_id(ef.fact_id)
            .expect("Fact missing");
        let mut fields = Vec::new();

        if let SymbolKind::Fact { fields: def_fields } = &meta.kind {
            for f in ef.fields {
                let typed_val = self.check_expression(f.value).sink(errors);
                let actual_ty_id = typed_val.value.ty.to_id(self);

                if let Some(field_def) = def_fields.iter().find(|df| df.name == f.name.value) {
                    if actual_ty_id != field_def.type_id {
                        let (src, span) = self.registry.get_source_and_span(f.name.loc);
                        errors.push(Box::new(TypeError::TypeMismatch {
                            expected: self
                                .table
                                .get_fqmn(field_def.type_id)
                                .cloned()
                                .unwrap_or_default(),
                            found: format!("{:?}", typed_val.value.ty),
                            src,
                            span,
                            loc: f.name.loc,
                        }));
                    }
                } else {
                    let (src, span) = self.registry.get_source_and_span(f.name.loc);
                    errors.push(Box::new(TypeError::UndefinedField {
                        fact_name: meta.fqmn.clone(),
                        field_name: f.name.value.clone(),
                        src,
                        span,
                        loc: f.name.loc,
                    }));
                }
                fields.push(TypedEmittedField {
                    name: f.name,
                    value: typed_val,
                });
            }
        }
        TypedEmittedFact {
            fact_id: ef.fact_id,
            fields,
        }
    }

    // --- Expressions ---

    fn check_expression(
        &mut self,
        expr: Spanned<LinkedExpression>,
    ) -> Checked<Spanned<TypedExpression>, TypeErrors> {
        let mut errors = TypeErrors::default();
        let (kind, ty) = match expr.value {
            LinkedExpression::Number(n) => (TypedExpressionKind::Number(n), Type::I64),
            LinkedExpression::StringLit(s) => (TypedExpressionKind::StringLit(s), Type::Str),
            LinkedExpression::Identifier(id) => match id {
                ResolvedId::Local(name) => {
                    let ty_id = self.scopes.lookup(&name.value).cloned().unwrap_or_else(|| {
                        let (src, span) = self.registry.get_source_and_span(name.loc);
                        errors.push(Box::new(TypeError::UnknownSymbol {
                            name: name.value.clone(),
                            src,
                            span,
                            loc: name.loc,
                        }));
                        SymbolId::INVALID_ID
                    });
                    (
                        TypedExpressionKind::LocalIdentifier(name.value),
                        self.id_to_type(ty_id),
                    )
                }
                ResolvedId::Global(gs) => (
                    TypedExpressionKind::Identifier(gs.value),
                    self.id_to_type(gs.value),
                ),
            },
            LinkedExpression::Binary {
                left,
                operator,
                right,
            } => {
                let l = self.check_expression(*left.clone()).sink(&mut errors);
                let r = self.check_expression(*right.clone()).sink(&mut errors);
                let op_id = match operator.value {
                    ResolvedId::Global(gs) => gs.value,
                    _ => SymbolId::INVALID_ID,
                };
                (
                    TypedExpressionKind::Binary {
                        left: Box::new(l),
                        operator: op_id,
                        right: Box::new(r),
                    },
                    Type::Unknown,
                )
            }
            LinkedExpression::Call { function, args } => {
                let f = self.check_expression(*function.clone()).sink(&mut errors);
                let a = args
                    .into_iter()
                    .map(|arg| self.check_expression(arg.clone()).sink(&mut errors))
                    .collect();
                (
                    TypedExpressionKind::Call {
                        function: Box::new(f),
                        args: a,
                    },
                    Type::Unknown,
                )
            }
            _ => (TypedExpressionKind::Number("0".into()), Type::Unknown),
        };

        Checked::with_errors(TypedExpression { ty, kind }.spanned(expr.loc), errors)
    }

    fn id_to_type(&self, id: SymbolId) -> Type {
        let fqmn = self.table.get_fqmn(id).map(|s| s.as_str()).unwrap_or("");
        match fqmn {
            "builtin.i64" => Type::I64,
            "builtin.str" => Type::Str,
            "builtin.bool" => Type::Bool,
            _ => Type::Fact(id),
        }
    }

    fn dummy_expr(&self) -> TypedExpression {
        TypedExpression {
            ty: Type::Unknown,
            kind: TypedExpressionKind::Number("0".into()),
        }
    }
}

impl Type {
    fn to_id(&self, checker: &TypeChecker) -> SymbolId {
        match self {
            Type::I64 => checker.builtin("i64"),
            Type::Str => checker.builtin("str"),
            Type::Bool => checker.builtin("bool"),
            Type::Fact(id) | Type::User(id) | Type::Node(id) => *id,
            Type::Unknown => SymbolId::INVALID_ID,
            _ => SymbolId::INVALID_ID,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::linker::linker::link_to_world;
    use crate::linker::linker::tests::setup_lowered_graph;

    fn check_text(files: &[(&str, &str)]) -> (TypeErrors, SymbolTable) {
        let lg = setup_lowered_graph(files);
        let (world, linker_errs) = link_to_world(vec![], lg);

        assert!(linker_errs.is_empty(), "Linker errors: {:?}", linker_errs);

        let (world, errors) = check_world(world).into_parts();
        (errors, world.table)
    }

    #[test]
    fn test_emit_type_error() {
        let (errors, _) = check_text(&[(
            "main",
            r#"
                fact User { id: builtin.i64 }
                node N {
                    match `(node)` {
                        emit User { id: "string_instead_of_int" }
                    }
                }
            "#,
        )]);

        assert!(!errors.is_empty(), "Should have type errors");
        assert!(
            errors
                .0
                .iter()
                .any(|e| matches!(e.as_ref(), TypeError::TypeMismatch { .. })),
            "Expected TypeMismatch, got: {:?}",
            errors
        );
    }

    #[test]
    fn test_emit_undefined_field() {
        let (errors, _) = check_text(&[(
            "main",
            r#"
                fact User { id: builtin.i64 }
                node N {
                    match `(node)` {
                        emit User { unknown_field: 1 }
                    }
                }
            "#,
        )]);

        assert!(
            errors
                .0
                .iter()
                .any(|e| matches!(e.as_ref(), TypeError::UndefinedField { .. })),
            "Expected UndefinedField, got: {:?}",
            errors
        );
    }

    #[test]
    fn test_local_variable_resolution() {
        let (errors, _) = check_text(&[(
            "main",
            r#"
                fact User { id: builtin.i64 }
                node N {
                    match `(node)` {
                        let x = 42
                        emit User { id: x }
                    }
                }
            "#,
        )]);

        assert!(
            errors.is_empty(),
            "Should resolve local variable 'x', but got: {:?}",
            errors
        );
    }

    #[test]
    fn test_capture_resolution() {
        let (errors, _) = check_text(&[(
            "main",
            r#"
                fact Log { msg: builtin.str }
                node N {
                    match `(identifier) @name` {
                        emit Log { msg: @name }
                    }
                }
            "#,
        )]);

        assert!(
            errors.is_empty(),
            "Should resolve capture @name, but got: {:?}",
            errors
        );
    }

    #[test]
    fn test_type_mismatch_with_local_var() {
        let (errors, _) = check_text(&[(
            "main",
            r#"
                fact User { id: builtin.i64 }
                node N {
                    match `(node)` {
                        let x = "not_an_int"
                        emit User { id: x }
                    }
                }
            "#,
        )]);

        assert!(
            errors
                .0
                .iter()
                .any(|e| matches!(e.as_ref(), TypeError::TypeMismatch { .. })),
            "Expected error because 'x' is str and User.id is i64"
        );
    }
}
