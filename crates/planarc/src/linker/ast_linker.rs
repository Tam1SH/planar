use super::meta::{FieldMetadata, FunctionParam, Visibility};
use crate::ast;
use crate::checked::{Checked, CheckedIteratorExt};
use crate::linker::error::{LinkerError, LinkerErrors};
use crate::linker::linked_ast::*;
use crate::linker::lookup::SymbolLookup;
use crate::linker::meta::{ResolvedId, SymbolId, SymbolKind};
use crate::linker::node_linker::NodeLinker;
use crate::spanned::{Location, Spanned, ToSpanned};
use rkyv::Archive;
use tracing::{debug, error, instrument, trace};

pub struct AstLinker<'a> {
    pub lookup: SymbolLookup<'a>,
}

pub struct SignatureUpdates {
    pub kinds: Vec<(SymbolId, SymbolKind)>,
    pub visibilities: Vec<(SymbolId, Visibility)>,
}

impl<'a> AstLinker<'a> {
    pub fn new(lookup: SymbolLookup<'a>) -> Self {
        Self { lookup }
    }

    pub fn link_vec<T, U>(
        &self,
        items: &[Spanned<T>],
        f: impl Fn(&Self, &T, Location) -> Checked<Spanned<U>, LinkerErrors>,
    ) -> Checked<Vec<Spanned<U>>, LinkerErrors> {
        items
            .iter()
            .map(|item| f(self, &item.value, item.loc))
            .collect_checked()
    }

    pub fn resolve_module_signatures(
        &self,
        module: &ast::Module,
    ) -> Checked<SignatureUpdates, LinkerErrors> {
        let mut kinds = Vec::new();
        let mut visibilities = Vec::new();
        let mut errors = LinkerErrors::default();

        for fact in &module.facts {
            if let Ok(res) = self
                .lookup
                .find_symbol(&fact.value.name.value, fact.value.name.loc)
            {
                let id = res.symbol_id();
                let mut fields = Vec::new();

                for field in &fact.value.fields {
                    let ty_res = self.resolve_type_ref(&field.value.ty).sink(&mut errors);

                    let type_id = ty_res.symbol.value.symbol_id();

                    fields.push(FieldMetadata {
                        name: field.value.name.value.clone(),
                        type_id,
                        attributes: field
                            .value
                            .attributes
                            .iter()
                            .map(|a| a.value.name.value.clone())
                            .collect(),
                        location: field.loc,
                    });
                }
                kinds.push((id, SymbolKind::Fact { fields }));
            }
        }

        for edge in &module.edges {
            if let Ok(res) = self
                .lookup
                .find_symbol(&edge.value.name.value, edge.value.name.loc)
            {
                let id = res.symbol_id();

                let from_res = self
                    .resolve_edge_endpoint(&edge.value.from.value, edge.value.from.loc)
                    .sink(&mut errors);
                let to_res = self
                    .resolve_edge_endpoint(&edge.value.to.value, edge.value.to.loc)
                    .sink(&mut errors);

                kinds.push((
                    id,
                    SymbolKind::Edge {
                        from: from_res,
                        to: to_res,
                    },
                ));
            }
        }

        for ext in &module.externs {
            for func in &ext.value.functions {
                if let Ok(res) = self
                    .lookup
                    .find_symbol(&func.value.name.value, func.value.name.loc)
                {
                    let id = res.symbol_id();
                    let mut params = Vec::new();

                    for arg in &func.value.args {
                        let arg_ty_res =
                            self.resolve_type_ref(&arg.value.ty.value).sink(&mut errors);

                        let type_id = arg_ty_res.symbol.value.symbol_id();

                        params.push(FunctionParam {
                            name: arg.value.name.value.clone(),
                            type_id,
                            location: arg.loc,
                        });
                    }

                    let return_type = func.value.return_type.as_ref().map(|rt| {
                        let ret_ty_res = self.resolve_type_ref(&rt.value).sink(&mut errors);
                        ret_ty_res.symbol.value.symbol_id()
                    });

                    kinds.push((
                        id,
                        SymbolKind::ExternFunction {
                            params,
                            return_type,
                        },
                    ));
                }
            }
        }

        for node in &module.nodes {
            let node_fqmn = format!("{}.{}", self.lookup.current_module, node.value.kind.value);

            if let Some(node_meta) = self.lookup.table.resolve_metadata(&node_fqmn) {
                let node_id = node_meta.id;

                for stmt in &node.value.statements {
                    if let ast::NodeStatement::Query(q) = &stmt.value {
                        let query_fqmn = format!("{}.{}", node_fqmn, q.value.name.value);

                        if let Some(query_meta) = self.lookup.table.resolve_metadata(&query_fqmn) {
                            visibilities.push((query_meta.id, Visibility::Scoped(node_id)));
                        }
                    }
                }
            }
        }
        Checked::with_errors(
            SignatureUpdates {
                kinds,
                visibilities,
            },
            errors,
        )
    }

    #[instrument(skip(self, loc, q), fields(query = %q.name.value))]
    pub fn resolve_query(
        &self,
        q: &ast::QueryDefinition,
        loc: Location,
    ) -> Checked<Spanned<LinkedQuery>, LinkerErrors> {
        let fqmn = format!("{}.{}", self.lookup.current_module, q.name.value);
        let mut errors = LinkerErrors::default();

        let id = match self.lookup.table.resolve_metadata(&fqmn) {
            Some(m) => m.id,
            None => {
                errors.push(self.lookup.error_unknown(&q.name.value, q.name.loc, None));
                SymbolId::INVALID_ID
            }
        };

        Checked::with_errors(
            LinkedQuery {
                id,
                name: q.name.value.clone(),
                query: q.value.value.clone(),
            }
            .spanned(loc),
            errors,
        )
    }

    #[instrument(skip(self, loc, ext))]
    pub fn resolve_extern_definition(
        &self,
        ext: &ast::ExternDefinition,
        loc: Location,
    ) -> Checked<Spanned<LinkedExternDefinition>, LinkerErrors> {
        let mut errors = LinkerErrors::default();

        let is_builtin = ext
            .attributes
            .iter()
            .any(|a| a.value.name.value == "builtin");

        let prefix = if is_builtin {
            "builtin".into()
        } else {
            self.lookup.current_module.clone()
        };

        let mut functions = Vec::new();
        for f in &ext.functions {
            let fqmn = format!("{}.{}", prefix, f.value.name.value);
            let meta = self.lookup.table.resolve_metadata(&fqmn);

            if meta.is_none() {
                errors.push(
                    self.lookup
                        .error_unknown(&f.value.name.value, f.value.name.loc, None),
                );
            }

            let mut args = Vec::new();
            for a in &f.value.args {
                let arg_ty = self.resolve_type_ref(&a.value.ty.value).sink(&mut errors);

                args.push(Spanned::new(
                    LinkedExternArgument {
                        name: a.value.name.value.clone(),
                        ty: arg_ty,
                    },
                    a.loc,
                ));
            }

            let return_ty = f
                .value
                .return_type
                .as_ref()
                .map(|r| self.resolve_type_ref(&r.value).sink(&mut errors));

            functions.push(Spanned::new(
                LinkedExternFunction {
                    id: meta.map(|m| m.id).unwrap_or(SymbolId::INVALID_ID),
                    name: f.value.name.value.clone(),
                    args,
                    return_ty,
                },
                f.loc,
            ));
        }

        Checked::with_errors(
            Spanned::new(LinkedExternDefinition { functions }, loc),
            errors,
        )
    }

    #[instrument(skip(self, loc, ty), fields(type = %ty.name.value))]
    pub fn resolve_type_decl(
        &self,
        ty: &ast::TypeDeclaration,
        loc: Location,
    ) -> Checked<Spanned<LinkedType>, LinkerErrors> {
        let fqmn = format!("{}.{}", self.lookup.current_module, ty.name.value);
        let mut errors = LinkerErrors::default();

        let meta = self.lookup.table.resolve_metadata(&fqmn);
        if meta.is_none() {
            errors.push(self.lookup.error_unknown(&ty.name.value, ty.name.loc, None));
        }

        let def_res = self
            .resolve_type_def(&ty.definition.value, ty.definition.loc)
            .sink(&mut errors);

        Checked::with_errors(
            Spanned::new(
                LinkedType {
                    id: meta.map(|m| m.id).unwrap_or(SymbolId::INVALID_ID),
                    name: ty.name.value.clone(),
                    attributes: self.resolve_attributes(ty.attributes.as_slice()),
                    definition: def_res,
                },
                loc,
            ),
            errors,
        )
    }

    #[instrument(skip(self, loc, fact), fields(fact = %fact.name.value))]
    pub fn resolve_fact(
        &self,
        fact: &ast::FactDefinition,
        loc: Location,
    ) -> Checked<Spanned<LinkedFact>, LinkerErrors> {
        let fqmn = format!("{}.{}", self.lookup.current_module, fact.name.value);
        let mut errors = LinkerErrors::default();

        let meta_res = self.lookup.table.resolve_metadata(&fqmn);
        if meta_res.is_none() {
            errors.push(
                self.lookup
                    .error_unknown(&fact.name.value, fact.name.loc, None),
            );
        }

        let fields = fact
            .fields
            .iter()
            .map(|f| {
                Spanned::new(
                    LinkedField {
                        attributes: self.resolve_attributes(&f.value.attributes),
                        name: f.value.name.value.clone(),
                        ty: self.resolve_type_ref(&f.value.ty).sink(&mut errors),
                    },
                    f.loc,
                )
            })
            .collect();

        Checked::with_errors(
            Spanned::new(
                LinkedFact {
                    id: meta_res.map(|m| m.id).unwrap_or(SymbolId::INVALID_ID),
                    attributes: self.resolve_attributes(&fact.attributes),
                    name: fact.name.value.clone(),
                    fields,
                },
                loc,
            ),
            errors,
        )
    }

    #[instrument(skip(self, loc, node), fields(node_kind = %node.kind.value))]
    pub fn resolve_node(
        &self,
        node: &ast::NodeDefinition,
        loc: Location,
    ) -> Checked<Spanned<LinkedNode>, LinkerErrors> {
        let fqmn = format!("{}.{}", self.lookup.current_module, node.kind.value);
        let mut errors = LinkerErrors::default();

        debug!(fqmn = %fqmn, "Resolving node definition");
        let node_id = self.lookup.resolve_id(&fqmn, loc).sink(&mut errors);

        let node_linker = NodeLinker::new(&self.lookup, node_id, fqmn);
        let statements = node_linker
            .resolve_statements(&node.statements)
            .sink(&mut errors);

        Checked::with_errors(
            Spanned::new(
                LinkedNode {
                    id: node_id,
                    kind: node.kind.value.clone(),
                    statements,
                },
                loc,
            ),
            errors,
        )
    }

    #[instrument(skip(self, loc, edge), fields(edge = %edge.name.value))]
    pub fn resolve_edge(
        &self,
        edge: &ast::EdgeDefinition,
        loc: Location,
    ) -> Checked<Spanned<LinkedEdge>, LinkerErrors> {
        let fqmn = format!("{}.{}", self.lookup.current_module, edge.name.value);
        let mut errors = LinkerErrors::default();

        let meta = self.lookup.table.resolve_metadata(&fqmn);
        let mut from_id = SymbolId::INVALID_ID;
        let mut to_id = SymbolId::INVALID_ID;

        if let Some(m) = meta {
            if let SymbolKind::Edge { from, to } = m.kind {
                from_id = from;
                to_id = to;
            }
        } else {
            errors.push(
                self.lookup
                    .error_unknown(&edge.name.value, edge.name.loc, None),
            );
        }

        Checked::with_errors(
            Spanned::new(
                LinkedEdge {
                    id: meta.map(|m| m.id).unwrap_or(SymbolId::INVALID_ID),
                    name: edge.name.value.clone(),
                    from: from_id,
                    to: to_id,
                    relation: edge.relation.value.clone(),
                },
                loc,
            ),
            errors,
        )
    }

    pub fn resolve_edge_endpoint(
        &self,
        name: &str,
        loc: Location,
    ) -> Checked<SymbolId, LinkerErrors> {
        let mut errors = LinkerErrors::default();

        let res_id = match self.lookup.find_symbol(name, loc) {
            Ok(res) => res.symbol_id(),
            Err(e) => {
                errors.push(e);
                return Checked::with_errors(SymbolId::INVALID_ID, errors);
            }
        };

        let meta = self.lookup.table.get_metadata_by_id(res_id);
        match meta {
            Some(m) if matches!(m.kind, SymbolKind::Fact { .. }) => Checked::new(res_id),
            _ => {
                let found_kind = meta
                    .map(|m| self.format_kind(&m.kind))
                    .unwrap_or_else(|| "Unknown".to_string());
                let (src, span) = self.lookup.registry.get_source_and_span(loc);
                errors.push(Box::new(LinkerError::InvalidSymbolKind {
                    name: name.to_string(),
                    expected: "Fact or Node".to_string(),
                    found: found_kind,
                    src,
                    span,
                    loc,
                }));
                Checked::with_errors(SymbolId::INVALID_ID, errors)
            }
        }
    }

    fn format_kind(&self, kind: &SymbolKind) -> String {
        match kind {
            SymbolKind::Type { .. } => "Type",
            SymbolKind::Fact { .. } => "Fact",
            SymbolKind::Node => "Node",
            SymbolKind::Query { .. } => "Query",
            SymbolKind::ExternFunction { .. } => "ExternFunction",
            SymbolKind::Edge { .. } => "Edge",
        }
        .to_string()
    }

    pub fn resolve_type_ref(
        &self,
        ty: &ast::TypeAnnotation,
    ) -> Checked<LinkedTypeReference, LinkerErrors> {
        let mut errors = LinkerErrors::default();

        let symbol = match self.lookup.find_symbol(&ty.name.value, ty.name.loc) {
            Ok(res) => res,
            Err(e) => {
                errors.push(e);
                ResolvedId::Global(Spanned::new(SymbolId::INVALID_ID, ty.name.loc))
            }
        };

        let args = ty
            .args
            .iter()
            .map(|arg| Spanned::new(self.resolve_type_ref(&arg.value).sink(&mut errors), arg.loc))
            .collect();

        let refinement = ty
            .refinement
            .as_ref()
            .map(|expr| self.resolve_expr(&expr.value, expr.loc).sink(&mut errors));

        Checked::with_errors(
            LinkedTypeReference {
                symbol: Spanned::new(symbol, ty.name.loc),
                args,
                refinement,
            },
            errors,
        )
    }

    pub fn resolve_type_def(
        &self,
        def: &ast::TypeDefinition,
        loc: Location,
    ) -> Checked<Spanned<LinkedTypeDefinition>, LinkerErrors> {
        let mut errors = LinkerErrors::default();

        let base_type = def
            .base_type
            .as_ref()
            .map(|base| self.resolve_type_ref(base).sink(&mut errors));

        Checked::with_errors(
            Spanned::new(LinkedTypeDefinition { base_type }, loc),
            errors,
        )
    }

    #[instrument(skip(self, loc, expr))]
    pub fn resolve_expr(
        &self,
        expr: &ast::Expression,
        loc: Location,
    ) -> Checked<Spanned<LinkedExpression>, LinkerErrors> {
        let mut errors = LinkerErrors::default();

        let linked = match expr {
            ast::Expression::It => {
                trace!(target: "linker::resolver", "Resolved 'it' context variable");
                LinkedExpression::Identifier(ResolvedId::Local(Spanned::new("it".to_string(), loc)))
            }

            ast::Expression::Identifier(name) => match self.lookup.find_symbol(name, loc) {
                Ok(res) => {
                    debug!(target: "linker::resolver", symbol = %name, "Resolved identifier");
                    LinkedExpression::Identifier(res)
                }
                Err(e) => {
                    errors.push(e);
                    LinkedExpression::Identifier(ResolvedId::Local(Spanned::new(name.clone(), loc)))
                }
            },

            ast::Expression::Number(n) => LinkedExpression::Number(n.clone()),

            ast::Expression::StringLit(s) => LinkedExpression::StringLit(s.clone()),

            ast::Expression::Binary { left, op, right } => {
                trace!(target: "linker::resolver", op = %op.value, "Resolving binary expression");

                let l_res = self.resolve_expr(&left.value, left.loc).sink(&mut errors);
                let r_res = self.resolve_expr(&right.value, right.loc).sink(&mut errors);

                let operator_res = match self.lookup.find_symbol(&op.value, op.loc) {
                    Ok(res) => Spanned::new(res, op.loc),
                    Err(e) => {
                        errors.push(e);
                        Spanned::new(
                            ResolvedId::Local(Spanned::new(op.value.clone(), op.loc)),
                            op.loc,
                        )
                    }
                };

                LinkedExpression::Binary {
                    left: Box::new(l_res),
                    operator: operator_res,
                    right: Box::new(r_res),
                }
            }

            ast::Expression::Call { function, args } => {
                trace!(target: "linker::resolver", "Resolving call expression");

                let f_res = self
                    .resolve_expr(&function.value, function.loc)
                    .sink(&mut errors);

                let mut linked_args = Vec::with_capacity(args.len());
                for arg in args {
                    let a_res = self.resolve_expr(&arg.value, arg.loc).sink(&mut errors);
                    linked_args.push(a_res);
                }

                LinkedExpression::Call {
                    function: Box::new(f_res),
                    args: linked_args,
                }
            }

            ast::Expression::InList(items) => {
                trace!(target: "linker::resolver", "Resolving InList expression");
                let mut linked_items = Vec::with_capacity(items.len());
                for item in items {
                    let i_res = self.resolve_expr(&item.value, item.loc).sink(&mut errors);
                    linked_items.push(i_res);
                }
                LinkedExpression::InList(linked_items)
            }

            ast::Expression::InRange { start, end } => {
                trace!(target: "linker::resolver", "Resolving InRange expression");
                let s_res = self.resolve_expr(&start.value, start.loc).sink(&mut errors);

                let linked_end = end.as_ref().map(|e| {
                    let e_res = self.resolve_expr(&e.value, e.loc).sink(&mut errors);
                    Box::new(e_res)
                });

                LinkedExpression::InRange {
                    start: Box::new(s_res),
                    end: linked_end,
                }
            }

            ast::Expression::OperatorIdentifier(op_name) => {
                match self.lookup.find_symbol(op_name, loc) {
                    Ok(res) => {
                        debug!(target: "linker::resolver", operator = %op_name, "Resolved operator identifier");
                        LinkedExpression::Identifier(res)
                    }
                    Err(e) => {
                        errors.push(e);
                        LinkedExpression::Identifier(ResolvedId::Local(Spanned::new(
                            op_name.clone(),
                            loc,
                        )))
                    }
                }
            }
        };

        Checked::with_errors(Spanned::new(linked, loc), errors)
    }

    pub fn resolve_attributes(
        &self,
        attrs: &[Spanned<ast::Attribute>],
    ) -> Vec<Spanned<LinkedAttribute>> {
        attrs
            .iter()
            .map(|attr| {
                Spanned::new(
                    LinkedAttribute {
                        name: attr.value.name.clone(),
                        args: vec![],
                    },
                    attr.loc,
                )
            })
            .collect()
    }
}
