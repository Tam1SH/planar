use tracing::{debug, error, instrument, trace};

use crate::ast;
use crate::linker::error::LinkerError;
use crate::linker::ids::ResolvedId;
use crate::linker::linked_ast::*;
use crate::linker::lookup::SymbolLookup;
use crate::spanned::{Location, Spanned};

pub struct AstLinker<'a> {
    pub lookup: SymbolLookup<'a>,
    pub errors: Vec<LinkerError>,
}

impl<'a> AstLinker<'a> {
    pub fn new(lookup: SymbolLookup<'a>) -> Self {
        Self {
            lookup,
            errors: Vec::new(),
        }
    }

    pub fn link_vec<T, U>(
        &mut self,
        items: &[Spanned<T>],
        f: impl Fn(&mut Self, &T, Location) -> Option<Spanned<U>>,
    ) -> Vec<Spanned<U>> {
        items
            .iter()
            .filter_map(|item| f(self, &item.value, item.loc))
            .collect()
    }

    #[instrument(skip(self, loc, q), fields(query = %q.name.value))]
    pub fn resolve_query(
        &mut self,
        q: &ast::QueryDefinition,
        loc: Location,
    ) -> Option<Spanned<LinkedQuery>> {
        let fqmn = format!("{}.{}", self.lookup.current_module, q.name.value);
        let meta = self.lookup.table.resolve_metadata(&fqmn).or_else(|| {
            error!(target: "linker::resolver", "Query metadata missing: {}", fqmn);
            None
        })?;

        Some(Spanned::new(
            LinkedQuery {
                id: meta.id,
                name: q.name.value.clone(),
                grammar: q.grammar.value.clone(),
                query: q.value.value.clone(),
            },
            loc,
        ))
    }

    #[instrument(skip(self, loc, ext))]
    pub fn resolve_extern_definition(
        &mut self,
        ext: &ast::ExternDefinition,
        loc: Location,
    ) -> Option<Spanned<LinkedExternDefinition>> {
        let is_builtin = ext
            .attributes
            .iter()
            .any(|a| a.value.name.value == "builtin");
        let prefix = if is_builtin {
            "builtin".to_string()
        } else {
            self.lookup.current_module.clone()
        };

        let mut functions = Vec::new();

        for f in &ext.functions {
            let fqmn = format!("{}.{}", prefix, f.value.name.value);
            let meta = match self.lookup.table.resolve_metadata(&fqmn) {
                Some(m) => m,
                None => continue,
            };

            let mut args = Vec::new();
            for a in &f.value.args {
                if let Some(ty) = self.resolve_type_ref(&a.value.ty.value) {
                    args.push(Spanned::new(
                        LinkedExternArgument {
                            name: a.value.name.value.clone(),
                            ty,
                        },
                        a.loc,
                    ));
                }
            }

            let return_ty = f
                .value
                .return_type
                .as_ref()
                .and_then(|r| self.resolve_type_ref(&r.value));

            functions.push(Spanned::new(
                LinkedExternFunction {
                    id: meta.id,
                    name: f.value.name.value.clone(),
                    args,
                    return_ty,
                },
                f.loc,
            ));
        }

        Some(Spanned::new(LinkedExternDefinition { functions }, loc))
    }

    #[instrument(skip(self, loc, ty), fields(type = %ty.name.value))]
    pub fn resolve_type_decl(
        &mut self,
        ty: &ast::TypeDeclaration,
        loc: Location,
    ) -> Option<Spanned<LinkedType>> {
        let fqmn = format!("{}.{}", self.lookup.current_module, ty.name.value);
        let meta = self.lookup.table.resolve_metadata(&fqmn).or_else(|| {
            error!(target: "linker::resolver", "Type symbol not found: {}", fqmn);
            None
        })?;

        let definition = self.resolve_type_def(&ty.definition.value, ty.definition.loc)?;

        Some(Spanned::new(
            LinkedType {
                id: meta.id,
                name: ty.name.value.clone(),
                attributes: self.resolve_attributes(ty.attributes.as_slice()),
                definition,
            },
            loc,
        ))
    }

    #[instrument(skip(self, loc, fact), fields(fact = %fact.name.value))]
    pub fn resolve_fact(
        &mut self,
        fact: &ast::FactDefinition,
        loc: Location,
    ) -> Option<Spanned<LinkedFact>> {
        let fqmn = format!("{}.{}", self.lookup.current_module, fact.name.value);
        let meta = self.lookup.table.resolve_metadata(&fqmn).or_else(|| {
            error!(target: "linker::resolver", "Fact symbol not found: {}", fqmn);
            None
        })?;

        let fields = fact
            .fields
            .iter()
            .filter_map(|f| {
                let linked_ty = self.resolve_type_ref(&f.value.ty)?;
                Some(Spanned::new(
                    LinkedField {
                        attributes: self.resolve_attributes(&f.value.attributes),
                        name: f.value.name.value.clone(),
                        ty: linked_ty,
                    },
                    f.loc,
                ))
            })
            .collect();

        Some(Spanned::new(
            LinkedFact {
                id: meta.id,
                attributes: self.resolve_attributes(&fact.attributes),
                name: fact.name.value.clone(),
                fields,
            },
            loc,
        ))
    }

    #[instrument(skip(self, loc), fields(node_kind = %node.kind.value))]
    pub fn resolve_node(
        &mut self,
        node: &ast::NodeDefinition,
        loc: Location,
    ) -> Option<Spanned<LinkedNode>> {
        trace!(target: "linker::resolver", "Resolving node definition");

        let fqmn = format!("{}.{}", self.lookup.current_module, node.kind.value);

        let meta = self.lookup.table.resolve_metadata(&fqmn).or_else(|| {
            error!(target: "linker::resolver", "Node symbol not found in table: {}", fqmn);
            None
        })?;

        let old_scope = self.lookup.current_node_id;
        self.lookup.current_node_id = Some(meta.id);
        debug!(target: "linker::resolver", node_id = %meta.id, "Entered node scope");

        let statements = node
            .statements
            .iter()
            .filter_map(|stmt| match &stmt.value {
                ast::NodeStatement::Match(m) => self
                    .resolve_match(&m.value)
                    .map(|l| Spanned::new(LinkedNodeStatement::Match(l), stmt.loc)),
                ast::NodeStatement::Query(q) => self
                    .resolve_nested_query(&q.value, q.loc)
                    .map(|l| Spanned::new(LinkedNodeStatement::Query(l.value), stmt.loc)),
            })
            .collect();

        self.lookup.current_node_id = old_scope;
        trace!(target: "linker::resolver", "Exited node scope");

        Some(Spanned::new(
            LinkedNode {
                id: meta.id,
                kind: node.kind.value.clone(),
                statements,
            },
            loc,
        ))
    }

    #[instrument(skip(self, loc, q), fields(query = %q.name.value))]
    fn resolve_nested_query(
        &mut self,
        q: &ast::QueryDefinition,
        loc: Location,
    ) -> Option<Spanned<LinkedQuery>> {
        let node_id = self.lookup.current_node_id?;
        let node_fqmn = self.lookup.table.get_fqmn(node_id)?;
        let fqmn = format!("{}.{}", node_fqmn, q.name.value);

        let meta = self.lookup.table.resolve_metadata(&fqmn).or_else(|| {
            error!(target: "linker::resolver", "Nested query symbol not found: {}", fqmn);
            None
        })?;

        debug!(target: "linker::resolver", query_id = %meta.id, "Resolved nested query");

        Some(Spanned::new(
            LinkedQuery {
                id: meta.id,
                name: q.name.value.clone(),
                grammar: q.grammar.value.clone(),
                query: q.value.value.clone(),
            },
            loc,
        ))
    }

    #[instrument(skip(self, m))]
    pub fn resolve_match(&mut self, m: &ast::MatchStatement) -> Option<LinkedMatchStatement> {
        let query_ref = match &m.query_ref.value {
            ast::MatchQueryReference::Identifier(name) => {
                trace!(target: "linker::resolver", query_name = %name, "Resolving match by identifier");
                match self.lookup.find_symbol(name, m.query_ref.loc) {
                    Ok(res) => {
                        debug!(target: "linker::resolver", query_id = %res.symbol_id(), "Match identifier resolved");
                        Spanned::new(
                            LinkedMatchQueryReference::Global(res.symbol_id()),
                            m.query_ref.loc,
                        )
                    }
                    Err(e) => {
                        self.errors.push(e);
                        return None;
                    }
                }
            }
            ast::MatchQueryReference::Raw(raw) => {
                trace!(target: "linker::resolver", "Match using raw query string");
                Spanned::new(LinkedMatchQueryReference::Raw(raw.clone()), m.query_ref.loc)
            }
        };
        Some(LinkedMatchStatement { query_ref })
    }

    pub fn resolve_type_ref(&mut self, ty: &ast::TypeAnnotation) -> Option<LinkedTypeReference> {
        let symbol = match self.lookup.find_symbol(&ty.name.value, ty.name.loc) {
            Ok(res) => Some(res),
            Err(e) => {
                self.errors.push(e);
                None
            }
        }?;

        let args = ty
            .args
            .iter()
            .filter_map(|arg| Some(Spanned::new(self.resolve_type_ref(&arg.value)?, arg.loc)))
            .collect();

        let refinement = ty
            .refinement
            .as_ref()
            .and_then(|expr| self.resolve_expr(&expr.value, expr.loc));

        Some(LinkedTypeReference {
            symbol: Spanned::new(symbol, ty.name.loc),
            args,
            refinement,
        })
    }

    pub fn resolve_type_def(
        &mut self,
        def: &ast::TypeDefinition,
        loc: Location,
    ) -> Option<Spanned<LinkedTypeDefinition>> {
        let base_type = def
            .base_type
            .as_ref()
            .and_then(|base| self.resolve_type_ref(base));

        let fields = def
            .fields
            .iter()
            .filter_map(|field| {
                let f = &field.value;
                let field_def = self.resolve_type_def(&f.definition.value, field.loc)?;

                Some(Spanned::new(
                    LinkedTypeField {
                        name: f.name.value.clone(),
                        definition: field_def.value,
                    },
                    field.loc,
                ))
            })
            .collect();

        Some(Spanned::new(
            LinkedTypeDefinition { base_type, fields },
            loc,
        ))
    }

    #[instrument(skip(self, loc, expr))]
    pub fn resolve_expr(
        &mut self,
        expr: &ast::Expression,
        loc: Location,
    ) -> Option<Spanned<LinkedExpression>> {
        let linked = match expr {
            ast::Expression::Identifier(name) => {
                if name == "it" {
                    trace!(target: "linker::resolver", "Resolved 'it' context variable");
                    Some(LinkedExpression::Identifier(ResolvedId::Local(
                        Spanned::new("it".to_string(), loc),
                    )))
                } else {
                    match self.lookup.find_symbol(name, loc) {
                        Ok(res) => {
                            debug!(target: "linker::resolver", symbol = %name, "Resolved identifier");
                            Some(LinkedExpression::Identifier(res))
                        }
                        Err(e) => {
                            self.errors.push(e);
                            None
                        }
                    }
                }
            }

            ast::Expression::Number(n) => Some(LinkedExpression::Number(n.clone())),
            ast::Expression::StringLit(s) => Some(LinkedExpression::StringLit(s.clone())),

            ast::Expression::Binary { left, op, right } => {
                trace!(target: "linker::resolver", op = %op.value, "Resolving binary expression");
                let linked_left = self.resolve_expr(&left.value, left.loc)?;
                let linked_right = self.resolve_expr(&right.value, right.loc)?;

                let operator_res = match self.lookup.find_symbol(&op.value, op.loc) {
                    Ok(res) => Spanned::new(res, op.loc),
                    Err(e) => {
                        self.errors.push(e);
                        return None;
                    }
                };

                Some(LinkedExpression::Binary {
                    left: Box::new(linked_left),
                    operator: operator_res,
                    right: Box::new(linked_right),
                })
            }

            ast::Expression::Call { function, args } => {
                trace!(target: "linker::resolver", "Resolving call expression");
                let linked_function = self.resolve_expr(&function.value, function.loc)?;
                let mut linked_args = Vec::with_capacity(args.len());

                for arg in args {
                    linked_args.push(self.resolve_expr(&arg.value, arg.loc)?);
                }

                Some(LinkedExpression::Call {
                    function: Box::new(linked_function),
                    args: linked_args,
                })
            }

            ast::Expression::InList(items) => {
                trace!(target: "linker::resolver", "Resolving InList expression");
                let mut linked_items = Vec::with_capacity(items.len());
                for item in items {
                    linked_items.push(self.resolve_expr(&item.value, item.loc)?);
                }
                Some(LinkedExpression::InList(linked_items))
            }

            ast::Expression::InRange { start, end } => {
                trace!(target: "linker::resolver", "Resolving InRange expression");
                let linked_start = self.resolve_expr(&start.value, start.loc)?;
                let linked_end = if let Some(e) = end {
                    Some(Box::new(self.resolve_expr(&e.value, e.loc)?))
                } else {
                    None
                };

                Some(LinkedExpression::InRange {
                    start: Box::new(linked_start),
                    end: linked_end,
                })
            }

            ast::Expression::OperatorIdentifier(op_name) => {
                match self.lookup.find_symbol(op_name, loc) {
                    Ok(res) => {
                        debug!(target: "linker::resolver", operator = %op_name, "Resolved operator identifier");
                        Some(LinkedExpression::Identifier(res))
                    }
                    Err(e) => {
                        self.errors.push(e);
                        None
                    }
                }
            }
        }?;

        Some(Spanned::new(linked, loc))
    }

    pub fn resolve_attributes(
        &mut self,
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
