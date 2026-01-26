use tracing::{debug, instrument, trace, trace_span, warn};

use super::lookup::SymbolLookup;
use crate::ast;
use crate::checked::{Checked, CheckedIteratorExt};
use crate::linker::error::LinkerErrors;
use crate::linker::linked_ast::*;
use crate::linker::meta::{ResolvedId, SymbolId, SymbolKind};
use crate::scope::ScopeStack;
use crate::spanned::{Location, Spanned};

pub struct NodeLinker<'a> {
    pub lookup: &'a SymbolLookup<'a>,
    pub node_id: SymbolId,
    pub node_fqmn: String,
}

impl<'a> NodeLinker<'a> {
    pub fn new(lookup: &'a SymbolLookup<'a>, node_id: SymbolId, node_fqmn: String) -> Self {
        Self {
            lookup,
            node_id,
            node_fqmn,
        }
    }

    #[instrument(skip(self, stmts), fields(count = stmts.len()))]
    pub fn resolve_statements(
        &self,
        stmts: &[Spanned<ast::NodeStatement>],
    ) -> Checked<Vec<LinkedNodeStatement>, LinkerErrors> {
        debug!("Processing node statements");
        stmts
            .iter()
            .enumerate()
            .map(|(i, stmt)| {
                let _span = trace_span!("statement", idx = i).entered();
                match &stmt.value {
                    ast::NodeStatement::Match(m) => MatchResolver::new(self)
                        .resolve(m)
                        .map(LinkedNodeStatement::Match),
                    ast::NodeStatement::Query(q) => {
                        self.resolve_nested_query(q).map(LinkedNodeStatement::Query)
                    }
                }
            })
            .collect_checked()
    }

    #[instrument(skip(self, q), fields(query_name = %q.value.name.value))]
    fn resolve_nested_query(
        &self,
        q: &Spanned<ast::QueryDefinition>,
    ) -> Checked<Spanned<LinkedQuery>, LinkerErrors> {
        let loc = q.loc;
        let q = &q.value;

        let fqmn = format!("{}.{}", self.node_fqmn, q.name.value);
        
        trace!(fqmn = %fqmn, "Resolving nested query");
        
        let mut errors = LinkerErrors::default();

        let meta = self.lookup.table.resolve_metadata(&fqmn);

        if meta.is_none() {
            warn!(target = %fqmn, "Nested query symbol not found in table");
            errors.push(self.lookup.error_unknown(&q.name.value, q.name.loc, None));
        }

        Checked::with_errors(
            Spanned::new(
                LinkedQuery {
                    id: meta.map(|m| m.id).unwrap_or(SymbolId::INVALID_ID),
                    name: q.name.value.clone(),
                    query: q.value.value.clone(),
                },
                loc,
            ),
            errors,
        )
    }
}

struct MatchResolver<'a, 'b> {
    parent: &'b NodeLinker<'a>,
    scopes: ScopeStack<Location>,
    allowed_captures: Vec<String>,
}

impl<'a, 'b> MatchResolver<'a, 'b> {
    fn new(linker: &'b NodeLinker<'a>) -> Self {
        Self {
            parent: linker,
            scopes: ScopeStack::default(),
            allowed_captures: Vec::new(),
        }
    }

    #[instrument(skip_all)]
    fn resolve(
        &mut self,
        m: &Spanned<ast::MatchStatement>,
    ) -> Checked<Spanned<LinkedMatchStatement>, LinkerErrors> {
        debug!("Resolving match statement");
        let mut errors = LinkerErrors::default();

        self.scopes.push();
        self.allowed_captures = self.extract_captures(&m.value.query_ref, &mut errors);

        if !self.allowed_captures.is_empty() {
            trace!(captures = ?self.allowed_captures, "Extracted captures from query");
        }

        let query_ref = self.resolve_query_ref(&m.value.query_ref, &mut errors);

        let body = m
            .value
            .statements
            .iter()
            .map(|stmt| self.resolve_block_stmt(stmt))
            .collect_checked()
            .sink(&mut errors);

        self.scopes.pop();
        self.allowed_captures.clear();

        Checked::with_errors(
            Spanned::new(LinkedMatchStatement { query_ref, body }, m.loc),
            errors,
        )
    }

    fn extract_captures(
        &self,
        query_ref: &Spanned<ast::MatchQueryReference>,
        errors: &mut LinkerErrors,
    ) -> Vec<String> {
        match &query_ref.value {
            ast::MatchQueryReference::Identifier(name) => {
                match self.parent.lookup.find_symbol_with_ctx(name, query_ref.loc, self.parent.node_id) {
                    Ok(res) => {
                        let id = res.symbol_id();
                        if let Some(meta) = self.parent.lookup.table.get_metadata_by_id(id)
                            && let SymbolKind::Query { captures, .. } = &meta.kind
                        {
                            return captures.iter().map(|c| c.value.clone()).collect();
                        }
                        vec![]
                    }
                    Err(e) => {
                        errors.push(e);
                        vec![]
                    }
                }
            }
            ast::MatchQueryReference::Raw { captures, .. } => {
                captures.iter().map(|c| c.value.clone()).collect()
            }
        }
    }

    fn resolve_query_ref(
        &self,
        query_ref: &Spanned<ast::MatchQueryReference>,
        errors: &mut LinkerErrors,
    ) -> Spanned<LinkedMatchQueryReference> {
        match &query_ref.value {
            ast::MatchQueryReference::Identifier(name) => {
                match self.parent.lookup.find_symbol_with_ctx(name, query_ref.loc, self.parent.node_id) {
                    Ok(res) => Spanned::new(
                        LinkedMatchQueryReference::Named(res.symbol_id()),
                        query_ref.loc,
                    ),
                    Err(e) => {
                        errors.push(e);
                        Spanned::new(
                            LinkedMatchQueryReference::Named(SymbolId::INVALID_ID),
                            query_ref.loc,
                        )
                    }
                }
            }
            ast::MatchQueryReference::Raw { value, captures } => Spanned::new(
                LinkedMatchQueryReference::Raw { captures: captures.clone(), source: value.clone() },
                query_ref.loc,
            ),
        }
    }

    #[instrument(skip(self, stmt))]
    fn resolve_block_stmt(
        &mut self,
        stmt: &Spanned<ast::BlockStatement>,
    ) -> Checked<Spanned<LinkedMatchItem>, LinkerErrors> {
        let mut errors = LinkerErrors::default();
        let item = match &stmt.value {
            ast::BlockStatement::Let(l) => {
                debug!(var = %l.name.value, "Resolving let binding");

                let val = self
                    .resolve_local_expr(&l.value.value, l.value.loc)
                    .sink(&mut errors);

                self.scopes.define(l.name.value.clone(), l.name.loc);

                LinkedMatchItem::Let(LinkedLetBinding {
                    name: l.name.clone(),
                    value: val,
                })
            }
            ast::BlockStatement::Emit(e) => {
                let emit = self.resolve_emit(e).sink(&mut errors);
                LinkedMatchItem::Emit(emit)
            }
            ast::BlockStatement::Capture(c) => {
                let cap_name = &c.name.value;

                debug!(capture = %cap_name, "Entering capture block");

                if !self.allowed_captures.iter().any(|name| name == cap_name) {
                    errors.push(
                        self.parent
                            .lookup
                            .error_unknown_capture(cap_name, c.name.loc),
                    );
                }

                self.scopes.push();
                self.scopes.define(c.name.value.clone(), c.name.loc);

                let body = c
                    .statements
                    .iter()
                    .map(|s| self.resolve_block_stmt(s))
                    .collect_checked()
                    .sink(&mut errors);

                self.scopes.pop();

                LinkedMatchItem::Capture(LinkedCapture {
                    name: c.name.clone(),
                    body,
                })
            }
        };
        Checked::with_errors(Spanned::new(item, stmt.loc), errors)
    }

    #[instrument(skip(self, e))]
    fn resolve_emit(
        &mut self,
        e: &ast::EmitStatement,
    ) -> Checked<LinkedEmitStatement, LinkerErrors> {
        trace!("Resolving emit");

        let mut errors = LinkerErrors::default();

        let left = self.resolve_emitted_fact(&e.left.value).sink(&mut errors);

        let right = e
            .right
            .as_ref()
            .map(|r| self.resolve_emitted_fact(&r.value).sink(&mut errors));

        let relation = e.relation.as_ref().map(|rel| {
            match self
                .parent
                .lookup
                .find_symbol_with_ctx(&rel.value, rel.loc, self.parent.node_id)
            {
                Ok(res) => Spanned::new(res.symbol_id(), rel.loc),
                Err(err) => {
                    errors.push(err);
                    Spanned::new(SymbolId::INVALID_ID, rel.loc)
                }
            }
        });

        Checked::with_errors(
            LinkedEmitStatement {
                left,
                right,
                relation,
                direction: e.direction.map(|d| d.into()),
            },
            errors,
        )
    }

    #[instrument(skip(self, fact), fields(fact = %fact.type_name.value))]
    fn resolve_emitted_fact(
        &mut self,
        fact: &ast::EmittedFact,
    ) -> Checked<LinkedEmittedFact, LinkerErrors> {
        trace!("Resolving emitted fact type and fields");

        let mut errors = LinkerErrors::default();

        let fact_id = match self.parent.lookup.find_symbol_with_ctx(
            &fact.type_name.value,
            fact.type_name.loc,
            self.parent.node_id,
        ) {
            Ok(res) => res.symbol_id(),
            Err(e) => {
                errors.push(e);
                SymbolId::INVALID_ID
            }
        };

        let fields = fact
            .fields
            .iter()
            .map(|f| LinkedEmittedField {
                name: f.value.name.clone(),
                value: self
                    .resolve_local_expr(&f.value.value.value, f.value.value.loc)
                    .sink(&mut errors),
            })
            .collect();

        Checked::with_errors(LinkedEmittedFact { fact_id, fields }, errors)
    }

    #[instrument(skip(self, expr), level = "trace", fields(kind = ?expr))]
    fn resolve_local_expr(
        &self,
        expr: &ast::Expression,
        loc: Location,
    ) -> Checked<Spanned<LinkedExpression>, LinkerErrors> {
        let mut errors = LinkerErrors::default();

        let linked = match expr {
            ast::Expression::It => {
                errors.push(self.parent.lookup.error_unknown(
                    "it",
                    loc,
                    Some("The 'it' variable is not allowed in node statements (match/emit). It is only for type refinements.".to_string()),
                ));
                LinkedExpression::Identifier(ResolvedId::Local(Spanned::new("it".into(), loc)))
            }

            ast::Expression::Identifier(name) => {
                if let Some(def_loc) = self.scopes.lookup(name) {
                    LinkedExpression::Identifier(ResolvedId::Local(Spanned::new(
                        name.clone(),
                        *def_loc,
                    )))
                }
                else if name.starts_with('@') {
                    let cap = name;
                    if !self.allowed_captures.iter().any(|c| c == cap) {
                        errors.push(self.parent.lookup.error_unknown_capture(cap, loc));
                    }
                    LinkedExpression::Identifier(ResolvedId::Local(Spanned::new(name.clone(), loc)))
                }
                else {
                    match self
                        .parent
                        .lookup
                        .find_symbol_with_ctx(name, loc, self.parent.node_id)
                    {
                        Ok(res) => LinkedExpression::Identifier(res),
                        Err(e) => {
                            errors.push(e);
                            LinkedExpression::Identifier(ResolvedId::Local(Spanned::new(
                                name.clone(),
                                loc,
                            )))
                        }
                    }
                }
            }

            ast::Expression::Binary { left, op, right } => {
                let l = self
                    .resolve_local_expr(&left.value, left.loc)
                    .sink(&mut errors);
                let r = self
                    .resolve_local_expr(&right.value, right.loc)
                    .sink(&mut errors);

                let operator = match self.parent.lookup.find_symbol(&op.value, op.loc) {
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
                    left: Box::new(l),
                    operator,
                    right: Box::new(r),
                }
            }

            ast::Expression::Call { function, args } => {
                let func = self
                    .resolve_local_expr(&function.value, function.loc)
                    .sink(&mut errors);

                let linked_args = args
                    .iter()
                    .map(|a| self.resolve_local_expr(&a.value, a.loc).sink(&mut errors))
                    .collect();

                LinkedExpression::Call {
                    function: Box::new(func),
                    args: linked_args,
                }
            }

            ast::Expression::Number(n) => LinkedExpression::Number(n.clone()),

            ast::Expression::StringLit(s) => LinkedExpression::StringLit(s.clone()),

            ast::Expression::InList(items) => {
                let linked_items = items
                    .iter()
                    .map(|i| self.resolve_local_expr(&i.value, i.loc).sink(&mut errors))
                    .collect();
                LinkedExpression::InList(linked_items)
            }

            ast::Expression::InRange { start, end } => {
                let s = self
                    .resolve_local_expr(&start.value, start.loc)
                    .sink(&mut errors);

                let e = end.as_ref().map(|e_expr| {
                    Box::new(
                        self.resolve_local_expr(&e_expr.value, e_expr.loc)
                            .sink(&mut errors),
                    )
                });

                LinkedExpression::InRange {
                    start: Box::new(s),
                    end: e,
                }
            }

            ast::Expression::OperatorIdentifier(op_name) => {
                match self.parent.lookup.find_symbol(op_name, loc) {
                    Ok(res) => LinkedExpression::Identifier(res),
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
}
