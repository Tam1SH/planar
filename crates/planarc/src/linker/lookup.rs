use tracing::{debug, instrument, trace, warn};

use crate::checked::Checked;
use crate::linker::error::{AmbiguousCandidate, LinkerError, LinkerErrors};
use crate::linker::meta::{ResolvedId, SymbolId, SymbolMetadata, Visibility};
use crate::linker::symbol_table::SymbolTable;
use crate::source_registry::SourceRegistry;
use crate::spanned::{Location, Spanned, ToSpanned};

struct LookupCandidate {
    fqmn: String,
    id: SymbolId,
    loc: Location,
}
#[derive(Clone)]
pub struct SymbolLookup<'a> {
    pub table: &'a SymbolTable,
    pub registry: &'a SourceRegistry,
    pub current_package: String,
    pub current_module: String,
    pub imports: Vec<String>,
    pub prelude: &'a Vec<String>,
}

impl<'a> SymbolLookup<'a> {
    #[instrument(skip(self, loc), fields(symbol = name, current_module = %self.current_module))]
    pub fn find_symbol(&self, name: &str, loc: Location) -> Result<ResolvedId, Box<LinkerError>> {
        self.find_symbol_internal(name, loc, None)
    }

    pub fn find_symbol_with_ctx(
        &self,
        name: &str,
        loc: Location,
        owner_id: SymbolId,
    ) -> Result<ResolvedId, Box<LinkerError>> {
        self.find_symbol_internal(name, loc, Some(owner_id))
    }

    #[instrument(skip_all)]
    fn find_symbol_internal(
        &self,
        name: &str,
        loc: Location,
        node_ctx: Option<SymbolId>,
    ) -> Result<ResolvedId, Box<LinkerError>> {
        trace!(target: "linker::lookup", "Starting symbol resolution");

        let to_global = |meta: &SymbolMetadata| ResolvedId::Global(meta.id.spanned(meta.location));

        if let Some(meta) = self.table.resolve_metadata(name) {
            if let Err(e) = self.check_access_metadata(meta, name, loc, node_ctx) {
                debug!(target: "linker::lookup", "Absolute match found but access denied: {}", name);
                return Err(e);
            }
            self.trace_success("absolute_fqmn", name, meta);
            return Ok(to_global(meta));
        }

        if let Some(owner_id) = node_ctx
            && let Some(node_fqmn) = self.table.get_fqmn(owner_id)
        {
            let node_local_fqmn = format!("{}.{}", node_fqmn, name);
            if let Ok(meta) = self.check_access_internal(&node_local_fqmn, loc, node_ctx) {
                self.trace_success("node_local", &node_local_fqmn, meta);
                return Ok(to_global(meta));
            }
        }

        let current_fqmn = format!("{}.{}", self.current_module, name);
        if let Ok(meta) = self.check_access_internal(&current_fqmn, loc, node_ctx) {
            self.trace_success("current_module", &current_fqmn, meta);
            return Ok(to_global(meta));
        }

        let candidates = self.collect_search_candidates(name, loc, node_ctx);

        if candidates.is_empty() {
            if let Some(fallback) = self.lookup_fallbacks(name, loc, node_ctx) {
                return Ok(fallback);
            }
            debug!(target: "linker::lookup", "No candidates found for {}", name);
            return Err(self.error_unknown(name, loc, None));
        }

        self.select_best_candidate(name, loc, candidates)
    }

    pub fn find_in_scope(&self, name: &str, scope_id: SymbolId) -> Option<ResolvedId> {
        let node_fqmn = self.table.get_fqmn(scope_id)?;
        let nested_fqmn = format!("{}.{}", node_fqmn, name);

        let meta = self.table.resolve_metadata(&nested_fqmn)?;

        if self
            .check_access_metadata(meta, &nested_fqmn, Location::default(), Some(scope_id))
            .is_ok()
        {
            return Some(ResolvedId::Global(Spanned::new(meta.id, meta.location)));
        }
        None
    }

    pub fn resolve_id(&self, fqmn: &str, loc: Location) -> Checked<SymbolId, LinkerErrors> {
        match self.table.name_to_id.get(fqmn) {
            Some(&id) => Checked::new(id),
            None => {
                let short_name = fqmn.rsplit_once('.').map(|(_, s)| s).unwrap_or(fqmn);
                let error = self.error_unknown(short_name, loc, None);

                Checked::with_errors(SymbolId::INVALID_ID, LinkerErrors::new(vec![error]))
            }
        }
    }

    pub fn check_access(
        &self,
        fqmn: &str,
        loc: Location,
    ) -> Result<&SymbolMetadata, Box<LinkerError>> {
        self.check_access_internal(fqmn, loc, None)
    }

    fn check_access_internal(
        &self,
        fqmn: &str,
        loc: Location,
        ctx: Option<SymbolId>,
    ) -> Result<&SymbolMetadata, Box<LinkerError>> {
        let meta = self
            .table
            .resolve_metadata(fqmn)
            .ok_or_else(|| self.error_unknown(fqmn, loc, None))?;
        self.check_access_metadata(meta, fqmn, loc, ctx)?;
        Ok(meta)
    }

    fn check_access_metadata(
        &self,
        meta: &SymbolMetadata,
        name: &str,
        loc: Location,
        current_node_id: Option<SymbolId>,
    ) -> Result<(), Box<LinkerError>> {
        let allowed = match meta.visibility {
            Visibility::Public => true,
            Visibility::Package => meta.package == self.current_package,
            Visibility::ModulePrivate => meta.module == self.current_module,
            Visibility::Scoped(owner_id) => Some(owner_id) == current_node_id,
        };

        if allowed {
            trace!(target: "linker::lookup", visibility = ?meta.visibility, "Access granted");
            Ok(())
        } else {
            warn!(target: "linker::lookup", visibility = ?meta.visibility, "Access denied to {}", name);
            Err(self.error_access_denied(name, loc, meta))
        }
    }

    fn collect_search_candidates(
        &self,
        name: &str,
        loc: Location,
        node_ctx: Option<SymbolId>,
    ) -> Vec<LookupCandidate> {
        let mut candidates = Vec::new();

        for import_path in &self.imports {
            if let Some(last_segment) = import_path.split('.').next_back() {
                let prefix = format!("{}.", last_segment);
                if name.starts_with(&prefix) {
                    let suffix = &name[prefix.len()..];
                    let fqmn = format!("{}.{}", import_path, suffix);
                    self.add_candidate(&mut candidates, &fqmn, loc, "import_suffix", node_ctx);
                } else if name == last_segment {
                    self.add_candidate(
                        &mut candidates,
                        import_path,
                        loc,
                        "import_direct",
                        node_ctx,
                    );
                }
            }
            let implicit_fqmn = format!("{}.{}", import_path, name);
            self.add_candidate(
                &mut candidates,
                &implicit_fqmn,
                loc,
                "import_implicit",
                node_ctx,
            );
        }

        if let Some((parent_pkg, _)) = self.current_module.rsplit_once('.') {
            let sibling_fqmn = format!("{}.{}", parent_pkg, name);
            if sibling_fqmn != self.current_module {
                self.add_candidate(&mut candidates, &sibling_fqmn, loc, "sibling", node_ctx);
            }
        }

        candidates
    }

    fn select_best_candidate(
        &self,
        name: &str,
        loc: Location,
        mut candidates: Vec<LookupCandidate>,
    ) -> Result<ResolvedId, Box<LinkerError>> {
        candidates.sort_by(|a, b| a.fqmn.cmp(&b.fqmn));
        candidates.dedup_by(|a, b| a.fqmn == b.fqmn);

        if candidates.len() == 1 {
            let c = candidates.remove(0);
            return Ok(ResolvedId::Global(Spanned::new(c.id, c.loc)));
        }

        if candidates.is_empty() {
            return Err(self.error_unknown(name, loc, None));
        }

        let error_candidates = candidates
            .into_iter()
            .map(|c| {
                let (src, c_span) = self.registry.get_source_and_span(c.loc);
                AmbiguousCandidate {
                    module_name: c.fqmn,
                    src,
                    span: c_span,
                    loc: c.loc,
                }
            })
            .collect();

        let (src, span) = self.registry.get_source_and_span(loc);

        Err(Box::new(LinkerError::AmbiguousReference {
            name: name.to_string(),
            src,
            span,
            candidates: error_candidates,
            loc,
        }))
    }

    fn lookup_fallbacks(
        &self,
        name: &str,
        loc: Location,
        node_ctx: Option<SymbolId>,
    ) -> Option<ResolvedId> {
        for prelude_pkg in self.prelude {
            let fqmn = format!("{}.{}", prelude_pkg, name);
            if self.check_access_internal(&fqmn, loc, node_ctx).is_ok()
                && let Some(meta) = self.table.resolve_metadata(&fqmn)
            {
                return Some(ResolvedId::Global(Spanned::new(meta.id, meta.location)));
            }
        }
        let builtin_fqmn = format!("builtin.{}", name);
        if self
            .check_access_internal(&builtin_fqmn, loc, node_ctx)
            .is_ok()
            && let Some(meta) = self.table.resolve_metadata(&builtin_fqmn)
        {
            return Some(ResolvedId::Global(Spanned::new(meta.id, meta.location)));
        }
        None
    }

    fn add_candidate(
        &self,
        list: &mut Vec<LookupCandidate>,
        fqmn: &str,
        loc: Location,
        strategy: &'static str,
        node_ctx: Option<SymbolId>,
    ) {
        if let Ok(meta) = self.check_access_internal(fqmn, loc, node_ctx) {
            trace!(target: "linker::lookup", %fqmn, %strategy, "Candidate added");
            list.push(LookupCandidate {
                fqmn: fqmn.to_string(),
                id: meta.id,
                loc: meta.location,
            });
        }
    }

    pub fn error_unknown_capture(&self, name: &str, loc: Location) -> Box<LinkerError> {
        let (src, span) = self.registry.get_source_and_span(loc);

        Box::new(LinkerError::UndefinedCapture {
            capture_name: name.to_string(),
            src,
            span,
            loc,
        })
    }

    pub fn error_unknown(
        &self,
        name: &str,
        loc: Location,
        help: Option<String>,
    ) -> Box<LinkerError> {
        let (src, span) = self.registry.get_source_and_span(loc);

        let help = help.or_else(|| {
            self.find_suggestion(name)
                .map(|s| format!("Did you mean '{}'?", s))
        });

        Box::new(LinkerError::UnknownSymbol {
            name: name.to_string(),
            src,
            span,
            loc,
            help,
        })
    }

    /// - current="app.main", fqmn="app.main.User" -> "User"
    /// - import="std.math",  fqmn="std.math.PI"   -> "math.PI"
    /// - no imports,         fqmn="other.Data"    -> "other.Data"
    fn relativize_fqmn(&self, fqmn: &str) -> String {
        let local_prefix = format!("{}.", self.current_module);
        if let Some(local_name) = fqmn.strip_prefix(&local_prefix) {
            return local_name.to_string();
        }

        for imp in &self.imports {
            let import_fqmn = &imp;

            if fqmn.starts_with(*import_fqmn)
                && let Some(remainder) = fqmn.strip_prefix(*import_fqmn)
            {
                if remainder.starts_with('.') {
                    let alias = import_fqmn.split('.').next_back().unwrap_or(import_fqmn);
                    return format!("{}{}", alias, remainder);
                } else if remainder.is_empty() {
                    let alias = import_fqmn.split('.').next_back().unwrap_or(import_fqmn);
                    return alias.to_string();
                }
            }
        }

        fqmn.to_string()
    }

    fn find_suggestion(&self, typo: &str) -> Option<String> {
        let mut best_match = None;
        let mut min_distance = usize::MAX;

        let threshold = std::cmp::max(typo.chars().count() / 3, 1);

        for fqmn in self.table.name_to_id.keys() {
            let visible_name = self.relativize_fqmn(fqmn);

            let dist = strsim::damerau_levenshtein(typo, &visible_name);

            if dist <= threshold && dist < min_distance {
                min_distance = dist;
                best_match = Some(visible_name);
            }
        }

        best_match
    }

    fn error_access_denied(
        &self,
        name: &str,
        loc: Location,
        meta: &SymbolMetadata,
    ) -> Box<LinkerError> {
        let (src, span) = self.registry.get_source_and_span(loc);
        let reason = match meta.visibility {
            Visibility::Package => format!("internal to package '{}'", meta.package),
            Visibility::ModulePrivate => format!("private to module '{}'", meta.module),
            Visibility::Scoped(_) => "private to its parent node".to_string(),
            _ => "restricted".to_string(),
        };
        Box::new(LinkerError::AccessViolation {
            name: name.to_string(),
            reason,
            src,
            span,
            loc,
        })
    }

    fn trace_success(&self, strategy: &str, fqmn: &str, meta: &SymbolMetadata) {
        debug!(
            target: "linker::lookup",
            strategy = strategy,
            fqmn = fqmn,
            id = %meta.id,
            origin = %meta.module,
            "Symbol resolved"
        );
    }
}
