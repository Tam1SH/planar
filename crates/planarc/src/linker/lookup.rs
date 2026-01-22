use crate::linker::error::{AmbiguousCandidate, LinkerError};
use crate::linker::ids::{ResolvedId, SymbolId};
use crate::linker::symbol_table::{SymbolMetadata, SymbolTable, Visibility};
use crate::source_registry::SourceRegistry;
use crate::spanned::{Location, Spanned};
use miette::NamedSource;
use tracing::{debug, instrument, trace, warn};

struct LookupCandidate {
    fqmn: String,
    id: SymbolId,
    loc: Location,
    strategy: &'static str,
}

pub struct SymbolLookup<'a> {
    pub table: &'a SymbolTable,
    pub registry: &'a SourceRegistry,
    pub current_package: String,
    pub current_module: String,
    pub imports: Vec<String>,
    pub prelude: Vec<String>,
    pub current_node_id: Option<SymbolId>,
}

impl<'a> SymbolLookup<'a> {
    #[instrument(skip(self, loc), fields(symbol = name, current_module = %self.current_module))]
    pub fn find_symbol(&self, name: &str, loc: Location) -> Result<ResolvedId, LinkerError> {
        trace!(target: "linker::lookup", "Starting symbol resolution");

        if let Some(resolved) = self.lookup_scoped(name, loc) {
            trace!(target: "linker::lookup", "Resolved via node scope");
            return Ok(resolved);
        }

        if let Some(meta) = self.table.resolve_metadata(name) {
            if let Err(e) = self.check_access_metadata(meta, name, loc) {
                debug!(target: "linker::lookup", "Absolute match found but access denied: {}", name);
                return Err(e);
            }
            self.trace_success("absolute_fqmn", name, meta);
            return Ok(ResolvedId::Global(Spanned::new(meta.id, meta.location)));
        }

        let candidates = self.collect_search_candidates(name, loc);

        if candidates.is_empty() {
            if let Some(fallback) = self.lookup_fallbacks(name, loc) {
                return Ok(fallback);
            }
            debug!(target: "linker::lookup", "No candidates found for {}", name);
            return Err(self.error_unknown(name, loc));
        }

        self.select_best_candidate(name, loc, candidates)
    }

    pub fn check_access(&self, fqmn: &str, loc: Location) -> Result<&SymbolMetadata, LinkerError> {
        let meta = self
            .table
            .resolve_metadata(fqmn)
            .ok_or_else(|| self.error_unknown(fqmn, loc))?;
        self.check_access_metadata(meta, fqmn, loc)?;
        Ok(meta)
    }

    fn check_access_metadata(
        &self,
        meta: &SymbolMetadata,
        name: &str,
        loc: Location,
    ) -> Result<(), LinkerError> {
        let allowed = match meta.visibility {
            Visibility::Public => true,
            Visibility::Package => meta.package == self.current_package,
            Visibility::ModulePrivate => meta.module == self.current_module,
            Visibility::Scoped(owner_id) => Some(owner_id) == self.current_node_id,
        };

        if allowed {
            trace!(target: "linker::lookup", visibility = ?meta.visibility, "Access granted");
            Ok(())
        } else {
            warn!(target: "linker::lookup", visibility = ?meta.visibility, "Access denied to {}", name);
            Err(self.error_access_denied(name, loc, meta))
        }
    }

    fn lookup_scoped(&self, name: &str, loc: Location) -> Option<ResolvedId> {
        let node_id = self.current_node_id?;
        let node_fqmn = self.table.get_fqmn(node_id)?;
        let nested_fqmn = format!("{}.{}", node_fqmn, name);

        let meta = self.table.resolve_metadata(&nested_fqmn)?;
        if self.check_access_metadata(meta, &nested_fqmn, loc).is_ok() {
            return Some(ResolvedId::Global(Spanned::new(meta.id, meta.location)));
        }
        None
    }

    fn collect_search_candidates(&self, name: &str, loc: Location) -> Vec<LookupCandidate> {
        let mut candidates = Vec::new();

        for import_path in &self.imports {
            if let Some(last_segment) = import_path.split('.').next_back() {
                let prefix = format!("{}.", last_segment);
                if name.starts_with(&prefix) {
                    let suffix = &name[prefix.len()..];
                    let fqmn = format!("{}.{}", import_path, suffix);
                    self.add_candidate(&mut candidates, &fqmn, loc, "import_suffix");
                } else if name == last_segment {
                    self.add_candidate(&mut candidates, import_path, loc, "import_direct");
                }
            }
            let implicit_fqmn = format!("{}.{}", import_path, name);
            self.add_candidate(&mut candidates, &implicit_fqmn, loc, "import_implicit");
        }

        let current_fqmn = format!("{}.{}", self.current_module, name);
        self.add_candidate(&mut candidates, &current_fqmn, loc, "current_module");

        if let Some((parent_pkg, _)) = self.current_module.rsplit_once('.') {
            let sibling_fqmn = format!("{}.{}", parent_pkg, name);
            self.add_candidate(&mut candidates, &sibling_fqmn, loc, "sibling");
        }

        candidates
    }

    fn select_best_candidate(
        &self,
        name: &str,
        loc: Location,
        mut candidates: Vec<LookupCandidate>,
    ) -> Result<ResolvedId, LinkerError> {
        candidates.sort_by(|a, b| a.fqmn.cmp(&b.fqmn));
        candidates.dedup_by(|a, b| a.fqmn == b.fqmn);

        if candidates.len() == 1 {
            let c = candidates.remove(0);
            return Ok(ResolvedId::Global(Spanned::new(c.id, c.loc)));
        }

        warn!(target: "linker::lookup", "Ambiguity detected for {}: {} candidates", name, candidates.len());

        let error_candidates = candidates
            .into_iter()
            .map(|c| {
                let (c_src, c_span) = self.registry.get_source_and_span(c.loc);
                AmbiguousCandidate {
                    module_name: c.fqmn,
                    src: NamedSource::new(c_src.name().to_string(), c_src.inner().to_string()),
                    span: c_span,
                    loc: c.loc,
                }
            })
            .collect();

        let (src, span) = self.registry.get_source_and_span(loc);
        Err(LinkerError::AmbiguousReference {
            name: name.to_string(),
            src: NamedSource::new(src.name().to_string(), src.inner().to_string()),
            span,
            candidates: error_candidates,
            loc,
        })
    }

    fn lookup_fallbacks(&self, name: &str, loc: Location) -> Option<ResolvedId> {
        for prelude_pkg in &self.prelude {
            let fqmn = format!("{}.{}", prelude_pkg, name);
            if let Ok(meta) = self.check_access(&fqmn, loc) {
                return Some(ResolvedId::Global(Spanned::new(meta.id, meta.location)));
            }
        }
        let builtin_fqmn = format!("builtin.{}", name);
        if let Ok(meta) = self.check_access(&builtin_fqmn, loc) {
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
    ) {
        if let Ok(meta) = self.check_access(fqmn, loc) {
            trace!(target: "linker::lookup", %fqmn, %strategy, "Candidate added");
            list.push(LookupCandidate {
                fqmn: fqmn.to_string(),
                id: meta.id,
                loc: meta.location,
                strategy,
            });
        }
    }

    fn error_unknown(&self, name: &str, loc: Location) -> LinkerError {
        let (src, span) = self.registry.get_source_and_span(loc);
        LinkerError::UnknownSymbol {
            name: name.to_string(),
            src: NamedSource::new(src.name().to_string(), src.inner().to_string()),
            span,
            loc,
        }
    }

    fn error_access_denied(&self, name: &str, loc: Location, meta: &SymbolMetadata) -> LinkerError {
        let (src, span) = self.registry.get_source_and_span(loc);
        let reason = match meta.visibility {
            Visibility::Package => format!("internal to package '{}'", meta.package),
            Visibility::ModulePrivate => format!("private to module '{}'", meta.module),
            Visibility::Scoped(_) => "private to its parent node".to_string(),
            _ => "restricted".to_string(),
        };
        LinkerError::AccessViolation {
            name: name.to_string(),
            reason,
            src: NamedSource::new(src.name().to_string(), src.inner().to_string()),
            span,
            loc,
        }
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
