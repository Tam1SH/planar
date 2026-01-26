use crate::Document;
use anyhow::{Context, Result};
use dashmap::DashMap;
use miette::NamedSource;
use planarc::module_loader::{DiscoveredModule, FsModuleLoader, ModuleLoader, PackageRoot, Source};
use planarc::source_registry::MietteSource;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tower_lsp::lsp_types::Url;

pub struct LspModuleLoader {
    pub documents: DashMap<String, Document>,
    inner: FsModuleLoader,
}

impl LspModuleLoader {
    pub fn new(documents: DashMap<String, Document>) -> Self {
        Self {
            documents,
            inner: FsModuleLoader,
        }
    }

    fn path_to_uri(&self, path: &Path) -> Option<String> {
        let absolute = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
        Url::from_file_path(absolute).ok().map(|u| u.to_string())
    }
}

impl ModuleLoader for LspModuleLoader {
    fn scan(&self, root: &PackageRoot) -> Result<Vec<DiscoveredModule>> {
        let modules = self.inner.scan(root)?;
        Ok(modules)
    }

    fn load(&self, path: &Path) -> Result<MietteSource> {
        if let Some(uri) = self.path_to_uri(path) {
            if let Some(doc) = self.documents.get(&uri) {
                return Ok(Arc::new(miette::NamedSource::new(
                    path.to_string_lossy().to_string(),
                    Arc::new(doc.source.clone()),
                )));
            }
        }

        self.inner.load(path)
    }
}
