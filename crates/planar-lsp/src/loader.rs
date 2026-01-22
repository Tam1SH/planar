use crate::Document;
use anyhow::{Context, Result};
use dashmap::DashMap;
use planarc::module_loader::{DiscoveredModule, FsModuleLoader, ModuleLoader, PackageRoot, Source};
use std::path::{Path, PathBuf};
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
        let mut modules = self.inner.scan(root)?;
        Ok(modules)
    }

    fn load(&self, path: &Path) -> Result<Source> {
        if let Some(uri) = self.path_to_uri(path) {
            if let Some(doc) = self.documents.get(&uri) {
                return Ok(Source {
                    origin: path.to_string_lossy().to_string(),
                    content: doc.source.clone(),
                });
            }
        }

        self.inner.load(path)
    }
}
