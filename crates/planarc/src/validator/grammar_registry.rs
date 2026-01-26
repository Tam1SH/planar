use std::{
    collections::{BTreeMap, HashMap},
    path::PathBuf,
    sync::{Arc, RwLock},
};

use anyhow::{Result, anyhow};
use tree_sitter::Language;

use crate::{artifact::model::GrammarMetadata, loader::LanguageProvider};

pub struct GrammarRegistry {
    loader: Box<dyn LanguageProvider + Send + Sync>,
    paths: BTreeMap<String, PathBuf>,
}

impl GrammarRegistry {
    pub fn new(loader: Box<dyn LanguageProvider + Send + Sync>) -> Self {
        Self {
            loader,
            paths: BTreeMap::default(),
        }
    }

    pub fn new_with_paths(
        loader: Box<dyn LanguageProvider + Send + Sync>,
        paths: BTreeMap<String, PathBuf>,
    ) -> Self {
        Self { loader, paths }
    }

    pub fn add_grammar(&mut self, name: String, path: PathBuf) {
        self.paths.insert(name, path);
    }

    pub fn get_language(&self, name: &str) -> Result<Language> {
        let path = self
            .paths
            .get(name)
            .ok_or_else(|| anyhow::anyhow!("Grammar '{}' not registered", name))?;
        self.loader.load_language(name, path)
    }

    pub fn to_metadata(self) -> BTreeMap<String, GrammarMetadata> {
        self.paths
            .into_keys()
            .map(|k| {
                (
                    k,
                    GrammarMetadata {
                        version: "latest".to_string(),
                    },
                )
            })
            .collect()
    }
}
