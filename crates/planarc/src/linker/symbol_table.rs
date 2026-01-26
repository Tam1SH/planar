use std::{collections::BTreeMap, usize};

use rkyv::{Archive, Deserialize, Serialize};

use crate::linker::error::PreviousDefinition;
use crate::source_registry::SourceRegistry;
use crate::{
    ast,
    linker::meta::{SymbolId, SymbolKind, SymbolMetadata, Visibility},
    spanned::{FileId, Location},
};

use super::error::LinkerError;
use super::meta::PendingSymbol;

#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq, Default)]
#[rkyv(derive(Debug))]
pub struct SymbolTable {
    pub name_to_id: BTreeMap<String, SymbolId>,
    pub symbols: BTreeMap<SymbolId, SymbolMetadata>,
    pub next_id: usize,
}

impl SymbolTable {
    pub fn with_builtins() -> Self {
        let mut table = Self::default();
        

        for name in ["str", "i64", "f64", "bool", "list"] {
            let fqmn = format!("builtin.{}", name);
            let id = SymbolId(table.next_id);

            let meta = SymbolMetadata {
                id,
                fqmn: fqmn.clone(),
                kind: SymbolKind::Type {
                    is_primitive: true,
                    base_type: None,
                    fields: vec![],
                },
                location: Location {
                    file_id: FileId(usize::MAX),
                    ..Default::default()
                },
                visibility: Visibility::Public,
                package: "builtin".to_string(),
                module: "builtin".to_string(),
            };

            table.name_to_id.insert(fqmn, id);
            table.symbols.insert(id, meta);
            table.next_id += 1;
        }
        table
    }

    pub fn resolve(&self, fqmn: &str) -> Option<(SymbolId, Location)> {
        let id = self.name_to_id.get(fqmn)?;
        self.symbols.get(id).map(|m| (m.id, m.location))
    }

    pub fn get_fqmn(&self, id: SymbolId) -> Option<&String> {
        self.symbols.get(&id).map(|m| &m.fqmn)
    }

    pub fn register(
        &mut self,
        ps: PendingSymbol,
        registry: &SourceRegistry, 
    ) -> Result<SymbolId, Box<LinkerError>> {
        
        let fqmn = format!("{}.{}", ps.module_name, ps.name);

        if let Some(&existing_id) = self.name_to_id.get(&fqmn) {
            let prev_metadata = &self.symbols[&existing_id];
            
            return Err(create_collision_error(
                &fqmn,
                ps.loc,
                prev_metadata.location,
                registry,
            ));
        }

        let id = SymbolId(self.symbols.len());

        self.symbols.insert(id, SymbolMetadata {
            id,
            fqmn: fqmn.clone(),
            kind: ps.kind,
            location: ps.loc,
            visibility: ps.visibility,
            package: ps.package_name,
            module: ps.module_name,
        });

        self.name_to_id.insert(fqmn, id);

        Ok(id)
    }

    pub fn update_kind(&mut self, id: SymbolId, kind: SymbolKind) {
        if let Some(metadata) = self.symbols.get_mut(&id) {
            metadata.kind = kind;
        }
    }

    pub fn update_visibility(&mut self, id: SymbolId, vis: Visibility) {
        if let Some(meta) = self.symbols.get_mut(&id) {
            meta.visibility = vis;
        }
    }

    pub fn resolve_metadata(&self, fqmn: &str) -> Option<&SymbolMetadata> {
        let id = self.name_to_id.get(fqmn)?;
        self.symbols.get(id)
    }

    pub fn get_metadata_by_id(&self, id: SymbolId) -> Option<&SymbolMetadata> {
        self.symbols.get(&id)
    }

    pub fn debug_keys(&self) -> Vec<&String> {
        self.name_to_id.keys().collect()
    }
}

pub fn map_visibility(ast_vis: &ast::Visibility, node_id: Option<SymbolId>) -> Visibility {
    match ast_vis {
        ast::Visibility::Pub => Visibility::Public,
        ast::Visibility::Package => Visibility::Package,
        ast::Visibility::Private => {
            if let Some(id) = node_id {
                Visibility::Scoped(id)
            } else {
                Visibility::ModulePrivate
            }
        }
    }
}

fn create_collision_error(
    name: &str,
    loc: Location,
    prev_loc: Location,
    reg: &SourceRegistry,
) -> Box<LinkerError> {
    let (src, span) = reg.get_source_and_span(loc);
    let (p_src, p_span) = reg.get_source_and_span(prev_loc);
    Box::new(LinkerError::SymbolCollision {
        name: name.to_string(),
        src,
        span,
        loc,
        related: vec![PreviousDefinition {
            src: p_src,
            span: p_span,
            loc: prev_loc,
        }],
    })
}
