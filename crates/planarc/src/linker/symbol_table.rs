use std::collections::BTreeMap;

use rkyv::{Archive, Deserialize, Serialize};

use crate::{
    ast,
    linker::ids::{SymbolId, SymbolKind},
    spanned::Location,
};

#[derive(Archive, Serialize, Deserialize, Debug, PartialEq, Eq, Clone, Copy)]
#[rkyv(derive(Debug))]
pub enum Visibility {
    Public,
    Package,
    ModulePrivate,
    Scoped(SymbolId),
}

#[derive(Archive, Serialize, Deserialize, Debug, PartialEq, Eq)]
#[rkyv(derive(Debug))]
pub struct SymbolMetadata {
    pub id: SymbolId,
    pub fqmn: String,
    pub kind: SymbolKind,
    pub location: Location,
    pub visibility: Visibility,
    pub package: String,
    pub module: String,
}

#[derive(Archive, Serialize, Deserialize, Debug, PartialEq, Eq, Default)]
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
                kind: SymbolKind::Type,
                location: Location::default(),
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

    pub fn insert(
        &mut self,
        fqmn: &str,
        kind: SymbolKind,
        loc: Location,
        visibility: Visibility,
        package: String,
        module: String,
    ) -> Result<SymbolId, Location> {
        if let Some(existing_id) = self.name_to_id.get(fqmn) {
            return Err(self.symbols[existing_id].location);
        }

        let id = SymbolId(self.next_id);
        self.next_id += 1;

        let meta = SymbolMetadata {
            id,
            fqmn: fqmn.to_string(),
            kind,
            location: loc,
            visibility,
            package,
            module,
        };

        self.name_to_id.insert(fqmn.to_string(), id);
        self.symbols.insert(id, meta);

        Ok(id)
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
