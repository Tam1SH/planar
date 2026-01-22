use crate::artifact::model::Bundle;
use crate::compiler::CompilationResult;
use std::collections::BTreeMap;

pub fn create_bundle(compilation: CompilationResult) -> Bundle {
    let mut files = BTreeMap::new();
    for (file_id, path) in compilation.registry.files.into_iter() {
        files.insert(file_id, path.origin);
    }

    let wasm_modules = BTreeMap::new();

    Bundle {
        symbol_table: compilation.symbol_table,
        modules: compilation.modules,
        wasm_modules,
        files,
        grammars: compilation.grammars.to_metadata(),
    }
}
