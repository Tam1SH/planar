pub mod builder;
pub mod header;
pub mod model;
pub mod reader;
pub mod writer;

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::fs;
    use std::path::Path;
    use tempfile::TempDir;

    use super::*;
    use crate::artifact::model::Bundle;
    use crate::artifact::reader::{LoadError, load_bundle};
    use crate::artifact::writer::write_bundle;
    use crate::compiler::Compiler;
    use crate::module_loader::{FsModuleLoader, PackageRoot};
    use crate::linker::meta::{ArchivedSymbolId, ResolvedId};

    fn compile_to_bundle(files: Vec<(&str, &str)>) -> Bundle {
        let temp = TempDir::new().expect("failed to create temp dir");
        let base_path = temp.path();

        let mut package_roots = std::collections::HashMap::new();

        for (rel_path, content) in files {
            let full_path = base_path.join(rel_path);
            if let Some(parent) = full_path.parent() {
                fs::create_dir_all(parent).unwrap();
            }
            fs::write(&full_path, content).unwrap();

            let first_dir = Path::new(rel_path)
                .components()
                .next()
                .unwrap()
                .as_os_str()
                .to_string_lossy()
                .to_string();

            package_roots
                .entry(first_dir.clone())
                .or_insert_with(|| base_path.join(&first_dir));
        }

        let roots: Vec<_> = package_roots
            .into_iter()
            .map(|(name, path)| PackageRoot { name, path })
            .collect();

        let compiler = Compiler::new(FsModuleLoader).with_prelude(vec![]);
        
        let result = compiler
            .compile(roots, BTreeMap::new())
            .expect("Compilation failed during bundle creation");

        if result.has_errors() {
            panic!("Test program has compilation errors: {:?}", result.errors);
        }

        Bundle {
            world: result.typed_world,
            wasm_modules: BTreeMap::from([
                ("logic".to_string(), vec![0xDE, 0xAD, 0xBE, 0xEF])
            ]),
            files: result.registry.files.iter()
                .map(|(id, source)| (*id, source.inner().to_string()))
                .collect(),
            grammars: BTreeMap::new(), 
        }
    }

    fn create_test_program() -> Bundle {
        compile_to_bundle(vec![
            ("core/models.pdl", "pub fact Base { id: builtin.str }"),
            ("app/main.pdl", "import core.models\nfact User { info: core.models.Base }"),
        ])
    }

    #[test]
    fn test_artifact_full_cycle_integrity() {
        let original = create_test_program();
        let mut buffer = Vec::new();

        write_bundle(&original, &mut buffer, Some(1337)).expect("Writing failed");

        let loaded = load_bundle(&buffer, Some(1337)).expect("Loading failed");
        let archived = loaded.archived;

        assert!(archived.world.table.next_id > 0);
        assert_eq!(
            archived.wasm_modules.get("logic").unwrap().as_slice(),
            &[0xDE, 0xAD, 0xBE, 0xEF]
        );

        let main_module = archived.world.modules.get("app.main").expect("Module 'app.main' missing");
        let fact = &main_module.facts[0].value;
        assert_eq!(fact.name, "User");
        
        let field_ty = &fact.fields[0].value.ty;
        match &field_ty.symbol.value {
            id => {
                assert!(archived.world.table.symbols.contains_key(id));
            }
            _ => panic!("Expected Global resolution for core.models.Base"),
        }
    }

    #[test]
    fn test_error_truncated_file() {
        let data = vec![b'P', b'D', b'L', b'A', 0, 0, 0, 1];
        let result = load_bundle(&data, Some(1337));
        assert!(matches!(result, Err(LoadError::Truncated)));
    }

    #[test]
    fn test_error_invalid_magic() {
        let prog = create_test_program();
        let mut buf = Vec::new();
        write_bundle(&prog, &mut buf, Some(1337)).unwrap();

        buf[0] = b'N'; buf[1] = b'O'; buf[2] = b'P'; buf[3] = b'E';

        let result = load_bundle(&buf, Some(1337));
        match result {
            Err(LoadError::InvalidMagic(m)) => assert_eq!(&m, b"NOPE"),
            _ => panic!("Should have failed with InvalidMagic"),
        }
    }

    #[test]
    fn test_error_version_mismatch() {
        let prog = create_test_program();
        let mut buf = Vec::new();
        write_bundle(&prog, &mut buf, Some(1337)).unwrap();

        buf[4] = 0xFE;
        buf[5] = 0xCA;

        let result = load_bundle(&buf, Some(1337));
        assert!(matches!(result, Err(LoadError::VersionMismatch { .. })));
    }

    #[test]
    fn test_error_checksum_body_corrupted() {
        let prog = create_test_program();
        let mut buf = Vec::new();
        write_bundle(&prog, &mut buf, Some(1337)).unwrap();

        let idx = buf.len() - 5;
        buf[idx] = !buf[idx];

        let result = load_bundle(&buf, Some(1337));
        assert!(matches!(result, Err(LoadError::ChecksumMismatch { .. })));
    }

    #[test]
    fn test_error_checksum_header_tampered() {
        let prog = create_test_program();
        let mut buf = Vec::new();
        write_bundle(&prog, &mut buf, Some(1337)).unwrap();

        buf[16] = buf[16].wrapping_add(1);

        let result = load_bundle(&buf, Some(1337));
        assert!(matches!(result, Err(LoadError::ChecksumMismatch { .. })));
    }

    #[test]
    fn test_empty_program_works() {
        let prog = compile_to_bundle(vec![]);
        let mut buf = Vec::new();
        write_bundle(&prog, &mut buf, Some(1337)).unwrap();

        let loaded = load_bundle(&buf, Some(1337)).expect("Empty program should be valid");
        assert!(loaded.archived.world.modules.is_empty());
    }

    #[test]
    fn test_program_binary_identity_snapshot() {
        let original = create_test_program();
        let mut buffer = Vec::new();
        write_bundle(&original, &mut buffer, Some(1337)).expect("Failed to write");

        let loaded = load_bundle(&buffer, Some(1337)).expect("Failed to load");
        let archived = loaded.archived;

        insta::assert_debug_snapshot!("program_roundtrip", (&original, archived));
    }
}