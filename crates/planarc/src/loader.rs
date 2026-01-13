use anyhow::{anyhow, Context, Result};
use libloading::{Library, Symbol};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use tree_sitter::Language;

use crate::common;


type LanguageFn = unsafe extern "C" fn() -> Language;

#[derive(Default)]
pub struct LanguageLoader {
    libs: RwLock<HashMap<String, Arc<Library>>>,
}

impl LanguageLoader {
    
    pub fn load(&self, lang_name: &str) -> Result<Language> {
        {
            let libs = self.libs.read().map_err(|_| anyhow!("Poisoned lock"))?;
            if let Some(lib) = libs.get(lang_name) {
                return unsafe { self.get_symbol(lib, lang_name) };
            }
        }

        let filename = common::format_filename(lang_name);
        let lib_path = common::local_dir().join(filename);

        if !lib_path.exists() {
            return Err(anyhow!(
                "Grammar binary for '{}' not found. Please run `planar setup` first.\nExpected path: {:?}",
                lang_name, lib_path
            ));
        }

        let lib = unsafe { Library::new(&lib_path) }
            .with_context(|| format!("Failed to load dynamic library at {:?}", lib_path))?;
        
        let arc_lib = Arc::new(lib);

        {
            let mut libs = self.libs.write().map_err(|_| anyhow!("Poisoned lock"))?;
            libs.insert(lang_name.to_string(), arc_lib.clone());
        }

        unsafe { self.get_symbol(&arc_lib, lang_name) }
    }

    #[allow(unsafe_op_in_unsafe_fn)]
    unsafe fn get_symbol(&self, lib: &Library, lang_name: &str) -> Result<Language> {
        
        let symbol_name = format!("tree_sitter_{}", lang_name.replace('-', "_"));
        
        let constructor: Symbol<LanguageFn> = lib.get(symbol_name.as_bytes())
            .with_context(|| format!("Failed to find symbol '{}' in grammar binary", symbol_name))?;

        Ok(constructor())
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use tree_sitter::{Node, Parser};
    use insta::assert_snapshot;

    fn format_tree(node: Node, source: &str, depth: usize) -> String {
        let kind = node.kind();
        let start = node.start_position();
        let end = node.end_position();
        let field_name = node.parent().and_then(|p| {
            let mut cursor = p.walk();
            for child in p.children(&mut cursor) {
                if child.id() == node.id() {
                    return p.field_name_for_child(child.id() as u32);
                }
            }
            None
        });

        let mut result = format!(
            "{}{}{} [{}, {}] - [{}, {}]",
            "  ".repeat(depth),
            if let Some(name) = field_name { format!("{}: ", name) } else { "".to_string() },
            kind,
            start.row, start.column,
            end.row, end.column
        );

        if node.child_count() == 0 {
            let text = &source[node.start_byte()..node.end_byte()];
            if !text.trim().is_empty() {
                result.push_str(&format!(": \"{}\"", text.replace('\n', "\\n")));
            }
        }

        for i in 0..node.child_count() {
            result.push('\n');
            result.push_str(&format_tree(node.child(i as u32).unwrap(), source, depth + 1));
        }
        result
    }

    fn run_snapshot_test(lang_name: &str, code: &str, snapshot_name: &str) {
        let loader = LanguageLoader::default();
        let lang = loader.load(lang_name).expect("Run `planar setup` first");
        
        let mut parser = Parser::new();
        parser.set_language(&lang).unwrap();
        let tree = parser.parse(code, None).unwrap();
        
        let formatted = format_tree(tree.root_node(), code, 0);
        assert_snapshot!(snapshot_name, formatted);
    }


    #[test]
    fn test_snapshot_yaml() {
        let code = "services:\n  web:\n    image: nginx";
        run_snapshot_test("yaml", code, "yaml_basic");
    }

    #[test]
    fn test_snapshot_json() {
        let code = r#"{"foo": [1, 2, 3], "bar": null}"#;
        run_snapshot_test("json", code, "json_basic");
    }
}