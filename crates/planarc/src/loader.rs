use anyhow::{Context, Result, anyhow};
use libloading::{Library, Symbol};
use std::collections::BTreeMap;
use std::path::Path;
use std::sync::{Arc, RwLock};
use tree_sitter::Language;

type LanguageFn = unsafe extern "C" fn() -> Language;

pub trait LanguageProvider {
    fn load_language(&self, lang_name: &str, path: &Path) -> Result<Language>;
}

#[derive(Default)]
pub struct DynamicLanguageLoader {
    libs: RwLock<BTreeMap<String, Arc<Library>>>,
}

impl LanguageProvider for DynamicLanguageLoader {
    fn load_language(&self, lang_name: &str, path: &Path) -> Result<Language> {
        {
            let libs = self.libs.read().unwrap();
            if let Some(lib) = libs.get(lang_name) {
                return unsafe { self.get_symbol(lib, lang_name) };
            }
        }

        let lib = unsafe { Library::new(path) }
            .with_context(|| format!("Failed to load dynamic library at {:?}", path))?;

        let arc_lib = Arc::new(lib);
        self.libs
            .write()
            .unwrap()
            .insert(lang_name.to_string(), arc_lib.clone());

        unsafe { self.get_symbol(&arc_lib, lang_name) }
    }
}

impl DynamicLanguageLoader {
    #[allow(unsafe_op_in_unsafe_fn)]
    unsafe fn get_symbol(&self, lib: &Library, lang_name: &str) -> Result<Language> {
        let symbol_name = format!("tree_sitter_{}", lang_name.replace('-', "_"));
        let constructor: Symbol<LanguageFn> = lib.get(symbol_name.as_bytes())?;
        Ok(constructor())
    }
}

#[cfg(test)]
pub struct MockLanguageLoader;

#[cfg(test)]
impl LanguageProvider for MockLanguageLoader {
    fn load_language(
        &self,
        name: &str,
        _: &std::path::Path,
    ) -> anyhow::Result<tree_sitter::Language> {
        match name {
            "pdl" => Ok(tree_sitter_planardl::LANGUAGE.into()),
            _ => Err(anyhow::anyhow!("Grammar {name} not found")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;
    use tree_sitter::{Node, Parser};

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
            if let Some(name) = field_name {
                format!("{}: ", name)
            } else {
                "".to_string()
            },
            kind,
            start.row,
            start.column,
            end.row,
            end.column
        );

        if node.child_count() == 0 {
            let text = &source[node.start_byte()..node.end_byte()];
            if !text.trim().is_empty() {
                result.push_str(&format!(": \"{}\"", text.replace('\n', "\\n")));
            }
        }

        for i in 0..node.child_count() {
            result.push('\n');
            result.push_str(&format_tree(node.child(i).unwrap(), source, depth + 1));
        }
        result
    }

    fn run_snapshot_test(lang_name: &str, code: &str, snapshot_name: &str) {
        // let loader = LanguageLoader::default();
        // let lang = loader.load(lang_name).expect("Run `planar setup` first");

        // let mut parser = Parser::new();
        // parser.set_language(&lang).unwrap();
        // let tree = parser.parse(code, None).unwrap();

        // let formatted = format_tree(tree.root_node(), code, 0);
        // assert_snapshot!(snapshot_name, formatted);
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
