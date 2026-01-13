use anyhow::{anyhow, Context, Result};
use tree_sitter::{Node, Parser, Tree};
use std::path::Path;
use std::fs;

use crate::LanguageLoader;

pub struct Compiler {
    loader: LanguageLoader,
}

pub struct CompilationUnit {
    pub pdl_tree: Tree,
    pub source_code: String,
    pub target_language: tree_sitter::Language,
    pub schema_name: String,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            loader: LanguageLoader::default(),
        }
    }

    
    pub fn compile_file<P: AsRef<Path>>(&self, path: P) -> Result<CompilationUnit> {
        let source_code = fs::read_to_string(&path)
            .with_context(|| format!("Failed to read source file: {:?}", path.as_ref()))?;

        self.compile_source(&source_code)
    }

    pub fn compile_source(&self, source_code: &str) -> Result<CompilationUnit> {
        
        let mut parser = Parser::new();
        let pdl_lang = tree_sitter_planardl::LANGUAGE;
        parser.set_language(&pdl_lang.into())
            .context("Failed to load PlanarDL grammar")?;

        let tree = parser.parse(source_code, None)
            .ok_or_else(|| anyhow!("Failed to parse PDL source code"))?;

        let root = tree.root_node();
        println!("{}", format_tree(root, "", 2));

        if root.has_error() {
            return Err(anyhow!("Syntax error in PDL file (root level)"));
        }


        let header_node = root.child_by_field_name("header")
            .ok_or_else(|| anyhow!("Missing 'header' definition in PDL file"))?;

            
        let (schema_name, grammar_name) = self.parse_header(header_node, source_code)?;

        println!("Planar: Schema '{}', loading grammar '{}'...", schema_name, grammar_name);

        
        let target_language = self.loader.load(&grammar_name)
            .with_context(|| format!("Failed to load target grammar '{}'", grammar_name))?;

        Ok(CompilationUnit {
            pdl_tree: tree,
            source_code: source_code.to_string(),
            target_language,
            schema_name,
        })
    }

    
    fn parse_header(&self, node: Node, source: &str) -> Result<(String, String)> {
        let mut cursor = node.walk();
        let children: Vec<Node> = node.children(&mut cursor).collect();

        
        let strings: Vec<String> = children.iter()
            .filter(|n| n.kind() == "string")
            .map(|n| {
                let text = &source[n.start_byte()..n.end_byte()];
                
                text.trim_matches('"').to_string()
            })
            .collect();

        if strings.len() < 2 {
            return Err(anyhow!("Invalid header. Expected: schema \"name\" grammar=\"lib_name\""));
        }

        let schema_name = strings[0].clone();
        let grammar_name = strings[1].clone();

        Ok((schema_name, grammar_name))
    }
}


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
        let start = node.start_byte();
        let end = node.end_byte();

        
        if let Some(text) = source.get(start..end) {
            if !text.trim().is_empty() {
                result.push_str(&format!(": \"{}\"", text.replace('\n', "\\n")));
            }
        } else {
            
            result.push_str(": <<INVALID_RANGE>>"); 
        }
    }

    for i in 0..node.child_count() {
        result.push('\n');
        if let Some(child) = node.child(i as u32) {
            result.push_str(&format_tree(child, source, depth + 1));
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn test_compile_header_loading() -> Result<()> {
        
        let code = r#"
            schema "nginx" grammar="nginx" 
        "#;

        let compiler = Compiler::new();
        
        match compiler.compile_source(code) {
            Ok(unit) => {
                assert_eq!(unit.schema_name, "nginx");
                println!("Successfully loaded grammar for nginx!");
            }
            Err(e) => {
                
                let msg = e.to_string();
                if msg.contains("Grammar binary") && msg.contains("not found") {
                    println!("Parser worked, but grammar lib is missing (expected in clean env)");
                } else {
                    return Err(e);
                }
            }
        }

        Ok(())
    }
}
