use sha2::{Digest, Sha256};
use tree_sitter::{Node, Tree};
use xxhash_rust::xxh3::Xxh3;

#[derive(Clone)]
pub struct IdGenerator {
    file_path: String,
}

impl IdGenerator {
    pub fn new(file_path: &str) -> Self {
        Self {
            file_path: file_path.to_string(),
        }
    }

    pub fn generate(&self, node: Node, source: &[u8]) -> String {
        let mut hasher = Xxh3::new();

        hasher.update(self.file_path.as_bytes());
        hasher.update(b"::");

        let mut path = Vec::new();
        let mut curr = Some(node);
        while let Some(n) = curr {
            path.push(n);
            curr = n.parent();
        }
        path.reverse();

        let mut leaf_idx = 0;
        let path_len = path.len();

        for (i, n) in path.into_iter().enumerate() {
            let kind = n.kind();
            let field = self.get_field_name(n).unwrap_or_default();
            let raw_content = &source[n.byte_range()];

            let idx = self.count_preceding_duplicates(n, source);

            if i == path_len - 1 {
                leaf_idx = idx;
            }

            hasher.update(kind.as_bytes());
            hasher.update(field.as_bytes());

            self.hash_normalized(&mut hasher, kind, raw_content);

            hasher.update(idx.to_string().as_bytes());
            hasher.update(b"->");
        }

        let hash_result = hasher.digest128();
        let hex_hash = format!("{:032x}", hash_result);

        if leaf_idx > 0 {
            format!("{}::{}", hex_hash, leaf_idx)
        } else {
            hex_hash
        }
    }

    fn hash_normalized(&self, hasher: &mut Xxh3, kind: &str, content: &[u8]) {
        if kind.contains("string") || kind.contains("literal") {
            hasher.update(content);
            return;
        }

        let mut in_whitespace = false;
        let mut first_char = true;

        for &byte in content {
            if byte.is_ascii_whitespace() {
                if !in_whitespace && !first_char {
                    hasher.update(b" ");
                    in_whitespace = true;
                }
            } else {
                hasher.update(&[byte]);
                in_whitespace = false;
                first_char = false;
            }
        }
    }

    fn get_field_name(&self, node: Node) -> Option<String> {
        let parent = node.parent()?;
        let target_id = node.id();
        let mut cursor = parent.walk();

        if cursor.goto_first_child() {
            loop {
                if cursor.node().id() == target_id {
                    return cursor.field_name().map(|s| s.to_string());
                }
                if !cursor.goto_next_sibling() {
                    break;
                }
            }
        }
        None
    }

    fn count_preceding_duplicates(&self, node: Node, source: &[u8]) -> usize {
        let mut count = 0;
        let mut current = node;
        let target_kind = node.kind();
        let target_field = self.get_field_name(node);
        let target_content = &source[node.byte_range()];

        while let Some(prev) = current.prev_sibling() {
            if prev.kind() == target_kind
                && self.get_field_name(prev) == target_field
                && self.are_nodes_equivalent(
                    target_kind,
                    target_content,
                    &source[prev.byte_range()],
                )
            {
                count += 1;
            }
            current = prev;
        }
        count
    }

    fn are_nodes_equivalent(&self, kind: &str, a: &[u8], b: &[u8]) -> bool {
        if kind.contains("string") {
            return a == b;
        }

        let iter_a = a.iter().filter(|&&x| !x.is_ascii_whitespace());
        let iter_b = b.iter().filter(|&&x| !x.is_ascii_whitespace());
        iter_a.eq(iter_b)
    }
}

pub struct NodeLocator {
    generator: IdGenerator,
}

impl NodeLocator {
    pub fn new(generator: IdGenerator) -> Self {
        Self { generator }
    }

    pub fn from_path(file_path: &str) -> Self {
        Self {
            generator: IdGenerator::new(file_path),
        }
    }

    pub fn locate(
        &self,
        tree: &Tree,
        source: &[u8],
        target_id: &str,
    ) -> Option<tree_sitter::Range> {
        let mut cursor = tree.walk();
        let mut visited_root = false;

        loop {
            let node = cursor.node();

            let current_id = self.generator.generate(node, source);

            if current_id == target_id {
                return Some(node.range());
            }

            if cursor.goto_first_child() {
                continue;
            }
            if cursor.goto_next_sibling() {
                continue;
            }
            loop {
                if !cursor.goto_parent() {
                    visited_root = true;
                    break;
                }
                if cursor.goto_next_sibling() {
                    break;
                }
            }
            if visited_root {
                break;
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tree_sitter::Parser;

    fn parse_rust(code: &str) -> Tree {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_rust::LANGUAGE.into())
            .unwrap();
        parser.parse(code, None).unwrap()
    }

    fn find_leaf_by_text<'a>(tree: &'a Tree, source: &[u8], text: &str) -> Node<'a> {
        let mut cursor = tree.walk();
        loop {
            let node = cursor.node();
            if node.child_count() == 0 && &source[node.byte_range()] == text.as_bytes() {
                return node;
            }
            if cursor.goto_first_child() {
                continue;
            }
            if cursor.goto_next_sibling() {
                continue;
            }
            loop {
                if !cursor.goto_parent() {
                    panic!("Node not found: {}", text);
                }
                if cursor.goto_next_sibling() {
                    break;
                }
            }
        }
    }

    #[test]
    fn test_entities_separation() {
        let code = "fn main() { let x = 42; }";
        let source = code.as_bytes();
        let tree = parse_rust(code);
        let path = "src/main.rs";

        let generator = IdGenerator::new(path);

        let node_42 = find_leaf_by_text(&tree, source, "42");
        let stable_id = generator.generate(node_42, source);

        println!("Generated ID: {}", stable_id);

        let locator = NodeLocator::new(generator.clone());

        let found_range = locator
            .locate(&tree, source, &stable_id)
            .expect("Locator должен найти ноду по ID");

        assert_eq!(found_range, node_42.range());
        assert_eq!(&source[found_range.start_byte..found_range.end_byte], b"42");
    }

    #[test]
    fn test_locator_fail() {
        let code = "fn main() {}";
        let locator = NodeLocator::from_path("src/test.rs");
        let tree = parse_rust(code);

        let result = locator.locate(&tree, code.as_bytes(), "non_existent_hash");
        assert!(result.is_none());
    }

    fn find_all_strings<'a>(tree: &'a Tree, source: &[u8]) -> Vec<Node<'a>> {
        let mut nodes = Vec::new();
        let mut cursor = tree.walk();
        let mut reached_root = false;
        loop {
            if cursor.node().kind() == "string_literal" {
                nodes.push(cursor.node());
            }
            if cursor.goto_first_child() {
                continue;
            }
            if cursor.goto_next_sibling() {
                continue;
            }
            loop {
                if !cursor.goto_parent() {
                    reached_root = true;
                    break;
                }
                if cursor.goto_next_sibling() {
                    break;
                }
            }
            if reached_root {
                break;
            }
        }
        nodes
    }

    #[test]
    fn test_duplicates_disambiguation() {
        let generator = IdGenerator::new("test.rs");
        let code = r#"
            fn main() {
                log("INFO");
                log("INFO");
            }
        "#;
        let tree = parse_rust(code);
        let source = code.as_bytes();
        let nodes = find_all_strings(&tree, source);

        assert_eq!(nodes.len(), 2);

        let id1 = generator.generate(nodes[0], source);
        let id2 = generator.generate(nodes[1], source);

        println!("ID 1: {}", id1);
        println!("ID 2: {}", id2);

        assert_ne!(id1, id2, "IDs must be unique for different calls");

        let locator = NodeLocator::new(generator);
        assert_eq!(
            locator.locate(&tree, source, &id1).unwrap().start_byte,
            nodes[0].start_byte()
        );
        assert_eq!(
            locator.locate(&tree, source, &id2).unwrap().start_byte,
            nodes[1].start_byte()
        );
    }
}
