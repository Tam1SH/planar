use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

fn main() {
    let node_types_json = Path::new("tree-sitter-pdl/src/node-types.json");
    let pdl_rs_output = Path::new("src/pdl.rs");

    println!("cargo:rerun-if-changed={}", node_types_json.display());

    let code = type_sitter_gen::generate_nodes(node_types_json)
        .expect("Failed to generate tree-sitter nodes")
        .into_string();
    fs::write(pdl_rs_output, code).expect("Failed to write pdl.rs");

    println!("cargo:rerun-if-changed=src");

    let timestamp = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs();

    println!("cargo:rustc-env=PLANAR_COMPILER_FINGERPRINT={}", timestamp);
}
