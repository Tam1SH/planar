use anyhow::{Context, bail};
use console::{style, Emoji};
use std::fs;
use std::path::{Path, PathBuf};

static SPARKLE: Emoji<'_, '_> = Emoji("✨ ", "");
static FILE: Emoji<'_, '_> = Emoji("📄 ", "");
static FOLDER: Emoji<'_, '_> = Emoji("gd ", ""); 


const MAIN_PDL_CONTENT: &str = r#"schema "nginx" grammar="tree-sitter-nginx"

import std.fs 

query IncludePattern = `
(directive
    (keyword) @kw
    (#eq? @kw "include")
    [(file) (mask)] @path
)
`

node IncludeDirective {
    match IncludePattern {
        @path {
            global -> std.fs.resolve_glob $CURRENT_FILE @path.text
        }
    }
}
"#;

pub fn run(path: Option<String>) -> anyhow::Result<()> {
    
    let (root_path, name) = match path {
        Some(p) => {
            let path = PathBuf::from(&p);
            let name = path.file_name()
                .context("Invalid path")?
                .to_string_lossy()
                .to_string();
            (path, name)
        }
        None => {
            let path = std::env::current_dir()?;
            let name = path.file_name()
                .context("Cannot determine directory name")?
                .to_string_lossy()
                .to_string();
            (path, name)
        }
    };

    if root_path.join("planar.kdl").exists() {
        bail!("Directory '{}' already contains a planar.kdl file", name);
    }

    let src_dir = root_path.join("src");
    fs::create_dir_all(&src_dir).context("Failed to create src directory")?;

    println!("{} Creating new Planar project: {}", SPARKLE, style(&name).bold().cyan());

    let kdl_content = format!(
        r#"package {{
    name "{}"
    version "0.1.0"
}}

dependencies {{
    // std git="https://github.com/planar-lang/std"
}}
"#,
        name
    );

    create_file(&root_path.join("planar.kdl"), &kdl_content)?;
    create_file(&src_dir.join("main.pdl"), MAIN_PDL_CONTENT)?;

    let gitignore_content = "/target\n";
    create_file(&root_path.join(".gitignore"), gitignore_content)?;

    println!(
        "\n{} Done! Run {} to build.",
        style("✔").green(),
        style("pdl build").bold()
    );

    Ok(())
}

fn create_file(path: &Path, content: &str) -> anyhow::Result<()> {
    fs::write(path, content).with_context(|| format!("Failed to write {:?}", path))?;
    let filename = path.file_name().unwrap().to_string_lossy();
    println!("  {} Created {}", FILE, style(filename).dim());
    Ok(())
}