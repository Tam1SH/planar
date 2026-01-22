use anyhow::{Context, Result};
use console::style;
use planarc::artifact::reader::load_bundle;
use std::path::PathBuf;

pub fn run(path: PathBuf) -> Result<()> {
    let data =
        std::fs::read(&path).with_context(|| format!("Failed to read artifact at {:?}", path))?;

    let program_data =
        load_bundle(&data, None).map_err(|e| anyhow::anyhow!("Failed to load program: {:?}", e))?;

    let program = program_data.archived;

    println!(
        "{}",
        style(format!("--- Artifact: {} ---", path.display()))
            .bold()
            .cyan()
    );

    println!("\n{}", style("Symbol Table:").underlined().yellow());

    println!(
        "{:<30} | {:<4} | {:<8} | {:<20}",
        "Fully Qualified Name", "ID", "Kind", "Origin"
    );
    println!("{}", "-".repeat(80));

    for (symbol_id, meta) in program.symbol_table.symbols.iter() {
        let kind_str = format!("{:?}", meta.kind);
        let span = &meta.location.span;
        let file_id = &meta.location.file_id;
        let fqdm = program.symbol_table.symbols
            .get(symbol_id)
            .map(|m| m.fqmn.as_str())
            .unwrap_or("unknown");

        let origin = if fqdm.starts_with("builtin.") {
            style("<builtin>").dim().to_string()
        } else {
            let file_path = program
                .files
                .get(file_id)
                .map(|s| s.as_str())
                .unwrap_or("unknown");

            format!(
                "{}:{}:{}",
                file_path,
                style(span.line).yellow(),
                style(span.col).dim()
            )
        };

        println!(
            "{:<30} | {:<4} | {:<8} | {:<20}",
            style(fqdm).green(),
            style(meta.id.0.to_string()).cyan(),
            kind_str,
            origin
        );
    }

    println!("\n{}", style("Grammars:").underlined().yellow());
    if program.grammars.is_empty() {
        println!("  {}", style("No grammars defined").dim());
    } else {
        for (name, metadata) in program.grammars.iter() {
            println!(
                "  {} (version: {})",
                style(name).green().bold(),
                style(&metadata.version).cyan()
            );
        }
    }

    println!("\n{}", style("Modules Structure:").underlined().yellow());
    for (mod_name, module) in program.modules.iter() {
        println!("  {} ", style(mod_name).bold().green());

        for fact in module.facts.iter() {
            println!(
                "    {} {} (id: {})",
                style("fact").magenta(),
                fact.value.name,
                fact.value.id.0
            );
        }

        for ty in module.types.iter() {
            println!(
                "    {} {} (id: {})",
                style("type").blue(),
                ty.value.name,
                ty.value.id.0
            );
        }
        for externs in module.externs.iter() {
            for func in externs.value.functions.iter() {
                println!(
                    "    {} {} (id: {})",
                    style("function").cyan(),
                    func.value.name,
                    func.value.id.0
                );
            }
        }
    }

    Ok(())
}
