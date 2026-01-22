use anyhow::{Result, anyhow};
use console::style;
use planar_pkg::config::PlanarContext;

pub fn run_set(key: String, value: String) -> Result<()> {
    let mut ctx = PlanarContext::new();

    match key.as_str() {
        "std-path" => {
            let path = std::path::PathBuf::from(value);

            let abs_path = path
                .canonicalize()
                .map_err(|_| anyhow!("Path {:?} does not exist", path))?;
            ctx.config.std_override_path = Some(abs_path);
        }
        "cache-dir" => {
            ctx.config.cache_dir = Some(std::path::PathBuf::from(value));
        }
        "registry-url" => {
            ctx.config.registry_url = Some(value);
        }
        _ => {
            return Err(anyhow!(
                "Unknown key: {}. Available keys: std-path, cache-dir, registry-url",
                key
            ));
        }
    }

    ctx.save()?;
    println!("{} Updated {} successfully", style("✔").green(), key);
    Ok(())
}

pub fn run_list() -> Result<()> {
    let ctx = PlanarContext::new();

    println!(
        "{}",
        style("Planar Global Configuration:").bold().underlined()
    );
    println!(
        "Config file: {}\n",
        style(PlanarContext::config_path().display()).dim()
    );

    let std_path = ctx
        .config
        .std_override_path
        .as_ref()
        .map(|p| p.display().to_string())
        .unwrap_or_else(|| style("not set (using github)").dim().to_string());

    let registry_url = if let Some(url) = &ctx.config.registry_url {
        url.clone()
    } else {
        format!("{} {}", ctx.registry_url(), style("(default)").dim())
    };

    let cache_dir = ctx.cache_dir.display().to_string();

    println!("{:<20} {}", style("std-path:").cyan(), std_path);
    println!("{:<20} {}", style("registry-url:").cyan(), registry_url);
    println!("{:<20} {}", style("cache-dir:").cyan(), cache_dir);

    Ok(())
}
