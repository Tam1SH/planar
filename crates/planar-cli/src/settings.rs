
use std::collections::HashMap;
use std::fs;
use anyhow::Context;
use serde::{Deserialize, Serialize};

const DEFAULT_REGISTRY: &str = "https://github.com/Tam1SH/planar-grammars-registry/releases/latest/download";

#[derive(Serialize, Deserialize, Default)]
struct Settings {
    values: HashMap<String, String>,
}

fn path() -> anyhow::Result<std::path::PathBuf> {
    let p = dirs::config_dir()
        .context("No config dir")?
        .join("planar/settings.json");
    if let Some(parent) = p.parent() {
        fs::create_dir_all(parent)?;
    }
    Ok(p)
}

fn load() -> Settings {
    fs::read_to_string(path().unwrap())
        .ok()
        .and_then(|s| serde_json::from_str(&s).ok())
        .unwrap_or_default()
}

pub fn set(key: &str, value: &str) -> anyhow::Result<()> {
    let mut s = load();
    s.values.insert(key.to_string(), value.to_string());
    let content = serde_json::to_string_pretty(&s)?;
    fs::write(path()?, content)?;
    println!("{} = {}", key, value);
    Ok(())
}

pub fn get(key: &str) -> anyhow::Result<String> {
    let s = load();
    s.values.get(key).cloned().context("Key not found")
}

pub fn get_registry() -> anyhow::Result<String> {
    // Env -> Config File -> Fallback
    if let Ok(env) = std::env::var("PLANAR_REGISTRY") {
        return Ok(env);
    }
    Ok(get("registry").unwrap_or_else(|_| DEFAULT_REGISTRY.to_string()))
}