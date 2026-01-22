use directories::ProjectDirs;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::PathBuf;

const APP_NAME: &str = "planar";
const DEFAULT_REGISTRY: &str =
    "https://github.com/project-planar/planar-grammars-registry/releases/download/latest";

#[derive(Debug, Deserialize, Serialize, Clone, Default)]
pub struct GlobalConfig {
    pub cache_dir: Option<PathBuf>,

    pub std_override_path: Option<PathBuf>,

    pub registry_url: Option<String>,
}

pub struct PlanarContext {
    pub config: GlobalConfig,
    pub config_dir: PathBuf,
    pub cache_dir: PathBuf,
}

impl PlanarContext {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let proj_dirs =
            ProjectDirs::from("", "", APP_NAME).expect("Could not determine home directory");

        Self::with_paths(
            proj_dirs.config_dir().to_path_buf(),
            proj_dirs.cache_dir().to_path_buf(),
        )
    }

    pub fn with_paths(config_dir: PathBuf, default_cache_dir: PathBuf) -> Self {
        let config_file = config_dir.join("config.json");

        if !config_dir.exists() {
            fs::create_dir_all(&config_dir).ok();
        }

        let config = if config_file.exists() {
            let content = fs::read_to_string(&config_file).unwrap_or_default();
            serde_json::from_str(&content).unwrap_or_else(|_| GlobalConfig::default())
        } else {
            let default_config = GlobalConfig::default();
            let content = serde_json::to_string_pretty(&default_config).unwrap();
            fs::write(&config_file, content).ok();
            default_config
        };

        let cache_dir = config.cache_dir.clone().unwrap_or(default_cache_dir);
        if !cache_dir.exists() {
            fs::create_dir_all(&cache_dir).ok();
        }

        Self {
            config,
            config_dir,
            cache_dir,
        }
    }

    pub fn config_path() -> PathBuf {
        let proj_dirs =
            ProjectDirs::from("", "", APP_NAME).expect("Could not determine home directory");
        proj_dirs.config_dir().join("config.json")
    }

    pub fn registry_url(&self) -> &str {
        self.config
            .registry_url
            .as_deref()
            .unwrap_or(DEFAULT_REGISTRY)
    }

    pub fn save(&self) -> anyhow::Result<()> {
        let path = self.config_dir.join("config.json");
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let content = serde_json::to_string_pretty(&self.config)?;
        std::fs::write(path, content)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    struct TestEnv {
        _tmp: TempDir,
        config_dir: PathBuf,
        cache_dir: PathBuf,
    }

    impl TestEnv {
        fn new() -> Self {
            let tmp = TempDir::new().unwrap();
            let config_dir = tmp.path().join("config");
            let cache_dir = tmp.path().join("cache");
            Self {
                _tmp: tmp,
                config_dir,
                cache_dir,
            }
        }

        fn create_context(&self) -> PlanarContext {
            PlanarContext::with_paths(self.config_dir.clone(), self.cache_dir.clone())
        }
    }

    #[test]
    fn test_creates_default_config_if_not_exists() {
        let env = TestEnv::new();
        let config_file = env.config_dir.join("config.json");

        assert!(!config_file.exists());

        let _ctx = env.create_context();

        assert!(config_file.exists(), "config.json should be created");
        let content = fs::read_to_string(config_file).unwrap();
        let parsed: GlobalConfig = serde_json::from_str(&content).unwrap();

        assert_eq!(parsed.registry_url, None);
    }

    #[test]
    fn test_loads_existing_config() {
        let env = TestEnv::new();
        fs::create_dir_all(&env.config_dir).unwrap();

        let custom_url = "https://my-registry.com";
        let config = GlobalConfig {
            registry_url: Some(custom_url.to_string()),
            ..GlobalConfig::default()
        };
        fs::write(
            env.config_dir.join("config.json"),
            serde_json::to_string(&config).unwrap(),
        )
        .unwrap();

        let ctx = env.create_context();
        assert_eq!(ctx.registry_url(), custom_url);
    }

    #[test]
    fn test_save_config() {
        let env = TestEnv::new();
        let mut ctx = env.create_context();

        let new_url = "https://new-registry.io";
        ctx.config.registry_url = Some(new_url.to_string());
        ctx.save().unwrap();

        let content = fs::read_to_string(env.config_dir.join("config.json")).unwrap();
        let parsed: GlobalConfig = serde_json::from_str(&content).unwrap();
        assert_eq!(parsed.registry_url.unwrap(), new_url);
    }

    #[test]
    fn test_registry_url_fallback() {
        let env = TestEnv::new();
        let ctx = env.create_context();

        assert_eq!(ctx.registry_url(), DEFAULT_REGISTRY);
    }

    #[test]
    fn test_cache_dir_override() {
        let env = TestEnv::new();
        let custom_cache = env.config_dir.join("custom_cache");

        let config = GlobalConfig {
            cache_dir: Some(custom_cache.clone()),
            ..GlobalConfig::default()
        };

        fs::create_dir_all(&env.config_dir).unwrap();
        fs::write(
            env.config_dir.join("config.json"),
            serde_json::to_string(&config).unwrap(),
        )
        .unwrap();

        let ctx = env.create_context();
        assert_eq!(ctx.cache_dir, custom_cache);
        assert!(custom_cache.exists());
    }
}
