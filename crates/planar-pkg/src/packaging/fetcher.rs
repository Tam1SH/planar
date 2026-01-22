use crate::model::planardl::{DependencyItemDef, DependencyItemDefData, GrammarItemDefData};
use crate::packaging::resolver::{DependencyKind, ResolverProgress};
use crate::packaging::target_info::TargetInfo;
use anyhow::{Context, anyhow};
use futures_util::StreamExt;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;
use tokio::io::AsyncWriteExt;
use tracing::{debug, info, instrument, warn};

pub enum ResolvedSource {
    Local(PathBuf),
    Cached(PathBuf),
}

impl ResolvedSource {
    pub fn path(&self) -> &Path {
        match self {
            ResolvedSource::Local(p) => p,
            ResolvedSource::Cached(p) => p,
        }
    }
}

#[derive(Deserialize, Serialize)]
pub struct RegistryManifest {
    #[serde(default)]
    pub generated_at: Option<String>,
    pub files: HashMap<String, String>,
}

pub struct PackageFetcher {
    cache_root: PathBuf,
}

impl PackageFetcher {
    pub fn new(cache_root: PathBuf) -> Self {
        Self { cache_root }
    }

    #[instrument(skip(self, item, base_path, registry_manifest, progress), fields(grammar = %name))]
    pub async fn fetch_grammar(
        &self,
        name: &str,
        item: &GrammarItemDefData,
        base_path: &Path,
        registry_url: &str,
        registry_manifest: Option<&RegistryManifest>,
        progress: &dyn ResolverProgress,
    ) -> anyhow::Result<PathBuf> {
        if let Some(path_str) = &item.path {
            debug!(path = %path_str, "Using local grammar path");
            progress.on_resolved(name, "local", DependencyKind::Grammar, true);
            return Ok(base_path.join(path_str).canonicalize()?);
        }

        let target_filename = TargetInfo::format_grammar_name(name);
        let dest_path = self.cache_root.join("grammars").join(&target_filename);

        if let Some(url_template) = &item.url {
            let url = self.resolve_url_templates(url_template);
            if !dest_path.exists() {
                info!(url = %url, "Downloading grammar from custom URL");
                self.download_file(&url, &dest_path).await?;
            }
            return Ok(dest_path);
        }

        debug!(registry = %registry_url, "Searching grammar in registry");
        let manifest = match registry_manifest {
            Some(m) => m,
            None => {
                let client = reqwest::Client::new();
                &client
                    .get(format!("{}/manifest.json", registry_url))
                    .send()
                    .await?
                    .json::<RegistryManifest>()
                    .await?
            }
        };

        let expected_hash = manifest
            .files
            .get(&target_filename)
            .ok_or_else(|| anyhow!("Grammar '{}' not found in registry for your platform", name))?;

        let is_valid = if dest_path.exists() {
            if let Some(manifest) = registry_manifest {
                if let Some(expected_hash) = manifest.files.get(&target_filename) {
                    Self::verify_hash(&dest_path, expected_hash)
                } else {
                    false
                }
            } else {
                true
            }
        } else {
            false
        };

        if is_valid {
            progress.on_resolved(name, "cached", DependencyKind::Grammar, false);
            return Ok(dest_path);
        }

        info!(grammar = %name, "Downloading grammar from registry");
        let url = format!("{}/{}", registry_url, target_filename);
        self.download_file(&url, &dest_path).await?;

        if !Self::verify_hash(&dest_path, expected_hash) {
            let _ = tokio::fs::remove_file(&dest_path).await;
            return Err(anyhow!("Hash mismatch for grammar {}", name));
        }

        Ok(dest_path)
    }

    #[instrument(skip(self, base_path, progress), fields(dep = %dep.name))]
    pub fn fetch(
        &self,
        dep: &DependencyItemDefData,
        base_path: &Path,
        progress: &dyn ResolverProgress,
    ) -> anyhow::Result<ResolvedSource> {
        if let Some(path_str) = &dep.path {
            let path = base_path.join(path_str).canonicalize()?;
            progress.on_resolved(&dep.name, "local", DependencyKind::Package, true);
            return Ok(ResolvedSource::Local(path));
        }

        if let Some(url) = &dep.git {
            let rev = dep
                .tag
                .as_deref()
                .or(dep.branch.as_deref())
                .unwrap_or("main");

            let sanitized_url = url
                .replace("https://", "")
                .replace("://", "/")
                .replace(":", "/");
            let target_dir = self.cache_root.join(sanitized_url).join(rev);

            if target_dir.exists() {
                progress.on_resolved(&dep.name, rev, DependencyKind::Package, false);
                return Ok(ResolvedSource::Cached(target_dir));
            }

            progress.on_fetch_start(&dep.name, rev, DependencyKind::Package);
            self.git_clone(url, rev, &target_dir)?;
            progress.on_fetch_done(&dep.name);

            return Ok(ResolvedSource::Cached(target_dir));
        }

        Err(anyhow!("No source for {}", dep.name))
    }

    fn resolve_url_templates(&self, template: &str) -> String {
        template
            .replace("{os}", TargetInfo::os())
            .replace("{arch}", TargetInfo::arch())
            .replace("{ext}", TargetInfo::ext())
    }

    #[instrument(skip(self, url, dest))]
    async fn download_file(&self, url: &str, dest: &Path) -> anyhow::Result<()> {
        debug!(from = %url, to = ?dest, "Starting file download");

        if let Some(parent) = dest.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        let response = reqwest::get(url)
            .await?
            .error_for_status()
            .context(format!("Failed to request grammar from {}", url))?;

        let mut file = tokio::fs::File::create(dest).await?;
        let mut stream = response.bytes_stream();

        while let Some(chunk) = stream.next().await {
            let data = chunk.context("Error while downloading byte stream")?;
            file.write_all(&data).await?;
        }

        file.flush().await?;
        Ok(())
    }

    fn verify_hash(path: &Path, expected_hash: &str) -> bool {
        let mut file = std::fs::File::open(path).ok().unwrap();
        let mut hasher = Sha256::new();
        std::io::copy(&mut file, &mut hasher).unwrap();
        hex::encode(hasher.finalize()) == expected_hash
    }

    #[instrument(skip(self, url, rev, dest))]
    fn git_clone(&self, url: &str, rev: &str, dest: &Path) -> anyhow::Result<()> {
        debug!("Executing git clone");
        std::fs::create_dir_all(dest.parent().ok_or(anyhow!("Invalid cache path"))?)?;

        let status = Command::new("git")
            .args(["clone", "--depth", "1", "--branch", rev, url])
            .arg(dest)
            .status()
            .context("Failed to execute git command. Is git installed?")?;

        if status.success() {
            Ok(())
        } else {
            let _ = std::fs::remove_dir_all(dest);
            Err(anyhow!("Git clone failed for {}", url))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::packaging::resolver::NoOpProgress;

    use super::*;
    use std::fs;
    use tempfile::TempDir;
    use wiremock::matchers::{method, path};
    use wiremock::{Mock, MockServer, ResponseTemplate};

    fn setup_test_git_repo(dir: &Path) {
        let run = |args: &[&str]| {
            let status = Command::new("git")
                .args(args)
                .current_dir(dir)
                .status()
                .unwrap();
            assert!(status.success());
        };

        run(&["init"]);
        run(&["config", "user.email", "test@planar.lang"]);
        run(&["config", "user.name", "Test Runner"]);

        fs::write(dir.join("planar.kdl"), "package { name \"my-lib\" }").unwrap();

        run(&["add", "."]);
        run(&["commit", "-m", "initial commit"]);
        run(&["tag", "v0.1.0"]);
    }

    #[tokio::test]
    async fn test_git_fetch_vendor_agnostic() {
        let tmp = TempDir::new().unwrap();

        let remote_dir = tmp.path().join("remote_server/repo");
        fs::create_dir_all(&remote_dir).unwrap();
        setup_test_git_repo(&remote_dir);

        let remote_url = format!("file://{}", remote_dir.to_str().unwrap());

        let cache_dir = tmp.path().join("cache");
        let fetcher = PackageFetcher::new(cache_dir);

        let dep = DependencyItemDefData {
            name: "my-lib".to_string(),
            path: None,
            git: Some(remote_url),
            branch: None,
            tag: Some("v0.1.0".to_string()),
        };

        let result = fetcher
            .fetch(&dep, tmp.path(), &NoOpProgress)
            .expect("Should fetch from local file:// url");

        assert!(result.path().exists());
        assert!(result.path().join("planar.kdl").exists());
        assert!(matches!(result, ResolvedSource::Cached(_)));

        let path_str = result.path().to_string_lossy();
        assert!(path_str.contains("remote_server/repo/v0.1.0"));
    }

    #[tokio::test]
    async fn test_fetch_grammar_full_flow() {
        let tmp = TempDir::new().unwrap();
        let server = MockServer::start().await;
        let fetcher = PackageFetcher::new(tmp.path().join("cache"));

        let grammar_name = "rust";
        let binary_data = b"precompiled-binary-content".to_vec();
        let hash = hex::encode(sha2::Sha256::digest(&binary_data));
        let filename = TargetInfo::format_grammar_name(grammar_name);

        // Mock Manifest
        Mock::given(method("GET"))
            .and(path("/manifest.json"))
            .respond_with(ResponseTemplate::new(200).set_body_json(RegistryManifest {
                generated_at: Default::default(),
                files: [(filename.clone(), hash)].into_iter().collect(),
            }))
            .mount(&server)
            .await;

        // Mock Binary
        Mock::given(method("GET"))
            .and(path(format!("/{}", filename)))
            .respond_with(
                ResponseTemplate::new(200).set_body_raw(binary_data, "application/octet-stream"),
            )
            .mount(&server)
            .await;

        let item = GrammarItemDefData {
            name: grammar_name.to_string(),
            path: None,
            url: None,
        };

        let path = fetcher
            .fetch_grammar(
                grammar_name,
                &item,
                tmp.path(),
                &server.uri(),
                None,
                &NoOpProgress,
            )
            .await
            .expect("Failed to fetch grammar");

        assert!(path.exists());
        assert_eq!(fs::read(path).unwrap(), b"precompiled-binary-content");
    }

    #[tokio::test]
    async fn test_url_template_resolution() {
        let tmp = TempDir::new().unwrap();
        let fetcher = PackageFetcher::new(tmp.path().join("cache"));

        let template = "https://example.com/{os}/{arch}/parser.{ext}";
        let resolved = fetcher.resolve_url_templates(template);

        assert!(resolved.contains(TargetInfo::os()));
        assert!(resolved.contains(TargetInfo::arch()));
        assert!(resolved.contains(TargetInfo::ext()));
        assert!(!resolved.contains("{os}"));
    }
}
