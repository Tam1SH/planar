use crate::config::PlanarContext;
use crate::model::planardl::{DependencyItemDef, DependencyItemDefData, PackageManifest};
use crate::packaging::fetcher::{PackageFetcher, RegistryManifest};
use crate::parser::ctx::ParseContext;
use crate::parser::parsable::KdlParsable;
use anyhow::{Context, Result, anyhow};
use kdl::KdlDocument;
use petgraph::graph::{DiGraph, NodeIndex};
use planarc::module_loader::PackageRoot;
use std::collections::{BTreeMap, VecDeque};
use std::path::PathBuf;
use tracing::{Instrument, debug, info, instrument};

const MANIFEST_NAME: &str = "planar.kdl";
const STD_REPO: &str = "planar/planardl-std";
const COMPILER_VERSION: &str = env!("CARGO_PKG_VERSION");

pub enum DependencyKind {
    Package,
    Grammar,
}

impl DependencyKind {
    pub fn label(&self) -> String {
        match self {
            DependencyKind::Package => "package".to_string(),
            DependencyKind::Grammar => "grammar".to_string(),
        }
    }
}

pub trait ResolverProgress: Send + Sync {
    fn on_start_resolve(&self, root_name: &str);
    fn on_fetch_start(&self, name: &str, ver: &str, kind: DependencyKind);
    fn on_fetch_done(&self, name: &str);
    fn on_error(&self, msg: &str);
    fn on_resolved(&self, name: &str, ver: &str, kind: DependencyKind, is_local: bool);
}

pub struct NoOpProgress;
impl ResolverProgress for NoOpProgress {
    fn on_start_resolve(&self, _: &str) {}
    fn on_fetch_start(&self, _: &str, _: &str, _: DependencyKind) {}
    fn on_fetch_done(&self, _: &str) {}
    fn on_error(&self, _: &str) {}
    fn on_resolved(&self, _: &str, _: &str, _: DependencyKind, _: bool) {}
}

pub struct WorkspaceResolver<'a> {
    context: PlanarContext,
    fetcher: PackageFetcher,
    registry_manifest: Option<RegistryManifest>,

    progress: &'a dyn ResolverProgress,

    pub packages: BTreeMap<String, ResolvedPackage>,
    pub grammar_paths: BTreeMap<String, PathBuf>,
    pub graph: DiGraph<String, ()>,
}

#[derive(Debug)]
pub struct ResolvedPackage {
    pub name: String,
    pub root_path: PathBuf,
    pub manifest: PackageManifest,
    pub graph_idx: NodeIndex,
}

impl<'a> WorkspaceResolver<'a> {
    pub fn new(ctx: PlanarContext, progress: &'a dyn ResolverProgress) -> Self {
        Self {
            fetcher: PackageFetcher::new(ctx.cache_dir.clone()),
            context: ctx,
            progress,
            packages: BTreeMap::new(),
            registry_manifest: None,
            grammar_paths: BTreeMap::new(),
            graph: DiGraph::new(),
        }
    }

    pub fn get_roots_for_compiler(&self) -> Vec<PackageRoot> {
        self.packages
            .values()
            .map(|pkg| PackageRoot {
                name: pkg.name.clone(),
                path: pkg.root_path.clone().join("src"),
            })
            .collect()
    }

    #[instrument(skip(self, root_path))]
    pub async fn resolve(&mut self, root_path: PathBuf) -> Result<()> {
        info!(root = ?root_path, "Starting workspace resolution");
        let root_manifest = self.load_manifest(&root_path)?;
        let root_name = root_manifest.package.name.clone();

        self.progress.on_start_resolve(&root_name);

        let root_idx = self.graph.add_node(root_name.clone());
        self.packages.insert(
            root_name.clone(),
            ResolvedPackage {
                name: root_name.clone(),
                root_path: root_path.clone(),
                manifest: root_manifest,
                graph_idx: root_idx,
            },
        );

        let mut queue = VecDeque::new();
        queue.push_back(root_name);

        while let Some(current_name) = queue.pop_front() {
            let span = tracing::info_span!("resolve_package", package = %current_name);

            async {
                debug!("Processing dependencies");

                let (manifest, base_path, current_idx) = {
                    let pkg = &self.packages[&current_name];
                    (pkg.manifest.clone(), pkg.root_path.clone(), pkg.graph_idx)
                };

                if let Some(grammars_def) = &manifest.grammars {
                    debug!(count = grammars_def.items.len(), "Processing grammars");

                    if self.registry_manifest.is_none() {
                        let url = self.context.registry_url().trim_end_matches('/');
                        let manifest_url = format!("{}/manifest.json", url);

                        let response = reqwest::get(&manifest_url).await.with_context(|| {
                            format!(
                                "Failed to fetch grammar registry manifest from {}",
                                manifest_url
                            )
                        })?;

                        let manifest =
                            response.json::<RegistryManifest>().await.with_context(|| {
                                format!(
                                    "Failed to parse registry manifest from {}.\n\
                                Make sure the registry URL is correct and returns valid JSON.",
                                    manifest_url
                                )
                            })?;

                        self.registry_manifest = Some(manifest);
                    }

                    for grammar_item in &grammars_def.items {
                        let path = self
                            .fetcher
                            .fetch_grammar(
                                &grammar_item.name,
                                grammar_item,
                                &base_path,
                                self.context.registry_url(),
                                self.registry_manifest.as_ref(),
                                self.progress,
                            )
                            .await?;

                        self.grammar_paths.insert(grammar_item.name.clone(), path);
                    }
                }

                let mut deps = manifest
                    .into_inner()
                    .dependencies
                    .map(|d| {
                        d.into_inner()
                            .items
                            .into_iter()
                            .map(|i| i.into_inner())
                            .collect::<Vec<_>>()
                    })
                    .unwrap_or_default();

                if current_name != "std" && !deps.iter().any(|d| d.name == "std") {
                    let std_dep = self.get_std_dependency();
                    deps.push(std_dep);
                }

                for dep_item in deps {
                    debug!(dependency = %dep_item.name, "Discovered dependency");
                    let display_ver = dep_item.tag.as_deref().unwrap_or("latest");

                    if let Some(existing) = self.packages.get(&dep_item.name) {
                        self.graph.update_edge(current_idx, existing.graph_idx, ());
                        continue;
                    }

                    self.progress.on_fetch_start(
                        &dep_item.name,
                        display_ver,
                        DependencyKind::Package,
                    );
                    let source = self.fetcher.fetch(&dep_item, &base_path, self.progress)?;
                    self.progress.on_fetch_done(&dep_item.name);

                    let dep_path = source.path().to_path_buf();
                    let dep_manifest = self.load_manifest(&dep_path)?;
                    let real_name = dep_manifest.package.name.clone();

                    // TODO: check dep_item.name == real_name

                    let dep_idx = self.graph.add_node(real_name.clone());
                    self.packages.insert(
                        real_name.clone(),
                        ResolvedPackage {
                            name: real_name.clone(),
                            root_path: dep_path,
                            manifest: dep_manifest,
                            graph_idx: dep_idx,
                        },
                    );

                    self.graph.update_edge(current_idx, dep_idx, ());
                    queue.push_back(real_name);
                }

                Ok::<(), anyhow::Error>(())
            }
            .instrument(span)
            .await?;
        }

        info!(
            packages = self.packages.len(),
            grammars = self.grammar_paths.len(),
            "Resolution completed"
        );

        Ok(())
    }

    fn get_std_dependency(&self) -> DependencyItemDefData {
        if let Some(local_path) = &self.context.config.std_override_path {
            DependencyItemDefData {
                name: "std".to_string(),
                path: Some(local_path.to_string_lossy().to_string()),
                git: None,
                branch: None,
                tag: None,
            }
        } else {
            DependencyItemDefData {
                name: "std".to_string(),
                path: None,
                git: Some(STD_REPO.to_string()),
                tag: Some(format!("v{}", COMPILER_VERSION)),
                branch: None,
            }
        }
    }

    fn load_manifest(&self, path: &PathBuf) -> Result<PackageManifest> {
        let file_path = path.join(MANIFEST_NAME);
        let content = std::fs::read_to_string(&file_path)
            .map_err(|_| anyhow!("Missing {}", MANIFEST_NAME))?
            .parse::<KdlDocument>()
            .map_err(|e| anyhow!("Failed to parse {:?}: {:?}", file_path, e))?;

        let ctx = ParseContext::new(content, &path.to_string_lossy());

        PackageManifest::parse_node(&ctx, &())
            .map_err(|e| anyhow!("Failed to parse {:?}: {:?}", file_path, e))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::GlobalConfig;
    use crate::packaging::target_info::TargetInfo;
    use sha2::Digest;
    use std::fs;
    use tempfile::TempDir;
    use wiremock::matchers::{method, path};
    use wiremock::{Mock, MockServer, ResponseTemplate};

    struct TestWorld {
        root: TempDir,
        cache_dir: PathBuf,
        std_path: PathBuf,
        server: MockServer,
    }

    impl TestWorld {
        async fn new() -> Self {
            let root = TempDir::new().expect("Failed to create temp dir");
            let cache_dir = root.path().join("cache");
            let std_path = root.path().join("std");
            let server = MockServer::start().await;

            fs::create_dir_all(&cache_dir).unwrap();

            let world = Self {
                root,
                cache_dir,
                std_path,
                server,
            };
            world.create_package("std", None, None, None);
            world
        }

        fn create_package(
            &self,
            name: &str,
            deps: Option<Vec<(&str, &str)>>,
            grammars: Option<Vec<(&str, Option<&str>)>>,
            custom_dir: Option<&str>,
        ) -> PathBuf {
            let dir_name = custom_dir.unwrap_or(name);
            let pkg_path = self.root.path().join(dir_name);
            fs::create_dir_all(&pkg_path).unwrap();

            let mut kdl = format!(
                r#"
                package {{
                    name "{}"
                    version "0.1.0"
                }}
            "#,
                name
            );

            if let Some(dependencies) = deps {
                kdl.push_str("dependencies {\n");
                for (dep_name, dep_path) in dependencies {
                    kdl.push_str(&format!(
                        r#"    "{}" path="{}"{}"#,
                        dep_name, dep_path, "\n"
                    ));
                }
                kdl.push_str("}\n");
            }

            if let Some(grammars_list) = grammars {
                kdl.push_str("grammars {\n");
                for (g_name, g_path) in grammars_list {
                    if let Some(p) = g_path {
                        kdl.push_str(&format!(r#"    {} path="{}"{}"#, g_name, p, "\n"));
                    } else {
                        kdl.push_str(&format!("    {}\n", g_name));
                    }
                }
                kdl.push_str("}\n");
            }

            fs::write(pkg_path.join("planar.kdl"), kdl).unwrap();
            pkg_path
        }

        fn context(&self) -> PlanarContext {
            PlanarContext {
                config: GlobalConfig {
                    cache_dir: Some(self.cache_dir.clone()),
                    std_override_path: Some(self.std_path.clone()),
                    registry_url: Some(self.server.uri()),
                },
                cache_dir: self.cache_dir.clone(),
                config_dir: self.root.path().join("config"),
            }
        }
    }

    #[tokio::test]
    async fn test_resolve_with_grammars() {
        let world = TestWorld::new().await;

        let grammar_name = "json";
        let filename = TargetInfo::format_grammar_name(grammar_name);
        let dummy_content = vec![1, 2, 3, 4];
        let expected_hash = hex::encode(sha2::Sha256::digest(&dummy_content));
        let filename_ = filename.clone();

        let manifest_json = serde_json::json!({
            "files": {
                filename: expected_hash
            }
        });

        Mock::given(method("GET"))
            .and(path("/manifest.json"))
            .respond_with(ResponseTemplate::new(200).set_body_json(manifest_json))
            .mount(&world.server)
            .await;

        Mock::given(method("GET"))
            .and(path(format!("/{}", filename_)))
            .respond_with(
                ResponseTemplate::new(200).set_body_raw(dummy_content, "application/octet-stream"),
            )
            .mount(&world.server)
            .await;

        let local_grammar_path = world.root.path().join("my_local_grammar.so");
        fs::write(&local_grammar_path, "local-binary-data").unwrap();

        let root_path = world.create_package(
            "app",
            None,
            Some(vec![
                ("json", None),
                ("custom", Some("../my_local_grammar.so")),
            ]),
            None,
        );

        let mut resolver = WorkspaceResolver::new(world.context(), &NoOpProgress);
        resolver
            .resolve(root_path)
            .await
            .expect("Resolution failed");

        assert!(resolver.grammar_paths.contains_key("json"));
        assert!(resolver.grammar_paths.contains_key("custom"));

        let cached_path = &resolver.grammar_paths["json"];
        assert!(cached_path.exists());
        assert!(cached_path.to_string_lossy().contains("cache/grammars"));

        let local_path = &resolver.grammar_paths["custom"];
        assert_eq!(
            local_path.canonicalize().unwrap(),
            local_grammar_path.canonicalize().unwrap()
        );
    }

    #[tokio::test]
    async fn test_diamond_dependency_async() {
        let world = TestWorld::new().await;

        world.create_package("shared_d", None, None, None);
        world.create_package("lib_b", Some(vec![("shared_d", "../shared_d")]), None, None);
        world.create_package("lib_c", Some(vec![("shared_d", "../shared_d")]), None, None);
        let root_path = world.create_package(
            "root",
            Some(vec![("lib_b", "../lib_b"), ("lib_c", "../lib_c")]),
            None,
            None,
        );

        let mut resolver = WorkspaceResolver::new(world.context(), &NoOpProgress);
        resolver.resolve(root_path).await.unwrap();

        assert_eq!(resolver.packages.len(), 5); // root, b, c, d, std
    }

    #[tokio::test]
    async fn test_circular_dependency_handling_async() {
        let world = TestWorld::new().await;

        world.create_package("pkg_b", Some(vec![("pkg_a", "../pkg_a")]), None, None);
        let root_path =
            world.create_package("pkg_a", Some(vec![("pkg_b", "../pkg_b")]), None, None);

        let mut resolver = WorkspaceResolver::new(world.context(), &NoOpProgress);
        resolver.resolve(root_path).await.unwrap();

        assert!(petgraph::algo::is_cyclic_directed(&resolver.graph));
    }
}
