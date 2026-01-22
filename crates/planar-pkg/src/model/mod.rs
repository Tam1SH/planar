use planar_config_macro::{NodeSchema, Parser, planar_node};
use std::path::PathBuf;
use strum::{Display, EnumString};

use crate::schema::{definitions::ValueKind, value_info::KdlValueInfo};

pub mod planardl;

// =============================================================================
// ROOT
// =============================================================================

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(root)]
pub struct PlanarConfigDef {
    // project "Name"
    #[node(child)]
    pub project: ProjectDef,

    // coverage { ... }
    #[node(child)]
    pub coverage: Option<CoverageDef>,

    // safety { ... }
    #[node(child)]
    pub safety: Option<SafetyDef>,

    // ignore "pattern" rule="..." (Global ignores)
    #[node(child, name = "ignore")]
    pub global_ignores: Vec<GlobalIgnoreDef>,

    // scope "name" { ... }
    #[node(child)]
    pub scopes: Vec<ScopeDef>,

    // workspace "name" { ... }
    #[node(child)]
    pub workspaces: Vec<WorkspaceDef>,

    // target "name" { ... }
    #[node(child)]
    pub targets: Vec<TargetDef>,
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "project")]
pub struct ProjectDef {
    #[node(arg)]
    pub name: String,
}

// =============================================================================
// COVERAGE & SAFETY
// =============================================================================

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "coverage")]
pub struct CoverageDef {
    // report "warning"
    #[node(child, flat)]
    pub report: ReportLevel,

    // ignore "docs/**" (Simple path ignore inside coverage)
    #[node(child, name = "ignore")]
    pub ignores: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, EnumString, Display)]
#[strum(serialize_all = "kebab-case")]
pub enum ReportLevel {
    Warning,
    Error,
    None,
}

impl KdlValueInfo for ReportLevel {
    fn value_kind() -> ValueKind {
        ValueKind::Enum(vec!["warning".into(), "error".into(), "none".into()])
    }
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "safety")]
pub struct SafetyDef {
    #[node(child)]
    pub contracts: Option<ContractsDef>,

    #[node(child)]
    pub stats: Option<StatsDef>,
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "contracts")]
pub struct ContractsDef {
    #[node(child, flat, name = "enforce-usage")]
    pub enforce_usage: Option<bool>,

    #[node(child, flat, name = "enforce-types")]
    pub enforce_types: Option<bool>,

    #[node(child, flat, name = "allow-breaking-unused")]
    pub allow_breaking_unused: Option<bool>,
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "stats")]
pub struct StatsDef {
    #[node(child, flat, name = "max-orphan-rate")]
    pub max_orphan_rate: Option<f32>,

    #[node(child, flat, name = "max-overlay-depth")]
    pub max_overlay_depth: Option<usize>,
}

// ignore "pattern" rule="rule-id"
#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "ignore")]
pub struct GlobalIgnoreDef {
    #[node(arg)]
    pub pattern: String,

    #[node(prop)]
    pub rule: String,
}

// =============================================================================
// SCOPES (Shared Libs)
// =============================================================================

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "scope")]
pub struct ScopeDef {
    #[node(arg)]
    pub name: String,

    #[node(child, name = "in")]
    pub search_paths: Vec<String>,

    #[node(child, name = "include")]
    pub includes: Vec<IncludeDef>,

    #[node(child)]
    pub resolution: Option<ResolutionDef>,
}

// =============================================================================
// WORKSPACES & TARGETS
// =============================================================================

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "workspace")]
pub struct WorkspaceDef {
    #[node(arg)]
    pub name: String,

    // in "apps/**"
    #[node(child, name = "in")]
    pub search_paths: Vec<String>,

    // anchor "package.json"
    #[node(child, name = "anchor")]
    pub anchors: Vec<String>,

    // ignore "**/node_modules/**"
    #[node(child, name = "ignore")]
    pub ignores: Vec<String>,

    // include "k8s/*.yaml" ...
    #[node(child, name = "include")]
    pub templates: Vec<IncludeDef>,

    #[node(child)]
    pub lint: Option<LintSectionDef>,

    #[node(child)]
    pub resolution: Option<ResolutionDef>,
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "target")]
pub struct TargetDef {
    #[node(arg)]
    pub name: String,

    #[node(child, flat)]
    pub root: Option<PathBuf>,

    #[node(child)]
    pub env: Option<EnvSectionDef>,

    #[node(child, name = "include")]
    pub includes: Vec<IncludeDef>,

    #[node(child)]
    pub resolution: Option<ResolutionDef>,
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "include")]
pub struct IncludeDef {
    #[node(arg)]
    pub pattern: String,

    #[node(prop)]
    pub language: Option<String>,

    #[node(prop)]
    pub role: Option<String>, // "base" | "overlay"
}

// =============================================================================
// RESOLUTION STRATEGY (Complex Polymorphism)
// =============================================================================

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "resolution")]
pub struct ResolutionDef {
    // mode "isolation"
    #[node(child, flat)]
    pub mode: Option<ResolutionMode>,

    // import "shared-infra"
    #[node(child, name = "import")]
    pub imports: Vec<String>,

    // import-peers "microservices" as-namespace=true
    #[node(child, name = "import-peers")]
    pub peer_imports: Vec<ImportPeersDef>,

    // bind-file "..." OR bind-scope "..." (Polymorphic list)
    #[node(child)]
    pub bindings: Vec<BindingDef>,
}

#[derive(Debug, Clone, PartialEq, EnumString, Display)]
#[strum(serialize_all = "kebab-case")]
pub enum ResolutionMode {
    Flat,
    Recursive,
    Isolation,
}

impl KdlValueInfo for ResolutionMode {
    fn value_kind() -> ValueKind {
        ValueKind::Enum(vec!["flat".into(), "recursive".into(), "isolation".into()])
    }
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "import-peers")]
pub struct ImportPeersDef {
    #[node(arg)]
    pub from_workspace: String,

    #[node(prop, name = "as-namespace")]
    pub as_namespace: Option<bool>,
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
pub enum BindingDef {
    #[node(name = "bind-file")]
    File(BindFileDef),
    #[node(name = "bind-scope")]
    Scope(BindScopeDef),
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "bind-file")]
pub struct BindFileDef {
    #[node(arg)]
    pub path: String,
    #[node(prop, name = "as")]
    pub alias: String,
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "bind-scope")]
pub struct BindScopeDef {
    #[node(arg)]
    pub name: String,
    #[node(prop, name = "as")]
    pub alias: String,
}

// =============================================================================
// ENVIRONMENT VARIABLES
// =============================================================================

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "env")]
pub struct EnvSectionDef {
    #[node(child)]
    pub items: Vec<EnvItemDef>,
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
pub enum EnvItemDef {
    #[node(name = "load-file")]
    LoadFile(EnvLoadFileDef),

    #[node(name = "var")]
    Var(EnvVarDef),
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "load-file")]
pub struct EnvLoadFileDef {
    #[node(arg)]
    pub path: String,
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "var")]
pub struct EnvVarDef {
    #[node(arg)]
    pub key: String,
    #[node(arg)]
    pub value: String,
}

// =============================================================================
// LINTERS
// =============================================================================

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "lint")]
pub struct LintSectionDef {
    #[node(child)]
    pub tools: Vec<LintToolDef>,
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "use")]
pub struct LintToolDef {
    #[node(arg)]
    pub tool_name: String,

    #[node(child, name = "arg")]
    pub args: Vec<LintArgDef>,
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(name = "arg")]
pub struct LintArgDef {
    #[node(arg)]
    pub flag: String,

    #[node(arg)]
    pub value: Option<String>,
}
