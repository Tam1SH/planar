use planar_config_macro::{NodeSchema, Parser, planar_node};

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
#[node(root)]
pub struct PackageManifest {
    #[node(child)]
    pub package: PackageInfo,

    #[node(child)]
    pub dependencies: Option<DependenciesDef>,

    #[node(child)]
    pub grammars: Option<GrammarsDef>,
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
pub struct PackageInfo {
    #[node(child)]
    pub name: String,
    #[node(child)]
    pub version: String,
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema, Default)]
#[node(name = "dependencies")]
pub struct DependenciesDef {
    #[node(dynamic_child)]
    pub items: Vec<DependencyItemDef>,
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
pub struct DependencyItemDef {
    #[node(node_name)]
    pub name: String,

    #[node(prop)]
    pub path: Option<String>,

    #[node(prop)]
    pub git: Option<String>,

    #[node(prop)]
    pub branch: Option<String>,

    #[node(prop)]
    pub tag: Option<String>,
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema, Default)]
#[node(name = "grammars")]
pub struct GrammarsDef {
    #[node(dynamic_child)]
    pub items: Vec<GrammarItemDef>,
}

#[planar_node]
#[derive(Parser, Clone, Debug, NodeSchema)]
pub struct GrammarItemDef {
    #[node(node_name)]
    pub name: String,

    #[node(prop)]
    pub url: Option<String>,

    #[node(prop)]
    pub path: Option<String>,
}
