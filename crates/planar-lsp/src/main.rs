use dashmap::DashMap;
use planar_pkg::config::PlanarContext;
use planar_pkg::packaging::resolver::{NoOpProgress, WorkspaceResolver};
use planarc::compiler::error::DiagnosticWithLocation;
use planarc::compiler::{CompilationResult, Compiler};
use planarc::linker;
use planarc::module_loader::PackageRoot;
use std::cmp::Reverse;
use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tree_sitter::StreamingIterator;
use tree_sitter::{Parser, Query, QueryCursor};
use tree_sitter_planardl::LANGUAGE;

mod loader;

use tower_lsp::lsp_types as lsp;

use crate::loader::LspModuleLoader;

fn map_to_lsp(err: &dyn DiagnosticWithLocation) -> lsp::Diagnostic {
    let loc = err.location();
    let span = loc.span;

    let range = lsp::Range {
        start: lsp::Position::new(span.line.saturating_sub(1), span.col.saturating_sub(1)),
        end: lsp::Position::new(span.line_end, span.col_end),
    };

    lsp::Diagnostic {
        range,
        severity: Some(lsp::DiagnosticSeverity::ERROR),
        code: err
            .code()
            .map(|c| lsp::NumberOrString::String(c.to_string())),
        source: Some("planar".to_string()),
        message: err.to_string(),
        ..Default::default()
    }
}

const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,        // 0
    SemanticTokenType::TYPE,           // 1
    SemanticTokenType::VARIABLE,       // 2
    SemanticTokenType::FUNCTION,       // 3
    SemanticTokenType::STRING,         // 4
    SemanticTokenType::NUMBER,         // 5
    SemanticTokenType::OPERATOR,       // 6
    SemanticTokenType::PARAMETER,      // 7
    SemanticTokenType::PROPERTY,       // 8
    SemanticTokenType::DECORATOR,      // 9
    SemanticTokenType::new("storage"), // 10
];

fn get_token_index(capture_name: &str) -> Option<u32> {
    match capture_name {
        "keyword" | "storage" => Some(0),
        "type" | "module" => Some(1),
        "variable" | "variable.builtin" | "label" => Some(2),
        "function" | "function.call" => Some(3),
        "string" | "string.special" => Some(4),
        "number" => Some(5),
        "operator" | "punctuation.delimiter" | "punctuation.bracket" => Some(6),
        "parameter" => Some(7),
        "property" => Some(8),
        "attribute" => Some(9),
        _ => None,
    }
}

#[derive(Clone)]
struct Document {
    tree: tree_sitter::Tree,
    source: String,
}

struct Backend {
    client: Client,
    documents: DashMap<String, Document>,
    query: Query,
    last_compilation: Arc<RwLock<Option<Arc<CompilationResult>>>>,
}

impl Backend {
    fn new(client: Client) -> Self {
        let highlights_scm = include_str!("../../planarc/tree-sitter-pdl/queries/highlights.scm");
        let lang = LANGUAGE.into();
        let query = Query::new(&lang, highlights_scm)
            .expect("Failed to parse tree-sitter highlights query");

        Self {
            client,
            documents: DashMap::new(),
            query,
            last_compilation: Arc::new(RwLock::new(None)),
        }
    }

    fn parse_text(&self, uri: &str, text: &str) {
        let mut parser = Parser::new();
        parser.set_language(&LANGUAGE.into()).unwrap();
        if let Some(tree) = parser.parse(text, None) {
            self.documents.insert(
                uri.to_string(),
                Document {
                    tree,
                    source: text.to_string(),
                },
            );
        }
    }

    async fn compile_and_report(&self, current_uri: &str) {
        let uri = Url::parse(current_uri).unwrap();
        let file_path = uri.to_file_path().expect("Invalid file path");

        let project_root =
            find_project_root(&file_path).unwrap_or_else(|| std::env::current_dir().unwrap());

        let planar_ctx = PlanarContext::new();
        let mut resolver = WorkspaceResolver::new(planar_ctx, &NoOpProgress);

        if let Err(e) = resolver.resolve(project_root.clone()).await {
            self.client
                .log_message(MessageType::ERROR, format!("Resolution failed: {}", e))
                .await;
            return;
        }

        let roots = resolver.get_roots_for_compiler();
        let loader = LspModuleLoader::new(self.documents.clone());
        let compiler = Compiler::new(loader);

        match compiler.compile(roots, resolver.grammar_paths) {
            Ok(result) => {
                let mut diagnostics_by_file: HashMap<String, Vec<lsp::Diagnostic>> = HashMap::new();

                for err in &result.errors.0 {
                    let loc = err.location();
                    if let Some(source) = result.registry.get(loc.file_id) {
                        if let Ok(url) = Url::from_file_path(&source.origin) {
                            let lsp_diag = map_to_lsp(err.as_ref());
                            diagnostics_by_file
                                .entry(url.to_string())
                                .or_default()
                                .push(lsp_diag);
                        }
                    }
                }

                if !diagnostics_by_file.contains_key(current_uri) {
                    diagnostics_by_file.insert(current_uri.to_string(), Vec::new());
                }

                for (uri_str, diags) in diagnostics_by_file {
                    if let Ok(uri) = Url::parse(&uri_str) {
                        self.client.publish_diagnostics(uri, diags, None).await;
                    }
                }

                let mut last = self.last_compilation.write().await;
                *last = Some(Arc::new(result));
            }
            Err(e) => {
                self.client
                    .log_message(MessageType::ERROR, format!("Compiler crashed: {}", e))
                    .await;
            }
        }
    }
}

fn find_project_root(start_path: &Path) -> Option<PathBuf> {
    let mut current = start_path.to_path_buf();
    loop {
        if current.join("planar.kdl").exists() {
            return Some(current);
        }
        if !current.pop() {
            break;
        }
    }
    None
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types: TOKEN_TYPES.to_vec(),
                                token_modifiers: vec![],
                            },
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: Some(false),
                            ..Default::default()
                        },
                    ),
                ),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "did_open").await;
        let uri = params.text_document.uri.as_str();
        self.parse_text(uri, &params.text_document.text);
        self.compile_and_report(uri).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "did_save").await;
        let uri = params.text_document.uri.as_str();
        if let Some(text) = params.text {
            self.parse_text(uri, &text);
        }
        self.compile_and_report(uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "did_change")
            .await;
        if let Some(change) = params.content_changes.first() {
            let uri = params.text_document.uri.as_str();
            self.parse_text(uri, &change.text);
            self.compile_and_report(uri).await;
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        self.client.log_message(MessageType::INFO, "lol").await;

        let uri = params.text_document.uri.as_str();
        let doc = match self.documents.get(uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let mut cursor = QueryCursor::new();
        let mut raw_tokens = Vec::new();

        let comp_guard = self.last_compilation.read().await;

        {
            let mut matches =
                cursor.matches(&self.query, doc.tree.root_node(), doc.source.as_bytes());
            while let Some(m) = matches.next() {
                for capture in m.captures {
                    let capture_name = self.query.capture_names()[capture.index as usize];

                    if let Some(token_type) = get_token_index(capture_name) {
                        let range = capture.node.range();

                        if range.start_point.row == range.end_point.row {
                            raw_tokens.push(RawToken {
                                line: range.start_point.row as u32,
                                start: range.start_point.column as u32,
                                length: (range.end_point.column - range.start_point.column) as u32,
                                token_type,
                                priority: 0,
                            });
                        }
                    }
                }
            }
        }

        self.client.log_message(MessageType::INFO, "lol 4").await;

        self.client
            .log_message(
                MessageType::INFO,
                format!("is_some: {}", comp_guard.as_ref().is_some()),
            )
            .await;

        if let Some(res) = comp_guard.as_ref() {
            self.client
                .log_message(
                    MessageType::INFO,
                    format!("symbols count: {}", res.symbol_table.symbols.len()),
                )
                .await;
            for (fqmn, metadata) in &res.symbol_table.symbols {
                self.client
                    .log_message(
                        MessageType::INFO,
                        format!(
                            "Checking symbol file: {} == {:?}",
                            fqmn,
                            res.registry.get(metadata.location.file_id)
                        ),
                    )
                    .await;

                if let Some(source) = res.registry.get(metadata.location.file_id) {
                    if let Ok(file_url) = Url::from_file_path(&source.origin) {
                        if file_url.as_str() == uri {
                            let span = metadata.location.span;

                            let token_type = match metadata.kind {
                                linker::ids::SymbolKind::ExternFunction => Some(3),
                                linker::ids::SymbolKind::Type => Some(1),
                                _ => None,
                            };

                            if let Some(ty) = token_type {
                                raw_tokens.push(RawToken {
                                    line: span.line.saturating_sub(1),
                                    start: span.col.saturating_sub(1),
                                    length: (span.end - span.start) as u32,
                                    token_type: ty,
                                    priority: 1,
                                });
                            }
                        }
                    }
                }
            }
        }

        drop(comp_guard);

        raw_tokens.sort_by_key(|t| (t.line, t.start, Reverse(t.priority)));
        raw_tokens.dedup_by(|a, b| a.line == b.line && a.start == b.start);

        let mut last_line = 0;
        let mut last_start = 0;
        let semantic_tokens = raw_tokens
            .iter()
            .map(|t| {
                let delta_line = t.line - last_line;
                let delta_start = if delta_line == 0 {
                    t.start - last_start
                } else {
                    t.start
                };

                last_line = t.line;
                last_start = t.start;

                SemanticToken {
                    delta_line,
                    delta_start,
                    length: t.length,
                    token_type: t.token_type,
                    token_modifiers_bitset: 0,
                }
            })
            .collect();

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        })))
    }
}

#[derive(Debug)]
struct RawToken {
    line: u32,
    start: u32,
    length: u32,
    token_type: u32,
    priority: u8,
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}
