# Planar 🛰️

**Planar** is a semantic intelligence platform and a knowledge base for your configuration files. 

It is an **LSP-first** engine that transforms a fragmented mess of YAML, JSON, KDL, Nginx, Caddy, or any custom configuration files into a unified semantic graph. Planar moves beyond simple "text linting" by understanding the relationships between resources across technology boundaries and projecting diagnostics from external tools directly back onto your source code.

## 💡 The Core Concept

Planar acts as a "Linker" for infrastructure:
1. **Extract**: Pulls data from the CST (via Tree-sitter) using **PlanarDL**. If a Tree-sitter grammar exists for your config, Planar can understand it.
2. **Graph**: Scans the entire repository and ingests the data into a built-in graph database to map connections (e.g., which Nginx upstream points to which K8s service, which sidecar is injected, which variables are substituted).
3. **Analyze**: Performs cross-file semantic analysis and contract testing on the resulting model.
4. **Proxy**: Orchestrates external linters (Trivy, Checkov, Kubeval) against the "effective config" and maps their errors back to the source via **Source Maps**.

---

## 🚀 Key Capabilities

*   **LSP-First**: All intelligence is available directly in your editor. Go-to-Definition, Hover, and Find Usages work across technology boundaries (e.g., jumping from a Caddyfile directive to the Docker container it proxies).
*   **Knowledge Base**: By scanning the entire repository, Planar builds a complete map of your system—from container images to ingress rules—allowing you to query and analyze the state of your infrastructure as a coherent graph.
*   **PlanarDL**: A DSL designed to map syntax (CST) to semantics (Graph). It defines how raw text becomes meaningful nodes and edges.
*   **Smart Proxy**: Planar makes existing linters "smarter." It handles the heavy lifting of merging overlays and remapping linter output (SARIF) back to the exact line in your base or override file.
*   **WASM Extensibility**: Supports WebAssembly modules to implement complex custom logic (like advanced CIDR validation or security policies) in Rust, Go, or C++.

---

## 📋 Roadmap

### Phase 1: Foundation (The Language)
- [ ] **PlanarDL** Parser & Runtime: Defining schemas and extraction rules.
- [ ] Project Manifest (`planar.kdl`): Discovery rules and target definitions.
- [ ] Package System: Loading rules and plugins from external registries/git.

### Phase 2: Semantic Intelligence (The Heart)
- [ ] **Kuzu Integration**: Implementing an embedded graph database to store and query repo-wide knowledge.
- [ ] **Scope Graphs**: Building the cross-file dependency graph by scanning the entire repository.
- [ ] **Cross-Analysis**: Detecting "orphan" resources and broken references between different technologies.
- [ ] **LSP Implementation**: Real-time navigation and hover info based on the global graph.

### Phase 3: Virtualization & Source Mapping
- [ ] **Overlay Engine**: Simulating the final "effective config" (Deep Merge logic).
- [ ] **Lossless Source Maps**: Maintaining a link between virtual documents and physical source lines.
- [ ] Unified Suppression: Global and inline error ignoring (`# planar-ignore`).

### Phase 4: Linter Orchestration (The Proxy)
- [ ] **Smart Proxy Execution**: Running external tools via stdin/stdout.
- [ ] **SARIF Reverse Remapping**: Translating raw linter output back to the user's source code.
- [ ] **Contract Testing**: Snapshoting the graph to detect breaking changes in infrastructure APIs.

---

## 🦀 Technical Stack

*   **Core**: **Rust** for performance and safety.
*   **Query Engine**: **Salsa** for incremental computation and instant LSP feedback.
*   **Parsing**: **Tree-sitter** for high-speed, polyglot CST extraction (supports any language with a grammar).
*   **Knowledge Base**: **Kuzu** — an embeddable graph database (Cypher-powered) for storing repo-wide relationships.
