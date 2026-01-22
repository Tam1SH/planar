use console::{Emoji, style};
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

use miette::{Context, miette};
use planar_pkg::config::PlanarContext;
use planar_pkg::model::planardl::PackageManifest;
use planar_pkg::packaging::resolver::{DependencyKind, ResolverProgress, WorkspaceResolver};
use planar_pkg::parser::ctx::ParseContext;
use planar_pkg::parser::parsable::KdlParsable;
use planarc::artifact::builder::create_bundle;
use planarc::artifact::writer::write_bundle;
use planarc::compiler::Compiler;
use planarc::module_loader::FsModuleLoader;

static TICK: Emoji<'_, '_> = Emoji("✔ ", "");
static HAMMER: Emoji<'_, '_> = Emoji("🔨 ", "");

pub async fn run(path: PathBuf, is_tracing: bool) -> miette::Result<()> {
    let start_time = Instant::now();
    let ctx = PlanarContext::new();
    let progress = CliProgress::new(is_tracing);

    // 1. Resolve
    let mut resolver = WorkspaceResolver::new(ctx, &progress);
    resolver
        .resolve(path.clone())
        .await
        .map_err(|e| miette!(e))?;

    let root_manifest = load_manifest(&path)?;
    let package_name = root_manifest.package.name.clone();
    let pkg_count = resolver.packages.len();

    progress.main_pb.finish_and_clear();
    println!(
        "{} {}Dependencies resolved",
        style("planar").bold().cyan(),
        TICK
    );

    // 2. Compile
    let roots = resolver.get_roots_for_compiler();
    let compiler = Compiler::new(FsModuleLoader);

    println!(
        "{} {}Compiling {}...",
        style("planar").bold().cyan(),
        HAMMER,
        style(&package_name).bold()
    );

    let result = compiler
        .compile(roots, resolver.grammar_paths)
        .with_context(|| format!("Compilation failed for {}", package_name))?;

    if result.has_errors() {
        for error in &result.errors.0 {
            eprintln!("{:?}", error);
        }
        let error_count = result.errors.0.len();
        eprintln!(
            "\n{} with {} {}",
            style("Build failed").red().bold(),
            style(error_count).red().bold(),
            if error_count == 1 { "error" } else { "errors" }
        );
        std::process::exit(1);
    }

    // 3. Artifact & Size Comparison
    let target_dir = path.join("target");
    let output_path = target_dir.join(format!("{}.pdla", package_name));

    let old_size = fs::metadata(&output_path).map(|m| m.len()).unwrap_or(0);

    let program = create_bundle(result);
    if !target_dir.exists() {
        fs::create_dir_all(&target_dir).map_err(|e| miette!(e))?;
    }

    let mut buffer = Vec::new();
    write_bundle(&program, &mut buffer, None).map_err(|e| miette!(e))?;
    fs::write(&output_path, &buffer).map_err(|e| miette!(e))?;

    let new_size = buffer.len() as u64;
    let duration = start_time.elapsed();

    // 4. Final Output
    print_final_report(
        &package_name,
        pkg_count,
        duration,
        &output_path,
        old_size,
        new_size,
    );

    Ok(())
}

fn print_final_report(
    name: &str,
    pkgs: usize,
    time: std::time::Duration,
    path: &Path,
    old: u64,
    new: u64,
) {
    let pkg_str = format!("{} package{}", pkgs, if pkgs == 1 { "" } else { "s" });

    println!(
        "{} {} {} {} in {:?}",
        style("planar").bold().cyan(),
        TICK,
        style("Finished").green().bold(),
        pkg_str,
        time
    );

    let size_str = format_size(new);
    let diff_str = format_size_diff(old, new);

    println!(
        "  {}  Artifact: {} ({}{})",
        style("➜").dim(),
        style(path.display()).bold(),
        style(size_str).cyan(),
        diff_str
    );
}

fn format_size(bytes: u64) -> String {
    if bytes < 1024 {
        format!("{} B", bytes)
    } else if bytes < 1024 * 1024 {
        format!("{:.1} KB", bytes as f64 / 1024.0)
    } else {
        format!("{:.1} MB", bytes as f64 / (1024.0 * 1024.0))
    }
}

fn format_size_diff(old: u64, new: u64) -> String {
    if old == 0 || old == new {
        return "".to_string();
    }
    if new > old {
        format!(" {})", style(format!("+{}", format_size(new - old))).red())
    } else {
        format!(
            " {})",
            style(format!("-{}", format_size(old - new))).green()
        )
    }
}

impl ResolverProgress for CliProgress {
    fn on_start_resolve(&self, name: &str) {
        self.main_pb
            .set_message(format!("Resolving {}...", style(name).bold()));
    }

    fn on_fetch_start(&self, name: &str, ver: &str, kind: DependencyKind) {
        let label = match kind {
            DependencyKind::Package => style(kind.label()).blue().to_string(),
            DependencyKind::Grammar => style(kind.label()).magenta().to_string(),
        };

        self.multi
            .println(format!(
                "  {} {} {} ({}) ({})",
                style("➜").dim(),
                style("Fetching").blue(),
                name,
                style(ver).dim(),
                label,
            ))
            .unwrap();
    }

    fn on_resolved(&self, name: &str, ver: &str, kind: DependencyKind, is_local: bool) {
        let status = if is_local {
            style("Local").magenta()
        } else {
            style("Cached").green()
        };

        let label = match kind {
            DependencyKind::Package => style(kind.label()).blue().to_string(),
            DependencyKind::Grammar => style(kind.label()).magenta().to_string(),
        };

        self.multi
            .println(format!(
                "  {} {} {} ({}) ({})",
                style("➜").dim(),
                status,
                name,
                style(ver).dim(),
                label,
            ))
            .unwrap();
    }

    fn on_fetch_done(&self, _name: &str) {}

    fn on_error(&self, msg: &str) {
        self.multi
            .println(format!("  {} {}", style("✘").red(), msg))
            .unwrap();
    }
}

fn load_manifest(path: &PathBuf) -> miette::Result<PackageManifest> {
    let file_path = path.join("planar.kdl");
    let content =
        fs::read_to_string(&file_path).map_err(|_| miette!("Missing planar.kdl in {:?}", path))?;
    let doc = content.parse::<kdl::KdlDocument>()?;
    let ctx = ParseContext::new(doc, &file_path.to_string_lossy());
    PackageManifest::parse_node(&ctx, &()).map_err(|e| miette!("Invalid manifest: {:?}", e))
}

struct CliProgress {
    multi: MultiProgress,
    main_pb: ProgressBar,
}

impl CliProgress {
    fn new(is_verbose: bool) -> Self {
        let multi = MultiProgress::new();
        let main_pb = multi.add(ProgressBar::new_spinner());
        main_pb.set_style(
            ProgressStyle::default_spinner()
                .template("{spinner:.green} {msg}")
                .unwrap(),
        );

        if is_verbose {
            main_pb.set_draw_target(indicatif::ProgressDrawTarget::hidden());
        } else {
            main_pb.set_style(
                ProgressStyle::default_spinner()
                    .template("{spinner:.green} {msg}")
                    .unwrap(),
            );
            main_pb.enable_steady_tick(std::time::Duration::from_millis(100));
        }

        Self { multi, main_pb }
    }
}
