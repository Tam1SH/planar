use std::path::PathBuf;

use anyhow::anyhow;
use clap::{Parser, Subcommand};
use console::{Emoji, style};
use tracing_subscriber::EnvFilter;

mod build;
mod global;
mod init;
mod inspect;
mod settings;

static LOOKING_GLASS: Emoji<'_, '_> = Emoji("🔍 ", "");

#[derive(Parser)]
#[command(name = "planar")]
#[command(about = "Polyglot Semantic Intelligence Platform", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Initialize a new Planar project
    Init {
        /// Project name (optional, defaults to current directory)
        name: Option<String>,
    },

    Build {
        /// Path to the project root
        #[arg(default_value = ".")]
        path: PathBuf,

        /// Verbosity level: -v (DEBUG), -vv (TRACE)
        #[arg(short, long, action = clap::ArgAction::Count)]
        verbose: u8,
    },

    /// Manage global configuration
    Global {
        #[command(subcommand)]
        action: GlobalAction,
    },

    /// Inspect a compiled .pdla artifact
    Inspect {
        /// Path to the .pdla file
        path: PathBuf,
    },
}

#[derive(Subcommand)]
enum GlobalAction {
    /// Set a configuration value (keys: std-path, cache-dir, token)
    Set { key: String, value: String },
    /// List all global configuration values
    List,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Init { name } => {
            init::run(name)?;
        }
        Commands::Build { path, verbose } => {
            init_tracing(verbose);
            build::run(path, verbose > 0)
                .await
                .map_err(|e| anyhow!(e))?;
        }
        Commands::Global { action } => match action {
            GlobalAction::Set { key, value } => global::run_set(key, value)?,
            GlobalAction::List => global::run_list()?,
        },
        Commands::Inspect { path } => {
            inspect::run(path)?;
        }
    }

    Ok(())
}

fn init_tracing(verbosity: u8) {
    if verbosity == 0 {
        return;
    }

    let level_str = match verbosity {
        1 => "debug",
        _ => "trace",
    };

    let filter_str = format!(
        "off,planar={},planarc={},planar_pkg={}",
        level_str, level_str, level_str
    );

    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::new(filter_str))
        .with_file(verbosity > 1)
        .with_line_number(verbosity > 1)
        .with_target(true)
        .init();
}
