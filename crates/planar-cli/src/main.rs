use clap::{Parser, Subcommand};
use console::{style, Emoji};

use crate::settings::get_registry;

mod setup;
mod settings;
mod init;

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
    /// Check configuration files
    Check {
        #[arg(short, long)]
        all: bool,
    },
    /// Install or update grammars
    Setup {
        #[arg(short, long)]
        force: bool,
    },
    /// Initialize a new Planar project
    Init {
        /// Project name (optional, defaults to current directory)
        name: Option<String>,
    },
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Init { name } => {
            init::run(name)?; 
        }
        Commands::Setup { force } => {
            println!("{} {}Setup Planar environment...", style("planar").bold().cyan(), LOOKING_GLASS);
            setup::run(force, get_registry()?).await?;
        }
        Commands::Check { .. } => {
            println!("Not implemented yet");
        }
    }

    Ok(())
}
