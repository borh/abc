use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, Subcommand};

#[derive(Debug, Parser)]
#[command(version, about = "Run morph analyzers over checked AAT JSON")]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    AnalyzeAat {
        #[arg(long, conflicts_with = "aat_dir")]
        aat: Option<PathBuf>,
        #[arg(long, conflicts_with = "aat")]
        aat_dir: Option<PathBuf>,
        #[arg(long, required = true)]
        analyzer: Vec<String>,
        #[arg(long)]
        analyses_output: PathBuf,
        #[arg(long)]
        comparisons_output: Option<PathBuf>,
        #[arg(long)]
        errors_output: Option<PathBuf>,
        #[arg(long)]
        resume: bool,
    },
}

fn main() -> Result<()> {
    let args = Args::parse();
    match args.command {
        Command::AnalyzeAat {
            aat,
            aat_dir,
            analyzer,
            analyses_output,
            comparisons_output,
            errors_output,
            resume,
        } => ab_morph_run::run_analyze_aat(
            aat.as_deref(),
            aat_dir.as_deref(),
            &analyzer,
            &analyses_output,
            comparisons_output.as_deref(),
            errors_output.as_deref(),
            resume,
        ),
    }
}
