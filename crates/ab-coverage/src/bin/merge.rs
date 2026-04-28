//! `ab-coverage-merge` — applies classifier or prevalence findings to
//! `data/aozora-syntax-coverage.toml` while preserving comments and
//! ordering.

use std::path::PathBuf;

use ab_coverage::merge::{
    ClassifierFindings, PrevalenceFindings, apply_classifier_findings, apply_prevalence_findings,
};
use anyhow::{Context, Result};
use clap::{Parser, Subcommand};

#[derive(Parser, Debug)]
#[command(
    version,
    about = "Apply classifier/prevalence findings to the syntax coverage matrix."
)]
struct Cli {
    #[arg(long, default_value = "data/aozora-syntax-coverage.toml")]
    matrix: PathBuf,
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand, Debug)]
enum Cmd {
    /// Update parsers.<id> / adapters.<id> sub-tables from a JSON findings file.
    Classifier {
        #[arg(long)]
        findings: PathBuf,
    },
    /// Update corpus_prevalence sub-tables from a JSON findings file.
    Prevalence {
        #[arg(long)]
        findings: PathBuf,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.cmd {
        Cmd::Classifier { findings } => {
            let raw = std::fs::read_to_string(&findings)
                .with_context(|| format!("read {}", findings.display()))?;
            let parsed: ClassifierFindings = serde_json::from_str(&raw)
                .with_context(|| format!("parse {}", findings.display()))?;
            let updated = apply_classifier_findings(&cli.matrix, &parsed)?;
            eprintln!("updated {updated} rows");
        }
        Cmd::Prevalence { findings } => {
            let raw = std::fs::read_to_string(&findings)
                .with_context(|| format!("read {}", findings.display()))?;
            let parsed: PrevalenceFindings = serde_json::from_str(&raw)
                .with_context(|| format!("parse {}", findings.display()))?;
            let updated = apply_prevalence_findings(&cli.matrix, &parsed)?;
            eprintln!("updated {updated} rows");
        }
    }
    Ok(())
}
