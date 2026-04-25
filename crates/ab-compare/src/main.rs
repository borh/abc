use std::path::PathBuf;

use ab_compare::compare_report_dirs;
use anyhow::Result;
use clap::Parser;

#[derive(Debug, Parser)]
#[command(version, about = "Compare two ab-check report directories")]
struct Args {
    #[arg(long)]
    reports_a: PathBuf,

    #[arg(long)]
    reports_b: PathBuf,

    #[arg(long)]
    output: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let summary = compare_report_dirs(&args.reports_a, &args.reports_b)?;
    if let Some(parent) = args.output.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let file = std::fs::File::create(args.output)?;
    serde_json::to_writer_pretty(file, &summary)?;
    Ok(())
}
