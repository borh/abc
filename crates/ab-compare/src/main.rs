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

    #[arg(long)]
    metrics_root: Option<PathBuf>,

    #[arg(long)]
    metrics_output: Option<PathBuf>,

    #[arg(long)]
    aats_a: Option<PathBuf>,

    #[arg(long)]
    aats_b: Option<PathBuf>,

    #[arg(long)]
    aat_diff_output: Option<PathBuf>,

    #[arg(long)]
    aat_diff_limit: Option<usize>,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let summary = compare_report_dirs(&args.reports_a, &args.reports_b)?;
    if let Some(parent) = args.output.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let file = std::fs::File::create(args.output)?;
    serde_json::to_writer_pretty(file, &summary)?;
    if let Some(metrics_root) = &args.metrics_root {
        let summary = ab_compare::metrics::summarize_aat_metrics(metrics_root)?;
        if let Some(path) = &args.metrics_output {
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            let file = std::fs::File::create(path)?;
            serde_json::to_writer_pretty(file, &summary)?;
        } else {
            serde_json::to_writer_pretty(std::io::stdout(), &summary)?;
        }
    }
    if let (Some(aats_a), Some(aats_b), Some(output)) =
        (&args.aats_a, &args.aats_b, &args.aat_diff_output)
    {
        let summary =
            ab_compare::aat_diff::compare_aat_dirs_with_limit(aats_a, aats_b, args.aat_diff_limit)?;
        if let Some(parent) = output.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let file = std::fs::File::create(output)?;
        serde_json::to_writer_pretty(file, &summary)?;
    }
    Ok(())
}
