use std::{path::PathBuf, time::Duration};

use ab_check::check::{BatchOptions, check_single, run_batch, schema_validator};
use anyhow::{Result, bail};
use clap::Parser;

#[derive(Debug, Parser)]
#[command(version, about = "Validate AAT and run parser invariant checks")]
struct Args {
    #[arg(long)]
    txt: Option<PathBuf>,

    #[arg(long)]
    aat: Option<PathBuf>,

    #[arg(long)]
    output: Option<PathBuf>,

    #[arg(long)]
    aat_output: Option<PathBuf>,

    #[arg(long)]
    index: Option<PathBuf>,

    #[arg(long)]
    corpus: Option<PathBuf>,

    #[arg(long, value_delimiter = ',')]
    features: Vec<String>,

    #[arg(long)]
    work_ids: Option<PathBuf>,

    #[arg(long)]
    adapter: Option<String>,

    #[arg(long, default_value_t = 1)]
    jobs: usize,

    #[arg(long, default_value = "60s")]
    per_work_timeout: String,
}

fn main() -> Result<()> {
    let args = Args::parse();
    if let (Some(txt), Some(aat)) = (&args.txt, &args.aat) {
        let validator = schema_validator()?;
        check_single(txt, aat, args.output.as_deref(), validator)?;
        return Ok(());
    }

    if let (Some(index), Some(adapter), Some(output), Some(corpus)) =
        (&args.index, &args.adapter, &args.output, &args.corpus)
    {
        run_batch(BatchOptions {
            index_path: index,
            corpus_root: corpus,
            features: &args.features,
            work_ids_path: args.work_ids.as_deref(),
            adapter,
            output_dir: output,
            aat_output_dir: args.aat_output.as_deref(),
            jobs: args.jobs,
            timeout: parse_duration(&args.per_work_timeout)?,
        })?;
        return Ok(());
    }

    bail!(
        "provide --txt and --aat for single-work checks, or --index/--corpus/--adapter/--output for batch checks"
    )
}

fn parse_duration(value: &str) -> Result<Duration> {
    if let Some(seconds) = value.strip_suffix('s') {
        Ok(Duration::from_secs(seconds.parse()?))
    } else {
        Ok(Duration::from_secs(value.parse()?))
    }
}
