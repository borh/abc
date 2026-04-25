use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
};

use ab_index::{
    features::FeatureDetector,
    index::{build_index, query_all, query_any, read_index, sample, write_index},
};
use anyhow::{Result, bail};
use clap::Parser;

#[derive(Debug, Parser)]
#[command(version, about = "Build and query Aozora Bunko feature indexes")]
struct Args {
    #[arg(long)]
    corpus: Option<PathBuf>,

    #[arg(long)]
    index: Option<PathBuf>,

    #[arg(long)]
    output: Option<PathBuf>,

    #[arg(long, default_value = "data/feature-patterns.toml")]
    patterns: PathBuf,

    #[arg(long)]
    query: Option<String>,

    #[arg(long, value_delimiter = ',')]
    query_all: Option<Vec<String>>,

    #[arg(long)]
    sample: Option<usize>,

    #[arg(long, value_delimiter = ',')]
    features: Vec<String>,
}

fn main() -> Result<()> {
    let args = Args::parse();

    if let Some(corpus) = &args.corpus {
        let detector = FeatureDetector::from_toml(&args.patterns)?;
        let index = build_index(corpus, &detector)?;
        if let Some(output) = &args.output {
            write_index(&index, output)?;
        } else {
            serde_json::to_writer_pretty(io::stdout(), &index)?;
            writeln!(io::stdout())?;
        }
        return Ok(());
    }

    let Some(index_path) = &args.index else {
        bail!("--index is required for query and sample operations");
    };
    let index = read_index(index_path)?;

    let ids = if let Some(feature) = &args.query {
        query_any(&index, std::slice::from_ref(feature))
    } else if let Some(features) = &args.query_all {
        query_all(&index, features)
    } else if let Some(limit) = args.sample {
        if args.features.is_empty() {
            bail!("--features is required with --sample");
        }
        sample(&index, limit, &args.features)
    } else {
        bail!("no operation requested");
    };

    let json = serde_json::to_string_pretty(&ids)?;
    if let Some(output) = &args.output {
        fs::write(output, format!("{json}\n"))?;
    } else {
        println!("{json}");
    }

    Ok(())
}
