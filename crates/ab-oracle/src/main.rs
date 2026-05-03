use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;

#[derive(Debug, Parser)]
struct Args {
    #[arg(long, default_value = "data/aat-oracle-cases.toml")]
    oracle: PathBuf,

    #[arg(long, default_value = "data/aat-upstream-observations.toml")]
    upstream: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let oracle = ab_oracle::data::load_oracle_cases(&args.oracle)?;
    let observations = ab_oracle::data::load_upstream_observations(&args.upstream)?;
    println!(
        "loaded {} oracle cases and {} upstream observations",
        oracle.case.len(),
        observations.observation.len()
    );
    Ok(())
}
