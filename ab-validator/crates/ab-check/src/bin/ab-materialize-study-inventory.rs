use std::{fs, path::PathBuf};

use ab_check::{check::read_indexed_source_bytes, encoding::hex_sha256};
use anyhow::{Context, Result, bail};
use clap::Parser;
use serde::Serialize;
use serde_json::Value;

#[derive(Parser)]
struct Args {
    #[arg(long)]
    index: PathBuf,
    #[arg(long)]
    corpus: PathBuf,
    #[arg(long)]
    output: PathBuf,
    #[arg(long, default_value_t = 0)]
    limit: usize,
}

#[derive(Serialize)]
struct Inventory {
    items: Vec<Item>,
}

#[derive(Serialize)]
struct Item {
    id: String,
    path: String,
    sha256: String,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let index: Value = serde_json::from_slice(&fs::read(&args.index)?)?;
    let works = index["works"]
        .as_array()
        .context("index works must be an array")?;
    let count = if args.limit == 0 {
        works.len()
    } else {
        args.limit.min(works.len())
    };
    let source_dir = args.output.join("sources");
    fs::create_dir_all(&source_dir)?;
    let mut items = Vec::with_capacity(count);
    for work in &works[..count] {
        let id = work["id"].as_str().context("work id must be a string")?;
        let indexed_path = work["txt_path"]
            .as_str()
            .context("txt_path must be a string")?;
        if id.is_empty() || id.contains(['/', '\\']) || id == "." || id == ".." {
            bail!("unsafe work id {id:?}");
        }
        let bytes = read_indexed_source_bytes(&args.corpus, indexed_path)
            .with_context(|| format!("failed to materialize {id}"))?;
        let item_id = format!("{id}-{}", &hex_sha256(indexed_path.as_bytes())[..12]);
        let relative = format!("{item_id}.txt");
        fs::write(source_dir.join(&relative), &bytes)?;
        items.push(Item {
            id: item_id,
            path: relative,
            sha256: format!("sha256:{}", hex_sha256(&bytes)),
        });
    }
    fs::write(
        args.output.join("inventory.json"),
        serde_json::to_vec(&Inventory { items })?,
    )?;
    Ok(())
}
