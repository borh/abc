use std::fs;
use std::path::PathBuf;

use ab_parser_rq_source_accountability::{
    CorpusInput, CorpusSourceEntry, QualificationIdentity, TaxonomyIdentity, WorkInput,
    analyze_corpus, analyze_work, canonical_json,
};
use anyhow::Result;
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(about = "Parser-RQ source-accountability analyzer")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    AnalyzeWork {
        #[arg(long)]
        original: PathBuf,
        #[arg(long)]
        parser_ir: PathBuf,
        #[arg(long)]
        corpus_entry: PathBuf,
        #[arg(long)]
        qualification: PathBuf,
        #[arg(long)]
        taxonomy: PathBuf,
        #[arg(long)]
        diagnostics_locator: String,
    },
    AnalyzeCorpus {
        #[arg(long)]
        corpus: PathBuf,
        #[arg(long)]
        source_root: PathBuf,
        #[arg(long)]
        parser_ir_root: PathBuf,
        #[arg(long)]
        qualification: PathBuf,
        #[arg(long)]
        taxonomy: PathBuf,
        #[arg(long)]
        store_root: PathBuf,
        #[arg(long)]
        index_out: PathBuf,
    },
}

fn read_json<T: serde::de::DeserializeOwned>(path: &PathBuf) -> Result<T> {
    Ok(serde_json::from_slice(&fs::read(path)?)?)
}

fn taxonomy(path: &PathBuf) -> Result<TaxonomyIdentity> {
    let bytes = fs::read(path)?;
    let value: serde_json::Value = serde_json::from_slice(&bytes)?;
    Ok(TaxonomyIdentity {
        taxonomy_version: serde_json::from_value(
            value
                .get("taxonomy_version")
                .cloned()
                .unwrap_or(serde_json::Value::Null),
        )?,
        taxonomy_hash: format!("sha256:{:x}", sha2::Sha256::digest(&bytes)),
        taxonomy_jcs_bytes: bytes,
    })
}

use sha2::Digest;

fn main() -> Result<()> {
    match Cli::parse().command {
        Command::AnalyzeWork {
            original,
            parser_ir,
            corpus_entry,
            qualification,
            taxonomy: taxonomy_path,
            diagnostics_locator,
        } => {
            let result = analyze_work(WorkInput {
                original_bytes: fs::read(original)?,
                parser_ir_bytes: fs::read(parser_ir)?,
                corpus_entry: read_json(&corpus_entry)?,
                qualification_identity: read_json(&qualification)?,
                taxonomy: taxonomy(&taxonomy_path)?,
                diagnostics_locator,
            });
            println!("{}", canonical_json(&result.record)?);
        }
        Command::AnalyzeCorpus {
            corpus,
            source_root,
            parser_ir_root,
            qualification,
            taxonomy: taxonomy_path,
            store_root,
            index_out,
        } => {
            let index = analyze_corpus(CorpusInput {
                entries: read_json::<Vec<CorpusSourceEntry>>(&corpus)?,
                source_root,
                parser_ir_root,
                store_root,
                index_out,
                qualification_identity: read_json::<QualificationIdentity>(&qualification)?,
                taxonomy: taxonomy(&taxonomy_path)?,
            })?;
            println!("{}", canonical_json(&index)?);
        }
    }
    Ok(())
}
