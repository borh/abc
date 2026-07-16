use std::fs;
use std::path::PathBuf;

use ab_parser_rq_source_accountability::{
    CorpusEntry, CorpusInput, CorpusSourceEntry, QualificationIdentity, RecordIndex,
    TaxonomyIdentity, WorkInput, aggregate, analyze_corpus, analyze_work, canonical_json,
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
    /// Analyze one work from immutable campaign capture inputs.
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
    /// Analyze an explicit closed corpus from immutable campaign capture roots.
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
    /// Authenticate a closed work-record index and aggregate exact byte evidence.
    Aggregate {
        #[arg(long)]
        corpus: PathBuf,
        #[arg(long = "work-record-index")]
        work_record_index: PathBuf,
        #[arg(long = "qualification-identity")]
        qualification_identity: PathBuf,
        #[arg(long)]
        taxonomy: PathBuf,
        #[arg(long)]
        out: PathBuf,
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
        Command::Aggregate {
            corpus,
            work_record_index,
            qualification_identity,
            taxonomy: taxonomy_path,
            out,
        } => {
            let corpus: Vec<CorpusEntry> = read_json::<Vec<CorpusSourceEntry>>(&corpus)?
                .into_iter()
                .map(|entry| entry.corpus_entry)
                .collect();
            let index = read_json::<RecordIndex>(&work_record_index)?;
            let records_root = work_record_index
                .parent()
                .ok_or_else(|| anyhow::anyhow!("work record index has no parent directory"))?;
            let identity = read_json::<QualificationIdentity>(&qualification_identity)?;
            let taxonomy = taxonomy(&taxonomy_path)?;
            let result = aggregate(&corpus, &index, records_root, &identity, &taxonomy)?;
            let bytes = canonical_json(&result)?;
            fs::write(out, &bytes)?;
            println!("{bytes}");
        }
    }
    Ok(())
}
