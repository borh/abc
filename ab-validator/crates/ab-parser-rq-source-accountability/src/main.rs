use std::fs;
use std::path::PathBuf;

use ab_parser_rq_source_accountability::{
    CorpusEntry, CorpusInput, CorpusSourceEntry, QualificationIdentity, RecognitionCorpusInput,
    RecognitionGenerationIndex, RecognitionIndex, RecordIndex, TaxonomyIdentity, WorkInput,
    aggregate, aggregate_recognition, analyze_corpus, analyze_recognition_corpus, analyze_work,
    canonical_json,
};
use ab_rq_artifact_store::write_atomic_summary;
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
        source: PathBuf,
        #[arg(long)]
        parser_ir: PathBuf,
        #[arg(long)]
        corpus_entry: PathBuf,
        #[arg(long)]
        qualification_identity: PathBuf,
        #[arg(long)]
        taxonomy: PathBuf,
        #[arg(long)]
        diagnostics_locator: String,
        #[arg(long)]
        out: PathBuf,
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
        store_root: PathBuf,
        #[arg(long)]
        out: PathBuf,
    },
    /// Derive one authenticated recognition record for every exact P1 member.
    AnalyzeRecognitionCorpus {
        #[arg(long)]
        membership_index: PathBuf,
        #[arg(long)]
        generation_index: PathBuf,
        #[arg(long)]
        store_root: PathBuf,
        #[arg(long)]
        index_out: PathBuf,
    },
    /// Authenticate and aggregate a closed source-recognition record index.
    AggregateRecognition {
        #[arg(long)]
        recognition_index: PathBuf,
        #[arg(long)]
        store_root: PathBuf,
        #[arg(long)]
        out: PathBuf,
    },
    /// Produce the P1 ledger, recognition records, and both aggregates for one
    /// explicit corpus generation. Membership is never discovered from output.
    CaptureCorpus {
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
        output_dir: PathBuf,
    },
}

fn read_json<T: serde::de::DeserializeOwned>(path: &PathBuf) -> Result<T> {
    Ok(serde_json::from_slice(&fs::read(path)?)?)
}

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(deny_unknown_fields)]
struct V1Taxonomy {
    #[serde(rename = "$schema")]
    schema: String,
    coordinate_system: ab_parser_rq_source_accountability::CoordinateSystem,
    rules: Vec<serde_json::Value>,
    schema_version: String,
    taxonomy_version: ab_parser_rq_source_accountability::TaxonomyVersion,
}

fn taxonomy(path: &PathBuf) -> Result<TaxonomyIdentity> {
    let bytes = fs::read(path)?;
    let value: V1Taxonomy = serde_json::from_slice(&bytes)?;
    anyhow::ensure!(
        value.schema == "https://w3id.org/abc/schemas/parser-rq-ignored-regions.schema.json",
        "taxonomy $schema mismatch"
    );
    anyhow::ensure!(
        value.schema_version == "abc/parser-rq-ignored-regions/v1",
        "taxonomy schema_version mismatch"
    );
    anyhow::ensure!(value.rules.is_empty(), "v1 taxonomy rules must be empty");
    let canonical = canonical_json(&value)?;
    anyhow::ensure!(
        canonical.as_bytes() == bytes,
        "taxonomy is not canonical JSON"
    );
    Ok(TaxonomyIdentity {
        taxonomy_version: value.taxonomy_version,
        taxonomy_hash: format!("sha256:{:x}", sha2::Sha256::digest(&bytes)),
        taxonomy_jcs_bytes: bytes,
    })
}

use sha2::Digest;

fn main() -> Result<()> {
    match Cli::parse().command {
        Command::AnalyzeWork {
            source,
            parser_ir,
            corpus_entry,
            qualification_identity,
            taxonomy: taxonomy_path,
            diagnostics_locator,
            out,
        } => {
            let result = analyze_work(WorkInput {
                original_bytes: fs::read(source)?,
                parser_ir_bytes: fs::read(parser_ir)?,
                corpus_entry: read_json(&corpus_entry)?,
                qualification_identity: read_json(&qualification_identity)?,
                taxonomy: taxonomy(&taxonomy_path)?,
                diagnostics_locator,
            });
            let bytes = canonical_json(&result.record)?;
            fs::write(out, &bytes)?;
            println!("{bytes}");
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
                source_root: source_root.clone(),
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
            store_root,
            out,
        } => {
            let corpus: Vec<CorpusEntry> = read_json::<Vec<CorpusSourceEntry>>(&corpus)?
                .into_iter()
                .map(|entry| entry.corpus_entry)
                .collect();
            let index = read_json::<RecordIndex>(&work_record_index)?;
            let identity = read_json::<QualificationIdentity>(&qualification_identity)?;
            let taxonomy = taxonomy(&taxonomy_path)?;
            let result = aggregate(&corpus, &index, &store_root, &identity, &taxonomy)?;
            let bytes = canonical_json(&result)?;
            fs::write(out, &bytes)?;
            println!("{bytes}");
        }
        Command::AnalyzeRecognitionCorpus {
            membership_index,
            generation_index,
            store_root,
            index_out,
        } => {
            let index = analyze_recognition_corpus(RecognitionCorpusInput {
                membership_index_bytes: fs::read(membership_index)?,
                generation_index: read_json::<RecognitionGenerationIndex>(&generation_index)?,
                store_root,
                index_out,
            })?;
            println!("{}", canonical_json(&index)?);
        }
        Command::AggregateRecognition {
            recognition_index,
            store_root,
            out,
        } => {
            let index = read_json::<RecognitionIndex>(&recognition_index)?;
            let aggregate = aggregate_recognition(&index, &store_root)?;
            let bytes = canonical_json(&aggregate)?;
            write_atomic_summary(&out, bytes.as_bytes())?;
            println!("{bytes}");
        }
        Command::CaptureCorpus {
            corpus,
            source_root,
            parser_ir_root,
            qualification,
            taxonomy: taxonomy_path,
            store_root,
            output_dir,
        } => {
            fs::create_dir_all(&output_dir)?;
            let identity = read_json::<QualificationIdentity>(&qualification)?;
            let taxonomy = taxonomy(&taxonomy_path)?;
            let corpus_entries = read_json::<Vec<CorpusSourceEntry>>(&corpus)?;
            let membership_path = output_dir.join("source-accountability-index.json");
            let membership = analyze_corpus(CorpusInput {
                entries: corpus_entries.clone(),
                source_root: source_root.clone(),
                parser_ir_root,
                store_root: store_root.clone(),
                index_out: membership_path.clone(),
                qualification_identity: identity.clone(),
                taxonomy: taxonomy.clone(),
            })?;
            let p1_aggregate = aggregate(
                &corpus_entries
                    .iter()
                    .map(|entry| entry.corpus_entry.clone())
                    .collect::<Vec<_>>(),
                &membership,
                &store_root,
                &identity,
                &taxonomy,
            )?;
            write_atomic_summary(
                &output_dir.join("source-accountability-aggregate.json"),
                canonical_json(&p1_aggregate)?.as_bytes(),
            )?;
            let identity_ref =
                ab_parser_rq_source_accountability::qualification_identity_ref(&identity)?;
            let source_root = fs::canonicalize(&source_root)?;
            let mut generation_records = Vec::with_capacity(corpus_entries.len());
            for entry in &corpus_entries {
                let relative = &entry.source_path;
                if relative.is_absolute()
                    || relative
                        .components()
                        .any(|component| matches!(component, std::path::Component::ParentDir))
                {
                    anyhow::bail!("source path escapes its configured root");
                }
                let source_path = fs::canonicalize(source_root.join(relative))?;
                if !source_path.starts_with(&source_root) {
                    anyhow::bail!("source path escapes its configured root");
                }
                let generation = ab_aozora_aat::capture_generation_from_bytes_for_identity(
                    &fs::read(source_path)?,
                    &identity_ref,
                )?;
                let published = generation.publish(&store_root)?;
                generation_records.push(
                    ab_parser_rq_source_accountability::RecognitionGenerationEntry {
                        work_id: entry.corpus_entry.work_id.clone(),
                        sha256: published.manifest.sha256,
                        bytes: published.manifest.bytes,
                        media_type: "application/json".to_owned(),
                        locator: published.manifest.locator,
                    },
                );
            }
            let generation_index = RecognitionGenerationIndex {
                records: generation_records,
            };
            write_atomic_summary(
                &output_dir.join("classified-source-generation-index.json"),
                canonical_json(&generation_index)?.as_bytes(),
            )?;
            let recognition = analyze_recognition_corpus(RecognitionCorpusInput {
                membership_index_bytes: fs::read(&membership_path)?,
                generation_index,
                store_root: store_root.clone(),
                index_out: output_dir.join("source-recognition-index.json"),
            })?;
            let recognition_aggregate = aggregate_recognition(&recognition, &store_root)?;
            write_atomic_summary(
                &output_dir.join("source-recognition-aggregate.json"),
                canonical_json(&recognition_aggregate)?.as_bytes(),
            )?;
            println!("{}", canonical_json(&recognition)?);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn public_cli_uses_designed_work_and_store_flags() {
        let cli = Cli::try_parse_from([
            "tool",
            "analyze-work",
            "--source",
            "source.txt",
            "--parser-ir",
            "ir.json",
            "--corpus-entry",
            "entry.json",
            "--qualification-identity",
            "identity.json",
            "--taxonomy",
            "taxonomy.json",
            "--diagnostics-locator",
            "diag.json",
            "--out",
            "record.json",
        ]);
        assert!(cli.is_ok());
        let aggregate = Cli::try_parse_from([
            "tool",
            "aggregate",
            "--corpus",
            "corpus.json",
            "--work-record-index",
            "index.json",
            "--store-root",
            "store",
            "--qualification-identity",
            "identity.json",
            "--taxonomy",
            "taxonomy.json",
            "--out",
            "aggregate.json",
        ]);
        assert!(aggregate.is_ok());
        let capture = Cli::try_parse_from([
            "tool",
            "capture-corpus",
            "--corpus",
            "corpus.json",
            "--source-root",
            "sources",
            "--parser-ir-root",
            "parser-ir",
            "--qualification",
            "identity.json",
            "--taxonomy",
            "taxonomy.json",
            "--store-root",
            "store",
            "--output-dir",
            "out",
        ]);
        assert!(capture.is_ok());
        let caller_generation = Cli::try_parse_from([
            "tool",
            "capture-corpus",
            "--corpus",
            "corpus.json",
            "--source-root",
            "sources",
            "--parser-ir-root",
            "parser-ir",
            "--qualification",
            "identity.json",
            "--taxonomy",
            "taxonomy.json",
            "--store-root",
            "store",
            "--generation-index",
            "caller.json",
            "--output-dir",
            "out",
        ]);
        assert!(caller_generation.is_err());
        let recognition = Cli::try_parse_from([
            "tool",
            "analyze-recognition-corpus",
            "--membership-index",
            "p1-index.json",
            "--generation-index",
            "generations.json",
            "--store-root",
            "store",
            "--index-out",
            "recognition-index.json",
        ]);
        assert!(recognition.is_ok());
        let aggregate_recognition = Cli::try_parse_from([
            "tool",
            "aggregate-recognition",
            "--recognition-index",
            "recognition-index.json",
            "--store-root",
            "store",
            "--out",
            "aggregate.json",
        ]);
        assert!(aggregate_recognition.is_ok());
    }

    #[test]
    fn taxonomy_parser_rejects_non_v1_and_noncanonical_documents() {
        let root = std::env::temp_dir().join(format!("taxonomy-cli-{}", std::process::id()));
        let cases = [
            br#"{"#.as_slice(),
            br#"{"$schema":"https://w3id.org/abc/schemas/parser-rq-ignored-regions.schema.json","coordinate_system":"decoded_utf8","extra":true,"rules":[],"schema_version":"abc/parser-rq-ignored-regions/v1","taxonomy_version":"parser-rq-ignored-regions-v1"}"#.as_slice(),
            br#"{"$schema":"https://w3id.org/abc/schemas/parser-rq-ignored-regions.schema.json","coordinate_system":"decoded_utf8","rules":[{}],"schema_version":"abc/parser-rq-ignored-regions/v1","taxonomy_version":"parser-rq-ignored-regions-v1"}"#.as_slice(),
            br#"{"$schema":"wrong","coordinate_system":"decoded_utf8","rules":[],"schema_version":"abc/parser-rq-ignored-regions/v1","taxonomy_version":"parser-rq-ignored-regions-v1"}"#.as_slice(),
            br#"{"$schema":"https://w3id.org/abc/schemas/parser-rq-ignored-regions.schema.json","coordinate_system":"bytes","rules":[],"schema_version":"abc/parser-rq-ignored-regions/v1","taxonomy_version":"parser-rq-ignored-regions-v2"}"#.as_slice(),
            br#"{ "coordinate_system":"decoded_utf8","rules":[],"schema_version":"abc/parser-rq-ignored-regions/v1","taxonomy_version":"parser-rq-ignored-regions-v1","$schema":"https://w3id.org/abc/schemas/parser-rq-ignored-regions.schema.json"}"#.as_slice(),
        ];
        for (index, bytes) in cases.iter().enumerate() {
            let path = root.with_extension(index.to_string());
            fs::write(&path, bytes).unwrap();
            assert!(taxonomy(&path).is_err(), "case {index} was accepted");
            fs::remove_file(path).unwrap();
        }
    }
}
