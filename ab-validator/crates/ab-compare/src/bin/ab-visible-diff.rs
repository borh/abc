use std::{collections::BTreeMap, path::PathBuf};

use ab_diff_utils::{
    AlignmentConfig, AlignmentKind, AlignmentRegion, ComparisonEvidence, algorithm_config_hash,
    align_pair, sentence_like_tokens,
};
use anyhow::{Context, Result};
use clap::Parser;
use serde::Serialize;
use serde_json::{Value, json};

const MAX_SAMPLES: usize = 8;
const MAX_SAMPLE_CHARS: usize = 160;

#[derive(Debug, Parser)]
#[command(
    version,
    about = "Emit an alignment-probe-v1 JSON diff for two visible-text files"
)]
struct Args {
    #[arg(long)]
    left: PathBuf,

    #[arg(long)]
    right: PathBuf,

    #[arg(long)]
    output: PathBuf,

    #[arg(long, default_value = "left")]
    left_witness: String,

    #[arg(long, default_value = "right")]
    right_witness: String,
}

#[derive(Debug, Serialize)]
struct AlignmentProbe {
    schema_version: String,
    evidence_level: ComparisonEvidence,
    algorithm_id: String,
    algorithm_config_hash: String,
    algorithm_config: Value,
    tokenization_id: String,
    normalization_id: String,
    left_witness: String,
    right_witness: String,
    summary: Value,
    diagnosis_event_count: usize,
    diagnosis_counts: BTreeMap<String, usize>,
    samples: Vec<ProbeSample>,
    truncated: bool,
    limits: Value,
}

#[derive(Debug, Serialize)]
struct ProbeSample {
    kind: String,
    diagnosis: String,
    left_range: [usize; 2],
    right_range: [usize; 2],
    left_text: String,
    right_text: String,
    truncated: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let left = std::fs::read_to_string(&args.left)
        .with_context(|| format!("failed to read {}", args.left.display()))?;
    let right = std::fs::read_to_string(&args.right)
        .with_context(|| format!("failed to read {}", args.right.display()))?;
    let probe = build_probe(
        &left,
        &right,
        args.left_witness.clone(),
        args.right_witness.clone(),
    )?;
    if let Some(parent) = args.output.parent()
        && !parent.as_os_str().is_empty()
    {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    std::fs::write(&args.output, serde_json::to_string_pretty(&probe)? + "\n")
        .with_context(|| format!("failed to write {}", args.output.display()))?;
    Ok(())
}

fn build_probe(
    left: &str,
    right: &str,
    left_witness: String,
    right_witness: String,
) -> Result<AlignmentProbe> {
    let config = AlignmentConfig::default();
    let left_tokens = sentence_like_tokens(left);
    let right_tokens = sentence_like_tokens(right);
    let result = align_pair(&left_tokens, &right_tokens, &config);
    let algorithm_config =
        serde_json::to_value(&config).context("failed to serialize alignment config")?;
    let mut diagnosis_counts = BTreeMap::new();
    let mut samples = Vec::new();
    for region in result
        .regions
        .iter()
        .filter(|region| region.kind != AlignmentKind::Equal)
    {
        let diagnosis = kind_name(region.kind).to_owned();
        *diagnosis_counts.entry(diagnosis.clone()).or_insert(0) += 1;
        if samples.len() < MAX_SAMPLES {
            samples.push(sample_for_region(region, &diagnosis));
        }
    }
    let diagnosis_event_count = diagnosis_counts.values().sum();
    Ok(AlignmentProbe {
        schema_version: "alignment-probe-v1".to_owned(),
        evidence_level: ComparisonEvidence::TokenSequenceAligned,
        algorithm_id: "abc-pairwise-token-align-v1".to_owned(),
        algorithm_config_hash: algorithm_config_hash(&config),
        algorithm_config,
        tokenization_id: "visible-text-sentence-like-text-run-v1".to_owned(),
        normalization_id: "unicode-whitespace-stripped-v1".to_owned(),
        left_witness,
        right_witness,
        summary: serde_json::to_value(&result.summary)
            .context("failed to serialize alignment summary")?,
        diagnosis_event_count,
        diagnosis_counts,
        samples,
        truncated: result.truncated,
        limits: json!({
            "max_tokens_per_window": config.max_tokens_per_window,
            "max_samples": MAX_SAMPLES,
            "max_sample_chars": MAX_SAMPLE_CHARS
        }),
    })
}

fn sample_for_region(region: &AlignmentRegion, diagnosis: &str) -> ProbeSample {
    ProbeSample {
        kind: kind_name(region.kind).to_owned(),
        diagnosis: diagnosis.to_owned(),
        left_range: region.left_range,
        right_range: region.right_range,
        left_text: truncate_sample(&region.left_text_sample),
        right_text: truncate_sample(&region.right_text_sample),
        truncated: region.truncated
            || region.left_text_sample.chars().count() > MAX_SAMPLE_CHARS
            || region.right_text_sample.chars().count() > MAX_SAMPLE_CHARS,
    }
}

fn kind_name(kind: AlignmentKind) -> &'static str {
    match kind {
        AlignmentKind::Equal => "equal",
        AlignmentKind::Insertion => "insertion",
        AlignmentKind::Deletion => "deletion",
        AlignmentKind::Substitution => "substitution",
        AlignmentKind::LikelyMovedBlock => "likely_moved_block",
        AlignmentKind::UnclassifiedMismatch => "unclassified_mismatch",
    }
}

fn truncate_sample(value: &str) -> String {
    value.chars().take(MAX_SAMPLE_CHARS).collect()
}
