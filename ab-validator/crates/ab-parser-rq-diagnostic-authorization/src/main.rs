use std::{fs, path::PathBuf};

use ab_parser_rq_diagnostic_authorization::{
    BoundaryInput, DiagnosticGapAggregateInput, DiagnosticGapExpectedWork, DiagnosticGapWorkInput,
    DiagnosticGapWorkResult, DiagnosticGapWorkStatus, aggregate_gap_partitions, authorize_boundary,
    derive_gap_partition, validate_gap_policy,
};
use ab_parser_rq_source_accountability::{
    RecognitionBlobRef, RecognitionWorkRecord, canonical_json,
};
use anyhow::Result;
use clap::{Parser, Subcommand};
use serde::Deserialize;
use serde_json::{Value, json};
use sha2::{Digest, Sha256};

#[derive(Parser)]
#[command(about = "Parser-RQ diagnostic authorization analyzer")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Consume an explicit closed R1/diagnostic index and derive R2 once.
    CaptureCorpus {
        #[arg(long)]
        index: PathBuf,
        #[arg(long)]
        policy: PathBuf,
        #[arg(long)]
        out: PathBuf,
    },
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct CorpusIndex {
    qualification_identity_ref: String,
    corpus_generation_ref: String,
    policy_artifact_hash: String,
    records: Vec<WorkIndex>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct WorkIndex {
    work_id: String,
    capture_generation_ref: String,
    decoded_source: PathBuf,
    decoded_source_hash: String,
    raw_diagnostics: PathBuf,
    raw_diagnostics_hash: String,
    source_recognition: PathBuf,
    source_recognition_hash: String,
    source_recognition_locator: String,
}

fn hash(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn intervals(values: &Option<Vec<ab_parser_rq_diagnostic_authorization::Interval>>) -> Value {
    values.as_ref().map_or(Value::Null, |values| {
        Value::Array(
            values
                .iter()
                .map(|value| json!({"start":value.start,"end":value.end}))
                .collect(),
        )
    })
}

fn work_json(result: &DiagnosticGapWorkResult) -> Value {
    match result.status {
        DiagnosticGapWorkStatus::Ok => json!({
            "schema_version":"abc/parser-rq-diagnostic-gap-result/v1",
            "status":"ok",
            "work_id":result.work_id,
            "capture_generation_ref":result.capture_generation_ref,
            "qualification_identity_ref":result.qualification_identity_ref,
            "policy_hash":result.policy_hash,
            "source_recognition_evidence":result.source_recognition_evidence,
            "diagnostic_authorization_evidence":result.diagnostic_authorization_evidence,
            "authorized_intervals":intervals(&result.authorized_intervals),
            "silent_intervals":intervals(&result.silent_intervals),
            "authorized_bytes":result.authorized_bytes,
            "silent_bytes":result.silent_bytes,
            "silent_drop_count":result.silent_drop_count,
            "diagnostic_count":result.diagnostic_count,
            "authorizing_diagnostic_count":result.authorizing_diagnostic_count,
            "observe_only_diagnostic_count":result.observe_only_diagnostic_count,
            "vacuous":result.vacuous,
        }),
        DiagnosticGapWorkStatus::Unavailable => json!({
            "schema_version":"abc/parser-rq-diagnostic-gap-result/v1",
            "status":"unavailable",
            "work_id":result.work_id,
            "capture_generation_ref":result.capture_generation_ref,
            "policy_hash":result.policy_hash,
            "unavailable_reasons":["diagnostic-gap-unavailable"],
        }),
    }
}

fn main() -> Result<()> {
    match Cli::parse().command {
        Command::CaptureCorpus { index, policy, out } => {
            let index: CorpusIndex = serde_json::from_slice(&fs::read(index)?)?;
            let policy_bytes = fs::read(policy)?;
            anyhow::ensure!(
                hash(&policy_bytes) == index.policy_artifact_hash,
                "policy hash mismatch"
            );
            let validated_policy = validate_gap_policy(&policy_bytes, &index.policy_artifact_hash)
                .map_err(anyhow::Error::msg)?;
            let mut expected = Vec::new();
            let mut works = Vec::new();
            for entry in &index.records {
                let decoded = fs::read(&entry.decoded_source)?;
                let raw = fs::read(&entry.raw_diagnostics)?;
                let recognition_bytes = fs::read(&entry.source_recognition)?;
                let recognition: RecognitionWorkRecord =
                    serde_json::from_slice(&recognition_bytes)?;
                let authorization = authorize_boundary(BoundaryInput {
                    raw_diagnostics: &raw,
                    raw_diagnostics_hash: &entry.raw_diagnostics_hash,
                    raw_diagnostics_bytes: raw.len() as u64,
                    policy_bytes: &policy_bytes,
                    policy_bytes_hash: &index.policy_artifact_hash,
                    decoded_source: &decoded,
                    decoded_source_hash: &entry.decoded_source_hash,
                    work_id: &entry.work_id,
                    capture_generation_ref: &entry.capture_generation_ref,
                    qualification_identity_ref: &index.qualification_identity_ref,
                    source_recognition_bytes: &recognition_bytes,
                    source_recognition_hash: &entry.source_recognition_hash,
                    source_recognition: &recognition,
                });
                let artifact_ref = RecognitionBlobRef {
                    sha256: entry.source_recognition_hash.clone(),
                    bytes: recognition_bytes.len() as u64,
                    media_type: "application/json".to_owned(),
                    locator: entry.source_recognition_locator.clone(),
                };
                let result = derive_gap_partition(DiagnosticGapWorkInput {
                    source_recognition: &recognition,
                    source_recognition_bytes: &recognition_bytes,
                    source_recognition_artifact_ref: artifact_ref,
                    source_recognition_value_hash: entry.source_recognition_hash.clone(),
                    authorization: &authorization,
                });
                expected.push(DiagnosticGapExpectedWork {
                    work_id: entry.work_id.clone(),
                    capture_generation_ref: entry.capture_generation_ref.clone(),
                    source_recognition_value_hash: entry.source_recognition_hash.clone(),
                });
                works.push(result);
            }
            let aggregate = aggregate_gap_partitions(DiagnosticGapAggregateInput {
                expected_works: &expected,
                qualification_identity_ref: &index.qualification_identity_ref,
                corpus_generation_ref: &index.corpus_generation_ref,
                policy_hash: validated_policy.policy_hash(),
                works: &works,
            });
            let value = json!({
                "schema_version":"abc/parser-rq-diagnostic-gap-corpus/v1",
                "qualification_identity_ref":index.qualification_identity_ref,
                "corpus_generation_ref":index.corpus_generation_ref,
                "works":works.iter().map(work_json).collect::<Vec<_>>(),
                "aggregate":{
                    "schema_version":aggregate.schema_version,
                    "status":format!("{:?}", aggregate.status).to_lowercase(),
                    "qualification_identity_ref":aggregate.qualification_identity_ref,
                    "corpus_generation_ref":aggregate.corpus_generation_ref,
                    "policy_hash":aggregate.policy_hash,
                    "policy_artifact_hash":aggregate.policy_artifact_hash,
                    "expected_work_ids":aggregate.expected_work_ids,
                    "observed_work_ids":aggregate.observed_work_ids,
                    "authorized_bytes":aggregate.authorized_bytes,
                    "silent_bytes":aggregate.silent_bytes,
                    "silent_drop_count":aggregate.silent_drop_count,
                    "diagnostic_count":aggregate.diagnostic_count,
                    "authorizing_diagnostic_count":aggregate.authorizing_diagnostic_count,
                    "observe_only_diagnostic_count":aggregate.observe_only_diagnostic_count,
                    "authorized_interval_count":aggregate.authorized_interval_count,
                    "vacuous":aggregate.vacuous,
                }
            });
            fs::write(out, canonical_json(&value)?)?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn capture_corpus_requires_explicit_paths() {
        assert!(
            Cli::try_parse_from([
                "tool",
                "capture-corpus",
                "--index",
                "index.json",
                "--policy",
                "policy.json",
                "--out",
                "out.json"
            ])
            .is_ok()
        );
        assert!(Cli::try_parse_from(["tool", "capture-corpus"]).is_err());
    }
}
