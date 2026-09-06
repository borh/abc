use std::collections::HashSet;
use std::path::{Path, PathBuf};

use ab_rq_artifact_store::{
    authenticate_blob, authenticate_blob_identity, publish_blob, write_atomic_summary,
};
use anyhow::Result;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sha2::{Digest, Sha256};

use crate::{
    RecognitionInput, RecognitionStatus, RecordIndex, analyze_recognition, canonical_json,
};

const POLICY_BYTES: &[u8] =
    include_bytes!("../../../research/data/parser-rq-ab-aozora-classified-source-v1.json");
const CORPUS_GENERATION_ALGORITHM: &str = "sha256-rfc8785-safe-integer-domain-abc-v1";
pub(crate) const MAX_SAFE_INTEGER: u64 = 9_007_199_254_740_991;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecognitionGenerationEntry {
    pub work_id: String,
    pub sha256: String,
    pub bytes: u64,
    pub media_type: String,
    pub locator: String,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecognitionGenerationIndex {
    pub records: Vec<RecognitionGenerationEntry>,
}

#[derive(Clone, Debug)]
pub struct RecognitionCorpusInput {
    pub membership_index_bytes: Vec<u8>,
    pub generation_index: RecognitionGenerationIndex,
    pub store_root: PathBuf,
    pub index_out: PathBuf,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecognitionRecordEntry {
    pub work_id: String,
    pub capture_generation_ref: String,
    pub sha256: String,
    pub bytes: u64,
    pub media_type: String,
    pub locator: String,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecognitionIndex {
    pub schema_version: String,
    pub qualification_identity_ref: String,
    pub corpus_generation_ref: String,
    pub corpus_generation_algorithm: String,
    pub policy_hash: String,
    pub membership_ref: String,
    pub coordinate_system: String,
    pub status: RecognitionStatus,
    pub expected_work_ids: Vec<String>,
    pub expected_work_count: u64,
    pub record_count: u64,
    pub records: Vec<RecognitionRecordEntry>,
    pub errors: Vec<String>,
}

fn hash(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn valid_hash(value: &str) -> bool {
    value.strip_prefix("sha256:").is_some_and(|digest| {
        digest.len() == 64
            && digest
                .bytes()
                .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
    })
}

fn rfc8785_string(value: &str) -> Result<String> {
    Ok(serde_json::to_string(value)?)
}

fn rfc8785_safe_integer_value(value: &Value, output: &mut String) -> Result<()> {
    match value {
        Value::Null => output.push_str("null"),
        Value::Bool(value) => output.push_str(if *value { "true" } else { "false" }),
        Value::String(value) => output.push_str(&rfc8785_string(value)?),
        Value::Number(value) => {
            if let Some(value) = value.as_i64() {
                anyhow::ensure!(
                    value.unsigned_abs() <= MAX_SAFE_INTEGER,
                    "integer outside safe domain"
                );
                output.push_str(&value.to_string());
            } else if let Some(value) = value.as_u64() {
                anyhow::ensure!(value <= MAX_SAFE_INTEGER, "integer outside safe domain");
                output.push_str(&value.to_string());
            } else {
                anyhow::bail!("floating-point number outside safe-integer domain");
            }
        }
        Value::Array(values) => {
            output.push('[');
            for (index, value) in values.iter().enumerate() {
                if index != 0 {
                    output.push(',');
                }
                rfc8785_safe_integer_value(value, output)?;
            }
            output.push(']');
        }
        Value::Object(values) => {
            output.push('{');
            let mut entries = values.iter().collect::<Vec<_>>();
            entries.sort_by(|(left, _), (right, _)| left.encode_utf16().cmp(right.encode_utf16()));
            for (index, (key, value)) in entries.into_iter().enumerate() {
                if index != 0 {
                    output.push(',');
                }
                output.push_str(&rfc8785_string(key)?);
                output.push(':');
                rfc8785_safe_integer_value(value, output)?;
            }
            output.push('}');
        }
    }
    Ok(())
}

pub fn rfc8785_safe_integer_json(value: &Value) -> Result<String> {
    let mut output = String::new();
    rfc8785_safe_integer_value(value, &mut output)?;
    Ok(output)
}

pub(crate) fn authenticate_corpus_generation_ref(index: &RecognitionIndex) -> Result<String> {
    let mut value = serde_json::to_value(index)?;
    value
        .as_object_mut()
        .ok_or_else(|| anyhow::anyhow!("recognition index is not an object"))?
        .remove("corpus_generation_ref");
    Ok(hash(rfc8785_safe_integer_json(&value)?.as_bytes()))
}

fn require_content_address(locator: &str, expected_hash: &str) -> Result<()> {
    let digest = expected_hash
        .strip_prefix("sha256:")
        .filter(|value| {
            value.len() == 64
                && value
                    .bytes()
                    .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
        })
        .ok_or_else(|| anyhow::anyhow!("invalid member hash"))?;
    anyhow::ensure!(
        locator.starts_with(&format!("sha256/{}/{}.", &digest[..2], digest)),
        "member locator is not its asserted content address"
    );
    Ok(())
}

fn member(root: &Path, value: &Value, name: &str) -> Result<(Vec<u8>, String)> {
    let member = value
        .pointer(&format!("/members/{name}"))
        .ok_or_else(|| anyhow::anyhow!("missing generation member {name}"))?;
    let locator = member["artifact_ref"]
        .as_str()
        .ok_or_else(|| anyhow::anyhow!("invalid member locator"))?;
    let expected_hash = member["value_hash"]
        .as_str()
        .ok_or_else(|| anyhow::anyhow!("invalid member hash"))?;
    require_content_address(locator, expected_hash)?;
    Ok((
        authenticate_blob_identity(root, locator, expected_hash)?,
        locator.to_owned(),
    ))
}

fn policy_hash() -> Result<String> {
    let value: Value = serde_json::from_slice(POLICY_BYTES)?;
    value["policy_hash"]
        .as_str()
        .map(str::to_owned)
        .ok_or_else(|| anyhow::anyhow!("compiled policy has no identity"))
}

pub fn analyze_recognition_corpus(input: RecognitionCorpusInput) -> Result<RecognitionIndex> {
    let membership: RecordIndex = serde_json::from_slice(&input.membership_index_bytes)?;
    anyhow::ensure!(
        canonical_json(&membership)?.as_bytes() == input.membership_index_bytes,
        "membership index is not canonical JSON"
    );
    anyhow::ensure!(
        valid_hash(&membership.identity_ref),
        "membership identity is invalid"
    );
    let membership_ref = hash(&input.membership_index_bytes);
    let policy_hash = policy_hash()?;
    let expected_work_ids = membership
        .records
        .iter()
        .map(|entry| entry.work_id.clone())
        .collect::<Vec<_>>();
    let expected_set = expected_work_ids.iter().collect::<HashSet<_>>();
    let generation_set = input
        .generation_index
        .records
        .iter()
        .map(|entry| &entry.work_id)
        .collect::<HashSet<_>>();
    let mut errors = Vec::new();
    if membership.status != crate::WorkStatus::Ok || !membership.errors.is_empty() {
        errors.push("membership-index-unavailable".to_owned());
    }
    if membership.expected_work_count != expected_work_ids.len() as u64
        || membership.record_count != membership.records.len() as u64
        || membership.expected_work_count > MAX_SAFE_INTEGER
        || membership.record_count > MAX_SAFE_INTEGER
        || expected_set.len() != expected_work_ids.len()
    {
        errors.push("membership-index-incoherent".to_owned());
    }
    if generation_set != expected_set
        || generation_set.len() != input.generation_index.records.len()
    {
        errors.push("generation-membership-mismatch".to_owned());
    }

    let mut records = Vec::new();
    if errors.is_empty() {
        let generations = input
            .generation_index
            .records
            .iter()
            .map(|entry| (entry.work_id.as_str(), entry))
            .collect::<std::collections::HashMap<_, _>>();
        for work_id in &expected_work_ids {
            let entry = generations[work_id.as_str()].clone();
            let analysis = (|| -> Result<_> {
                anyhow::ensure!(
                    entry.media_type == "application/json",
                    "manifest media type"
                );
                let manifest_bytes = authenticate_blob(
                    &input.store_root,
                    &entry.locator,
                    &entry.sha256,
                    entry.bytes,
                )?;
                let manifest: Value = serde_json::from_slice(&manifest_bytes)?;
                let (decoded_source, _) = member(&input.store_root, &manifest, "decoded_source")?;
                let (parser_output, _) = member(&input.store_root, &manifest, "parser_output")?;
                let (raw_diagnostics, _) = member(&input.store_root, &manifest, "raw_diagnostics")?;
                let (ledger_bytes, ledger_locator) =
                    member(&input.store_root, &manifest, "classified_source_ledger")?;
                Ok(analyze_recognition(RecognitionInput {
                    decoded_source,
                    parser_output,
                    raw_diagnostics,
                    ledger_bytes,
                    policy_bytes: POLICY_BYTES.to_vec(),
                    generation_manifest: manifest_bytes,
                    qualification_identity_ref: membership.identity_ref.clone(),
                    work_id: entry.work_id.clone(),
                    ledger_locator,
                }))
            })();
            match analysis {
                Ok(analysis) => {
                    if analysis.record.status == RecognitionStatus::Unavailable {
                        errors.push(format!("work-unavailable:{}", entry.work_id));
                    }
                    let Some(capture_generation_ref) =
                        analysis.record.capture_generation_ref.clone()
                    else {
                        errors.push(format!("capture-generation-unavailable:{}", entry.work_id));
                        continue;
                    };
                    anyhow::ensure!(
                        valid_hash(&capture_generation_ref),
                        "authenticated capture generation ref invalid"
                    );
                    let bytes = canonical_json(&analysis.record)?.into_bytes();
                    let blob = publish_blob(&input.store_root, "json", &bytes)?;
                    anyhow::ensure!(
                        blob.bytes <= MAX_SAFE_INTEGER,
                        "record exceeds safe-integer domain"
                    );
                    records.push(RecognitionRecordEntry {
                        work_id: entry.work_id,
                        capture_generation_ref,
                        sha256: blob.sha256,
                        bytes: blob.bytes,
                        media_type: "application/json".to_owned(),
                        locator: blob.locator,
                    });
                }
                Err(_) => errors.push(format!("generation-unavailable:{}", entry.work_id)),
            }
        }
    }
    let mut index = RecognitionIndex {
        schema_version: "abc/parser-rq-source-recognition-index/v1".to_owned(),
        qualification_identity_ref: membership.identity_ref,
        corpus_generation_ref: format!("sha256:{}", "0".repeat(64)),
        corpus_generation_algorithm: CORPUS_GENERATION_ALGORITHM.to_owned(),
        policy_hash,
        membership_ref,
        coordinate_system: "decoded_utf8".to_owned(),
        status: if errors.is_empty() {
            RecognitionStatus::Ok
        } else {
            RecognitionStatus::Unavailable
        },
        expected_work_count: expected_work_ids.len() as u64,
        expected_work_ids,
        record_count: records.len() as u64,
        records,
        errors,
    };
    index.corpus_generation_ref = authenticate_corpus_generation_ref(&index)?;
    write_atomic_summary(&input.index_out, canonical_json(&index)?.as_bytes())?;
    Ok(index)
}
