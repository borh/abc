use std::collections::HashSet;
use std::path::Path;

use ab_rq_artifact_store::authenticate_blob;
use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::interval::{Interval, normalize, subtract};
use crate::recognition_corpus::{MAX_SAFE_INTEGER, authenticate_corpus_generation_ref};
use crate::{
    RecognitionIndex, RecognitionInterval, RecognitionStatus, RecognitionWorkRecord, canonical_json,
};

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecognitionWorkCompleteness {
    pub expected: u64,
    pub observed: u64,
    pub complete: bool,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecognitionWorkInterval {
    pub work_id: String,
    pub start: u64,
    pub end: u64,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecognitionAggregate {
    pub schema_version: String,
    pub qualification_identity_ref: String,
    pub corpus_generation_ref: String,
    pub corpus_generation_algorithm: String,
    pub policy_hash: String,
    pub membership_ref: String,
    pub coordinate_system: String,
    pub status: RecognitionStatus,
    pub work_completeness: RecognitionWorkCompleteness,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub eligible_bytes: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub recognized_bytes: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub accounted_bytes: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub semantic_gap_bytes: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub unaccounted_bytes: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub semantic_gaps: Option<Vec<RecognitionWorkInterval>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub unaccounted: Option<Vec<RecognitionWorkInterval>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub errors: Option<Vec<String>>,
}

fn valid_intervals(intervals: &[RecognitionInterval], bound: u64) -> bool {
    intervals.iter().enumerate().all(|(index, interval)| {
        interval.start < interval.end
            && interval.end <= bound
            && (index == 0 || intervals[index - 1].end < interval.start)
    })
}

fn total(intervals: &[RecognitionInterval]) -> Option<u64> {
    intervals.iter().try_fold(0_u64, |sum, interval| {
        sum.checked_add(interval.end.checked_sub(interval.start)?)
    })
}

fn semantic_intervals(intervals: &[RecognitionInterval], bound: u64) -> Result<Vec<Interval>> {
    let bound = usize::try_from(bound)?;
    let raw = intervals
        .iter()
        .map(|interval| {
            Interval::new(
                usize::try_from(interval.start)?,
                usize::try_from(interval.end)?,
                bound,
            )
        })
        .collect::<Result<Vec<_>>>()?;
    anyhow::ensure!(normalize(raw.clone()) == raw, "intervals are not canonical");
    Ok(raw)
}

fn validate_record(record: &RecognitionWorkRecord, index: &RecognitionIndex) -> Result<()> {
    anyhow::ensure!(record.status == RecognitionStatus::Ok && record.errors.is_empty());
    anyhow::ensure!(
        record.qualification_identity_ref.as_deref()
            == Some(index.qualification_identity_ref.as_str())
            && record.policy_hash.as_deref() == Some(index.policy_hash.as_str())
    );
    let eligible = record
        .eligible_bytes
        .ok_or_else(|| anyhow::anyhow!("eligible absent"))?;
    anyhow::ensure!(
        eligible <= MAX_SAFE_INTEGER,
        "eligible exceeds safe-integer domain"
    );
    let recognized = record
        .recognized
        .as_deref()
        .ok_or_else(|| anyhow::anyhow!("recognized absent"))?;
    let accounted = record
        .accounted
        .as_deref()
        .ok_or_else(|| anyhow::anyhow!("accounted absent"))?;
    let gaps = record
        .semantic_gaps
        .as_deref()
        .ok_or_else(|| anyhow::anyhow!("gaps absent"))?;
    let unaccounted = record
        .unaccounted
        .as_deref()
        .ok_or_else(|| anyhow::anyhow!("unaccounted absent"))?;
    let recognized_bytes = record
        .recognized_bytes
        .ok_or_else(|| anyhow::anyhow!("recognized total absent"))?;
    let accounted_bytes = record
        .accounted_bytes
        .ok_or_else(|| anyhow::anyhow!("accounted total absent"))?;
    let gap_bytes = record
        .semantic_gap_bytes
        .ok_or_else(|| anyhow::anyhow!("gap total absent"))?;
    let unaccounted_bytes = record
        .unaccounted_bytes
        .ok_or_else(|| anyhow::anyhow!("unaccounted total absent"))?;
    anyhow::ensure!(valid_intervals(recognized, eligible) && valid_intervals(accounted, eligible));
    anyhow::ensure!(valid_intervals(gaps, eligible) && valid_intervals(unaccounted, eligible));
    let recognized_semantic = semantic_intervals(recognized, eligible)?;
    let accounted_semantic = semantic_intervals(accounted, eligible)?;
    let gaps_semantic = semantic_intervals(gaps, eligible)?;
    let unaccounted_semantic = semantic_intervals(unaccounted, eligible)?;
    let eligible_bound = usize::try_from(eligible)?;
    let full = if eligible_bound == 0 {
        Vec::new()
    } else {
        vec![Interval::new(0, eligible_bound, eligible_bound)?]
    };
    anyhow::ensure!(subtract(&recognized_semantic, &accounted_semantic).is_empty());
    anyhow::ensure!(subtract(&full, &recognized_semantic) == gaps_semantic);
    anyhow::ensure!(subtract(&full, &accounted_semantic) == unaccounted_semantic);
    anyhow::ensure!(total(recognized) == record.recognized_bytes);
    anyhow::ensure!(total(accounted) == record.accounted_bytes);
    anyhow::ensure!(total(gaps) == record.semantic_gap_bytes);
    anyhow::ensure!(total(unaccounted) == record.unaccounted_bytes);
    anyhow::ensure!(recognized_bytes.checked_add(gap_bytes) == Some(eligible));
    anyhow::ensure!(accounted_bytes.checked_add(unaccounted_bytes) == Some(eligible));
    anyhow::ensure!(recognized_bytes <= accounted_bytes);
    Ok(())
}

pub fn aggregate_recognition(
    index: &RecognitionIndex,
    store: &Path,
) -> Result<RecognitionAggregate> {
    let expected = index.expected_work_count;
    let observed = index.record_count;
    let expected_set = index.expected_work_ids.iter().collect::<HashSet<_>>();
    let record_set = index
        .records
        .iter()
        .map(|entry| &entry.work_id)
        .collect::<HashSet<_>>();
    let complete = expected_set == record_set
        && expected_set.len() == index.expected_work_ids.len()
        && record_set.len() == index.records.len()
        && expected == observed
        && observed == index.records.len() as u64;
    let mut errors = Vec::new();
    if index.status != RecognitionStatus::Ok || !index.errors.is_empty() {
        errors.push("recognition-index-unavailable".to_owned());
    }
    let corpus_ref_valid = authenticate_corpus_generation_ref(index)
        .map(|value| value == index.corpus_generation_ref)
        .unwrap_or(false);
    if index.corpus_generation_algorithm != "sha256-rfc8785-safe-integer-domain-abc-v1"
        || !corpus_ref_valid
    {
        errors.push("corpus-generation-identity-mismatch".to_owned());
    }
    if !complete {
        errors.push("recognition-membership-incomplete".to_owned());
    }
    if expected > MAX_SAFE_INTEGER
        || observed > MAX_SAFE_INTEGER
        || index
            .records
            .iter()
            .any(|entry| entry.bytes > MAX_SAFE_INTEGER)
    {
        errors.push("safe-integer-domain-exceeded".to_owned());
    }
    let mut eligible = 0_u64;
    let mut recognized = 0_u64;
    let mut accounted = 0_u64;
    let mut gap_bytes = 0_u64;
    let mut unaccounted_bytes = 0_u64;
    let mut semantic_gaps = Vec::new();
    let mut unaccounted = Vec::new();
    if errors.is_empty() {
        for entry in &index.records {
            let result = (|| -> Result<RecognitionWorkRecord> {
                anyhow::ensure!(entry.media_type == "application/json");
                let bytes = authenticate_blob(store, &entry.locator, &entry.sha256, entry.bytes)?;
                let record: RecognitionWorkRecord = serde_json::from_slice(&bytes)?;
                anyhow::ensure!(canonical_json(&record)?.as_bytes() == bytes);
                anyhow::ensure!(record.work_id.as_deref() == Some(entry.work_id.as_str()));
                validate_record(&record, index)?;
                anyhow::ensure!(
                    record.capture_generation_ref.as_deref()
                        == Some(entry.capture_generation_ref.as_str()),
                    "capture generation mismatch"
                );
                Ok(record)
            })();
            match result {
                Ok(record) => {
                    let Some(next) = eligible.checked_add(record.eligible_bytes.unwrap()) else {
                        errors.push("aggregate-total-overflow".into());
                        break;
                    };
                    eligible = next;
                    let Some(next) = recognized.checked_add(record.recognized_bytes.unwrap())
                    else {
                        errors.push("aggregate-total-overflow".into());
                        break;
                    };
                    recognized = next;
                    let Some(next) = accounted.checked_add(record.accounted_bytes.unwrap()) else {
                        errors.push("aggregate-total-overflow".into());
                        break;
                    };
                    accounted = next;
                    let Some(next) = gap_bytes.checked_add(record.semantic_gap_bytes.unwrap())
                    else {
                        errors.push("aggregate-total-overflow".into());
                        break;
                    };
                    gap_bytes = next;
                    let Some(next) =
                        unaccounted_bytes.checked_add(record.unaccounted_bytes.unwrap())
                    else {
                        errors.push("aggregate-total-overflow".into());
                        break;
                    };
                    unaccounted_bytes = next;
                    for interval in record.semantic_gaps.unwrap() {
                        semantic_gaps.push(RecognitionWorkInterval {
                            work_id: entry.work_id.clone(),
                            start: interval.start,
                            end: interval.end,
                        });
                    }
                    for interval in record.unaccounted.unwrap() {
                        unaccounted.push(RecognitionWorkInterval {
                            work_id: entry.work_id.clone(),
                            start: interval.start,
                            end: interval.end,
                        });
                    }
                }
                Err(_) => errors.push(format!("recognition-record-invalid:{}", entry.work_id)),
            }
        }
    }
    if [
        eligible,
        recognized,
        accounted,
        gap_bytes,
        unaccounted_bytes,
    ]
    .into_iter()
    .any(|total| total > MAX_SAFE_INTEGER)
    {
        errors.push("safe-integer-domain-exceeded".to_owned());
    }
    let base = RecognitionAggregate {
        schema_version: "abc/parser-rq-source-recognition-aggregate/v1".to_owned(),
        qualification_identity_ref: index.qualification_identity_ref.clone(),
        corpus_generation_ref: index.corpus_generation_ref.clone(),
        corpus_generation_algorithm: index.corpus_generation_algorithm.clone(),
        policy_hash: index.policy_hash.clone(),
        membership_ref: index.membership_ref.clone(),
        coordinate_system: "decoded_utf8".to_owned(),
        status: if errors.is_empty() {
            RecognitionStatus::Ok
        } else {
            RecognitionStatus::Unavailable
        },
        work_completeness: RecognitionWorkCompleteness {
            expected,
            observed,
            complete,
        },
        eligible_bytes: errors.is_empty().then_some(eligible),
        recognized_bytes: errors.is_empty().then_some(recognized),
        accounted_bytes: errors.is_empty().then_some(accounted),
        semantic_gap_bytes: errors.is_empty().then_some(gap_bytes),
        unaccounted_bytes: errors.is_empty().then_some(unaccounted_bytes),
        semantic_gaps: errors.is_empty().then_some(semantic_gaps),
        unaccounted: errors.is_empty().then_some(unaccounted),
        errors: (!errors.is_empty()).then_some(errors),
    };
    Ok(base)
}
