use std::collections::BTreeSet;

use crate::{
    AuthorizationStatus, DiagnosticGapAggregate, DiagnosticGapAggregateInput,
    DiagnosticGapWorkInput, DiagnosticGapWorkResult, DiagnosticGapWorkStatus, Interval,
    SourceRecognitionEvidence,
};
use ab_parser_rq_source_accountability::{
    Interval as R1Interval, RecognitionStatus, canonical_json, reconcile,
};
use sha2::{Digest, Sha256};

fn sha256(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}
fn total(intervals: &[Interval]) -> Option<u64> {
    intervals.iter().try_fold(0_u64, |sum, value| {
        sum.checked_add(value.end.checked_sub(value.start)?)
    })
}
fn unavailable_aggregate(expected: &[String], error: &str) -> DiagnosticGapAggregate {
    DiagnosticGapAggregate {
        status: DiagnosticGapWorkStatus::Unavailable,
        qualification_identity_ref: None,
        corpus_generation_ref: None,
        policy_hash: None,
        expected_work_ids: expected.to_vec(),
        observed_work_ids: vec![],
        authorized_bytes: None,
        silent_bytes: None,
        silent_drop_count: None,
        errors: vec![error.to_owned()],
    }
}

/// Partition authenticated R1 semantic gaps with already-authorized exact spans.
#[must_use]
pub fn derive_gap_partition(input: DiagnosticGapWorkInput<'_>) -> DiagnosticGapWorkResult {
    let r1 = input.source_recognition;
    let auth = input.authorization;
    if auth.status != AuthorizationStatus::Ok {
        return DiagnosticGapWorkResult::unavailable("diagnostic-authorization-unavailable");
    }
    let (Some(work_id), Some(generation), Some(qualification), Some(gaps), Some(policy_hash)) = (
        r1.work_id.as_deref(),
        r1.capture_generation_ref.as_deref(),
        r1.qualification_identity_ref.as_deref(),
        r1.semantic_gaps.as_deref(),
        auth.policy_hash.as_deref(),
    ) else {
        return DiagnosticGapWorkResult::unavailable("source-recognition-incomplete");
    };
    if r1.status != RecognitionStatus::Ok
        || canonical_json(r1).ok().as_deref().map(str::as_bytes)
            != Some(input.source_recognition_bytes)
        || sha256(input.source_recognition_bytes) != input.source_recognition_value_hash
        || input.source_recognition_artifact_ref.sha256 != input.source_recognition_value_hash
        || input.source_recognition_artifact_ref.bytes
            != input.source_recognition_bytes.len() as u64
        || input.source_recognition_artifact_ref.media_type != "application/json"
    {
        return DiagnosticGapWorkResult::unavailable("source-recognition-evidence-mismatch");
    }
    let Some(authorized) = auth.authorized_intervals.as_deref() else {
        return DiagnosticGapWorkResult::unavailable("diagnostic-authorization-incomplete");
    };
    let bound = match r1
        .eligible_bytes
        .and_then(|value| usize::try_from(value).ok())
    {
        Some(value) => value,
        None => return DiagnosticGapWorkResult::unavailable("source-recognition-gap-invalid"),
    };
    let convert = |value: Interval| {
        R1Interval::new(
            usize::try_from(value.start).ok()?,
            usize::try_from(value.end).ok()?,
            bound,
        )
        .ok()
    };
    let gap_values = gaps
        .iter()
        .map(|v| Interval {
            start: v.start,
            end: v.end,
        })
        .collect::<Vec<_>>();
    if gap_values
        .windows(2)
        .any(|pair| pair[0].end >= pair[1].start)
        || total(&gap_values) != r1.semantic_gap_bytes
        || r1
            .recognized_bytes
            .and_then(|recognized| recognized.checked_add(r1.semantic_gap_bytes?))
            != r1.eligible_bytes
    {
        return DiagnosticGapWorkResult::unavailable("source-recognition-gap-invalid");
    }
    let Some(gap_intervals) = gap_values
        .iter()
        .copied()
        .map(convert)
        .collect::<Option<Vec<_>>>()
    else {
        return DiagnosticGapWorkResult::unavailable("source-recognition-gap-invalid");
    };
    let Some(authorized_intervals) = authorized
        .iter()
        .copied()
        .map(convert)
        .collect::<Option<Vec<_>>>()
    else {
        return DiagnosticGapWorkResult::unavailable("authorized-interval-invalid");
    };
    let partition = reconcile(&gap_intervals, &authorized_intervals);
    let diagnosed = partition
        .diagnosed
        .iter()
        .map(|v| Interval {
            start: v.start() as u64,
            end: v.end() as u64,
        })
        .collect::<Vec<_>>();
    if diagnosed != authorized {
        return DiagnosticGapWorkResult::unavailable("authorized-interval-outside-semantic-gap");
    }
    let silent = partition
        .silent
        .iter()
        .map(|v| Interval {
            start: v.start() as u64,
            end: v.end() as u64,
        })
        .collect::<Vec<_>>();
    let (Some(authorized_bytes), Some(silent_bytes), Some(gap_bytes)) =
        (total(&diagnosed), total(&silent), total(&gap_values))
    else {
        return DiagnosticGapWorkResult::unavailable("partition-total-overflow");
    };
    if authorized_bytes.checked_add(silent_bytes) != Some(gap_bytes) {
        return DiagnosticGapWorkResult::unavailable("partition-conservation-failed");
    }
    DiagnosticGapWorkResult {
        status: DiagnosticGapWorkStatus::Ok,
        work_id: Some(work_id.to_owned()),
        capture_generation_ref: Some(generation.to_owned()),
        qualification_identity_ref: Some(qualification.to_owned()),
        policy_hash: Some(policy_hash.to_owned()),
        source_recognition_evidence: Some(SourceRecognitionEvidence {
            relation: "partitions-semantic-gaps-of".to_owned(),
            artifact_ref: input.source_recognition_artifact_ref,
            value_hash: input.source_recognition_value_hash,
            qualification_identity_ref: qualification.to_owned(),
            capture_generation_ref: generation.to_owned(),
            work_id: work_id.to_owned(),
        }),
        authorized_intervals: Some(diagnosed),
        silent_intervals: Some(silent),
        authorized_bytes: Some(authorized_bytes),
        silent_bytes: Some(silent_bytes),
        silent_drop_count: Some(partition.silent_drops),
        errors: vec![],
    }
}

/// Fold work partitions only when they exactly match the declared corpus work set.
#[must_use]
pub fn aggregate_gap_partitions(input: DiagnosticGapAggregateInput<'_>) -> DiagnosticGapAggregate {
    let expected = input
        .expected_works
        .iter()
        .map(|work| work.work_id.clone())
        .collect::<BTreeSet<_>>();
    let expected_work_ids = input
        .expected_works
        .iter()
        .map(|work| work.work_id.clone())
        .collect::<Vec<_>>();
    let observed = input
        .works
        .iter()
        .filter_map(|work| work.work_id.clone())
        .collect::<Vec<_>>();
    let observed_set = observed.iter().cloned().collect::<BTreeSet<_>>();
    if expected.len() != input.expected_works.len()
        || observed_set.len() != input.works.len()
        || expected != observed_set
        || input.works.iter().any(|work| {
            let expected_work = work.work_id.as_deref().and_then(|work_id| {
                input
                    .expected_works
                    .iter()
                    .find(|item| item.work_id == work_id)
            });
            let exact_r1 = work.source_recognition_evidence.as_ref().zip(expected_work);
            work.status != DiagnosticGapWorkStatus::Ok
                || work.qualification_identity_ref.as_deref()
                    != Some(input.qualification_identity_ref)
                || work.policy_hash.as_deref() != Some(input.policy_hash)
                || exact_r1.is_none_or(|(evidence, expected)| {
                    work.capture_generation_ref.as_deref()
                        != Some(expected.capture_generation_ref.as_str())
                        || evidence.capture_generation_ref != expected.capture_generation_ref
                        || evidence.value_hash != expected.source_recognition_value_hash
                })
        })
    {
        return unavailable_aggregate(
            &expected_work_ids,
            "diagnostic-gap-work-set-or-identity-mismatch",
        );
    }
    let sum = |f: fn(&DiagnosticGapWorkResult) -> Option<u64>| {
        input
            .works
            .iter()
            .try_fold(0_u64, |sum, work| sum.checked_add(f(work)?))
    };
    let (Some(authorized_bytes), Some(silent_bytes), Some(silent_drop_count)) = (
        sum(|w| w.authorized_bytes),
        sum(|w| w.silent_bytes),
        sum(|w| w.silent_drop_count),
    ) else {
        return unavailable_aggregate(&expected_work_ids, "diagnostic-gap-total-overflow");
    };
    DiagnosticGapAggregate {
        status: DiagnosticGapWorkStatus::Ok,
        qualification_identity_ref: Some(input.qualification_identity_ref.to_owned()),
        corpus_generation_ref: Some(input.corpus_generation_ref.to_owned()),
        policy_hash: Some(input.policy_hash.to_owned()),
        expected_work_ids,
        observed_work_ids: observed,
        authorized_bytes: Some(authorized_bytes),
        silent_bytes: Some(silent_bytes),
        silent_drop_count: Some(silent_drop_count),
        errors: vec![],
    }
}
