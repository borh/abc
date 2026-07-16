use std::collections::BTreeSet;

use crate::{
    AuthorizationStatus, DiagnosticAuthorizationEvidence, DiagnosticGapAggregate,
    DiagnosticGapAggregateInput, DiagnosticGapWorkInput, DiagnosticGapWorkResult,
    DiagnosticGapWorkStatus, Interval, SourceRecognitionEvidence, WorkResultSeal,
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
        schema_version: None,
        status: DiagnosticGapWorkStatus::Unavailable,
        qualification_identity_ref: None,
        corpus_generation_ref: None,
        policy_hash: None,
        policy_artifact_hash: None,
        expected_work_ids: expected.to_vec(),
        observed_work_ids: vec![],
        authorized_bytes: None,
        silent_bytes: None,
        silent_drop_count: None,
        diagnostic_count: None,
        authorizing_diagnostic_count: None,
        observe_only_diagnostic_count: None,
        authorized_interval_count: None,
        vacuous: None,
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
    if !auth.errors.is_empty() {
        return DiagnosticGapWorkResult::unavailable("diagnostic-authorization-audit-mismatch");
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
    let Some(origin) = auth.origin.as_ref() else {
        return DiagnosticGapWorkResult::unavailable("diagnostic-authorization-origin-missing");
    };
    if origin.work_id != work_id
        || origin.capture_generation_ref != generation
        || origin.qualification_identity_ref != qualification
        || origin.policy_hash != policy_hash
        || origin.source_recognition_hash != input.source_recognition_value_hash
        || origin.decoded_source_hash != work_id
        || origin.raw_diagnostics_hash.is_empty()
    {
        return DiagnosticGapWorkResult::unavailable("diagnostic-authorization-origin-mismatch");
    }
    let (Some(diagnostic_count), Some(authorizing_count), Some(observe_only_count), Some(vacuous)) = (
        auth.diagnostic_count,
        auth.authorizing_diagnostic_count,
        auth.observe_only_diagnostic_count,
        auth.vacuous,
    ) else {
        return DiagnosticGapWorkResult::unavailable("diagnostic-authorization-incomplete");
    };
    if authorizing_count.checked_add(observe_only_count) != Some(diagnostic_count)
        || vacuous != (diagnostic_count == 0)
        || origin.diagnostic_count != diagnostic_count
        || origin.authorizing_diagnostic_count != authorizing_count
        || origin.observe_only_diagnostic_count != observe_only_count
        || origin.vacuous != vacuous
        || origin.authorized_intervals.as_slice() != authorized
    {
        return DiagnosticGapWorkResult::unavailable("diagnostic-authorization-audit-mismatch");
    }
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
        diagnostic_authorization_evidence: Some(DiagnosticAuthorizationEvidence {
            decoded_source_hash: origin.decoded_source_hash.clone(),
            raw_diagnostics_hash: origin.raw_diagnostics_hash.clone(),
            raw_diagnostics_bytes: origin.raw_diagnostics_bytes,
            policy_hash: origin.policy_hash.clone(),
            policy_artifact_hash: origin.policy_artifact_hash.clone(),
            policy_artifact_bytes: origin.policy_artifact_bytes,
            source_recognition_hash: origin.source_recognition_hash.clone(),
        }),
        authorized_intervals: Some(diagnosed.clone()),
        silent_intervals: Some(silent.clone()),
        authorized_bytes: Some(authorized_bytes),
        silent_bytes: Some(silent_bytes),
        silent_drop_count: Some(partition.silent_drops),
        diagnostic_count: Some(diagnostic_count),
        authorizing_diagnostic_count: Some(authorizing_count),
        observe_only_diagnostic_count: Some(observe_only_count),
        vacuous: Some(vacuous),
        errors: vec![],
        seal: Some(WorkResultSeal {
            authorized_intervals: diagnosed,
            silent_intervals: silent,
            authorized_bytes,
            silent_bytes,
            silent_drop_count: partition.silent_drops,
            diagnostic_count,
            authorizing_diagnostic_count: authorizing_count,
            observe_only_diagnostic_count: observe_only_count,
            vacuous,
        }),
    }
}

/// Fold work partitions only when they exactly match the declared corpus work set.
#[must_use]
pub fn aggregate_gap_partitions(input: DiagnosticGapAggregateInput<'_>) -> DiagnosticGapAggregate {
    let policy_artifact_hash = input
        .works
        .first()
        .and_then(|work| work.diagnostic_authorization_evidence.as_ref())
        .map(|evidence| evidence.policy_artifact_hash.clone());
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
            let authorization = work.diagnostic_authorization_evidence.as_ref();
            let sealed = work.seal.as_ref();
            work.status != DiagnosticGapWorkStatus::Ok
                || work.qualification_identity_ref.as_deref()
                    != Some(input.qualification_identity_ref)
                || work.policy_hash.as_deref() != Some(input.policy_hash)
                || !work.errors.is_empty()
                || sealed.is_none_or(|seal| {
                    work.authorized_intervals.as_ref() != Some(&seal.authorized_intervals)
                        || work.silent_intervals.as_ref() != Some(&seal.silent_intervals)
                        || work.authorized_bytes != Some(seal.authorized_bytes)
                        || work.silent_bytes != Some(seal.silent_bytes)
                        || work.silent_drop_count != Some(seal.silent_drop_count)
                        || work.diagnostic_count != Some(seal.diagnostic_count)
                        || work.authorizing_diagnostic_count
                            != Some(seal.authorizing_diagnostic_count)
                        || work.observe_only_diagnostic_count
                            != Some(seal.observe_only_diagnostic_count)
                        || work.vacuous != Some(seal.vacuous)
                })
                || authorization.is_none_or(|evidence| {
                    evidence.policy_hash != input.policy_hash
                        || evidence.source_recognition_hash
                            != work
                                .source_recognition_evidence
                                .as_ref()
                                .map_or("", |value| value.value_hash.as_str())
                        || evidence.decoded_source_hash != work.work_id.as_deref().unwrap_or("")
                        || evidence.raw_diagnostics_hash.is_empty()
                        || Some(&evidence.policy_artifact_hash) != policy_artifact_hash.as_ref()
                })
                || exact_r1.is_none_or(|(evidence, expected)| {
                    work.capture_generation_ref.as_deref()
                        != Some(expected.capture_generation_ref.as_str())
                        || evidence.work_id != expected.work_id
                        || evidence.qualification_identity_ref != input.qualification_identity_ref
                        || evidence.relation != "partitions-semantic-gaps-of"
                        || evidence.capture_generation_ref != expected.capture_generation_ref
                        || evidence.value_hash != expected.source_recognition_value_hash
                        || evidence.artifact_ref.sha256 != expected.source_recognition_value_hash
                        || evidence.artifact_ref.sha256 != evidence.value_hash
                        || evidence.artifact_ref.media_type != "application/json"
                        || evidence.artifact_ref.bytes == 0
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
    let (
        Some(authorized_bytes),
        Some(silent_bytes),
        Some(silent_drop_count),
        Some(diagnostic_count),
        Some(authorizing_count),
        Some(observe_only_count),
        Some(authorized_interval_count),
    ) = (
        sum(|w| w.authorized_bytes),
        sum(|w| w.silent_bytes),
        sum(|w| w.silent_drop_count),
        sum(|w| w.diagnostic_count),
        sum(|w| w.authorizing_diagnostic_count),
        sum(|w| w.observe_only_diagnostic_count),
        input.works.iter().try_fold(0_u64, |sum, work| {
            sum.checked_add(u64::try_from(work.authorized_intervals.as_ref()?.len()).ok()?)
        }),
    )
    else {
        return unavailable_aggregate(&expected_work_ids, "diagnostic-gap-total-overflow");
    };
    DiagnosticGapAggregate {
        schema_version: Some("abc/parser-rq-diagnostic-gap-aggregate/v1".to_owned()),
        status: DiagnosticGapWorkStatus::Ok,
        qualification_identity_ref: Some(input.qualification_identity_ref.to_owned()),
        corpus_generation_ref: Some(input.corpus_generation_ref.to_owned()),
        policy_hash: Some(input.policy_hash.to_owned()),
        policy_artifact_hash,
        expected_work_ids: expected_work_ids.clone(),
        observed_work_ids: expected_work_ids.clone(),
        authorized_bytes: Some(authorized_bytes),
        silent_bytes: Some(silent_bytes),
        silent_drop_count: Some(silent_drop_count),
        diagnostic_count: Some(diagnostic_count),
        authorizing_diagnostic_count: Some(authorizing_count),
        observe_only_diagnostic_count: Some(observe_only_count),
        authorized_interval_count: Some(authorized_interval_count),
        vacuous: Some(diagnostic_count == 0),
        errors: vec![],
    }
}
