use ab_parser_rq_diagnostic_authorization::{
    AuthorizationAnalysis, AuthorizationStatus, BoundaryInput, DiagnosticGapAggregateInput,
    DiagnosticGapExpectedWork, DiagnosticGapWorkInput, DiagnosticGapWorkStatus,
    aggregate_gap_partitions, authorize_boundary, derive_gap_partition,
};
use ab_parser_rq_source_accountability::{
    RecognitionBlobRef, RecognitionInterval, RecognitionStatus, RecognitionWorkRecord,
    canonical_json,
};

fn hash(ch: char) -> String {
    format!("sha256:{}", ch.to_string().repeat(64))
}

fn digest(bytes: &[u8]) -> String {
    use sha2::{Digest, Sha256};
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn record(work_id: &str, generation: &str, gaps: &[(u64, u64)]) -> RecognitionWorkRecord {
    let gap_bytes = gaps.iter().map(|(start, end)| end - start).sum::<u64>();
    let eligible = gaps.iter().map(|(_, end)| *end).max().unwrap_or(0);
    let mut source = vec![b'a'; eligible as usize];
    if eligible >= 3 {
        source.splice(0..3, "\u{e001}".as_bytes().iter().copied());
    }
    RecognitionWorkRecord {
        schema_version: "abc/parser-rq-source-recognition-work/v1".into(),
        qualification_identity_ref: Some(hash('4')),
        capture_generation_ref: Some(generation.into()),
        policy_hash: Some(hash('5')),
        instrument_version: "parser-rq-source-recognition-v1".into(),
        work_id: Some(work_id.to_owned()),
        coordinate_system: "decoded_utf8".into(),
        ledger: Some(RecognitionBlobRef {
            sha256: hash('6'),
            bytes: 1,
            media_type: "application/json".into(),
            locator: "ledger.json".into(),
        }),
        // The diagnostic-authorization aggregate reads `semantic_gap_bytes`
        // and nothing else from this record, so the region partition is
        // irrelevant here and is left absent rather than fabricated.
        regions: None,
        status: RecognitionStatus::Ok,
        eligible_bytes: Some(eligible),
        recognized_bytes: Some(eligible - gap_bytes),
        accounted_bytes: Some(eligible),
        semantic_gap_bytes: Some(gap_bytes),
        unaccounted_bytes: Some(0),
        recognized: Some(if gaps.is_empty() && eligible > 0 {
            vec![RecognitionInterval {
                start: 0,
                end: eligible,
            }]
        } else {
            vec![]
        }),
        accounted: Some(if eligible > 0 {
            vec![RecognitionInterval {
                start: 0,
                end: eligible,
            }]
        } else {
            vec![]
        }),
        semantic_gaps: Some(
            gaps.iter()
                .map(|&(start, end)| RecognitionInterval { start, end })
                .collect(),
        ),
        unaccounted: Some(vec![]),
        errors: vec![],
    }
}

fn source(record: &RecognitionWorkRecord) -> Vec<u8> {
    let mut value = vec![b'a'; record.eligible_bytes.unwrap() as usize];
    if value.len() >= 3 {
        value.splice(0..3, "\u{e001}".as_bytes().iter().copied());
    }
    value
}

const POLICY: &[u8] =
    include_bytes!("../../../../abc/data/parser-rq-ab-aozora-diagnostic-gap-v1.json");

fn authorization(
    record: &RecognitionWorkRecord,
    source: &[u8],
    intervals: &[(u64, u64)],
) -> AuthorizationAnalysis {
    let data = intervals
        .iter()
        .map(|&(start, end)| {
            serde_json::json!({
                "kind":"source_contains_pua", "code":"source-contains-pua", "severity":"warning",
                "source":"source", "span":{"start":start,"end":end}, "codepoint":"\u{e001}"
            })
        })
        .collect::<Vec<_>>();
    let raw = serde_json::to_vec(&serde_json::json!({"schemaVersion":3,"data":data})).unwrap();
    let r1_bytes = canonical_json(record).unwrap().into_bytes();
    authorize_boundary(BoundaryInput {
        raw_diagnostics: &raw,
        raw_diagnostics_hash: &digest(&raw),
        raw_diagnostics_bytes: raw.len() as u64,
        policy_bytes: POLICY,
        policy_bytes_hash: &digest(POLICY),
        decoded_source: source,
        decoded_source_hash: &digest(source),
        work_id: record.work_id.as_deref().unwrap(),
        capture_generation_ref: record.capture_generation_ref.as_deref().unwrap(),
        qualification_identity_ref: record.qualification_identity_ref.as_deref().unwrap(),
        source_recognition_bytes: &r1_bytes,
        source_recognition_hash: &digest(&r1_bytes),
        source_recognition: record,
    })
}

fn input<'a>(
    record: &'a RecognitionWorkRecord,
    auth: &'a AuthorizationAnalysis,
) -> DiagnosticGapWorkInput<'a> {
    let bytes = canonical_json(record).unwrap().into_bytes();
    let leaked = Box::leak(bytes.into_boxed_slice());
    let value_hash = digest(leaked);
    DiagnosticGapWorkInput {
        source_recognition: record,
        source_recognition_bytes: leaked,
        source_recognition_artifact_ref: RecognitionBlobRef {
            sha256: value_hash.clone(),
            bytes: leaked.len() as u64,
            media_type: "application/json".into(),
            locator: "recognition.json".into(),
        },
        source_recognition_value_hash: value_hash,
        authorization: auth,
    }
}

fn expected(
    result: &ab_parser_rq_diagnostic_authorization::DiagnosticGapWorkResult,
) -> DiagnosticGapExpectedWork {
    let evidence = result.source_recognition_evidence.as_ref().unwrap();
    DiagnosticGapExpectedWork {
        work_id: evidence.work_id.clone(),
        capture_generation_ref: evidence.capture_generation_ref.clone(),
        source_recognition_value_hash: evidence.value_hash.clone(),
    }
}

#[test]
fn partitions_exact_gap_and_keeps_one_byte_residual_silent() {
    let r1 = record("work-a", &hash('1'), &[(0, 4)]);
    let auth = authorization(&r1, &source(&r1), &[(0, 3)]);
    let result = derive_gap_partition(input(&r1, &auth));
    assert_eq!(result.status, DiagnosticGapWorkStatus::Ok);
    assert_eq!(result.authorized_intervals, Some(vec![(0, 3).into()]));
    assert_eq!(result.silent_intervals, Some(vec![(3, 4).into()]));
    assert_eq!(result.authorized_bytes, Some(3));
    assert_eq!(result.silent_bytes, Some(1));
    assert_eq!(result.silent_drop_count, Some(1));
    let evidence = result.diagnostic_authorization_evidence.as_ref().unwrap();
    assert_eq!(evidence.policy_artifact_hash, digest(POLICY));
    assert_ne!(
        evidence.policy_artifact_hash,
        result.policy_hash.as_deref().unwrap()
    );
}

#[test]
fn empty_authenticated_diagnostics_are_available_and_vacuous() {
    let r1 = record("work-a", &hash('1'), &[(2, 5)]);
    let auth = authorization(&r1, &source(&r1), &[]);
    let result = derive_gap_partition(input(&r1, &auth));
    assert_eq!(result.status, DiagnosticGapWorkStatus::Ok);
    assert_eq!(result.authorized_intervals, Some(vec![]));
    assert_eq!(result.silent_intervals, Some(vec![(2, 5).into()]));
    assert_eq!(result.silent_drop_count, Some(1));
}

#[test]
fn authorization_outside_the_gap_fails_closed_instead_of_expanding_r2() {
    let r1 = record("work-a", &hash('1'), &[(2, 5)]);
    let auth = authorization(&r1, &source(&r1), &[(0, 3)]);
    let result = derive_gap_partition(input(&r1, &auth));
    assert_eq!(result.status, DiagnosticGapWorkStatus::Unavailable);
    assert!(result.authorized_intervals.is_none());
    assert!(result.silent_intervals.is_none());
}

#[test]
fn corpus_fold_requires_exact_work_and_generation_identities() {
    let r1a = record("work-a", &hash('1'), &[(0, 2)]);
    let r1b = record("work-b", &hash('2'), &[(0, 3)]);
    let auth_a = authorization(&r1a, &source(&r1a), &[]);
    let auth_b = authorization(&r1b, &source(&r1b), &[]);
    let a = derive_gap_partition(input(&r1a, &auth_a));
    let b = derive_gap_partition(input(&r1b, &auth_b));
    let expected_works = [expected(&a), expected(&b)];
    let result = aggregate_gap_partitions(DiagnosticGapAggregateInput {
        expected_works: &expected_works,
        qualification_identity_ref: &hash('4'),
        corpus_generation_ref: &hash('3'),
        policy_hash: a.policy_hash.as_deref().unwrap(),
        works: &[a.clone(), b.clone()],
    });
    assert_eq!(result.status, DiagnosticGapWorkStatus::Ok);
    assert_eq!(
        result.schema_version.as_deref(),
        Some("abc/parser-rq-diagnostic-gap-aggregate/v1")
    );
    assert_eq!(result.silent_drop_count, Some(2));
    assert_eq!(result.silent_bytes, Some(5));

    for bad in [
        DiagnosticGapAggregateInput {
            expected_works: &expected_works[..1],
            qualification_identity_ref: &hash('4'),
            corpus_generation_ref: &hash('3'),
            policy_hash: a.policy_hash.as_deref().unwrap(),
            works: &[a.clone(), b.clone()],
        },
        DiagnosticGapAggregateInput {
            expected_works: &expected_works,
            qualification_identity_ref: &hash('4'),
            corpus_generation_ref: &hash('3'),
            policy_hash: &hash('7'),
            works: &[a.clone(), a.clone()],
        },
    ] {
        assert_eq!(
            aggregate_gap_partitions(bad).status,
            DiagnosticGapWorkStatus::Unavailable
        );
    }
    let mut wrong_generation = expected_works.clone();
    wrong_generation[0].capture_generation_ref = hash('9');
    assert_eq!(
        aggregate_gap_partitions(DiagnosticGapAggregateInput {
            expected_works: &wrong_generation,
            qualification_identity_ref: &hash('4'),
            corpus_generation_ref: &hash('3'),
            policy_hash: &hash('7'),
            works: &[a, b],
        })
        .status,
        DiagnosticGapWorkStatus::Unavailable
    );
}

#[test]
fn deriving_r2_does_not_mutate_or_reencode_r1() {
    let r1 = record("work-a", &hash('1'), &[(0, 2)]);
    let before = canonical_json(&r1).unwrap();
    let auth = authorization(&r1, &source(&r1), &[]);
    let _ = derive_gap_partition(input(&r1, &auth));
    assert_eq!(canonical_json(&r1).unwrap(), before);

    let mut malformed = authorization(&r1, &source(&r1), &[]);
    malformed.status = AuthorizationStatus::Unavailable;
    malformed.policy_hash = None;
    let result = derive_gap_partition(input(&r1, &malformed));
    assert_eq!(result.status, DiagnosticGapWorkStatus::Unavailable);
    assert!(result.authorized_intervals.is_none());
    assert_eq!(canonical_json(&r1).unwrap(), before);
}

#[test]
fn authorization_cannot_be_substituted_across_work_or_generation() {
    let r1a = record("work-a", &hash('1'), &[(0, 3)]);
    let r1b = record("work-b", &hash('2'), &[(0, 4)]);
    let auth_a = authorization(&r1a, &source(&r1a), &[(0, 3)]);
    let result = derive_gap_partition(input(&r1b, &auth_a));
    assert_eq!(result.status, DiagnosticGapWorkStatus::Unavailable);
    assert!(result.authorized_intervals.is_none());

    let mut tampered = authorization(&r1a, &source(&r1a), &[(0, 3)]);
    tampered.authorized_intervals = Some(vec![]);
    assert_eq!(
        derive_gap_partition(input(&r1a, &tampered)).status,
        DiagnosticGapWorkStatus::Unavailable
    );
}

#[test]
fn corpus_fold_is_expected_index_ordered_and_rejects_redundant_evidence_drift() {
    let r1a = record("work-a", &hash('1'), &[(0, 2)]);
    let r1b = record("work-b", &hash('2'), &[(0, 3)]);
    let a = derive_gap_partition(input(&r1a, &authorization(&r1a, &source(&r1a), &[])));
    let b = derive_gap_partition(input(&r1b, &authorization(&r1b, &source(&r1b), &[])));
    let expected_works = [expected(&a), expected(&b)];
    let aggregate = |works: &[ab_parser_rq_diagnostic_authorization::DiagnosticGapWorkResult]| {
        aggregate_gap_partitions(DiagnosticGapAggregateInput {
            expected_works: &expected_works,
            qualification_identity_ref: &hash('4'),
            corpus_generation_ref: &hash('3'),
            policy_hash: a.policy_hash.as_deref().unwrap(),
            works,
        })
    };
    let reversed = aggregate(&[b.clone(), a.clone()]);
    assert_eq!(reversed.status, DiagnosticGapWorkStatus::Ok);
    assert_eq!(reversed.observed_work_ids, reversed.expected_work_ids);

    let mut forged = a.clone();
    forged
        .source_recognition_evidence
        .as_mut()
        .unwrap()
        .relation = "forged".into();
    assert_eq!(
        aggregate(&[forged, b.clone()]).status,
        DiagnosticGapWorkStatus::Unavailable
    );

    let mut mutations = Vec::new();
    let mut value = a.clone();
    value.silent_bytes = Some(999);
    mutations.push(value);
    let mut value = a.clone();
    value.silent_drop_count = Some(999);
    mutations.push(value);
    let mut value = a.clone();
    value.diagnostic_count = Some(999);
    mutations.push(value);
    let mut value = a.clone();
    value.vacuous = Some(false);
    mutations.push(value);
    let mut value = a.clone();
    value.silent_intervals = Some(vec![]);
    mutations.push(value);
    let mut value = a.clone();
    value.errors.push("forged".into());
    mutations.push(value);
    for mutated in mutations {
        assert_eq!(
            aggregate(&[mutated, b.clone()]).status,
            DiagnosticGapWorkStatus::Unavailable
        );
    }
}
