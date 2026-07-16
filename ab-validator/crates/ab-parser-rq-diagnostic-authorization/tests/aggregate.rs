use ab_parser_rq_diagnostic_authorization::{
    AuthorizationAnalysis, AuthorizationStatus, DiagnosticGapAggregateInput,
    DiagnosticGapExpectedWork, DiagnosticGapWorkInput, DiagnosticGapWorkStatus, Interval,
    aggregate_gap_partitions, derive_gap_partition,
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
    RecognitionWorkRecord {
        schema_version: "abc/parser-rq-source-recognition-work/v1".into(),
        qualification_identity_ref: Some(hash('4')),
        capture_generation_ref: Some(generation.into()),
        policy_hash: Some(hash('5')),
        instrument_version: "parser-rq-source-recognition-v1".into(),
        work_id: Some(work_id.into()),
        coordinate_system: "decoded_utf8".into(),
        ledger: Some(RecognitionBlobRef {
            sha256: hash('6'),
            bytes: 1,
            media_type: "application/json".into(),
            locator: "ledger.json".into(),
        }),
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

fn authorization(intervals: &[(u64, u64)], vacuous: bool) -> AuthorizationAnalysis {
    AuthorizationAnalysis {
        status: AuthorizationStatus::Ok,
        policy_hash: Some(hash('7')),
        diagnostic_count: Some(if vacuous { 0 } else { intervals.len() as u64 }),
        authorizing_diagnostic_count: Some(intervals.len() as u64),
        observe_only_diagnostic_count: Some(0),
        vacuous: Some(vacuous),
        authorized_intervals: Some(intervals.iter().copied().map(Interval::from).collect()),
        errors: vec![],
    }
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
    let r1 = record("work-a", &hash('1'), &[(0, 10)]);
    let auth = authorization(&[(0, 9)], false);
    let result = derive_gap_partition(input(&r1, &auth));
    assert_eq!(result.status, DiagnosticGapWorkStatus::Ok);
    assert_eq!(result.authorized_intervals, Some(vec![(0, 9).into()]));
    assert_eq!(result.silent_intervals, Some(vec![(9, 10).into()]));
    assert_eq!(result.authorized_bytes, Some(9));
    assert_eq!(result.silent_bytes, Some(1));
    assert_eq!(result.silent_drop_count, Some(1));
}

#[test]
fn empty_authenticated_diagnostics_are_available_and_vacuous() {
    let r1 = record("work-a", &hash('1'), &[(2, 5)]);
    let auth = authorization(&[], true);
    let result = derive_gap_partition(input(&r1, &auth));
    assert_eq!(result.status, DiagnosticGapWorkStatus::Ok);
    assert_eq!(result.authorized_intervals, Some(vec![]));
    assert_eq!(result.silent_intervals, Some(vec![(2, 5).into()]));
    assert_eq!(result.silent_drop_count, Some(1));
}

#[test]
fn authorization_outside_the_gap_fails_closed_instead_of_expanding_r2() {
    let r1 = record("work-a", &hash('1'), &[(2, 5)]);
    let auth = authorization(&[(1, 3)], false);
    let result = derive_gap_partition(input(&r1, &auth));
    assert_eq!(result.status, DiagnosticGapWorkStatus::Unavailable);
    assert!(result.authorized_intervals.is_none());
    assert!(result.silent_intervals.is_none());
}

#[test]
fn corpus_fold_requires_exact_work_and_generation_identities() {
    let r1a = record("work-a", &hash('1'), &[(0, 2)]);
    let r1b = record("work-b", &hash('2'), &[(0, 2)]);
    let auth = authorization(&[], true);
    let a = derive_gap_partition(input(&r1a, &auth));
    let b = derive_gap_partition(input(&r1b, &auth));
    let expected_works = [expected(&a), expected(&b)];
    let result = aggregate_gap_partitions(DiagnosticGapAggregateInput {
        expected_works: &expected_works,
        qualification_identity_ref: &hash('4'),
        corpus_generation_ref: &hash('3'),
        policy_hash: &hash('7'),
        works: &[a.clone(), b.clone()],
    });
    assert_eq!(result.status, DiagnosticGapWorkStatus::Ok);
    assert_eq!(result.silent_drop_count, Some(2));
    assert_eq!(result.silent_bytes, Some(4));

    for bad in [
        DiagnosticGapAggregateInput {
            expected_works: &expected_works[..1],
            qualification_identity_ref: &hash('4'),
            corpus_generation_ref: &hash('3'),
            policy_hash: &hash('7'),
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
    let _ = derive_gap_partition(input(&r1, &authorization(&[], true)));
    assert_eq!(canonical_json(&r1).unwrap(), before);

    let mut malformed = authorization(&[], true);
    malformed.status = AuthorizationStatus::Unavailable;
    malformed.policy_hash = None;
    let result = derive_gap_partition(input(&r1, &malformed));
    assert_eq!(result.status, DiagnosticGapWorkStatus::Unavailable);
    assert!(result.authorized_intervals.is_none());
    assert_eq!(canonical_json(&r1).unwrap(), before);
}
