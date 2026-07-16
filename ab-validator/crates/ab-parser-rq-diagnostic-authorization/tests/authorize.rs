use ab_parser_rq_diagnostic_authorization::{
    AuthorizationStatus, BoundaryInput, authorize_boundary, validate_diagnostic_capture,
    validate_gap_policy, validate_work_context,
};
use ab_parser_rq_source_accountability::{RecognitionStatus, RecognitionWorkRecord};

const POLICY: &[u8] =
    include_bytes!("../../../../abc/data/parser-rq-ab-aozora-diagnostic-gap-v1.json");

fn hash(bytes: &[u8]) -> String {
    use sha2::{Digest, Sha256};
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn record(source: &[u8]) -> RecognitionWorkRecord {
    let source_len = source.len();
    let work_id = hash(source);
    serde_json::from_value(serde_json::json!({
      "schema_version":"abc/parser-rq-source-recognition-work/v1",
      "qualification_identity_ref":format!("sha256:{}", "4".repeat(64)),
      "capture_generation_ref":format!("sha256:{}", "1".repeat(64)),
      "policy_hash":format!("sha256:{}", "5".repeat(64)),
      "instrument_version":"parser-rq-source-recognition-v1", "work_id":work_id,
      "coordinate_system":"decoded_utf8", "ledger":{"sha256":format!("sha256:{}", "6".repeat(64)),"bytes":1,"media_type":"application/json","locator":"x"},
      "status":"ok", "eligible_bytes":source_len,"recognized_bytes":0,"accounted_bytes":source_len,
      "semantic_gap_bytes":source_len,"unaccounted_bytes":0,"recognized":[],"accounted":[{"start":0,"end":source_len}],
      "semantic_gaps":[{"start":0,"end":source_len}],"unaccounted":[]
    })).unwrap()
}

fn run(
    raw: Vec<u8>,
    source: Vec<u8>,
) -> ab_parser_rq_diagnostic_authorization::AuthorizationAnalysis {
    let r1 = record(&source);
    run_with_record(raw, source, r1)
}

fn run_with_record(
    raw: Vec<u8>,
    source: Vec<u8>,
    r1: RecognitionWorkRecord,
) -> ab_parser_rq_diagnostic_authorization::AuthorizationAnalysis {
    let work_id = hash(&source);
    let r1_bytes = ab_parser_rq_source_accountability::canonical_json(&r1)
        .unwrap()
        .into_bytes();
    authorize_boundary(BoundaryInput {
        raw_diagnostics: &raw,
        raw_diagnostics_hash: &hash(&raw),
        raw_diagnostics_bytes: raw.len() as u64,
        policy_bytes: POLICY,
        policy_bytes_hash: &hash(POLICY),
        decoded_source: &source,
        decoded_source_hash: &work_id,
        work_id: &work_id,
        capture_generation_ref: &format!("sha256:{}", "1".repeat(64)),
        qualification_identity_ref: &format!("sha256:{}", "4".repeat(64)),
        source_recognition_bytes: &r1_bytes,
        source_recognition_hash: &hash(&r1_bytes),
        source_recognition: &r1,
    })
}

#[test]
fn every_closed_policy_row_has_its_declared_effect() {
    let policy: serde_json::Value = serde_json::from_slice(POLICY).unwrap();
    for row in policy["rules"].as_array().unwrap() {
        let pua = row["code"] == "source-contains-pua";
        let mut entry = serde_json::json!({"kind":row["kind"],"code":row["code"],
            "severity":row["severity"],"source":row["source"],"span":{"start":0,"end":3}});
        if pua {
            entry["codepoint"] = serde_json::json!("\u{e001}");
        }
        let raw =
            serde_json::to_vec(&serde_json::json!({"schemaVersion":3,"data":[entry]})).unwrap();
        let result = run(raw, "\u{e001}".as_bytes().to_vec());
        match row["disposition"].as_str().unwrap() {
            "reject_internal" => assert_eq!(
                result.status,
                AuthorizationStatus::Unavailable,
                "{}",
                row["code"]
            ),
            "authorize_exact_span" => {
                assert_eq!(result.authorized_intervals, Some(vec![(0, 3).into()]))
            }
            "observe_only" => {
                assert_eq!(
                    result.status,
                    AuthorizationStatus::Ok,
                    "{}: {:?}",
                    row["code"],
                    result.errors
                );
                assert_eq!(result.authorized_intervals, Some(vec![]));
            }
            other => panic!("unexpected disposition {other}"),
        }
    }
}

#[test]
fn only_pua_authorizes_and_observe_only_does_not() {
    let raw = serde_json::to_vec(&serde_json::json!({"schemaVersion":3,"data":[
      {"kind":"source_contains_pua","code":"source-contains-pua","severity":"warning","source":"source","span":{"start":0,"end":3},"codepoint":"\u{e001}"},
      {"kind":"unclosed_bracket","code":"unclosed-bracket","severity":"error","source":"source","span":{"start":3,"end":6}}
    ]})).unwrap();
    let result = run(raw, "\u{e001}字".as_bytes().to_vec());
    assert_eq!(
        result.status,
        AuthorizationStatus::Ok,
        "{:?}",
        result.errors
    );
    assert_eq!(result.authorized_intervals.unwrap(), vec![(0, 3).into()]);
    assert_eq!(result.authorizing_diagnostic_count, Some(1));
    assert_eq!(result.observe_only_diagnostic_count, Some(1));
}

#[test]
fn empty_authenticated_capture_is_available_and_vacuous() {
    let result = run(
        br#"{"schemaVersion":3,"data":[]}"#.to_vec(),
        b"abc".to_vec(),
    );
    assert_eq!(
        result.status,
        AuthorizationStatus::Ok,
        "{:?}",
        result.errors
    );
    assert_eq!(result.vacuous, Some(true));
    assert_eq!(result.authorized_intervals, Some(vec![]));
}

#[test]
fn malformed_unknown_internal_mismatch_duplicate_and_bad_endpoints_fail_closed() {
    let cases = [
      br#"{}"#.to_vec(),
      br#"{"schemaVersion":3,"data":[{"kind":"new","code":"new","severity":"warning","source":"source","span":{"start":0,"end":1}}]}"#.to_vec(),
      br#"{"schemaVersion":3,"data":[{"kind":"residual_annotation_marker","code":"residual-annotation-marker","severity":"error","source":"internal","span":{"start":0,"end":1}}]}"#.to_vec(),
      br#"{"schemaVersion":3,"data":[{"kind":"unclosed_bracket","code":"unclosed-bracket","severity":"warning","source":"source","span":{"start":0,"end":1}}]}"#.to_vec(),
      br#"{"schemaVersion":3,"data":[{"kind":"unclosed_bracket","code":"unclosed-bracket","severity":"error","source":"source","span":{"start":0,"end":1}},{"kind":"unclosed_bracket","code":"unclosed-bracket","severity":"error","source":"source","span":{"start":0,"end":1}}]}"#.to_vec(),
      br#"{"schemaVersion":3,"data":[{"kind":"unclosed_bracket","code":"unclosed-bracket","severity":"error","source":"source","span":{"start":1,"end":2}}]}"#.to_vec(),
    ];
    for raw in cases {
        let result = run(raw, "字".as_bytes().to_vec());
        assert_eq!(result.status, AuthorizationStatus::Unavailable);
        assert!(result.authorized_intervals.is_none());
    }
}

#[test]
fn unavailable_r1_and_identity_drift_fail_closed() {
    let raw = br#"{"schemaVersion":3,"data":[]}"#.to_vec();
    let source = b"abc".to_vec();
    let mut cases = Vec::new();
    let mut unavailable = record(&source);
    unavailable.status = RecognitionStatus::Unavailable;
    cases.push(unavailable);
    let mut work = record(&source);
    work.work_id = Some("wrong".to_owned());
    cases.push(work);
    let mut generation = record(&source);
    generation.capture_generation_ref = Some(format!("sha256:{}", "9".repeat(64)));
    cases.push(generation);
    let mut qualification = record(&source);
    qualification.qualification_identity_ref = Some(format!("sha256:{}", "9".repeat(64)));
    cases.push(qualification);
    for r1 in cases {
        let result = run_with_record(raw.clone(), source.clone(), r1);
        assert_eq!(result.status, AuthorizationStatus::Unavailable);
        assert!(result.authorized_intervals.is_none());
    }
}

#[test]
fn boundary_constructors_fail_before_pure_authorization() {
    let raw = br#"{"schemaVersion":3,"data":[]}"#;
    assert!(validate_diagnostic_capture(raw, "sha256:bad", raw.len() as u64).is_err());
    assert!(validate_gap_policy(POLICY, "sha256:bad").is_err());
    let source = b"abc";
    let r1 = record(source);
    let r1_bytes = ab_parser_rq_source_accountability::canonical_json(&r1)
        .unwrap()
        .into_bytes();
    assert!(
        validate_work_context(
            source,
            "sha256:bad",
            &hash(source),
            &format!("sha256:{}", "1".repeat(64)),
            &format!("sha256:{}", "4".repeat(64)),
            &r1_bytes,
            &hash(&r1_bytes),
            &r1
        )
        .is_err()
    );
}
