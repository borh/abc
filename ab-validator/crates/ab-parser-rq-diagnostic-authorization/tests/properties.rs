use ab_parser_rq_diagnostic_authorization::{
    AuthorizationStatus, BoundaryInput, authorize_boundary,
};
use ab_parser_rq_source_accountability::{RecognitionWorkRecord, canonical_json};
use hegel::generators;
use sha2::{Digest, Sha256};

const POLICY: &[u8] =
    include_bytes!("../../../../abc/data/parser-rq-ab-aozora-diagnostic-gap-v1.json");
fn hash(b: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(b))
}
fn record(source: &[u8]) -> RecognitionWorkRecord {
    let n = source.len();
    serde_json::from_value(serde_json::json!({"schema_version":"abc/parser-rq-source-recognition-work/v1","qualification_identity_ref":format!("sha256:{}","4".repeat(64)),"capture_generation_ref":format!("sha256:{}","1".repeat(64)),"policy_hash":format!("sha256:{}","5".repeat(64)),"instrument_version":"parser-rq-source-recognition-v2","work_id":hash(source),"coordinate_system":"decoded_utf8","ledger":{"sha256":format!("sha256:{}","6".repeat(64)),"bytes":1,"media_type":"application/json","locator":"x"},"status":"ok","eligible_bytes":n,"recognized_bytes":0,"accounted_bytes":n,"semantic_gap_bytes":n,"unaccounted_bytes":0,"recognized":[],"accounted":[{"start":0,"end":n}],"semantic_gaps":[{"start":0,"end":n}],"unaccounted":[]})).unwrap()
}
fn run(
    entries: Vec<serde_json::Value>,
    source: &[u8],
) -> ab_parser_rq_diagnostic_authorization::AuthorizationAnalysis {
    let raw = serde_json::to_vec(&serde_json::json!({"schemaVersion":3,"data":entries})).unwrap();
    let r = record(source);
    let rb = canonical_json(&r).unwrap().into_bytes();
    let work_id = hash(source);
    authorize_boundary(BoundaryInput {
        raw_diagnostics: &raw,
        raw_diagnostics_hash: &hash(&raw),
        raw_diagnostics_bytes: raw.len() as u64,
        policy_bytes: POLICY,
        policy_bytes_hash: &hash(POLICY),
        decoded_source: source,
        decoded_source_hash: &work_id,
        work_id: &work_id,
        capture_generation_ref: &format!("sha256:{}", "1".repeat(64)),
        qualification_identity_ref: &format!("sha256:{}", "4".repeat(64)),
        source_recognition_bytes: &rb,
        source_recognition_hash: &hash(&rb),
        source_recognition: &r,
    })
}
fn pua() -> serde_json::Value {
    serde_json::json!({"kind":"source_contains_pua","code":"source-contains-pua","severity":"warning","source":"source","span":{"start":0,"end":3},"codepoint":"\u{e001}"})
}
fn note(start: u64, end: u64) -> serde_json::Value {
    serde_json::json!({"kind":"unclosed_bracket","code":"unclosed-bracket","severity":"error","source":"source","span":{"start":start,"end":end}})
}

#[hegel::test]
fn diagnostic_order_does_not_change_authorized_value(tc: hegel::TestCase) {
    let reverse = tc.draw(generators::booleans());
    let mut entries = vec![pua(), note(0, 4)];
    if reverse {
        entries.reverse();
    }
    let result = run(entries, "\u{e001}x".as_bytes());
    assert_eq!(result.status, AuthorizationStatus::Ok);
    assert_eq!(result.authorized_intervals, Some(vec![(0, 3).into()]));
}

#[hegel::test]
fn duplicate_semantic_identity_always_fails_closed(tc: hegel::TestCase) {
    let end = tc.draw(generators::integers::<u8>().min_value(1).max_value(8)) as u64;
    let entry = note(0, end);
    let source = vec![b'x'; end as usize];
    let result = run(vec![entry.clone(), entry], &source);
    assert_eq!(result.status, AuthorizationStatus::Unavailable);
    assert!(result.authorized_intervals.is_none());
}

#[hegel::test]
fn overlaps_never_expand_the_authorizing_interval(tc: hegel::TestCase) {
    let end = tc.draw(generators::integers::<u8>().min_value(3).max_value(4)) as u64;
    let result = run(vec![note(0, end), pua()], "\u{e001}x".as_bytes());
    assert_eq!(result.authorized_intervals, Some(vec![(0, 3).into()]));
}
