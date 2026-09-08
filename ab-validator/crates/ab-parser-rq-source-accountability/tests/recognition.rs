use ab_capture::{CaptureGeneration, capture_generation_from_bytes};
use ab_parser_rq_source_accountability::{
    RecognitionInput, RecognitionStatus, analyze_recognition,
};
use serde_json::Value;
use sha2::{Digest, Sha256};

const POLICY: &[u8] =
    include_bytes!("../../../research/data/parser-rq-ab-aozora-classified-source-v1.json");
const WORK_SCHEMA: &[u8] =
    include_bytes!("../../../research/schemas/parser-rq-source-recognition-work.schema.json");

fn sha256(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn input(source: &str) -> RecognitionInput {
    let generation = capture_generation_from_bytes(source.as_bytes()).unwrap();
    let manifest: Value = serde_json::from_slice(&generation.manifest).unwrap();
    RecognitionInput {
        decoded_source: generation.decoded_source,
        parser_output: generation.parser_output,
        raw_diagnostics: generation.raw_diagnostics,
        ledger_bytes: generation.classified_source_ledger,
        policy_bytes: POLICY.to_vec(),
        generation_manifest: generation.manifest,
        qualification_identity_ref: manifest["qualification_identity_ref"]
            .as_str()
            .unwrap()
            .to_owned(),
        work_id: manifest["work_id"].as_str().unwrap().to_owned(),
        ledger_locator: "ledger.json".to_owned(),
    }
}

fn canonical(value: &Value, output: &mut String) {
    match value {
        Value::Null => output.push_str("null"),
        Value::Bool(value) => output.push_str(if *value { "true" } else { "false" }),
        Value::Number(value) => output.push_str(&value.to_string()),
        Value::String(value) => output.push_str(&serde_json::to_string(value).unwrap()),
        Value::Array(values) => {
            output.push('[');
            for (index, value) in values.iter().enumerate() {
                if index > 0 {
                    output.push(',');
                }
                canonical(value, output);
            }
            output.push(']');
        }
        Value::Object(values) => {
            output.push('{');
            let mut values = values.iter().collect::<Vec<_>>();
            values.sort_unstable_by_key(|(key, _)| *key);
            for (index, (key, value)) in values.into_iter().enumerate() {
                if index > 0 {
                    output.push(',');
                }
                output.push_str(&serde_json::to_string(key).unwrap());
                output.push(':');
                canonical(value, output);
            }
            output.push('}');
        }
    }
}

fn canonical_bytes(value: &Value) -> Vec<u8> {
    let mut output = String::new();
    canonical(value, &mut output);
    output.push('\n');
    output.into_bytes()
}

fn refresh_generation(input: &mut RecognitionInput, ledger: Value) {
    input.ledger_bytes = canonical_bytes(&ledger);
    let mut manifest: Value = serde_json::from_slice(&input.generation_manifest).unwrap();
    let ledger_hash = sha256(&input.ledger_bytes);
    let digest = ledger_hash.strip_prefix("sha256:").unwrap();
    manifest["members"]["classified_source_ledger"] = serde_json::json!({
        "artifact_ref": format!("sha256/{}/{}.json", &digest[..2], digest),
        "value_hash": ledger_hash,
    });
    manifest.as_object_mut().unwrap().remove("generation_ref");
    let mut identity = canonical_bytes(&manifest);
    identity.pop();
    manifest["generation_ref"] = Value::String(sha256(&identity));
    input.generation_manifest = canonical_bytes(&manifest);
}

fn refresh_decoded_member(input: &mut RecognitionInput, decoded: Vec<u8>) {
    input.decoded_source = decoded;
    let decoded_hash = sha256(&input.decoded_source);
    let decoded_digest = decoded_hash.strip_prefix("sha256:").unwrap();
    let decoded_member = serde_json::json!({
        "artifact_ref": format!("sha256/{}/{}.txt", &decoded_digest[..2], decoded_digest),
        "value_hash": decoded_hash,
    });
    let mut ledger: Value = serde_json::from_slice(&input.ledger_bytes).unwrap();
    ledger["decoded_source"]["artifact_ref"] = decoded_member["artifact_ref"].clone();
    ledger["decoded_source"]["value_hash"] = decoded_member["value_hash"].clone();
    ledger["decoded_source"]["bytes"] = Value::from(input.decoded_source.len());
    let mut manifest: Value = serde_json::from_slice(&input.generation_manifest).unwrap();
    manifest["members"]["decoded_source"] = decoded_member;
    input.generation_manifest = canonical_bytes(&manifest);
    refresh_generation(input, ledger);
}

fn assert_unavailable_without_totals(input: RecognitionInput, error: &str) {
    let analysis = analyze_recognition(input);
    assert_eq!(analysis.record.status, RecognitionStatus::Unavailable);
    assert!(analysis.record.eligible_bytes.is_none());
    assert!(analysis.record.recognized.is_none());
    assert!(analysis.record.errors.iter().any(|actual| actual == error));
}

fn assert_schema_valid(record: &impl serde::Serialize) {
    let schema: Value = serde_json::from_slice(WORK_SCHEMA).unwrap();
    let validator = jsonschema::validator_for(&schema).unwrap();
    let value = serde_json::to_value(record).unwrap();
    validator
        .validate(&value)
        .unwrap_or_else(|error| panic!("record violates work schema: {error}; record={value}"));
}

#[test]
fn fully_typed_source_is_fully_recognized_and_accounted() {
    let analysis = analyze_recognition(input("本文｜青梅《おうめ》\n"));
    assert_eq!(
        analysis.record.status,
        RecognitionStatus::Ok,
        "{:?}",
        analysis.record.errors
    );
    assert_eq!(
        analysis.record.recognized_bytes,
        analysis.record.eligible_bytes
    );
    assert_eq!(
        analysis.record.accounted_bytes,
        analysis.record.eligible_bytes
    );
    assert_eq!(analysis.record.semantic_gap_bytes, Some(0));
    assert_eq!(analysis.record.unaccounted_bytes, Some(0));
}

#[test]
fn overlapping_approved_facts_are_unioned_without_double_counting() {
    let analysis = analyze_recognition(input("A\r\nB"));
    assert_eq!(
        analysis.record.status,
        RecognitionStatus::Ok,
        "{:?}",
        analysis.record.errors
    );
    assert_eq!(analysis.record.eligible_bytes, Some(4));
    assert_eq!(analysis.record.recognized_bytes, Some(4));
    assert_eq!(analysis.record.accounted_bytes, Some(4));
}

#[test]
fn opaque_source_is_accounted_but_not_recognized() {
    let analysis = analyze_recognition(input("［＃未知］"));
    assert_eq!(
        analysis.record.status,
        RecognitionStatus::Ok,
        "{:?}",
        analysis.record.errors
    );
    assert_eq!(
        analysis.record.accounted_bytes,
        analysis.record.eligible_bytes
    );
    assert!(analysis.record.recognized_bytes < analysis.record.accounted_bytes);
    assert_eq!(analysis.record.unaccounted_bytes, Some(0));
    assert!(analysis.record.semantic_gap_bytes.unwrap() > 0);
}

#[test]
fn recovered_source_remains_a_semantic_gap() {
    let analysis = analyze_recognition(input("｜"));
    assert_eq!(
        analysis.record.status,
        RecognitionStatus::Ok,
        "{:?}",
        analysis.record.errors
    );
    assert_eq!(
        analysis.record.accounted_bytes,
        analysis.record.eligible_bytes
    );
    assert_eq!(analysis.record.recognized_bytes, Some(0));
    assert_eq!(
        analysis.record.semantic_gap_bytes,
        analysis.record.eligible_bytes
    );
}

#[test]
fn invalid_target_identity_is_unavailable() {
    let mut input = input("本文");
    let mut ledger: Value = serde_json::from_slice(&input.ledger_bytes).unwrap();
    ledger["entries"][0]["target_identity"]["value_hash"] =
        Value::String(format!("sha256:{}", "0".repeat(64)));
    refresh_generation(&mut input, ledger);
    assert_unavailable_without_totals(input, "capture-generation-invalid");
}

#[test]
fn target_pointer_must_resolve_within_the_authenticated_target() {
    let mut input = input("本文");
    let mut ledger: Value = serde_json::from_slice(&input.ledger_bytes).unwrap();
    ledger["entries"][0]["target_pointer"] = Value::String("/absent".to_owned());
    refresh_generation(&mut input, ledger);
    assert_unavailable_without_totals(input, "ledger-evidence-invalid");
}

#[test]
fn policy_does_not_authorize_unknown_fact_rows() {
    let mut input = input("本文");
    let mut ledger: Value = serde_json::from_slice(&input.ledger_bytes).unwrap();
    ledger["entries"][0]["construct_id"] = Value::String("invented_construct".to_owned());
    refresh_generation(&mut input, ledger);
    assert_unavailable_without_totals(input, "ledger-policy-row-unknown");
}

#[test]
fn supplied_policy_must_be_the_authenticated_closed_abc_value() {
    let mut input = input("本文");
    let mut policy: Value = serde_json::from_slice(&input.policy_bytes).unwrap();
    policy["rules"][0]["construct_id"] = Value::String("invented_construct".to_owned());
    input.policy_bytes = serde_json::to_vec(&policy).unwrap();
    assert_unavailable_without_totals(input, "classified-source-policy-invalid");
}

#[test]
fn generation_qualification_and_work_mismatches_are_unavailable() {
    let mut generation = input("本文");
    generation.generation_manifest.push(b' ');
    assert_unavailable_without_totals(generation, "capture-generation-invalid");

    let mut qualification = input("本文");
    qualification.qualification_identity_ref = format!("sha256:{}", "0".repeat(64));
    assert_unavailable_without_totals(qualification, "qualification-identity-mismatch");

    let mut work = input("本文");
    work.work_id = "sha256:not-the-work".to_owned();
    assert_unavailable_without_totals(work, "work-identity-mismatch");
}

#[test]
fn invalid_utf8_and_interval_boundaries_are_unavailable() {
    let mut utf8 = input("本文");
    refresh_decoded_member(&mut utf8, vec![0xff]);
    assert_unavailable_without_totals(utf8, "decoded-source-invalid-utf8");

    for mutation in ["empty", "reversed", "mid-codepoint", "out-of-bounds"] {
        let mut input = input("本文");
        let mut ledger: Value = serde_json::from_slice(&input.ledger_bytes).unwrap();
        match mutation {
            "empty" => {
                ledger["entries"][0]["start"] = Value::from(1);
                ledger["entries"][0]["end"] = Value::from(1);
            }
            "reversed" => {
                ledger["entries"][0]["start"] = Value::from(2);
                ledger["entries"][0]["end"] = Value::from(1);
            }
            "mid-codepoint" => ledger["entries"][0]["end"] = Value::from(1),
            "out-of-bounds" => ledger["entries"][0]["end"] = Value::from(99),
            _ => unreachable!(),
        }
        refresh_generation(&mut input, ledger);
        assert_unavailable_without_totals(input, "ledger-interval-invalid");
    }
}

#[test]
fn successful_analysis_does_not_mutate_or_reinterpret_legacy_p1() {
    let input = input("本文");
    let generation = CaptureGeneration {
        decoded_source: input.decoded_source.clone(),
        parser_output: input.parser_output.clone(),
        raw_diagnostics: input.raw_diagnostics.clone(),
        classified_source_ledger: input.ledger_bytes.clone(),
        manifest: input.generation_manifest.clone(),
    };
    assert!(ab_capture::verify_capture_generation(&generation).is_ok());
    assert_eq!(
        analyze_recognition(input).record.status,
        RecognitionStatus::Ok
    );
}

#[test]
fn available_and_unavailable_records_validate_against_the_abc_protocol() {
    let available = analyze_recognition(input("本文")).record;
    assert_schema_valid(&available);

    let mut invalid = input("本文");
    invalid.work_id = "different-work".to_owned();
    let unavailable = analyze_recognition(invalid).record;
    assert_schema_valid(&unavailable);
}

#[test]
fn malformed_untrusted_metadata_always_yields_a_schema_valid_unavailable_record() {
    let mut cases = Vec::new();

    let mut malformed_generation = input("本文");
    let mut manifest: Value =
        serde_json::from_slice(&malformed_generation.generation_manifest).unwrap();
    manifest["generation_ref"] = Value::String("not-a-hash".to_owned());
    malformed_generation.generation_manifest = canonical_bytes(&manifest);
    cases.push(malformed_generation);

    let mut missing_generation = input("本文");
    let mut manifest: Value =
        serde_json::from_slice(&missing_generation.generation_manifest).unwrap();
    manifest.as_object_mut().unwrap().remove("generation_ref");
    missing_generation.generation_manifest = canonical_bytes(&manifest);
    cases.push(missing_generation);

    let mut malformed_manifest_qualification = input("本文");
    let mut manifest: Value =
        serde_json::from_slice(&malformed_manifest_qualification.generation_manifest).unwrap();
    manifest["qualification_identity_ref"] = Value::String("not-a-hash".to_owned());
    malformed_manifest_qualification.generation_manifest = canonical_bytes(&manifest);
    cases.push(malformed_manifest_qualification);

    let mut empty_manifest_work_id = input("本文");
    let mut manifest: Value =
        serde_json::from_slice(&empty_manifest_work_id.generation_manifest).unwrap();
    manifest["work_id"] = Value::String(String::new());
    empty_manifest_work_id.generation_manifest = canonical_bytes(&manifest);
    cases.push(empty_manifest_work_id);

    let mut malformed_policy = input("本文");
    let mut ledger: Value = serde_json::from_slice(&malformed_policy.ledger_bytes).unwrap();
    ledger["policy_hash"] = Value::String("not-a-hash".to_owned());
    refresh_generation(&mut malformed_policy, ledger);
    cases.push(malformed_policy);

    let mut missing_policy = input("本文");
    let mut ledger: Value = serde_json::from_slice(&missing_policy.ledger_bytes).unwrap();
    ledger.as_object_mut().unwrap().remove("policy_hash");
    refresh_generation(&mut missing_policy, ledger);
    cases.push(missing_policy);

    let mut malformed_qualification = input("本文");
    malformed_qualification.qualification_identity_ref = "not-a-hash".to_owned();
    cases.push(malformed_qualification);

    let mut empty_locator = input("本文");
    empty_locator.ledger_locator.clear();
    cases.push(empty_locator);

    let mut empty_work_id = input("本文");
    empty_work_id.work_id.clear();
    cases.push(empty_work_id);

    for case in cases {
        let record = analyze_recognition(case).record;
        assert_eq!(record.status, RecognitionStatus::Unavailable);
        assert_schema_valid(&record);
    }
}
