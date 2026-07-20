use std::fs;

use ab_parser_study_report::evidence::{EvidenceError, load_index};
use sha2::{Digest, Sha256};
use tempfile::TempDir;

fn record(candidate: &str, reason: &str) -> serde_json::Value {
    serde_json::json!({
        "schema_id": "https://w3id.org/abc/schemas/parser-study-axis-evidence-v1",
        "schema_version": 1,
        "study_id": "aozora-parser-neutral-comparison-2026-07",
        "candidate": candidate,
        "axis": "diagnostics",
        "measurement_mode": "native",
        "parser_revision": "revision",
        "adapter_revision": null,
        "corpus_hash": format!("sha256:{}", "0".repeat(64)),
        "required_inputs": [
            {"role": "source_markup", "artifact": "diagnostic_fixture", "state": "absent", "reason": reason},
            {"role": "third_party_capture", "artifact": "diagnostic_fixture_capture", "state": "absent", "reason": reason}
        ],
        "metrics": [
            {"metric": "diagnostic_presence", "disposition": "unavailable", "reason": reason}
        ],
        "case_witnesses": []
    })
}

fn write_record(root: &TempDir, locator: &str, value: &serde_json::Value) -> serde_json::Value {
    let bytes = serde_json::to_vec(value).expect("serialize record");
    let path = root.path().join(locator);
    fs::create_dir_all(path.parent().expect("record parent")).expect("create parent");
    fs::write(&path, &bytes).expect("write record");
    serde_json::json!({
        "candidate": value["candidate"],
        "axis": value["axis"],
        "measurement_mode": value["measurement_mode"],
        "record_ref": {
            "sha256": format!("sha256:{:x}", Sha256::digest(&bytes)),
            "bytes": bytes.len(),
            "media_type": "application/json"
        },
        "locator": locator
    })
}

fn index(records: Vec<serde_json::Value>) -> String {
    serde_json::to_string(&serde_json::json!({
        "schema_id": "https://w3id.org/abc/schemas/parser-study-evidence-index-v1",
        "schema_version": 1,
        "study_id": "aozora-parser-neutral-comparison-2026-07",
        "records": records
    }))
    .expect("serialize index")
}

#[test]
fn index_rehashes_members_and_rejects_tampering_and_escaping_paths() {
    let root = TempDir::new().expect("temporary evidence root");
    let member = write_record(
        &root,
        "records/aozora.json",
        &record("aozora", "not captured"),
    );
    let index_json = index(vec![member]);
    assert_eq!(
        load_index(root.path(), &index_json)
            .expect("valid index")
            .len(),
        1
    );

    fs::write(root.path().join("records/aozora.json"), b"tampered").expect("tamper record");
    assert!(matches!(
        load_index(root.path(), &index_json),
        Err(EvidenceError::ContentMismatch { .. })
    ));

    let escaping = index(vec![serde_json::json!({
        "candidate": "aozora",
        "axis": "diagnostics",
        "measurement_mode": "native",
        "record_ref": {"sha256": format!("sha256:{}", "0".repeat(64)), "bytes": 0, "media_type": "application/json"},
        "locator": "../escape.json"
    })]);
    assert!(matches!(
        load_index(root.path(), &escaping),
        Err(EvidenceError::UnsafeLocator(_))
    ));
}

#[test]
fn duplicate_candidate_axis_mode_is_rejected_even_with_distinct_files() {
    let root = TempDir::new().expect("temporary evidence root");
    let value = record("aozora", "not captured");
    let first = write_record(&root, "records/first.json", &value);
    let second = write_record(&root, "records/second.json", &value);
    assert!(matches!(
        load_index(root.path(), &index(vec![first, second])),
        Err(EvidenceError::DuplicateRecordIdentity { .. })
    ));
}
