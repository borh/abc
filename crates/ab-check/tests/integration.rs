use std::{fs, path::Path, process::Command};

use ab_check::check::schema_validator;
use serde_json::Value;

#[test]
fn aat_schema_accepts_nested_fixture_and_rejects_event_nodes() {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let schema = schema_validator().unwrap();
    let valid: Value = serde_json::from_str(
        &fs::read_to_string(manifest.join("../../data/fixtures/aat-valid-nested.json")).unwrap(),
    )
    .unwrap();
    let invalid: Value = serde_json::from_str(
        &fs::read_to_string(manifest.join("../../data/fixtures/aat-invalid-event-node.json"))
            .unwrap(),
    )
    .unwrap();

    assert!(schema.validate(&valid).is_ok());
    assert!(schema.validate(&invalid).is_err());
}

#[test]
fn test_adapter_ruby_output_passes_core_properties() {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let adapter = manifest.join("../../adapters/test-adapter/test-adapter");
    let txt = manifest.join("fixtures/test_ruby.txt");
    let aat = std::env::temp_dir().join(format!("test-ruby-{}.aat.json", std::process::id()));

    let input = fs::File::open(&txt).unwrap();
    let output = fs::File::create(&aat).unwrap();
    let status = Command::new("bash")
        .arg(&adapter)
        .arg("--mode")
        .arg("aat")
        .stdin(input)
        .stdout(output)
        .status()
        .unwrap();
    assert!(status.success());

    let check = Command::new(env!("CARGO_BIN_EXE_ab-check"))
        .arg("--txt")
        .arg(&txt)
        .arg("--aat")
        .arg(&aat)
        .output()
        .unwrap();
    assert!(check.status.success());
    let report: serde_json::Value = serde_json::from_slice(&check.stdout).unwrap();
    assert_eq!(report["results"]["schema_valid"]["pass"], true);
    assert_eq!(report["results"]["ruby_completeness"]["pass"], true);

    let _ = fs::remove_file(aat);
}

#[test]
fn duplicate_ruby_base_fails_visible_projection() {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let txt = manifest.join("fixtures/test_ruby.txt");
    let bad = manifest.join("fixtures/test_bad_duplicate_ruby.aat.json");
    let check = Command::new(env!("CARGO_BIN_EXE_ab-check"))
        .arg("--txt")
        .arg(&txt)
        .arg("--aat")
        .arg(&bad)
        .output()
        .unwrap();
    assert!(check.status.success());
    let report: serde_json::Value = serde_json::from_slice(&check.stdout).unwrap();
    assert_eq!(report["results"]["visible_text_body_order"]["pass"], false);
}
