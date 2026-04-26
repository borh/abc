use std::{fs, path::Path, process::Command};

#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;

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
fn aat_schema_accepts_adapter_metrics_metadata() {
    let schema = schema_validator().unwrap();
    let value = serde_json::json!({
        "version": 1,
        "work_id": "metrics_fixture",
        "blocks": [
            {
                "kind": "paragraph",
                "content": [{"kind": "text", "value": "本文"}]
            }
        ],
        "meta": {
            "adapter": "aozora-rs",
            "adapter_version": "test",
            "source_encoding": "utf-8",
            "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
            "parse_complete": true,
            "warnings": [],
            "metrics": {
                "decode_ms": 1.0,
                "body_selection_ms": 2.0,
                "tokenize_ms": 3.0,
                "scopenize_ms": 4.0,
                "retokenize_ms": 5.0,
                "aat_build_ms": 6.0,
                "projection_check_ms": 7.0,
                "fallback_build_ms": 0.0,
                "source_bytes": 8,
                "validation_body_bytes": 9,
                "parser_body_bytes": 10,
                "tokenized_count": 11,
                "retokenized_count": 12,
                "fallback_used": false,
                "fallback_reason": "none"
            }
        }
    });

    assert!(schema.validate(&value).is_ok());
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

#[test]
fn batch_mode_can_persist_mutated_aat_output() {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let temp = tempfile::tempdir().unwrap();
    let adapter = temp.path().join("test-adapter");
    write_portable_test_adapter(
        &manifest.join("../../adapters/test-adapter/test-adapter"),
        &adapter,
    );
    let corpus = temp.path().join("corpus");
    fs::create_dir(&corpus).unwrap();
    fs::write(corpus.join("1_ruby_1.txt"), "吾輩《わがはい》は猫である。").unwrap();
    let index = temp.path().join("index.json");
    fs::write(
        &index,
        r#"{"works":[{"id":"000001_1","txt_path":"1_ruby_1.txt","features":["ruby"]}]}"#,
    )
    .unwrap();
    let reports = temp.path().join("reports");
    let aat_output = temp.path().join("aat");

    let check = Command::new(env!("CARGO_BIN_EXE_ab-check"))
        .arg("--index")
        .arg(&index)
        .arg("--corpus")
        .arg(&corpus)
        .arg("--adapter")
        .arg(&adapter)
        .arg("--output")
        .arg(&reports)
        .arg("--aat-output")
        .arg(&aat_output)
        .output()
        .unwrap();
    assert!(
        check.status.success(),
        "{}",
        String::from_utf8_lossy(&check.stderr)
    );

    let report_path = only_json_file(&reports.join("test-adapter"));
    let aat_path = only_json_file(&aat_output.join("test-adapter"));
    assert!(report_path.exists());
    assert!(aat_path.exists());
    let persisted_aat: Value = serde_json::from_slice(&fs::read(aat_path).unwrap()).unwrap();
    assert_eq!(persisted_aat["work_id"], "000001_1");
    assert_eq!(
        persisted_aat["meta"]["adapter_version"],
        "test-adapter 0.0.1 test"
    );
}

fn write_portable_test_adapter(source: &Path, destination: &Path) {
    let script = fs::read_to_string(source).unwrap();
    let bash = Command::new("bash")
        .arg("-c")
        .arg("command -v bash")
        .output()
        .ok()
        .and_then(|output| {
            output
                .status
                .success()
                .then(|| String::from_utf8_lossy(&output.stdout).trim().to_owned())
        })
        .filter(|path| !path.is_empty())
        .unwrap_or_else(|| "/usr/bin/env bash".to_owned());
    fs::write(
        destination,
        script.replacen("#!/usr/bin/env bash", &format!("#!{bash}"), 1),
    )
    .unwrap();
    #[cfg(unix)]
    {
        let mut permissions = fs::metadata(destination).unwrap().permissions();
        permissions.set_mode(0o755);
        fs::set_permissions(destination, permissions).unwrap();
    }
}

fn only_json_file(dir: &Path) -> std::path::PathBuf {
    let files = fs::read_dir(dir)
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.extension().and_then(|extension| extension.to_str()) == Some("json"))
        .collect::<Vec<_>>();
    assert_eq!(files.len(), 1);
    files.into_iter().next().unwrap()
}
