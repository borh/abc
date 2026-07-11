use std::{fs, path::Path, process::Command};

#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;

use ab_check::check::{check_single, schema_validator_for_version};
use serde_json::Value;

#[test]
fn aat_schema_accepts_nested_fixture_and_rejects_event_nodes() {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let schema = schema_validator_for_version(1).unwrap();
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
    let schema = schema_validator_for_version(1).unwrap();
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
                "parse_body_strategy": "separator_fallback",
                "parser_body_bytes": 10,
                "tokenized_count": 11,
                "retokenized_count": 12,
                "parser_nodes": 13,
                "parser_normalized_nodes": 14,
                "source_supplement_nodes": 15,
                "source_fallback_nodes": 16,
                "fallback_used": false,
                "fallback_reason": "none"
            }
        }
    });

    assert!(schema.validate(&value).is_ok());
}

#[test]
fn aat_schema_accepts_semantic_summary_metadata() {
    let schema = schema_validator_for_version(1).unwrap();
    let value = serde_json::json!({
        "version": 1,
        "work_id": "semantic_summary_fixture",
        "blocks": [
            {
                "kind": "paragraph",
                "content": [{"kind": "ruby", "base": "吾輩", "reading": "わがはい", "direction": "right"}]
            }
        ],
        "meta": {
            "adapter": "aozora-rs",
            "adapter_version": "test",
            "source_encoding": "utf-8",
            "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
            "parse_complete": true,
            "warnings": [],
            "semantic_summary": {
                "syntax": {
                    "ruby.basic": [
                        {
                            "kind": "ruby",
                            "value": {
                                "base_projection": "吾輩",
                                "reading": "わがはい",
                                "placement": "right"
                            },
                            "provenance": "parser"
                        }
                    ]
                }
            }
        }
    });

    assert!(schema.validate(&value).is_ok());
}

#[test]
fn check_single_accepts_deeply_nested_aat_json() {
    let temp = tempfile::tempdir().unwrap();
    let txt_path = temp.path().join("source.txt");
    let aat_path = temp.path().join("deep.aat.json");
    fs::write(&txt_path, "本文").unwrap();

    let mut inline = r#"{"kind":"text","value":"本文"}"#.to_owned();
    for _ in 0..140 {
        inline = format!(r#"{{"kind":"style","style_type":"nested","content":[{inline}]}}"#);
    }
    let aat = format!(
        r#"{{
            "version": 1,
            "work_id": "deep_fixture",
            "blocks": [
                {{"kind": "paragraph", "content": [{inline}]}}
            ],
            "meta": {{
                "adapter": "fixture",
                "adapter_version": "fixture",
                "source_encoding": "utf-8",
                "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
                "parse_complete": true,
                "warnings": []
            }}
        }}"#
    );
    fs::write(&aat_path, aat).unwrap();

    let report_path = temp.path().join("report.json");
    let report = check_single(&txt_path, &aat_path, Some(&report_path)).unwrap();

    assert_eq!(report.work_id, "deep_fixture");
    assert!(report.results["schema_valid"].pass);
}

#[test]
fn aat_schema_documents_raw_source_marker_extensions() {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let schema_doc: Value = serde_json::from_str(
        &fs::read_to_string(manifest.join("../../data/aat-schema.json")).unwrap(),
    )
    .unwrap();
    let raw_properties = &schema_doc["$defs"]["raw"]["properties"];

    assert_eq!(
        raw_properties["x-provenance"]["enum"],
        serde_json::json!(["parser-derived", "source-derived", "adapter-derived"])
    );
    assert_eq!(
        raw_properties["x-source-marker-kind"]["type"],
        serde_json::json!("string")
    );

    let schema = schema_validator_for_version(1).unwrap();
    let value = serde_json::json!({
        "version": 1,
        "work_id": "raw_source_marker_fixture",
        "blocks": [
            {
                "kind": "paragraph",
                "content": [
                    {
                        "kind": "raw",
                        "source": "［＃未知の注記］",
                        "x-provenance": "source-derived",
                        "x-source-marker-kind": "CommandFullwidth",
                        "span": {
                            "line_start": 1,
                            "line_end": 1,
                            "byte_start": 0,
                            "byte_end": 24
                        }
                    }
                ]
            }
        ],
        "meta": {
            "adapter": "fixture",
            "adapter_version": "fixture",
            "source_encoding": "utf-8",
            "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
            "parse_complete": true,
            "warnings": []
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
    let files = corpus.join("cards/000001/files");
    fs::create_dir_all(&files).unwrap();
    fs::write(files.join("1_ruby_1.txt"), "吾輩《わがはい》は猫である。").unwrap();
    let index = temp.path().join("index.json");
    fs::write(
        &index,
        r#"{"works":[{"id":"000001_1","txt_path":"cards/000001/files/1_ruby_1.txt","features":["ruby"]}]}"#,
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

#[test]
fn batch_mode_does_not_emit_aat_for_aozora_tools_reference_text() {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let temp = tempfile::tempdir().unwrap();
    let adapter = temp.path().join("test-adapter");
    write_portable_test_adapter(
        &manifest.join("../../adapters/test-adapter/test-adapter"),
        &adapter,
    );
    let corpus = temp.path().join("corpus");
    let tools = corpus.join("tools");
    fs::create_dir_all(&tools).unwrap();
    fs::write(tools.join("JISTABLE.TXT"), "JIS漢字コード表 (JIS X 0208)").unwrap();
    let index = temp.path().join("index.json");
    fs::write(
        &index,
        r#"{"works":[{"id":"JISTABLE","txt_path":"tools/JISTABLE.TXT","features":["jis_code"]}]}"#,
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

    assert!(!reports.join("test-adapter").exists());
    assert!(!aat_output.join("test-adapter").exists());
}

#[test]
fn batch_mode_does_not_emit_aat_for_non_card_reference_text() {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let temp = tempfile::tempdir().unwrap();
    let adapter = temp.path().join("test-adapter");
    write_portable_test_adapter(
        &manifest.join("../../adapters/test-adapter/test-adapter"),
        &adapter,
    );
    let corpus = temp.path().join("corpus");
    let reference = corpus.join("reference");
    fs::create_dir_all(&reference).unwrap();
    fs::write(reference.join("ruby_reference.txt"), "基準《きじゅん》資料").unwrap();
    let index = temp.path().join("index.json");
    fs::write(
        &index,
        r#"{"works":[{"id":"ruby_reference","txt_path":"reference/ruby_reference.txt","features":["ruby"]}]}"#,
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

    assert!(!reports.join("test-adapter").exists());
    assert!(!aat_output.join("test-adapter").exists());
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
