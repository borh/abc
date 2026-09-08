//! Wire-contract tests for the `ab-aozora` binary: `--version` identity
//! fields, one AAT document per stdin, and exit 1 / empty-stdout on
//! usage errors.
use std::io::Write;
use std::process::{Command, Stdio};
use std::str;

fn bin() -> &'static str {
    env!("CARGO_BIN_EXE_ab-aozora")
}

#[test]
fn version_carries_the_identity_fields() {
    let out = Command::new(bin()).arg("--version").output().unwrap();
    assert!(out.status.success());
    let text = String::from_utf8(out.stdout).unwrap();
    for field in [
        "ab-aozora 0.6.0",
        "aat-schema 2",
        "facade 0.3.0",
        "diagnostics-schema 3",
        "git ",
    ] {
        assert!(text.contains(field), "--version missing {field:?}: {text}");
    }
}

#[test]
fn emits_one_aat_document_with_rotated_identity() {
    let mut child = Command::new(bin())
        .args(["--mode", "aat"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all("底本：テスト\n".as_bytes())
        .unwrap();
    let out = child.wait_with_output().unwrap();
    assert!(out.status.success());
    let doc: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(doc["meta"]["adapter"], "ab-aozora");
}

#[test]
fn usage_errors_exit_1_with_empty_stdout() {
    for argv in [vec!["--mode", "html"], vec!["--frobnicate"]] {
        let out = Command::new(bin()).args(&argv).output().unwrap();
        assert_eq!(out.status.code(), Some(1), "argv {argv:?}");
        assert!(out.stdout.is_empty(), "partial stdout on {argv:?}");
        assert!(!out.stderr.is_empty());
    }
}

#[test]
fn mode_diagnostics_emits_schema3_envelope() {
    let mut child = Command::new(bin())
        .args(["--mode", "diagnostics"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all("あ［＃ここから\n".as_bytes())
        .unwrap();
    let out = child.wait_with_output().unwrap();
    assert!(out.status.success());
    let doc: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(doc["schemaVersion"], 3);
    assert!(
        doc["data"]
            .as_array()
            .unwrap()
            .iter()
            .all(|e| e["code"].is_string())
    );
}

#[test]
fn production_diagnostics_accept_literal_private_use_source() {
    let actual = ab_aat::diagnostics_json_from_bytes("本文\u{e001}終わり".as_bytes())
        .expect("diagnostics capture");
    let diagnostic: serde_json::Value = serde_json::from_slice(&actual).unwrap();
    assert_eq!(diagnostic["schemaVersion"], 3);
    assert_eq!(diagnostic["data"], serde_json::json!([]));
}

#[test]
fn mode_diagnostics_no_partial_stdout_contract_holds() {
    // Unknown mode still exits 1 with empty stdout (contract unchanged).
    let out = Command::new(bin())
        .args(["--mode", "nodes"])
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(1));
    assert!(out.stdout.is_empty());
}
