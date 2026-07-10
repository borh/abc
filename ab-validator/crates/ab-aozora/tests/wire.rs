//! Wire-contract tests for the `ab-aozora` binary: `--version` identity
//! fields, one AAT document per stdin, and exit 1 / empty-stdout on
//! usage errors.
use std::io::Write;
use std::process::{Command, Stdio};

fn bin() -> &'static str {
    env!("CARGO_BIN_EXE_ab-aozora")
}

#[test]
fn version_carries_the_identity_fields() {
    let out = Command::new(bin()).arg("--version").output().unwrap();
    assert!(out.status.success());
    let text = String::from_utf8(out.stdout).unwrap();
    for field in ["ab-aozora", "aat-schema 1", "wire-schema 2", "git "] {
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
