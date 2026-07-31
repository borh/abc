use std::path::{Path, PathBuf};
use std::process::Command;

use serde_json::Value;

const HASH: &str = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";

// Workspace-relative, never through the checkout's parent: the workspace
// directory is not guaranteed to be NAMED `ab-validator` (the nix test
// sandbox stages it as `source`), only to contain this crate and to have
// the abc tree as a sibling.
fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

#[test]
fn qualify_preserves_convert_parser_ir_bytes() {
    let root = workspace_root();
    let crate_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let temp = tempfile::tempdir().unwrap();
    let policy = temp.path().join("policy.json");
    std::fs::write(
        &policy,
        serde_json::to_vec(&serde_json::json!({
            "policy_hash": HASH,
            "expected_work_ids": ["fixture"]
        }))
        .unwrap(),
    )
    .unwrap();

    let aat = crate_root.join("tests/fixtures/nested-sentence-basic.aat.json");
    let mapping = root.join("data/aat-to-parser-ir-mapping-v1.json");
    let abc_root = root.join("../abc");
    let converted = temp.path().join("converted.json");
    let divergence = temp.path().join("divergence.json");
    let qualified = temp.path().join("qualified.json");
    let ledger = temp.path().join("ledger.json");
    let record = temp.path().join("record.json");
    let binary = env!("CARGO_BIN_EXE_ab-aat-to-parser-ir");

    let convert = Command::new(binary)
        .args(["convert", "--aat"])
        .arg(&aat)
        .arg("--mapping")
        .arg(&mapping)
        .arg("--abc-root")
        .arg(&abc_root)
        .arg("--parser-ir-out")
        .arg(&converted)
        .arg("--divergence-out")
        .arg(&divergence)
        .output()
        .unwrap();
    assert!(
        convert.status.success(),
        "{}",
        String::from_utf8_lossy(&convert.stderr)
    );

    let qualify = Command::new(binary)
        .args(["qualify", "--aat"])
        .arg(&aat)
        .arg("--mapping")
        .arg(&mapping)
        .arg("--abc-root")
        .arg(&abc_root)
        .arg("--work-id")
        .arg("fixture")
        .arg("--qualification-identity-ref")
        .arg(HASH)
        .arg("--policy")
        .arg(&policy)
        .arg("--parser-ir-out")
        .arg(&qualified)
        .arg("--ledger-out")
        .arg(&ledger)
        .arg("--record-out")
        .arg(&record)
        .output()
        .unwrap();
    assert!(
        qualify.status.success(),
        "{}",
        String::from_utf8_lossy(&qualify.stderr)
    );

    assert_eq!(
        std::fs::read(converted).unwrap(),
        std::fs::read(qualified).unwrap()
    );
    assert!(!ledger.exists());
    let record: Value = serde_json::from_slice(&std::fs::read(record).unwrap()).unwrap();
    assert_eq!(record["status"], "schema_valid");
    assert_eq!(record["policy_hash"], HASH);
}
