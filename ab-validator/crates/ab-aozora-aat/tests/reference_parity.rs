//! Byte parity against the frozen adapter, modulo the two identity
//! pointers. Env-gated: set `AB_REFERENCE_ADAPTER_BIN` (a built
//! adapters/aozora binary) and `AB_AOZORA_BIN` (the pinned upstream aozora)
//! or the test skips (prints SKIP, passes) — mirrors the shim goldens'
//! env-gating so `cargo test --workspace` stays hermetic.
use std::env;
use std::fs;
use std::io::Write;
use std::process::{Command, Stdio};

fn normalize(mut bytes: Vec<u8>) -> Vec<u8> {
    let doc: serde_json::Value = serde_json::from_slice(&bytes).unwrap();
    let meta = doc.get("meta").and_then(|m| m.as_object()).unwrap();
    for key in ["adapter", "adapter_version"] {
        let value = serde_json::to_string(meta.get(key).unwrap()).unwrap();
        let needle = format!("\"{key}\":{value}");
        let hay = String::from_utf8(bytes.clone()).unwrap();
        assert_eq!(
            hay.matches(&needle).count(),
            1,
            "expected exactly one serialized occurrence of {needle}"
        );
        bytes = hay
            .replacen(&needle, &format!("\"{key}\":\"__X__\""), 1)
            .into_bytes();
    }
    bytes
}

#[test]
fn byte_parity_with_frozen_adapter_modulo_identity() {
    let (Ok(reference_bin), Ok(_)) = (
        env::var("AB_REFERENCE_ADAPTER_BIN"),
        env::var("AB_AOZORA_BIN"),
    ) else {
        eprintln!("SKIP: AB_REFERENCE_ADAPTER_BIN / AB_AOZORA_BIN not set");
        return;
    };
    for entry in fs::read_dir(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/data"
    ))
    .unwrap()
    {
        let path = entry.unwrap().path();
        let bytes = fs::read(&path).unwrap();
        let mut child = Command::new(&reference_bin)
            .args(["--mode", "aat"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();
        child.stdin.take().unwrap().write_all(&bytes).unwrap();
        let reference = child.wait_with_output().unwrap();
        assert!(reference.status.success(), "reference failed on {path:?}");
        let ours = ab_aozora_aat::aat_json_from_bytes(&bytes).unwrap();
        assert_eq!(
            normalize(ours),
            normalize(reference.stdout),
            "byte divergence on {path:?}"
        );
    }
}
