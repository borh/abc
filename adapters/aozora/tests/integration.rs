use std::{
    path::PathBuf,
    process::{Command, Stdio},
};

fn adapter_bin() -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("target/release/aozora-adapter");
    if !path.exists() {
        path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push("target/debug/aozora-adapter");
    }
    path
}

fn run_aat(source: &str) -> serde_json::Value {
    let mut child = Command::new(adapter_bin())
        .arg("--mode")
        .arg("aat")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("spawn aozora-adapter");
    {
        use std::io::Write;
        child
            .stdin
            .as_mut()
            .expect("stdin")
            .write_all(source.as_bytes())
            .expect("write source");
    }
    let output = child.wait_with_output().expect("adapter output");
    assert!(
        output.status.success(),
        "adapter failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).expect("AAT JSON")
}

#[test]
fn version_mentions_upstream_aozora() {
    let output = Command::new(adapter_bin())
        .arg("--version")
        .output()
        .expect("version output");
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.starts_with("aozora-adapter 0.1.0 aozora "));
}

#[test]
fn emits_schema_valid_aat_for_core_constructs() {
    let source = [
        "｜青梅《おうめ》",
        "※［＃「口＋世」、U+546D］",
        "［＃返り点一］",
        "［＃ここから2字下げ］",
        "字下げ本文",
        "［＃ここで字下げ終わり］",
        "［＃改ページ］",
    ]
    .join("\n");
    let aat = run_aat(&source);

    assert_eq!(aat["version"], 1);
    assert_eq!(aat["meta"]["adapter"], "aozora");
    assert!(
        aat["meta"]["adapter_version"]
            .as_str()
            .unwrap()
            .contains("aozora ")
    );
    assert_eq!(aat["meta"]["parse_complete"], true);

    let text = serde_json::to_string(&aat).unwrap();
    assert!(text.contains(r#""kind":"ruby""#));
    assert!(text.contains(r#""kind":"gaiji""#));
    assert!(text.contains(r#""x-source-marker-kind":"kaeriten""#));
    assert!(text.contains(r#""kind":"jisage_block""#));
    assert!(text.contains(r#""x-break-kind":"page""#));

    let schema_text = std::fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../data/aat-schema.json"),
    )
    .expect("schema");
    let schema: serde_json::Value = serde_json::from_str(&schema_text).unwrap();
    let validator = jsonschema::validator_for(&schema).unwrap();
    validator.validate(&aat).expect("schema-valid AAT");
}
