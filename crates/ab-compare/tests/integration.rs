#[test]
fn compares_two_report_sets() {
    let temp = tempfile::tempdir().unwrap();
    let a = temp.path().join("a");
    let b = temp.path().join("b");
    std::fs::create_dir_all(&a).unwrap();
    std::fs::create_dir_all(&b).unwrap();

    std::fs::write(a.join("000001-abc.json"), report("aozora2", true)).unwrap();
    std::fs::write(b.join("000001-abc.json"), report("aozora-rs", false)).unwrap();

    let output = std::process::Command::new(env!("CARGO_BIN_EXE_ab-compare"))
        .arg("--reports-a")
        .arg(&a)
        .arg("--reports-b")
        .arg(&b)
        .arg("--output")
        .arg(temp.path().join("summary.json"))
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let summary: serde_json::Value =
        serde_json::from_slice(&std::fs::read(temp.path().join("summary.json")).unwrap()).unwrap();
    assert_eq!(summary["common_reports"], 1);
    assert_eq!(summary["only_a"], 0);
    assert_eq!(summary["only_b"], 0);
    assert_eq!(summary["result_differences"][0]["property"], "schema_valid");
}

#[test]
fn preserves_duplicate_work_ids_with_filename_suffix() {
    let temp = tempfile::tempdir().unwrap();
    let a = temp.path().join("a");
    let b = temp.path().join("b");
    std::fs::create_dir_all(&a).unwrap();
    std::fs::create_dir_all(&b).unwrap();

    std::fs::write(
        a.join("000001-first.json"),
        report_for_work("aozora2", "000001_1", true),
    )
    .unwrap();
    std::fs::write(
        a.join("000001-second.json"),
        report_for_work("aozora2", "000001_1", true),
    )
    .unwrap();
    std::fs::write(
        b.join("000001-first.json"),
        report_for_work("aozora-rs", "000001_1", true),
    )
    .unwrap();
    std::fs::write(
        b.join("000001-second.json"),
        report_for_work("aozora-rs", "000001_1", true),
    )
    .unwrap();

    let output = std::process::Command::new(env!("CARGO_BIN_EXE_ab-compare"))
        .arg("--reports-a")
        .arg(&a)
        .arg("--reports-b")
        .arg(&b)
        .arg("--output")
        .arg(temp.path().join("duplicates.json"))
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let summary: serde_json::Value =
        serde_json::from_slice(&std::fs::read(temp.path().join("duplicates.json")).unwrap())
            .unwrap();
    assert_eq!(summary["common_reports"], 2);
    assert_eq!(summary["only_a"], 0);
    assert_eq!(summary["only_b"], 0);
}

fn report(adapter: &str, pass: bool) -> String {
    report_for_work(adapter, "000001_1", pass)
}

fn report_for_work(adapter: &str, work_id: &str, pass: bool) -> String {
    format!(
        r#"{{
  "adapter": "{adapter}",
  "adapter_version": "test",
  "work_id": "{work_id}",
  "results": {{
    "schema_valid": {{
      "pass": {pass},
      "confidence": "strict"
    }}
  }}
}}"#
    )
}
