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

#[test]
fn summarizes_aat_metrics() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path();
    std::fs::create_dir_all(root.join("aozora-rs-adapter")).unwrap();
    std::fs::write(
        root.join("aozora-rs-adapter/one.aat.json"),
        r#"{
          "work_id": "one",
          "meta": {
            "adapter": "aozora-rs",
            "metrics": {
              "decode_ms": 1.0,
              "body_selection_ms": 2.0,
              "tokenize_ms": 3.0,
              "scopenize_ms": 4.0,
              "retokenize_ms": 5.0,
              "aat_build_ms": 6.0,
              "projection_check_ms": 7.0,
              "fallback_build_ms": 0.0,
              "parser_nodes": 13,
              "parser_normalized_nodes": 14,
              "regex_supplement_nodes": 15,
              "regex_fallback_nodes": 16,
              "fallback_used": false,
              "fallback_reason": "none"
            }
          }
        }"#,
    )
    .unwrap();

    let summary = ab_compare::metrics::summarize_aat_metrics(root).unwrap();
    assert_eq!(summary.adapter, "aozora-rs");
    assert_eq!(summary.works, 1);
    assert_eq!(summary.fallbacks, 0);
    assert_eq!(summary.stage_totals_ms["tokenize"], 3.0);
    assert_eq!(summary.node_totals["regex_supplement"], 15);
    assert_eq!(summary.node_totals["regex_fallback"], 16);
    assert_eq!(summary.slowest_works[0].work_id, "one");
    assert_eq!(summary.slowest_works[0].stages_ms["projection_check"], 7.0);
}

#[test]
fn detects_aat_structural_differences_when_reports_match() {
    let temp = tempfile::tempdir().unwrap();
    let a = temp.path().join("a");
    let b = temp.path().join("b");
    std::fs::create_dir_all(&a).unwrap();
    std::fs::create_dir_all(&b).unwrap();

    std::fs::write(
        a.join("one.json"),
        r#"{
          "work_id": "one",
          "blocks": [
            {"kind": "paragraph", "content": [{"kind": "text", "value": "吾輩は猫"}]}
          ],
          "meta": {"adapter": "a"}
        }"#,
    )
    .unwrap();
    std::fs::write(
        b.join("one.json"),
        r#"{
          "work_id": "one",
          "blocks": [
            {"kind": "heading", "level": 1, "content": [{"kind": "text", "value": "吾輩は猫"}]}
          ],
          "meta": {"adapter": "b"}
        }"#,
    )
    .unwrap();

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();
    assert_eq!(summary.common_aat, 1);
    assert_eq!(summary.structural_differences.len(), 1);
    assert_eq!(summary.structural_differences[0].work_id, "one");
    assert_eq!(
        summary.structural_differences[0].a_block_kinds["paragraph"],
        1
    );
    assert_eq!(
        summary.structural_differences[0].b_block_kinds["heading"],
        1
    );
    assert!(!summary.structural_differences[0].visible_text_differs);
}

#[test]
fn preserves_duplicate_work_ids_in_aat_comparison() {
    let temp = tempfile::tempdir().unwrap();
    let a = temp.path().join("a");
    let b = temp.path().join("b");
    std::fs::create_dir_all(&a).unwrap();
    std::fs::create_dir_all(&b).unwrap();

    for name in ["first", "second"] {
        std::fs::write(a.join(format!("{name}.json")), aat_for_work("same", "本文")).unwrap();
        std::fs::write(b.join(format!("{name}.json")), aat_for_work("same", "本文")).unwrap();
    }

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();
    assert_eq!(summary.common_aat, 2);
    assert_eq!(summary.only_a, 0);
    assert_eq!(summary.only_b, 0);
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

fn aat_for_work(work_id: &str, text: &str) -> String {
    format!(
        r#"{{
  "work_id": "{work_id}",
  "blocks": [
    {{"kind": "paragraph", "content": [{{"kind": "text", "value": "{text}"}}]}}
  ],
  "meta": {{"adapter": "test"}}
}}"#
    )
}
