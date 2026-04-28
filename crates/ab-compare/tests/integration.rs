use std::collections::BTreeMap;

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
              "source_bytes": 1234,
              "validation_body_bytes": 1000,
              "parser_nodes": 13,
              "parser_normalized_nodes": 14,
              "source_supplement_nodes": 15,
              "source_fallback_nodes": 16,
              "fallback_used": true,
              "fallback_reason": "large_body"
            }
          }
        }"#,
    )
    .unwrap();

    let summary = ab_compare::metrics::summarize_aat_metrics(root).unwrap();
    assert_eq!(summary.adapter, "aozora-rs");
    assert_eq!(summary.works, 1);
    assert_eq!(summary.fallbacks, 1);
    assert_eq!(summary.fallback_reason_counts["large_body"], 1);
    assert_eq!(summary.stage_totals_ms["tokenize"], 3.0);
    assert_eq!(summary.node_totals["source_supplement"], 15);
    assert_eq!(summary.node_totals["source_fallback"], 16);
    assert_eq!(summary.slowest_works[0].work_id, "one");
    assert_eq!(summary.slowest_works[0].stages_ms["projection_check"], 7.0);
    assert_eq!(summary.source_supplement_hotspots[0].work_id, "one");
    assert_eq!(summary.source_supplement_hotspots[0].nodes, 15);
    assert_eq!(summary.source_fallback_hotspots[0].work_id, "one");
    assert_eq!(summary.source_fallback_hotspots[0].nodes, 16);
    assert_eq!(summary.fallback_hotspots[0].work_id, "one");
    assert_eq!(summary.fallback_hotspots[0].reason, "large_body");
    assert_eq!(summary.fallback_hotspots[0].source_bytes, 1234);
    assert_eq!(summary.fallback_hotspots[0].validation_body_bytes, 1000);
    assert_eq!(summary.fallback_hotspots[0].parser_nodes, 13);
    assert_eq!(
        summary.fallback_hotspots[0].dominant_stage,
        "projection_check"
    );
}

#[test]
fn triage_report_buckets_differences_by_feature_and_metrics() {
    let index = serde_json::json!({
      "works_count": 2,
      "works": [
        {"id": "one", "features": ["ruby", "gaiji"]},
        {"id": "two", "features": ["jisage_block"]}
      ]
    });
    let comparison = ab_compare::CompareSummary {
        adapter_a: "aozora2".to_owned(),
        adapter_b: "aozora-rs".to_owned(),
        common_reports: 2,
        only_a: 0,
        only_b: 0,
        result_differences: vec![
            ab_compare::ResultDifference {
                work_id: "one".to_owned(),
                property: "visible_text_body_order".to_owned(),
                a_pass: false,
                b_pass: true,
            },
            ab_compare::ResultDifference {
                work_id: "two".to_owned(),
                property: "ruby_completeness".to_owned(),
                a_pass: false,
                b_pass: true,
            },
        ],
    };
    let aat = ab_compare::aat_diff::AatCompareSummary {
        common_aat: 2,
        only_a: 0,
        only_b: 0,
        structural_difference_count: 1,
        visible_text_difference_count: 0,
        normalized_visible_text_difference_count: 0,
        same_visible_structural_difference_count: 1,
        semantic_hash_difference_counts: BTreeMap::new(),
        semantic_summary_hash_difference_counts: BTreeMap::from([(
            "summary:ruby.basic".to_owned(),
            1,
        )]),
        normalized_visible_difference_buckets: BTreeMap::new(),
        a_semantic_totals: BTreeMap::new(),
        b_semantic_totals: BTreeMap::new(),
        structural_differences: Vec::new(),
        coverage_only_difference_count: 0,
        coverage_differences: Vec::new(),
        coverage_metrics_missing: 0,
    };
    let metrics = ab_compare::metrics::MetricsSummary {
        adapter: "aozora-rs".to_owned(),
        works: 2,
        fallbacks: 1,
        stage_totals_ms: BTreeMap::new(),
        fallback_reason_counts: BTreeMap::from([("projection_mismatch".to_owned(), 1)]),
        node_totals: BTreeMap::from([
            ("source_supplement".to_owned(), 12),
            ("source_fallback".to_owned(), 1),
        ]),
        slowest_works: Vec::new(),
        source_supplement_hotspots: vec![ab_compare::metrics::NodeHotspot {
            work_id: "one".to_owned(),
            nodes: 12,
            total_ms: 9.0,
            fallback_used: false,
            stages_ms: BTreeMap::new(),
        }],
        source_fallback_hotspots: vec![ab_compare::metrics::NodeHotspot {
            work_id: "two".to_owned(),
            nodes: 1,
            total_ms: 11.0,
            fallback_used: true,
            stages_ms: BTreeMap::new(),
        }],
        fallback_hotspots: vec![ab_compare::metrics::FallbackHotspot {
            work_id: "two".to_owned(),
            reason: "projection_mismatch".to_owned(),
            source_bytes: 2048,
            validation_body_bytes: 1024,
            parser_nodes: 20,
            parser_normalized_nodes: 3,
            source_fallback_nodes: 1,
            total_ms: 11.0,
            dominant_stage: "fallback_build".to_owned(),
            stages_ms: BTreeMap::from([("fallback_build".to_owned(), 8.0)]),
        }],
    };

    let report =
        ab_compare::triage::build_triage_report(&index, &comparison, Some(&aat), Some(&metrics));

    assert_eq!(report.adapters.a, "aozora2");
    assert_eq!(
        report
            .result_differences
            .by_property
            .get(&"visible_text_body_order".to_owned())
            .unwrap()
            .count,
        1
    );
    assert_eq!(
        report
            .result_differences
            .by_feature
            .get(&"gaiji".to_owned())
            .unwrap()
            .count,
        1
    );
    assert_eq!(
        report
            .result_differences
            .by_feature
            .get(&"ruby".to_owned())
            .unwrap()
            .count,
        1
    );
    assert_eq!(
        report
            .result_differences
            .by_feature
            .get(&"jisage_block".to_owned())
            .unwrap()
            .count,
        1
    );
    assert_eq!(
        report.semantic_summary.difference_counts.as_ref().unwrap()["summary:ruby.basic"],
        1
    );
    assert_eq!(report.fallbacks.as_ref().unwrap().works, 1);
    assert_eq!(
        report.fallbacks.as_ref().unwrap().by_reason["projection_mismatch"],
        1
    );
    let fallback = &report.fallbacks.as_ref().unwrap().hotspots[0];
    assert_eq!(fallback.work_id, "two");
    assert_eq!(fallback.reason, "projection_mismatch");
    assert_eq!(fallback.features, vec!["jisage_block"]);
    assert_eq!(fallback.source_bytes, 2048);
    assert_eq!(fallback.parser_nodes, 20);
    assert_eq!(fallback.dominant_stage, "fallback_build");
    assert_eq!(report.source_supplements.as_ref().unwrap().total_nodes, 12);
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
    assert_eq!(summary.a_semantic_totals["block:paragraph"], 1);
    assert_eq!(summary.b_semantic_totals["block:heading"], 1);
    assert_eq!(
        summary.structural_differences[0].b_semantic_counts["heading_level:1"],
        1
    );
    assert!(!summary.structural_differences[0].visible_text_differs);
}

#[test]
fn records_ruby_gaiji_and_provenance_semantics() {
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
            {"kind": "paragraph", "content": [
              {"kind": "ruby", "base": "吾輩", "reading": "わがはい"},
              {"kind": "gaiji", "description": "「口＋世」、U+546D", "resolved": ""}
            ]}
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
            {"kind": "paragraph", "content": [
              {"kind": "text", "value": "吾輩", "x-provenance": "parser_normalized"},
              {"kind": "ruby", "base": "", "reading": "われ", "x-provenance": "source_supplement"},
              {"kind": "gaiji", "description": "「口＋世」、U+546D", "resolved": "", "x-provenance": "source_supplement"}
            ]}
          ],
          "meta": {"adapter": "b"}
        }"#,
    )
    .unwrap();

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();
    assert_eq!(summary.common_aat, 1);
    assert_eq!(summary.structural_differences.len(), 1);
    assert_eq!(summary.a_semantic_totals["inline:ruby"], 1);
    assert_eq!(summary.b_semantic_totals["provenance:source_supplement"], 2);
    assert_eq!(
        summary.structural_differences[0].b_semantic_counts["provenance:parser_normalized"],
        1
    );
    assert_eq!(
        summary.structural_differences[0].b_semantic_counts["gaiji_unresolved"],
        1
    );
    assert!(
        !summary.structural_differences[0]
            .a_semantic_counts
            .contains_key("ruby_reading:わがはい")
    );
    assert_ne!(
        summary.structural_differences[0].a_semantic_hashes["ruby_readings"],
        summary.structural_differences[0].b_semantic_hashes["ruby_readings"]
    );
    assert_eq!(summary.semantic_hash_difference_counts["ruby_readings"], 1);
    assert!(summary.structural_differences[0].semantic_hashes_differ["ruby_readings"]);
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

#[test]
fn can_limit_aat_difference_samples_without_losing_counts() {
    let temp = tempfile::tempdir().unwrap();
    let a = temp.path().join("a");
    let b = temp.path().join("b");
    std::fs::create_dir_all(&a).unwrap();
    std::fs::create_dir_all(&b).unwrap();

    for work_id in ["one", "two"] {
        std::fs::write(
            a.join(format!("{work_id}.json")),
            aat_for_work(work_id, "左"),
        )
        .unwrap();
        std::fs::write(
            b.join(format!("{work_id}.json")),
            aat_for_work(work_id, "右"),
        )
        .unwrap();
    }

    let summary = ab_compare::aat_diff::compare_aat_dirs_with_limit(&a, &b, Some(1)).unwrap();
    assert_eq!(summary.common_aat, 2);
    assert_eq!(summary.structural_difference_count, 2);
    assert_eq!(summary.visible_text_difference_count, 2);
    assert_eq!(summary.structural_differences.len(), 1);
}

#[test]
fn block_boundaries_project_as_line_breaks() {
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
            {"kind": "paragraph", "content": [{"kind": "text", "value": "上\n下"}]}
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
            {"kind": "paragraph", "content": [{"kind": "text", "value": "上"}]},
            {"kind": "paragraph", "content": [{"kind": "text", "value": "下"}]}
          ],
          "meta": {"adapter": "b"}
        }"#,
    )
    .unwrap();

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();
    assert_eq!(summary.structural_difference_count, 1);
    assert_eq!(summary.visible_text_difference_count, 0);
    assert_eq!(summary.normalized_visible_text_difference_count, 0);
    assert_eq!(summary.same_visible_structural_difference_count, 1);
}

#[test]
fn normalized_visible_text_ignores_whitespace_formatting() {
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
            {"kind": "paragraph", "content": [{"kind": "text", "value": "\r\n上  下\r\n"}]}
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
            {"kind": "paragraph", "content": [{"kind": "text", "value": "上\n下"}]}
          ],
          "meta": {"adapter": "b"}
        }"#,
    )
    .unwrap();

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();
    assert_eq!(summary.structural_difference_count, 1);
    assert_eq!(summary.visible_text_difference_count, 1);
    assert_eq!(summary.normalized_visible_text_difference_count, 0);
    assert!(!summary.structural_differences[0].normalized_visible_text_differs);
}

#[test]
fn buckets_normalized_visible_differences_by_semantic_hash_changes() {
    let temp = tempfile::tempdir().unwrap();
    let a = temp.path().join("a");
    let b = temp.path().join("b");
    std::fs::create_dir_all(&a).unwrap();
    std::fs::create_dir_all(&b).unwrap();

    std::fs::write(
        a.join("visible-only.json"),
        aat_for_work("visible-only", "左"),
    )
    .unwrap();
    std::fs::write(
        b.join("visible-only.json"),
        aat_for_work("visible-only", "右"),
    )
    .unwrap();
    std::fs::write(
        a.join("visible-ruby.json"),
        r#"{
          "work_id": "visible-ruby",
          "blocks": [
            {"kind": "paragraph", "content": [
              {"kind": "text", "value": "左"},
              {"kind": "ruby", "base": "吾輩", "reading": "わがはい"}
            ]}
          ],
          "meta": {"adapter": "a"}
        }"#,
    )
    .unwrap();
    std::fs::write(
        b.join("visible-ruby.json"),
        r#"{
          "work_id": "visible-ruby",
          "blocks": [
            {"kind": "paragraph", "content": [
              {"kind": "text", "value": "右"},
              {"kind": "ruby", "base": "吾輩", "reading": "われ"}
            ]}
          ],
          "meta": {"adapter": "b"}
        }"#,
    )
    .unwrap();

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();

    assert_eq!(summary.normalized_visible_text_difference_count, 2);
    assert_eq!(
        summary.normalized_visible_difference_buckets["visible_only"],
        1
    );
    assert_eq!(
        summary.normalized_visible_difference_buckets["visible_and_ruby_readings"],
        1
    );
    let visible_only = summary
        .structural_differences
        .iter()
        .find(|difference| difference.work_id == "visible-only")
        .unwrap();
    let first_difference = visible_only
        .normalized_visible_first_difference
        .as_ref()
        .unwrap();
    assert_eq!(first_difference.char_index, 0);
    assert_eq!(first_difference.a_snippet, "左");
    assert_eq!(first_difference.b_snippet, "右");
}

#[test]
fn unresolved_gaiji_description_is_semantic_not_visible_text() {
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
            {"kind": "paragraph", "content": [
              {"kind": "text", "value": "上"},
              {"kind": "gaiji", "description": "注記", "resolved": ""},
              {"kind": "text", "value": "下"}
            ]}
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
            {"kind": "paragraph", "content": [{"kind": "text", "value": "上下"}]}
          ],
          "meta": {"adapter": "b"}
        }"#,
    )
    .unwrap();

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();
    assert_eq!(summary.visible_text_difference_count, 0);
    assert_eq!(summary.normalized_visible_text_difference_count, 0);
    assert_eq!(summary.a_semantic_totals["gaiji_unresolved"], 1);
}

#[test]
fn detects_semantic_summary_differences_when_blocks_match() {
    let temp = tempfile::tempdir().unwrap();
    let a = temp.path().join("a");
    let b = temp.path().join("b");
    std::fs::create_dir_all(&a).unwrap();
    std::fs::create_dir_all(&b).unwrap();

    std::fs::write(
        a.join("one.json"),
        aat_with_semantic_summary("one", "わがはい"),
    )
    .unwrap();
    std::fs::write(b.join("one.json"), aat_with_semantic_summary("one", "われ")).unwrap();

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();

    assert_eq!(summary.common_aat, 1);
    assert_eq!(
        summary.semantic_summary_hash_difference_counts["summary:ruby.basic"],
        1
    );
    assert_eq!(summary.structural_differences.len(), 1);
    assert_eq!(
        summary.structural_differences[0]
            .a_semantic_summary_hashes
            .len(),
        1
    );
    assert!(summary.structural_differences[0].semantic_summary_hashes_differ["summary:ruby.basic"]);
    assert!(summary.semantic_hash_difference_counts.is_empty());
}

#[test]
fn missing_semantic_summaries_compare_like_existing_aat() {
    let temp = tempfile::tempdir().unwrap();
    let a = temp.path().join("a");
    let b = temp.path().join("b");
    std::fs::create_dir_all(&a).unwrap();
    std::fs::create_dir_all(&b).unwrap();

    std::fs::write(a.join("one.json"), aat_for_work("one", "本文")).unwrap();
    std::fs::write(b.join("one.json"), aat_for_work("one", "本文")).unwrap();

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();

    assert_eq!(summary.common_aat, 1);
    assert_eq!(summary.structural_difference_count, 0);
    assert!(summary.semantic_summary_hash_difference_counts.is_empty());
}

#[test]
fn malformed_semantic_summary_is_ignored() {
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
            {"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}
          ],
          "meta": {"adapter": "a", "semantic_summary": "bad"}
        }"#,
    )
    .unwrap();
    std::fs::write(b.join("one.json"), aat_for_work("one", "本文")).unwrap();

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();

    assert_eq!(summary.common_aat, 1);
    assert_eq!(summary.structural_difference_count, 0);
    assert!(summary.semantic_summary_hash_difference_counts.is_empty());
}

#[test]
fn coverage_delta_recorded_when_fallback_differs_and_hashes_differ() {
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
            {"kind": "paragraph", "content": [{"kind": "text", "value": "左"}]}
          ],
          "meta": {"adapter": "a", "metrics": {"fallback_used": true, "fallback_reason": "aborted", "source_bytes": 100}}
        }"#,
    )
    .unwrap();
    std::fs::write(
        b.join("one.json"),
        r#"{
          "work_id": "one",
          "blocks": [
            {"kind": "paragraph", "content": [{"kind": "text", "value": "右"}]}
          ],
          "meta": {"adapter": "b", "metrics": {"fallback_used": false, "source_bytes": 200}}
        }"#,
    )
    .unwrap();

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();

    assert_eq!(summary.structural_difference_count, 1);
    assert_eq!(summary.coverage_only_difference_count, 0);
    let difference = &summary.structural_differences[0];
    assert!(difference.visible_text_differs);
    let coverage = difference.coverage_mismatch.as_ref().unwrap();
    assert!(coverage.a_had_fallback);
    assert!(!coverage.b_had_fallback);
    assert_eq!(coverage.a_fallback_reason.as_deref(), Some("aborted"));
    assert_eq!(coverage.b_fallback_reason, None);
    assert_eq!(coverage.a_source_bytes, Some(100));
    assert_eq!(coverage.b_source_bytes, Some(200));
}

#[test]
fn coverage_only_difference_count_incremented_when_hashes_match() {
    let temp = tempfile::tempdir().unwrap();
    let a = temp.path().join("a");
    let b = temp.path().join("b");
    std::fs::create_dir_all(&a).unwrap();
    std::fs::create_dir_all(&b).unwrap();

    let a_json = serde_json::json!({
        "meta": {
            "adapter": "a",
            "metrics": {"fallback_used": true, "fallback_reason": "aborted"}
        },
        "work_id": "one",
        "blocks": [
            {"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}
        ]
    });
    let b_json = serde_json::json!({
        "meta": {
            "adapter": "b",
            "metrics": {"fallback_used": false}
        },
        "work_id": "one",
        "blocks": [
            {"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}
        ]
    });

    std::fs::write(a.join("one.json"), serde_json::to_string(&a_json).unwrap()).unwrap();
    std::fs::write(b.join("one.json"), serde_json::to_string(&b_json).unwrap()).unwrap();

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();

    assert_eq!(summary.structural_difference_count, 0);
    assert_eq!(summary.visible_text_difference_count, 0);
    assert_eq!(summary.same_visible_structural_difference_count, 0);
    assert_eq!(summary.coverage_only_difference_count, 1);
    let difference = &summary.coverage_differences[0];
    assert!(!difference.visible_text_differs);
    assert_eq!(difference.work_id, "one");
    assert!(difference.coverage_mismatch.is_some());
    assert!(
        difference
            .coverage_mismatch
            .as_ref()
            .unwrap()
            .a_had_fallback
    );
    assert!(
        !difference
            .coverage_mismatch
            .as_ref()
            .unwrap()
            .b_had_fallback
    );
}

#[test]
fn missing_metrics_skips_coverage_comparison() {
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
            {"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}
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
            {"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}
          ],
          "meta": {"adapter": "b", "metrics": {"fallback_used": true, "fallback_reason": "aborted"}}
        }"#,
    )
    .unwrap();

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();

    assert_eq!(summary.structural_difference_count, 0);
    assert_eq!(summary.coverage_only_difference_count, 0);
    assert!(summary.structural_differences.is_empty());
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

fn aat_with_semantic_summary(work_id: &str, reading: &str) -> String {
    format!(
        r#"{{
  "work_id": "{work_id}",
  "blocks": [
    {{"kind": "paragraph", "content": [{{"kind": "text", "value": "本文"}}]}}
  ],
  "meta": {{
    "adapter": "test",
    "semantic_summary": {{
      "syntax": {{
        "ruby.basic": [
          {{
            "kind": "ruby",
            "value": {{"base_projection": "吾輩", "reading": "{reading}", "placement": "right"}},
            "provenance": "parser"
          }}
        ]
      }}
    }}
  }}
}}"#
    )
}
