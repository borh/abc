use std::path::{Path, PathBuf};
use std::sync::Mutex;

use ab_aat_to_parser_ir::{
    ConversionOptions, ConversionRequest, MappingDocument, SchemaSet,
    divergence::{AatMeta, DivergenceRecorder},
    mapping::MappingRule,
    schema::{read_json, schema_hash, validate_value},
};
use serde_json::{Value, json};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn abc_root(repo: &Path) -> PathBuf {
    std::env::var_os("AB_ABC_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|| repo.join("data/abc-schemas"))
}

fn roots() -> (PathBuf, PathBuf) {
    let repo = repo_root();
    let abc = abc_root(&repo);
    (repo, abc)
}

fn default_test_options() -> ConversionOptions {
    ConversionOptions::default()
}

fn schemas_and_mapping() -> (SchemaSet, MappingDocument) {
    let (repo_root, abc_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v1.json"))
            .unwrap();
    let schemas =
        SchemaSet::load_for_aat_version(&repo_root, &abc_root, mapping.source_aat_version).unwrap();
    (schemas, mapping)
}

#[test]
fn schema_set_rejects_unknown_aat_version() {
    let (repo_root, abc_root) = roots();
    let err = SchemaSet::load_for_aat_version(&repo_root, &abc_root, 3).unwrap_err();
    assert!(
        err.to_string().contains("unsupported AAT schema version 3"),
        "{err}"
    );
}

#[test]
fn validated_v1_conversion_succeeds_under_v1_tuple() {
    // full pipeline proof, not just preflight: any existing v1 fixture through
    // convert() with the v1 tuple must succeed WITH input validation on.
    let (schemas, mapping) = schemas_and_mapping();
    let aat = include_fixture_json("nested-sentence-basic.aat.json");
    ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: default_test_options(),
    })
    .unwrap();
}

#[test]
fn v1_document_under_v2_tuple_fails_input_validation() {
    let (repo_root, abc_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v2.json"))
            .unwrap();
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &abc_root, 2).unwrap();
    // v1 fixture (version:1) must be REJECTED by the v2 tuple's input validation.
    let aat = include_fixture_json("nested-sentence-basic.aat.json");
    let err = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: default_test_options(),
    })
    .unwrap_err();
    assert!(err.to_string().contains("AAT validation failed"), "{err}");
}

#[test]
fn v2_document_under_v1_tuple_fails_input_validation() {
    let (repo_root, abc_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v1.json"))
            .unwrap();
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &abc_root, 1).unwrap();
    // v2 doc (version:2, using a v2-only construct) must be REJECTED by the v1
    // tuple's input validation.
    let aat = json!({
        "version": 2, "work_id": "t-jizume",
        "blocks": [{ "kind": "jizume_block", "width": 21, "children": [
            { "kind": "paragraph", "content": [{ "kind": "text", "value": "本文" }] } ] }],
        "meta": v2_test_meta()
    });
    let err = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: default_test_options(),
    })
    .unwrap_err();
    assert!(err.to_string().contains("AAT validation failed"), "{err}");
}

#[test]
fn preflight_rejects_mismatched_mapping_schema_tuple() {
    let (repo_root, abc_root) = roots();
    // v1 mapping paired with the v2 schema file must fail preflight.
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v1.json"))
            .unwrap();
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &abc_root, 2).unwrap();
    let err = mapping.preflight(&schemas).unwrap_err();
    assert!(err.to_string().contains("tuple mismatch"), "{err}");
}

fn include_fixture_json(name: &str) -> serde_json::Value {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join(name);
    let bytes = std::fs::read(&path)
        .unwrap_or_else(|err| panic!("failed to read fixture {}: {err}", path.display()));
    serde_json::from_slice(&bytes)
        .unwrap_or_else(|err| panic!("failed to parse fixture {}: {err}", path.display()))
}

/// Reconstruct the visible text of a parser-IR sentence from its node_range,
/// for fragment-attribution assertions. Handles the inline node types emitted
/// by the quote-synthesis converter (text, quote). Other types fall back to
/// their `text` field.
fn visible_text_for_sentence(nodes: &[Value], sentence: &Value) -> String {
    let start = sentence["node_range"]["start"].as_u64().unwrap() as usize;
    let end = sentence["node_range"]["end"].as_u64().unwrap() as usize;
    let mut out = String::new();
    for node in &nodes[start..end] {
        match node["type"].as_str() {
            Some("text") | Some("quote") => {
                out.push_str(node["text"].as_str().unwrap_or(""));
            }
            Some("ruby") => {
                out.push_str(node["ruby"]["base"].as_str().unwrap_or(""));
            }
            Some("line-break") => out.push('\n'),
            _ => out.push_str(node["text"].as_str().unwrap_or("")),
        }
    }
    out
}

fn ortho_fixture_bundle() -> ab_aat_to_parser_ir::ortho_annotations::OrthoAnnotationsBundle {
    serde_json::from_value(json!({
        "work_id": "000000",
        "primary_text_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
        "coordinate_system": "decoded_utf8",
        "detector_id": "HeuristicV1",
        "annotations": [
            {
                "source_byte_range": { "start": 0, "end": 24 },
                "normalized_text": "吾輩は猫である。",
                "kind": "ScriptKatakanaToHiragana",
                "confidence": null
            },
            {
                "source_byte_range": { "start": 24, "end": 48 },
                "normalized_text": "名前はまだ無い。",
                "kind": "ScriptKatakanaToHiragana",
                "confidence": null
            }
        ]
    }))
    .unwrap()
}

fn ortho_fixture_aat() -> Value {
    json!({
        "version": 1,
        "work_id": "000000",
        "meta": {
            "adapter": "fixture",
            "adapter_version": "fixture 0.1.0",
            "source_encoding": "utf-8",
            "source_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "parse_complete": true,
            "warnings": []
        },
        "blocks": [{
            "kind": "paragraph",
            "content": [{
                "kind": "text",
                "value": "吾輩ハ猫デアル。名前ハマダ無イ。"
            }]
        }]
    })
}

#[test]
fn tei_eaj_alignment_probe_classifies_melos_tail_addition() {
    let temp = tempfile::tempdir().unwrap();
    let workset_path = temp.path().join("workset.json");
    std::fs::write(&workset_path, minimal_melos_alignment_workset(temp.path())).unwrap();

    let report = ab_aat_to_parser_ir::tei_eaj_alignment_probe::run_tei_eaj_alignment_probe(
        ab_aat_to_parser_ir::tei_eaj_alignment_probe::TeiEajAlignmentProbeConfig {
            workset_path,
            max_rows: None,
        },
    )
    .unwrap();

    assert_eq!(report.rows.len(), 1);
    let probe = report.rows[0].alignment_probe.as_ref().unwrap();
    assert_eq!(probe.diagnosis_counts.get("tail_addition"), Some(&1));
    assert_eq!(
        probe.diagnosis_event_count,
        probe.diagnosis_counts.values().sum::<usize>()
    );
    assert_eq!(probe.samples[0].diagnosis, "tail_addition");
    assert_eq!(
        probe.samples[0].left_text,
        "（古伝説と、シルレルの詩から。）"
    );
    assert!(!probe.samples[0].left_text.contains("せきめん"));
    assert!(!probe.samples[0].left_text.contains("底本注"));
    assert_eq!(
        probe.samples[0]
            .adapter_context
            .get("left_path")
            .and_then(|value| value.as_str()),
        Some("/TEI[1]/text[1]/body[1]/p[1]#run2")
    );
    assert_eq!(
        probe.samples[0]
            .adapter_context
            .get("left_features")
            .and_then(|value| value.as_array())
            .unwrap()[0],
        serde_json::json!("source-attribution")
    );
}

#[test]
fn cli_tei_eaj_alignment_probe_writes_reports() {
    let temp = tempfile::tempdir().unwrap();
    let workset = temp.path().join("workset.json");
    let summary = temp.path().join("alignment-summary.json");
    let report = temp.path().join("alignment-report.md");
    std::fs::write(&workset, minimal_melos_alignment_workset(temp.path())).unwrap();

    let status = std::process::Command::new(env!("CARGO_BIN_EXE_ab-aat-to-parser-ir"))
        .arg("tei-eaj-alignment-probe")
        .arg("--workset")
        .arg(&workset)
        .arg("--summary-json")
        .arg(&summary)
        .arg("--report-md")
        .arg(&report)
        .status()
        .unwrap();

    assert!(status.success());
    let summary_json: serde_json::Value = ab_aat_to_parser_ir::schema::read_json(&summary).unwrap();
    assert_eq!(
        summary_json.pointer("/rows/0/alignment_probe/diagnosis_counts/tail_addition"),
        Some(&serde_json::json!(1))
    );
    let report_text = std::fs::read_to_string(report).unwrap();
    assert!(report_text.contains("TEI-EAJ Alignment Probe"));
    assert!(report_text.contains("tail_addition"));
}

#[test]
fn tei_eaj_alignment_probe_does_not_call_source_attribution_substitution_tail_addition() {
    let temp = tempfile::tempdir().unwrap();
    let workset_path = temp.path().join("workset.json");
    std::fs::write(
        &workset_path,
        melos_alignment_workset_with_xml(
            temp.path(),
            r#"<TEI xmlns="http://www.tei-c.org/ns/1.0"><text><body><p>（古伝説と、シルレルの詩から。）</p></body></text></TEI>"#,
            r#"<TEI xmlns="http://www.tei-c.org/ns/1.0"><text><body><p>別の本文。</p></body></text></TEI>"#,
        ),
    )
    .unwrap();

    let report = ab_aat_to_parser_ir::tei_eaj_alignment_probe::run_tei_eaj_alignment_probe(
        ab_aat_to_parser_ir::tei_eaj_alignment_probe::TeiEajAlignmentProbeConfig {
            workset_path,
            max_rows: None,
        },
    )
    .unwrap();

    let probe = report.rows[0].alignment_probe.as_ref().unwrap();
    assert_eq!(probe.diagnosis_counts.get("tail_addition"), None);
    assert_eq!(probe.diagnosis_counts.get("substitution"), Some(&1));
}

fn minimal_melos_alignment_workset(root: &std::path::Path) -> String {
    melos_alignment_workset_with_xml(
        root,
        r#"<TEI xmlns="http://www.tei-c.org/ns/1.0"><text><body><p>メロスは激怒した。勇者は、ひどく<ruby><rb>赤面した</rb><rt>せきめんした</rt></ruby>。<note>底本注</note>（古伝説と、シルレルの詩から。）</p></body></text></TEI>"#,
        r#"<TEI xmlns="http://www.tei-c.org/ns/1.0"><text><body><p>メロスは激怒した。</p><p>勇者は、ひどく赤面した。</p></body></text></TEI>"#,
    )
}

fn melos_alignment_workset_with_xml(
    root: &std::path::Path,
    abc_xml: &str,
    tei_eaj_xml: &str,
) -> String {
    let abc_tei = root.join("abc-melos.xml");
    let tei_eaj_root = root.join("tei-eaj");
    let tei_eaj_file = tei_eaj_root.join("data/complete/tei_lib_lv4/1567_tei.xml");
    std::fs::create_dir_all(tei_eaj_file.parent().unwrap()).unwrap();
    std::fs::write(&abc_tei, abc_xml).unwrap();
    std::fs::write(&tei_eaj_file, tei_eaj_xml).unwrap();
    format!(
        r#"{{
  "schema_version": "tei-eaj-aozora-workset-export-v1",
  "tei_eaj_source": {{"revision": "fixture", "root": "{}"}},
  "abc_inputs": {{"counterparts": [{{"path": "{}", "work_id": "1567"}}], "tei_dirs": [], "tei_specs": []}},
  "summary": {{
    "tei_eaj_file_count": 1,
    "tei_eaj_work_id_count": 1,
    "abc_counterpart_count": 1,
    "compared_file_count": 1,
    "missing_counterpart_count": 0,
    "no_work_id_count": 0,
    "uncompared_file_count": 0,
    "base_text_equal_count": 0,
    "base_text_mismatch_count": 1,
    "base_text_relation_counts": {{"tei_eaj_subset_of_abc": 1}}
  }},
  "candidate_work_ids": ["1567"],
  "missing_abc_counterpart_work_ids": [],
  "no_work_id_files": [],
  "files": [{{
    "work_id": "1567",
    "work_id_method": "filename_work_id",
    "tei_eaj_file": "data/complete/tei_lib_lv4/1567_tei.xml",
    "state": "complete",
    "level": "Level 4",
    "title": "走れメロス",
    "abc_tei": "{}",
    "comparison_status": "compared",
    "base_text_equal": false,
    "base_text_relation": "tei_eaj_subset_of_abc",
    "base_text_length_delta": 16,
    "abc_body_base_text_length": 40,
    "tei_eaj_body_base_text_length": 24,
    "abc_p_count": 1,
    "tei_eaj_p_count": 2,
    "abc_note_count": 1,
    "tei_eaj_note_count": 0,
    "first_difference": {{
      "index": 24,
      "abc": "メロスは激怒した。勇者は、ひどく赤面した。（古伝説と、シルレルの詩から。）",
      "tei_eaj": "メロスは激怒した。勇者は、ひどく赤面した。"
    }}
  }}]
}}"#,
        tei_eaj_root.display(),
        abc_tei.display(),
        abc_tei.display()
    )
}

struct AlwaysNormalizeDetector;

struct RecordingDetector {
    observed_char_offsets: Mutex<Vec<usize>>,
}

impl ab_ortho_detect::OrthoDetector for AlwaysNormalizeDetector {
    fn detector_id(&self) -> ab_ortho_detect::OrthoDetectorId {
        ab_ortho_detect::OrthoDetectorId::HeuristicV1
    }

    fn detect(
        &self,
        sentences: &[ab_plaintext::SentenceSpan<'_>],
    ) -> Vec<ab_ortho_detect::OrthoAnnotation> {
        sentences
            .iter()
            .map(|sentence| ab_ortho_detect::OrthoAnnotation {
                source_byte_range: sentence.byte_offset..sentence.byte_offset + sentence.text.len(),
                normalized_text: sentence
                    .text
                    .replace('ハ', "は")
                    .replace('デ', "で")
                    .replace('ア', "あ"),
                kind: ab_ortho_detect::OrthoNormalization::ScriptKatakanaToHiragana,
                confidence: None,
            })
            .collect()
    }
}

impl ab_ortho_detect::OrthoDetector for RecordingDetector {
    fn detector_id(&self) -> ab_ortho_detect::OrthoDetectorId {
        ab_ortho_detect::OrthoDetectorId::HeuristicV1
    }

    fn detect(
        &self,
        sentences: &[ab_plaintext::SentenceSpan<'_>],
    ) -> Vec<ab_ortho_detect::OrthoAnnotation> {
        *self.observed_char_offsets.lock().unwrap() = sentences
            .iter()
            .map(|sentence| sentence.char_offset)
            .collect();
        Vec::new()
    }
}

fn base_meta(source_encoding: &str, source_hash: &str) -> Value {
    json!({
        "adapter": "fixture",
        "adapter_version": "fixture 0.1.0",
        "source_encoding": source_encoding,
        "source_hash": source_hash,
        "parse_complete": true,
        "warnings": []
    })
}

#[test]
fn supplied_bundle_hash_is_distinct_from_primary_text_hash() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "identity-test",
        "blocks": [],
        "meta": base_meta(
            "utf-8",
            "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        )
    });
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: ConversionOptions {
            work_content_hash: Some(
                "sha256:2222222222222222222222222222222222222222222222222222222222222222"
                    .to_owned(),
            ),
            ..ConversionOptions::default()
        },
    })
    .unwrap();
    assert_eq!(
        output.parser_ir["source"]["work_content_hash"],
        "sha256:2222222222222222222222222222222222222222222222222222222222222222"
    );
    assert_eq!(
        output.parser_ir["source"]["primary_text_hash"],
        "sha256:1111111111111111111111111111111111111111111111111111111111111111"
    );
    assert!(!has_divergence_record(
        &output,
        "AMBIGUITY",
        "meta.source_hash",
        Some("source.work_content_hash")
    ));
    assert!(has_divergence_record(
        &output,
        "AMBIGUITY",
        "meta.source_hash",
        Some("source.primary_text_hash")
    ));
}

#[test]
fn explicit_aat_primary_text_hash_is_the_recorded_provenance() {
    let (schemas, mapping) = schemas_and_mapping();
    let hash = "sha256:1111111111111111111111111111111111111111111111111111111111111111";
    let mut meta = base_meta("utf-8", hash);
    meta["primary_text_hash"] = json!(hash);
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: json!({
            "version": 1,
            "work_id": "identity-test",
            "blocks": [],
            "meta": meta
        }),
        mapping,
        schemas,
        options: ConversionOptions::default(),
    })
    .unwrap();
    assert!(has_divergence_record(
        &output,
        "AMBIGUITY",
        "meta.primary_text_hash",
        Some("source.primary_text_hash")
    ));
    assert!(!has_divergence_record(
        &output,
        "AMBIGUITY",
        "meta.source_hash",
        Some("source.primary_text_hash")
    ));
}

#[test]
fn aat_primary_text_alias_mismatch_is_rejected() {
    let (schemas, mapping) = schemas_and_mapping();
    let mut aat = json!({
        "version": 1,
        "work_id": "identity-test",
        "blocks": [],
        "meta": base_meta(
            "utf-8",
            "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        )
    });
    aat["meta"]["primary_text_hash"] =
        json!("sha256:2222222222222222222222222222222222222222222222222222222222222222");
    let error = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: ConversionOptions::default(),
    })
    .unwrap_err();
    assert!(error.to_string().contains("primary_text_hash"), "{error}");
}

#[test]
fn invalid_supplied_work_content_hash_is_rejected() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "identity-test",
        "blocks": [],
        "meta": base_meta(
            "utf-8",
            "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        )
    });
    let error = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: ConversionOptions {
            work_content_hash: Some("sha256:not-a-hash".to_owned()),
            ..ConversionOptions::default()
        },
    })
    .unwrap_err();
    assert!(error.to_string().contains("work_content_hash"), "{error}");
}

fn has_divergence_record(
    output: &ab_aat_to_parser_ir::ConversionOutput,
    category: &str,
    aat_pointer: &str,
    parser_ir_pointer: Option<&str>,
) -> bool {
    output
        .divergence_bundle
        .pointer("/records")
        .and_then(Value::as_array)
        .expect("divergence bundle records")
        .iter()
        .any(|record| {
            record["category"] == category
                && record["aat_pointer"] == aat_pointer
                && match parser_ir_pointer {
                    Some(pointer) => record["parser_ir_pointer"] == pointer,
                    None => record["parser_ir_pointer"].is_null(),
                }
        })
}

#[test]
fn legacy_schema_hashes_match_mapping_artifact() {
    let repo = repo_root();
    let abc = abc_root(&repo);
    let schemas = SchemaSet::load_for_aat_version(&repo, &abc, 1).unwrap();

    assert_eq!(
        schema_hash(&schemas.mapping_schema).unwrap(),
        "sha256:e6af01115ccdb7c5cad086eee4c458230f6b6f55e0dfee7791730b48994283e2"
    );
    assert_eq!(
        schema_hash(&schemas.parser_ir_schema).unwrap(),
        "sha256:43a6a6d86ca5eca062508e6cae633d19bf5248f15c5bb46153a6d8580ea916ec"
    );
}

#[test]
fn legacy_canonicalization_escapes_slashes() {
    let value: Value = json!({"id": "https://abc.local/x", "b": 1, "a": 2});
    let payload = ab_aat_to_parser_ir::schema::abc_legacy_json_c14n_v0(&value).unwrap();
    assert_eq!(
        String::from_utf8(payload).unwrap(),
        r#"{"a":2,"b":1,"id":"https:\/\/abc.local\/x"}"#
    );
}

#[test]
fn read_json_accepts_deeply_nested_aat_values() {
    let temp = tempfile::tempdir().unwrap();
    let path = temp.path().join("deep.aat.json");
    let mut inline = r#"{"kind":"text","value":"本文"}"#.to_owned();
    for _ in 0..140 {
        inline = format!(r#"{{"kind":"style","style_type":"nested","content":[{inline}]}}"#);
    }
    std::fs::write(
        &path,
        format!(
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
        ),
    )
    .unwrap();

    let value = read_json(&path).unwrap();

    assert_eq!(value["work_id"], "deep_fixture");
}

#[test]
fn mapping_preflight_accepts_checked_in_v1_artifact() {
    let (schemas, mapping) = schemas_and_mapping();

    let index = mapping.preflight(&schemas).unwrap();

    assert_eq!(mapping.mapping_version, "0.4.0");
    assert_eq!(
        mapping.target_parser_ir_schema_hash,
        schema_hash(&schemas.parser_ir_schema).unwrap()
    );
    assert_eq!(mapping.transform_rule_descriptions.len(), 681);
    assert!(mapping.transform_rule_descriptions.iter().all(|rule| {
        !matches!(
            rule.parser_ir_pointer.as_deref(),
            Some("sentence_segmentation" | "sentences" | "orthographic_annotations")
        )
    }));
    let synthetic_pointers: std::collections::BTreeSet<_> = mapping
        .synthetic_evidence_descriptions
        .iter()
        .map(|entry| entry.parser_ir_pointer.as_str())
        .collect();
    assert!(synthetic_pointers.contains("sentence_segmentation"));
    assert!(synthetic_pointers.contains("sentences"));
    assert!(synthetic_pointers.contains("sentences[].tags"));
    assert!(synthetic_pointers.contains("orthographic_annotations"));
    assert!(synthetic_pointers.contains("source.work_content_hash"));
    assert!(
        !mapping
            .transform_rule_descriptions
            .iter()
            .any(|rule| rule.category == "STRUCTURAL"
                && rule
                    .aat_pointer
                    .as_deref()
                    .is_some_and(|pointer| pointer.contains("paragraph"))),
        "paragraph blocks should be represented by parser-IR paragraphs[], not STRUCTURAL loss"
    );
    assert!(
        index
            .require_rule("UNSUPPORTED", Some("blocks[].content[].warigaki"), None)
            .unwrap()
            .description
            .contains("warigaki")
    );
}

#[test]
fn mapping_preflight_accepts_checked_in_v2_artifact() {
    let (repo_root, abc_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v2.json"))
            .unwrap();
    assert_eq!(mapping.mapping_version, "0.5.0");
    assert_eq!(mapping.source_aat_version, 2);
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &abc_root, 2).unwrap();
    mapping.preflight(&schemas).unwrap();
}

#[test]
fn frozen_v2_0_3_0_artifact_retains_historical_coordinates() {
    let (repo_root, abc_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v2-0.3.0.json"))
            .unwrap();
    assert_eq!(mapping.mapping_version, "0.3.0");
    assert_eq!(
        mapping.document_hash,
        "sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40"
    );
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &abc_root, 2).unwrap();
    assert_ne!(
        mapping.target_parser_ir_schema_hash,
        schema_hash(&schemas.parser_ir_schema).unwrap(),
        "the frozen generation must retain its historical parser-IR coordinate"
    );
}

#[test]
fn frozen_v2_0_4_0_artifact_retains_phase5_coordinates() {
    let (repo_root, abc_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v2-0.4.0.json"))
            .unwrap();
    assert_eq!(mapping.mapping_version, "0.4.0");
    assert_eq!(
        mapping.document_hash,
        "sha256:cf177bee98af086fe728cbc1942e4f631b26ed5bb55aedc7d91b21f467c41f30"
    );
    assert_eq!(mapping.source_aat_version, 2);
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &abc_root, 2).unwrap();
    assert_ne!(
        mapping.target_parser_ir_schema_hash,
        schema_hash(&schemas.parser_ir_schema).unwrap(),
        "the frozen Phase 5 generation must retain its historical parser-IR coordinate"
    );
}

#[test]
fn mapping_preflight_rejects_wrong_target_schema_hash() {
    let (schemas, mut mapping) = schemas_and_mapping();
    mapping.target_parser_ir_schema_hash = format!("sha256:{}", "0".repeat(64));

    let error = mapping.preflight(&schemas).unwrap_err().to_string();

    assert!(error.contains("target parser-IR schema hash"));
}

#[test]
fn mapping_preflight_rejects_stale_gaiji_pointers() {
    let (schemas, mut mapping) = schemas_and_mapping();
    mapping.transform_rule_descriptions.push(MappingRule {
        rule_id: "A-99".to_owned(),
        category: "AMBIGUITY".to_owned(),
        aat_pointer: Some("blocks[].content[].gaiji.raw_marker".to_owned()),
        parser_ir_pointer: Some("gaiji.raw_marker".to_owned()),
        action: "project".to_owned(),
        description: "stale pointer fixture".to_owned(),
    });

    let error = mapping.preflight(&schemas).unwrap_err().to_string();

    assert!(error.contains("A-99"));
    assert!(error.contains("gaiji.raw_marker"));
}

#[test]
fn orthographic_annotations_inject_into_schema_valid_parser_ir() {
    let (schemas, mapping) = schemas_and_mapping();
    let bundle = ortho_fixture_bundle();
    let expected = serde_json::to_value(&bundle).unwrap();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: ortho_fixture_aat(),
        mapping,
        schemas: schemas.clone(),
        options: ConversionOptions {
            orthographic_annotations: Some(bundle),
            ..default_test_options()
        },
    })
    .unwrap();

    assert_eq!(
        output.parser_ir.get("orthographic_annotations"),
        Some(&expected)
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn distinct_bundle_hash_does_not_change_annotation_coordinate_identity() {
    let (schemas, mapping) = schemas_and_mapping();
    let primary = "sha256:1111111111111111111111111111111111111111111111111111111111111111";
    let bundle = "sha256:2222222222222222222222222222222222222222222222222222222222222222";
    let annotations: ab_aat_to_parser_ir::ortho_annotations::OrthoAnnotationsBundle =
        serde_json::from_value(json!({
            "work_id": "000000",
            "primary_text_hash": primary,
            "coordinate_system": "decoded_utf8",
            "detector_id": "HeuristicV1",
            "annotations": []
        }))
        .unwrap();
    let mut aat = ortho_fixture_aat();
    aat["meta"]["primary_text_hash"] = json!(primary);
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: ConversionOptions {
            work_content_hash: Some(bundle.to_owned()),
            orthographic_annotations: Some(annotations),
            ..default_test_options()
        },
    })
    .unwrap();

    assert_eq!(output.parser_ir["source"]["work_content_hash"], bundle);
    assert_eq!(output.parser_ir["source"]["primary_text_hash"], primary);
    assert_eq!(
        output.parser_ir["orthographic_annotations"]["primary_text_hash"],
        primary
    );
    assert!(
        output.parser_ir["orthographic_annotations"]
            .get("work_content_hash")
            .is_none()
    );
}

#[test]
fn detect_orthographic_annotations_uses_parser_ir_sentence_coordinates() {
    let (schemas, mapping) = schemas_and_mapping();
    let bundle = ab_aat_to_parser_ir::ortho_detect::detect_orthographic_annotations(
        include_fixture_json("sentence-segmentation-input.aat.json"),
        mapping,
        schemas,
        &AlwaysNormalizeDetector,
    )
    .unwrap();

    assert_eq!(bundle.work_id, "000000");
    assert_eq!(
        bundle.primary_text_hash,
        "sha256:1111111111111111111111111111111111111111111111111111111111111111"
    );
    assert_eq!(
        bundle.coordinate_system,
        ab_aat_to_parser_ir::ortho_annotations::OrthoCoordinateSystem::DecodedUtf8
    );
    assert_eq!(bundle.annotations.len(), 3);
    assert_eq!(bundle.annotations[0].source_byte_range, 0..24);
    assert_eq!(bundle.annotations[1].source_byte_range, 24..48);
    assert_eq!(bundle.annotations[2].source_byte_range, 48..63);
}

#[test]
fn detect_orthographic_annotations_populates_sentence_char_offsets() {
    let (schemas, mapping) = schemas_and_mapping();
    let detector = RecordingDetector {
        observed_char_offsets: Mutex::new(Vec::new()),
    };

    ab_aat_to_parser_ir::ortho_detect::detect_orthographic_annotations(
        include_fixture_json("sentence-segmentation-input.aat.json"),
        mapping,
        schemas,
        &detector,
    )
    .unwrap();

    assert_eq!(
        *detector.observed_char_offsets.lock().unwrap(),
        vec![0, 8, 16]
    );
}

#[test]
fn parser_ir_emits_split_sentence_rows_and_ortho_tags() {
    let (schemas, mapping) = schemas_and_mapping();
    let bundle = ortho_fixture_bundle();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: include_fixture_json("sentence-segmentation-input.aat.json"),
        mapping,
        schemas: schemas.clone(),
        options: ConversionOptions {
            orthographic_annotations: Some(bundle),
            ..default_test_options()
        },
    })
    .unwrap();

    assert_eq!(
        output
            .parser_ir
            .pointer("/sentence_segmentation/splitter_id"),
        Some(&json!("ab-plaintext-japanese-v2"))
    );
    assert_eq!(
        output.parser_ir.pointer("/paragraphs/0/node_range"),
        Some(&json!({"start":0,"end":2}))
    );
    assert_eq!(
        output.parser_ir.pointer("/paragraphs/1/node_range"),
        Some(&json!({"start":2,"end":3}))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/0/span"),
        Some(&json!({"start":0,"end":24,"coordinate_system":"decoded_utf8"}))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/1/span"),
        Some(&json!({"start":24,"end":48,"coordinate_system":"decoded_utf8"}))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/2/span"),
        Some(&json!({"start":48,"end":63,"coordinate_system":"decoded_utf8"}))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/0/tags"),
        Some(&json!(["orthographic-katakana"]))
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/sentences/0/orthographic_annotation_indices"),
        Some(&json!([0]))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/1/tags"),
        Some(&json!(["orthographic-katakana"]))
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/sentences/1/orthographic_annotation_indices"),
        Some(&json!([1]))
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn sentence_segmentation_uses_ruby_base_not_reading() {
    let (schemas, mapping) = schemas_and_mapping();
    let first_sentence = "名前はまだ無い。";
    let full_text = "名前はまだ無い。ここは次。";
    let aat = json!({
        "version": 1,
        "work_id": "ruby-sentence",
        "meta": base_meta(
            "utf-8",
            "sha256:abababababababababababababababababababababababababababababababab",
        ),
        "blocks": [{
            "kind": "paragraph",
            "content": [{
                "kind": "ruby",
                "base": "名前",
                "reading": "めいしょう"
            }, {
                "kind": "text",
                "value": "はまだ無い。ここは次。"
            }]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    assert_eq!(
        output.parser_ir.pointer("/sentences/0/span/start"),
        Some(&json!(0))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/0/span/end"),
        Some(&json!(first_sentence.len()))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/1/span/start"),
        Some(&json!(first_sentence.len()))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/1/span/end"),
        Some(&json!(full_text.len()))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/0/node_range"),
        Some(&json!({"start": 0, "end": 2}))
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/0/ruby/reading"),
        Some(&json!("めいしょう"))
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn checked_in_schema_accepts_sentence_segmentation_and_ortho_annotations() {
    let (schemas, mapping) = schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: ortho_fixture_aat(),
        mapping,
        schemas: schemas.clone(),
        options: ConversionOptions {
            orthographic_annotations: Some(ortho_fixture_bundle()),
            ..default_test_options()
        },
    })
    .unwrap();

    assert!(output.parser_ir.get("sentence_segmentation").is_some());
    assert!(output.parser_ir.get("sentences").is_some());
    assert!(output.parser_ir.get("orthographic_annotations").is_some());
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn orthographic_annotations_bundle_matches_golden_fixture() {
    let expected = read_json(
        &Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures/ortho-annotations-expected.json"),
    )
    .unwrap();

    assert_eq!(
        serde_json::to_value(ortho_fixture_bundle()).unwrap(),
        expected
    );
}

#[test]
fn ortho_indices_cover_multiple_annotations_in_one_sentence() {
    // Regression matrix (6b): a single sentence whose byte span overlaps two
    // orthographic annotations records both indices and a single katakana tag.
    let (schemas, mapping) = schemas_and_mapping();
    let hash = "sha256:3333333333333333333333333333333333333333333333333333333333333333";
    let aat = json!({
        "version": 1,
        "work_id": "000000",
        "meta": base_meta("utf-8", hash),
        "blocks": [{
            "kind": "paragraph",
            "content": [{ "kind": "text", "value": "吾輩ハ猫デアル。" }]
        }]
    });
    let bundle: ab_aat_to_parser_ir::ortho_annotations::OrthoAnnotationsBundle =
        serde_json::from_value(json!({
            "work_id": "000000",
            "primary_text_hash": hash,
            "coordinate_system": "decoded_utf8",
            "detector_id": "HeuristicV1",
            "annotations": [
                {"source_byte_range":{"start":0,"end":12},"normalized_text":"吾輩は猫","kind":"ScriptKatakanaToHiragana","confidence":null},
                {"source_byte_range":{"start":12,"end":24},"normalized_text":"であある。","kind":"ScriptKatakanaToHiragana","confidence":null}
            ]
        }))
        .unwrap();

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: ConversionOptions {
            orthographic_annotations: Some(bundle),
            ..default_test_options()
        },
    })
    .unwrap();

    assert_eq!(
        output.parser_ir.pointer("/sentences/0/span"),
        Some(&json!({"start":0,"end":24,"coordinate_system":"decoded_utf8"}))
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/sentences/0/orthographic_annotation_indices"),
        Some(&json!([0, 1]))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/0/tags"),
        Some(&json!(["orthographic-katakana"]))
    );
    // Exactly one sentence row: both annotations land in it.
    assert_eq!(output.parser_ir.pointer("/sentences/1"), None);
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn ortho_annotation_spanning_two_sentences_tags_both() {
    // Regression matrix (6c): a single annotation whose byte range crosses a
    // sentence boundary is recorded (and tagged) on both sentences; it must not
    // be snapped to one side.
    let (schemas, mapping) = schemas_and_mapping();
    let hash = "sha256:3333333333333333333333333333333333333333333333333333333333333333";
    let aat = json!({
        "version": 1,
        "work_id": "000000",
        "meta": base_meta("utf-8", hash),
        "blocks": [{
            "kind": "paragraph",
            "content": [{ "kind": "text", "value": "吾輩ハ猫。名前ハ無イ。" }]
        }]
    });
    let bundle: ab_aat_to_parser_ir::ortho_annotations::OrthoAnnotationsBundle =
        serde_json::from_value(json!({
            "work_id": "000000",
            "primary_text_hash": hash,
            "coordinate_system": "decoded_utf8",
            "detector_id": "HeuristicV1",
            "annotations": [
                {"source_byte_range":{"start":6,"end":21},"normalized_text":"猫。名前ハ","kind":"ScriptKatakanaToHiragana","confidence":null}
            ]
        }))
        .unwrap();

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: ConversionOptions {
            orthographic_annotations: Some(bundle),
            ..default_test_options()
        },
    })
    .unwrap();

    assert_eq!(
        output.parser_ir.pointer("/sentences/0/span"),
        Some(&json!({"start":0,"end":15,"coordinate_system":"decoded_utf8"}))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/1/span"),
        Some(&json!({"start":15,"end":33,"coordinate_system":"decoded_utf8"}))
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/sentences/0/orthographic_annotation_indices"),
        Some(&json!([0]))
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/sentences/1/orthographic_annotation_indices"),
        Some(&json!([0]))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/0/tags"),
        Some(&json!(["orthographic-katakana"]))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/1/tags"),
        Some(&json!(["orthographic-katakana"]))
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn coalesces_sentence_boundary_inside_atomic_ruby_child_from_aat() {
    // Regression matrix (6e): a sentence terminal inside a `ruby` base nested in
    // an emphasis. `ruby` stays atomic (B-D2), so conversion coalesces the
    // sentence bounds around the ruby child instead of slicing the node.
    let (schemas, mapping) = schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: include_fixture_json("atomic-boundary-emphasis-input.aat.json"),
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();
    assert_eq!(
        output.parser_ir.pointer("/sentences/0/span/start"),
        Some(&json!(0))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/0/span/end"),
        Some(&json!(9))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/0/node_range"),
        Some(&json!({"start":0,"end":1}))
    );
    assert_eq!(output.parser_ir.pointer("/sentences/1"), None);
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn splits_emphasis_container_at_sentence_boundary_keeping_ruby_whole() {
    // Phase B (B1): an emphasis carrying inline_children with an interior terminal
    // in a splittable text child splits into two sibling emphases; a ruby sibling
    // is kept whole. This is the case the old hard-fail regressed.
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "000000",
        "meta": base_meta(
            "utf-8",
            "sha256:5555555555555555555555555555555555555555555555555555555555555555",
        ),
        "blocks": [{
            "kind": "paragraph",
            "content": [{
                "kind": "style",
                "style_type": "sesame_dot",
                "content": [
                    { "kind": "text", "value": "甲。乙" },
                    { "kind": "ruby", "base": "丙", "reading": "へい", "direction": "right" }
                ]
            }]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    // Two emphasis siblings; the ruby stays a single whole child in the second.
    assert_eq!(
        output.parser_ir.pointer("/nodes/0/type"),
        Some(&json!("emphasis"))
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/0/text"),
        Some(&json!("甲。"))
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/1/type"),
        Some(&json!("emphasis"))
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/1/inline_children/1/ruby/base"),
        Some(&json!("丙"))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/0/span/end"),
        Some(&json!(6))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/1/span/start"),
        Some(&json!(6))
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn folded_pointer_protocol_matches_generator_examples() {
    use ab_aat_to_parser_ir::mapping::fold_aat_pointer;

    assert_eq!(
        fold_aat_pointer("blocks[3].children[1].heading.content[0].warigaki"),
        "blocks[].children[].heading.content[].warigaki"
    );
    assert_eq!(
        fold_aat_pointer("blocks[9].content[2].content[0].warigaki"),
        "blocks[].content[].content[].warigaki"
    );
    assert_eq!(
        fold_aat_pointer("meta.source_encoding=windows-31j-lossy"),
        "meta.source_encoding"
    );
    assert_eq!(fold_aat_pointer("(emphasis.text)"), "(emphasis.text)");
}

#[test]
fn divergence_records_aggregate_by_mapping_rule_and_validate_against_abc_schema() {
    let (schemas, mapping) = schemas_and_mapping();
    let index = mapping.preflight(&schemas).unwrap();

    let mut recorder = DivergenceRecorder::new(index);
    recorder
        .record(
            "AMBIGUITY",
            Some("meta.source_hash"),
            Some("source.primary_text_hash"),
            Some(json!(
                "sha256:0000000000000000000000000000000000000000000000000000000000000000"
            )),
            Some(json!(
                "sha256:0000000000000000000000000000000000000000000000000000000000000000"
            )),
        )
        .unwrap();
    recorder
        .record(
            "AMBIGUITY",
            Some("meta.source_hash"),
            Some("source.primary_text_hash"),
            None,
            None,
        )
        .unwrap();

    let validators = ab_aat_to_parser_ir::schema::SchemaValidators::compile(&schemas).unwrap();
    let bundle = recorder
        .bundle(
            AatMeta {
                work_id: "fixture".to_owned(),
                version: 1,
                adapter: "fixture".to_owned(),
                adapter_version: "fixture 0.1.0".to_owned(),
                source_hash:
                    "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                        .to_owned(),
                parse_complete: true,
                metrics: Value::Null,
                semantic_summary: Value::Null,
            },
            &validators,
            &mapping,
        )
        .unwrap();

    assert_eq!(
        bundle.pointer("/summary/AMBIGUITY").and_then(Value::as_u64),
        Some(2)
    );
    assert_eq!(
        bundle.pointer("/records/0/count").and_then(Value::as_u64),
        Some(2)
    );
    assert_eq!(
        bundle
            .pointer("/records/0/first_path")
            .and_then(Value::as_str),
        Some("meta.source_hash")
    );

    validate_value(&schemas.bundle_schema, &bundle, "bundle").unwrap();
    let record = bundle.pointer("/records/0").unwrap();
    validate_value(
        &schemas.abc_divergence_record_schema,
        record,
        "ABC divergence record",
    )
    .unwrap();
}

#[test]
fn converts_text_ruby_gaiji_and_validates_parser_ir() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "minimal",
        "meta": base_meta(
            "utf-8",
            "sha256:1111111111111111111111111111111111111111111111111111111111111111",
        ),
        "blocks": [{
            "kind": "paragraph",
            "content": [
                {"kind": "text", "value": "A"},
                {"kind": "ruby", "base": "B", "reading": "bee", "direction": "right"},
                {"kind": "gaiji", "description": "[gaiji]", "resolved": "G", "jis_code": "1-2-3", "unresolved_reason": null}
            ]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/type")
            .and_then(Value::as_str),
        Some("text")
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/1/ruby/direction")
            .and_then(Value::as_str),
        Some("right")
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/2/gaiji/raw_marker")
            .and_then(Value::as_str),
        Some("[gaiji]")
    );
    assert_eq!(
        output.parser_ir.pointer("/paragraphs/0/node_range/start"),
        Some(&json!(0))
    );
    assert_eq!(
        output.parser_ir.pointer("/paragraphs/0/node_range/end"),
        Some(&json!(3))
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/paragraphs/0/span_source")
            .and_then(Value::as_str),
        Some("derived")
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/paragraphs/0/role")
            .and_then(Value::as_str),
        Some("body")
    );
    assert!(
        output
            .divergence_bundle
            .pointer("/records")
            .and_then(Value::as_array)
            .unwrap()
            .iter()
            .any(|record| record["category"] == "AMBIGUITY"
                && record["aat_pointer"] == "blocks[].content[].gaiji.jis_code"
                && record["parser_ir_pointer"] == "gaiji.reference")
    );
    assert!(
        !output
            .divergence_bundle
            .pointer("/records")
            .and_then(Value::as_array)
            .unwrap()
            .iter()
            .any(|record| record["message"]
                .as_str()
                .unwrap_or("")
                .contains("direction"))
    );

    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn projects_source_derived_page_break_paragraph_to_page_break_node() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "page-break",
        "meta": base_meta(
            "utf-8",
            "sha256:1212121212121212121212121212121212121212121212121212121212121212",
        ),
        "blocks": [
            {
                "kind": "paragraph",
                "content": [],
                "x-break-kind": "page",
                "x-provenance": "source-derived"
            },
            {
                "kind": "paragraph",
                "content": [{"kind": "text", "value": "本文"}]
            }
        ]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/type")
            .and_then(Value::as_str),
        Some("page-break")
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/marker")
            .and_then(Value::as_str),
        Some("page")
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/1/type")
            .and_then(Value::as_str),
        Some("text")
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/paragraphs")
            .and_then(Value::as_array)
            .map(Vec::len),
        Some(1)
    );
    assert_eq!(
        output.parser_ir.pointer("/paragraphs/0/node_range/start"),
        Some(&json!(1))
    );
    assert_eq!(
        output.parser_ir.pointer("/paragraphs/0/node_range/end"),
        Some(&json!(2))
    );

    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn projects_source_derived_line_break_inline_to_line_break_node() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "line-break",
        "meta": base_meta(
            "utf-8",
            "sha256:1212121212121212121212121212121212121212121212121212121212121212",
        ),
        "blocks": [{
            "kind": "paragraph",
            "content": [
                {
                    "kind": "text",
                    "value": "前\n",
                    "x-break-kind": "line",
                    "x-provenance": "source-derived"
                },
                {"kind": "text", "value": "後"}
            ]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/type")
            .and_then(Value::as_str),
        Some("text")
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/text")
            .and_then(Value::as_str),
        Some("前")
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/1/type")
            .and_then(Value::as_str),
        Some("line-break")
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/1/marker")
            .and_then(Value::as_str),
        Some("line")
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/2/text")
            .and_then(Value::as_str),
        Some("後")
    );
    assert_eq!(
        output.parser_ir.pointer("/paragraphs/0/node_range"),
        Some(&json!({"start":0,"end":3}))
    );

    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn recovers_direct_raw_without_rendering_parser_residue_as_body_text() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "direct-raw-recovery",
        "meta": base_meta(
            "utf-8",
            "sha256:9191919191919191919191919191919191919191919191919191919191919191",
        ),
        "blocks": [
            {
                "kind": "paragraph",
                "content": [
                    {"kind": "raw", "source": "改頁", "x-provenance": "source-derived"}
                ]
            },
            {
                "kind": "paragraph",
                "content": [
                    {"kind": "raw", "source": "「姿が」は底本では「艇が」", "x-provenance": "source-derived"}
                ]
            },
            {
                "kind": "paragraph",
                "content": [
                    {"kind": "raw", "source": "BlockStart(Yokogumi)", "x-provenance": "parser-derived"}
                ]
            }
        ]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes")
            .and_then(Value::as_array)
            .unwrap(),
        &vec![
            json!({
                "type": "page-break",
                "span": {"start": 0, "end": 0, "line": null, "column": null, "coordinate_system": "decoded_utf8"},
                "marker": "page",
                "page_number": null,
            }),
            json!({
                "type": "editor-note",
                "span": {"start": 0, "end": 0, "line": null, "column": null, "coordinate_system": "decoded_utf8"},
                "note": {
                    "raw": "「姿が」は底本では「艇が」",
                    "category": "misc"
                }
            }),
        ]
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/paragraphs")
            .and_then(Value::as_array)
            .map(Vec::len),
        Some(2),
        "parser-derived raw residue must not create an empty body paragraph range"
    );
    assert!(has_divergence_record(
        &output,
        "UNSUPPORTED",
        "blocks[].content[].raw",
        None
    ));
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn recovers_accent_and_inline_yokogumi_without_fatal_conversion() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "accent-yokogumi-recovery",
        "meta": base_meta(
            "utf-8",
            "sha256:9292929292929292929292929292929292929292929292929292929292929292",
        ),
        "blocks": [
            {
                "kind": "paragraph",
                "content": [
                    {
                        "kind": "accent",
                        "code": "1-09-63",
                        "name": "アキュートアクセント付きE小文字",
                        "resolved": "é"
                    },
                    {
                        "kind": "yokogumi",
                        "content": [
                            {"kind": "text", "value": "ABC"}
                        ]
                    },
                    {
                        "kind": "style",
                        "style_type": "bold",
                        "content": [
                            {
                                "kind": "accent",
                                "code": "1-09-78",
                                "name": "グレーブアクセント付きU小文字",
                                "resolved": "ù"
                            },
                            {
                                "kind": "yokogumi",
                                "content": [
                                    {"kind": "text", "value": "12"}
                                ]
                            }
                        ]
                    }
                ]
            },
            {
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "（"},
                    {
                        "kind": "accent",
                        "code": "1-09-63",
                        "name": "アキュートアクセント付きE小文字",
                        "resolved": "é"
                    },
                    {
                        "kind": "yokogumi",
                        "content": [
                            {"kind": "text", "value": "ABC"}
                        ]
                    },
                    {"kind": "text", "value": "から。）"}
                ]
            }
        ]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    assert_eq!(
        output.parser_ir.pointer("/nodes/0"),
        Some(&json!({
            "type": "emphasis",
            "span": {"start": 0, "end": 2, "line": null, "column": null, "coordinate_system": "decoded_utf8"},
            "text": "é",
            "style": "1-09-63",
        }))
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/1"),
        Some(&json!({
            "type": "layout-span",
            "span": {"start": 2, "end": 5, "line": null, "column": null, "coordinate_system": "decoded_utf8"},
            "text": "ABC",
            "inline_children": [{
                "type": "text",
                "span": {"start": 2, "end": 5, "line": null, "column": null, "coordinate_system": "decoded_utf8"},
                "text": "ABC"
            }],
            "layout": {"kind": "yokogumi", "source": "aat-inline", "direction": "horizontal", "marker": null},
        }))
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/2/text"),
        Some(&json!("ù12"))
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/3/type"),
        Some(&json!("source-note"))
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/3/text"),
        Some(&json!("（éABCから。）"))
    );
    assert!(has_divergence_record(
        &output,
        "AMBIGUITY",
        "blocks[].content[].accent",
        Some("emphasis")
    ));
    assert!(has_divergence_record(
        &output,
        "INVENTION",
        "blocks[].content[].accent.code",
        Some("emphasis.style")
    ));
    assert!(has_divergence_record(
        &output,
        "LOSS",
        "blocks[].content[].accent.name",
        None
    ));
    assert!(
        !has_divergence_record(
            &output,
            "UNSUPPORTED",
            "blocks[].content[].yokogumi",
            Some("emphasis(?)")
        ),
        "inline yokogumi should be represented as layout-span after schema delta"
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn projects_burasage_style_wrapper_to_paragraph_layout() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "burasage-layout",
        "meta": base_meta(
            "utf-8",
            "sha256:1212121212121212121212121212121212121212121212121212121212121212",
        ),
        "blocks": [{
            "kind": "paragraph",
            "content": [{
                "kind": "style",
                "style_type": "burasage",
                "x-indent-first": 0,
                "x-indent-rest": 1,
                "x-provenance": "source-derived",
                "content": [{"kind": "text", "value": "本文"}]
            }]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    assert_eq!(
        output.parser_ir.pointer("/paragraphs/0/layout"),
        Some(&json!({
            "kind": "burasage",
            "source": "aat-style",
            "first_line_indent": 0,
            "continuation_indent": 1
        }))
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/type")
            .and_then(Value::as_str),
        Some("text")
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/text")
            .and_then(Value::as_str),
        Some("本文")
    );
    assert!(
        !output
            .parser_ir
            .pointer("/nodes")
            .and_then(Value::as_array)
            .unwrap()
            .iter()
            .any(|node| node["type"] == "emphasis" && node["style"] == "burasage")
    );

    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn projects_chitsuki_style_wrapper_to_paragraph_layout() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "chitsuki-layout",
        "meta": base_meta(
            "utf-8",
            "sha256:1212121212121212121212121212121212121212121212121212121212121212",
        ),
        "blocks": [{
            "kind": "paragraph",
            "content": [{
                "kind": "style",
                "style_type": "chitsuki",
                "x-align": "right",
                "x-offset": 1,
                "x-provenance": "source-derived",
                "content": [{"kind": "text", "value": "了"}]
            }]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    assert_eq!(
        output.parser_ir.pointer("/paragraphs/0/layout"),
        Some(&json!({
            "kind": "chitsuki",
            "source": "aat-style",
            "align": "right",
            "offset_from_end": 1
        }))
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/type")
            .and_then(Value::as_str),
        Some("text")
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/text")
            .and_then(Value::as_str),
        Some("了")
    );
    assert!(
        !output
            .parser_ir
            .pointer("/nodes")
            .and_then(Value::as_array)
            .unwrap()
            .iter()
            .any(|node| node["type"] == "emphasis" && node["style"] == "chitsuki")
    );

    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn projects_jisage_block_wrapped_paragraph_to_paragraph_layout() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "jisage-layout",
        "meta": base_meta(
            "utf-8",
            "sha256:1212121212121212121212121212121212121212121212121212121212121212",
        ),
        "blocks": [{
            "kind": "jisage_block",
            "x-indent": 4,
            "children": [{
                "kind": "paragraph",
                "content": [{"kind": "text", "value": "字下げ"}]
            }]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    assert_eq!(
        output.parser_ir.pointer("/paragraphs/0/layout"),
        Some(&json!({
            "kind": "jisage",
            "source": "aat-block",
            "indent": 4
        }))
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/type")
            .and_then(Value::as_str),
        Some("text")
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/text")
            .and_then(Value::as_str),
        Some("字下げ")
    );
    assert!(
        !output
            .parser_ir
            .pointer("/nodes")
            .and_then(Value::as_array)
            .unwrap()
            .iter()
            .any(|node| node["type"] == "indentation")
    );

    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

fn v2_schemas_and_mapping() -> (SchemaSet, MappingDocument) {
    let (repo_root, abc_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v2.json"))
            .unwrap();
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &abc_root, 2).unwrap();
    (schemas, mapping)
}

/// A schema-valid v2 `meta` block for tests: `warnings: []` keeps the fixture
/// minimal (the individual warning-field LOSS accounting is exercised
/// separately in `map_warnings`, not by these layout/source-note tests).
fn v2_test_meta() -> Value {
    json!({
        "adapter": "fixture",
        "adapter_version": "fixture 0.1.0",
        "source_encoding": "utf-8",
        "source_hash": "sha256:1212121212121212121212121212121212121212121212121212121212121212",
        "parse_complete": true,
        "warnings": []
    })
}

fn paragraph_layout_of(output: &ab_aat_to_parser_ir::ConversionOutput, index: usize) -> Value {
    output
        .parser_ir
        .pointer(&format!("/paragraphs/{index}/layout"))
        .cloned()
        .unwrap_or(Value::Null)
}

#[test]
fn v2_jizume_block_projects_paragraph_layout() {
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat = json!({
        "version": 2, "work_id": "t-jizume",
        "blocks": [{ "kind": "jizume_block", "width": 21, "children": [
            { "kind": "paragraph", "content": [{ "kind": "text", "value": "本文" }] } ] }],
        "meta": v2_test_meta()
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    let layout = paragraph_layout_of(&output, 0);
    assert_eq!(
        layout,
        json!({ "kind": "jizume", "source": "aat-block", "width": 21 })
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/text")
            .and_then(Value::as_str),
        Some("本文")
    );

    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn v2_jizume_block_mixed_children_fallback_accounts_invention() {
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat = json!({
        "version": 2, "work_id": "t-jizume-fallback",
        "blocks": [{ "kind": "jizume_block", "width": 21, "children": [
            { "kind": "paragraph", "content": [{ "kind": "text", "value": "本文" }] },
            { "kind": "heading", "level": 1, "style": "normal",
              "content": [{ "kind": "text", "value": "見出し" }] } ] }],
        "meta": v2_test_meta()
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    let nodes = output
        .parser_ir
        .pointer("/nodes")
        .and_then(Value::as_array)
        .unwrap();
    assert!(
        nodes.iter().any(|node| node["type"] == "indentation"),
        "mixed-children jizume_block fallback must emit an indentation node"
    );

    assert!(has_divergence_record(
        &output,
        "STRUCTURAL",
        "blocks[].jizume_block",
        None
    ));
    assert!(has_divergence_record(
        &output,
        "INVENTION",
        "blocks[].jizume_block",
        Some("indentation")
    ));

    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn v2_typed_layout_fields_project_without_x_names() {
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat = json!({
        "version": 2, "work_id": "t-burasage",
        "blocks": [{ "kind": "paragraph", "content": [{
            "kind": "style", "style_type": "burasage",
            "indent_first": 6, "indent_rest": 7, "x-provenance": "source-derived",
            "content": [{ "kind": "text", "value": "本文" }] }] }],
        "meta": v2_test_meta()
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    let layout = paragraph_layout_of(&output, 0);
    assert_eq!(
        layout,
        json!({
            "kind": "burasage",
            "source": "aat-style",
            "first_line_indent": 6,
            "continuation_indent": 7
        })
    );

    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn v2_never_applies_source_note_heuristic() {
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat = json!({
        "version": 2, "work_id": "t-heuristic-v2",
        "blocks": [
            { "kind": "paragraph", "content": [{ "kind": "text", "value": "本文" }] },
            { "kind": "paragraph", "content": [{ "kind": "text", "value": "（テストから。）" }] }
        ],
        "meta": v2_test_meta()
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    assert!(
        !output
            .parser_ir
            .pointer("/nodes")
            .and_then(Value::as_array)
            .unwrap()
            .iter()
            .any(|node| node["type"] == "source-note"),
        "v2 documents must never trigger the v1 source-attribution heuristic"
    );
    assert!(
        output
            .parser_ir
            .pointer("/paragraphs")
            .and_then(Value::as_array)
            .unwrap()
            .iter()
            .all(|paragraph| paragraph["role"] == "body")
    );

    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn v1_heuristic_still_fires_byte_identically() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1, "work_id": "t-heuristic-v1",
        "blocks": [
            { "kind": "paragraph", "content": [{ "kind": "text", "value": "本文" }] },
            { "kind": "paragraph", "content": [{ "kind": "text", "value": "（テストから。）" }] }
        ],
        "meta": base_meta(
            "utf-8",
            "sha256:1212121212121212121212121212121212121212121212121212121212121212",
        )
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    let nodes = output
        .parser_ir
        .pointer("/nodes")
        .and_then(Value::as_array)
        .unwrap();
    assert!(
        nodes
            .iter()
            .any(|node| node["type"] == "source-note" && node["classification"] == "heuristic")
    );
    let paragraphs = output
        .parser_ir
        .pointer("/paragraphs")
        .and_then(Value::as_array)
        .unwrap();
    assert!(
        paragraphs
            .iter()
            .any(|paragraph| paragraph["role"] == "source-note"
                && paragraph["classification"] == "heuristic")
    );

    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn v2_explicit_source_note_converts_direct() {
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat = json!({
        "version": 2, "work_id": "t-source-note",
        "blocks": [
            { "kind": "paragraph", "content": [{ "kind": "text", "value": "本文" }] },
            {
                "kind": "source_note",
                "placement": "back",
                "region_class": "terminal_provenance",
                "content": [{ "kind": "text", "value": "底本：テスト文庫" }],
                "span": { "line_start": 10, "line_end": 10, "byte_start": 100, "byte_end": 120 }
            }
        ],
        "meta": v2_test_meta()
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    let nodes = output
        .parser_ir
        .pointer("/nodes")
        .and_then(Value::as_array)
        .unwrap();
    let note_index = nodes
        .iter()
        .position(|node| node["type"] == "source-note")
        .expect("source-note node present");
    let note = &nodes[note_index];
    assert_eq!(note["note_type"], json!("source-attribution"));
    assert_eq!(note["placement"], json!("back"));
    assert_eq!(note["classification"], json!("direct"));
    assert_eq!(note["source_pointer"], json!("blocks[1]"));
    assert_eq!(note["text"], json!("底本：テスト文庫"));

    let paragraphs = output
        .parser_ir
        .pointer("/paragraphs")
        .and_then(Value::as_array)
        .unwrap();
    let row = paragraphs
        .iter()
        .find(|paragraph| paragraph["role"] == "source-note")
        .expect("source-note paragraph row present");
    assert_eq!(row["classification"], json!("direct"));
    assert_eq!(
        row["node_range"],
        json!({ "start": note_index, "end": note_index + 1 })
    );

    assert!(has_divergence_record(
        &output,
        "STRUCTURAL",
        "blocks[].source_note",
        Some("source-note")
    ));
    assert!(has_divergence_record(
        &output,
        "LOSS",
        "blocks[].source_note.region_class",
        Some("(source-note.note_type)")
    ));

    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn v2_source_note_unknown_region_class_fails_closed() {
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat = json!({
        "version": 2, "work_id": "t-source-note-unmapped",
        "blocks": [
            {
                "kind": "source_note",
                "placement": "back",
                "region_class": "colophon_metadata",
                "content": [{ "kind": "text", "value": "発行者：テスト" }],
                "span": { "line_start": 10, "line_end": 10, "byte_start": 100, "byte_end": 120 }
            }
        ],
        "meta": v2_test_meta()
    });

    let error = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: default_test_options(),
    })
    .unwrap_err();

    assert!(
        error
            .to_string()
            .contains("unmapped source_note region_class"),
        "{error}"
    );
}

#[test]
fn v2_warning_span_loss_record_is_schema_valid_scalar() {
    // C3 audit blocker regression: a v2 `meta.warnings[].span` is a
    // structured object (line_start/line_end/byte_start/byte_end), not a
    // schema-legal `source_value` scalar (string/integer/boolean/null per
    // aat-parser-ir-divergence.schema.json). Conversion must still succeed,
    // and the resulting divergence record's `source_value` must validate
    // against the divergence-record schema (which `recorder.bundle()`
    // already enforces per-record; this pins the regression at the
    // integration level too).
    let (schemas, mapping) = v2_schemas_and_mapping();
    let mut meta = v2_test_meta();
    meta["warnings"] = json!([{
        "code": "FIXTURE_WARNING",
        "severity": "warning",
        "message": "fixture warning with span",
        "span": { "line_start": 3, "line_end": 3, "byte_start": 10, "byte_end": 20 }
    }]);
    let aat = json!({
        "version": 2, "work_id": "t-warning-span",
        "blocks": [{ "kind": "paragraph", "content": [{ "kind": "text", "value": "本文" }] }],
        "meta": meta
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    let span_record = output
        .divergence_bundle
        .pointer("/records")
        .and_then(Value::as_array)
        .expect("divergence bundle records")
        .iter()
        .find(|record| record["aat_pointer"] == "meta.warnings[].span")
        .expect("meta.warnings[].span LOSS record present");
    assert!(
        span_record["source_value"].is_null(),
        "structured span must not leak into source_value: {span_record}"
    );

    validate_value(
        &schemas.abc_divergence_record_schema,
        span_record,
        "ABC divergence record",
    )
    .unwrap();
    validate_value(&schemas.bundle_schema, &output.divergence_bundle, "bundle").unwrap();
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn projects_measured_figure_inline_to_image_node() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "figure",
        "meta": base_meta(
            "utf-8",
            "sha256:1212121212121212121212121212121212121212121212121212121212121212",
        ),
        "blocks": [{
            "kind": "paragraph",
            "content": [
                {"kind": "text", "value": "A"},
                {
                    "kind": "figure",
                    "filename": "figures/001.png",
                    "alt": "Figure alt",
                    "css_class": "illustration",
                    "width": 640,
                    "height": 480,
                    "caption": [{"kind": "text", "value": "caption"}]
                },
                {"kind": "text", "value": "B"}
            ]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    let nodes = output.parser_ir["nodes"].as_array().unwrap();
    assert_eq!(nodes[1]["type"], "image");
    assert_eq!(nodes[1]["src"], "figures/001.png");
    assert_eq!(nodes[1]["alt"], "Figure alt");
    assert_eq!(nodes[1]["span"]["start"], 1);
    assert_eq!(nodes[1]["span"]["end"], 1);
    assert_eq!(nodes[2]["text"], "B");
    assert_eq!(nodes[2]["span"]["start"], 1);
    for (category, aat_pointer, parser_ir_pointer) in [
        (
            "INVENTION",
            "blocks[].content[].figure.filename",
            Some("image.src"),
        ),
        ("LOSS", "blocks[].content[].figure.caption", None),
        ("LOSS", "blocks[].content[].figure.css_class", None),
        ("LOSS", "blocks[].content[].figure.height", None),
        ("LOSS", "blocks[].content[].figure.width", None),
    ] {
        assert!(
            has_divergence_record(&output, category, aat_pointer, parser_ir_pointer),
            "missing measured figure divergence {category} {aat_pointer:?} {parser_ir_pointer:?}"
        );
    }

    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn measured_policy_projects_style_heading_warning_and_warigaki() {
    let (schemas, mapping) = schemas_and_mapping();
    let mut meta = base_meta(
        "windows-31j-lossy",
        "sha256:2222222222222222222222222222222222222222222222222222222222222222",
    );
    meta["warnings"] = json!([{ "message": "fixture warning", "line": 2 }]);
    let aat = json!({
        "version": 1,
        "work_id": "policy",
        "meta": meta,
        "blocks": [
            {
                "kind": "jisage_block",
                "children": [{
                    "kind": "heading",
                    "level": 1,
                    "style": "normal",
                    "content": [{
                        "kind": "warigaki",
                        "upper": [{"kind": "text", "value": "U"}],
                        "lower": [{"kind": "text", "value": "L"}]
                    }]
                }]
            },
            {
                "kind": "paragraph",
                "content": [
                    {"kind": "style", "style_type": "kaeriten", "content": [{"kind": "text", "value": "K"}]},
                    {"kind": "warigaki", "upper": [{"kind": "text", "value": "X"}], "lower": [{"kind": "text", "value": "Y"}]}
                ]
            }
        ]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    let nodes = output.parser_ir["nodes"].as_array().unwrap();
    assert!(
        nodes
            .iter()
            .any(|node| node["type"] == "heading" && node["text"] == "UL")
    );
    assert!(nodes.iter().any(|node| node["type"] == "emphasis"
        && node["style"] == "kaeriten"
        && node["text"] == "K"));
    assert!(
        nodes
            .iter()
            .any(|node| node["type"] == "text" && node["text"] == "X")
    );
    assert!(
        nodes
            .iter()
            .any(|node| node["type"] == "text" && node["text"] == "Y")
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/source/encoding")
            .and_then(Value::as_str),
        Some("Shift_JIS")
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/warnings/0/code")
            .and_then(Value::as_str),
        Some("AAT_WARNING")
    );
    assert!(has_divergence_record(
        &output,
        "UNSUPPORTED",
        "blocks[].children[].heading.content[].warigaki",
        None
    ));
    assert!(has_divergence_record(
        &output,
        "AMBIGUITY",
        "meta.source_encoding",
        Some("source.encoding")
    ));
    assert!(
        !output
            .divergence_bundle
            .pointer("/records")
            .and_then(Value::as_array)
            .unwrap()
            .iter()
            .any(|record| record["parser_ir_pointer"] == "warnings[].span.line")
    );

    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn preserves_ruby_inside_emphasis_inline_children() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "emphasis-inline-children",
        "meta": base_meta(
            "utf-8",
            "sha256:9393939393939393939393939393939393939393939393939393939393939393",
        ),
        "blocks": [{
            "kind": "paragraph",
            "content": [{
                "kind": "style",
                "style_type": "bold",
                "content": [
                    {"kind": "text", "value": "前"},
                    {"kind": "ruby", "base": "東京", "reading": "とうきょう", "direction": "right"},
                    {"kind": "text", "value": "後"}
                ]
            }]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    assert_eq!(
        output.parser_ir.pointer("/nodes/0/type"),
        Some(&json!("emphasis"))
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/0/text"),
        Some(&json!("前東京後"))
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/0/inline_children/0/text"),
        Some(&json!("前"))
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/inline_children/1/ruby/reading"),
        Some(&json!("とうきょう"))
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/0/inline_children/2/text"),
        Some(&json!("後"))
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn preserves_gaiji_inside_emphasis_inline_children() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "emphasis-inline-gaiji",
        "meta": base_meta(
            "utf-8",
            "sha256:a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1",
        ),
        "blocks": [{
            "kind": "paragraph",
            "content": [{
                "kind": "style",
                "style_type": "bold",
                "content": [
                    {"kind": "text", "value": "前"},
                    {
                        "kind": "gaiji",
                        "description": "gaiji-G",
                        "resolved": "G",
                        "jis_code": null,
                        "unresolved_reason": null
                    },
                    {"kind": "text", "value": "後"}
                ]
            }]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    assert_eq!(
        output.parser_ir.pointer("/nodes/0/text"),
        Some(&json!("前G後"))
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/inline_children/1/gaiji/unicode"),
        Some(&json!("G"))
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/inline_children/1/gaiji/raw_marker"),
        Some(&json!("gaiji-G"))
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn preserves_nested_inline_container_children() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "nested-inline-container",
        "meta": base_meta(
            "utf-8",
            "sha256:9494949494949494949494949494949494949494949494949494949494949494",
        ),
        "blocks": [{
            "kind": "paragraph",
            "content": [{
                "kind": "style",
                "style_type": "outer",
                "content": [{
                    "kind": "tcy",
                    "content": [
                        {"kind": "text", "value": "12"}
                    ]
                }]
            }]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    assert_eq!(
        output.parser_ir.pointer("/nodes/0/style"),
        Some(&json!("outer"))
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/0/text"),
        Some(&json!("12"))
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/inline_children/0/layout/kind"),
        Some(&json!("tcy"))
    );
    assert_eq!(
        output
            .parser_ir
            .pointer("/nodes/0/inline_children/0/inline_children/0/text"),
        Some(&json!("12"))
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn inline_children_depth_limit_records_warning() {
    let (schemas, mapping) = schemas_and_mapping();
    let mut nested = json!({"kind": "text", "value": "深"});
    for _ in 0..65 {
        nested = json!({
            "kind": "style",
            "style_type": "nested",
            "content": [nested]
        });
    }
    let aat = json!({
        "version": 1,
        "work_id": "inline-depth-warning",
        "meta": base_meta(
            "utf-8",
            "sha256:b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2",
        ),
        "blocks": [{
            "kind": "paragraph",
            "content": [nested]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    assert_eq!(
        output.parser_ir.pointer("/nodes/0/type"),
        Some(&json!("emphasis"))
    );
    let warnings = output
        .parser_ir
        .pointer("/warnings")
        .and_then(Value::as_array)
        .expect("warnings array");
    assert!(
        warnings.iter().any(|warning| {
            warning["message"]
                .as_str()
                .is_some_and(|message| message.contains("inline_children depth limit"))
                && warning["message"]
                    .as_str()
                    .is_some_and(|message| message.contains("blocks[0].content[0]"))
        }),
        "expected inline_children depth warning, got {warnings:?}"
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn measured_policy_flattens_epub3_tcy_and_block_containers() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "epub3-containers",
        "meta": base_meta(
            "utf-8",
            "sha256:4444444444444444444444444444444444444444444444444444444444444444",
        ),
        "blocks": [
            {
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "第"},
                    {"kind": "tcy", "content": [{"kind": "text", "value": "10"}]},
                    {"kind": "text", "value": "章"}
                ]
            },
            {
                "kind": "keigakomi_block",
                "children": []
            },
            {
                "kind": "yokogumi_block",
                "children": [
                    {
                        "kind": "paragraph",
                        "content": [{"kind": "text", "value": "横組"}]
                    }
                ]
            }
        ]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    let projected_text = output
        .parser_ir
        .pointer("/nodes")
        .and_then(Value::as_array)
        .unwrap()
        .iter()
        .filter_map(|node| {
            if node["type"] == "text" || node["type"] == "emphasis" || node["type"] == "layout-span"
            {
                node["text"].as_str()
            } else {
                None
            }
        })
        .collect::<String>();
    assert_eq!(projected_text, "第10章横組");

    assert!(
        !output
            .divergence_bundle
            .pointer("/records")
            .and_then(Value::as_array)
            .unwrap()
            .iter()
            .any(|record| {
                record["category"] == "UNSUPPORTED"
                    && record["aat_pointer"]
                        .as_str()
                        .is_some_and(|pointer| pointer.contains("tcy"))
            }),
        "inline tcy should be represented as layout-span after schema delta"
    );
    assert!(
        output
            .divergence_bundle
            .pointer("/records")
            .and_then(Value::as_array)
            .unwrap()
            .iter()
            .any(|record| {
                record["category"] == "UNSUPPORTED"
                    && record["aat_pointer"]
                        .as_str()
                        .is_some_and(|pointer| pointer.contains("keigakomi_block"))
            }),
        "expected measured keigakomi_block unsupported divergence"
    );
    assert!(
        output
            .divergence_bundle
            .pointer("/records")
            .and_then(Value::as_array)
            .unwrap()
            .iter()
            .any(|record| {
                record["category"] == "UNSUPPORTED"
                    && record["aat_pointer"]
                        .as_str()
                        .is_some_and(|pointer| pointer.contains("yokogumi_block"))
            }),
        "expected measured yokogumi_block unsupported divergence"
    );

    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn structural_probe_detects_melos_level3_gap() {
    let (schemas, mapping) = schemas_and_mapping();
    let temp = tempfile::tempdir().unwrap();
    let aat_path = temp.path().join("melos.aat.json");
    let aat = json!({
        "version": 1,
        "work_id": "000035_1567",
        "meta": base_meta(
            "utf-8",
            "sha256:5555555555555555555555555555555555555555555555555555555555555555",
        ),
        "blocks": [
            {
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "メロスは激怒した。"}
                ]
            },
            {
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "（古伝説と、シルレルの詩から。）"}
                ]
            }
        ]
    });
    std::fs::write(&aat_path, serde_json::to_string_pretty(&aat).unwrap()).unwrap();

    let summary = ab_aat_to_parser_ir::structural_probe::run_structural_probe(
        ab_aat_to_parser_ir::structural_probe::StructuralProbeConfig {
            inputs: vec![
                ab_aat_to_parser_ir::structural_probe::StructuralProbeInput {
                    label: "fixture".to_owned(),
                    path: aat_path,
                },
            ],
            mapping,
            schemas,
        },
    )
    .unwrap();

    assert_eq!(summary.totals.inputs, 1);
    let item = &summary.inputs[0];
    assert_eq!(item.label, "fixture");
    assert_eq!(item.aat.paragraph_blocks, 2);
    assert_eq!(
        item.aat.final_visible_text.as_deref(),
        Some("（古伝説と、シルレルの詩から。）")
    );
    assert_eq!(
        item.aat.final_source_attribution_text.as_deref(),
        Some("（古伝説と、シルレルの詩から。）")
    );
    assert!(item.aat.final_source_attribution_candidate);
    assert!(item.conversion.success);
    assert_eq!(item.parser_ir.paragraph_count, 2);
    assert!(item.parser_ir.paragraphs_represented);
    assert!(item.parser_ir.source_attribution_represented);
    assert!(item.verdict.residual_free);
    assert!(item.divergence.paragraph_structural_records.is_empty());
}

#[test]
fn tei_eaj_structural_expansion_classifies_parser_ir_and_evidence_gaps() {
    let (schemas, mapping) = schemas_and_mapping();
    let temp = tempfile::tempdir().unwrap();
    let aat_dir = temp.path().join("aat");
    let mixed_aat_dir = temp.path().join("mixed-aat");
    let failing_aat_dir = temp.path().join("failing-aat");
    std::fs::create_dir_all(&aat_dir).unwrap();
    std::fs::create_dir_all(&mixed_aat_dir).unwrap();
    std::fs::create_dir_all(&failing_aat_dir).unwrap();
    let tei_root = temp.path().join("tei-eaj");
    let tei_file = tei_root.join("data/complete/tei_lib_lv3/236_tei.xml");
    std::fs::create_dir_all(tei_file.parent().unwrap()).unwrap();
    std::fs::write(
        &tei_file,
        r#"<TEI><teiHeader><p>見出し</p><note>ヘッダ</note></teiHeader><text><body><p>一</p><p>二<note>注</note></p></body></text></TEI>"#,
    )
    .unwrap();
    let aat_path = aat_dir.join("000035_1567-fixture.json");
    let aat = json!({
        "version": 1,
        "work_id": "000035_1567",
        "meta": base_meta(
            "utf-8",
            "sha256:5656565656565656565656565656565656565656565656565656565656565656",
        ),
        "blocks": [
            {
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "メロスは激怒した。"}
                ]
            },
            {
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "（古伝説と、シルレルの詩から。）"}
                ]
            }
        ]
    });
    std::fs::write(&aat_path, serde_json::to_string_pretty(&aat).unwrap()).unwrap();
    let mixed_aat_path = mixed_aat_dir.join("000035_1567-mixed-fixture.json");
    let mixed_aat = json!({
        "version": 1,
        "work_id": "000035_1567",
        "meta": base_meta(
            "utf-8",
            "sha256:5757575757575757575757575757575757575757575757575757575757575757",
        ),
        "blocks": [
            {
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "勇者は、ひどく赤面した。（古伝説と、シルレルの詩から。）"}
                ]
            }
        ]
    });
    std::fs::write(
        &mixed_aat_path,
        serde_json::to_string_pretty(&mixed_aat).unwrap(),
    )
    .unwrap();
    let failing_aat_path = failing_aat_dir.join("000035_1567-failing-fixture.json");
    let failing_aat = json!({
        "version": 1,
        "work_id": "000035_1567",
        "meta": base_meta(
            "utf-8",
            "sha256:5959595959595959595959595959595959595959595959595959595959595959",
        ),
        "blocks": [
            {
                "kind": "paragraph",
                "content": [
                    {"kind": "raw", "source": "unmapped parser residue"}
                ]
            }
        ]
    });
    std::fs::write(
        &failing_aat_path,
        serde_json::to_string_pretty(&failing_aat).unwrap(),
    )
    .unwrap();

    let workset_path = temp.path().join("tei-eaj-workset.json");
    let workset = json!({
        "schema_version": "tei-eaj-aozora-workset-export-v1",
        "summary": {
            "tei_eaj_file_count": 2,
            "tei_eaj_work_id_count": 2,
            "abc_counterpart_count": 1,
            "compared_file_count": 1,
            "missing_counterpart_count": 1,
            "no_work_id_count": 0,
            "base_text_equal_count": 0,
            "base_text_mismatch_count": 1,
            "uncompared_file_count": 1
        },
        "tei_eaj_source": {
            "revision": "fixture-revision",
            "root": tei_root.display().to_string()
        },
        "abc_inputs": {
            "counterparts": [{"path": "tests/fixtures/tei-eaj-alignment-probe/abc-melos-single-p.xml", "work_id": "1567"}],
            "tei_dirs": [],
            "tei_specs": []
        },
        "candidate_work_ids": ["1567", "236"],
        "missing_abc_counterpart_work_ids": ["236"],
        "no_work_id_files": [],
        "files": [
            {
                "abc_body_base_text_length": 9806,
                "abc_note_count": 1,
                "abc_p_count": 1,
                "abc_tei": "tests/fixtures/tei-eaj-alignment-probe/abc-melos-single-p.xml",
                "base_text_equal": false,
                "comparison_status": "compared",
                "first_difference": {
                    "abc": "勇者は、ひどく赤面した。（古伝説と、シルレルの詩から。）",
                    "index": 9790,
                    "tei_eaj": "勇者は、ひどく赤面した。"
                },
                "level": "Level 4",
                "state": "complete",
                "tei_eaj_body_base_text_length": 9790,
                "tei_eaj_file": "data/complete/tei_lib_lv4/1567_tei.xml",
                "tei_eaj_note_count": 0,
                "tei_eaj_p_count": 19,
                "title": "走れメロス",
                "work_id": "1567"
            },
            {
                "abc_body_base_text_length": null,
                "abc_note_count": null,
                "abc_p_count": null,
                "abc_tei": null,
                "base_text_equal": null,
                "comparison_status": "missing_abc_counterpart",
                "first_difference": null,
                "level": "Level 3",
                "state": "complete",
                "tei_eaj_body_base_text_length": 1517,
                "tei_eaj_file": "data/complete/tei_lib_lv3/236_tei.xml",
                "tei_eaj_note_count": null,
                "tei_eaj_p_count": null,
                "title": "ア、秋",
                "work_id": "236"
            }
        ]
    });
    std::fs::write(
        &workset_path,
        serde_json::to_string_pretty(&workset).unwrap(),
    )
    .unwrap();

    let summary = ab_aat_to_parser_ir::structural_probe::run_tei_eaj_structural_expansion(
        ab_aat_to_parser_ir::structural_probe::TeiEajStructuralExpansionConfig {
            workset_path,
            aat_dirs: vec![
                ab_aat_to_parser_ir::structural_probe::StructuralProbeInput {
                    label: "fixture-adapter".to_owned(),
                    path: aat_dir,
                },
                ab_aat_to_parser_ir::structural_probe::StructuralProbeInput {
                    label: "mixed-adapter".to_owned(),
                    path: mixed_aat_dir,
                },
                ab_aat_to_parser_ir::structural_probe::StructuralProbeInput {
                    label: "failing-adapter".to_owned(),
                    path: failing_aat_dir,
                },
            ],
            mapping,
            schemas,
        },
    )
    .unwrap();

    assert_eq!(
        summary.workset.schema_version,
        "tei-eaj-aozora-workset-export-v1"
    );
    assert_eq!(summary.totals.tei_eaj_files, 2);
    assert_eq!(summary.totals.rows_with_aat_evidence, 1);
    assert_eq!(summary.totals.parser_ir_gap_rows, 0);
    assert_eq!(summary.totals.evidence_gap_rows, 1);

    let melos = summary
        .rows
        .iter()
        .find(|row| row.tei.work_id.as_deref() == Some("1567"))
        .unwrap();
    assert_eq!(melos.tei.tei_eaj_p_count, Some(19));
    assert_eq!(melos.aat_inputs.len(), 3);
    assert!(
        melos
            .aat_inputs
            .iter()
            .any(|input| { input.label == "failing-adapter:1567" && input.conversion.success })
    );
    assert!(!melos.classification.parser_ir_gap);
    assert!(!melos.classification.source_attribution_gap);
    assert!(melos.classification.adapter_gap);
    assert!(!melos.classification.evidence_gap);

    let missing = summary
        .rows
        .iter()
        .find(|row| row.tei.work_id.as_deref() == Some("236"))
        .unwrap();
    assert_eq!(missing.tei.tei_eaj_p_count, Some(2));
    assert_eq!(missing.tei.tei_eaj_note_count, Some(1));
    assert!(missing.aat_inputs.is_empty());
    assert!(missing.classification.evidence_gap);
    assert!(
        missing
            .classification
            .notes
            .iter()
            .any(|note| note.contains("no AAT evidence"))
    );

    let markdown =
        ab_aat_to_parser_ir::structural_probe::render_tei_eaj_expansion_markdown(&summary);
    assert!(markdown.contains("# TEI-EAJ Structural Expansion"));
    assert!(markdown.contains("| 1567 | 走れメロス | compared | 19 | 1 | 3 | false |"));
    assert!(markdown.contains("parser-IR gap"));
    assert!(markdown.contains("evidence gap"));
}

#[test]
fn tei_eaj_structural_expansion_maps_tei_file_ids_to_aozora_work_ids() {
    let (schemas, mapping) = schemas_and_mapping();
    let temp = tempfile::tempdir().unwrap();
    let aat_dir = temp.path().join("aat");
    std::fs::create_dir_all(&aat_dir).unwrap();
    let aat_path = aat_dir.join("000879_104-fixture.json");
    let aat = json!({
        "version": 1,
        "work_id": "000879_104",
        "meta": base_meta(
            "utf-8",
            "sha256:5858585858585858585858585858585858585858585858585858585858585858",
        ),
        "blocks": [
            {
                "kind": "paragraph",
                "content": [{"kind": "text", "value": "長崎小品一。"}]
            },
            {
                "kind": "paragraph",
                "content": [{"kind": "text", "value": "長崎小品二。"}]
            }
        ]
    });
    std::fs::write(&aat_path, serde_json::to_string_pretty(&aat).unwrap()).unwrap();

    let workset_path = temp.path().join("tei-eaj-workset.json");
    let workset = json!({
        "schema_version": "tei-eaj-aozora-workset-export-v1",
        "summary": {
            "tei_eaj_file_count": 2,
            "tei_eaj_work_id_count": 1,
            "compared_file_count": 0,
            "missing_counterpart_count": 2,
            "no_work_id_count": 0
        },
        "candidate_work_ids": ["15099"],
        "missing_abc_counterpart_work_ids": ["15099"],
        "no_work_id_files": [],
        "files": [
            {
                "abc_body_base_text_length": null,
                "abc_note_count": null,
                "abc_p_count": null,
                "abc_tei": null,
                "base_text_equal": null,
                "comparison_status": "missing_abc_counterpart",
                "first_difference": null,
                "level": "Level 3",
                "state": "complete",
                "tei_eaj_body_base_text_length": 100,
                "tei_eaj_file": "data/complete/tei_lib_lv3/15099_tei.xml",
                "tei_eaj_note_count": 0,
                "tei_eaj_p_count": 1,
                "title": "長崎小品",
                "work_id": "15099"
            },
            {
                "abc_body_base_text_length": null,
                "abc_note_count": null,
                "abc_p_count": null,
                "abc_tei": null,
                "base_text_equal": null,
                "comparison_status": "missing_abc_counterpart",
                "first_difference": null,
                "level": "Level 4",
                "state": "complete",
                "tei_eaj_body_base_text_length": 200,
                "tei_eaj_file": "data/complete/tei_lib_lv4/104_15099.xml",
                "tei_eaj_note_count": 0,
                "tei_eaj_p_count": 57,
                "title": "長崎小品",
                "work_id": "15099"
            }
        ]
    });
    std::fs::write(
        &workset_path,
        serde_json::to_string_pretty(&workset).unwrap(),
    )
    .unwrap();

    let summary = ab_aat_to_parser_ir::structural_probe::run_tei_eaj_structural_expansion(
        ab_aat_to_parser_ir::structural_probe::TeiEajStructuralExpansionConfig {
            workset_path,
            aat_dirs: vec![
                ab_aat_to_parser_ir::structural_probe::StructuralProbeInput {
                    label: "fixture-adapter".to_owned(),
                    path: aat_dir,
                },
            ],
            mapping,
            schemas,
        },
    )
    .unwrap();

    assert_eq!(summary.totals.rows_with_aat_evidence, 2);
    assert_eq!(summary.totals.evidence_gap_rows, 0);
    assert!(summary.rows.iter().all(|row| row.aat_inputs.len() == 1));
    assert!(summary.rows.iter().all(|row| {
        row.aat_inputs[0].label == "fixture-adapter:15099"
            && row.aat_inputs[0].aat.work_id.as_deref() == Some("000879_104")
            && !row.classification.evidence_gap
    }));
}

#[test]
fn heading_preserves_structured_inline_children() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "heading-losses",
        "meta": base_meta(
            "utf-8",
            "sha256:7777777777777777777777777777777777777777777777777777777777777777",
        ),
        "blocks": [
            {
                "kind": "jisage_block",
                "children": [
                    {
                        "kind": "heading",
                        "level": 2,
                        "style": "normal",
                        "content": [
                            {"kind": "font_size", "size_type": "large", "level": 1, "content": [{"kind": "text", "value": "F"}]},
                            {"kind": "gaiji", "description": "gaiji", "resolved": "G", "unresolved_reason": null},
                            {"kind": "ruby", "base": "R", "reading": "read"},
                            {"kind": "style", "style_type": "bold", "content": [{"kind": "text", "value": "S"}]},
                            {"kind": "raw", "source": "[raw]"}
                        ]
                    }
                ]
            }
        ]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    let heading = output
        .parser_ir
        .pointer("/nodes/1")
        .expect("heading node should follow indentation node");
    assert_eq!(heading["type"], "heading");
    assert_eq!(heading["text"], "FGRS");
    assert_eq!(
        heading.pointer("/inline_children/0/type"),
        Some(&json!("layout-span"))
    );
    assert_eq!(
        heading.pointer("/inline_children/0/layout/kind"),
        Some(&json!("font-size"))
    );
    assert_eq!(
        heading.pointer("/inline_children/1/type"),
        Some(&json!("gaiji"))
    );
    assert_eq!(
        heading.pointer("/inline_children/2/type"),
        Some(&json!("ruby"))
    );
    assert_eq!(
        heading.pointer("/inline_children/3/type"),
        Some(&json!("emphasis"))
    );
    assert!(
        !has_divergence_record(
            &output,
            "LOSS",
            "blocks[].children[].heading.content[].font_size",
            None,
        ),
        "font_size inside heading should be represented after schema delta"
    );
    assert!(
        !has_divergence_record(
            &output,
            "LOSS",
            "blocks[].children[].heading.content[].ruby",
            None,
        ),
        "ruby inside heading should be represented after schema delta"
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn converts_inline_layout_scopes_to_layout_span() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "layout-span-conversion",
        "meta": base_meta(
            "utf-8",
            "sha256:dadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadada",
        ),
        "blocks": [{
            "kind": "paragraph",
            "content": [
                {"kind": "font_size", "size_type": "large", "level": 1, "content": [{"kind": "text", "value": "大"}]},
                {"kind": "tcy", "content": [{"kind": "text", "value": "12"}]},
                {"kind": "keigakomi", "content": [{"kind": "text", "value": "囲"}]},
                {"kind": "yokogumi", "content": [{"kind": "text", "value": "横"}]}
            ]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    let kinds: Vec<_> = output.parser_ir["nodes"]
        .as_array()
        .expect("nodes")
        .iter()
        .map(|node| node.pointer("/layout/kind").and_then(Value::as_str))
        .collect();
    assert_eq!(
        kinds,
        vec![
            Some("font-size"),
            Some("tcy"),
            Some("keigakomi"),
            Some("yokogumi")
        ]
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/0/layout/size_type"),
        Some(&json!("large"))
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/0/layout/level"),
        Some(&json!(1))
    );
    assert!(
        output.parser_ir["nodes"]
            .as_array()
            .expect("nodes")
            .iter()
            .all(|node| node["type"] == "layout-span")
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn recovers_caption_and_quote_block_children_without_fatal_divergence() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "block-recovery",
        "meta": base_meta(
            "utf-8",
            "sha256:3434343434343434343434343434343434343434343434343434343434343434",
        ),
        "blocks": [
            {
                "kind": "caption_block",
                "children": [{
                    "kind": "paragraph",
                    "content": [{"kind": "text", "value": "Caption text"}]
                }]
            },
            {
                "kind": "quote_block",
                "children": [{
                    "kind": "paragraph",
                    "content": [{"kind": "text", "value": "Quote text"}]
                }]
            }
        ]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    let text_nodes: Vec<&str> = output
        .parser_ir
        .pointer("/nodes")
        .and_then(Value::as_array)
        .unwrap()
        .iter()
        .filter_map(|node| node["text"].as_str())
        .collect();
    assert_eq!(text_nodes, vec!["Caption text", "Quote text"]);
    assert!(has_divergence_record(
        &output,
        "STRUCTURAL",
        "blocks[].caption_block",
        None
    ));
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn unmeasured_inline_kind_refuses_by_default() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "unknown",
        "meta": base_meta(
            "utf-8",
            "sha256:3333333333333333333333333333333333333333333333333333333333333333",
        ),
        "blocks": [{"kind": "paragraph", "content": [{"kind": "x-local-fixture", "value": "x"}]}]
    });

    let error = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: ConversionOptions {
            validate_input_aat: false,
            ..default_test_options()
        },
    })
    .unwrap_err()
    .to_string();

    assert!(error.contains("unsupported inline kind"));
}

#[test]
fn converts_checked_in_real_measured_aat_fixtures() {
    let (schemas, _) = schemas_and_mapping();
    let repo = repo_root();
    let mapping_path = repo.join("data/aat-to-parser-ir-mapping-v1.json");

    for fixture in [
        "tests/fixtures/aat-parser-ir/real-aozora-rs-sample.aat.json",
        "tests/fixtures/aat-parser-ir/real-aozora2html-sample.aat.json",
    ] {
        let aat = ab_aat_to_parser_ir::schema::read_json(&repo.join(fixture)).unwrap();
        let output = ab_aat_to_parser_ir::convert(ConversionRequest {
            aat,
            mapping: MappingDocument::from_path(&mapping_path).unwrap(),
            schemas: schemas.clone(),
            options: default_test_options(),
        })
        .unwrap_or_else(|error| panic!("{fixture} failed conversion: {error:#}"));

        let node_count = output
            .parser_ir
            .pointer("/nodes")
            .and_then(Value::as_array)
            .map_or(0, Vec::len);
        assert!(node_count > 0, "{fixture} produced no parser-IR nodes");
        assert!(
            !output.emitted_rule_ids.is_empty(),
            "{fixture} did not exercise any measured divergence rule"
        );
        validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
        validate_value(&schemas.bundle_schema, &output.divergence_bundle, "bundle").unwrap();
    }
}

#[test]
fn cli_convert_writes_parser_ir_and_divergence_bundle() {
    let repo = repo_root();
    let abc = abc_root(&repo);
    let temp = tempfile::tempdir().unwrap();
    let aat = temp.path().join("input.aat.json");
    let parser_ir = temp.path().join("parser-ir.json");
    let divergence = temp.path().join("divergence.json");
    std::fs::write(
        &aat,
        r#"{
  "version": 1,
  "work_id": "cli",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:6666666666666666666666666666666666666666666666666666666666666666",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "A"}]}]
}"#,
    )
    .unwrap();

    let status = std::process::Command::new(env!("CARGO_BIN_EXE_ab-aat-to-parser-ir"))
        .arg("convert")
        .arg("--aat")
        .arg(&aat)
        .arg("--mapping")
        .arg(repo.join("data/aat-to-parser-ir-mapping-v1.json"))
        .arg("--work-content-hash")
        .arg("sha256:7777777777777777777777777777777777777777777777777777777777777777")
        .arg("--parser-ir-out")
        .arg(&parser_ir)
        .arg("--divergence-out")
        .arg(&divergence)
        .arg("--abc-root")
        .arg(abc)
        .status()
        .unwrap();

    assert!(status.success());
    assert!(parser_ir.exists());
    assert!(divergence.exists());
    let output = read_json(&parser_ir).unwrap();
    assert_eq!(
        output["source"]["work_content_hash"],
        "sha256:7777777777777777777777777777777777777777777777777777777777777777"
    );
    assert_eq!(
        output["source"]["primary_text_hash"],
        "sha256:6666666666666666666666666666666666666666666666666666666666666666"
    );
}

#[test]
fn cli_convert_rejects_mapping_generation_version_mismatch() {
    let repo = repo_root();
    let abc = abc_root(&repo);
    let temp = tempfile::tempdir().unwrap();
    let aat = temp.path().join("input.aat.json");
    let parser_ir = temp.path().join("parser-ir.json");
    let divergence = temp.path().join("divergence.json");
    std::fs::write(
        &aat,
        r#"{
  "version": 1,
  "work_id": "cli",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:6666666666666666666666666666666666666666666666666666666666666666",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "A"}]}]
}"#,
    )
    .unwrap();

    let output = std::process::Command::new(env!("CARGO_BIN_EXE_ab-aat-to-parser-ir"))
        .arg("convert")
        .arg("--aat")
        .arg(&aat)
        .arg("--mapping")
        .arg(repo.join("data/aat-to-parser-ir-mapping-v1.json"))
        .arg("--parser-ir-out")
        .arg(&parser_ir)
        .arg("--divergence-out")
        .arg(&divergence)
        .arg("--abc-root")
        .arg(&abc)
        .arg("--expect-mapping-version")
        .arg("0.9.9")
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert!(!parser_ir.exists());
    assert!(!divergence.exists());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("mapping generation mismatch"),
        "stderr was: {stderr}"
    );
}

#[test]
fn cli_convert_accepts_matching_mapping_generation_version_and_hash() {
    let repo = repo_root();
    let abc = abc_root(&repo);
    let temp = tempfile::tempdir().unwrap();
    let aat = temp.path().join("input.aat.json");
    let parser_ir = temp.path().join("parser-ir.json");
    let divergence = temp.path().join("divergence.json");
    std::fs::write(
        &aat,
        r#"{
  "version": 1,
  "work_id": "cli",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:6666666666666666666666666666666666666666666666666666666666666666",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "A"}]}]
}"#,
    )
    .unwrap();

    let mapping_path = repo.join("data/aat-to-parser-ir-mapping-v1.json");
    let mapping = MappingDocument::from_path(&mapping_path).unwrap();

    let status = std::process::Command::new(env!("CARGO_BIN_EXE_ab-aat-to-parser-ir"))
        .arg("convert")
        .arg("--aat")
        .arg(&aat)
        .arg("--mapping")
        .arg(&mapping_path)
        .arg("--parser-ir-out")
        .arg(&parser_ir)
        .arg("--divergence-out")
        .arg(&divergence)
        .arg("--abc-root")
        .arg(&abc)
        .arg("--expect-mapping-version")
        .arg(&mapping.mapping_version)
        .arg("--expect-mapping-hash")
        .arg(&mapping.document_hash)
        .status()
        .unwrap();

    assert!(status.success());
    assert!(parser_ir.exists());
    assert!(divergence.exists());
}

#[test]
fn cli_convert_with_ortho_annotations_emits_sentence_tags() {
    let repo = repo_root();
    let abc = abc_root(&repo);
    let temp = tempfile::tempdir().unwrap();
    let aat = temp.path().join("input.aat.json");
    let ortho = temp.path().join("ortho.json");
    let parser_ir = temp.path().join("parser-ir.json");
    let divergence = temp.path().join("divergence.json");
    std::fs::write(
        &aat,
        serde_json::to_string_pretty(&ortho_fixture_aat()).unwrap(),
    )
    .unwrap();
    std::fs::write(
        &ortho,
        serde_json::to_string_pretty(&ortho_fixture_bundle()).unwrap(),
    )
    .unwrap();

    let status = std::process::Command::new(env!("CARGO_BIN_EXE_ab-aat-to-parser-ir"))
        .arg("convert")
        .arg("--aat")
        .arg(&aat)
        .arg("--ortho-annotations")
        .arg(&ortho)
        .arg("--mapping")
        .arg(repo.join("data/aat-to-parser-ir-mapping-v1.json"))
        .arg("--parser-ir-out")
        .arg(&parser_ir)
        .arg("--divergence-out")
        .arg(&divergence)
        .arg("--abc-root")
        .arg(abc)
        .status()
        .unwrap();

    assert!(status.success());
    let parser_ir_json = read_json(&parser_ir).unwrap();
    assert_eq!(
        parser_ir_json.pointer("/sentences/0/tags/0"),
        Some(&json!("orthographic-katakana"))
    );
    assert!(parser_ir_json.get("orthographic_annotations").is_some());
    assert!(divergence.exists());
}

#[test]
fn cli_detect_ortho_annotations_reports_missing_dictionary() {
    let repo = repo_root();
    let abc = abc_root(&repo);
    let temp = tempfile::tempdir().unwrap();
    let aat = temp.path().join("input.aat.json");
    let ortho = temp.path().join("ortho.json");
    let missing_dict = temp.path().join("missing-vibrato.dic");
    std::fs::write(
        &aat,
        serde_json::to_string_pretty(&ortho_fixture_aat()).unwrap(),
    )
    .unwrap();

    let output = std::process::Command::new(env!("CARGO_BIN_EXE_ab-aat-to-parser-ir"))
        .arg("detect-ortho-annotations")
        .arg("--aat")
        .arg(&aat)
        .arg("--mapping")
        .arg(repo.join("data/aat-to-parser-ir-mapping-v1.json"))
        .arg("--ortho-annotations-out")
        .arg(&ortho)
        .arg("--abc-root")
        .arg(abc)
        .env("AB_VIBRATO_DICT", &missing_dict)
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert!(!ortho.exists());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("detect-ortho-annotations requires AB_VIBRATO_DICT"));
    assert!(stderr.contains("No such file or directory"));
}

#[test]
fn cli_audit_corpus_reports_successes_and_failures() {
    let repo = repo_root();
    let abc = abc_root(&repo);
    let temp = tempfile::tempdir().unwrap();
    let aat_dir = temp.path().join("aat");
    std::fs::create_dir_all(&aat_dir).unwrap();
    let summary = temp.path().join("summary.json");
    let report = temp.path().join("report.md");

    std::fs::write(
        aat_dir.join("pass.aat.json"),
        r#"{
  "version": 1,
  "work_id": "audit-pass",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:9999999999999999999999999999999999999999999999999999999999999999",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "A"}]}]
}"#,
    )
    .unwrap();
    std::fs::write(
        aat_dir.join("fail.aat.json"),
        r#"{
  "version": 1,
  "work_id": "audit-fail",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [{"kind": "paragraph", "content": [{"kind": "x-local-fixture", "value": "Q"}]}]
}"#,
    )
    .unwrap();

    let status = std::process::Command::new(env!("CARGO_BIN_EXE_ab-aat-to-parser-ir"))
        .arg("audit-corpus")
        .arg("--aat-dir")
        .arg(&aat_dir)
        .arg("--mapping")
        .arg(repo.join("data/aat-to-parser-ir-mapping-v1.json"))
        .arg("--summary-json")
        .arg(&summary)
        .arg("--report-md")
        .arg(&report)
        .arg("--jobs")
        .arg("2")
        .arg("--abc-root")
        .arg(abc)
        .status()
        .unwrap();

    assert!(status.success());
    let summary: Value = ab_aat_to_parser_ir::schema::read_json(&summary).unwrap();
    assert_eq!(summary.pointer("/totals/files_attempted"), Some(&json!(2)));
    assert_eq!(summary.pointer("/totals/files_succeeded"), Some(&json!(1)));
    assert_eq!(summary.pointer("/totals/files_failed"), Some(&json!(1)));
    assert_eq!(
        summary.pointer("/compatibility_candidates/0/evidence_scope/files_scanned"),
        Some(&json!(2))
    );
    assert_eq!(
        summary.pointer("/compatibility_candidates/0/evidence_scope/files_succeeded"),
        Some(&json!(1))
    );
    assert_eq!(
        summary.pointer("/compatibility_candidates/0/evidence_scope/files_failed"),
        Some(&json!(1))
    );
    assert_eq!(
        summary
            .pointer("/top_errors/0/count")
            .and_then(Value::as_u64),
        Some(1)
    );
    assert!(report.is_file());
    let report_text = std::fs::read_to_string(&report).unwrap();
    assert_eq!(
        report_text.lines().next(),
        Some("# AAT Parser-IR Conversion Audit")
    );
}

#[test]
fn cli_audit_corpus_reports_raw_node_provenance_inventory() {
    let repo = repo_root();
    let abc = abc_root(&repo);
    let temp = tempfile::tempdir().unwrap();
    let aat_dir = temp.path().join("aat");
    std::fs::create_dir_all(&aat_dir).unwrap();
    let summary = temp.path().join("summary.json");
    let report = temp.path().join("report.md");

    std::fs::write(
        aat_dir.join("direct-raw.aat.json"),
        r#"{
  "version": 1,
  "work_id": "audit-direct-raw",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [{"kind": "paragraph", "content": [{"kind": "raw", "source": "改頁"}]}]
}"#,
    )
    .unwrap();
    std::fs::write(
        aat_dir.join("projected-raw.aat.json"),
        r#"{
  "version": 1,
  "work_id": "audit-projected-raw",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:2222222222222222222222222222222222222222222222222222222222222222",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        {"kind": "style", "style_type": "bold", "content": [{"kind": "raw", "source": "<br/>"}]}
      ]
    }
  ]
}"#,
    )
    .unwrap();

    let status = std::process::Command::new(env!("CARGO_BIN_EXE_ab-aat-to-parser-ir"))
        .arg("audit-corpus")
        .arg("--aat-dir")
        .arg(&aat_dir)
        .arg("--mapping")
        .arg(repo.join("data/aat-to-parser-ir-mapping-v1.json"))
        .arg("--summary-json")
        .arg(&summary)
        .arg("--report-md")
        .arg(&report)
        .arg("--jobs")
        .arg("2")
        .arg("--abc-root")
        .arg(abc)
        .status()
        .unwrap();

    assert!(status.success());
    let summary: Value = ab_aat_to_parser_ir::schema::read_json(&summary).unwrap();
    assert_eq!(summary.pointer("/totals/files_attempted"), Some(&json!(2)));
    assert_eq!(summary.pointer("/totals/files_succeeded"), Some(&json!(2)));
    assert_eq!(summary.pointer("/totals/files_failed"), Some(&json!(0)));
    assert_eq!(summary.pointer("/raw_nodes/nodes_total"), Some(&json!(2)));
    assert_eq!(
        summary.pointer("/raw_nodes/files_with_raw"),
        Some(&json!(2))
    );
    assert_eq!(
        summary.pointer("/raw_nodes/fatal_direct_failures"),
        Some(&json!(0))
    );
    assert_eq!(
        summary.pointer("/raw_nodes/inferred_provenance/source-derived"),
        Some(&json!(1))
    );
    assert_eq!(
        summary.pointer("/raw_nodes/inferred_provenance/parser-derived"),
        Some(&json!(1))
    );
    assert_eq!(
        summary.pointer("/raw_nodes/source_classes/aozora-command"),
        Some(&json!(1))
    );
    assert_eq!(
        summary.pointer("/raw_nodes/source_classes/html-fragment"),
        Some(&json!(1))
    );
    assert_eq!(
        summary.pointer("/raw_nodes/by_corpus/aat/nodes_total"),
        Some(&json!(2))
    );
    assert_eq!(
        summary
            .pointer("/raw_nodes/samples/0/source_preview")
            .and_then(Value::as_str),
        Some("改頁")
    );
    let report_text = std::fs::read_to_string(&report).unwrap();
    assert!(report_text.contains("## Raw Nodes"));
}

#[test]
fn cli_default_roots_follow_mapping_path_not_current_directory() {
    let repo = repo_root();
    let temp = tempfile::tempdir().unwrap();
    let fake_cwd = temp.path().join("fake-repo");
    let fake_data = fake_cwd.join("data");
    std::fs::create_dir_all(&fake_data).unwrap();
    std::fs::write(fake_data.join("aat-schema.json"), "{}").unwrap();

    let aat = temp.path().join("input.aat.json");
    let parser_ir = temp.path().join("parser-ir.json");
    let divergence = temp.path().join("divergence.json");
    std::fs::write(
        &aat,
        r#"{
  "version": 1,
  "work_id": "cli-root",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:8888888888888888888888888888888888888888888888888888888888888888",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "A"}]}]
}"#,
    )
    .unwrap();

    let status = std::process::Command::new(env!("CARGO_BIN_EXE_ab-aat-to-parser-ir"))
        .current_dir(&fake_cwd)
        .arg("convert")
        .arg("--aat")
        .arg(&aat)
        .arg("--mapping")
        .arg(repo.join("data/aat-to-parser-ir-mapping-v1.json"))
        .arg("--parser-ir-out")
        .arg(&parser_ir)
        .arg("--divergence-out")
        .arg(&divergence)
        .status()
        .unwrap();

    assert!(status.success());
    assert!(parser_ir.is_file());
    assert!(divergence.is_file());
}

#[test]
fn cli_tei_eaj_structural_expansion_writes_reports() {
    let repo = repo_root();
    let abc = abc_root(&repo);
    let temp = tempfile::tempdir().unwrap();
    let aat_dir = temp.path().join("aat");
    std::fs::create_dir_all(&aat_dir).unwrap();
    let workset = temp.path().join("workset.json");
    let summary = temp.path().join("tei-summary.json");
    let report = temp.path().join("tei-report.md");

    std::fs::write(
        aat_dir.join("000035_1567-fixture.json"),
        r#"{
  "version": 1,
  "work_id": "000035_1567",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:5656565656565656565656565656565656565656565656565656565656565656",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [
    {"kind": "paragraph", "content": [{"kind": "text", "value": "メロスは激怒した。"}]},
    {"kind": "paragraph", "content": [{"kind": "text", "value": "（古伝説と、シルレルの詩から。）"}]}
  ]
}"#,
    )
    .unwrap();
    std::fs::write(
        &workset,
        r#"{
  "schema_version": "tei-eaj-aozora-workset-export-v1",
  "summary": {
    "tei_eaj_file_count": 1,
    "tei_eaj_work_id_count": 1,
    "abc_counterpart_count": 1,
    "compared_file_count": 1,
    "missing_counterpart_count": 0,
    "no_work_id_count": 0,
    "base_text_equal_count": 0,
    "base_text_mismatch_count": 1,
    "uncompared_file_count": 0
  },
  "tei_eaj_source": {"revision": "fixture", "root": "/fixture"},
  "abc_inputs": {"counterparts": [], "tei_dirs": [], "tei_specs": []},
  "candidate_work_ids": ["1567"],
  "missing_abc_counterpart_work_ids": [],
  "no_work_id_files": [],
  "files": [{
    "abc_body_base_text_length": 9806,
    "abc_note_count": 1,
    "abc_p_count": 1,
    "abc_tei": "tests/fixtures/tei-eaj-alignment-probe/abc-melos-single-p.xml",
    "base_text_equal": false,
    "comparison_status": "compared",
    "first_difference": null,
    "level": "Level 4",
    "state": "complete",
    "tei_eaj_body_base_text_length": 9790,
    "tei_eaj_file": "data/complete/tei_lib_lv4/1567_tei.xml",
    "tei_eaj_note_count": 0,
    "tei_eaj_p_count": 19,
    "title": "走れメロス",
    "work_id": "1567"
  }]
}"#,
    )
    .unwrap();

    let status = std::process::Command::new(env!("CARGO_BIN_EXE_ab-aat-to-parser-ir"))
        .arg("tei-eaj-structural-expansion")
        .arg("--workset")
        .arg(&workset)
        .arg("--aat-dir")
        .arg(format!("fixture={}", aat_dir.display()))
        .arg("--mapping")
        .arg(repo.join("data/aat-to-parser-ir-mapping-v1.json"))
        .arg("--summary-json")
        .arg(&summary)
        .arg("--report-md")
        .arg(&report)
        .arg("--abc-root")
        .arg(abc)
        .status()
        .unwrap();

    assert!(status.success());
    let summary: Value = ab_aat_to_parser_ir::schema::read_json(&summary).unwrap();
    assert_eq!(summary.pointer("/totals/tei_eaj_files"), Some(&json!(1)));
    assert_eq!(
        summary.pointer("/totals/parser_ir_gap_rows"),
        Some(&json!(0))
    );
    assert_eq!(
        summary.pointer("/totals/source_attribution_gap_rows"),
        Some(&json!(0))
    );
    assert_eq!(
        summary.pointer("/rows/0/tei/tei_eaj_p_count"),
        Some(&json!(19))
    );
    assert!(report.is_file());
    let report_text = std::fs::read_to_string(&report).unwrap();
    assert!(report_text.contains("TEI-EAJ Structural Expansion"));
}

#[test]
fn quote_node_emission_from_text() {
    let (schemas, mapping) = schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: include_fixture_json("quote-node-emission.aat.json"),
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();
    let nodes = output.parser_ir["nodes"].as_array().unwrap();
    let quote_nodes: Vec<_> = nodes.iter().filter(|n| n["type"] == "quote").collect();
    assert_eq!(quote_nodes.len(), 2, "expected open+close quote nodes");
    assert_eq!(quote_nodes[0]["marker_type"], "open");
    assert_eq!(quote_nodes[0]["text"], "「");
    assert!(quote_nodes[0]["nesting_level"].is_null());
    assert_eq!(quote_nodes[1]["marker_type"], "close");
    assert_eq!(quote_nodes[1]["text"], "」");
    // Sub-segments carry synthetic spans (decoded_utf8 coordinate system).
    assert_eq!(quote_nodes[0]["span"]["coordinate_system"], "decoded_utf8");
    // The text nodes around the markers are split out, not merged.
    let text_nodes: Vec<_> = nodes
        .iter()
        .filter(|n| n["type"] == "text")
        .map(|n| n["text"].as_str().unwrap())
        .collect();
    assert_eq!(
        text_nodes,
        vec!["先生は", "綺麗だ", "といった。"],
        "text node should be split at 「」 markers"
    );
}

#[test]
fn fragment_assembly_single_inner_sentence() {
    let (schemas, mapping) = schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: include_fixture_json("nested-sentence-basic.aat.json"),
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();
    let sentences = output.parser_ir["sentences"].as_array().unwrap();
    // Outer-I, one inner sentence, outer-F.
    assert_eq!(sentences.len(), 3);
    assert_eq!(sentences[0]["part"], "I");
    assert!(
        sentences[1].get("part").is_none(),
        "inner sentence has no part"
    );
    assert_eq!(sentences[2]["part"], "F");
    // I <-> F linking (skips the inner sentence).
    assert_eq!(sentences[0]["next_id"], sentences[2]["id"]);
    assert_eq!(sentences[2]["prev_id"], sentences[0]["id"]);
    assert_eq!(sentences[0]["fragment_group"], "fg000000");
    assert_eq!(sentences[2]["fragment_group"], "fg000000");
    // Framing-punctuation redistribution: the 「 belongs to the inner sentence,
    // not the outer-I fragment.
    let nodes = output.parser_ir["nodes"].as_array().unwrap();
    let i_text = visible_text_for_sentence(nodes, &sentences[0]);
    let inner_text = visible_text_for_sentence(nodes, &sentences[1]);
    assert_eq!(i_text, "先生は梢を見上げて、");
    assert!(
        inner_text.starts_with('「'),
        "inner sentence keeps the open marker: {inner_text}"
    );
    assert!(
        inner_text.ends_with('」'),
        "inner sentence keeps the close marker: {inner_text}"
    );
}

#[test]
fn fragment_assembly_multiple_inner_sentences() {
    let (schemas, mapping) = schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: include_fixture_json("nested-sentence-multiple.aat.json"),
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();
    let sentences = output.parser_ir["sentences"].as_array().unwrap();
    // Outer-I, two inner sentences, outer-F.
    assert_eq!(sentences.len(), 4);
    assert_eq!(sentences[0]["part"], "I");
    assert!(
        sentences[1].get("part").is_none(),
        "inner sentence 1 has no part"
    );
    assert!(
        sentences[2].get("part").is_none(),
        "inner sentence 2 has no part"
    );
    assert_eq!(sentences[3]["part"], "F");
    assert_eq!(sentences[0]["next_id"], sentences[3]["id"]);
    assert_eq!(sentences[3]["prev_id"], sentences[0]["id"]);
    // Spans tile the paragraph with no gaps.
    for w in sentences.windows(2) {
        let prev_end = w[0]["span"]["end"].as_u64().unwrap();
        let next_start = w[1]["span"]["start"].as_u64().unwrap();
        assert_eq!(prev_end, next_start, "sentence spans must be contiguous");
    }
}

#[test]
fn fragment_field_coherence_holds() {
    let (schemas, mapping) = schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: include_fixture_json("nested-sentence-multiple.aat.json"),
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();
    let sentences = output.parser_ir["sentences"].as_array().unwrap();
    for s in sentences {
        let part = s.get("part").and_then(|v| v.as_str());
        let next = s.get("next_id");
        let prev = s.get("prev_id");
        let group = s.get("fragment_group");
        let present = |v: &serde_json::Value| !(v.is_null());
        match part {
            Some("I") => {
                assert!(next.is_some() && present(next.unwrap()));
                assert!(prev.is_none() || !present(prev.unwrap()));
                assert!(group.is_some() && present(group.unwrap()));
            }
            Some("F") => {
                assert!(prev.is_some() && present(prev.unwrap()));
                assert!(next.is_none() || !present(next.unwrap()));
                assert!(group.is_some() && present(group.unwrap()));
            }
            None => {
                assert!(group.is_none() || !present(group.unwrap()));
                assert!(next.is_none() || !present(next.unwrap()));
                assert!(prev.is_none() || !present(prev.unwrap()));
            }
            other => panic!("unexpected part: {other:?}"),
        }
    }
}

#[test]
fn ruby_node_span_is_decoded_not_raw_source() {
    // Regression for the converter coordinate-system bug. The AAT emits a RAW
    // source span for a ruby node — byte_start=9, byte_end=24 covering the source
    // markup `下《した》` (15 bytes) — but the parser-IR coordinate system is
    // decoded_utf8. The converter must project the ruby node's span to the DECODED
    // base (`下`, 3 bytes at decoded offset 9), i.e. [9, 12], so it stays consistent
    // with sibling text spans and the visible-text sentence splitter. Copying the
    // raw AAT offsets here (the old `map_span` behaviour) desynchronised node spans
    // and made ruby-heavy corpora fail — or crash — sentence projection. See
    // `map_node_span`.
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "ruby-decoded-span",
        "blocks": [{
            "kind": "paragraph",
            "content": [
                {"kind": "text", "value": "先生は"},
                {
                    "kind": "ruby",
                    "base": "下",
                    "reading": "した",
                    "direction": "right",
                    "span": {"byte_start": 9, "byte_end": 24, "line_start": 1, "line_end": 1}
                },
                {"kind": "text", "value": "です。"}
            ]
        }],
        "meta": {
            "adapter": "fixture",
            "adapter_version": "0.0.0",
            "source_encoding": "utf-8",
            "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
            "parse_complete": true,
            "warnings": []
        }
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();

    let nodes = output.parser_ir["nodes"].as_array().unwrap();
    let ruby = nodes
        .iter()
        .find(|n| n["type"] == "ruby")
        .expect("parser-IR should contain a ruby node");
    assert_eq!(ruby["ruby"]["base"], "下");
    assert_eq!(
        ruby["span"]["start"], 9,
        "decoded start is after 先生は (9 bytes)"
    );
    assert_eq!(
        ruby["span"]["end"], 12,
        "decoded end covers the base 下 (3 bytes), NOT the raw source markup (would be 24)"
    );
    assert_eq!(ruby["span"]["coordinate_system"], "decoded_utf8");

    // The whole (ruby-bearing) document must convert and remain schema-valid — the
    // sentence projection over the now-consistent decoded spans succeeds.
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}
