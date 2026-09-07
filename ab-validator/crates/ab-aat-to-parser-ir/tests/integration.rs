use std::path::{Path, PathBuf};
use std::sync::Mutex;

use ab_aat_to_parser_ir::{
    ConversionOptions, ConversionRequest, MappingDocument, QualificationConversion, SchemaSet,
    divergence::{AatMeta, DivergenceRecorder},
    mapping::MappingRule,
    qualification::{QualificationRequest, QualificationStatus, qualify_work},
    schema::{read_json, schema_hash, validate_value},
};
use serde_json::{Value, json};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn research_root(repo: &Path) -> PathBuf {
    std::env::var_os("AB_RESEARCH_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|| repo.join("research"))
}

fn roots() -> (PathBuf, PathBuf) {
    let repo = repo_root();
    let abc = research_root(&repo);
    (repo, abc)
}

fn default_test_options() -> ConversionOptions {
    ConversionOptions::default()
}

fn schemas_and_mapping() -> (SchemaSet, MappingDocument) {
    let (repo_root, research_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v1.json"))
            .unwrap();
    let schemas =
        SchemaSet::load_for_aat_version(&repo_root, &research_root, mapping.source_aat_version)
            .unwrap();
    (schemas, mapping)
}

#[test]
fn schema_set_rejects_unknown_aat_version() {
    let (repo_root, research_root) = roots();
    let err = SchemaSet::load_for_aat_version(&repo_root, &research_root, 3).unwrap_err();
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

fn converter_with_rejecting_parser_ir_schema() -> ab_aat_to_parser_ir::PreparedConverter {
    let (mut schemas, mut mapping) = schemas_and_mapping();
    schemas.parser_ir_schema = json!({
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "not": {}
    });
    mapping.target_parser_ir_schema_hash = schema_hash(&schemas.parser_ir_schema).unwrap();
    ab_aat_to_parser_ir::PreparedConverter::new(mapping, schemas).unwrap()
}

#[test]
fn production_convert_still_hard_aborts_on_invalid_parser_ir() {
    let error = converter_with_rejecting_parser_ir_schema()
        .convert(
            include_fixture_json("nested-sentence-basic.aat.json"),
            default_test_options(),
        )
        .unwrap_err();
    assert!(
        error
            .to_string()
            .starts_with("parser-IR validation failed at"),
        "{error}"
    );
}

#[test]
fn qualification_conversion_retains_invalid_value_without_divergence_bundle() {
    let outcome = converter_with_rejecting_parser_ir_schema()
        .convert_for_qualification(
            include_fixture_json("nested-sentence-basic.aat.json"),
            default_test_options(),
        )
        .unwrap();
    let QualificationConversion::Invalid { parser_ir, errors } = outcome else {
        panic!("expected invalid qualification outcome");
    };
    assert_eq!(
        parser_ir["schema_id"],
        "https://w3id.org/abc/schemas/parser-ir.schema.json"
    );
    assert!(!errors.is_empty());
}

#[test]
fn qualification_and_production_valid_outputs_are_identical() {
    let (schemas, mapping) = schemas_and_mapping();
    let converter = ab_aat_to_parser_ir::PreparedConverter::new(mapping, schemas).unwrap();
    let aat = include_fixture_json("nested-sentence-basic.aat.json");
    let production = converter
        .convert(aat.clone(), default_test_options())
        .unwrap();
    let QualificationConversion::Valid(qualification) = converter
        .convert_for_qualification(aat, default_test_options())
        .unwrap()
    else {
        panic!("expected valid qualification outcome");
    };
    assert_eq!(production.parser_ir, qualification.parser_ir);
    assert_eq!(
        production.divergence_bundle,
        qualification.divergence_bundle
    );
    assert_eq!(production.emitted_rule_ids, qualification.emitted_rule_ids);
}

#[test]
fn qualification_capture_records_valid_parser_ir_bytes() {
    let (schemas, mapping) = schemas_and_mapping();
    let converter = ab_aat_to_parser_ir::PreparedConverter::new(mapping, schemas).unwrap();
    let capture = qualify_work(QualificationRequest {
        converter: &converter,
        aat: include_fixture_json("nested-sentence-basic.aat.json"),
        options: default_test_options(),
        work_id: "fixture".to_owned(),
        qualification_identity_ref:
            "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_owned(),
        policy_hash: "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
            .to_owned(),
    })
    .unwrap();
    assert_eq!(capture.record.status, QualificationStatus::SchemaValid);
    assert!(capture.parser_ir_bytes.is_some());
    assert!(capture.validation_ledger_bytes.is_none());
}

#[test]
fn qualification_capture_records_invalid_value_and_full_ledger() {
    let converter = converter_with_rejecting_parser_ir_schema();
    let capture = qualify_work(QualificationRequest {
        converter: &converter,
        aat: include_fixture_json("nested-sentence-basic.aat.json"),
        options: default_test_options(),
        work_id: "fixture".to_owned(),
        qualification_identity_ref:
            "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_owned(),
        policy_hash: "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
            .to_owned(),
    })
    .unwrap();
    assert_eq!(capture.record.status, QualificationStatus::SchemaInvalid);
    assert!(capture.parser_ir_bytes.is_some());
    assert!(capture.validation_ledger_bytes.is_some());
    assert!(!capture.record.validation_witnesses.unwrap().is_empty());
}

#[test]
fn qualification_capture_turns_conversion_failure_into_no_output() {
    let (schemas, mapping) = schemas_and_mapping();
    let converter = ab_aat_to_parser_ir::PreparedConverter::new(mapping, schemas).unwrap();
    let capture = qualify_work(QualificationRequest {
        converter: &converter,
        aat: Value::Null,
        options: default_test_options(),
        work_id: "fixture".to_owned(),
        qualification_identity_ref:
            "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_owned(),
        policy_hash: "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
            .to_owned(),
    })
    .unwrap();
    assert_eq!(capture.record.status, QualificationStatus::NoOutput);
    assert!(capture.parser_ir_bytes.is_none());
    assert!(capture.record.failure.is_some());
}

#[test]
fn v1_document_under_v2_tuple_fails_input_validation() {
    let (repo_root, research_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v2.json"))
            .unwrap();
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &research_root, 2).unwrap();
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
    let (repo_root, research_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v1.json"))
            .unwrap();
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &research_root, 1).unwrap();
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
    let (repo_root, research_root) = roots();
    // v1 mapping paired with the v2 schema file must fail preflight.
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v1.json"))
            .unwrap();
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &research_root, 2).unwrap();
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

fn visible_text_for_node_range(nodes: &[Value], region: &Value) -> String {
    let start = region["node_range"]["start"].as_u64().unwrap() as usize;
    let end = region["node_range"]["end"].as_u64().unwrap() as usize;
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
        "coordinate_system": "parser_text_utf8",
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
    let abc = research_root(&repo);
    let schemas = SchemaSet::load_for_aat_version(&repo, &abc, 1).unwrap();

    assert_eq!(
        schema_hash(&schemas.mapping_schema).unwrap(),
        "sha256:e6af01115ccdb7c5cad086eee4c458230f6b6f55e0dfee7791730b48994283e2"
    );
    let mapping =
        MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v1.json")).unwrap();
    assert_eq!(
        schema_hash(&schemas.parser_ir_schema).unwrap(),
        mapping.target_parser_ir_schema_hash
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

    mapping.preflight(&schemas).unwrap();

    assert_eq!(mapping.mapping_version, "0.14.0");
    assert_eq!(
        mapping.target_parser_ir_schema_hash,
        schema_hash(&schemas.parser_ir_schema).unwrap()
    );
    assert!(mapping.transform_rule_descriptions.iter().all(|rule| {
        !rule.aat_pointer.as_deref().is_some_and(|pointer| {
            rule.category == "LOSS"
                && (pointer.ends_with("heading.indent") || pointer.ends_with("ruby.base_content"))
        })
    }));
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
}

#[test]
fn mapping_preflight_accepts_checked_in_v2_artifact() {
    let (repo_root, research_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v2.json"))
            .unwrap();
    assert_eq!(mapping.mapping_version, "0.15.0");
    assert_eq!(mapping.source_aat_version, 2);
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &research_root, 2).unwrap();
    mapping.preflight(&schemas).unwrap();
}

#[test]
fn frozen_v2_0_3_0_artifact_retains_historical_coordinates() {
    let (repo_root, research_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v2-0.3.0.json"))
            .unwrap();
    assert_eq!(mapping.mapping_version, "0.3.0");
    assert_eq!(
        mapping.document_hash,
        "sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40"
    );
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &research_root, 2).unwrap();
    assert_ne!(
        mapping.target_parser_ir_schema_hash,
        schema_hash(&schemas.parser_ir_schema).unwrap(),
        "the frozen generation must retain its historical parser-IR coordinate"
    );
}

#[test]
fn frozen_v2_0_4_0_artifact_retains_phase5_coordinates() {
    let (repo_root, research_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v2-0.4.0.json"))
            .unwrap();
    assert_eq!(mapping.mapping_version, "0.4.0");
    assert_eq!(
        mapping.document_hash,
        "sha256:cf177bee98af086fe728cbc1942e4f631b26ed5bb55aedc7d91b21f467c41f30"
    );
    assert_eq!(mapping.source_aat_version, 2);
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &research_root, 2).unwrap();
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
            "coordinate_system": "parser_text_utf8",
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
        ab_aat_to_parser_ir::ortho_annotations::OrthoCoordinateSystem::ParserTextUtf8
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
fn checked_in_schema_preserves_ortho_annotations_without_sentence_analysis() {
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

    assert!(output.parser_ir.get("sentence_segmentation").is_none());
    assert!(output.parser_ir.get("sentences").is_none());
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
fn preserves_emphasis_with_internal_ruby_sentence_boundary() {
    let (schemas, mapping) = schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: include_fixture_json("atomic-boundary-emphasis-input.aat.json"),
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();
    assert_eq!(output.parser_ir["nodes"].as_array().unwrap().len(), 1);
    assert!(output.parser_ir.get("sentences").is_none());
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn preserves_emphasis_across_internal_sentence_boundaries() {
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

    assert_eq!(output.parser_ir["nodes"].as_array().unwrap().len(), 1);
    assert_eq!(output.parser_ir["nodes"][0]["type"], "emphasis");
    assert_eq!(output.parser_ir["nodes"][0]["text"], "甲。乙丙");
    assert_eq!(
        output.parser_ir["nodes"][0]["inline_children"][1]["ruby"]["base"],
        "丙"
    );
    assert!(output.parser_ir.get("sentences").is_none());
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
                "span": {"start": 0, "end": 0, "coordinate_system": "parser_text_utf8"},
                "marker": "page",
                "page_number": null,
            }),
            json!({
                "type": "editor-note",
                "span": {"start": 0, "end": 0, "coordinate_system": "parser_text_utf8"},
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
            "span": {"start": 0, "end": 2, "coordinate_system": "parser_text_utf8"},
            "text": "é",
            "style": "1-09-63",
        }))
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/1"),
        Some(&json!({
            "type": "layout-span",
            "span": {"start": 2, "end": 5, "coordinate_system": "parser_text_utf8"},
            "text": "ABC",
            "inline_children": [{
                "type": "text",
                "span": {"start": 2, "end": 5, "coordinate_system": "parser_text_utf8"},
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
    let (repo_root, research_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v2.json"))
            .unwrap();
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &research_root, 2).unwrap();
    (schemas, mapping)
}

/// A schema-valid v2 `meta` block for tests: `warnings: []` keeps the fixture
/// minimal; diagnostic preservation is exercised separately.
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
fn emitted_colophon_note_converts_to_transcriber_note() {
    let (schemas, mapping) = v2_schemas_and_mapping();
    let source =
        "本文。\n\n底本：「作品集」\n※「□」には、底本では「◆」が内接しています。\n入力：入力者\n";
    let aat =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();
    let notes: Vec<_> = output.parser_ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|node| node["type"] == "source-note")
        .collect();
    assert_eq!(notes.len(), 2);
    assert_eq!(notes[0]["note_type"], "source-attribution");
    assert_eq!(notes[1]["note_type"], "transcriber-note");
    assert_eq!(notes[1]["placement"], "back");
    assert_eq!(notes[1]["classification"], "direct");
    assert_eq!(
        notes[1]["text"],
        "※「□」には、底本では「◆」が内接しています。\n入力：入力者\n"
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn preserves_source_diagnostics_and_parse_completion() {
    let (schemas, mapping) = v2_schemas_and_mapping();
    let converter = ab_aat_to_parser_ir::PreparedConverter::new(mapping, schemas).unwrap();
    for complete in [true, false] {
        let mut meta = v2_test_meta();
        meta["parse_complete"] = json!(complete);
        meta["warnings"] = json!([
            {"code": "unclosed-bracket", "severity": "error", "message": "unclosed bracket",
             "span": {"line_start": 3, "line_end": 4, "byte_start": 10, "byte_end": 20}},
            {"code": "source-note", "severity": "note", "message": "source note"},
            {"code": "future.warning", "severity": "warning", "message": "retained warning"}
        ]);
        let output = converter.convert(json!({
            "version": 2, "work_id": "diagnostics", "meta": meta,
            "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}]
        }), default_test_options()).unwrap();
        assert_eq!(output.parser_ir["derived_from"]["parse_complete"], complete);
        assert_eq!(
            output.parser_ir["errors"],
            json!([{
                "severity": "error", "code": "unclosed-bracket", "message": "unclosed bracket",
                "span": {"start": 10, "end": 20, "coordinate_system": "decoded_utf8", "line": 3, "end_line": 4},
                "construct": null, "recovery": null
            }])
        );
        assert_eq!(output.parser_ir["warnings"][0]["severity"], "note");
        assert_eq!(output.parser_ir["warnings"][0]["code"], "source-note");
        assert_eq!(output.parser_ir["warnings"][1]["code"], "future.warning");
        let records = output.divergence_bundle["records"].as_array().unwrap();
        assert!(!records.iter().any(|record| {
            record["category"] == "LOSS"
                && record["aat_pointer"].as_str().is_some_and(|pointer| {
                    pointer == "meta.parse_complete" || pointer.starts_with("meta.warnings[].")
                })
        }));
    }
}

#[test]
fn malformed_source_retains_parser_error_through_conversion() {
    let source = "本文\nstray］";
    let aat =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    let (schemas, mapping) = v2_schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: default_test_options(),
    })
    .unwrap();
    assert_eq!(output.parser_ir["derived_from"]["parse_complete"], false);
    let error = &output.parser_ir["errors"][0];
    assert_eq!(error["code"], "unmatched-close");
    assert_eq!(error["severity"], "error");
    assert_eq!(
        error["span"],
        json!({
            "start": 12, "end": 15, "line": 2, "end_line": 2,
            "coordinate_system": "decoded_utf8"
        })
    );
}

#[test]
fn ruby_reading_preserves_nested_inline_semantics_and_local_coordinates() {
    let (schemas, mapping) = v2_schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: json!({
            "version": 2, "work_id": "rich-reading", "meta": v2_test_meta(),
            "blocks": [{"kind": "paragraph", "content": [{
                "kind": "ruby", "base": "漢", "reading": "かな𠮷",
                "reading_content": [{"kind": "style", "style_type": "bold", "content": [
                    {"kind": "text", "value": "かな"},
                    {"kind": "gaiji", "description": "𠮷", "resolved": "𠮷", "unresolved_reason": null}
                ]}]
            }]}]
        }), mapping, schemas, options: default_test_options(),
    }).unwrap();
    let ruby = &output.parser_ir["nodes"][0];
    let reading = &ruby["reading_children"][0];
    assert_eq!(reading["type"], "emphasis");
    assert_eq!(reading["style"], "bold");
    assert_eq!(
        reading["span"],
        json!({"start":0,"end":10,"coordinate_system":"reading_utf8"})
    );
    assert_eq!(reading["inline_children"][1]["type"], "gaiji");
    assert_eq!(
        reading["inline_children"][1]["span"]["coordinate_system"],
        "reading_utf8"
    );
    assert_eq!(
        ruby["span"],
        json!({"start":0,"end":3,"coordinate_system":"parser_text_utf8"})
    );
}

#[test]
fn source_decoding_outcome_survives_normalized_encoding_label() {
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(&[0x81]).unwrap()).unwrap();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: default_test_options(),
    })
    .unwrap();
    assert_eq!(output.parser_ir["source"]["encoding"], "Shift_JIS");
    assert_eq!(
        output.parser_ir["source"]["decode_outcome"],
        "windows-31j-lossy"
    );
}

#[test]
fn quoted_base_text_variant_targets_supplied_ruby_reading() {
    let source = "私は籠《ざる》［＃ルビの「ざる」は底本では「さる」］をさげ";
    let aat =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    let (schemas, mapping) = v2_schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: default_test_options(),
    })
    .unwrap();
    let ruby = output.parser_ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|node| node["type"] == "ruby")
        .unwrap();
    let variant = &ruby["reading_children"][0];
    assert_eq!(variant["type"], "base-text-variant");
    assert_eq!(variant["text"], "ざる");
    assert_eq!(variant["variant"]["base_text"], "さる");
    assert_eq!(variant["span"]["coordinate_system"], "reading_utf8");
    assert_eq!(variant["source_span"]["start"], source.find('［').unwrap());
}

#[test]
fn nonadjacent_or_mismatched_reading_variants_remain_unresolved() {
    for source in [
        "籠《ざる》と［＃ルビの「ざる」は底本では「さる」］",
        "籠《ざる》［＃ルビの「さる」は底本では「ざる」］",
    ] {
        let (schemas, mapping) = v2_schemas_and_mapping();
        let aat =
            serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
                .unwrap();
        let output = ab_aat_to_parser_ir::convert(ConversionRequest {
            aat,
            mapping,
            schemas,
            options: default_test_options(),
        })
        .unwrap();
        let nodes = output.parser_ir["nodes"].as_array().unwrap();
        let note = nodes
            .iter()
            .find(|node| node["type"] == "editor-note")
            .unwrap();
        assert_eq!(note["note"]["resolution"], "unresolved");
        assert_eq!(note["source_span"]["start"], source.find('［').unwrap());
        assert!(
            nodes
                .iter()
                .filter(|node| node["type"] == "ruby")
                .all(|node| node.get("reading_children").is_none())
        );
    }
}

#[test]
fn unknown_source_directive_survives_parser_derived_provenance() {
    let source = "前［＃未定義の範囲指定開始］後";
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: default_test_options(),
    })
    .unwrap();
    let note = output.parser_ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|node| node["type"] == "editor-note")
        .unwrap();
    assert_eq!(note["note"]["raw"], "［＃未定義の範囲指定開始］");
    assert_eq!(note["note"]["resolution"], "unresolved");
    assert_eq!(note["source_span"]["start"], "前".len());
    assert_eq!(note["source_span"]["end"], source.len() - "後".len());
    let problems = output.parser_ir["interpretation_problems"]
        .as_array()
        .unwrap();
    assert_eq!(problems.len(), 1);
    assert_eq!(problems[0]["source_span"], note["source_span"]);
    assert_eq!(problems[0]["kind"], "unknown-notation");
    assert_eq!(
        problems[0]["aspects"],
        json!(["content", "structure", "layout"])
    );
    assert_eq!(problems[0]["influence"], json!({"kind": "document"}));
}

#[test]
fn warichu_preserves_distinct_rich_halves_in_body_and_reading() {
    for reading in [false, true] {
        let warichu = json!({"kind": "warigaki", "upper": [{"kind":"style", "style_type":"bold",
            "content":[{"kind":"text","value":"上"}]}],
            "lower":[{"kind":"gaiji","description":"𠮷","resolved":"𠮷","unresolved_reason":null}]});
        let content = if reading {
            json!({"kind":"ruby", "base":"字", "reading":"上𠮷", "reading_content":[warichu]})
        } else {
            warichu
        };
        let (schemas, mapping) = v2_schemas_and_mapping();
        let output = ab_aat_to_parser_ir::convert(ConversionRequest {
            aat: json!({"version":2,"work_id":"warichu","meta":v2_test_meta(),
                "blocks":[{"kind":"paragraph","content":[content]}]}),
            mapping,
            schemas,
            options: default_test_options(),
        })
        .unwrap();
        let node = if reading {
            &output.parser_ir["nodes"][0]["reading_children"][0]
        } else {
            &output.parser_ir["nodes"][0]
        };
        assert_eq!(node["type"], "warichu");
        assert_eq!(node["text"], "上𠮷");
        assert_eq!(node["upper_children"][0]["type"], "emphasis");
        assert_eq!(node["lower_children"][0]["type"], "gaiji");
        assert_eq!(node["lower_children"][0]["span"]["start"], 3);
        assert_eq!(node["lower_children"][0]["span"]["end"], 7);
        assert_eq!(
            node["upper_children"][0]["inline_children"][0]["span"]["coordinate_system"],
            if reading {
                "reading_utf8"
            } else {
                "parser_text_utf8"
            }
        );
    }
}

#[test]
fn source_warichu_preserves_unsplit_layout_without_invented_halves() {
    let source = "前［＃割り注］上※［＃「吉」、U+20BB7］［＃割り注終わり］後";
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: default_test_options(),
    })
    .unwrap();
    let node = output.parser_ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|node| node["type"] == "warichu")
        .unwrap();
    assert_eq!(node["text"], "上𠮷");
    assert!(node.get("upper_children").is_none());
    assert!(node.get("lower_children").is_none());
    assert_eq!(node["inline_children"][1]["type"], "gaiji");
    assert_eq!(node["source_span"]["start"], "前".len());
    assert_eq!(node["source_span"]["end"], source.len() - "後".len());
    assert_eq!(output.parser_ir["interpretation_problems"], json!([]));
}

#[test]
fn source_left_underline_preserves_native_mark_and_ruby_target() {
    let source = "東京《とうきょう》［＃「東京」の左に傍線］";
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: default_test_options(),
    })
    .unwrap();
    let node = &output.parser_ir["nodes"][0];
    assert_eq!(node["type"], "emphasis");
    assert_eq!(node["style"], "bosen");
    assert_eq!(node["decoration"], json!({"kind":"傍線","position":"left"}));
    assert_eq!(node["inline_children"][0]["type"], "ruby");
    assert_eq!(node["text"], "東京");
}

#[test]
fn established_source_facts_survive_without_claiming_raw_markers() {
    let source = "東京《とうきょう》［＃「東京」の左に傍線］［＃割り注］※［＃「吉」、U+20BB7］［＃割り注終わり］［＃未知の指定］";
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat: Value =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    let expected = aat["meta"]["interpretation_facts"].clone();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: default_test_options(),
    })
    .unwrap();
    assert_eq!(output.parser_ir["interpretation_facts"], expected);
    let facts = expected.as_array().unwrap();
    for (kind, count) in [("ruby", 1), ("gaiji", 1), ("emphasis", 1), ("warichu", 2)] {
        assert_eq!(
            facts.iter().filter(|fact| fact["kind"] == kind).count(),
            count
        );
    }
    assert_eq!(facts.len(), 5);
    for fact in facts {
        assert_eq!(fact["outcome"], "established");
        assert_eq!(fact["source_span"]["coordinate_system"], "decoded_utf8");
        assert!(
            fact["source_span"]["end"].as_u64().unwrap() <= source.find("［＃未知").unwrap() as u64
        );
    }
    assert_eq!(
        output.parser_ir["interpretation_problems"]
            .as_array()
            .unwrap()
            .len(),
        1
    );
}

#[test]
fn nested_base_text_alternative_retains_explicit_unresolved_influence() {
    let directive = "［＃「※［＃「目＋旬」、第3水準1-88-80］《めくば》せを」は底本では「※［＃「目＋句」、第4水準2-81-91］《めくば》せを」］";
    let source = format!("目{directive}後。");
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: default_test_options(),
    })
    .unwrap();
    let problems = output.parser_ir["interpretation_problems"]
        .as_array()
        .unwrap();
    assert_eq!(problems.len(), 1);
    assert_eq!(problems[0]["kind"], "unresolved-variant");
    assert_eq!(problems[0]["raw"], directive);
    assert_eq!(problems[0]["aspects"], json!(["content", "structure"]));
    assert_eq!(problems[0]["influence"], json!({"kind":"document"}));
    assert_eq!(output.parser_ir["interpretation_facts"], json!([]));
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
    let warichu = nodes.iter().find(|node| node["type"] == "warichu").unwrap();
    assert_eq!(warichu["upper_children"][0]["text"], "X");
    assert_eq!(warichu["lower_children"][0]["text"], "Y");
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
    assert!(!has_divergence_record(
        &output,
        "UNSUPPORTED",
        "blocks[].children[].heading.content[].warigaki",
        None
    ));
    assert_eq!(
        output.parser_ir["source"]["decode_outcome"],
        "windows-31j-lossy"
    );
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
    let abc = research_root(&repo);
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
        .arg("--research-root")
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
    let abc = research_root(&repo);
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
        .arg("--research-root")
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
    let abc = research_root(&repo);
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
        .arg("--research-root")
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
fn cli_convert_preserves_independent_ortho_annotations() {
    let repo = repo_root();
    let abc = research_root(&repo);
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
        .arg("--research-root")
        .arg(abc)
        .status()
        .unwrap();

    assert!(status.success());
    let parser_ir_json = read_json(&parser_ir).unwrap();
    assert!(parser_ir_json.get("sentences").is_none());
    assert!(parser_ir_json.get("orthographic_annotations").is_some());
    assert!(divergence.exists());
}

#[test]
fn cli_detect_ortho_annotations_reports_missing_dictionary() {
    let repo = repo_root();
    let abc = research_root(&repo);
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
        .arg("--research-root")
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
    let abc = research_root(&repo);
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
        .arg("--research-root")
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
    let abc = research_root(&repo);
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
        .arg("--research-root")
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
    let abc = research_root(&repo);
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
        .arg("--research-root")
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
    // Sub-segments carry synthetic spans (parser_text_utf8 coordinate system).
    assert_eq!(
        quote_nodes[0]["span"]["coordinate_system"],
        "parser_text_utf8"
    );
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
fn ruby_node_spans_distinguish_parser_text_and_source_markup() {
    // Ruby occupies its base width in parser text and its full markup width in source.
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
    assert_eq!(ruby["span"]["coordinate_system"], "parser_text_utf8");
    assert_eq!(
        ruby["source_span"],
        json!({"start": 9, "end": 24, "line": 1, "coordinate_system": "decoded_utf8"})
    );
    assert!(
        nodes
            .iter()
            .filter(|n| n["type"] == "text")
            .all(|n| n.get("source_span").is_none())
    );

    // The whole (ruby-bearing) document must convert and remain schema-valid — the
    // The node spans retain the same visible-text coordinate basis.
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn adapter_conversion_spans_address_full_source_across_body_and_terminal_provenance() {
    let source = "\u{feff}作品名\r\n著者名\r\n\r\n本\u{e001}文です。\r\n\r\n底本：テスト本\r\n";
    let bytes = source.as_bytes();
    let decoded = ab_aozora_aat::decode_source_bytes(bytes).unwrap();
    assert!(decoded.span_text.len() < decoded.text.len());
    assert!(decoded.tail_offset > decoded.span_text.len());

    let aat: Value =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(bytes).unwrap()).unwrap();
    let version = aat["version"].as_u64().unwrap();
    let (repo_root, research_root) = roots();
    let mapping = MappingDocument::from_path(
        &repo_root.join(format!("data/aat-to-parser-ir-mapping-v{version}.json")),
    )
    .unwrap();
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &research_root, version).unwrap();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: default_test_options(),
    })
    .unwrap();

    let nodes = output.parser_ir["nodes"].as_array().unwrap();
    let body = nodes.iter().find(|node| node["type"] == "text").unwrap();
    assert_eq!(
        body["span"],
        json!({"start": 0, "end": 18, "coordinate_system": "parser_text_utf8"})
    );
    assert_eq!(
        body["source_span"],
        json!({"start": 24, "end": 42, "line": 4, "coordinate_system": "decoded_utf8"})
    );
    for node in nodes {
        assert_eq!(node["span"]["coordinate_system"], "parser_text_utf8");
        if let Some(span) = node.get("source_span") {
            assert_eq!(span["coordinate_system"], "decoded_utf8");
            let start = span["start"].as_u64().unwrap() as usize;
            let end = span["end"].as_u64().unwrap() as usize;
            assert!(
                decoded.text.get(start..end).is_some(),
                "source extent must address real UTF-8 boundaries: {span}"
            );
        }
    }
    let start = body["source_span"]["start"].as_u64().unwrap() as usize;
    let end = body["source_span"]["end"].as_u64().unwrap() as usize;
    assert_eq!(&decoded.text[start..end], "本\u{e001}文です。");
    let tail = nodes
        .iter()
        .find(|node| node["type"] == "source-note")
        .unwrap();
    let tail_start = tail["source_span"]["start"].as_u64().unwrap() as usize;
    assert_eq!(tail_start, decoded.text.find("底本：").unwrap());
    assert!(tail_start > decoded.span_text.len());
    assert_eq!(tail["span"]["start"], 18);
}

#[test]
fn heading_indent_and_gaiji_ruby_base_survive_conversion() {
    let (schemas, mapping) = schemas_and_mapping();
    let ruby = json!({
        "kind": "ruby", "base": "犍陀多", "reading": "かんだた",
        "base_content": [
            {"kind": "gaiji", "description": "特のへん＋廴＋聿", "resolved": "犍", "jis_code": "第3水準1-87-71", "unresolved_reason": null},
            {"kind": "text", "value": "陀多"}
        ]
    });
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: json!({
            "version": 1, "work_id": "source-layout",
            "meta": base_meta("utf-8", "sha256:7777777777777777777777777777777777777777777777777777777777777777"),
            "blocks": [
                {"kind": "heading", "level": 2, "x-indent": 8, "style": "normal", "content": [ruby.clone()]},
                {"kind": "paragraph", "content": [ruby]}
            ]
        }),
        mapping, schemas: schemas.clone(), options: default_test_options(),
    }).unwrap();
    assert_eq!(output.parser_ir["nodes"][0]["indent"], 8);
    for ruby_node in [
        &output.parser_ir["nodes"][0]["inline_children"][0],
        &output.parser_ir["nodes"][1],
    ] {
        assert_eq!(ruby_node["ruby"]["base"], "犍陀多");
        assert_eq!(ruby_node["inline_children"][0]["gaiji"]["unicode"], "犍");
        assert_eq!(ruby_node["inline_children"][1]["text"], "陀多");
    }
    assert!(!has_divergence_record(
        &output,
        "LOSS",
        "blocks[].heading.indent",
        None
    ));
    assert!(!has_divergence_record(
        &output,
        "LOSS",
        "blocks[].content[].ruby.base_content",
        None
    ));
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn unresolved_gaiji_ruby_uses_child_projection_for_decoded_coordinates() {
    for wrapper in ["paragraph", "chitsuki", "emphasis", "heading"] {
        let (schemas, mapping) = v2_schemas_and_mapping();
        let ruby = json!({
            "kind": "ruby", "base": "※［＃「てへん＋諂のつくり」、13-下-25］", "reading": "ひね",
            "base_content": [{
                "kind": "gaiji", "description": "てへん＋諂のつくり",
                "jis_code": "13-下-25", "resolved": null, "unresolved_reason": "unresolved"
            }]
        });
        let content = json!([
            ruby,
            {"kind": "text", "value": "花\n\n"},
            {"kind": "text", "value": "　六月の事なりき。年ごとに"}
        ]);
        let block = match wrapper {
            "paragraph" => json!({"kind": "paragraph", "content": content}),
            "chitsuki" => json!({"kind": "paragraph", "content": [{
                "kind": "style", "style_type": "chitsuki", "align": "right",
                "offset_from_end": 1, "x-provenance": "source-derived", "content": content
            }]}),
            "emphasis" => json!({"kind": "paragraph", "content": [{
                "kind": "style", "style_type": "bouten", "content": content
            }]}),
            _ => json!({"kind": "heading", "level": 2, "style": "normal", "content": content}),
        };
        let output = ab_aat_to_parser_ir::convert(ConversionRequest {
            aat: json!({"version": 2, "work_id": "unresolved-ruby", "meta": v2_test_meta(), "blocks": [block]}),
            mapping, schemas: schemas.clone(), options: default_test_options(),
        }).unwrap_or_else(|error| panic!("{wrapper}: {error}"));
        let first = &output.parser_ir["nodes"][0];
        let ruby_node = if first["type"] == "ruby" {
            first
        } else {
            &first["inline_children"][0]
        };
        assert_eq!(ruby_node["ruby"]["base"], "てへん＋諂のつくり");
        assert_eq!(ruby_node["span"]["end"], "てへん＋諂のつくり".len());
        assert_eq!(
            ruby_node["inline_children"][0]["span"]["end"],
            ruby_node["span"]["end"]
        );
        assert!(ruby_node["inline_children"][0]["gaiji"]["unicode"].is_null());
        assert_eq!(
            ruby_node["inline_children"][0]["gaiji"]["raw_marker"],
            "てへん＋諂のつくり"
        );
        let expected = "てへん＋諂のつくり花\n\n　六月の事なりき。年ごとに";
        if wrapper == "heading" {
            assert_eq!(first["text"], expected);
            assert_eq!(first["span"]["end"], expected.len());
        } else {
            let nodes = output.parser_ir["nodes"].as_array().unwrap();
            let text = visible_text_for_node_range(nodes, &output.parser_ir["paragraphs"][0]);
            assert_eq!(text, expected);
            assert_eq!(
                output.parser_ir["paragraphs"][0]["span"]["end"],
                expected.len()
            );
        }
        validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
    }
}

#[test]
fn source_corrections_and_one_compound_sign_survive_validated_conversion() {
    let source = "題\n作者\n\n甍《いらか》［＃「甍の」は底本では「薨の」］先。\n［＃ここから４字下げ、横書き、中央揃え、罫囲み］\nRESTAURANT\n西洋料理店\nWILDCAT HOUSE\n山猫軒\n［＃ここで字下げ終わり］\nといふ札。\n\n底本：本\n";
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat: Value =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    let mut wrong_owner = aat.clone();
    wrong_owner["blocks"][1]["kind"] = json!("quote_block");
    assert!(validate_value(&schemas.aat_schema, &wrong_owner, "AAT").is_err());
    let mut wrong_children = aat.clone();
    wrong_children["blocks"][1]["children"][0] =
        json!({"kind": "heading", "level": 2, "style": "", "content": []});
    assert!(validate_value(&schemas.aat_schema, &wrong_children, "AAT").is_err());
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();
    let ir = &output.parser_ir;
    let notes: Vec<_> = ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|node| node["type"] == "editor-note")
        .collect();
    assert_eq!(notes.len(), 1);
    assert_eq!(
        notes[0]["note"],
        json!({"raw": "［＃「甍の」は底本では「薨の」］", "category": "variant", "resolution": "unresolved"})
    );
    assert_eq!(ir["layout_blocks"].as_array().unwrap().len(), 1);
    let block = &ir["layout_blocks"][0];
    assert_eq!(block["paragraph_range"], json!({"start": 1, "end": 5}));
    assert_eq!(block["indent"], 4);
    assert_eq!(block["direction"], "horizontal");
    assert_eq!(block["align"], "center");
    assert_eq!(block["border"], "solid");
    for paragraph in &ir["paragraphs"].as_array().unwrap()[1..5] {
        assert!(
            paragraph.get("layout").is_none(),
            "shared border/indent belongs to its block"
        );
    }
    validate_value(&schemas.parser_ir_schema, ir, "parser-IR").unwrap();
}

#[test]
fn source_markup_gaps_and_gaiji_have_independent_projection_and_source_extents() {
    let source = "\u{feff}作品名\r\n著者名\r\n\r\n前※［＃「てへん＋劣」、第3水準1-84-77］後。\r\n\r\n底本：テスト本\r\n";
    let decoded = ab_aozora_aat::decode_source_bytes(source.as_bytes()).unwrap();
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();
    let nodes = output.parser_ir["nodes"].as_array().unwrap();
    let gaiji = nodes.iter().find(|node| node["type"] == "gaiji").unwrap();
    assert_eq!(gaiji["gaiji"]["unicode"], "挘");
    assert_eq!(
        gaiji["span"],
        json!({"start": 3, "end": 6, "coordinate_system": "parser_text_utf8"})
    );
    let start = gaiji["source_span"]["start"].as_u64().unwrap() as usize;
    let end = gaiji["source_span"]["end"].as_u64().unwrap() as usize;
    assert_eq!(
        &decoded.text[start..end],
        "※［＃「てへん＋劣」、第3水準1-84-77］"
    );
    let following = nodes
        .iter()
        .find(|node| {
            node["text"]
                .as_str()
                .is_some_and(|text| text.starts_with("後。"))
        })
        .unwrap();
    assert_eq!(following["span"]["start"], 6);
    assert_eq!(following["source_span"]["start"], end);
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn enclosing_indent_survives_line_local_closing_alignment() {
    let source = "前。\n［＃ここから２字下げ］\n附記。\n［＃地から２字上げ］（大正四年八月）\n［＃ここで字下げ終わり］\n後。\n";
    let (schemas, mapping) = v2_schemas_and_mapping();
    for date_offset in [2, 3] {
        let changed = source.replace("地から２字上げ", &format!("地から{date_offset}字上げ"));
        let aat = serde_json::from_slice(
            &ab_aozora_aat::aat_json_from_bytes(changed.as_bytes()).unwrap(),
        )
        .unwrap();
        let output = ab_aat_to_parser_ir::convert(ConversionRequest {
            aat,
            mapping: mapping.clone(),
            schemas: schemas.clone(),
            options: default_test_options(),
        })
        .unwrap();
        let ir = &output.parser_ir;
        assert_eq!(ir["paragraphs"].as_array().unwrap().len(), 4);
        assert_eq!(
            ir["layout_blocks"],
            json!([{
                "paragraph_range": {"start": 1, "end": 3}, "indent": 2, "source_pointer": "blocks[1]"
            }])
        );
        assert_eq!(ir["paragraphs"][2]["layout"]["kind"], "chitsuki");
        assert_eq!(
            ir["paragraphs"][2]["layout"]["offset_from_end"],
            date_offset
        );
        for index in [0, 1, 3] {
            assert!(ir["paragraphs"][index].get("layout").is_none());
        }
        validate_value(&schemas.parser_ir_schema, ir, "parser-IR").unwrap();
        let mut malformed = ir.clone();
        malformed["layout_blocks"][0]
            .as_object_mut()
            .unwrap()
            .remove("indent");
        assert!(validate_value(&schemas.parser_ir_schema, &malformed, "parser-IR").is_err());
        let mut malformed = ir.clone();
        malformed["layout_blocks"][0]["align"] = json!("left");
        assert!(validate_value(&schemas.parser_ir_schema, &malformed, "parser-IR").is_err());
    }
}

#[test]
fn structured_ruby_reading_preserves_resolved_gaiji_in_body_and_heading() {
    for wrapper in ["paragraph", "heading"] {
        let (schemas, mapping) = v2_schemas_and_mapping();
        let mut block = json!({"kind": wrapper, "content": [{
            "kind": "ruby", "base": "淡絹",
            "reading": "※［＃濁点付き片仮名ヱ、1-7-84］エル",
            "reading_content": [
                {"kind": "gaiji", "description": "濁点付き片仮名ヱ", "jis_code": "1-7-84", "resolved": "ヹ", "unresolved_reason": null, "span": {"byte_start": 9, "byte_end": 54, "line_start": 1, "line_end": 1}},
                {"kind": "text", "value": "エル"}
            ]
        }]});
        if wrapper == "heading" {
            block["level"] = json!(2);
            block["style"] = json!("normal");
        }
        let output = ab_aat_to_parser_ir::convert(ConversionRequest {
            aat: json!({"version": 2, "work_id": "gaiji-reading", "meta": v2_test_meta(), "blocks": [block]}),
            mapping, schemas: schemas.clone(), options: default_test_options(),
        }).unwrap();
        let first = &output.parser_ir["nodes"][0];
        let ruby = if wrapper == "paragraph" {
            first
        } else {
            &first["inline_children"][0]
        };
        assert_eq!(ruby["ruby"]["reading"], "ヹエル");
        assert_eq!(ruby["ruby"]["base"], "淡絹");
        assert_eq!(ruby["span"]["end"], "淡絹".len());
        let reading = &ruby["reading_children"];
        assert_eq!(reading[0]["gaiji"]["unicode"], "ヹ");
        assert_eq!(reading[0]["gaiji"]["reference"], "1-7-84");
        assert_eq!(reading[0]["gaiji"]["raw_marker"], "濁点付き片仮名ヱ");
        assert_eq!(
            reading[0]["source_span"],
            json!({"start": 9, "end": 54, "coordinate_system": "decoded_utf8", "line": 1})
        );
        assert_eq!(reading[0]["span"]["coordinate_system"], "reading_utf8");
        assert!(reading[1].get("source_span").is_none());
        assert_eq!(reading[1]["text"], "エル");
        let mut invalid = output.parser_ir.clone();
        let target = if wrapper == "paragraph" {
            &mut invalid["nodes"][0]
        } else {
            &mut invalid["nodes"][0]["inline_children"][0]
        };
        target["reading_children"][0]["span"] =
            json!({"start": 0, "end": 3, "coordinate_system": "parser_text_utf8"});
        assert!(
            validate_value(
                &schemas.parser_ir_schema,
                &invalid,
                "invented reading body span"
            )
            .is_err()
        );
        validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
    }
}

#[test]
fn corpus_warichu_with_multiple_sentences_preserves_one_source_container() {
    let text = "宇宙にはあらゆる象徴瀰漫す。しかして、その神秘的な法則と配列の妙義は、隠れたる事象を人に告げ、あるいは予め告げ知らしむ。";
    let source = format!("題\n作者\n\n前［＃割り注］{text}［＃割り注終わり］後。\n\n底本：本\n");
    let aat: Value =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    let (schemas, mapping) = v2_schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: default_test_options(),
    })
    .unwrap();
    let nodes = output.parser_ir["nodes"].as_array().unwrap();
    let warichu: Vec<_> = nodes
        .iter()
        .filter(|node| node["type"] == "warichu")
        .collect();
    assert_eq!(warichu.len(), 1);
    assert_eq!(warichu[0]["text"], text);
    assert_eq!(warichu[0]["inline_children"][0]["text"], text);
    let start = warichu[0]["source_span"]["start"].as_u64().unwrap() as usize;
    let end = warichu[0]["source_span"]["end"].as_u64().unwrap() as usize;
    assert_eq!(
        &source[start..end],
        format!("［＃割り注］{text}［＃割り注終わり］")
    );
    assert!(warichu[0].get("upper_children").is_none());
    assert!(warichu[0].get("lower_children").is_none());
    assert!(output.parser_ir.get("sentences").is_none());
    assert!(output.parser_ir.get("sentence_segmentation").is_none());
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}

#[test]
fn raw_source_retention_records_exact_occurrence_independent_of_nesting() {
    for depth in [0, 1, 3, 7] {
        let (schemas, mapping) = v2_schemas_and_mapping();
        let raw = "［＃レ］";
        let mut node = json!({"kind":"raw", "source":raw,
            "x-provenance":"parser-derived", "x-source-marker-kind":"kaeriten",
            "span":{"byte_start":0,"byte_end":raw.len(),"line_start":1,"line_end":1}});
        for _ in 0..depth {
            node = json!({"kind":"style", "style_type":"bold", "content":[node]});
        }
        let output = ab_aat_to_parser_ir::convert(ConversionRequest {
            aat: json!({"version":2,"work_id":"nested-raw","meta":v2_test_meta(),
                "blocks":[{"kind":"paragraph","content":[node]}]}),
            mapping,
            schemas,
            options: default_test_options(),
        })
        .unwrap();
        let mut note = &output.parser_ir["nodes"][0];
        for _ in 0..depth {
            note = &note["inline_children"][0];
        }
        assert_eq!(note["note"]["raw"], raw);
        assert_eq!(note["note"]["resolution"], "unresolved");
        assert_eq!(note["source_span"]["start"], 0);
        assert_eq!(note["source_span"]["end"], raw.len());
        let record = output.divergence_bundle["records"]
            .as_array()
            .unwrap()
            .iter()
            .find(|record| record["parser_ir_pointer"] == "nodes[].note.raw")
            .unwrap();
        assert_eq!(record["category"], "UNSUPPORTED");
        assert_eq!(record["source_value"], raw);
        assert_eq!(
            record["first_path"],
            format!("blocks[0]{}.raw", ".content[0]".repeat(depth + 1))
        );
    }
}

#[test]
fn native_forced_break_and_supplied_kunten_survive_nested_warichu() {
    let source = "題\n作者\n\n前［＃地から３字上げ］［＃割り注］磯。此云［＃レ］志。［＃改行］次［＃割り注終わり］後。\n\n底本：本\n";
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas,
        options: default_test_options(),
    })
    .unwrap();
    let warichu = output.parser_ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|node| node["type"] == "warichu")
        .unwrap();
    assert_eq!(warichu["type"], "warichu");
    assert_eq!(warichu["text"], "磯。此云志。\n次");
    let children = warichu["inline_children"].as_array().unwrap();
    let line_break = children
        .iter()
        .find(|node| node["type"] == "line-break")
        .unwrap();
    let start = line_break["source_span"]["start"].as_u64().unwrap() as usize;
    let end = line_break["source_span"]["end"].as_u64().unwrap() as usize;
    assert_eq!(&source[start..end], "［＃改行］");
    let note = children
        .iter()
        .find(|node| node["type"] == "kunten")
        .unwrap();
    assert_eq!(note["kunten_kind"], "return-mark");
    assert_eq!(note["text"], "レ");
    let start = note["source_span"]["start"].as_u64().unwrap() as usize;
    let end = note["source_span"]["end"].as_u64().unwrap() as usize;
    assert_eq!(&source[start..end], "［＃レ］");
    assert_eq!(output.parser_ir["interpretation_problems"], json!([]));
}

#[test]
fn principal_text_alternatives_preserve_supplied_text_and_rich_targets() {
    for (body, current, base_text, has_ruby) in [
        (
            "積る甍の［＃「甍の」は底本では「薨の」］雪。",
            "甍の",
            "薨の",
            false,
        ),
        (
            "前｜東京《とうきょう》の町［＃「東京の町」は底本では「東亰の町」］後。",
            "東京の町",
            "東亰の町",
            true,
        ),
        ("字字［＃「字」は底本では「別」］。", "字", "別", false),
    ] {
        let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
        let (schemas, mapping) = v2_schemas_and_mapping();
        let aat =
            serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
                .unwrap();
        let output = ab_aat_to_parser_ir::convert(ConversionRequest {
            aat,
            mapping,
            schemas,
            options: default_test_options(),
        })
        .unwrap();
        let variant = output.parser_ir["nodes"]
            .as_array()
            .unwrap()
            .iter()
            .find(|node| node["type"] == "base-text-variant")
            .expect(body);
        assert_eq!(variant["text"], current);
        assert_eq!(variant["variant"]["base_text"], base_text);
        assert_eq!(variant["inline_children"][0]["type"] == "ruby", has_ruby);
        if has_ruby {
            assert_eq!(
                variant["inline_children"][0]["ruby"]["reading"],
                "とうきょう"
            );
        }
        let start = variant["source_span"]["start"].as_u64().unwrap() as usize;
        let end = variant["source_span"]["end"].as_u64().unwrap() as usize;
        assert_eq!(
            &source[start..end],
            format!("［＃「{current}」は底本では「{base_text}」］")
        );
        assert!(
            output.parser_ir["interpretation_problems"]
                .as_array()
                .unwrap()
                .is_empty()
        );
    }
}

#[test]
fn principal_text_alternatives_do_not_search_past_mismatches_or_split_ruby() {
    for body in [
        "前の字。別の字［＃「前の字」は底本では「旧字」］。",
        "｜東京《とうきょう》［＃「京」は底本では「亰」］。",
        "先の字。\n［＃「字」は底本では「別」］",
    ] {
        let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
        let (schemas, mapping) = v2_schemas_and_mapping();
        let aat =
            serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
                .unwrap();
        let output = ab_aat_to_parser_ir::convert(ConversionRequest {
            aat,
            mapping,
            schemas,
            options: default_test_options(),
        })
        .unwrap();
        assert_eq!(
            output.parser_ir["interpretation_problems"][0]["kind"], "unresolved-variant",
            "{body}"
        );
        assert!(
            !output.parser_ir["nodes"]
                .as_array()
                .unwrap()
                .iter()
                .any(|node| node["type"] == "base-text-variant")
        );
    }
}
