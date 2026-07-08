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

fn schemas_and_mapping() -> (SchemaSet, MappingDocument) {
    let repo = repo_root();
    let abc = abc_root(&repo);
    let schemas = SchemaSet::load(&repo, &abc).unwrap();
    let mapping =
        MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v1.json")).unwrap();
    (schemas, mapping)
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

fn ortho_fixture_bundle() -> ab_aat_to_parser_ir::ortho_annotations::OrthoAnnotationsBundle {
    serde_json::from_value(json!({
        "work_id": "000000",
        "work_content_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
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
    let schemas = SchemaSet::load(&repo, &abc).unwrap();

    assert_eq!(
        schema_hash(&schemas.mapping_schema).unwrap(),
        "sha256:e6af01115ccdb7c5cad086eee4c458230f6b6f55e0dfee7791730b48994283e2"
    );
    assert_eq!(
        schema_hash(&schemas.parser_ir_schema).unwrap(),
        "sha256:0b495bb5c12c4d76482afefdaedb5464a74672ffbd5282f9c67d5f419d39a340"
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
fn mapping_preflight_accepts_checked_in_v2_artifact() {
    let (schemas, mapping) = schemas_and_mapping();

    let index = mapping.preflight(&schemas).unwrap();

    assert_eq!(mapping.mapping_version, "0.2.6");
    assert_eq!(
        mapping.target_parser_ir_schema_hash,
        schema_hash(&schemas.parser_ir_schema).unwrap()
    );
    assert_eq!(mapping.transform_rule_descriptions.len(), 680);
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
            ..ConversionOptions::default()
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
        bundle.work_content_hash,
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
            ..ConversionOptions::default()
        },
    })
    .unwrap();

    assert_eq!(
        output
            .parser_ir
            .pointer("/sentence_segmentation/splitter_id"),
        Some(&json!("ab-plaintext-japanese-v1"))
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
        options: ConversionOptions::default(),
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
            ..ConversionOptions::default()
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
            "work_content_hash": hash,
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
            ..ConversionOptions::default()
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
            "work_content_hash": hash,
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
            ..ConversionOptions::default()
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
fn rejects_sentence_boundary_inside_atomic_emphasis_from_aat() {
    // Regression matrix (6e): shared AAT fixture reproducing the dominant real-corpus
    // sentence-projection failure — a sentence terminal inside an emphasis node that
    // carries inline_children (here, mixed text + ruby). Conversion must fail rather
    // than snap the boundary.
    let (schemas, mapping) = schemas_and_mapping();
    let error = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: include_fixture_json("atomic-boundary-emphasis-input.aat.json"),
        mapping,
        schemas,
        options: ConversionOptions::default(),
    })
    .unwrap_err()
    .to_string();
    assert!(
        error.contains("sentence boundary falls inside atomic node emphasis"),
        "unexpected error: {error}"
    );
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
            Some("source.work_content_hash"),
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
            Some("source.work_content_hash"),
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
        options: ConversionOptions::default(),
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
        options: ConversionOptions::default(),
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
        options: ConversionOptions::default(),
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
        options: ConversionOptions::default(),
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
        options: ConversionOptions::default(),
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
        options: ConversionOptions::default(),
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
        options: ConversionOptions::default(),
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
        options: ConversionOptions::default(),
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
        options: ConversionOptions::default(),
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
        options: ConversionOptions::default(),
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
        options: ConversionOptions::default(),
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
        options: ConversionOptions::default(),
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
        options: ConversionOptions::default(),
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
        options: ConversionOptions::default(),
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
        options: ConversionOptions::default(),
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
            "counterparts": [{"path": "paper/demo-melos-real/tei.xml", "work_id": "1567"}],
            "tei_dirs": ["paper"],
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
                "abc_tei": "paper/demo-melos-real/tei.xml",
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
        options: ConversionOptions::default(),
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
        options: ConversionOptions::default(),
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
        options: ConversionOptions::default(),
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
            ..ConversionOptions::default()
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
            options: ConversionOptions::default(),
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
    "abc_tei": "paper/demo-melos-real/tei.xml",
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
