use std::path::{Path, PathBuf};

use ab_aat_to_parser_ir::{
    ConversionOptions, ConversionRequest, MappingDocument, SchemaSet,
    divergence::{AatMeta, DivergenceRecorder},
    mapping::MappingRule,
    schema::{schema_hash, validate_value},
};
use serde_json::{Value, json};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn abc_root(repo: &Path) -> PathBuf {
    std::env::var_os("AB_ABC_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|| repo.join("../abc"))
}

fn schemas_and_mapping() -> (SchemaSet, MappingDocument) {
    let repo = repo_root();
    let abc = abc_root(&repo);
    let schemas = SchemaSet::load(&repo, &abc).unwrap();
    let mapping =
        MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v1.json")).unwrap();
    (schemas, mapping)
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
fn legacy_schema_hashes_match_mapping_artifact() {
    let repo = repo_root();
    let abc = abc_root(&repo);
    let schemas = SchemaSet::load(&repo, &abc).unwrap();

    assert_eq!(
        schema_hash(&schemas.mapping_schema).unwrap(),
        "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"
    );
    assert_eq!(
        schema_hash(&schemas.parser_ir_schema).unwrap(),
        "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"
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
fn mapping_preflight_accepts_checked_in_v1_1_artifact() {
    let (schemas, mapping) = schemas_and_mapping();

    let index = mapping.preflight(&schemas).unwrap();

    assert_eq!(mapping.mapping_version, "0.1.1");
    assert_eq!(mapping.transform_rule_descriptions.len(), 118);
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
            &schemas,
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
    assert!(output.emitted_rule_ids.contains("A-18"));
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
    assert!(output.emitted_rule_ids.contains("U-09"));
    assert!(output.emitted_rule_ids.contains("A-27"));
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
