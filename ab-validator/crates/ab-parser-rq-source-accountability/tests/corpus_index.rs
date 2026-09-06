use std::fs;
use std::path::{Path, PathBuf};

use ab_parser_rq_source_accountability::{
    CorpusEntry, CorpusInput, CorpusSourceEntry, QualificationIdentity, TaxonomyIdentity,
    TaxonomyVersion, analyze_corpus, canonical_json,
};
use serde_json::json;
use sha2::{Digest, Sha256};

fn hash(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn temp_root(name: &str) -> PathBuf {
    let path = std::env::temp_dir().join(format!(
        "ab-parser-rq-{name}-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&path).unwrap();
    path
}

fn qualification() -> QualificationIdentity {
    QualificationIdentity {
        parser_git_rev: "abc123".into(),
        aat_version: 2,
        aat_adapter: "ab-aozora-aat".into(),
        aat_adapter_version: "0.1.0".into(),
        mapping_id: "https://example.test/mapping".into(),
        mapping_version: "1".into(),
        mapping_hash: format!("sha256:{}", "1".repeat(64)),
        mapping_schema_hash: format!("sha256:{}", "2".repeat(64)),
        parser_ir_schema_id: "https://example.test/parser-ir".into(),
        parser_ir_schema_hash: format!("sha256:{}", "3".repeat(64)),
        corpus_snapshot_hash: format!("sha256:{}", "4".repeat(64)),
        corpus_list_hash: format!("sha256:{}", "5".repeat(64)),
        predicate_set_hash: format!("sha256:{}", "6".repeat(64)),
        instrument_versions: std::collections::BTreeMap::from([(
            "source_accountability".into(),
            "parser-rq-source-accountability-v1".into(),
        )]),
        instrument_policy_hashes: std::collections::BTreeMap::from([(
            "source_recognition".into(),
            format!("sha256:{}", "7".repeat(64)),
        )]),
    }
}

fn parser_ir(source: &[u8]) -> Vec<u8> {
    let q = qualification();
    serde_json::to_vec(&json!({
        "schema_id": q.parser_ir_schema_id,
        "schema_hash": q.parser_ir_schema_hash,
        "derived_from": {
            "aat_version": q.aat_version,
            "aat_adapter": q.aat_adapter,
            "aat_adapter_version": q.aat_adapter_version,
            "mapping_id": q.mapping_id,
            "mapping_version": q.mapping_version,
            "mapping_schema_hash": q.mapping_schema_hash
        },
        "source": {"work_content_hash": hash(source)},
        "nodes": [{"span": {"start": 0, "end": source.len(), "coordinate_system": "decoded_utf8"}}]
    }))
    .unwrap()
}

fn input(root: &std::path::Path) -> CorpusInput {
    let source_root = root.join("source");
    let parser_ir_root = root.join("ir");
    let store_root = root.join("store");
    fs::create_dir_all(&source_root).unwrap();
    fs::create_dir_all(&parser_ir_root).unwrap();
    let mut entries = Vec::new();
    for (work_id, source_path, bytes) in [
        ("work-b", "nested/b.txt", b"b".as_slice()),
        ("work-a", "a.txt", b"a".as_slice()),
    ] {
        let path = source_root.join(source_path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, bytes).unwrap();
        fs::write(
            parser_ir_root.join(format!("{work_id}.json")),
            parser_ir(bytes),
        )
        .unwrap();
        entries.push(CorpusSourceEntry {
            corpus_entry: CorpusEntry {
                work_id: work_id.into(),
                original_sha256: hash(bytes),
            },
            source_path: source_path.into(),
        });
    }
    fs::write(source_root.join("unrelated.txt"), b"unrelated").unwrap();
    let taxonomy_jcs_bytes = br#"{"$schema":"https://w3id.org/abc/schemas/parser-rq-ignored-regions.schema.json","coordinate_system":"decoded_utf8","rules":[],"schema_version":"abc/parser-rq-ignored-regions/v1","taxonomy_version":"parser-rq-ignored-regions-v1"}"#.to_vec();
    CorpusInput {
        entries,
        source_root,
        parser_ir_root,
        store_root,
        index_out: root.join("index.json"),
        qualification_identity: qualification(),
        taxonomy: TaxonomyIdentity {
            taxonomy_version: TaxonomyVersion::V1,
            taxonomy_hash: hash(&taxonomy_jcs_bytes),
            taxonomy_jcs_bytes,
        },
    }
}

#[test]
fn deterministic_index_uses_only_explicit_corpus_in_canonical_order() {
    let root = temp_root("deterministic");
    let input = input(&root);
    let first = analyze_corpus(input.clone()).unwrap();
    let second = analyze_corpus(input).unwrap();
    assert_eq!(
        canonical_json(&first).unwrap(),
        canonical_json(&second).unwrap()
    );
    assert_eq!(
        first
            .records
            .iter()
            .map(|r| r.work_id.as_str())
            .collect::<Vec<_>>(),
        vec!["work-a", "work-b"]
    );
    assert!(!canonical_json(&first).unwrap().contains("unrelated"));
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn duplicate_work_ids_fail_before_writing_index() {
    let root = temp_root("duplicate");
    let mut input = input(&root);
    input.entries.push(input.entries[0].clone());
    let error = analyze_corpus(input.clone()).unwrap_err();
    assert!(error.to_string().contains("duplicate work_id"));
    assert!(!input.index_out.exists());
    assert!(!input.store_root.exists());
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn generated_index_validates_against_live_abc_schema() {
    let root = temp_root("schema");
    let input = input(&root);
    let index = analyze_corpus(input).unwrap();
    let instance = serde_json::to_value(index).unwrap();
    let schema_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(2)
        .unwrap()
        .join("research/schemas/parser-rq-source-accountability-index.schema.json");
    let schema: serde_json::Value =
        serde_json::from_slice(&fs::read(schema_path).unwrap()).unwrap();
    let validator = jsonschema::validator_for(&schema).unwrap();
    let errors = validator
        .iter_errors(&instance)
        .map(|error| error.to_string())
        .collect::<Vec<_>>();
    assert!(errors.is_empty(), "schema errors: {errors:?}");
    assert_eq!(instance["expected_work_count"], 2);
    assert_eq!(instance["record_count"], 2);
    assert_eq!(instance["status"], "ok");
    assert_eq!(instance["errors"], json!([]));
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn record_index_references_exact_content_addressed_bytes() {
    let root = temp_root("content-address");
    let input = input(&root);
    let index = analyze_corpus(input.clone()).unwrap();
    for entry in &index.records {
        let bytes = fs::read(input.store_root.join(&entry.locator)).unwrap();
        assert_eq!(entry.bytes, bytes.len() as u64);
        assert_eq!(entry.sha256, hash(&bytes));
        assert!(
            entry
                .locator
                .ends_with(&format!("{}.json", &entry.sha256[7..]))
        );
    }
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn index_locators_resolve_against_a_store_root_the_index_does_not_live_in() {
    // This property used to be tested through the `aggregate` subcommand,
    // which the node-span coverage retirement removed. It is a property of the
    // index rather than of the aggregate, so it outlives it: the index is
    // written outside the store, and every locator it carries resolves inside
    // the store and authenticates there.
    let root = temp_root("separate-store-root");
    let input = input(&root);
    let index = analyze_corpus(input.clone()).unwrap();
    assert_ne!(input.index_out.parent(), Some(input.store_root.as_path()));
    assert!(input.index_out.exists());
    for entry in &index.records {
        let resolved = input.store_root.join(&entry.locator);
        assert!(resolved.starts_with(&input.store_root));
        assert_eq!(hash(&fs::read(&resolved).unwrap()), entry.sha256);
    }
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn the_retired_aggregate_subcommand_is_gone() {
    let status =
        std::process::Command::new(env!("CARGO_BIN_EXE_ab-parser-rq-source-accountability"))
            .args(["aggregate", "--help"])
            .status()
            .unwrap();
    assert!(!status.success());
}

#[test]
fn missing_declared_input_fails_before_any_publication() {
    for missing in ["source", "ir"] {
        let root = temp_root(missing);
        let input = input(&root);
        if missing == "source" {
            fs::remove_file(input.source_root.join("a.txt")).unwrap();
        } else {
            fs::remove_file(input.parser_ir_root.join("work-a.json")).unwrap();
        }
        assert!(analyze_corpus(input.clone()).is_err());
        assert!(!input.index_out.exists());
        assert!(!input.store_root.exists());
        fs::remove_dir_all(root).unwrap();
    }
}

#[test]
fn source_paths_reject_traversal_absolute_and_symlink_escape() {
    let root = temp_root("escape");
    let outside = root.join("outside.txt");
    fs::write(&outside, b"a").unwrap();
    for escaped in [PathBuf::from("../outside.txt"), outside.clone()] {
        let mut input = input(&root);
        input.entries[1].source_path = escaped;
        assert!(analyze_corpus(input.clone()).is_err());
        assert!(!input.index_out.exists());
        assert!(!input.store_root.exists());
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::symlink;
        let mut input = input(&root);
        let link = input.source_root.join("escape.txt");
        symlink(&outside, &link).unwrap();
        input.entries[1].source_path = "escape.txt".into();
        assert!(analyze_corpus(input.clone()).is_err());
        assert!(!input.index_out.exists());
        assert!(!input.store_root.exists());
    }
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn parser_ir_work_id_rejects_traversal_and_absolute_forms() {
    for work_id in ["../escape", "/absolute"] {
        let root = temp_root("work-id-escape");
        let mut input = input(&root);
        input.entries[0].corpus_entry.work_id = work_id.into();
        assert!(analyze_corpus(input.clone()).is_err());
        assert!(!input.index_out.exists());
        assert!(!input.store_root.exists());
        fs::remove_dir_all(root).unwrap();
    }
}

#[test]
fn unavailable_work_makes_index_truthfully_unavailable() {
    let root = temp_root("unavailable");
    let mut input = input(&root);
    input.entries[0].corpus_entry.original_sha256 = hash(b"not-the-source");
    let index = analyze_corpus(input).unwrap();
    let value = serde_json::to_value(index).unwrap();
    assert_eq!(value["status"], "unavailable");
    assert_eq!(value["errors"], json!(["work-unavailable:work-b"]));
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn canonical_json_rejects_floating_point_numbers() {
    assert!(canonical_json(&json!({"value": 1.5})).is_err());
    assert_eq!(
        canonical_json(&json!({"b": 2, "a": {"d": 4, "c": 3}})).unwrap(),
        r#"{"a":{"c":3,"d":4},"b":2}"#
    );
}
