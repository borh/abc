use std::fs;
use std::path::PathBuf;

use ab_parser_rq_source_accountability::{
    AdapterCoordinates, CorpusEntry, CorpusInput, CorpusSourceEntry, QualificationIdentity,
    TaxonomyIdentity, TaxonomyVersion, analyze_corpus, canonical_json,
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
        adapter_coordinates: AdapterCoordinates {
            aat_version: 2,
            aat_adapter: "ab-aozora-aat".into(),
            aat_adapter_version: Some("0.1.0".into()),
        },
        mapping_id: "https://example.test/mapping".into(),
        mapping_version: "1".into(),
        mapping_hash: format!("sha256:{}", "1".repeat(64)),
        mapping_schema_hash: format!("sha256:{}", "2".repeat(64)),
        parser_ir_schema_id: "https://example.test/parser-ir".into(),
        parser_ir_schema_hash: format!("sha256:{}", "3".repeat(64)),
        corpus_snapshot_hash: format!("sha256:{}", "4".repeat(64)),
        corpus_list_hash: format!("sha256:{}", "5".repeat(64)),
        predicate_set_hash: format!("sha256:{}", "6".repeat(64)),
        instrument_versions: vec!["parser-rq-source-accountability-v1".into()],
    }
}

fn parser_ir(source: &[u8]) -> Vec<u8> {
    let q = qualification();
    serde_json::to_vec(&json!({
        "schema_id": q.parser_ir_schema_id,
        "schema_hash": q.parser_ir_schema_hash,
        "derived_from": {
            "aat_version": q.adapter_coordinates.aat_version,
            "aat_adapter": q.adapter_coordinates.aat_adapter,
            "aat_adapter_version": q.adapter_coordinates.aat_adapter_version,
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
    let taxonomy_jcs_bytes = br#"{"coordinate_system":"decoded_utf8","rules":[],"schema_version":"abc/parser-rq-ignored-regions/v1","taxonomy_version":"parser-rq-ignored-regions-v1"}"#.to_vec();
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
