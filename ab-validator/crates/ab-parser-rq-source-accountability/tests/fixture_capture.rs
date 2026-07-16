use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

use ab_parser_rq_source_accountability::{
    CorpusEntry, CorpusInput, CorpusSourceEntry, QualificationIdentity, TaxonomyIdentity,
    TaxonomyVersion, aggregate, analyze_corpus, canonical_json,
};
use serde_json::{Value, json};
use sha2::{Digest, Sha256};

fn hash(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn identity() -> QualificationIdentity {
    QualificationIdentity {
        aat_version: 2,
        aat_adapter: "fixture-adapter".into(),
        aat_adapter_version: "1.0.0".into(),
        mapping_id: "https://example.test/mapping".into(),
        mapping_version: "1".into(),
        mapping_hash: format!("sha256:{}", "1".repeat(64)),
        mapping_schema_hash: format!("sha256:{}", "2".repeat(64)),
        parser_ir_schema_id: "https://example.test/parser-ir".into(),
        parser_ir_schema_hash: format!("sha256:{}", "3".repeat(64)),
        parser_git_rev: "fixture-revision".into(),
        corpus_snapshot_hash: format!("sha256:{}", "4".repeat(64)),
        corpus_list_hash: format!("sha256:{}", "5".repeat(64)),
        predicate_set_hash: format!("sha256:{}", "6".repeat(64)),
        instrument_versions: BTreeMap::from([(
            "source_accountability".into(),
            "parser-rq-source-accountability-v1".into(),
        )]),
    }
}

fn parser_ir(source: &[u8]) -> Vec<u8> {
    let identity = identity();
    serde_json::to_vec(&json!({
        "schema_id": identity.parser_ir_schema_id,
        "schema_hash": identity.parser_ir_schema_hash,
        "derived_from": {
            "aat_version": identity.aat_version,
            "aat_adapter": identity.aat_adapter,
            "aat_adapter_version": identity.aat_adapter_version,
            "mapping_id": identity.mapping_id,
            "mapping_version": identity.mapping_version,
            "mapping_schema_hash": identity.mapping_schema_hash
        },
        "source": {"work_content_hash": hash(source)},
        "nodes": [{"span": {
            "start": 0,
            "end": source.len(),
            "coordinate_system": "decoded_utf8"
        }}]
    }))
    .unwrap()
}

fn temp_root(label: &str) -> PathBuf {
    let path = std::env::temp_dir().join(format!(
        "parser-rq-fixture-{label}-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&path).unwrap();
    path
}

fn taxonomy_bytes() -> Vec<u8> {
    br#"{"coordinate_system":"decoded_utf8","rules":[],"schema_version":"abc/parser-rq-ignored-regions/v1","taxonomy_version":"parser-rq-ignored-regions-v1"}"#.to_vec()
}

fn capture(root: &Path) -> (String, String, String) {
    let source_root = root.join("source");
    let ir_root = root.join("ir");
    let store = root.join("store");
    fs::create_dir_all(&source_root).unwrap();
    fs::create_dir_all(&ir_root).unwrap();
    fs::create_dir_all(&store).unwrap();
    let works = [("fixture-a", "a.txt", "青空"), ("fixture-b", "b.txt", "海")];
    let entries = works
        .iter()
        .map(|(work_id, source_path, source)| {
            fs::write(source_root.join(source_path), source.as_bytes()).unwrap();
            fs::write(
                ir_root.join(format!("{work_id}.json")),
                parser_ir(source.as_bytes()),
            )
            .unwrap();
            CorpusSourceEntry {
                corpus_entry: CorpusEntry {
                    work_id: (*work_id).into(),
                    original_sha256: hash(source.as_bytes()),
                },
                source_path: (*source_path).into(),
            }
        })
        .collect::<Vec<_>>();
    let taxonomy_bytes = taxonomy_bytes();
    let taxonomy = TaxonomyIdentity {
        taxonomy_version: TaxonomyVersion::V1,
        taxonomy_hash: hash(&taxonomy_bytes),
        taxonomy_jcs_bytes: taxonomy_bytes.clone(),
    };
    let index_path = store.join("index.json");
    let index = analyze_corpus(CorpusInput {
        entries: entries.clone(),
        source_root,
        parser_ir_root: ir_root,
        store_root: store.clone(),
        index_out: index_path.clone(),
        qualification_identity: identity(),
        taxonomy: taxonomy.clone(),
    })
    .unwrap();
    let aggregate = aggregate(
        &entries
            .into_iter()
            .map(|entry| entry.corpus_entry)
            .collect::<Vec<_>>(),
        &index,
        &store,
        &identity(),
        &taxonomy,
    )
    .unwrap();
    let index_bytes = canonical_json(&index).unwrap();
    let aggregate_bytes = canonical_json(&aggregate).unwrap();
    fs::write(store.join("aggregate.json"), &aggregate_bytes).unwrap();
    let identity_bytes = canonical_json(&identity()).unwrap();
    fs::write(store.join("identity.json"), &identity_bytes).unwrap();
    fs::write(store.join("taxonomy.json"), &taxonomy_bytes).unwrap();
    let blob = |locator: &str, bytes: &[u8]| {
        json!({
            "locator": locator,
            "ref": {
                "sha256": hash(bytes),
                "bytes": bytes.len(),
                "media_type": "application/json"
            }
        })
    };
    let manifest = canonical_json(&json!({
        "blobs": [
            blob("aggregate.json", aggregate_bytes.as_bytes()),
            blob("identity.json", identity_bytes.as_bytes()),
            blob("index.json", index_bytes.as_bytes()),
            blob("taxonomy.json", &taxonomy_bytes)
        ],
        "denominator": {"value": 9, "unit": "decoded_utf8_bytes"}
    }))
    .unwrap();
    (index_bytes, aggregate_bytes, manifest)
}

fn fixture(name: &str) -> String {
    fs::read_to_string(fixture_path(name)).unwrap()
}

fn fixture_path(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(3)
        .unwrap()
        .join("abc/test/fixtures/parser-rq/source-accountability")
        .join(name)
}

#[test]
fn capture_derives_byte_identical_witnesses_and_mutation_drifts() {
    let first_root = temp_root("first");
    let second_root = temp_root("second");
    let first = capture(&first_root);
    let second = capture(&second_root);
    assert_eq!(first, second);
    if std::env::var_os("BLESS_PARSER_RQ_FIXTURE").is_some() {
        fs::create_dir_all(fixture_path("index.json").parent().unwrap()).unwrap();
        fs::write(fixture_path("index.json"), &first.0).unwrap();
        fs::write(fixture_path("aggregate.json"), &first.1).unwrap();
        fs::write(fixture_path("manifest.json"), &first.2).unwrap();
    }
    assert_eq!(first.0, fixture("index.json"));
    assert_eq!(first.1, fixture("aggregate.json"));
    assert_eq!(first.2, fixture("manifest.json"));
    let mut mutation: Value = serde_json::from_str(&first.1).unwrap();
    mutation["covered_eligible_bytes"] = json!(8);
    assert_ne!(
        canonical_json(&mutation).unwrap(),
        fixture("aggregate.json")
    );
    fs::remove_dir_all(first_root).unwrap();
    fs::remove_dir_all(second_root).unwrap();
}
