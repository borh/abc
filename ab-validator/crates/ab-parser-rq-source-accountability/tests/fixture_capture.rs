use std::collections::BTreeMap;
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
        instrument_policy_hashes: BTreeMap::from([(
            "source_recognition".into(),
            format!("sha256:{}", "7".repeat(64)),
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
    br#"{"$schema":"https://w3id.org/abc/schemas/parser-rq-ignored-regions.schema.json","coordinate_system":"decoded_utf8","rules":[],"schema_version":"abc/parser-rq-ignored-regions/v1","taxonomy_version":"parser-rq-ignored-regions-v1"}"#.to_vec()
}

fn capture(root: &Path, mutate_source: bool) -> BTreeMap<String, String> {
    let source_root = root.join("source");
    let ir_root = root.join("ir");
    let store = root.join("store");
    fs::create_dir_all(&source_root).unwrap();
    fs::create_dir_all(&ir_root).unwrap();
    fs::create_dir_all(&store).unwrap();
    let works = [
        (
            "fixture-a",
            "a.txt",
            if mutate_source { "青空!" } else { "青空" },
        ),
        ("fixture-b", "b.txt", "海"),
    ];
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
        entries,
        source_root,
        parser_ir_root: ir_root,
        store_root: store.clone(),
        index_out: index_path.clone(),
        qualification_identity: identity(),
        taxonomy: taxonomy.clone(),
    })
    .unwrap();
    let index_bytes = canonical_json(&index).unwrap();
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
            blob("identity.json", identity_bytes.as_bytes()),
            blob("index.json", index_bytes.as_bytes()),
            blob("taxonomy.json", &taxonomy_bytes)
        ],
        "denominator": {"value": 9, "unit": "decoded_utf8_bytes"}
    }))
    .unwrap();
    BTreeMap::from([
        ("identity.json".into(), identity_bytes),
        ("index.json".into(), index_bytes),
        ("manifest.json".into(), manifest),
        (
            "taxonomy.json".into(),
            String::from_utf8(taxonomy_bytes).unwrap(),
        ),
    ])
}

fn fixture(name: &str) -> String {
    fs::read_to_string(fixture_path(name)).unwrap()
}

fn fixture_path(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(2)
        .unwrap()
        .join("research/test/fixtures/parser-rq/source-accountability")
        .join(name)
}

fn verify_committed_witnesses(artifacts: &BTreeMap<String, String>) -> Result<(), Vec<String>> {
    let fixture_root = fixture_path("index.json").parent().unwrap().to_path_buf();
    let committed_names = fs::read_dir(&fixture_root)
        .unwrap()
        .map(|entry| entry.unwrap().file_name().to_string_lossy().into_owned())
        .collect::<std::collections::BTreeSet<_>>();
    let generated_names = artifacts.keys().cloned().collect();
    let mut drift = committed_names
        .symmetric_difference(&generated_names)
        .cloned()
        .collect::<Vec<_>>();
    drift.extend(
        artifacts
            .iter()
            .filter_map(|(name, bytes)| (bytes != &fixture(name)).then_some(name.clone())),
    );
    if drift.is_empty() { Ok(()) } else { Err(drift) }
}

#[test]
fn capture_derives_byte_identical_witnesses_and_mutation_drifts() {
    let first_root = temp_root("first");
    let second_root = temp_root("second");
    let mutation_root = temp_root("mutation");
    let first = capture(&first_root, false);
    let second = capture(&second_root, false);
    assert_eq!(first, second);
    if std::env::var_os("BLESS_PARSER_RQ_FIXTURE").is_some() {
        fs::create_dir_all(fixture_path("index.json").parent().unwrap()).unwrap();
        for (name, bytes) in &first {
            fs::write(fixture_path(name), bytes).unwrap();
        }
    }
    verify_committed_witnesses(&first).unwrap();
    let mutated = capture(&mutation_root, true);
    assert!(verify_committed_witnesses(&mutated).is_err());
    fs::remove_dir_all(first_root).unwrap();
    fs::remove_dir_all(second_root).unwrap();
    fs::remove_dir_all(mutation_root).unwrap();
}
