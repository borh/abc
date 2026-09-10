//! The seam between the two halves of this crate, exercised end to end.
//!
//! `analyze_corpus` produces a membership index; `analyze_recognition_corpus`
//! authenticates against it and records its hash as `membership_ref` in every
//! recognition record. Every other test drives recognition from a hand-written
//! membership index, so nothing else here asserts that a real one is accepted.
//! That is the property the node-span coverage retirement must not break: the
//! coverage quantity leaves the work record, changing the record hashes,
//! the index bytes, and rotating `membership_ref`; the recognition path
//! must keep authenticating across that rotation.

use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

use ab_capture::capture_generation_from_bytes_for_identity_and_work;
use ab_parser_rq_source_accountability::{
    CorpusEntry, CorpusInput, CorpusSourceEntry, QualificationIdentity, RecognitionCorpusInput,
    RecognitionGenerationEntry, RecognitionGenerationIndex, RecognitionStatus, TaxonomyIdentity,
    TaxonomyVersion, analyze_corpus, analyze_recognition_corpus, canonical_json,
    qualification_identity_ref,
};
use serde_json::json;
use sha2::{Digest, Sha256};

const WORKS: [(&str, &str, &str); 2] = [
    ("seam-a", "a.txt", "青空文庫\n底本：テスト\n"),
    ("seam-b", "b.txt", "海の記録\n底本：テスト\n"),
];

fn hash(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn temp_root(label: &str) -> PathBuf {
    let path = std::env::temp_dir().join(format!(
        "parser-rq-seam-{label}-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&path).unwrap();
    path
}

fn identity() -> QualificationIdentity {
    QualificationIdentity {
        aat_version: 2,
        aat_adapter: "seam-adapter".into(),
        aat_adapter_version: "1.0.0".into(),
        mapping_id: "https://example.test/mapping".into(),
        mapping_version: "1".into(),
        mapping_hash: format!("sha256:{}", "1".repeat(64)),
        mapping_schema_hash: format!("sha256:{}", "2".repeat(64)),
        parser_ir_schema_id: "https://example.test/parser-ir".into(),
        parser_ir_schema_hash: format!("sha256:{}", "3".repeat(64)),
        parser_git_rev: "seam-revision".into(),
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

fn taxonomy() -> TaxonomyIdentity {
    let bytes = br#"{"$schema":"https://w3id.org/soranoha/schemas/parser-rq-ignored-regions.schema.json","coordinate_system":"decoded_utf8","rules":[],"schema_version":"abc/parser-rq-ignored-regions/v1","taxonomy_version":"parser-rq-ignored-regions-v1"}"#.to_vec();
    TaxonomyIdentity {
        taxonomy_version: TaxonomyVersion::V1,
        taxonomy_hash: hash(&bytes),
        taxonomy_jcs_bytes: bytes,
    }
}

/// Lay out one campaign capture root and derive its membership index, exactly
/// as the `capture-corpus` subcommand does.
fn membership(root: &Path) -> (PathBuf, PathBuf) {
    let source_root = root.join("source");
    let ir_root = root.join("ir");
    let store = root.join("store");
    for directory in [&source_root, &ir_root, &store] {
        fs::create_dir_all(directory).unwrap();
    }
    let entries = WORKS
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
    let index_out = root.join("source-accountability-index.json");
    let index = analyze_corpus(CorpusInput {
        entries,
        source_root,
        parser_ir_root: ir_root,
        store_root: store.clone(),
        index_out: index_out.clone(),
        qualification_identity: identity(),
        taxonomy: taxonomy(),
    })
    .unwrap();
    assert!(index.errors.is_empty(), "{:?}", index.errors);
    (index_out, store)
}

fn generations(store: &Path, identity_ref: &str) -> RecognitionGenerationIndex {
    RecognitionGenerationIndex {
        records: WORKS
            .iter()
            .map(|(work_id, _, source)| {
                let generation = capture_generation_from_bytes_for_identity_and_work(
                    source.as_bytes(),
                    identity_ref,
                    work_id,
                )
                .unwrap();
                let published = generation.publish(store).unwrap();
                RecognitionGenerationEntry {
                    work_id: (*work_id).into(),
                    sha256: published.manifest.sha256,
                    bytes: published.manifest.bytes,
                    media_type: "application/json".into(),
                    locator: published.manifest.locator,
                }
            })
            .collect(),
    }
}

#[test]
fn a_real_membership_index_authenticates_recognition_and_seals_its_own_bytes() {
    let root = temp_root("authenticates");
    let (index_path, store) = membership(&root);
    let index_bytes = fs::read(&index_path).unwrap();
    let identity_ref = qualification_identity_ref(&identity()).unwrap();

    let recognition = analyze_recognition_corpus(RecognitionCorpusInput {
        membership_index_bytes: index_bytes.clone(),
        generation_index: generations(&store, &identity_ref),
        store_root: store,
        index_out: root.join("source-recognition-index.json"),
    })
    .unwrap();

    assert_eq!(
        recognition.status,
        RecognitionStatus::Ok,
        "{:?}",
        recognition.errors
    );
    // The membership index is accepted only as the exact canonical bytes
    // `analyze_corpus` wrote. A record-shape change moves those bytes, and this
    // is the assertion that ties the two halves together across that move.
    assert_eq!(recognition.membership_ref, hash(&index_bytes));
    assert_eq!(recognition.qualification_identity_ref, identity_ref);
    assert_eq!(
        recognition.expected_work_ids,
        WORKS.iter().map(|(id, _, _)| *id).collect::<Vec<_>>()
    );
    assert_eq!(recognition.record_count, WORKS.len() as u64);
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn membership_bytes_are_rejected_when_reserialized_rather_than_replayed() {
    let root = temp_root("reserialized");
    let (index_path, store) = membership(&root);
    let index_bytes = fs::read(&index_path).unwrap();
    let identity_ref = qualification_identity_ref(&identity()).unwrap();
    let generation_index = generations(&store, &identity_ref);

    // Semantically identical, byte-different: pretty-printed rather than
    // canonical. Recognition must refuse it, because `membership_ref` is a
    // claim about bytes and not about meaning.
    let reserialized = serde_json::to_vec_pretty(
        &serde_json::from_slice::<serde_json::Value>(&index_bytes).unwrap(),
    )
    .unwrap();
    assert_ne!(reserialized, index_bytes);
    assert!(
        analyze_recognition_corpus(RecognitionCorpusInput {
            membership_index_bytes: reserialized,
            generation_index,
            store_root: store,
            index_out: root.join("rejected-index.json"),
        })
        .is_err()
    );
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn the_membership_index_this_seam_rests_on_is_canonical_and_coverage_free() {
    let root = temp_root("shape");
    let (index_path, _) = membership(&root);
    let index_bytes = fs::read(&index_path).unwrap();
    let value: serde_json::Value = serde_json::from_slice(&index_bytes).unwrap();
    assert_eq!(canonical_json(&value).unwrap().as_bytes(), index_bytes);
    // The index carries provenance and content addresses, never a measurement.
    // Retiring node-span coverage therefore changes the record hashes it lists
    // without changing its own shape.
    let mut keys = value.as_object().unwrap().keys().collect::<Vec<_>>();
    keys.sort();
    assert_eq!(
        keys,
        [
            "coordinate_system",
            "errors",
            "expected_work_count",
            "identity_ref",
            "record_count",
            "records",
            "schema_version",
            "status",
            "taxonomy_hash",
            "taxonomy_version",
        ]
    );
    fs::remove_dir_all(root).unwrap();
}
