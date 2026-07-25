use std::fs;
use std::path::{Path, PathBuf};

use ab_aozora_capture::capture_generation_from_bytes_for_identity;
use ab_parser_rq_source_accountability::{
    RecognitionCorpusInput, RecognitionGenerationEntry, RecognitionGenerationIndex,
    RecognitionStatus, aggregate_recognition, analyze_recognition_corpus, canonical_json,
};
use serde_json::{Value, json};
use sha2::{Digest, Sha256};

const QUALIFICATION_IDENTITY_REF: &str =
    "sha256:0cdbd4e18072cf0693cbc2626e29d89bd2fa88d3b2f9fb78c1de173cab1164d2";

fn hash(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn fixture_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .and_then(Path::parent)
        .unwrap()
        .join("abc/test/fixtures/parser-rq/source-recognition-capture")
}

fn identity() -> Value {
    json!({
        "aat_version": 2,
        "aat_adapter": "fixture-adapter",
        "aat_adapter_version": "1.0.0",
        "mapping_id": "https://example.test/mapping",
        "mapping_version": "1",
        "mapping_hash": format!("sha256:{}", "1".repeat(64)),
        "mapping_schema_hash": format!("sha256:{}", "2".repeat(64)),
        "parser_ir_schema_id": "https://example.test/parser-ir",
        "parser_ir_schema_hash": format!("sha256:{}", "3".repeat(64)),
        "parser_git_rev": "fixture-revision",
        "corpus_snapshot_hash": format!("sha256:{}", "4".repeat(64)),
        "corpus_list_hash": format!("sha256:{}", "5".repeat(64)),
        "predicate_set_hash": format!("sha256:{}", "6".repeat(64)),
        "instrument_versions": {
            "source_accountability": "parser-rq-source-accountability-v1",
            "source_span_coverage": "parser-rq-source-recognition-v1"
        }
    })
}

fn publish_fixture(root: &Path) -> (Value, Value, Vec<u8>) {
    let store = root.join("store");
    let identity_bytes = canonical_json(&identity()).unwrap().into_bytes();
    // ABC's published qualification identity uses its frozen historical JCS
    // path (including escaped slashes), so its authoritative ref is supplied
    // to capture rather than inferred with Rust's member-byte canonicalizer.
    let identity_ref = QUALIFICATION_IDENTITY_REF.to_owned();
    let cases = [
        ("clean", "本文\n"),
        ("opaque-unknown", "本文［＃未知］\n"),
        ("recovered-malformed", "［＃tail"),
    ];
    let mut generation_records = Vec::new();
    let mut membership_records = Vec::new();
    let mut capture_blobs = Vec::new();
    for (_, source) in cases {
        let generation =
            capture_generation_from_bytes_for_identity(source.as_bytes(), &identity_ref).unwrap();
        let manifest: Value = serde_json::from_slice(&generation.manifest).unwrap();
        let published = generation.publish(&store).unwrap();
        for blob in [
            &published.decoded_source,
            &published.parser_output,
            &published.raw_diagnostics,
            &published.classified_source_ledger,
            &published.manifest,
        ] {
            capture_blobs.push(json!({
                "locator": blob.locator,
                "ref": {
                    "sha256": blob.sha256,
                    "bytes": blob.bytes,
                    "media_type": if blob.locator.ends_with(".txt") {
                        "text/plain"
                    } else {
                        "application/json"
                    }
                }
            }));
        }
        generation_records.push(RecognitionGenerationEntry {
            work_id: manifest["work_id"].as_str().unwrap().to_owned(),
            sha256: published.manifest.sha256,
            bytes: published.manifest.bytes,
            media_type: "application/json".into(),
            locator: published.manifest.locator,
        });
        let membership_record = canonical_json(&json!({
            "work_id": manifest["work_id"],
            "original_source": {"sha256": manifest["work_id"]}
        }))
        .unwrap()
        .into_bytes();
        let membership_blob = publish_test_blob(&store, "json", &membership_record);
        capture_blobs.push(membership_blob.clone());
        membership_records.push(json!({
            "work_id": manifest["work_id"],
            "sha256": membership_blob["ref"]["sha256"],
            "bytes": membership_blob["ref"]["bytes"],
            "media_type": membership_blob["ref"]["media_type"],
            "locator": membership_blob["locator"]
        }));
    }
    let membership = json!({
        "schema_version": "abc/parser-rq-source-accountability-index/v1",
        "identity_ref": identity_ref,
        "taxonomy_version": "parser-rq-ignored-regions-v1",
        "taxonomy_hash": format!("sha256:{}", "7".repeat(64)),
        "coordinate_system": "decoded_utf8",
        "status": "ok",
        "expected_work_count": 3,
        "record_count": 3,
        "records": membership_records,
        "errors": []
    });
    let membership_bytes = canonical_json(&membership).unwrap().into_bytes();
    let membership_blob = publish_test_blob(&store, "json", &membership_bytes);
    fs::write(root.join("membership-index.json"), &membership_bytes).unwrap();
    fs::write(
        root.join("generation-index.json"),
        canonical_json(&RecognitionGenerationIndex {
            records: generation_records.clone(),
        })
        .unwrap(),
    )
    .unwrap();
    let input = RecognitionCorpusInput {
        membership_index_bytes: membership_bytes.clone(),
        generation_index: RecognitionGenerationIndex {
            records: generation_records,
        },
        store_root: store,
        index_out: root.join("recognition-index.json"),
    };
    let index = analyze_recognition_corpus(input.clone()).unwrap();
    assert_eq!(index.status, RecognitionStatus::Ok, "{:?}", index.errors);
    let aggregate = aggregate_recognition(&index, &input.store_root).unwrap();
    let index_bytes = canonical_json(&index).unwrap().into_bytes();
    let aggregate_bytes = canonical_json(&aggregate).unwrap().into_bytes();
    fs::write(root.join("store/recognition-index.json"), &index_bytes).unwrap();
    fs::write(
        root.join("store/recognition-aggregate.json"),
        &aggregate_bytes,
    )
    .unwrap();
    fs::write(root.join("store/identity.json"), &identity_bytes).unwrap();
    let mut blobs = vec![
        manifest_blob("recognition-index.json", &index_bytes),
        manifest_blob("recognition-aggregate.json", &aggregate_bytes),
        manifest_blob("identity.json", &identity_bytes),
    ];
    blobs.push(membership_blob);
    blobs.extend(capture_blobs);
    for record in &index.records {
        blobs.push(json!({
            "locator": record.locator,
            "ref": {
                "sha256": record.sha256,
                "bytes": record.bytes,
                "media_type": record.media_type
            }
        }));
    }
    blobs.sort_by(|left, right| left["locator"].as_str().cmp(&right["locator"].as_str()));
    blobs.dedup_by(|left, right| left["locator"] == right["locator"]);
    let p0_manifest = json!({
        "blobs": blobs,
        "denominator": {
            "value": aggregate.eligible_bytes.unwrap(),
            "unit": "decoded_utf8_bytes"
        }
    });
    fs::write(
        root.join("manifest.json"),
        canonical_json(&p0_manifest).unwrap(),
    )
    .unwrap();
    (
        serde_json::to_value(index).unwrap(),
        serde_json::to_value(aggregate).unwrap(),
        identity_bytes,
    )
}

fn publish_test_blob(root: &Path, extension: &str, bytes: &[u8]) -> Value {
    let sha256 = hash(bytes);
    let digest = sha256.strip_prefix("sha256:").unwrap();
    let locator = format!("sha256/{}/{}.{}", &digest[..2], digest, extension);
    let path = root.join(&locator);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, bytes).unwrap();
    json!({
        "locator": locator,
        "ref": {
            "sha256": sha256,
            "bytes": bytes.len(),
            "media_type": "application/json"
        }
    })
}

fn manifest_blob(locator: &str, bytes: &[u8]) -> Value {
    json!({
        "locator": locator,
        "ref": {
            "sha256": hash(bytes),
            "bytes": bytes.len(),
            "media_type": "application/json"
        }
    })
}

#[test]
fn production_fixture_covers_clean_opaque_and_recovered_semantics() {
    let root = tempfile_root("semantics");
    let (index, aggregate, _) = publish_fixture(&root);
    assert_eq!(index["record_count"], 3);
    assert!(
        aggregate["recognized_bytes"].as_u64().unwrap()
            < aggregate["accounted_bytes"].as_u64().unwrap()
    );
    assert_eq!(aggregate["accounted_bytes"], aggregate["eligible_bytes"]);
    assert!(aggregate["semantic_gap_bytes"].as_u64().unwrap() > 0);
    let projections = index["records"]
        .as_array()
        .unwrap()
        .iter()
        .map(|entry| {
            let record: Value = serde_json::from_slice(
                &fs::read(root.join("store").join(entry["locator"].as_str().unwrap())).unwrap(),
            )
            .unwrap();
            (
                entry["work_id"].as_str().unwrap().to_owned(),
                (
                    record["eligible_bytes"].as_u64().unwrap(),
                    record["recognized_bytes"].as_u64().unwrap(),
                    record["accounted_bytes"].as_u64().unwrap(),
                ),
            )
        })
        .collect::<std::collections::BTreeMap<_, _>>();
    assert_eq!(
        projections[&hash("本文\n".as_bytes())],
        (7, 7, 7),
        "clean must be fully recognized"
    );
    assert_eq!(
        projections[&hash("本文［＃未知］\n".as_bytes())],
        (22, 7, 22),
        "opaque source form must be accounted but not recognized"
    );
    assert_eq!(
        projections[&hash("［＃tail".as_bytes())],
        (10, 4, 10),
        "recovered malformed input must retain its recovered semantic gap"
    );
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn committed_fixture_is_a_byte_identical_production_regeneration() {
    let first = tempfile_root("first");
    let second = tempfile_root("second");
    let generated_first = publish_fixture(&first);
    let generated_second = publish_fixture(&second);
    assert_eq!(generated_first, generated_second);
    assert_eq!(file_map(&first), file_map(&second));
    if std::env::var_os("UPDATE_RECOGNITION_FIXTURE").is_some() {
        if fixture_root().exists() {
            fs::remove_dir_all(fixture_root()).unwrap();
        }
        copy_tree(&first, &fixture_root());
    }
    assert_eq!(file_map(&fixture_root()), file_map(&first));
    fs::remove_dir_all(first).unwrap();
    fs::remove_dir_all(second).unwrap();
}

fn file_map(root: &Path) -> std::collections::BTreeMap<String, Vec<u8>> {
    fn visit(base: &Path, path: &Path, files: &mut std::collections::BTreeMap<String, Vec<u8>>) {
        for entry in fs::read_dir(path).unwrap() {
            let entry = entry.unwrap();
            if entry.file_type().unwrap().is_dir() {
                visit(base, &entry.path(), files);
            } else {
                files.insert(
                    entry
                        .path()
                        .strip_prefix(base)
                        .unwrap()
                        .to_string_lossy()
                        .into_owned(),
                    fs::read(entry.path()).unwrap(),
                );
            }
        }
    }
    let mut files = std::collections::BTreeMap::new();
    visit(root, root, &mut files);
    files
}

fn copy_tree(source: &Path, destination: &Path) {
    for (relative, bytes) in file_map(source) {
        let path = destination.join(relative);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, bytes).unwrap();
    }
}

fn tempfile_root(name: &str) -> PathBuf {
    let root = std::env::temp_dir().join(format!(
        "recognition-fixture-{name}-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&root).unwrap();
    root
}
