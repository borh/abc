use std::fs;
use std::path::{Path, PathBuf};

use ab_aozora_capture::{CaptureGeneration, capture_generation_from_bytes};
use ab_parser_rq_source_accountability::{
    RecognitionCorpusInput, RecognitionGenerationEntry, RecognitionGenerationIndex,
    RecognitionIndex, RecognitionStatus, RecognitionWorkRecord, aggregate_recognition,
    analyze_recognition_corpus, canonical_json, rfc8785_safe_integer_json,
};
use ab_rq_artifact_store::publish_blob;
use serde_json::{Value, json};
use sha2::{Digest, Sha256};

type ProtocolMutation = (&'static str, fn(&mut Value, &mut Value));

fn hash(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn temp(name: &str) -> PathBuf {
    let path = std::env::temp_dir().join(format!(
        "recognition-corpus-{name}-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&path).unwrap();
    path
}

fn fixture_bytes(name: &str) -> Vec<u8> {
    fs::read(
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../../abc/test/fixtures/parser-rq/classified-source-capture")
            .join(name),
    )
    .unwrap()
}

fn assert_schema(name: &str, value: &impl serde::Serialize) {
    let schema_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../abc/schemas")
        .join(name);
    let schema: Value = serde_json::from_slice(&fs::read(schema_path).unwrap()).unwrap();
    let instance = serde_json::to_value(value).unwrap();
    let validator = jsonschema::validator_for(&schema).unwrap();
    let errors = validator
        .iter_errors(&instance)
        .map(|error| error.to_string())
        .collect::<Vec<_>>();
    assert!(errors.is_empty(), "schema errors: {errors:?}");
}

fn publish_generation(store: &Path, work_id: &str) -> RecognitionGenerationEntry {
    let members = [
        ("decoded.txt", "decoded_source", "txt"),
        ("parser-output.json", "parser_output", "json"),
        ("raw-diagnostics.json", "raw_diagnostics", "json"),
        ("ledger.json", "classified_source_ledger", "json"),
    ];
    let manifest: Value = serde_json::from_slice(&fixture_bytes("generation.json")).unwrap();
    assert_eq!(manifest["work_id"], work_id);
    for (file, key, extension) in members {
        let published = publish_blob(store, extension, &fixture_bytes(file)).unwrap();
        assert_eq!(manifest["members"][key]["artifact_ref"], published.locator);
        assert_eq!(manifest["members"][key]["value_hash"], published.sha256);
    }
    let bytes = fixture_bytes("generation.json");
    let published = publish_blob(store, "json", &bytes).unwrap();
    RecognitionGenerationEntry {
        work_id: work_id.into(),
        sha256: published.sha256,
        bytes: published.bytes,
        media_type: "application/json".into(),
        locator: published.locator,
    }
}

fn publish_capture(store: &Path, generation: &CaptureGeneration) -> RecognitionGenerationEntry {
    let manifest: Value = serde_json::from_slice(&generation.manifest).unwrap();
    let published = generation.publish(store).unwrap();
    RecognitionGenerationEntry {
        work_id: manifest["work_id"].as_str().unwrap().into(),
        sha256: published.manifest.sha256,
        bytes: published.manifest.bytes,
        media_type: "application/json".into(),
        locator: published.manifest.locator,
    }
}

fn reseal_index(index: &mut Value) {
    index["corpus_generation_ref"] = json!(format!("sha256:{}", "0".repeat(64)));
    let mut projection = index.clone();
    projection
        .as_object_mut()
        .unwrap()
        .remove("corpus_generation_ref");
    index["corpus_generation_ref"] = json!(hash(
        rfc8785_safe_integer_json(&projection).unwrap().as_bytes()
    ));
}

fn republish_record(store: &Path, index: &mut Value, record: &Value) {
    let bytes = canonical_json(record).unwrap().into_bytes();
    let blob = publish_blob(store, "json", &bytes).unwrap();
    index["records"][0]["sha256"] = json!(blob.sha256);
    index["records"][0]["bytes"] = json!(blob.bytes);
    index["records"][0]["locator"] = json!(blob.locator);
    reseal_index(index);
}

fn inputs(root: &Path) -> RecognitionCorpusInput {
    let store = root.join("store");
    let manifest: Value = serde_json::from_slice(&fixture_bytes("generation.json")).unwrap();
    let work_id = manifest["work_id"].as_str().unwrap().to_owned();
    let generation = publish_generation(&store, &work_id);
    let membership = json!({
        "schema_version": "abc/parser-rq-source-accountability-index/v1",
        "identity_ref": manifest["qualification_identity_ref"],
        "taxonomy_version": "parser-rq-ignored-regions-v1",
        "taxonomy_hash": format!("sha256:{}", "1".repeat(64)),
        "coordinate_system": "decoded_utf8",
        "status": "ok",
        "expected_work_count": 1,
        "record_count": 1,
        "records": [{
            "work_id": work_id,
            "sha256": format!("sha256:{}", "2".repeat(64)),
            "bytes": 1,
            "media_type": "application/json",
            "locator": "unused.json"
        }],
        "errors": []
    });
    RecognitionCorpusInput {
        membership_index_bytes: canonical_json(&membership).unwrap().into_bytes(),
        generation_index: RecognitionGenerationIndex {
            records: vec![generation],
        },
        store_root: store,
        index_out: root.join("recognition-index.json"),
    }
}

#[test]
fn corpus_is_deterministic_and_aggregate_preserves_work_coordinates() {
    let root = temp("deterministic");
    let input = inputs(&root);
    let first = analyze_recognition_corpus(input.clone()).unwrap();
    let first_bytes = fs::read(&input.index_out).unwrap();
    let second = analyze_recognition_corpus(input.clone()).unwrap();
    assert_eq!(first, second);
    assert_eq!(first_bytes, fs::read(&input.index_out).unwrap());
    assert_eq!(first.status, RecognitionStatus::Ok, "{:?}", first.errors);
    assert_schema("parser-rq-source-recognition-index.schema.json", &first);

    let aggregate = aggregate_recognition(&first, &input.store_root).unwrap();
    assert_eq!(aggregate.status, RecognitionStatus::Ok);
    assert_schema(
        "parser-rq-source-recognition-aggregate.schema.json",
        &aggregate,
    );
    assert!(aggregate.work_completeness.complete);
    assert!(
        aggregate
            .semantic_gaps
            .as_ref()
            .unwrap()
            .iter()
            .all(|gap| { gap.work_id == first.expected_work_ids[0] })
    );
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn exact_membership_and_unavailable_members_fail_closed() {
    for mutation in ["missing", "duplicate", "extra", "blob"] {
        let root = temp(mutation);
        let mut input = inputs(&root);
        match mutation {
            "missing" => input.generation_index.records.clear(),
            "duplicate" => input
                .generation_index
                .records
                .push(input.generation_index.records[0].clone()),
            "extra" => {
                let mut extra = input.generation_index.records[0].clone();
                extra.work_id = "extra".into();
                input.generation_index.records.push(extra);
            }
            "blob" => input.generation_index.records[0].bytes += 1,
            _ => unreachable!(),
        }
        let index = analyze_recognition_corpus(input.clone()).unwrap();
        assert_eq!(index.status, RecognitionStatus::Unavailable, "{mutation}");
        assert!(!index.errors.is_empty());
        let aggregate = aggregate_recognition(&index, &input.store_root).unwrap();
        assert_eq!(aggregate.status, RecognitionStatus::Unavailable);
        assert!(aggregate.eligible_bytes.is_none());
        fs::remove_dir_all(root).unwrap();
    }
}

#[test]
fn unavailable_work_is_published_but_cannot_contribute_aggregate_totals() {
    let root = temp("mixed-unavailable");
    let mut input = inputs(&root);
    let mut membership: Value = serde_json::from_slice(&input.membership_index_bytes).unwrap();
    membership["identity_ref"] = json!(format!("sha256:{}", "9".repeat(64)));
    input.membership_index_bytes = canonical_json(&membership).unwrap().into_bytes();
    let index = analyze_recognition_corpus(input.clone()).unwrap();
    assert_eq!(index.status, RecognitionStatus::Unavailable);
    assert_eq!(index.record_count, 1);
    assert_schema("parser-rq-source-recognition-index.schema.json", &index);
    let aggregate = aggregate_recognition(&index, &input.store_root).unwrap();
    assert_eq!(aggregate.status, RecognitionStatus::Unavailable);
    assert!(aggregate.eligible_bytes.is_none());
    assert_schema(
        "parser-rq-source-recognition-aggregate.schema.json",
        &aggregate,
    );
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn unauthenticated_capture_identity_is_absent_from_closed_index() {
    let root = temp("wrong-generation-ref");
    let mut input = inputs(&root);
    let original = &input.generation_index.records[0];
    let bytes = fs::read(input.store_root.join(&original.locator)).unwrap();
    let mut manifest: Value = serde_json::from_slice(&bytes).unwrap();
    manifest["generation_ref"] = json!(format!("sha256:{}", "f".repeat(64)));
    let mutated = canonical_json(&manifest).unwrap().into_bytes();
    let blob = publish_blob(&input.store_root, "json", &mutated).unwrap();
    input.generation_index.records[0].sha256 = blob.sha256;
    input.generation_index.records[0].bytes = blob.bytes;
    input.generation_index.records[0].locator = blob.locator;

    let index = analyze_recognition_corpus(input.clone()).unwrap();
    assert_eq!(index.status, RecognitionStatus::Unavailable);
    assert_eq!(index.record_count, 0);
    assert!(index.records.is_empty());
    assert_schema("parser-rq-source-recognition-index.schema.json", &index);
    let aggregate = aggregate_recognition(&index, &input.store_root).unwrap();
    assert_eq!(aggregate.status, RecognitionStatus::Unavailable);
    assert_schema(
        "parser-rq-source-recognition-aggregate.schema.json",
        &aggregate,
    );
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn resealed_protocol_mutations_cannot_contribute_aggregate_totals() {
    let cases: &[ProtocolMutation] = &[
        ("index-schema", |index, _| {
            index["schema_version"] = json!("wrong")
        }),
        ("index-coordinate", |index, _| {
            index["coordinate_system"] = json!("wrong")
        }),
        ("index-qualification", |index, _| {
            index["qualification_identity_ref"] = json!("sha256:wrong")
        }),
        ("index-policy", |index, _| {
            index["policy_hash"] = json!("sha256:wrong")
        }),
        ("index-membership", |index, _| {
            index["membership_ref"] = json!("sha256:wrong")
        }),
        ("index-expected-count", |index, _| {
            index["expected_work_count"] = json!(2)
        }),
        ("index-record-count", |index, _| {
            index["record_count"] = json!(0)
        }),
        ("index-entry-work", |index, _| {
            index["records"][0]["work_id"] = json!("another-work")
        }),
        ("entry-capture", |index, _| {
            index["records"][0]["capture_generation_ref"] = json!("sha256:wrong")
        }),
        ("entry-capture-well-formed", |index, _| {
            index["records"][0]["capture_generation_ref"] =
                json!(format!("sha256:{}", "e".repeat(64)))
        }),
        ("entry-media", |index, _| {
            index["records"][0]["media_type"] = json!("text/plain")
        }),
        ("work-schema", |_, record| {
            record["schema_version"] = json!("wrong")
        }),
        ("work-instrument", |_, record| {
            record["instrument_version"] = json!("wrong")
        }),
        ("work-coordinate", |_, record| {
            record["coordinate_system"] = json!("wrong")
        }),
        ("work-qualification", |_, record| {
            record["qualification_identity_ref"] = json!(format!("sha256:{}", "e".repeat(64)))
        }),
        ("work-policy", |_, record| {
            record["policy_hash"] = json!(format!("sha256:{}", "e".repeat(64)))
        }),
        ("work-capture", |_, record| {
            record["capture_generation_ref"] = json!(format!("sha256:{}", "e".repeat(64)))
        }),
        ("work-qualification-absent", |_, record| {
            record
                .as_object_mut()
                .unwrap()
                .remove("qualification_identity_ref");
        }),
        ("work-capture-absent", |_, record| {
            record
                .as_object_mut()
                .unwrap()
                .remove("capture_generation_ref");
        }),
        ("work-policy-absent", |_, record| {
            record.as_object_mut().unwrap().remove("policy_hash");
        }),
        ("work-id-absent", |_, record| {
            record.as_object_mut().unwrap().remove("work_id");
        }),
        ("work-status", |_, record| {
            record["status"] = json!("unavailable");
            record["errors"] = json!(["mutated"]);
        }),
        ("work-ledger-absent", |_, record| {
            record.as_object_mut().unwrap().remove("ledger");
        }),
        ("work-ledger-hash", |_, record| {
            record["ledger"]["sha256"] = json!(format!("sha256:{}", "f".repeat(64)))
        }),
        ("work-ledger-bytes", |_, record| {
            record["ledger"]["bytes"] = json!(record["ledger"]["bytes"].as_u64().unwrap() + 1)
        }),
        ("work-ledger-media", |_, record| {
            record["ledger"]["media_type"] = json!("text/plain")
        }),
        ("work-ledger-locator", |_, record| {
            record["ledger"]["locator"] = json!(
                "sha256/ff/ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff.json"
            )
        }),
    ];
    for (name, mutate) in cases {
        let root = temp(name);
        let input = inputs(&root);
        let valid = analyze_recognition_corpus(input.clone()).unwrap();
        let mut index = serde_json::to_value(&valid).unwrap();
        let record_bytes = fs::read(
            input
                .store_root
                .join(index["records"][0]["locator"].as_str().unwrap()),
        )
        .unwrap();
        let mut record: Value = serde_json::from_slice(&record_bytes).unwrap();
        mutate(&mut index, &mut record);
        if name.starts_with("work-") {
            republish_record(&input.store_root, &mut index, &record);
        } else {
            reseal_index(&mut index);
        }
        let index = serde_json::from_value(index).unwrap();
        let aggregate = aggregate_recognition(&index, &input.store_root).unwrap();
        assert_eq!(aggregate.status, RecognitionStatus::Unavailable, "{name}");
        assert!(aggregate.eligible_bytes.is_none(), "{name}");
        let aggregate_value = serde_json::to_value(&aggregate).unwrap();
        if *name == "index-qualification" {
            assert!(aggregate_value.get("qualification_identity_ref").is_none());
        }
        if *name == "index-policy" {
            assert!(aggregate_value.get("policy_hash").is_none());
        }
        if *name == "index-membership" {
            assert!(aggregate_value.get("membership_ref").is_none());
        }
        assert_schema(
            "parser-rq-source-recognition-aggregate.schema.json",
            &aggregate,
        );
        fs::remove_dir_all(root).unwrap();
    }
}

/// A malformed region set is data to reject, not a reason to abort.
///
/// The three regions are read back from the store, so the aggregate validator
/// is a trust boundary and every one of these shapes is something a corrupted
/// or hostile record can present. Adjacency equalities alone accept several of
/// them: `header 0..10, body 10..5, tail 5..5` satisfies both
/// `header.end == body.start` and `body.end == tail.start` while the body runs
/// backwards, and the endpoint subtraction that follows then underflows.
///
/// Reaching the end of this test IS the assertion for that case. An unchecked
/// subtraction panics the process in a debug build and silently wraps to a
/// nonsense total in a release build, and neither is a verdict.
#[test]
fn malformed_region_sets_are_rejected_rather_than_arithmetic_faults() {
    let cases: &[(&str, Value)] = &[
        (
            "body-inverted",
            json!({"header": {"start": 0, "end": 10},
                   "body": {"start": 10, "end": 5},
                   "tail": {"start": 5, "end": 5}}),
        ),
        (
            "tail-inverted",
            json!({"header": {"start": 0, "end": 0},
                   "body": {"start": 0, "end": 10},
                   "tail": {"start": 10, "end": 5}}),
        ),
        // An inverted header needs `start > end` and the header must start at
        // zero, so `header.start == 0` already covers that shape and there is
        // no separate case for it here.
        (
            "header-does-not-start-at-zero",
            json!({"header": {"start": 4, "end": 10},
                   "body": {"start": 10, "end": 20},
                   "tail": {"start": 20, "end": 20}}),
        ),
        (
            "header-and-body-not-contiguous",
            json!({"header": {"start": 0, "end": 4},
                   "body": {"start": 10, "end": 20},
                   "tail": {"start": 20, "end": 20}}),
        ),
        (
            "body-and-tail-not-contiguous",
            json!({"header": {"start": 0, "end": 4},
                   "body": {"start": 4, "end": 10},
                   "tail": {"start": 14, "end": 20}}),
        ),
        (
            "beyond-the-safe-integer-domain",
            json!({"header": {"start": 0, "end": 0},
                   "body": {"start": 0, "end": 9007199254740992_u64},
                   "tail": {"start": 9007199254740992_u64, "end": 9007199254740992_u64}}),
        ),
    ];
    for (name, regions) in cases {
        let root = temp(&format!("regions-{name}"));
        let input = inputs(&root);
        let valid = analyze_recognition_corpus(input.clone()).unwrap();
        let mut index = serde_json::to_value(&valid).unwrap();
        let record_bytes = fs::read(
            input
                .store_root
                .join(index["records"][0]["locator"].as_str().unwrap()),
        )
        .unwrap();
        let mut record: Value = serde_json::from_slice(&record_bytes).unwrap();
        record["regions"] = regions.clone();
        republish_record(&input.store_root, &mut index, &record);
        let index = serde_json::from_value(index).unwrap();
        let aggregate = aggregate_recognition(&index, &input.store_root).unwrap();
        assert_eq!(aggregate.status, RecognitionStatus::Unavailable, "{name}");
        assert!(aggregate.eligible_bytes.is_none(), "{name}");
        assert_schema(
            "parser-rq-source-recognition-aggregate.schema.json",
            &aggregate,
        );
        fs::remove_dir_all(root).unwrap();
    }
}

#[test]
fn closed_protocol_types_reject_unknown_fields() {
    let root = temp("unknown-fields");
    let input = inputs(&root);
    let index = analyze_recognition_corpus(input.clone()).unwrap();
    let mut index_value = serde_json::to_value(&index).unwrap();
    index_value["unknown"] = json!(true);
    assert!(serde_json::from_value::<RecognitionIndex>(index_value).is_err());

    let record_bytes = fs::read(input.store_root.join(&index.records[0].locator)).unwrap();
    let mut record: Value = serde_json::from_slice(&record_bytes).unwrap();
    record["unknown"] = json!(true);
    assert!(serde_json::from_value::<RecognitionWorkRecord>(record).is_err());
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn zero_work_is_a_valid_exact_fold() {
    let root = temp("zero");
    let membership = json!({
        "schema_version": "abc/parser-rq-source-accountability-index/v1",
        "identity_ref": format!("sha256:{}", "1".repeat(64)),
        "taxonomy_version": "parser-rq-ignored-regions-v1",
        "taxonomy_hash": format!("sha256:{}", "2".repeat(64)),
        "coordinate_system": "decoded_utf8",
        "status": "ok", "expected_work_count": 0, "record_count": 0,
        "records": [], "errors": []
    });
    let input = RecognitionCorpusInput {
        membership_index_bytes: canonical_json(&membership).unwrap().into_bytes(),
        generation_index: RecognitionGenerationIndex { records: vec![] },
        store_root: root.join("store"),
        index_out: root.join("index.json"),
    };
    fs::create_dir_all(&input.store_root).unwrap();
    let index = analyze_recognition_corpus(input.clone()).unwrap();
    assert_eq!(index.status, RecognitionStatus::Ok);
    let aggregate = aggregate_recognition(&index, &input.store_root).unwrap();
    assert_eq!(aggregate.status, RecognitionStatus::Ok);
    assert_eq!(aggregate.eligible_bytes, Some(0));
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn distinct_multi_work_captures_follow_membership_order_and_cannot_be_swapped() {
    let root = temp("multi");
    let store = root.join("store");
    let first = capture_generation_from_bytes("本文一\n".as_bytes()).unwrap();
    let second = capture_generation_from_bytes("本文二｜漢字《かんじ》\n".as_bytes()).unwrap();
    let first_entry = publish_capture(&store, &first);
    let second_entry = publish_capture(&store, &second);
    let manifest_a: Value = serde_json::from_slice(&first.manifest).unwrap();
    let manifest_b: Value = serde_json::from_slice(&second.manifest).unwrap();
    assert_ne!(manifest_a["generation_ref"], manifest_b["generation_ref"]);
    let ids = [second_entry.work_id.clone(), first_entry.work_id.clone()];
    let membership = json!({
        "schema_version": "abc/parser-rq-source-accountability-index/v1",
        "identity_ref": manifest_a["qualification_identity_ref"],
        "taxonomy_version": "parser-rq-ignored-regions-v1",
        "taxonomy_hash": format!("sha256:{}", "1".repeat(64)),
        "coordinate_system": "decoded_utf8", "status": "ok",
        "expected_work_count": 2, "record_count": 2,
        "records": ids.iter().map(|id| json!({
            "work_id": id, "sha256": format!("sha256:{}", "2".repeat(64)),
            "bytes": 1, "media_type": "application/json", "locator": "unused.json"
        })).collect::<Vec<_>>(), "errors": []
    });
    let input = RecognitionCorpusInput {
        membership_index_bytes: canonical_json(&membership).unwrap().into_bytes(),
        generation_index: RecognitionGenerationIndex {
            records: vec![first_entry, second_entry],
        },
        store_root: store,
        index_out: root.join("index.json"),
    };
    let index = analyze_recognition_corpus(input.clone()).unwrap();
    assert_eq!(index.status, RecognitionStatus::Ok, "{:?}", index.errors);
    assert_eq!(index.expected_work_ids, ids);
    assert_eq!(
        index
            .records
            .iter()
            .map(|entry| &entry.work_id)
            .collect::<Vec<_>>(),
        ids.iter().collect::<Vec<_>>()
    );
    assert_ne!(
        index.records[0].capture_generation_ref,
        index.records[1].capture_generation_ref
    );
    let aggregate = aggregate_recognition(&index, &input.store_root).unwrap();
    assert_eq!(aggregate.status, RecognitionStatus::Ok);

    let mut swapped = index.clone();
    let temporary = swapped.records[0].capture_generation_ref.clone();
    swapped.records[0].capture_generation_ref = swapped.records[1].capture_generation_ref.clone();
    swapped.records[1].capture_generation_ref = temporary;
    assert_eq!(
        aggregate_recognition(&swapped, &input.store_root)
            .unwrap()
            .status,
        RecognitionStatus::Unavailable
    );
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn shared_store_rejects_traversal_and_symlink_locators() {
    let root = temp("locator");
    let mut input = inputs(&root);
    input.generation_index.records[0].locator = "../outside.json".into();
    assert_eq!(
        analyze_recognition_corpus(input.clone()).unwrap().status,
        RecognitionStatus::Unavailable
    );
    #[cfg(unix)]
    {
        use std::os::unix::fs::symlink;
        let outside = root.join("outside.json");
        fs::write(&outside, b"{}").unwrap();
        let link = input.store_root.join("manifest-link.json");
        symlink(&outside, &link).unwrap();
        input.generation_index.records[0].locator = "manifest-link.json".into();
        input.generation_index.records[0].sha256 = hash(b"{}");
        input.generation_index.records[0].bytes = 2;
        assert_eq!(
            analyze_recognition_corpus(input).unwrap().status,
            RecognitionStatus::Unavailable
        );
    }
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn rust_matches_every_shared_rfc8785_safe_integer_vector() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join(
        "../../../abc/test/fixtures/canonicalization/rfc8785-safe-integer-domain-abc-v1-vectors.json",
    );
    let fixture: Value = serde_json::from_slice(&fs::read(path).unwrap()).unwrap();
    for vector in fixture["vectors"].as_array().unwrap() {
        let actual = rfc8785_safe_integer_json(&vector["input"]).unwrap();
        assert_eq!(actual, vector["canonical_json"], "{}", vector["name"]);
        assert_eq!(
            hash(actual.as_bytes()).strip_prefix("sha256:").unwrap(),
            vector["sha256"],
            "{}",
            vector["name"]
        );
    }
    for invalid in [
        json!(9007199254740992_u64),
        json!(-9007199254740992_i64),
        json!(1.5),
        json!(1.0),
    ] {
        assert!(rfc8785_safe_integer_json(&invalid).is_err());
    }
}
