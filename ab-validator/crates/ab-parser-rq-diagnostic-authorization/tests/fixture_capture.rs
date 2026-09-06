use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
};

use ab_aozora_capture::capture_generation_from_bytes_for_identity;
use ab_parser_rq_diagnostic_authorization::{
    BoundaryInput, DiagnosticGapAggregateInput, DiagnosticGapExpectedWork, DiagnosticGapWorkInput,
    aggregate_gap_partitions, authorize_boundary, derive_gap_partition,
};
use ab_parser_rq_source_accountability::{
    RecognitionBlobRef, RecognitionCorpusInput, RecognitionGenerationEntry,
    RecognitionGenerationIndex, RecognitionStatus, RecognitionWorkRecord, aggregate_recognition,
    analyze_recognition_corpus, canonical_json,
};
use serde_json::{Value, json};
use sha2::{Digest, Sha256};

const IDENTITY_REF: &str =
    "sha256:6ad6a02c2e9e16e53cfe5cda3786142e58a632a1fc35875334a188721e3bb146";
const POLICY: &[u8] =
    include_bytes!("../../../research/data/parser-rq-ab-aozora-diagnostic-gap-v1.json");

fn hash(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn fixture_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(2)
        .unwrap()
        .join("research/test/fixtures/parser-rq/diagnostic-gap-capture")
}

fn identity() -> Value {
    json!({
        "aat_version":2,"aat_adapter":"fixture-adapter","aat_adapter_version":"1.0.0",
        "mapping_id":"https://example.test/mapping","mapping_version":"1",
        "mapping_hash":format!("sha256:{}", "1".repeat(64)),
        "mapping_schema_hash":format!("sha256:{}", "2".repeat(64)),
        "parser_ir_schema_id":"https://example.test/parser-ir",
        "parser_ir_schema_hash":format!("sha256:{}", "3".repeat(64)),
        "parser_git_rev":"fixture-revision",
        "corpus_snapshot_hash":format!("sha256:{}", "4".repeat(64)),
        "corpus_list_hash":format!("sha256:{}", "5".repeat(64)),
        "predicate_set_hash":format!("sha256:{}", "6".repeat(64)),
        "instrument_versions":{"source_accountability":"parser-rq-source-accountability-v1",
                               "source_span_coverage":"parser-rq-source-recognition-v4",
                               "metadata_attribution":"parser-rq-source-recognition-v4"},
        "instrument_policy_hashes":{"source_recognition":format!("sha256:{}", "7".repeat(64))}
    })
}

fn publish(root: &Path, ext: &str, media: &str, bytes: &[u8]) -> Value {
    let sha = hash(bytes);
    let digest = &sha[7..];
    let locator = format!("sha256/{}/{}.{}", &digest[..2], digest, ext);
    let path = root.join(&locator);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    if path.exists() {
        assert_eq!(fs::read(&path).unwrap(), bytes);
    } else {
        fs::write(&path, bytes).unwrap();
    }
    json!({"locator":locator,"ref":{"sha256":sha,"bytes":bytes.len(),"media_type":media}})
}

fn interval_json(values: &[ab_parser_rq_diagnostic_authorization::Interval]) -> Value {
    Value::Array(
        values
            .iter()
            .map(|v| json!({"start":v.start,"end":v.end}))
            .collect(),
    )
}

fn result_json(result: &ab_parser_rq_diagnostic_authorization::DiagnosticGapWorkResult) -> Value {
    let source = result.source_recognition_evidence.as_ref().unwrap();
    let auth = result.diagnostic_authorization_evidence.as_ref().unwrap();
    json!({
      "schema_version":"abc/parser-rq-diagnostic-gap-result/v1","status":"ok",
      "work_id":result.work_id,"capture_generation_ref":result.capture_generation_ref,
      "qualification_identity_ref":result.qualification_identity_ref,"policy_hash":result.policy_hash,
      "source_recognition_evidence":{"relation":source.relation,"artifact_ref":{
        "sha256":source.artifact_ref.sha256,"bytes":source.artifact_ref.bytes,
        "media_type":source.artifact_ref.media_type,"locator":source.artifact_ref.locator},
        "value_hash":source.value_hash,"qualification_identity_ref":source.qualification_identity_ref,
        "capture_generation_ref":source.capture_generation_ref,"work_id":source.work_id},
      "diagnostic_authorization_evidence":{"decoded_source_hash":auth.decoded_source_hash,
        "raw_diagnostics_hash":auth.raw_diagnostics_hash,"raw_diagnostics_bytes":auth.raw_diagnostics_bytes,
        "policy_hash":auth.policy_hash,"policy_artifact_hash":auth.policy_artifact_hash,
        "policy_artifact_bytes":auth.policy_artifact_bytes,"source_recognition_hash":auth.source_recognition_hash},
      "authorized_intervals":interval_json(result.authorized_intervals.as_ref().unwrap()),
      "silent_intervals":interval_json(result.silent_intervals.as_ref().unwrap()),
      "authorized_bytes":result.authorized_bytes,"silent_bytes":result.silent_bytes,
      "silent_drop_count":result.silent_drop_count,"diagnostic_count":result.diagnostic_count,
      "authorizing_diagnostic_count":result.authorizing_diagnostic_count,
      "observe_only_diagnostic_count":result.observe_only_diagnostic_count,"vacuous":result.vacuous
    })
}

fn aggregate_json(value: &ab_parser_rq_diagnostic_authorization::DiagnosticGapAggregate) -> Value {
    json!({"schema_version":value.schema_version,"status":"ok",
      "qualification_identity_ref":value.qualification_identity_ref,
      "corpus_generation_ref":value.corpus_generation_ref,"policy_hash":value.policy_hash,
      "policy_artifact_hash":value.policy_artifact_hash,
      "expected_work_ids":value.expected_work_ids,"observed_work_ids":value.observed_work_ids,
      "authorized_bytes":value.authorized_bytes,"silent_bytes":value.silent_bytes,
      "silent_drop_count":value.silent_drop_count,"diagnostic_count":value.diagnostic_count,
      "authorizing_diagnostic_count":value.authorizing_diagnostic_count,
      "observe_only_diagnostic_count":value.observe_only_diagnostic_count,
      "authorized_interval_count":value.authorized_interval_count,"vacuous":value.vacuous})
}

fn generate(root: &Path) -> BTreeMap<String, String> {
    let store = root.join("store");
    fs::create_dir_all(&store).unwrap();
    let cases = [
        ("clean-vacuous", "本文\n"),
        ("authorized-pua", "\u{e001}"),
        ("observe-only", "［＃改ページ"),
        ("opaque-unknown", "本文［＃未知］\n"),
        ("silent-gap", "［＃tail"),
    ];
    let mut generations = vec![];
    let mut membership = vec![];
    let mut blobs = vec![];
    let mut captured = BTreeMap::new();
    let mut outcomes = BTreeMap::new();
    for (name, source) in cases {
        let generation =
            capture_generation_from_bytes_for_identity(source.as_bytes(), IDENTITY_REF).unwrap();
        let manifest: Value = serde_json::from_slice(&generation.manifest).unwrap();
        let work_id = manifest["work_id"].as_str().unwrap().to_owned();
        let published = generation.publish(&store).unwrap();
        for (blob, media) in [
            (&published.decoded_source, "text/plain"),
            (&published.parser_output, "application/json"),
            (&published.raw_diagnostics, "application/json"),
            (&published.classified_source_ledger, "application/json"),
            (&published.manifest, "application/json"),
        ] {
            blobs.push(json!({"locator":blob.locator,"ref":{"sha256":blob.sha256,"bytes":blob.bytes,"media_type":media}}));
        }
        generations.push(RecognitionGenerationEntry {
            work_id: work_id.clone(),
            sha256: published.manifest.sha256,
            bytes: published.manifest.bytes,
            media_type: "application/json".into(),
            locator: published.manifest.locator,
        });
        let membership_record = canonical_json(&json!({
            "work_id": work_id,
            "original_source": {"sha256": work_id}
        }))
        .unwrap()
        .into_bytes();
        let membership_blob = publish(&store, "json", "application/json", &membership_record);
        membership.push(json!({
            "work_id":work_id,
            "sha256":membership_blob["ref"]["sha256"],
            "bytes":membership_blob["ref"]["bytes"],
            "media_type":membership_blob["ref"]["media_type"],
            "locator":membership_blob["locator"]
        }));
        blobs.push(membership_blob);
        captured.insert(
            work_id.clone(),
            (generation.decoded_source, generation.raw_diagnostics),
        );
        outcomes.insert(name.to_owned(), work_id);
    }
    let membership_value = json!({"schema_version":"abc/parser-rq-source-accountability-index/v1",
      "identity_ref":IDENTITY_REF,"taxonomy_version":"parser-rq-ignored-regions-v1",
      "taxonomy_hash":format!("sha256:{}","7".repeat(64)),"coordinate_system":"decoded_utf8",
      "status":"ok","expected_work_count":5,"record_count":5,"records":membership,"errors":[]});
    let membership_bytes = canonical_json(&membership_value).unwrap().into_bytes();
    let membership_blob = publish(&store, "json", "application/json", &membership_bytes);
    blobs.push(membership_blob);
    let input = RecognitionCorpusInput {
        membership_index_bytes: membership_bytes,
        generation_index: RecognitionGenerationIndex {
            records: generations,
        },
        store_root: store.clone(),
        index_out: root.join("recognition-index.json"),
    };
    let index = analyze_recognition_corpus(input.clone()).unwrap();
    assert_eq!(index.status, RecognitionStatus::Ok);
    let r1_aggregate = aggregate_recognition(&index, &store).unwrap();
    let index_bytes = canonical_json(&index).unwrap().into_bytes();
    let aggregate_bytes = canonical_json(&r1_aggregate).unwrap().into_bytes();
    fs::write(store.join("recognition-index.json"), &index_bytes).unwrap();
    fs::write(store.join("recognition-aggregate.json"), &aggregate_bytes).unwrap();
    blobs.push(json!({"locator":"recognition-index.json","ref":{"sha256":hash(&index_bytes),"bytes":index_bytes.len(),"media_type":"application/json"}}));
    blobs.push(json!({"locator":"recognition-aggregate.json","ref":{"sha256":hash(&aggregate_bytes),"bytes":aggregate_bytes.len(),"media_type":"application/json"}}));
    let policy_blob = publish(&store, "json", "application/json", POLICY);
    blobs.push(policy_blob);
    let mut results = vec![];
    let mut expected = vec![];
    let mut observed = BTreeMap::new();
    for entry in &index.records {
        let record_bytes = fs::read(store.join(&entry.locator)).unwrap();
        let record: RecognitionWorkRecord = serde_json::from_slice(&record_bytes).unwrap();
        let (decoded, raw) = &captured[&entry.work_id];
        let auth = authorize_boundary(BoundaryInput {
            raw_diagnostics: raw,
            raw_diagnostics_hash: &hash(raw),
            raw_diagnostics_bytes: raw.len() as u64,
            policy_bytes: POLICY,
            policy_bytes_hash: &hash(POLICY),
            decoded_source: decoded,
            decoded_source_hash: &hash(decoded),
            work_id: &entry.work_id,
            capture_generation_ref: &entry.capture_generation_ref,
            qualification_identity_ref: IDENTITY_REF,
            source_recognition_bytes: &record_bytes,
            source_recognition_hash: &hash(&record_bytes),
            source_recognition: &record,
        });
        let result = derive_gap_partition(DiagnosticGapWorkInput {
            source_recognition: &record,
            source_recognition_bytes: &record_bytes,
            source_recognition_artifact_ref: RecognitionBlobRef {
                sha256: entry.sha256.clone(),
                bytes: entry.bytes,
                media_type: entry.media_type.clone(),
                locator: entry.locator.clone(),
            },
            source_recognition_value_hash: entry.sha256.clone(),
            authorization: &auth,
        });
        assert_eq!(
            result.status,
            ab_parser_rq_diagnostic_authorization::DiagnosticGapWorkStatus::Ok,
            "{}",
            result.errors.join(",")
        );
        let name = outcomes
            .iter()
            .find_map(|(name, id)| (id == &entry.work_id).then_some(name.clone()))
            .unwrap();
        observed.insert(
            name,
            (
                result.authorized_bytes.unwrap(),
                result.silent_bytes.unwrap(),
                result.diagnostic_count.unwrap(),
                result.authorizing_diagnostic_count.unwrap(),
                result.observe_only_diagnostic_count.unwrap(),
                result.vacuous.unwrap(),
            ),
        );
        expected.push(DiagnosticGapExpectedWork {
            work_id: entry.work_id.clone(),
            capture_generation_ref: entry.capture_generation_ref.clone(),
            source_recognition_value_hash: entry.sha256.clone(),
        });
        let bytes = canonical_json(&result_json(&result)).unwrap().into_bytes();
        blobs.push(publish(&store, "json", "application/json", &bytes));
        results.push(result);
    }
    let aggregate = aggregate_gap_partitions(DiagnosticGapAggregateInput {
        expected_works: &expected,
        qualification_identity_ref: IDENTITY_REF,
        corpus_generation_ref: &index.corpus_generation_ref,
        policy_hash: results[0].policy_hash.as_deref().unwrap(),
        works: &results,
    });
    assert_eq!(observed["clean-vacuous"], (0, 0, 0, 0, 0, true));
    assert_eq!(observed["authorized-pua"], (3, 0, 1, 1, 0, false));
    assert_eq!(observed["observe-only"], (0, 6, 1, 0, 1, false));
    assert_eq!(observed["opaque-unknown"], (0, 15, 0, 0, 0, true));
    assert_eq!(observed["silent-gap"], (0, 6, 1, 0, 1, false));
    let gap_bytes = canonical_json(&aggregate_json(&aggregate))
        .unwrap()
        .into_bytes();
    fs::write(store.join("diagnostic-gap-aggregate.json"), &gap_bytes).unwrap();
    blobs.push(json!({"locator":"diagnostic-gap-aggregate.json","ref":{"sha256":hash(&gap_bytes),"bytes":gap_bytes.len(),"media_type":"application/json"}}));
    let identity_bytes = canonical_json(&identity()).unwrap().into_bytes();
    fs::write(store.join("identity.json"), &identity_bytes).unwrap();
    blobs.push(json!({"locator":"identity.json","ref":{"sha256":hash(&identity_bytes),"bytes":identity_bytes.len(),"media_type":"application/json"}}));
    for entry in &index.records {
        blobs.push(json!({"locator":entry.locator,"ref":{"sha256":entry.sha256,"bytes":entry.bytes,"media_type":entry.media_type}}));
    }
    blobs.sort_by(|a, b| a["locator"].as_str().cmp(&b["locator"].as_str()));
    blobs.dedup_by(|a, b| a["locator"] == b["locator"]);
    let fixture = json!({"outcomes":outcomes,"aggregate":{"silent_drop_count":aggregate.silent_drop_count,
      "diagnostic_count":aggregate.diagnostic_count,"vacuous":aggregate.vacuous}});
    fs::write(
        root.join("expected-outcomes.json"),
        canonical_json(&fixture).unwrap(),
    )
    .unwrap();
    fs::write(
        root.join("manifest.json"),
        canonical_json(&json!({"blobs":blobs,
      "denominator":{"value":r1_aggregate.eligible_bytes,"unit":"decoded_utf8_bytes"}}))
        .unwrap(),
    )
    .unwrap();
    outcomes
}

fn file_map(root: &Path) -> BTreeMap<String, Vec<u8>> {
    fn walk(base: &Path, p: &Path, out: &mut BTreeMap<String, Vec<u8>>) {
        for e in fs::read_dir(p).unwrap() {
            let e = e.unwrap();
            if e.file_type().unwrap().is_dir() {
                walk(base, &e.path(), out)
            } else {
                out.insert(
                    e.path()
                        .strip_prefix(base)
                        .unwrap()
                        .to_string_lossy()
                        .into_owned(),
                    fs::read(e.path()).unwrap(),
                );
            }
        }
    }
    let mut out = BTreeMap::new();
    walk(root, root, &mut out);
    out
}
fn temp(name: &str) -> PathBuf {
    let p = std::env::temp_dir().join(format!("diagnostic-gap-{name}-{}", std::process::id()));
    let _ = fs::remove_dir_all(&p);
    fs::create_dir_all(&p).unwrap();
    p
}

#[test]
fn production_fixture_is_deterministic_and_binds_each_outcome_to_a_work() {
    let first = temp("first");
    let second = temp("second");
    let outcomes = generate(&first);
    assert_eq!(outcomes, generate(&second));
    assert_eq!(file_map(&first), file_map(&second));
    assert_eq!(outcomes.len(), 5);
    assert_eq!(
        outcomes
            .values()
            .collect::<std::collections::BTreeSet<_>>()
            .len(),
        5
    );
    if std::env::var_os("UPDATE_DIAGNOSTIC_GAP_FIXTURE").is_some() {
        let _ = fs::remove_dir_all(fixture_root());
        fs::create_dir_all(fixture_root()).unwrap();
        for (name, bytes) in file_map(&first) {
            let p = fixture_root().join(name);
            fs::create_dir_all(p.parent().unwrap()).unwrap();
            fs::write(p, bytes).unwrap();
        }
    }
    assert_eq!(file_map(&fixture_root()), file_map(&first));
    let _ = fs::remove_dir_all(first);
    let _ = fs::remove_dir_all(second);
}
