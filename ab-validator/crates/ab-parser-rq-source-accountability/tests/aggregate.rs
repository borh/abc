use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use ab_parser_rq_source_accountability::{
    BlobRef, CoordinateSystem, CorpusEntry, CoverageBasis, DecodedBlobRef, DecodedEncoding,
    InstrumentVersion, JsonMediaType, QualificationIdentity, RecordIndex, RecordIndexEntry,
    RecordIndexSchemaVersion, TaxonomyIdentity, TaxonomyVersion, WireInterval, WorkRecord,
    WorkSchemaVersion, WorkStatus, aggregate, canonical_json,
};
use sha2::{Digest, Sha256};

static SEQ: AtomicU64 = AtomicU64::new(0);

fn hash(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn qualification() -> QualificationIdentity {
    QualificationIdentity {
        parser_git_rev: "rev".into(),
        aat_version: 1,
        aat_adapter: "aat".into(),
        aat_adapter_version: "fixture".into(),
        mapping_id: "mapping".into(),
        mapping_version: "1".into(),
        mapping_hash: hash(b"m"),
        mapping_schema_hash: hash(b"ms"),
        parser_ir_schema_id: "ir".into(),
        parser_ir_schema_hash: hash(b"ir"),
        corpus_snapshot_hash: hash(b"snapshot"),
        corpus_list_hash: hash(b"list"),
        predicate_set_hash: hash(b"predicates"),
        instrument_versions: std::collections::BTreeMap::from([(
            "source_accountability".into(),
            "parser-rq-source-accountability-v1".into(),
        )]),
    }
}

fn identity_ref(q: &QualificationIdentity) -> String {
    hash(canonical_json(q).unwrap().as_bytes())
}

fn taxonomy() -> TaxonomyIdentity {
    let bytes = br#"{"coordinate_system":"decoded_utf8","rules":[],"schema_version":"abc/parser-rq-ignored-regions/v1","taxonomy_version":"parser-rq-ignored-regions-v1"}"#.to_vec();
    TaxonomyIdentity {
        taxonomy_version: TaxonomyVersion::V1,
        taxonomy_hash: hash(&bytes),
        taxonomy_jcs_bytes: bytes,
    }
}

fn record(
    id: &str,
    covered: u64,
    eligible: u64,
    q: &QualificationIdentity,
    t: &TaxonomyIdentity,
) -> WorkRecord {
    WorkRecord {
        schema_version: WorkSchemaVersion::V1,
        identity_ref: identity_ref(q),
        instrument_version: InstrumentVersion::V1,
        work_id: id.into(),
        original_source: BlobRef {
            sha256: hash(id.as_bytes()),
            bytes: id.len() as u64,
        },
        decoded_source: DecodedBlobRef {
            sha256: hash(&vec![b'x'; eligible as usize]),
            bytes: eligible,
            encoding: DecodedEncoding::Utf8,
        },
        parser_ir: ab_parser_rq_source_accountability::ParserIrBlobRef {
            schema_id: q.parser_ir_schema_id.clone(),
            schema_hash: q.parser_ir_schema_hash.clone(),
            sha256: hash(b"parser-ir"),
            bytes: 9,
        },
        diagnostics: None,
        taxonomy_version: t.taxonomy_version,
        taxonomy_hash: t.taxonomy_hash.clone(),
        coordinate_system: CoordinateSystem::DecodedUtf8,
        coverage_basis: CoverageBasis::NodeSpans,
        status: WorkStatus::Ok,
        ignored: vec![],
        eligible: vec![WireInterval {
            start: 0,
            end: eligible,
        }],
        covered_eligible: if covered == 0 {
            vec![]
        } else {
            vec![WireInterval {
                start: 0,
                end: covered,
            }]
        },
        uncovered_eligible: if covered == eligible {
            vec![]
        } else {
            vec![WireInterval {
                start: covered,
                end: eligible,
            }]
        },
        decoded_source_bytes: eligible,
        ignored_bytes: 0,
        eligible_bytes: eligible,
        covered_eligible_bytes: covered,
        uncovered_eligible_bytes: eligible - covered,
        errors: vec![],
    }
}

struct Fixture {
    root: PathBuf,
    corpus: Vec<CorpusEntry>,
    index: RecordIndex,
    q: QualificationIdentity,
    t: TaxonomyIdentity,
}
impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.root);
    }
}

fn fixture(spec: &[(&str, u64, u64)]) -> Fixture {
    let q = qualification();
    let t = taxonomy();
    let root = std::env::temp_dir().join(format!(
        "parser-rq-aggregate-{}-{}",
        std::process::id(),
        SEQ.fetch_add(1, Ordering::Relaxed)
    ));
    fs::create_dir_all(&root).unwrap();
    let mut corpus = vec![];
    let mut entries = vec![];
    for &(id, covered, eligible) in spec {
        let rec = record(id, covered, eligible, &q, &t);
        let bytes = canonical_json(&rec).unwrap().into_bytes();
        let locator = format!("{id}.json");
        fs::write(root.join(&locator), &bytes).unwrap();
        corpus.push(CorpusEntry {
            work_id: id.into(),
            original_sha256: hash(id.as_bytes()),
        });
        entries.push(RecordIndexEntry {
            work_id: id.into(),
            sha256: hash(&bytes),
            bytes: bytes.len() as u64,
            media_type: JsonMediaType::ApplicationJson,
            locator,
        });
    }
    let count = entries.len() as u64;
    let index = RecordIndex {
        schema_version: RecordIndexSchemaVersion::V1,
        identity_ref: identity_ref(&q),
        taxonomy_version: t.taxonomy_version,
        taxonomy_hash: t.taxonomy_hash.clone(),
        coordinate_system: CoordinateSystem::DecodedUtf8,
        status: WorkStatus::Ok,
        expected_work_count: count,
        record_count: count,
        records: entries,
        errors: vec![],
    };
    Fixture {
        root,
        corpus,
        index,
        q,
        t,
    }
}

fn run(f: &Fixture) -> ab_parser_rq_source_accountability::AggregateRecord {
    aggregate(&f.corpus, &f.index, &f.root, &f.q, &f.t).unwrap()
}

#[test]
fn aggregate_uses_bytes_not_work_count() {
    let f = fixture(&[("a", 9, 10), ("b", 100, 100)]);
    let result = run(&f);
    assert_eq!(result.covered_eligible_bytes, Some(109));
    assert_eq!(result.eligible_bytes, Some(110));
    assert!(!result.exactly_covered());
}

#[test]
fn aggregate_preserves_work_tagged_uncovered_witnesses() {
    let f = fixture(&[("a", 4, 10)]);
    let result = run(&f);
    assert_eq!(result.uncovered_eligible_bytes, Some(6));
    assert_eq!(result.uncovered.as_ref().unwrap()[0].work_id, "a");
    assert_eq!(
        (
            result.uncovered.as_ref().unwrap()[0].start,
            result.uncovered.as_ref().unwrap()[0].end
        ),
        (4, 10)
    );
}

fn unavailable_without_numbers(result: &ab_parser_rq_source_accountability::AggregateRecord) {
    assert_eq!(result.status, WorkStatus::Unavailable);
    assert!(result.eligible_bytes.is_none());
    assert!(result.covered_eligible_bytes.is_none());
    assert!(result.uncovered_eligible_bytes.is_none());
    assert!(result.uncovered.is_none());
}

#[test]
fn exact_work_set_failures_are_unavailable() {
    let mut missing = fixture(&[("a", 1, 1), ("b", 1, 1)]);
    missing.index.records.pop();
    missing.index.record_count -= 1;
    unavailable_without_numbers(&run(&missing));
    let mut duplicate = fixture(&[("a", 1, 1)]);
    duplicate
        .index
        .records
        .push(duplicate.index.records[0].clone());
    duplicate.index.record_count += 1;
    unavailable_without_numbers(&run(&duplicate));
    let mut extra = fixture(&[("a", 1, 1)]);
    extra.corpus.clear();
    unavailable_without_numbers(&run(&extra));
}

#[test]
fn index_entry_must_name_the_authenticated_record() {
    let mut f = fixture(&[("a", 1, 1), ("b", 1, 1)]);
    f.index.records[0].work_id = "b".into();
    f.index.records[1].work_id = "a".into();
    unavailable_without_numbers(&run(&f));
}

#[test]
fn tampered_hash_or_byte_length_is_unavailable_before_decode() {
    let mut bad_hash = fixture(&[("a", 1, 1)]);
    bad_hash.index.records[0].sha256 = hash(b"other");
    unavailable_without_numbers(&run(&bad_hash));
    let mut bad_len = fixture(&[("a", 1, 1)]);
    bad_len.index.records[0].bytes += 1;
    unavailable_without_numbers(&run(&bad_len));
    let malformed = fixture(&[("a", 1, 1)]);
    fs::write(malformed.root.join("a.json"), b"not json").unwrap();
    unavailable_without_numbers(&run(&malformed));
}

#[test]
fn identity_taxonomy_and_unavailable_work_propagate() {
    let mut identity = fixture(&[("a", 1, 1)]);
    identity.index.identity_ref = hash(b"wrong");
    unavailable_without_numbers(&run(&identity));
    let mut taxonomy = fixture(&[("a", 1, 1)]);
    taxonomy.index.taxonomy_hash = hash(b"wrong");
    unavailable_without_numbers(&run(&taxonomy));
    let mut unavailable = fixture(&[("a", 1, 1)]);
    mutate_record(&mut unavailable, |r| {
        r.status = WorkStatus::Unavailable;
        r.errors = vec!["failed".into()];
    });
    unavailable_without_numbers(&run(&unavailable));
    let mut record_identity = fixture(&[("a", 1, 1)]);
    mutate_record(&mut record_identity, |r| r.identity_ref = hash(b"wrong"));
    unavailable_without_numbers(&run(&record_identity));
    let mut record_taxonomy = fixture(&[("a", 1, 1)]);
    mutate_record(&mut record_taxonomy, |r| r.taxonomy_hash = hash(b"wrong"));
    unavailable_without_numbers(&run(&record_taxonomy));
}

fn mutate_record(f: &mut Fixture, change: impl FnOnce(&mut WorkRecord)) {
    let path = f.root.join(&f.index.records[0].locator);
    let mut record: WorkRecord = serde_json::from_slice(&fs::read(&path).unwrap()).unwrap();
    change(&mut record);
    let bytes = canonical_json(&record).unwrap().into_bytes();
    fs::write(path, &bytes).unwrap();
    f.index.records[0].sha256 = hash(&bytes);
    f.index.records[0].bytes = bytes.len() as u64;
}

#[test]
fn supplied_totals_cannot_override_interval_conservation() {
    let mut f = fixture(&[("a", 1, 2)]);
    mutate_record(&mut f, |r| r.covered_eligible_bytes = 2);
    unavailable_without_numbers(&run(&f));
}

#[test]
fn v1_rejects_forged_ignored_regions() {
    let mut f = fixture(&[("a", 1, 2)]);
    mutate_record(&mut f, |r| {
        r.ignored = vec![WireInterval { start: 0, end: 1 }];
        r.ignored_bytes = 1;
        r.eligible = vec![WireInterval { start: 1, end: 2 }];
        r.eligible_bytes = 1;
        r.covered_eligible = vec![WireInterval { start: 1, end: 2 }];
        r.covered_eligible_bytes = 1;
        r.uncovered_eligible.clear();
        r.uncovered_eligible_bytes = 0;
    });
    unavailable_without_numbers(&run(&f));
}

#[test]
fn v1_rejects_lossy_ok_records() {
    let mut f = fixture(&[("a", 1, 1)]);
    mutate_record(&mut f, |r| {
        r.decoded_source.encoding = DecodedEncoding::Windows31jLossy
    });
    unavailable_without_numbers(&run(&f));
}

#[test]
fn v1_requires_one_full_eligible_interval() {
    let mut split = fixture(&[("a", 2, 2)]);
    mutate_record(&mut split, |r| {
        r.eligible = vec![
            WireInterval { start: 0, end: 1 },
            WireInterval { start: 1, end: 2 },
        ];
    });
    unavailable_without_numbers(&run(&split));

    let mut non_full = fixture(&[("a", 1, 2)]);
    mutate_record(&mut non_full, |r| {
        r.eligible = vec![WireInterval { start: 0, end: 1 }];
        r.eligible_bytes = 1;
        r.ignored = vec![WireInterval { start: 1, end: 2 }];
        r.ignored_bytes = 1;
        r.covered_eligible = vec![WireInterval { start: 0, end: 1 }];
        r.covered_eligible_bytes = 1;
        r.uncovered_eligible.clear();
        r.uncovered_eligible_bytes = 0;
    });
    unavailable_without_numbers(&run(&non_full));
}

#[test]
fn zero_denominator_is_unavailable() {
    let f = fixture(&[("a", 0, 0)]);
    unavailable_without_numbers(&run(&f));
}

#[test]
fn locator_must_stay_below_record_root() {
    let mut f = fixture(&[("a", 1, 1)]);
    f.index.records[0].locator = "../a.json".into();
    assert_eq!(
        run(&f).errors.unwrap(),
        ["record-locator-invalid:a", "zero-eligible-byte-denominator"]
    );
}

#[test]
fn missing_locator_preserves_unavailable_wire_reason() {
    let mut f = fixture(&[("a", 1, 1)]);
    f.index.records[0].locator = "missing.json".into();
    assert_eq!(
        run(&f).errors.unwrap(),
        [
            "record-locator-unavailable:a",
            "zero-eligible-byte-denominator"
        ]
    );
}

#[test]
fn authenticated_mismatch_preserves_blob_mismatch_wire_reason() {
    let mut f = fixture(&[("a", 1, 1)]);
    f.index.records[0].sha256 = hash(b"other");
    assert_eq!(
        run(&f).errors.unwrap(),
        ["record-blob-mismatch:a", "zero-eligible-byte-denominator"]
    );
}

#[cfg(unix)]
#[test]
fn unreadable_record_preserves_read_failed_wire_reason() {
    use std::os::unix::fs::PermissionsExt;

    let f = fixture(&[("a", 1, 1)]);
    let path = f.root.join(&f.index.records[0].locator);
    fs::set_permissions(&path, fs::Permissions::from_mode(0o000)).unwrap();
    let result = run(&f);
    fs::set_permissions(&path, fs::Permissions::from_mode(0o600)).unwrap();
    assert_eq!(
        result.errors.unwrap(),
        ["record-read-failed:a", "zero-eligible-byte-denominator"]
    );
}

#[test]
fn aggregate_wire_records_validate_against_live_schema() {
    let schema_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(3)
        .unwrap()
        .join("abc/schemas/parser-rq-source-accountability-aggregate.schema.json");
    let schema: serde_json::Value =
        serde_json::from_slice(&fs::read(schema_path).unwrap()).unwrap();
    let validator = jsonschema::validator_for(&schema).unwrap();
    let ok = fixture(&[("a", 1, 2)]);
    let mut unavailable = fixture(&[("a", 1, 1)]);
    unavailable.index.records.clear();
    unavailable.index.record_count = 0;
    for record in [run(&ok), run(&unavailable)] {
        let instance = serde_json::to_value(record).unwrap();
        let errors = validator
            .iter_errors(&instance)
            .map(|error| error.to_string())
            .collect::<Vec<_>>();
        assert!(errors.is_empty(), "schema errors: {errors:?}");
    }
}
