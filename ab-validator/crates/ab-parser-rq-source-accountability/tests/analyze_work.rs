use ab_parser_rq_source_accountability::{
    CorpusEntry, DecodedEncoding, QualificationIdentity, TaxonomyIdentity, TaxonomyVersion,
    WorkInput, WorkRecord, WorkStatus, analyze_work,
};
use serde_json::{Value, json};
use sha2::{Digest, Sha256};

fn hash(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn qualification() -> QualificationIdentity {
    QualificationIdentity {
        parser_git_rev: "abc123".into(),
        aat_version: 2,
        aat_adapter: "ab-aat".into(),
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

fn parser_ir(nodes: Value) -> Vec<u8> {
    let q = qualification();
    let source = CURRENT_SOURCE.with(|source| source.borrow().clone());
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
        "source": {"work_content_hash": hash(&source)},
        "nodes": nodes,
        "paragraphs": [{"span":{"start":0,"end":999,"coordinate_system":"decoded_utf8"}}],
        "sentences": [{"span":{"start":0,"end":999,"coordinate_system":"decoded_utf8"}}]
    }))
    .unwrap()
}

fn input(original: Vec<u8>, nodes: Value) -> WorkInput {
    CURRENT_SOURCE.with(|source| *source.borrow_mut() = original.clone());
    let taxonomy_jcs_bytes = br#"{"coordinate_system":"decoded_utf8","rules":[],"schema_version":"abc/parser-rq-ignored-regions/v1","taxonomy_version":"parser-rq-ignored-regions-v1"}"#.to_vec();
    WorkInput {
        corpus_entry: CorpusEntry {
            work_id: "work-1".into(),
            original_sha256: hash(&original),
        },
        original_bytes: original,
        parser_ir_bytes: parser_ir(nodes),
        qualification_identity: qualification(),
        taxonomy: TaxonomyIdentity {
            taxonomy_version: TaxonomyVersion::V1,
            taxonomy_hash: hash(&taxonomy_jcs_bytes),
            taxonomy_jcs_bytes,
        },
        diagnostics_locator: "work-1.diagnostics.json".into(),
    }
}

thread_local! {
    static CURRENT_SOURCE: std::cell::RefCell<Vec<u8>> = const { std::cell::RefCell::new(Vec::new()) };
}

#[test]
fn lossy_shift_jis_is_unavailable() {
    let result = analyze_work(input(
        vec![0xff, 0x82],
        json!([
            {"span":{"start":0,"end":3,"coordinate_system":"decoded_utf8","line":1,"column":null}}
        ]),
    ));
    assert_eq!(result.record.status, WorkStatus::Unavailable);
    assert_eq!(
        result.record.decoded_source.encoding,
        DecodedEncoding::Windows31jLossy
    );
    assert_eq!(result.record.errors, ["lossy-source-decode"]);
}

#[test]
fn work_record_rejects_unknown_wire_discriminators() {
    let result = analyze_work(input(
        b"x".to_vec(),
        json!([{"span":{"start":0,"end":1,"coordinate_system":"decoded_utf8"}}]),
    ));
    let original = serde_json::to_value(&result.record).unwrap();
    for (pointer, invalid) in [
        (
            "/schema_version",
            "abc/parser-rq-source-accountability-work/v9",
        ),
        ("/instrument_version", "parser-rq-source-accountability-v9"),
        ("/decoded_source/encoding", "utf-16"),
        (
            "/diagnostics/profile",
            "abc/authorized-parser-diagnostics-schema-v3",
        ),
        ("/diagnostics/media_type", "text/plain"),
        ("/taxonomy_version", "parser-rq-ignored-regions-v9"),
        ("/coordinate_system", "raw_bytes"),
        ("/status", "partial"),
    ] {
        let mut changed = original.clone();
        *changed.pointer_mut(pointer).unwrap() = json!(invalid);
        assert!(
            serde_json::from_value::<WorkRecord>(changed).is_err(),
            "accepted {pointer}"
        );
    }
}

#[test]
fn node_spans_no_longer_affect_the_record_at_all() {
    // v1 read `nodes[*].span` and let it decide both a coverage quantity and
    // the record's status. v2 does not read it, so these three parser-IR
    // documents -- no nodes, spans with no declared coordinate, and spans that
    // run past the end of the decoded source -- must now produce equal records.
    // Under v1 the second and third were `unavailable`.
    //
    // `parser_ir` is excluded from the comparison and asserted separately: the
    // three documents genuinely differ, and the record still records which one
    // it saw. That is provenance, which v2 keeps; what it stops doing is
    // drawing a measurement or a verdict out of the spans inside.
    let records = [
        json!([]),
        json!([{"span":{"start":0,"end":1}}]),
        json!([{"span":{"start":0,"end":9_999,"coordinate_system":"raw_bytes"}}]),
    ]
    .map(|nodes| {
        let record = analyze_work(input(b"abc".to_vec(), nodes)).record;
        assert_eq!(record.status, WorkStatus::Ok, "{:?}", record.errors);
        let mut value = serde_json::to_value(&record).unwrap();
        let parser_ir = value["parser_ir"].take();
        (value, parser_ir)
    });
    assert_eq!(records[0].0, records[1].0);
    assert_eq!(records[1].0, records[2].0);
    assert_ne!(records[0].1, records[1].1);
    assert_ne!(records[1].1, records[2].1);
}

#[test]
fn a_v1_record_cannot_be_read_as_v2() {
    // The wire enums are closed with `deny_unknown_fields`, so the version
    // string changing is what makes prior evidence protocol-incompatible
    // rather than silently reinterpretable. Both directions must fail.
    let record = analyze_work(input(
        b"x".to_vec(),
        json!([{"span":{"start":0,"end":1,"coordinate_system":"decoded_utf8"}}]),
    ))
    .record;
    let v2 = serde_json::to_value(&record).unwrap();
    assert_eq!(
        v2["schema_version"],
        "abc/parser-rq-source-accountability-work/v2"
    );

    let mut downgraded = v2.clone();
    downgraded["schema_version"] = json!("abc/parser-rq-source-accountability-work/v1");
    assert!(serde_json::from_value::<WorkRecord>(downgraded).is_err());

    let mut v1_shaped = v2;
    v1_shaped["coverage_basis"] = json!("parser_ir.nodes[*].span");
    v1_shaped["eligible_bytes"] = json!(1);
    assert!(serde_json::from_value::<WorkRecord>(v1_shaped).is_err());
}

#[test]
fn bom_crlf_and_sanitizer_coordinates_address_full_decoded_text() {
    let mut original = vec![0xef, 0xbb, 0xbf];
    original.extend_from_slice("見出し\r\nあ\u{e001}い\r\n底本：本\r\n".as_bytes());
    let decoded = ab_aat::decode_source_bytes(&original).unwrap();
    let post_collision_start = decoded.text.find('\u{e001}').unwrap() + '\u{e001}'.len_utf8();
    let post_collision_end = post_collision_start + "い".len();
    let tail_start = decoded.text.find("底本：").unwrap();
    let result = analyze_work(input(
        original,
        json!([
            {"span":{"start":0,"end":9,"coordinate_system":"decoded_utf8","line":1,"column":null}},
            {"span":{"start":post_collision_start,"end":post_collision_end,"coordinate_system":"decoded_utf8","line":2,"column":null}},
            {"span":{"start":tail_start,"end":tail_start + "底本：本".len(),"coordinate_system":"decoded_utf8","line":3,"column":null}}
        ]),
    ));
    assert_eq!(&decoded.text.as_bytes()[0..9], "見出し".as_bytes());
    assert_eq!(
        &decoded.text.as_bytes()[post_collision_start..post_collision_end],
        "い".as_bytes()
    );
    assert_eq!(
        &decoded.text.as_bytes()[tail_start..tail_start + "底本：本".len()],
        "底本：本".as_bytes()
    );
    assert_eq!(result.record.status, WorkStatus::Ok);
    assert!(
        serde_json::from_slice::<Value>(result.diagnostics_bytes.as_ref().unwrap()).unwrap()["data"]
            .is_array()
    );
}
