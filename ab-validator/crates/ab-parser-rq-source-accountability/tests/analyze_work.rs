use ab_parser_rq_source_accountability::{
    AdapterCoordinates, CorpusEntry, QualificationIdentity, TaxonomyIdentity, WorkInput,
    analyze_work,
};
use serde_json::{Value, json};
use sha2::{Digest, Sha256};

fn hash(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
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

fn parser_ir(nodes: Value) -> Vec<u8> {
    let q = qualification();
    let source = CURRENT_SOURCE.with(|source| source.borrow().clone());
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
            taxonomy_version: "parser-rq-ignored-regions-v1".into(),
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
fn lossy_shift_jis_is_unavailable_even_when_span_covers_replacement() {
    let result = analyze_work(input(
        vec![0xff, 0x82],
        json!([
            {"span":{"start":0,"end":3,"coordinate_system":"decoded_utf8","line":1,"column":null}}
        ]),
    ));
    assert_eq!(result.record.status, "unavailable");
    assert_eq!(result.record.errors, ["lossy-source-decode"]);
}

#[test]
fn absent_coordinate_system_is_unavailable() {
    let result = analyze_work(input(b"x".to_vec(), json!([{"span":{"start":0,"end":1}}])));
    assert_eq!(result.record.status, "unavailable");
    assert_eq!(
        result.record.errors,
        ["node-span-coordinate-system-missing"]
    );
}

#[test]
fn paragraph_and_sentence_spans_do_not_cover_missing_nodes() {
    let result = analyze_work(input(b"abc".to_vec(), json!([])));
    assert_eq!(result.record.covered_eligible_bytes, 0);
    assert_eq!(result.record.uncovered_eligible_bytes, 3);
}

#[test]
fn nested_node_spans_count_union_bytes_once() {
    let result = analyze_work(input(
        b"0123456789".to_vec(),
        json!([
            {"span":{"start":0,"end":10,"coordinate_system":"decoded_utf8","line":1,"column":null}},
            {"span":{"start":2,"end":4,"coordinate_system":"decoded_utf8","line":1,"column":null}}
        ]),
    ));
    assert_eq!(result.record.status, "ok");
    assert_eq!(result.record.covered_eligible_bytes, 10);
    assert_eq!(result.record.uncovered_eligible_bytes, 0);
}

#[test]
fn bom_crlf_and_sanitizer_coordinates_address_full_decoded_text() {
    let mut original = vec![0xef, 0xbb, 0xbf];
    original.extend_from_slice("見出し\r\nあ\u{e001}い\r\n底本：本\r\n".as_bytes());
    let decoded = ab_aozora_aat::decode_source_bytes(&original).unwrap();
    let tail_start = decoded.text.find("底本：").unwrap();
    let result = analyze_work(input(
        original,
        json!([
            {"span":{"start":0,"end":9,"coordinate_system":"decoded_utf8","line":1,"column":null}},
            {"span":{"start":tail_start,"end":tail_start + "底本：本".len(),"coordinate_system":"decoded_utf8","line":3,"column":null}}
        ]),
    ));
    assert_eq!(&decoded.text.as_bytes()[0..9], "見出し".as_bytes());
    assert_eq!(
        &decoded.text.as_bytes()[tail_start..tail_start + "底本：本".len()],
        "底本：本".as_bytes()
    );
    assert_eq!(result.record.status, "ok");
    assert!(serde_json::from_slice::<Value>(&result.diagnostics_bytes).unwrap()["data"].is_array());
}
