use std::collections::BTreeMap;

use ab_parser_rq_source_accountability::{
    QualificationIdentity, canonical_json, qualification_identity_ref,
};

#[test]
fn qualification_identity_matches_p0_flat_wire_and_golden_ref() {
    let identity = QualificationIdentity {
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
    };
    let bytes = canonical_json(&identity).unwrap();
    assert!(!bytes.contains("adapter_coordinates"));
    assert!(bytes.contains("\"instrument_versions\":{\"source_accountability\""));
    assert_eq!(
        qualification_identity_ref(&identity).unwrap(),
        "sha256:45b662893c840cdb68647baa9c2af48fdbc7dae14cb064f903d35c939e220088"
    );
}
