use ab_aat_to_parser_ir::qualification::{
    QualificationAggregateStatus, QualificationStatus, QualificationWorkRecord,
    aggregate_work_records,
};
use hegel::generators;

const HASH: &str = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";

fn record(work_id: &str, status: QualificationStatus) -> QualificationWorkRecord {
    let mut value = serde_json::json!({
        "schema_id": "https://w3id.org/abc/schemas/parser-rq-parser-ir-conformance-work.schema.json",
        "schema_version": "1.0.0",
        "work_id": work_id,
        "qualification_identity_ref": HASH,
        "policy_hash": HASH,
        "status": status,
    });
    match status {
        QualificationStatus::SchemaValid => {
            value["parser_ir"] = blob();
        }
        QualificationStatus::SchemaInvalid => {
            value["parser_ir"] = blob();
            value["validation_ledger"] = blob();
            value["validation_witnesses"] = serde_json::json!(["invalid"]);
        }
        QualificationStatus::NoOutput => {
            value["failure"] = serde_json::json!({
                "kind": "conversion_failed",
                "message": "no output"
            });
        }
        QualificationStatus::Unavailable => {
            value["reason"] = serde_json::json!("blob_unavailable");
        }
    }
    serde_json::from_value(value).unwrap()
}

fn blob() -> serde_json::Value {
    serde_json::json!({"sha256": HASH, "bytes": 2, "media_type": "application/json"})
}

fn generated_records(tc: &hegel::TestCase) -> (Vec<String>, Vec<QualificationWorkRecord>) {
    let ids: Vec<u16> = tc.draw(
        generators::vecs(generators::integers::<u16>())
            .min_size(1)
            .max_size(30)
            .unique(true),
    );
    let work_ids: Vec<String> = ids.into_iter().map(|id| format!("w{id}")).collect();
    let records = work_ids
        .iter()
        .map(|work_id| {
            let status = tc.draw(generators::sampled_from(vec![
                QualificationStatus::SchemaValid,
                QualificationStatus::SchemaInvalid,
                QualificationStatus::NoOutput,
            ]));
            record(work_id, status)
        })
        .collect();
    (work_ids, records)
}

#[hegel::test]
fn aggregate_is_permutation_invariant(tc: hegel::TestCase) {
    let (work_ids, mut records) = generated_records(&tc);
    let rotation = tc.draw(generators::integers::<usize>()) % records.len();
    let expected = aggregate_work_records(&work_ids, &records).unwrap();
    records.rotate_left(rotation);
    assert_eq!(
        expected,
        aggregate_work_records(&work_ids, &records).unwrap()
    );
}

#[hegel::test]
fn aggregate_rejects_every_membership_mutation(tc: hegel::TestCase) {
    let (work_ids, records) = generated_records(&tc);
    let mut omitted = records.clone();
    omitted.pop();
    assert!(aggregate_work_records(&work_ids, &omitted).is_err());

    let mut duplicated = records.clone();
    duplicated.push(records[0].clone());
    assert!(aggregate_work_records(&work_ids, &duplicated).is_err());

    let mut extra = records;
    extra.push(record("extra", QualificationStatus::SchemaValid));
    assert!(aggregate_work_records(&work_ids, &extra).is_err());
}

#[hegel::test]
fn aggregate_counts_match_an_independent_model(tc: hegel::TestCase) {
    let (work_ids, records) = generated_records(&tc);
    let aggregate = aggregate_work_records(&work_ids, &records).unwrap();
    let valid = records
        .iter()
        .filter(|record| record.status == QualificationStatus::SchemaValid)
        .count() as u64;
    let invalid = records
        .iter()
        .filter(|record| record.status == QualificationStatus::SchemaInvalid)
        .count() as u64;
    let no_output = records
        .iter()
        .filter(|record| record.status == QualificationStatus::NoOutput)
        .count() as u64;
    assert_eq!(aggregate.expected_works, work_ids.len() as u64);
    assert_eq!(aggregate.schema_valid_outputs, valid);
    assert_eq!(aggregate.schema_invalid_outputs, invalid);
    assert_eq!(aggregate.generated_outputs, valid + invalid);
    assert_eq!(aggregate.no_output_works, no_output);
}

#[hegel::test]
fn replacing_valid_with_invalid_never_improves_the_ratio(tc: hegel::TestCase) {
    let (work_ids, mut records) = generated_records(&tc);
    records[0] = record(&work_ids[0], QualificationStatus::SchemaValid);
    let before = aggregate_work_records(&work_ids, &records)
        .unwrap()
        .parser_ir_schema_validation
        .unwrap();
    records[0] = record(&work_ids[0], QualificationStatus::SchemaInvalid);
    let after = aggregate_work_records(&work_ids, &records)
        .unwrap()
        .parser_ir_schema_validation
        .unwrap();
    assert!(after <= before);
}

#[test]
fn all_no_output_is_a_typed_available_failure() {
    let work_ids = vec!["a".to_owned(), "b".to_owned()];
    let records = vec![
        record("a", QualificationStatus::NoOutput),
        record("b", QualificationStatus::NoOutput),
    ];
    let aggregate = aggregate_work_records(&work_ids, &records).unwrap();
    assert_eq!(
        aggregate.status,
        QualificationAggregateStatus::NoParserIrOutput
    );
    assert_eq!(aggregate.parser_ir_schema_validation, None);
}

#[test]
fn unavailable_work_makes_the_aggregate_unavailable() {
    let work_ids = vec!["a".to_owned()];
    let aggregate =
        aggregate_work_records(&work_ids, &[record("a", QualificationStatus::Unavailable)])
            .unwrap();
    assert_eq!(aggregate.status, QualificationAggregateStatus::Unavailable);
    assert_eq!(aggregate.parser_ir_schema_validation, None);
}
