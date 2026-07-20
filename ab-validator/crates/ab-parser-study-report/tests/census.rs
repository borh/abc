use ab_parser_study_report::census::derive_census;
use ab_parser_study_report::evidence::{AxisEvidenceRecord, AxisEvidenceSet};
use ab_parser_study_report::{Axis, Candidate, MeasurementMode};

const PREREGISTRATION: &str =
    include_str!("../../../docs/studies/aozora-parser-comparison-preregistration.json");
const POLICY: &str = include_str!("../../../data/parser-study-axis-policy-v1.json");

#[test]
fn census_is_the_exact_frozen_108_row_matrix() {
    let census = derive_census(PREREGISTRATION, POLICY, &AxisEvidenceSet::empty())
        .expect("derive empty-evidence census");
    assert_eq!(census.rows().len(), 108);
    let first = census.rows()[0].key();
    assert_eq!(first.candidate, Candidate::Aozora);
    assert_eq!(first.axis, Axis::ConstructCoverage);
    assert_eq!(first.measurement_mode, MeasurementMode::Native);
}

#[test]
fn diagnostic_and_malformed_robustness_captures_are_mechanical_debt() {
    let census = derive_census(PREREGISTRATION, POLICY, &AxisEvidenceSet::empty())
        .expect("derive empty-evidence census");
    assert!(census.capture_debt().iter().any(|key| {
        key.axis == Axis::Diagnostics && key.artifact == "diagnostic_fixture_capture"
    }));
    assert!(census.capture_debt().iter().any(|key| {
        key.axis == Axis::Robustness && key.artifact == "malformed_fixture_capture"
    }));
}

fn evidence_with_absence_reason(reason: &str) -> AxisEvidenceSet {
    let record: AxisEvidenceRecord = serde_json::from_value(serde_json::json!({
        "schema_id": "https://w3id.org/abc/schemas/parser-study-axis-evidence-v1",
        "schema_version": 1,
        "study_id": "aozora-parser-neutral-comparison-2026-07",
        "candidate": "aozora",
        "axis": "diagnostics",
        "measurement_mode": "native",
        "parser_revision": "revision",
        "adapter_revision": null,
        "corpus_hash": format!("sha256:{}", "0".repeat(64)),
        "required_inputs": [
            {"role": "source_markup", "artifact": "diagnostic_fixture", "state": "absent", "reason": reason},
            {"role": "third_party_capture", "artifact": "diagnostic_fixture_capture", "state": "absent", "reason": reason}
        ],
        "metrics": [{"metric": "diagnostic_presence", "disposition": "unavailable", "reason": reason}],
        "case_witnesses": []
    }))
    .expect("axis evidence record");
    AxisEvidenceSet::from_records(vec![record]).expect("unique evidence")
}

#[test]
fn changing_an_absence_reason_cannot_clear_capture_debt() {
    let first = derive_census(
        PREREGISTRATION,
        POLICY,
        &evidence_with_absence_reason("not run"),
    )
    .expect("first census");
    let second = derive_census(
        PREREGISTRATION,
        POLICY,
        &evidence_with_absence_reason("independent authority unavailable"),
    )
    .expect("second census");
    assert_eq!(first.capture_debt(), second.capture_debt());
}

#[test]
fn excluded_candidate_uses_build_failure_instead_of_fake_capture_debt() {
    let census = derive_census(PREREGISTRATION, POLICY, &AxisEvidenceSet::empty())
        .expect("derive empty-evidence census");
    let row = census
        .rows()
        .iter()
        .find(|row| {
            let key = row.key();
            key.candidate == Candidate::AozoraParserJs && key.axis == Axis::Diagnostics
        })
        .expect("excluded diagnostic row");
    assert!(row.has_preregistered_build_failure());
    assert!(row.required_inputs().iter().all(
        |input| input.role() != ab_parser_study_report::evidence::InputRole::ThirdPartyCapture
    ));
}
