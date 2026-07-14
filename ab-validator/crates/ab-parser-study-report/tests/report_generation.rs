//! Generation + drift contract: reports are a deterministic, provenance-tied
//! function of the committed raw run manifests and the frozen preregistration.

use std::fs;

use ab_parser_study_report::generate::{GeneratedReports, generate_reports};
use ab_parser_study_report::{Axis, Candidate, MeasurementMode, RowStatus, StudyReport};

const RUN_MANIFESTS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../reports/parser-study/runs/aozora-parser-neutral-comparison-2026-07/run-manifests.json"
));

const PREREGISTRATION: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../docs/studies/aozora-parser-comparison-preregistration.json"
));

const COMMITTED_MACHINE_REPORT: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../reports/parser-study/reports/aozora-parser-neutral-comparison-2026-07/comparison-result.json"
);

const COMMITTED_NARRATIVE_REPORT: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../reports/parser-study/reports/aozora-parser-neutral-comparison-2026-07/comparison-report.md"
);

fn generate() -> GeneratedReports {
    generate_reports(RUN_MANIFESTS, PREREGISTRATION).expect("generation succeeds")
}

#[test]
fn generation_is_byte_identical_across_runs() {
    let first = generate();
    let second = generate();
    assert_eq!(
        first.machine_json, second.machine_json,
        "machine report must regenerate byte-identically"
    );
    assert_eq!(
        first.narrative_markdown, second.narrative_markdown,
        "narrative report must regenerate byte-identically"
    );
}

#[test]
fn machine_report_is_a_valid_complete_study_report() {
    let generated = generate();
    let report: StudyReport = serde_json::from_str(&generated.machine_json)
        .expect("machine report parses as StudyReport");
    assert_eq!(report.rows().len(), 108);
    assert_eq!(
        report.study_id(),
        "aozora-parser-neutral-comparison-2026-07"
    );

    let document: serde_json::Value =
        serde_json::from_str(&generated.machine_json).expect("machine report is json");
    let schema = serde_json::from_str(include_str!(
        "../../../schemas/parser-comparison-result.schema.json"
    ))
    .expect("parse result schema");
    let validator = jsonschema::validator_for(&schema).expect("compile result schema");
    assert!(
        validator.is_valid(&document),
        "generated machine report must satisfy the frozen result schema"
    );
}

#[test]
fn robustness_rows_carry_exact_preregistered_parse_completion_counts() {
    let generated = generate();
    let report: StudyReport = serde_json::from_str(&generated.machine_json).expect("parse report");
    // aozora native corpus: success 17885, failure 1 -> 17885 / 17886.
    let aozora_native = report
        .rows()
        .iter()
        .find(|row| {
            row.candidate() == Candidate::Aozora
                && row.axis() == Axis::Robustness
                && row.measurement_mode() == MeasurementMode::Native
        })
        .expect("aozora native robustness row present");
    assert_eq!(aozora_native.status(), RowStatus::Measured);
    assert_eq!(aozora_native.numerator(), Some(17885));
    assert_eq!(aozora_native.denominator(), Some(17886));

    // aozora-epub3 adapter corpus: success 17773, failure 113 -> 17773 / 17886.
    let epub3_adapter = report
        .rows()
        .iter()
        .find(|row| {
            row.candidate() == Candidate::AozoraEpub3
                && row.axis() == Axis::Robustness
                && row.measurement_mode() == MeasurementMode::AdapterNormalized
        })
        .expect("aozora-epub3 adapter robustness row present");
    assert_eq!(epub3_adapter.status(), RowStatus::Measured);
    assert_eq!(epub3_adapter.numerator(), Some(17773));
    assert_eq!(epub3_adapter.denominator(), Some(17886));
}

#[test]
fn axes_without_committed_raw_data_are_missing_not_zero() {
    let generated = generate();
    let report: StudyReport = serde_json::from_str(&generated.machine_json).expect("parse report");
    for row in report.rows() {
        if row.axis() != Axis::Robustness
            || row.candidate() == Candidate::AozoraParserJs
            || row.candidate() == Candidate::AbAozora
        {
            assert_ne!(
                row.status(),
                RowStatus::Measured,
                "axis {:?} for {:?} has no committed raw data and must not be measured",
                row.axis(),
                row.candidate()
            );
            assert_eq!(row.numerator(), None);
            assert_eq!(row.denominator(), None);
        }
    }
}

#[test]
fn committed_reports_match_regeneration_from_raw_manifests() {
    let generated = generate();
    let committed_machine = fs::read_to_string(COMMITTED_MACHINE_REPORT)
        .expect("committed machine report exists on disk");
    let committed_narrative = fs::read_to_string(COMMITTED_NARRATIVE_REPORT)
        .expect("committed narrative report exists on disk");
    assert_eq!(
        generated.machine_json, committed_machine,
        "committed machine report drifted from the raw manifests"
    );
    assert_eq!(
        generated.narrative_markdown, committed_narrative,
        "committed narrative report drifted from the raw manifests"
    );
}
