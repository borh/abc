//! Generation + drift contract: reports are a deterministic, provenance-tied
//! function of the committed raw run manifests and the frozen preregistration.

use std::fs;

use ab_parser_study_report::generate::{GeneratedReports, generate_reports};
use ab_parser_study_report::{
    Axis, Candidate, MeasurementMode, Missingness, RowStatus, StudyReport,
};

const RUN_MANIFESTS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../reports/parser-study/runs/aozora-parser-neutral-comparison-2026-07/run-manifests.json"
));

const PREREGISTRATION: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../docs/studies/aozora-parser-comparison-preregistration.json"
));

const APPENDIX_MANIFESTS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../reports/parser-study/runs/aozora-parser-neutral-comparison-2026-07/appendix-run-manifests.json"
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
    generate_reports(RUN_MANIFESTS, PREREGISTRATION, APPENDIX_MANIFESTS)
        .expect("generation succeeds")
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
        // Robustness is measured only where committed parse outcomes exist: every
        // executable existing-parser lane and the ab-aozora appendix corpus lane.
        // The excluded aozora-parser.js has no build, so even its robustness row is
        // unmeasured. Every non-robustness axis remains unmeasured for everyone.
        let expect_unmeasured =
            row.axis() != Axis::Robustness || row.candidate() == Candidate::AozoraParserJs;
        if expect_unmeasured {
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
fn ab_aozora_appendix_is_measured_noncomparable_or_missing_never_zero() {
    let generated = generate();
    let report: StudyReport = serde_json::from_str(&generated.machine_json).expect("parse report");
    let rows: Vec<_> = report
        .rows()
        .iter()
        .filter(|row| row.candidate() == Candidate::AbAozora)
        .collect();
    // Native-only appendix: exactly the 9 native rows, no adapter lane.
    assert_eq!(rows.len(), 9);
    for row in &rows {
        assert_eq!(row.measurement_mode(), MeasurementMode::Native);
        assert_eq!(
            row.parser_revision(),
            "ac2be926738f919faf44300e2999b3548d724297"
        );
    }

    // Robustness is really measured from the pinned corpus: 17886 / 17886, no zero.
    let robustness = rows
        .iter()
        .find(|row| row.axis() == Axis::Robustness)
        .expect("ab-aozora robustness row present");
    assert_eq!(robustness.status(), RowStatus::Measured);
    assert_eq!(robustness.numerator(), Some(17886));
    assert_eq!(robustness.denominator(), Some(17886));
    assert_eq!(robustness.missingness(), Missingness::None);

    // Owned-contract axes with no native competitor analogue are non-comparable,
    // never a competitor zero and never a blocker-missing.
    for axis in [Axis::Spans, Axis::Diagnostics] {
        let row = rows
            .iter()
            .find(|row| row.axis() == axis)
            .expect("owned axis row");
        assert_eq!(row.status(), RowStatus::NonComparable, "{axis:?}");
        assert_eq!(row.missingness(), Missingness::NonComparable, "{axis:?}");
        assert_eq!(row.numerator(), None);
        assert_eq!(row.denominator(), None);
    }

    // Axes with no valid instrument stay caveated missing (unavailable), not zero.
    for axis in [
        Axis::ConstructCoverage,
        Axis::Fidelity,
        Axis::Performance,
        Axis::Maintenance,
        Axis::Packaging,
        Axis::License,
    ] {
        let row = rows
            .iter()
            .find(|row| row.axis() == axis)
            .expect("missing axis row");
        assert_eq!(row.status(), RowStatus::Failed, "{axis:?}");
        assert_eq!(row.missingness(), Missingness::Unavailable, "{axis:?}");
        assert_eq!(row.numerator(), None);
        assert_eq!(row.denominator(), None);
    }
}

const PLACEHOLDER_REV: &str = "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef";

#[test]
fn native_wrapper_lanes_are_disclosed_and_revision_is_data_derived() {
    let real_rev = "ac2be926738f919faf44300e2999b3548d724297";

    // (1) Revision derivation, not value-equality: substitute the baseline
    // revision at its source (the preregistration candidate revision, which
    // feeds row.parser_revision()) and assert both reports track the data. A
    // hardcoded literal equal to the current revision would fail both arms.
    let altered_prereg = PREREGISTRATION.replace(real_rev, PLACEHOLDER_REV);
    let gen_rev = generate_reports(RUN_MANIFESTS, &altered_prereg, APPENDIX_MANIFESTS)
        .expect("generation succeeds");
    assert!(
        gen_rev.narrative_markdown.contains(PLACEHOLDER_REV),
        "appendix prose must interpolate the revision from data, not hardcode it"
    );
    assert!(
        !gen_rev.narrative_markdown.contains(real_rev),
        "no stale literal of the committed revision may survive in the prose"
    );
    assert!(
        gen_rev.machine_json.contains(&PLACEHOLDER_REV[..8]),
        "the short-form revision stamp in the robustness caveat must track the data"
    );

    // (2) Timeout derivation: substitute the appendix timeout and assert the
    // appendix prose tracks it rather than a `300` literal.
    let altered_timeout =
        APPENDIX_MANIFESTS.replace("\"timeout_seconds\": 300", "\"timeout_seconds\": 999");
    let gen_to = generate_reports(RUN_MANIFESTS, PREREGISTRATION, &altered_timeout)
        .expect("generation succeeds");
    assert!(
        gen_to.narrative_markdown.contains("999 s"),
        "appendix timeout prose must interpolate timeout_seconds from data"
    );

    // (3) Wrapper-native disclosure: aozora2html / aozora-epub3 "native" lanes
    // run through their `*-adapter` binary in `--mode html`; the report must
    // disclose that "native" means the parser-native output format via a thin
    // wrapper, not a direct parser invocation.
    let reports = generate();
    assert!(
        reports
            .narrative_markdown
            .contains("parser-native output format"),
        "the wrapper-native lanes must be disclosed"
    );
    assert!(
        reports
            .narrative_markdown
            .contains("not a direct parser invocation"),
        "the wrapper-native disclosure must state it is not a direct invocation"
    );
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
