use ab_parser_study_report::{
    Axis, Candidate, MeasurementMode, Missingness, ProvenanceStage, ResultRow, RowStatus,
    StudyReport,
};

const HASH: &str = "sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";

fn row(candidate: Candidate, axis: Axis, mode: MeasurementMode) -> ResultRow {
    ResultRow::new(
        candidate,
        axis,
        mode,
        "parser-revision",
        Some("adapter-revision".into()),
        HASH,
        RowStatus::Measured,
        Some(1),
        Some(2),
        Missingness::None,
        vec!["bounded instrument".into()],
        vec![ProvenanceStage::NativeParser],
    )
    .expect("valid measured row")
}

#[test]
fn source_lexer_fallback_cannot_be_native_capability() {
    let result = ResultRow::new(
        Candidate::AozoraRs,
        Axis::ConstructCoverage,
        MeasurementMode::Native,
        "parser-revision",
        Some("adapter-revision".into()),
        HASH,
        RowStatus::Measured,
        Some(1),
        Some(1),
        Missingness::None,
        vec!["fallback".into()],
        vec![ProvenanceStage::SourceLexerFallback],
    );
    assert!(
        result.is_err(),
        "fallback-derived results must not be native"
    );
}

#[test]
fn mapper_projection_cannot_be_native_capability() {
    let mut result = row(Candidate::Aozora2, Axis::Fidelity, MeasurementMode::Native);
    result.provenance = vec![ProvenanceStage::Mapper];
    assert!(
        result.validate().is_err(),
        "mapper-derived results must not be native"
    );
}

#[test]
fn report_requires_every_candidate_axis_pair_and_preserves_missing_rows() {
    let mut rows = Vec::new();
    for candidate in Candidate::ALL {
        for axis in Axis::ALL {
            rows.push(
                ResultRow::new(
                    candidate,
                    axis,
                    MeasurementMode::Native,
                    "parser-revision",
                    None,
                    HASH,
                    RowStatus::NonComparable,
                    None,
                    None,
                    Missingness::NonComparable,
                    vec!["no native analogue".into()],
                    vec![ProvenanceStage::NativeParser],
                )
                .expect("valid non-comparable row"),
            );
        }
    }
    let report = StudyReport::new("study", rows).expect("complete matrix");
    assert_eq!(report.rows.len(), Candidate::ALL.len() * Axis::ALL.len());
    let document = serde_json::to_value(report).expect("serialize report");
    let schema = serde_json::from_str(include_str!(
        "../../../schemas/parser-comparison-result.schema.json"
    ))
    .expect("parse result schema");
    let validator = jsonschema::validator_for(&schema).expect("compile result schema");
    assert!(validator.is_valid(&document));
}

#[test]
fn report_rejects_an_incomplete_candidate_axis_matrix() {
    let result = StudyReport::new(
        "study",
        vec![row(
            Candidate::Aozora,
            Axis::ConstructCoverage,
            MeasurementMode::Native,
        )],
    );
    assert!(
        result.is_err(),
        "missing candidate-axis rows must be rejected"
    );
}

#[test]
fn failed_axis_is_retained_with_explicit_missingness_and_no_imputed_counts() {
    let failed = ResultRow::new(
        Candidate::AozoraParserJs,
        Axis::Packaging,
        MeasurementMode::Native,
        "parser-revision",
        None,
        HASH,
        RowStatus::Failed,
        None,
        None,
        Missingness::BuildFailure,
        vec!["pinned source has no dependency lock".into()],
        vec![ProvenanceStage::NativeParser],
    )
    .expect("failure is a reportable result");
    assert_eq!(failed.missingness, Missingness::BuildFailure);
    assert_eq!(failed.denominator, None);
}
