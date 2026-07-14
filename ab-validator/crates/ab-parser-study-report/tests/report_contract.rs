use ab_parser_study_report::{
    Axis, Candidate, MeasurementMode, Missingness, ProvenanceStage, ResultRow, RowStatus,
    StudyReport,
};

const HASH: &str = "sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";

fn row(candidate: Candidate, axis: Axis, mode: MeasurementMode) -> ResultRow {
    let (adapter_revision, provenance) = match mode {
        MeasurementMode::Native => (None, vec![ProvenanceStage::NativeParser]),
        MeasurementMode::AdapterNormalized => (
            Some("adapter-revision".into()),
            vec![ProvenanceStage::NativeParser, ProvenanceStage::Adapter],
        ),
    };
    ResultRow::new(
        candidate,
        axis,
        mode,
        "parser-revision",
        adapter_revision,
        HASH,
        RowStatus::Measured,
        Some(1),
        Some(2),
        Missingness::None,
        vec!["bounded instrument".into()],
        provenance,
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
        None,
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
    let document = serde_json::to_value(row(
        Candidate::Aozora2,
        Axis::Fidelity,
        MeasurementMode::Native,
    ))
    .expect("serialize row");
    let mut object = document.as_object().expect("row object").clone();
    object.insert("provenance".into(), serde_json::json!(["mapper"]));
    assert!(
        serde_json::from_value::<ResultRow>(object.into()).is_err(),
        "deserialization must reject mapper-derived native rows"
    );
}

#[test]
fn report_requires_every_candidate_axis_pair_and_preserves_missing_rows() {
    let mut rows = Vec::new();
    for candidate in Candidate::ALL {
        for axis in Axis::ALL {
            for mode in candidate.required_modes() {
                rows.push(row(candidate, axis, *mode));
            }
        }
    }
    let report = StudyReport::new("study", rows).expect("complete matrix");
    assert_eq!(report.rows().len(), 108);
    let document = serde_json::to_value(report).expect("serialize report");
    let schema = serde_json::from_str(include_str!(
        "../../../schemas/parser-comparison-result.schema.json"
    ))
    .expect("parse result schema");
    let validator = jsonschema::validator_for(&schema).expect("compile result schema");
    assert!(validator.is_valid(&document));
}

#[test]
fn parse_rejects_report_with_incomplete_mode_matrix() {
    let mut rows = Vec::new();
    for candidate in Candidate::ALL {
        for axis in Axis::ALL {
            for mode in candidate.required_modes() {
                rows.push(row(candidate, axis, *mode));
            }
        }
    }
    let report = StudyReport::new("study", rows).expect("complete report");
    let mut document = serde_json::to_value(report).expect("serialize report");
    document["rows"].as_array_mut().expect("rows array").pop();
    assert!(serde_json::from_value::<StudyReport>(document).is_err());
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
fn report_rejects_missing_adapter_lane_for_included_candidate() {
    let mut rows = Vec::new();
    for candidate in Candidate::ALL {
        for axis in Axis::ALL {
            for mode in candidate.required_modes() {
                if !(candidate == Candidate::Aozora2
                    && axis == Axis::Spans
                    && *mode == MeasurementMode::AdapterNormalized)
                {
                    rows.push(row(candidate, axis, *mode));
                }
            }
        }
    }
    assert!(StudyReport::new("study", rows).is_err());
}

#[test]
fn parse_rejects_native_adapter_revision_and_adapted_without_derived_stage() {
    let mut native = serde_json::to_value(row(
        Candidate::Aozora,
        Axis::Fidelity,
        MeasurementMode::Native,
    ))
    .expect("serialize native row");
    native["adapter_revision"] = serde_json::json!("not-allowed");
    assert!(serde_json::from_value::<ResultRow>(native).is_err());

    let mut adapted = serde_json::to_value(row(
        Candidate::Aozora,
        Axis::Fidelity,
        MeasurementMode::AdapterNormalized,
    ))
    .expect("serialize adapted row");
    adapted["provenance"] = serde_json::json!(["native_parser"]);
    assert!(serde_json::from_value::<ResultRow>(adapted).is_err());
}

#[test]
fn parse_rejects_status_missingness_mismatch() {
    let mut document = serde_json::to_value(row(
        Candidate::Aozora,
        Axis::Packaging,
        MeasurementMode::Native,
    ))
    .expect("serialize row");
    document["status"] = serde_json::json!("failed");
    document["numerator"] = serde_json::Value::Null;
    document["denominator"] = serde_json::Value::Null;
    document["missingness"] = serde_json::json!("non-comparable");
    assert!(serde_json::from_value::<ResultRow>(document).is_err());
}

#[test]
fn schema_rejects_the_same_attribution_and_missingness_violations() {
    let schema: serde_json::Value = serde_json::from_str(include_str!(
        "../../../schemas/parser-comparison-result.schema.json"
    ))
    .expect("parse schema");
    let row_schema = schema["$defs"]["row"].clone();
    let validator = jsonschema::validator_for(&row_schema).expect("compile row schema");

    let mut native = serde_json::to_value(row(
        Candidate::Aozora,
        Axis::Fidelity,
        MeasurementMode::Native,
    ))
    .expect("serialize row");
    native["adapter_revision"] = serde_json::json!("forbidden");
    assert!(!validator.is_valid(&native));

    let mut mismatch = serde_json::to_value(row(
        Candidate::Aozora,
        Axis::Fidelity,
        MeasurementMode::Native,
    ))
    .expect("serialize row");
    mismatch["status"] = serde_json::json!("failed");
    mismatch["numerator"] = serde_json::Value::Null;
    mismatch["denominator"] = serde_json::Value::Null;
    mismatch["missingness"] = serde_json::json!("non-comparable");
    assert!(!validator.is_valid(&mismatch));
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
    assert_eq!(failed.missingness(), Missingness::BuildFailure);
    assert_eq!(failed.denominator(), None);
}
