use ab_parser_study_report::{
    Axis, Candidate, MeasurementMode, Missingness, ProvenanceStage, RawRunManifest, ResultRow,
    RowStatus, StudyReport,
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

#[test]
fn raw_run_manifest_requires_all_frozen_identity_hashes() {
    let missing_environment = serde_json::json!({
        "schema_version": 1,
        "study_id": "aozora-parser-neutral-comparison-2026-07",
        "candidate": "aozora2",
        "measurement_mode": "native",
        "parser_revision": "93420b53c7d52579a0ca3fde466cef8ce6d89879",
        "adapter_revision": null,
        "corpus_hash": HASH,
        "timeout_seconds": 300,
        "protocol_hash": HASH,
        "status": "build-failure",
        "raw_outputs": []
    });
    assert!(serde_json::from_value::<RawRunManifest>(missing_environment).is_err());
}

#[test]
fn raw_run_manifest_retains_build_failure_without_outputs() {
    let manifest: RawRunManifest = serde_json::from_value(serde_json::json!({
        "schema_version": 1,
        "study_id": "aozora-parser-neutral-comparison-2026-07",
        "candidate": "aozora",
        "measurement_mode": "native",
        "parser_revision": "1a4f864603970983719655aa4af4525958ac2d38",
        "adapter_revision": null,
        "corpus_hash": HASH,
        "environment_hash": HASH,
        "timeout_seconds": 300,
        "protocol_hash": HASH,
        "status": "build-failure",
        "failure_stage": "parser-build",
        "failure_detail": "pinned derivation did not build",
        "raw_outputs": []
    }))
    .expect("build failure is evidence");
    assert_eq!(manifest.raw_outputs().len(), 0);
}

#[test]
fn adapted_run_requires_adapter_revision_and_content_addressed_outputs() {
    let invalid = serde_json::json!({
        "schema_version": 1,
        "study_id": "study",
        "candidate": "aozora-rs",
        "measurement_mode": "adapter_normalized",
        "parser_revision": "rev",
        "adapter_revision": null,
        "corpus_hash": HASH,
        "environment_hash": HASH,
        "timeout_seconds": 300,
        "protocol_hash": HASH,
        "status": "measured",
        "raw_outputs": [{"work_id":"1", "path":"raw/1.json", "sha256":HASH}]
    });
    assert!(serde_json::from_value::<RawRunManifest>(invalid).is_err());
}

#[test]
fn checked_raw_run_bundle_validates_every_included_lane() {
    let manifests: Vec<RawRunManifest> = serde_json::from_str(include_str!(
        "../../../reports/parser-study/runs/aozora-parser-neutral-comparison-2026-07/run-manifests.json"
    ))
    .expect("checked raw manifests validate");
    assert_eq!(manifests.len(), 10);
    let actual: std::collections::BTreeSet<_> = manifests
        .iter()
        .map(|manifest| (manifest.candidate(), manifest.measurement_mode()))
        .collect();
    let expected: std::collections::BTreeSet<_> = [
        Candidate::Aozora,
        Candidate::Aozora2,
        Candidate::AozoraRs,
        Candidate::Aozora2html,
        Candidate::AozoraEpub3,
    ]
    .into_iter()
    .flat_map(|candidate| {
        [MeasurementMode::Native, MeasurementMode::AdapterNormalized]
            .into_iter()
            .map(move |mode| (candidate, mode))
    })
    .collect();
    assert_eq!(actual, expected);
}

#[test]
fn measured_run_requires_at_least_one_raw_work_output() {
    let empty = serde_json::json!({
        "schema_version": 1, "study_id": "study", "candidate": "aozora2",
        "measurement_mode": "native", "parser_revision": "rev", "adapter_revision": null,
        "corpus_hash": HASH, "environment_hash": HASH, "timeout_seconds": 300,
        "protocol_hash": HASH, "status": "measured", "raw_outputs": []
    });
    assert!(serde_json::from_value::<RawRunManifest>(empty).is_err());
}
