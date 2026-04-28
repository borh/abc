use std::path::PathBuf;

use ab_coverage::{
    AdapterCell, CorpusPrevalence, CoverageBasis, CoverageMatrix, ParserCell, Recognition,
    RowAatFidelity, RowError, SchemaValidator, ValidationOptions,
};

fn matrix_path() -> PathBuf {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("data/aozora-syntax-coverage.toml")
}

#[test]
fn matrix_schema_valid() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let errors = SchemaValidator::validate(&matrix, ValidationOptions::lenient());
    assert!(
        errors.is_empty(),
        "matrix has {} schema errors:\n{}",
        errors.len(),
        join_errors(&errors)
    );
}

#[test]
fn matrix_required_keys_present() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let opts = ValidationOptions {
        allow_unknown: true,
        required_keys: &["aozora2", "aozora-rs", "aozora2html"],
    };
    let errors = SchemaValidator::validate(&matrix, opts);
    assert!(
        errors.is_empty(),
        "every row must declare cells for the three known parser/adapter ids:\n{}",
        join_errors(&errors)
    );
}

#[test]
fn matrix_corpus_prevalence_present_for_every_row() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    for row in matrix.rows() {
        assert!(
            row.corpus_prevalence.is_some(),
            "row {} is missing [syntax.corpus_prevalence]",
            row.id
        );
    }
}

#[test]
fn forbidden_combinations_rejected() {
    use std::collections::BTreeMap;
    let mut parsers = BTreeMap::new();
    parsers.insert(
        "fake".to_string(),
        ParserCell {
            recognition: Recognition::Aborts,
            evidence: String::new(),
            notes: String::new(),
        },
    );
    let mut adapters = BTreeMap::new();
    adapters.insert(
        "fake".to_string(),
        AdapterCell {
            aat_fidelity: RowAatFidelity::Preserved,
            evidence: String::new(),
            notes: String::new(),
        },
    );
    let row = ab_coverage::matrix::Row {
        id: "test.row".into(),
        priority: 1,
        category: "test".into(),
        feature_keys: vec![],
        reference_sources: vec![],
        source_examples: vec![],
        source_patterns: vec![],
        ir_nodes: vec![],
        aat_nodes: vec![],
        tei_projection: String::new(),
        plaintext_projection: String::new(),
        comparison_projection: String::new(),
        validation_properties: vec![],
        adapter_expectations: vec![],
        status: ab_coverage::RowStatus::NeedsResearch,
        status_reason: String::new(),
        parsers,
        adapters,
        corpus_prevalence: Some(CorpusPrevalence {
            works_with_feature: 0,
            total_occurrences: 0,
            detector_id: String::new(),
            coverage_basis: CoverageBasis::NotRun,
            sample_works: vec![],
        }),
    };
    let dummy = make_matrix(vec![row]);
    let opts = ValidationOptions {
        allow_unknown: false,
        required_keys: &[],
    };
    let errors = SchemaValidator::validate(&dummy, opts);
    assert!(
        errors
            .iter()
            .any(|e| e.message.contains("forbidden combination")),
        "expected forbidden-combination error, got: {:?}",
        errors
    );
}

fn make_matrix(rows: Vec<ab_coverage::matrix::Row>) -> CoverageMatrix {
    // Round-trip through TOML to avoid exposing private constructors.
    let mut buf = String::new();
    for row in rows {
        buf.push_str(&serialize_row(&row));
    }
    let path = std::env::temp_dir().join(format!("ab-coverage-test-{}.toml", std::process::id()));
    std::fs::write(&path, buf).unwrap();
    let matrix = CoverageMatrix::from_toml(&path).unwrap();
    let _ = std::fs::remove_file(&path);
    matrix
}

fn serialize_row(row: &ab_coverage::matrix::Row) -> String {
    let mut out = String::from("[[syntax]]\n");
    out.push_str(&format!("id = {:?}\n", row.id));
    out.push_str(&format!("priority = {}\n", row.priority));
    out.push_str(&format!("category = {:?}\n", row.category));
    out.push_str("feature_keys = []\n");
    out.push_str("reference_sources = []\n");
    out.push_str("source_examples = []\n");
    out.push_str("source_patterns = []\n");
    out.push_str("ir_nodes = []\n");
    out.push_str("aat_nodes = []\n");
    out.push_str("tei_projection = \"\"\n");
    out.push_str("plaintext_projection = \"\"\n");
    out.push_str("comparison_projection = \"\"\n");
    out.push_str("validation_properties = []\n");
    out.push_str("adapter_expectations = []\n");
    let status = match row.status {
        ab_coverage::RowStatus::Covered => "covered",
        ab_coverage::RowStatus::Partial => "partial",
        ab_coverage::RowStatus::NotModeled => "not_modeled",
        ab_coverage::RowStatus::NeedsResearch => "needs_research",
    };
    out.push_str(&format!("status = \"{status}\"\n"));
    out.push_str(&format!("status_reason = {:?}\n", row.status_reason));
    for (id, cell) in &row.parsers {
        out.push_str(&format!("\n[syntax.parsers.\"{id}\"]\n"));
        out.push_str(&format!(
            "recognition = \"{}\"\n",
            cell.recognition.as_str()
        ));
        out.push_str(&format!("evidence = {:?}\n", cell.evidence));
        out.push_str(&format!("notes = {:?}\n", cell.notes));
    }
    for (id, cell) in &row.adapters {
        out.push_str(&format!("\n[syntax.adapters.\"{id}\"]\n"));
        out.push_str(&format!(
            "aat_fidelity = \"{}\"\n",
            cell.aat_fidelity.as_str()
        ));
        out.push_str(&format!("evidence = {:?}\n", cell.evidence));
        out.push_str(&format!("notes = {:?}\n", cell.notes));
    }
    if let Some(prev) = &row.corpus_prevalence {
        out.push_str("\n[syntax.corpus_prevalence]\n");
        out.push_str(&format!(
            "works_with_feature = {}\n",
            prev.works_with_feature
        ));
        out.push_str(&format!("total_occurrences = {}\n", prev.total_occurrences));
        out.push_str(&format!("detector_id = {:?}\n", prev.detector_id));
        let basis = match prev.coverage_basis {
            CoverageBasis::FullCorpus => "full_corpus",
            CoverageBasis::StratifiedSample => "stratified_sample",
            CoverageBasis::NotRun => "not_run",
        };
        out.push_str(&format!("coverage_basis = \"{basis}\"\n"));
        out.push_str("sample_works = []\n");
    }
    out
}

fn join_errors(errors: &[RowError]) -> String {
    errors
        .iter()
        .map(|e| format!("  - {e}"))
        .collect::<Vec<_>>()
        .join("\n")
}
