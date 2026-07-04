use std::path::PathBuf;

use ab_coverage::matrix::RepresentabilityStatus;
use ab_coverage::source_inventory::{inventory_document, patterns_from_rows};
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
fn oracle_case_syntax_rows_exist_and_link_back() {
    #[derive(serde::Deserialize)]
    struct OracleCases {
        case: Vec<OracleCase>,
    }

    #[derive(serde::Deserialize)]
    struct OracleCase {
        id: String,
        syntax_row_ids: Vec<String>,
    }

    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let rows = matrix
        .rows()
        .iter()
        .map(|row| (row.id.as_str(), row))
        .collect::<std::collections::BTreeMap<_, _>>();
    let oracle_path = matrix_path()
        .parent()
        .unwrap()
        .join("aat-oracle-cases.toml");
    let oracle: OracleCases =
        toml::from_str(&std::fs::read_to_string(oracle_path).unwrap()).unwrap();

    for case in oracle.case {
        for row_id in &case.syntax_row_ids {
            let row = rows.get(row_id.as_str()).unwrap_or_else(|| {
                panic!("oracle case {} references missing row {}", case.id, row_id)
            });
            assert!(
                row.oracle_cases.iter().any(|linked| linked == &case.id),
                "row {} must link back to oracle case {}",
                row.id,
                case.id
            );
        }
    }
}

#[test]
fn kunten_rows_detect_real_fixture_spellings() {
    use ab_coverage::detectors::{DetectorContext, DetectorRegistry};

    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let registry = DetectorRegistry::from_matrix(matrix.rows());
    let fixture = matrix_path()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("tests/fixtures/kunten-source-excerpt.txt");
    let source = std::fs::read_to_string(&fixture).expect("read kunten fixture");
    let empty_aat = serde_json::json!({
        "version": 1,
        "work_id": "kunten-fixture",
        "blocks": [],
        "meta": {"adapter": "fixture", "adapter_version": "fixture"}
    });
    let ctx = DetectorContext {
        aat: &empty_aat,
        source: &source,
    };

    assert!(
        registry.detect("kunten.kaeriten", &ctx) > 0,
        "kunten.kaeriten must not false-zero on compact real fixture markers"
    );
    assert!(
        registry.detect("kunten.okurigana", &ctx) > 0,
        "kunten.okurigana must not false-zero on compact real fixture markers"
    );
}

#[test]
fn source_inventory_classifies_common_corpus_command_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃ここで字下げ終わり］",
        "［＃地から１字上げ］",
        "［＃ここから改行天付き、折り返して１字下げ］",
        "［＃小さな文字終わり］",
        "［＃１段階小さな文字］",
        "［＃中見出し終わり］",
        "［＃太字終わり］",
        "［＃割り注終わり］",
        "［＃横組み終わり］",
        "［＃「（c）」は縦中横］",
        "［＃改丁］",
        "［＃改段］",
        "［＃キャプション終わり］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known corpus command variants should not remain unknown"
    );
    for row_id in [
        "indentation.jisage_block",
        "indentation.chitsuki",
        "indentation.burasage",
        "decoration.font_size",
        "heading.basic",
        "decoration.bold_italic",
        "warichu.basic",
        "layout.yokogumi",
        "layout.tcy",
        "break.page_line",
        "caption.block",
    ] {
        assert!(
            summary.row_counts.contains_key(row_id),
            "expected source inventory row {row_id}"
        );
    }
}

#[test]
fn source_inventory_classifies_next_high_volume_command_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃行右小書き］",
        "［＃行右小書き終わり］",
        "［＃「＊」は行右小書き］",
        "［＃横組み］",
        "［＃ここで横組み終わり］",
        "［＃ここから２６字詰め］",
        "［＃ここで字詰め終わり］",
        "［＃傍点終わり］",
        "［＃傍線終わり］",
        "〔欄外に〕",
        "〔訳註〕",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known high-volume corpus marker variants should not remain unknown"
    );
    for row_id in [
        "decoration.font_size",
        "layout.yokogumi",
        "indentation.jizume",
        "decoration.boten",
        "decoration.bousen",
        "annotation.chuuki",
    ] {
        assert!(
            summary.row_counts.contains_key(row_id),
            "expected source inventory row {row_id}"
        );
    }
}

#[test]
fn source_inventory_classifies_font_size_subscript_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃「b」は下付き小文字］",
        "［＃「a,1」は下付き小文字］",
        "［＃「大字」は１段階小さな文字］",
        "［＃大きな文字終わり］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known font-size and subscript corpus variants should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("decoration.font_size")
            .map(|count| count.occurrences),
        Some(4)
    );
}

#[test]
fn decoration_font_size_has_typed_representability() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let row = matrix
        .rows()
        .iter()
        .find(|row| row.id == "decoration.font_size")
        .expect("decoration.font_size row");
    let representability = row
        .representability
        .as_ref()
        .expect("decoration.font_size needs a reviewed representability cell");

    assert_eq!(representability.status, RepresentabilityStatus::Typed);
    assert!(representability.raw_fallback);
    assert!(
        representability
            .aat_nodes
            .iter()
            .any(|node| node == "font_size")
    );
    assert!(
        row.tei_projection.contains("hi"),
        "font-size source markers need a TEI P5 hi projection"
    );
}

#[test]
fn source_inventory_classifies_bold_italic_corpus_closing_markers() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = ["［＃ここで太字終わり］", "［＃ここで斜体終わり］"].join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known bold/italic corpus closing markers should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("decoration.bold_italic")
            .map(|count| count.occurrences),
        Some(2)
    );
}

#[test]
fn decoration_bold_italic_has_typed_representability() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let row = matrix
        .rows()
        .iter()
        .find(|row| row.id == "decoration.bold_italic")
        .expect("decoration.bold_italic row");
    let representability = row
        .representability
        .as_ref()
        .expect("decoration.bold_italic needs a reviewed representability cell");

    assert_eq!(representability.status, RepresentabilityStatus::Typed);
    assert!(representability.raw_fallback);
    assert!(
        representability
            .aat_nodes
            .iter()
            .any(|node| node == "style")
    );
    assert!(
        row.tei_projection.contains("bold") && row.tei_projection.contains("italic"),
        "bold/italic source markers need TEI P5 hi rend projections"
    );
}

#[test]
fn source_inventory_classifies_keigakomi_block_markers() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃罫囲み］",
        "［＃罫囲み終わり］",
        "［＃ここから罫囲み］",
        "［＃ここで罫囲み終わり］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known keigakomi corpus markers should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("decoration.keigakomi")
            .map(|count| count.occurrences),
        Some(4)
    );
}

#[test]
fn decoration_keigakomi_has_typed_representability() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let row = matrix
        .rows()
        .iter()
        .find(|row| row.id == "decoration.keigakomi")
        .expect("decoration.keigakomi row");
    let representability = row
        .representability
        .as_ref()
        .expect("decoration.keigakomi needs a reviewed representability cell");

    assert_eq!(representability.status, RepresentabilityStatus::Typed);
    assert!(representability.raw_fallback);
    assert!(
        representability
            .aat_nodes
            .iter()
            .any(|node| node == "keigakomi")
    );
    assert!(
        row.tei_projection.contains("keigakomi"),
        "keigakomi source markers need a TEI projection preserving the ruled-box intent"
    );
}

#[test]
fn source_inventory_classifies_tcy_block_markers() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃縦中横］",
        "［＃縦中横終わり］",
        "［＃ここで縦中横終わり］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known tcy block corpus variants should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("layout.tcy")
            .map(|count| count.occurrences),
        Some(3)
    );
}

#[test]
fn layout_tcy_has_typed_representability() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let row = matrix
        .rows()
        .iter()
        .find(|row| row.id == "layout.tcy")
        .expect("layout.tcy row");
    let representability = row
        .representability
        .as_ref()
        .expect("layout.tcy needs a reviewed representability cell");

    assert_eq!(representability.status, RepresentabilityStatus::Typed);
    assert!(representability.raw_fallback);
    assert!(representability.aat_nodes.iter().any(|node| node == "tcy"));
    assert!(
        row.tei_projection.contains("tcy"),
        "tcy source markers need a TEI projection preserving the tcy layout intent"
    );
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
        tei_projection: "fixture-tei".to_owned(),
        plaintext_projection: String::new(),
        comparison_projection: String::new(),
        validation_properties: vec![],
        adapter_expectations: vec![],
        oracle_cases: vec![],
        status: ab_coverage::RowStatus::NeedsResearch,
        status_reason: String::new(),
        parsers,
        adapters,
        representability: None,
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

#[test]
fn schema_matrix_parses_representability_cell() {
    let matrix = matrix_from_toml_str(
        "representability-parse",
        r#"
[[syntax]]
id = "fixture.typed"
priority = 1
category = "fixture"
feature_keys = []
reference_sources = []
source_examples = []
source_patterns = []
ir_nodes = []
aat_nodes = ["ruby"]
tei_projection = "ruby/rb/rt"
plaintext_projection = ""
comparison_projection = ""
validation_properties = []
adapter_expectations = []
status = "needs_research"
status_reason = "fixture"

[syntax.representability]
source_inventory_row = "fixture.typed"
status = "typed"
aat_nodes = ["ruby"]
raw_fallback = true
evidence = "fixture"
notes = "fixture"
"#,
    );

    assert_eq!(matrix.rows()[0].id, "fixture.typed");
    let representability = matrix.rows()[0]
        .representability
        .as_ref()
        .expect("representability cell");
    assert_eq!(representability.source_inventory_row, "fixture.typed");
    assert_eq!(representability.status, RepresentabilityStatus::Typed);
    assert_eq!(representability.aat_nodes, ["ruby"]);
    assert!(representability.raw_fallback);
    assert_eq!(representability.evidence, "fixture");
    assert_eq!(representability.notes, "fixture");
}

#[test]
fn schema_matrix_rejects_represented_source_row_without_tei_projection() {
    let matrix = matrix_from_toml_str(
        "representability-without-tei-projection",
        r#"
[[syntax]]
id = "fixture.typed_without_tei"
priority = 1
category = "fixture"
feature_keys = []
reference_sources = []
source_examples = []
source_patterns = []
ir_nodes = []
aat_nodes = ["ruby"]
tei_projection = ""
plaintext_projection = ""
comparison_projection = ""
validation_properties = []
adapter_expectations = []
status = "needs_research"
status_reason = "fixture"

[syntax.representability]
source_inventory_row = "fixture.typed_without_tei"
status = "typed"
aat_nodes = ["ruby"]
raw_fallback = true
"#,
    );
    let errors = SchemaValidator::validate(&matrix, ValidationOptions::lenient());

    assert!(
        errors.iter().any(|e| e.message.contains("tei_projection")),
        "expected tei_projection error, got: {:?}",
        errors
    );
}

#[test]
fn schema_matrix_rejects_typed_representability_without_aat_nodes() {
    let matrix = matrix_from_toml_str(
        "representability-typed-without-aat",
        r#"
[[syntax]]
id = "fixture.typed_without_aat"
priority = 1
category = "fixture"
feature_keys = []
reference_sources = []
source_examples = []
source_patterns = []
ir_nodes = []
aat_nodes = []
tei_projection = ""
plaintext_projection = ""
comparison_projection = ""
validation_properties = []
adapter_expectations = []
status = "needs_research"
status_reason = "fixture"

[syntax.representability]
source_inventory_row = "fixture.typed_without_aat"
status = "typed"
aat_nodes = []
raw_fallback = true
"#,
    );
    let errors = SchemaValidator::validate(&matrix, ValidationOptions::lenient());

    assert!(
        errors
            .iter()
            .any(|e| e.message.contains("typed") && e.message.contains("aat_nodes")),
        "expected typed/aat_nodes error, got: {:?}",
        errors
    );
}

#[test]
fn schema_matrix_rejects_unsupported_representability_with_raw_fallback() {
    let matrix = matrix_from_toml_str(
        "representability-unsupported-raw-fallback",
        r#"
[[syntax]]
id = "fixture.unsupported"
priority = 1
category = "fixture"
feature_keys = []
reference_sources = []
source_examples = []
source_patterns = []
ir_nodes = []
aat_nodes = []
tei_projection = ""
plaintext_projection = ""
comparison_projection = ""
validation_properties = []
adapter_expectations = []
status = "needs_research"
status_reason = "fixture"

[syntax.representability]
source_inventory_row = "fixture.unsupported"
status = "unsupported"
raw_fallback = true
"#,
    );
    let errors = SchemaValidator::validate(&matrix, ValidationOptions::lenient());

    assert!(
        errors.iter().any(|e| e.message.contains("raw_fallback")),
        "expected unsupported/raw_fallback error, got: {:?}",
        errors
    );
}

#[test]
fn schema_matrix_rejects_empty_source_inventory_row() {
    let matrix = matrix_from_toml_str(
        "representability-empty-source-row",
        r#"
[[syntax]]
id = "fixture.empty_source_row"
priority = 1
category = "fixture"
feature_keys = []
reference_sources = []
source_examples = []
source_patterns = []
ir_nodes = []
aat_nodes = []
tei_projection = ""
plaintext_projection = ""
comparison_projection = ""
validation_properties = []
adapter_expectations = []
status = "needs_research"
status_reason = "fixture"

[syntax.representability]
source_inventory_row = ""
status = "needs_research"
raw_fallback = true
"#,
    );
    let errors = SchemaValidator::validate(&matrix, ValidationOptions::lenient());

    assert!(
        errors
            .iter()
            .any(|e| e.message.contains("source_inventory_row")),
        "expected source_inventory_row error, got: {:?}",
        errors
    );
}

#[test]
fn schema_matrix_rejects_unknown_source_inventory_row() {
    let matrix = matrix_from_toml_str(
        "representability-unknown-source-row",
        r#"
[[syntax]]
id = "fixture.unknown_source_row"
priority = 1
category = "fixture"
feature_keys = []
reference_sources = []
source_examples = []
source_patterns = []
ir_nodes = []
aat_nodes = []
tei_projection = ""
plaintext_projection = ""
comparison_projection = ""
validation_properties = []
adapter_expectations = []
status = "needs_research"
status_reason = "fixture"

[syntax.representability]
source_inventory_row = "fixture.missing"
status = "needs_research"
raw_fallback = true
"#,
    );
    let errors = SchemaValidator::validate(&matrix, ValidationOptions::lenient());

    assert!(
        errors.iter().any(|e| e.message.contains("fixture.missing")),
        "expected missing source row error, got: {:?}",
        errors
    );
}

#[test]
fn schema_matrix_does_not_infer_representability_from_corpus_prevalence() {
    let matrix = matrix_from_toml_str(
        "representability-no-prevalence-inference",
        r#"
[[syntax]]
id = "fixture.prevalent_without_representability"
priority = 1
category = "fixture"
feature_keys = []
reference_sources = []
source_examples = []
source_patterns = []
ir_nodes = []
aat_nodes = []
tei_projection = ""
plaintext_projection = ""
comparison_projection = ""
validation_properties = []
adapter_expectations = []
status = "needs_research"
status_reason = "fixture"

[syntax.corpus_prevalence]
works_with_feature = 1
total_occurrences = 42
detector_id = "fixture_prevalent_without_representability"
coverage_basis = "full_corpus"
sample_works = ["work"]
"#,
    );
    let errors = SchemaValidator::validate(&matrix, ValidationOptions::lenient());

    assert!(
        errors
            .iter()
            .all(|e| !e.message.contains("representability")),
        "corpus_prevalence must not imply representability requirements, got: {:?}",
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

fn matrix_from_toml_str(name: &str, toml: &str) -> CoverageMatrix {
    let path = std::env::temp_dir().join(format!(
        "ab-coverage-test-{}-{name}.toml",
        std::process::id()
    ));
    std::fs::write(&path, toml).unwrap();
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
    out.push_str("oracle_cases = []\n");
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
    if let Some(cell) = &row.representability {
        out.push_str("\n[syntax.representability]\n");
        out.push_str(&format!(
            "source_inventory_row = {:?}\n",
            cell.source_inventory_row
        ));
        out.push_str(&format!("status = {:?}\n", cell.status.as_str()));
        out.push_str(&format!("aat_nodes = {:?}\n", cell.aat_nodes));
        out.push_str(&format!("raw_fallback = {}\n", cell.raw_fallback));
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
