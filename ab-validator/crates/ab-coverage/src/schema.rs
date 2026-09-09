use std::{collections::BTreeSet, fmt};

use crate::matrix::{CoverageMatrix, Recognition, RepresentabilityStatus, Row, RowAatFidelity};

/// Options for `SchemaValidator::validate`.
#[derive(Debug, Clone, Copy)]
pub struct ValidationOptions {
    /// If true, `recognition = "unknown"` and `aat_fidelity = "unknown"` are
    /// allowed  where rows are filled
    /// parser-by-parser).
    pub allow_unknown: bool,
    /// If true, every row must declare cells for these parser/adapter ids.
    pub required_keys: &'static [&'static str],
}

impl ValidationOptions {
    #[must_use]
    pub const fn lenient() -> Self {
        Self {
            allow_unknown: true,
            required_keys: &[],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RowError {
    pub row_id: String,
    pub message: String,
}

impl fmt::Display for RowError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {}", self.row_id, self.message)
    }
}

pub struct SchemaValidator;

impl SchemaValidator {
    #[must_use]
    pub fn validate(matrix: &CoverageMatrix, opts: ValidationOptions) -> Vec<RowError> {
        let mut errors = Vec::new();
        let row_ids = matrix
            .rows()
            .iter()
            .map(|row| row.id.as_str())
            .collect::<BTreeSet<_>>();
        for row in matrix.rows() {
            validate_row(row, opts, &row_ids, &mut errors);
        }
        errors
    }
}

fn validate_row(
    row: &Row,
    opts: ValidationOptions,
    row_ids: &BTreeSet<&str>,
    errors: &mut Vec<RowError>,
) {
    if row.id.is_empty() {
        errors.push(RowError {
            row_id: "<missing id>".into(),
            message: "row id must be non-empty".into(),
        });
    }

    for key in opts.required_keys {
        if !row.parsers.contains_key(*key) {
            errors.push(RowError {
                row_id: row.id.clone(),
                message: format!("missing parsers.{key} sub-table"),
            });
        }
        if !row.adapters.contains_key(*key) {
            errors.push(RowError {
                row_id: row.id.clone(),
                message: format!("missing adapters.{key} sub-table"),
            });
        }
    }

    for (parser_id, cell) in &row.parsers {
        if !opts.allow_unknown && cell.recognition.is_unknown() {
            errors.push(RowError {
                row_id: row.id.clone(),
                message: format!(
                    "parsers.{parser_id}.recognition = \"unknown\" is not allowed in strict mode"
                ),
            });
        }
        if let Some(adapter_cell) = row.adapters.get(parser_id) {
            check_combination(
                row,
                parser_id,
                cell.recognition,
                adapter_cell.aat_fidelity,
                opts,
                errors,
            );
        }
    }

    for (adapter_id, cell) in &row.adapters {
        if !opts.allow_unknown && cell.aat_fidelity.is_unknown() {
            errors.push(RowError {
                row_id: row.id.clone(),
                message: format!(
                    "adapters.{adapter_id}.aat_fidelity = \"unknown\" is not allowed in strict mode"
                ),
            });
        }
    }

    if let Some(prev) = &row.corpus_prevalence
        && prev.sample_works.len() > 5
    {
        errors.push(RowError {
            row_id: row.id.clone(),
            message: format!(
                "corpus_prevalence.sample_works has {} entries (max 5)",
                prev.sample_works.len()
            ),
        });
    }

    validate_representability(row, row_ids, errors);
}

fn validate_representability(row: &Row, row_ids: &BTreeSet<&str>, errors: &mut Vec<RowError>) {
    let Some(cell) = &row.representability else {
        return;
    };

    if cell.source_inventory_row.is_empty() {
        errors.push(RowError {
            row_id: row.id.clone(),
            message: "representability.source_inventory_row must be non-empty".into(),
        });
    } else if !row_ids.contains(cell.source_inventory_row.as_str()) {
        errors.push(RowError {
            row_id: row.id.clone(),
            message: format!(
                "representability.source_inventory_row = \"{}\" does not name an existing matrix row id",
                cell.source_inventory_row
            ),
        });
    }

    if representability_requires_tei_projection(cell.status) && !has_tei_projection(row) {
        errors.push(RowError {
            row_id: row.id.clone(),
            message: format!(
                "representability.status = \"{}\" requires non-empty tei_projection",
                cell.status.as_str()
            ),
        });
    }

    match cell.status {
        RepresentabilityStatus::Typed if cell.aat_nodes.is_empty() => {
            errors.push(RowError {
                row_id: row.id.clone(),
                message: "representability.status = \"typed\" requires non-empty representability.aat_nodes".into(),
            });
        }
        RepresentabilityStatus::Unsupported if cell.raw_fallback => {
            errors.push(RowError {
                row_id: row.id.clone(),
                message: "representability.status = \"unsupported\" requires raw_fallback = false"
                    .into(),
            });
        }
        _ => {}
    }
}

fn representability_requires_tei_projection(status: RepresentabilityStatus) -> bool {
    matches!(
        status,
        RepresentabilityStatus::Typed
            | RepresentabilityStatus::RawPreserved
            | RepresentabilityStatus::OutOfBody
    )
}

fn has_tei_projection(row: &Row) -> bool {
    let projection = row.tei_projection.trim();
    !projection.is_empty() && !projection.eq_ignore_ascii_case("n/a")
}

fn check_combination(
    row: &Row,
    id: &str,
    recognition: Recognition,
    fidelity: RowAatFidelity,
    opts: ValidationOptions,
    errors: &mut Vec<RowError>,
) {
    // While placeholders are tolerated, skip the cross-field rules.
    if opts.allow_unknown && (recognition.is_unknown() || fidelity.is_unknown()) {
        return;
    }

    use Recognition::*;
    use RowAatFidelity::*;
    let invalid = match (recognition, fidelity) {
        // aborts => only not_applicable.
        (Aborts, NotApplicable) => false,
        (Aborts, _) => true,
        // parsed => not synthesised.
        (Parsed, Synthesised) => true,
        // unrecognised => only synthesised | not_applicable.
        (Unrecognised, Preserved | Lossy | Dropped) => true,
        _ => false,
    };

    if invalid {
        errors.push(RowError {
            row_id: row.id.clone(),
            message: format!(
                "forbidden combination for {id}: recognition = \"{}\" + aat_fidelity = \"{}\"",
                recognition.as_str(),
                fidelity.as_str()
            ),
        });
    }
}
