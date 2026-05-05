use std::fmt;

use crate::matrix::{CoverageMatrix, Recognition, Row, RowAatFidelity};

/// Knobs for `SchemaValidator::validate`.
#[derive(Debug, Clone, Copy)]
pub struct ValidationOptions {
    /// If true, `recognition = "unknown"` and `aat_fidelity = "unknown"` are
    /// allowed (used during Task 3 ramp-up where rows are filled
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

    #[must_use] 
    pub const fn strict() -> Self {
        Self {
            allow_unknown: false,
            required_keys: &["aozora2", "aozora-rs", "aozora2html"],
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
        for row in matrix.rows() {
            validate_row(row, opts, &mut errors);
        }
        errors
    }
}

fn validate_row(row: &Row, opts: ValidationOptions, errors: &mut Vec<RowError>) {
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
