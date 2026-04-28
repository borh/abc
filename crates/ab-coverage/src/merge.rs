//! Structure-preserving merger for `data/aozora-syntax-coverage.toml`.
//!
//! Two operations:
//! - `apply_classifier_findings` — replaces `parsers.<id>` and `adapters.<id>`
//!   sub-tables for the rows present in the JSON input. Used by Task 3.
//! - `apply_prevalence_findings` — overwrites only the
//!   `corpus_prevalence.{works_with_feature, total_occurrences, sample_works,
//!   coverage_basis}` fields on the rows present in the JSON input. Used by
//!   Task 4 Step 5.
//!
//! Both operations preserve comments, ordering, and untouched fields by
//! editing through `toml_edit::DocumentMut`.

use anyhow::{Context, Result, anyhow};
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, fs, path::Path};
use toml_edit::{Array, DocumentMut, Item, Table, Value, value};

#[derive(Debug, Deserialize)]
pub struct ClassifierFindings {
    pub rows: BTreeMap<String, RowFindings>,
}

#[derive(Debug, Deserialize, Default)]
pub struct RowFindings {
    #[serde(default)]
    pub parsers: BTreeMap<String, ParserFinding>,
    #[serde(default)]
    pub adapters: BTreeMap<String, AdapterFinding>,
}

#[derive(Debug, Deserialize)]
pub struct ParserFinding {
    pub recognition: String,
    #[serde(default)]
    pub evidence: String,
    #[serde(default)]
    pub notes: String,
}

#[derive(Debug, Deserialize)]
pub struct AdapterFinding {
    pub aat_fidelity: String,
    #[serde(default)]
    pub evidence: String,
    #[serde(default)]
    pub notes: String,
}

#[derive(Debug, Deserialize)]
pub struct PrevalenceFindings {
    pub rows: BTreeMap<String, PrevalenceRow>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct PrevalenceRow {
    pub works_with_feature: u64,
    pub total_occurrences: u64,
    #[serde(default)]
    pub coverage_basis: Option<String>,
    #[serde(default)]
    pub sample_works: Vec<String>,
}

pub fn apply_classifier_findings(
    matrix_path: &Path,
    findings: &ClassifierFindings,
) -> Result<usize> {
    let raw = fs::read_to_string(matrix_path)
        .with_context(|| format!("read {}", matrix_path.display()))?;
    let mut doc: DocumentMut = raw
        .parse()
        .with_context(|| format!("parse {}", matrix_path.display()))?;

    let mut updated = 0usize;
    let rows = doc
        .get_mut("syntax")
        .and_then(|item| item.as_array_of_tables_mut())
        .ok_or_else(|| anyhow!("missing [[syntax]] array of tables"))?;

    for row in rows.iter_mut() {
        let row_id = row
            .get("id")
            .and_then(|i| i.as_str())
            .ok_or_else(|| anyhow!("syntax row missing string id"))?
            .to_string();
        let Some(finding) = findings.rows.get(&row_id) else {
            continue;
        };

        if !finding.parsers.is_empty() {
            let parsers = ensure_subtable(row, "parsers")?;
            for (id, cell) in &finding.parsers {
                let mut tbl = Table::new();
                tbl.set_implicit(false);
                tbl.insert("recognition", value(cell.recognition.clone()));
                tbl.insert("evidence", value(cell.evidence.clone()));
                tbl.insert("notes", value(cell.notes.clone()));
                parsers.insert(id, Item::Table(tbl));
            }
        }
        if !finding.adapters.is_empty() {
            let adapters = ensure_subtable(row, "adapters")?;
            for (id, cell) in &finding.adapters {
                let mut tbl = Table::new();
                tbl.set_implicit(false);
                tbl.insert("aat_fidelity", value(cell.aat_fidelity.clone()));
                tbl.insert("evidence", value(cell.evidence.clone()));
                tbl.insert("notes", value(cell.notes.clone()));
                adapters.insert(id, Item::Table(tbl));
            }
        }
        updated += 1;
    }

    fs::write(matrix_path, doc.to_string())
        .with_context(|| format!("write {}", matrix_path.display()))?;
    Ok(updated)
}

pub fn apply_prevalence_findings(
    matrix_path: &Path,
    findings: &PrevalenceFindings,
) -> Result<usize> {
    let raw = fs::read_to_string(matrix_path)
        .with_context(|| format!("read {}", matrix_path.display()))?;
    let mut doc: DocumentMut = raw
        .parse()
        .with_context(|| format!("parse {}", matrix_path.display()))?;

    let mut updated = 0usize;
    let rows = doc
        .get_mut("syntax")
        .and_then(|item| item.as_array_of_tables_mut())
        .ok_or_else(|| anyhow!("missing [[syntax]] array of tables"))?;

    for row in rows.iter_mut() {
        let row_id = row
            .get("id")
            .and_then(|i| i.as_str())
            .ok_or_else(|| anyhow!("syntax row missing string id"))?
            .to_string();
        let Some(prev) = findings.rows.get(&row_id) else {
            continue;
        };
        let table = ensure_subtable(row, "corpus_prevalence")?;
        table.insert(
            "works_with_feature",
            value(prev.works_with_feature as i64),
        );
        table.insert("total_occurrences", value(prev.total_occurrences as i64));
        if let Some(basis) = &prev.coverage_basis {
            table.insert("coverage_basis", value(basis.clone()));
        }
        let mut arr = Array::new();
        for w in &prev.sample_works {
            arr.push(Value::from(w.as_str()));
        }
        table.insert("sample_works", value(arr));
        updated += 1;
    }

    fs::write(matrix_path, doc.to_string())
        .with_context(|| format!("write {}", matrix_path.display()))?;
    Ok(updated)
}

fn ensure_subtable<'a>(row: &'a mut Table, key: &str) -> Result<&'a mut Table> {
    if !row.contains_key(key) {
        let mut tbl = Table::new();
        tbl.set_implicit(true);
        row.insert(key, Item::Table(tbl));
    }
    row.get_mut(key)
        .and_then(|item| item.as_table_mut())
        .ok_or_else(|| anyhow!("row sub-table {key} is not a table"))
}

