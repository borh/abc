use std::{collections::BTreeMap, fs, path::Path};

use anyhow::{Context, Result};
use serde::Deserialize;

/// Top-level coverage matrix loaded from `data/aozora-syntax-coverage.toml`.
#[derive(Debug, Clone)]
pub struct CoverageMatrix {
    rows: Vec<Row>,
}

impl CoverageMatrix {
    /// Load a coverage matrix from TOML.
    ///
    /// # Errors
    ///
    /// Returns an error when the matrix file cannot be read or parsed.
    pub fn from_toml(path: &Path) -> Result<Self> {
        let raw = fs::read_to_string(path)
            .with_context(|| format!("failed to read coverage matrix {}", path.display()))?;
        let parsed: MatrixFile = toml::from_str(&raw)
            .with_context(|| format!("failed to parse coverage matrix {}", path.display()))?;
        Ok(Self {
            rows: parsed.syntax,
        })
    }

    #[must_use]
    pub fn rows(&self) -> &[Row] {
        &self.rows
    }
}

/// One `[[syntax]]` entry. Mirrors `crates/ab-index/src/syntax_coverage.rs::SyntaxRow`
/// and adds the open-keyed `parsers` / `adapters` maps and the prevalence sub-table.
#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct Row {
    pub id: String,
    pub priority: u8,
    pub category: String,
    pub feature_keys: Vec<String>,
    pub reference_sources: Vec<String>,
    pub source_examples: Vec<String>,
    pub source_patterns: Vec<String>,
    pub ir_nodes: Vec<String>,
    pub aat_nodes: Vec<String>,
    pub tei_projection: String,
    pub plaintext_projection: String,
    pub comparison_projection: String,
    pub validation_properties: Vec<String>,
    pub adapter_expectations: Vec<String>,
    #[serde(default)]
    pub oracle_cases: Vec<String>,
    pub status: RowStatus,
    pub status_reason: String,
    #[serde(default)]
    pub parsers: BTreeMap<String, ParserCell>,
    #[serde(default)]
    pub adapters: BTreeMap<String, AdapterCell>,
    pub corpus_prevalence: Option<CorpusPrevalence>,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum RowStatus {
    Covered,
    Partial,
    NotModeled,
    NeedsResearch,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct ParserCell {
    pub recognition: Recognition,
    #[serde(default)]
    pub evidence: String,
    #[serde(default)]
    pub notes: String,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum Recognition {
    Parsed,
    Normalised,
    Unrecognised,
    Aborts,
    Unknown,
}

impl Recognition {
    #[must_use]
    pub fn is_unknown(self) -> bool {
        matches!(self, Recognition::Unknown)
    }

    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Recognition::Parsed => "parsed",
            Recognition::Normalised => "normalised",
            Recognition::Unrecognised => "unrecognised",
            Recognition::Aborts => "aborts",
            Recognition::Unknown => "unknown",
        }
    }
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct AdapterCell {
    pub aat_fidelity: RowAatFidelity,
    #[serde(default)]
    pub evidence: String,
    #[serde(default)]
    pub notes: String,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum RowAatFidelity {
    Preserved,
    Lossy,
    Dropped,
    Synthesised,
    NotApplicable,
    Unknown,
}

impl RowAatFidelity {
    #[must_use]
    pub fn is_unknown(self) -> bool {
        matches!(self, RowAatFidelity::Unknown)
    }

    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            RowAatFidelity::Preserved => "preserved",
            RowAatFidelity::Lossy => "lossy",
            RowAatFidelity::Dropped => "dropped",
            RowAatFidelity::Synthesised => "synthesised",
            RowAatFidelity::NotApplicable => "not_applicable",
            RowAatFidelity::Unknown => "unknown",
        }
    }
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct CorpusPrevalence {
    pub works_with_feature: u64,
    pub total_occurrences: u64,
    #[serde(default)]
    pub detector_id: String,
    pub coverage_basis: CoverageBasis,
    pub sample_works: Vec<String>,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum CoverageBasis {
    FullCorpus,
    StratifiedSample,
    NotRun,
}

#[derive(Debug, Deserialize)]
struct MatrixFile {
    syntax: Vec<Row>,
}
