use std::{fs, path::Path};

use anyhow::{Context, Result};
use serde::Deserialize;

#[derive(Debug, Clone)]
pub struct SyntaxCoverage {
    rows: Vec<SyntaxRow>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct SyntaxRow {
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
    pub status: SyntaxStatus,
    pub status_reason: String,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum SyntaxStatus {
    Covered,
    Partial,
    NotModeled,
    NeedsResearch,
}

#[derive(Debug, Deserialize)]
struct SyntaxCoverageFile {
    syntax: Vec<SyntaxRow>,
}

impl SyntaxCoverage {
    /// Load syntax coverage data from TOML.
    ///
    /// # Errors
    /// 
    /// Returns an error when the file cannot be read or parsed as TOML.
    pub fn from_toml(path: &Path) -> Result<Self> {
        let raw = fs::read_to_string(path)
            .with_context(|| format!("failed to read syntax coverage {}", path.display()))?;
        let parsed: SyntaxCoverageFile = toml::from_str(&raw)
            .with_context(|| format!("failed to parse syntax coverage {}", path.display()))?;
        Ok(Self {
            rows: parsed.syntax,
        })
    }

    #[must_use] 
    pub fn rows(&self) -> &[SyntaxRow] {
        &self.rows
    }
}
