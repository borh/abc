use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct RubyReadingEvidenceDetail {
    pub ruby_base: String,
    pub ruby_reading: String,
    pub ruby_reading_norm: String,
    pub classification: RubyOracleClassification,
    pub per_analyzer: BTreeMap<String, AnalyzerRubyReadingEvidence>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum RubyOracleClassification {
    Resolved,
    NonstandardRuby,
    NoComparableReading,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct AnalyzerRubyReadingEvidence {
    pub reading: Option<String>,
    pub norm: Option<String>,
    #[serde(rename = "match")]
    pub matches: bool,
    pub align: RubyReadingAlignment,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum RubyReadingAlignment {
    Exact,
    BoundaryMisalign,
    NoReading,
}
