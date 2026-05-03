use std::{collections::BTreeMap, fs, path::Path};

use anyhow::{Context, Result};
use serde::Deserialize;

#[derive(Debug, Clone, Deserialize, PartialEq)]
pub struct OracleCases {
    pub aat_version: u64,
    pub case: Vec<OracleCase>,
}

#[derive(Debug, Clone, Deserialize, PartialEq)]
pub struct OracleCase {
    pub id: String,
    pub syntax_row_ids: Vec<String>,
    pub category: String,
    pub source_utf8: String,
    pub notes: Option<String>,
    pub oracle: OracleExpectations,
}

#[derive(Debug, Clone, Default, Deserialize, PartialEq)]
pub struct OracleExpectations {
    pub visible_text: Option<String>,
    #[serde(default)]
    pub nodes: Vec<NodeAssertion>,
    #[serde(default)]
    pub sequence: Vec<SequenceAssertion>,
    #[serde(default)]
    pub gaiji: Vec<GaijiAssertion>,
}

#[derive(Debug, Clone, Deserialize, PartialEq)]
pub struct NodeAssertion {
    pub selector: String,
    pub kind: String,
    pub count: Option<u64>,
    pub min_count: Option<u64>,
    pub absent: Option<bool>,
    #[serde(default)]
    pub fields: BTreeMap<String, toml::Value>,
    #[serde(default)]
    pub field_absent: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct SequenceAssertion {
    pub selector: String,
    pub kinds: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct GaijiAssertion {
    pub selector: String,
    pub description: String,
    pub resolved: Option<String>,
    pub jis_code: Option<String>,
    pub unresolved_reason: Option<String>,
    pub source: Option<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq)]
pub struct UpstreamObservations {
    pub observation: Vec<UpstreamObservation>,
}

#[derive(Debug, Clone, Deserialize, PartialEq)]
pub struct UpstreamObservation {
    pub case_id: String,
    pub adapter: String,
    pub status: String,
    pub summary: String,
    pub selector: String,
    pub kind: String,
    #[serde(default)]
    pub fields: BTreeMap<String, toml::Value>,
    pub evidence: Option<String>,
}

pub fn load_oracle_cases(path: &Path) -> Result<OracleCases> {
    let input = fs::read_to_string(path)
        .with_context(|| format!("failed to read oracle cases {}", path.display()))?;
    toml::from_str(&input)
        .with_context(|| format!("failed to parse oracle cases {}", path.display()))
}

pub fn load_upstream_observations(path: &Path) -> Result<UpstreamObservations> {
    let input = fs::read_to_string(path)
        .with_context(|| format!("failed to read upstream observations {}", path.display()))?;
    toml::from_str(&input)
        .with_context(|| format!("failed to parse upstream observations {}", path.display()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn data_path(name: &str) -> std::path::PathBuf {
        std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .join("data")
            .join(name)
    }

    #[test]
    fn loads_seed_oracle_cases() {
        let cases = load_oracle_cases(&data_path("aat-oracle-cases.toml")).unwrap();

        assert_eq!(cases.aat_version, 1);
        assert!(cases.case.iter().any(|case| case.id == "gaiji.jis.2-13-47"));
    }

    #[test]
    fn loads_upstream_observations_separately() {
        let observations =
            load_upstream_observations(&data_path("aat-upstream-observations.toml")).unwrap();

        assert!(
            observations
                .observation
                .iter()
                .any(|observation| observation.case_id == "gaiji.jis.2-13-47"
                    && observation.adapter == "aozora-rs")
        );
    }
}
