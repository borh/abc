use std::{collections::BTreeMap, fs, path::Path};

use anyhow::{Context, Result};
use serde::Deserialize;

#[derive(Debug, Clone, Deserialize, PartialEq)]
pub struct OracleCases {
    pub aat_version: u64,
    pub evidence: Vec<Evidence>,
    pub case: Vec<OracleCase>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct Evidence {
    pub id: String,
    pub kind: EvidenceKind,
    pub citation: String,
    pub locator: Option<String>,
    pub url: Option<String>,
    pub supports: String,
    pub independent: bool,
    pub notes: Option<String>,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum EvidenceKind {
    ReferenceTable,
    Unicode,
    AozoraRule,
    CuratorNote,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct ReviewEntry {
    pub status: ReviewStatus,
    pub reviewer: String,
    pub reviewed_at: String,
    pub notes: Option<String>,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum ReviewStatus {
    Draft,
    Reviewed,
    Disputed,
    Retired,
}

#[derive(Debug, Clone, Deserialize, PartialEq)]
pub struct OracleCase {
    pub id: String,
    pub syntax_row_ids: Vec<String>,
    pub category: String,
    pub source_utf8: String,
    pub evidence_ids: Vec<String>,
    pub review: Vec<ReviewEntry>,
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
    #[serde(default)]
    pub evidence_ids: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct SequenceAssertion {
    pub selector: String,
    pub kinds: Vec<String>,
    #[serde(default)]
    pub evidence_ids: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct GaijiAssertion {
    pub selector: String,
    pub description: String,
    pub resolved: Option<String>,
    pub jis_code: Option<String>,
    pub unresolved_reason: Option<String>,
    pub source: Option<String>,
    #[serde(default)]
    pub evidence_ids: Vec<String>,
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

impl OracleCase {
    pub fn current_review_status(&self) -> ReviewStatus {
        self.review
            .last()
            .map(|entry| entry.status)
            .unwrap_or(ReviewStatus::Draft)
    }
}

impl ReviewStatus {
    pub fn as_str(self) -> &'static str {
        match self {
            ReviewStatus::Draft => "draft",
            ReviewStatus::Reviewed => "reviewed",
            ReviewStatus::Disputed => "disputed",
            ReviewStatus::Retired => "retired",
        }
    }
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
    fn loads_oracle_evidence_and_review_history() {
        let cases = load_oracle_cases(&data_path("aat-oracle-cases.toml")).unwrap();
        let evidence = cases
            .evidence
            .iter()
            .find(|evidence| evidence.id == "jis-x-0213-2-13-47")
            .unwrap();
        assert_eq!(evidence.kind, EvidenceKind::ReferenceTable);
        assert!(evidence.independent);
        assert_eq!(
            evidence.url.as_deref(),
            Some("https://www.x0213.org/codetable/jisx0213-2004-std.txt")
        );

        let case = cases
            .case
            .iter()
            .find(|case| case.id == "gaiji.jis.2-13-47")
            .unwrap();
        assert_eq!(case.evidence_ids, vec!["jis-x-0213-2-13-47"]);
        assert!(case.syntax_row_ids.iter().any(|id| id == "gaiji.marker"));
        assert_eq!(case.current_review_status(), ReviewStatus::Reviewed);

        let ruby_evidence = cases
            .evidence
            .iter()
            .find(|evidence| evidence.id == "aozora-rule-ruby-basic")
            .unwrap();
        assert_eq!(ruby_evidence.kind, EvidenceKind::AozoraRule);

        let ruby_case = cases
            .case
            .iter()
            .find(|case| case.id == "ruby.basic.simple")
            .unwrap();
        assert_eq!(ruby_case.current_review_status(), ReviewStatus::Reviewed);

        let heading_evidence = cases
            .evidence
            .iter()
            .find(|evidence| evidence.id == "aozora-rule-heading-basic")
            .unwrap();
        assert_eq!(heading_evidence.kind, EvidenceKind::AozoraRule);

        let heading_case = cases
            .case
            .iter()
            .find(|case| case.id == "heading.basic.large")
            .unwrap();
        assert_eq!(heading_case.current_review_status(), ReviewStatus::Reviewed);
        assert_eq!(heading_case.syntax_row_ids, vec!["heading.basic"]);

        let emphasis_case = cases
            .case
            .iter()
            .find(|case| case.id == "emphasis.boten.basic")
            .unwrap();
        assert_eq!(
            emphasis_case.current_review_status(),
            ReviewStatus::Reviewed
        );
        assert_eq!(emphasis_case.syntax_row_ids, vec!["emphasis.basic"]);

        let indentation_case = cases
            .case
            .iter()
            .find(|case| case.id == "indentation.jisage_block.basic")
            .unwrap();
        assert_eq!(
            indentation_case.current_review_status(),
            ReviewStatus::Reviewed
        );
        assert_eq!(
            indentation_case.syntax_row_ids,
            vec!["indentation.basic", "indentation.jisage_block"]
        );

        let warichu_case = cases
            .case
            .iter()
            .find(|case| case.id == "warichu.basic.inline")
            .unwrap();
        assert_eq!(warichu_case.current_review_status(), ReviewStatus::Reviewed);
        assert_eq!(warichu_case.syntax_row_ids, vec!["warichu.basic"]);

        let figure_case = cases
            .case
            .iter()
            .find(|case| case.id == "figure.image_caption.basic")
            .unwrap();
        assert_eq!(figure_case.current_review_status(), ReviewStatus::Reviewed);
        assert_eq!(figure_case.syntax_row_ids, vec!["figure.image_caption"]);

        let gaiji_ruby_case = cases
            .case
            .iter()
            .find(|case| case.id == "ruby.gaiji.inline_base")
            .unwrap();
        assert_eq!(
            gaiji_ruby_case.current_review_status(),
            ReviewStatus::Reviewed
        );
        assert_eq!(
            gaiji_ruby_case.evidence_ids,
            vec!["jis-x-0213-1-15-23", "aozora-rule-ruby-basic"]
        );
        assert_eq!(
            gaiji_ruby_case.oracle.visible_text.as_deref(),
            Some("噯が出た。")
        );
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
        assert!(observations.observation.iter().any(|observation| {
            observation.case_id == "ruby.gaiji.inline_base"
                && observation.adapter == "aozora2"
                && observation.kind == "ruby"
                && observation.fields.get("base").and_then(toml::Value::as_str) == Some("噯")
        }));
    }
}
