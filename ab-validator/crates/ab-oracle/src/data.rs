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

/// Load oracle cases from a TOML file.
///
/// # Errors
///
/// Returns an error when the file cannot be read or parsed.
pub fn load_oracle_cases(path: &Path) -> Result<OracleCases> {
    let input = fs::read_to_string(path)
        .with_context(|| format!("failed to read oracle cases {}", path.display()))?;
    toml::from_str(&input)
        .with_context(|| format!("failed to parse oracle cases {}", path.display()))
}

/// Load upstream observations from a TOML file.
///
/// # Errors
///
/// Returns an error when the file cannot be read or parsed.
pub fn load_upstream_observations(path: &Path) -> Result<UpstreamObservations> {
    let input = fs::read_to_string(path)
        .with_context(|| format!("failed to read upstream observations {}", path.display()))?;
    toml::from_str(&input)
        .with_context(|| format!("failed to parse upstream observations {}", path.display()))
}

impl OracleCase {
    #[must_use]
    pub fn current_review_status(&self) -> ReviewStatus {
        self.review
            .last()
            .map(|entry| entry.status)
            .unwrap_or(ReviewStatus::Draft)
    }
}

impl ReviewStatus {
    #[must_use]
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

        let break_case = cases
            .case
            .iter()
            .find(|case| case.id == "break.page.basic")
            .unwrap();
        assert_eq!(break_case.current_review_status(), ReviewStatus::Reviewed);
        assert_eq!(break_case.syntax_row_ids, vec!["break.page_line"]);

        let double_ruby_case = cases
            .case
            .iter()
            .find(|case| case.id == "ruby.double.right_left")
            .unwrap();
        assert_eq!(
            double_ruby_case.current_review_status(),
            ReviewStatus::Reviewed
        );
        assert_eq!(double_ruby_case.syntax_row_ids, vec!["ruby.double"]);

        let left_ruby_case = cases
            .case
            .iter()
            .find(|case| case.id == "ruby.placement.left")
            .unwrap();
        assert_eq!(
            left_ruby_case.current_review_status(),
            ReviewStatus::Reviewed
        );
        assert_eq!(
            left_ruby_case.syntax_row_ids,
            vec!["ruby.placement_directional"]
        );

        let nested_ruby_case = cases
            .case
            .iter()
            .find(|case| case.id == "ruby.nested_forbidden.target_includes_ruby")
            .unwrap();
        assert_eq!(
            nested_ruby_case.current_review_status(),
            ReviewStatus::Reviewed
        );
        assert_eq!(
            nested_ruby_case.syntax_row_ids,
            vec!["ruby.nested_forbidden"]
        );

        let chuuki_case = cases
            .case
            .iter()
            .find(|case| case.id == "annotation.chuuki.basic")
            .unwrap();
        assert_eq!(chuuki_case.current_review_status(), ReviewStatus::Reviewed);
        assert_eq!(chuuki_case.syntax_row_ids, vec!["annotation.chuuki"]);

        let bouki_case = cases
            .case
            .iter()
            .find(|case| case.id == "annotation.bouki.basic")
            .unwrap();
        assert_eq!(bouki_case.current_review_status(), ReviewStatus::Reviewed);
        assert_eq!(bouki_case.syntax_row_ids, vec!["annotation.bouki"]);

        let dakuten_case = cases
            .case
            .iter()
            .find(|case| case.id == "gaiji.dakuten_katakana.ve")
            .unwrap();
        assert_eq!(dakuten_case.current_review_status(), ReviewStatus::Reviewed);
        assert_eq!(dakuten_case.syntax_row_ids, vec!["gaiji.dakuten_katakana"]);

        let un_embed_case = cases
            .case
            .iter()
            .find(|case| case.id == "gaiji.un_embed.description_only")
            .unwrap();
        assert_eq!(
            un_embed_case.current_review_status(),
            ReviewStatus::Reviewed
        );
        assert_eq!(un_embed_case.syntax_row_ids, vec!["gaiji.un_embed"]);

        let kunoji_case = cases
            .case
            .iter()
            .find(|case| case.id == "iteration.kunoji.basic")
            .unwrap();
        assert_eq!(kunoji_case.current_review_status(), ReviewStatus::Reviewed);
        assert_eq!(kunoji_case.syntax_row_ids, vec!["iteration.kunoji"]);

        let accent_case = cases
            .case
            .iter()
            .find(|case| case.id == "accent.diacritic.e_acute")
            .unwrap();
        assert_eq!(accent_case.current_review_status(), ReviewStatus::Reviewed);
        assert_eq!(accent_case.syntax_row_ids, vec!["accent.diacritic"]);

        let expected_remaining_reviewed_cases = [
            ("kunten.kaeriten.basic", "kunten.kaeriten"),
            ("kunten.okurigana.basic", "kunten.okurigana"),
            ("heading.inline_form.large", "heading.inline_form"),
            ("heading.dogyo.medium", "heading.dogyo"),
            ("heading.mado.small", "heading.mado"),
            ("decoration.boten.white_sesame", "decoration.boten"),
            ("decoration.bousen.double", "decoration.bousen"),
            ("decoration.bold_italic.both", "decoration.bold_italic"),
            ("decoration.font_size.larger", "decoration.font_size"),
            ("decoration.keigakomi.inline", "decoration.keigakomi"),
            (
                "decoration.direction_override.left_boten",
                "decoration.direction_override",
            ),
            (
                "indentation.jisage_oneline.two_chars",
                "indentation.jisage_oneline",
            ),
            ("indentation.chitsuki.line", "indentation.chitsuki"),
            ("indentation.jizume.width", "indentation.jizume"),
            ("indentation.burasage.hanging", "indentation.burasage"),
            ("layout.yokogumi.inline", "layout.yokogumi"),
            ("layout.tcy.inline", "layout.tcy"),
            ("warigaki.parenthetical.basic", "warigaki.parenthetical"),
            ("caption.inline.basic", "caption.inline"),
            ("caption.block.basic", "caption.block"),
            ("figure.image_inline.basic", "figure.image_inline"),
            ("editor_note.unmapped.blank", "editor_note.unmapped"),
            ("reference.frontref.boten", "reference.frontref"),
            ("break.line_explicit.basic", "break.line_explicit"),
        ];
        for (case_id, syntax_row_id) in expected_remaining_reviewed_cases {
            let oracle_case = cases
                .case
                .iter()
                .find(|case| case.id == case_id)
                .unwrap_or_else(|| panic!("missing oracle case {case_id}"));
            assert_eq!(oracle_case.current_review_status(), ReviewStatus::Reviewed);
            assert_eq!(oracle_case.syntax_row_ids, vec![syntax_row_id]);
        }

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
