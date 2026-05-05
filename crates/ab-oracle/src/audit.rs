use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::Path,
};

use crate::{
    data::{OracleCases, ReviewStatus},
    evaluate::evidence_strength,
};
use anyhow::{Context, Result};

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct SyntaxCoverageGap {
    pub syntax_row_id: String,
    pub oracle_cases: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct OracleAudit {
    pub total_cases: usize,
    pub total_evidence: usize,
    pub review_status_counts: BTreeMap<String, usize>,
    pub evidence_strength_counts: BTreeMap<String, usize>,
    pub curator_only_cases: Vec<String>,
    pub unreviewed_cases: Vec<String>,
    pub syntax_rows_without_reviewed_oracle_coverage: Vec<SyntaxCoverageGap>,
}

/// Build a summary of oracle review/evidence status for all cases.
pub fn audit_oracle(cases: &OracleCases, syntax_coverage: Option<&str>) -> OracleAudit {
    let mut review_status_counts = BTreeMap::new();
    let mut evidence_strength_counts = BTreeMap::new();
    let mut curator_only_cases = Vec::new();
    let mut unreviewed_cases = Vec::new();
    let evidence_by_id = cases
        .evidence
        .iter()
        .map(|evidence| (evidence.id.as_str(), evidence))
        .collect::<BTreeMap<_, _>>();

    for case in &cases.case {
        let review_status = case.current_review_status();
        *review_status_counts
            .entry(review_status.as_str().to_owned())
            .or_insert(0) += 1;

        let strength = evidence_strength(case, &cases.evidence).to_owned();
        *evidence_strength_counts.entry(strength).or_insert(0) += 1;

        let linked_evidence = case
            .evidence_ids
            .iter()
            .filter_map(|id| evidence_by_id.get(id.as_str()).copied())
            .collect::<Vec<_>>();
        if !linked_evidence.is_empty()
            && linked_evidence
                .iter()
                .all(|evidence| matches!(evidence.kind, crate::data::EvidenceKind::CuratorNote))
        {
            curator_only_cases.push(case.id.clone());
        }

        if review_status != ReviewStatus::Reviewed {
            unreviewed_cases.push(case.id.clone());
        }
    }

    OracleAudit {
        total_cases: cases.case.len(),
        total_evidence: cases.evidence.len(),
        review_status_counts,
        evidence_strength_counts,
        curator_only_cases,
        unreviewed_cases,
        syntax_rows_without_reviewed_oracle_coverage: syntax_coverage
            .map(|input| syntax_rows_without_reviewed_coverage(cases, input))
            .unwrap_or_default(),
    }
}

#[must_use]
pub fn render_audit_markdown(audit: &OracleAudit) -> String {
    let mut out = format!(
        "# Oracle Evidence Audit\n\n- total_cases: {}\n- total_evidence: {}\n",
        audit.total_cases, audit.total_evidence
    );
    out.push_str("\n## Review Status\n\n| status | cases |\n| --- | --- |\n");
    for (status, count) in &audit.review_status_counts {
        out.push_str(&format!("| {status} | {count} |\n"));
    }
    out.push_str("\n## Evidence Strength\n\n| strength | cases |\n| --- | --- |\n");
    for (strength, count) in &audit.evidence_strength_counts {
        out.push_str(&format!("| {strength} | {count} |\n"));
    }

    out.push_str("\n## Curator-Only Cases\n\n");
    push_list_or_empty(&mut out, &audit.curator_only_cases);

    out.push_str("\n## Unreviewed Cases\n\n");
    push_list_or_empty(&mut out, &audit.unreviewed_cases);

    out.push_str("\n## Syntax Rows Without Reviewed Oracle Coverage\n\n");
    if audit
        .syntax_rows_without_reviewed_oracle_coverage
        .is_empty()
    {
        out.push_str("_None._\n");
    } else {
        out.push_str("| syntax_row_id | oracle_cases |\n| --- | --- |\n");
        for gap in &audit.syntax_rows_without_reviewed_oracle_coverage {
            out.push_str(&format!(
                "| {} | {} |\n",
                gap.syntax_row_id,
                gap.oracle_cases.join(", ")
            ));
        }
    }

    out
}

/// Write audit summary JSON to disk.
///
/// # Errors
///
/// Returns an error when the output directory cannot be created or writing fails.
pub fn write_json_audit(audit: &OracleAudit, path: &Path) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    let file =
        fs::File::create(path).with_context(|| format!("failed to create {}", path.display()))?;
    serde_json::to_writer_pretty(file, audit)
        .with_context(|| format!("failed to write {}", path.display()))
}

/// Read audit summary JSON from disk.
///
/// # Errors
///
/// Returns an error when the file cannot be read or decoded as JSON.
pub fn read_json_audit(path: &Path) -> Result<OracleAudit> {
    let input =
        fs::read(path).with_context(|| format!("failed to read audit {}", path.display()))?;
    serde_json::from_slice(&input).with_context(|| format!("failed to parse {}", path.display()))
}

fn push_list_or_empty(out: &mut String, items: &[String]) {
    if items.is_empty() {
        out.push_str("_None._\n");
    } else {
        for item in items {
            out.push_str(&format!("- `{item}`\n"));
        }
    }
}

fn syntax_rows_without_reviewed_coverage(
    cases: &OracleCases,
    syntax_coverage: &str,
) -> Vec<SyntaxCoverageGap> {
    let reviewed_case_ids = cases
        .case
        .iter()
        .filter(|case| case.current_review_status() == ReviewStatus::Reviewed)
        .map(|case| case.id.as_str())
        .collect::<BTreeSet<_>>();

    let Ok(value) = toml::from_str::<toml::Value>(syntax_coverage) else {
        return Vec::new();
    };
    let Some(rows) = value.get("syntax").and_then(toml::Value::as_array) else {
        return Vec::new();
    };

    rows.iter()
        .filter_map(|row| {
            let syntax_row_id = row.get("id")?.as_str()?.to_owned();
            let oracle_cases = row
                .get("oracle_cases")
                .and_then(toml::Value::as_array)
                .map(|items| {
                    items
                        .iter()
                        .filter_map(toml::Value::as_str)
                        .map(str::to_owned)
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();
            let has_reviewed_coverage = oracle_cases
                .iter()
                .any(|case_id| reviewed_case_ids.contains(case_id.as_str()));

            (!has_reviewed_coverage).then_some(SyntaxCoverageGap {
                syntax_row_id,
                oracle_cases,
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::{Evidence, EvidenceKind, OracleCase, OracleExpectations, ReviewEntry};

    fn evidence(id: &str, kind: EvidenceKind) -> Evidence {
        Evidence {
            id: id.to_owned(),
            kind,
            citation: "fixture".to_owned(),
            locator: None,
            url: None,
            supports: "fixture".to_owned(),
            independent: true,
            notes: None,
        }
    }

    fn case(id: &str, evidence_ids: &[&str], status: ReviewStatus) -> OracleCase {
        OracleCase {
            id: id.to_owned(),
            syntax_row_ids: vec!["fixture.syntax".to_owned()],
            category: "fixture".to_owned(),
            source_utf8: "本文".to_owned(),
            evidence_ids: evidence_ids.iter().map(|id| (*id).to_owned()).collect(),
            review: vec![ReviewEntry {
                status,
                reviewer: "test".to_owned(),
                reviewed_at: "2026-05-03".to_owned(),
                notes: None,
            }],
            notes: None,
            oracle: OracleExpectations::default(),
        }
    }

    #[test]
    fn audit_counts_review_status_strength_and_weak_cases() {
        let cases = OracleCases {
            aat_version: 1,
            evidence: vec![
                evidence("unicode", EvidenceKind::Unicode),
                evidence("reference", EvidenceKind::ReferenceTable),
                evidence("curated", EvidenceKind::CuratorNote),
            ],
            case: vec![
                case("reviewed", &["unicode"], ReviewStatus::Reviewed),
                case("draft-reference", &["reference"], ReviewStatus::Draft),
                case("draft-curated", &["curated"], ReviewStatus::Draft),
            ],
        };

        let audit = audit_oracle(&cases, None);

        assert_eq!(audit.total_cases, 3);
        assert_eq!(audit.total_evidence, 3);
        assert_eq!(audit.review_status_counts["draft"], 2);
        assert_eq!(audit.review_status_counts["reviewed"], 1);
        assert_eq!(audit.evidence_strength_counts["normative"], 1);
        assert_eq!(audit.evidence_strength_counts["reference"], 1);
        assert_eq!(audit.evidence_strength_counts["curated"], 1);
        assert_eq!(audit.curator_only_cases, vec!["draft-curated"]);
        assert_eq!(
            audit.unreviewed_cases,
            vec!["draft-reference", "draft-curated"]
        );
    }

    #[test]
    fn audit_lists_syntax_rows_without_reviewed_oracle_coverage() {
        let cases = OracleCases {
            aat_version: 1,
            evidence: vec![evidence("reference", EvidenceKind::ReferenceTable)],
            case: vec![
                case("draft-case", &["reference"], ReviewStatus::Draft),
                case("reviewed-case", &["reference"], ReviewStatus::Reviewed),
            ],
        };
        let syntax_coverage = r#"
            [[syntax]]
            id = "covered.row"
            oracle_cases = ["reviewed-case"]

            [[syntax]]
            id = "draft.only"
            oracle_cases = ["draft-case"]

            [[syntax]]
            id = "no.oracle"
        "#;

        let audit = audit_oracle(&cases, Some(syntax_coverage));

        assert_eq!(
            audit.syntax_rows_without_reviewed_oracle_coverage,
            vec![
                SyntaxCoverageGap {
                    syntax_row_id: "draft.only".to_owned(),
                    oracle_cases: vec!["draft-case".to_owned()],
                },
                SyntaxCoverageGap {
                    syntax_row_id: "no.oracle".to_owned(),
                    oracle_cases: Vec::new(),
                },
            ]
        );
    }

    #[test]
    fn markdown_names_audit_sections() {
        let audit = OracleAudit {
            total_cases: 1,
            total_evidence: 1,
            review_status_counts: [("draft".to_owned(), 1)].into_iter().collect(),
            evidence_strength_counts: [("curated".to_owned(), 1)].into_iter().collect(),
            curator_only_cases: vec!["draft-curated".to_owned()],
            unreviewed_cases: vec!["draft-curated".to_owned()],
            syntax_rows_without_reviewed_oracle_coverage: vec![SyntaxCoverageGap {
                syntax_row_id: "ruby.basic".to_owned(),
                oracle_cases: vec!["draft-curated".to_owned()],
            }],
        };

        let markdown = render_audit_markdown(&audit);

        assert!(markdown.contains("Review Status"));
        assert!(markdown.contains("Evidence Strength"));
        assert!(markdown.contains("Curator-Only Cases"));
        assert!(markdown.contains("Syntax Rows Without Reviewed Oracle Coverage"));
    }
}
