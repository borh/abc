use crate::data::{Evidence, EvidenceKind, OracleCase, OracleCases, ReviewStatus};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OracleQualityError {
    pub case_id: String,
    pub message: String,
}

pub struct EvidenceIndex<'a> {
    pub evidence: std::collections::BTreeMap<&'a str, &'a Evidence>,
    pub duplicate_ids: Vec<String>,
}

#[must_use]
pub fn build_evidence_index(cases: &OracleCases) -> EvidenceIndex<'_> {
    let mut evidence = std::collections::BTreeMap::new();
    let mut duplicate_ids = Vec::new();
    for record in &cases.evidence {
        if evidence.insert(record.id.as_str(), record).is_some() {
            duplicate_ids.push(record.id.clone());
        }
    }
    EvidenceIndex {
        evidence,
        duplicate_ids,
    }
}

#[must_use]
pub fn validate_case_quality(
    case: &OracleCase,
    evidence_index: &EvidenceIndex<'_>,
) -> Vec<OracleQualityError> {
    let mut errors = Vec::new();
    check_ids(
        &case.id,
        "case.evidence_ids",
        &case.evidence_ids,
        evidence_index,
        &mut errors,
    );

    for assertion in &case.oracle.nodes {
        if !assertion.evidence_ids.is_empty() {
            check_ids(
                &case.id,
                "case.oracle.nodes.evidence_ids",
                &assertion.evidence_ids,
                evidence_index,
                &mut errors,
            );
        }
    }
    for assertion in &case.oracle.sequence {
        if !assertion.evidence_ids.is_empty() {
            check_ids(
                &case.id,
                "case.oracle.sequence.evidence_ids",
                &assertion.evidence_ids,
                evidence_index,
                &mut errors,
            );
        }
    }
    for assertion in &case.oracle.gaiji {
        if !assertion.evidence_ids.is_empty() {
            check_ids(
                &case.id,
                "case.oracle.gaiji.evidence_ids",
                &assertion.evidence_ids,
                evidence_index,
                &mut errors,
            );
        }
    }

    if case.current_review_status() == ReviewStatus::Reviewed {
        let has_non_curator_evidence = case.evidence_ids.iter().any(|id| {
            evidence_index
                .evidence
                .get(id.as_str())
                .is_some_and(|record| {
                    record.independent && !matches!(record.kind, EvidenceKind::CuratorNote)
                })
        });
        if !has_non_curator_evidence {
            errors.push(OracleQualityError {
                case_id: case.id.clone(),
                message:
                    "reviewed cases require at least one independent non-curator evidence record"
                        .to_owned(),
            });
        }
    }

    errors
}

#[must_use]
pub fn validate_oracle_quality(cases: &OracleCases) -> Vec<OracleQualityError> {
    let evidence_index = build_evidence_index(cases);
    let mut errors = Vec::new();
    for duplicate in &evidence_index.duplicate_ids {
        errors.push(OracleQualityError {
            case_id: "<evidence>".to_owned(),
            message: format!("duplicate evidence id {duplicate:?}"),
        });
    }
    for case in &cases.case {
        errors.extend(validate_case_quality(case, &evidence_index));
    }
    errors
}

fn check_ids(
    case_id: &str,
    field_name: &str,
    ids: &[String],
    evidence_index: &EvidenceIndex<'_>,
    errors: &mut Vec<OracleQualityError>,
) {
    for id in ids {
        match evidence_index.evidence.get(id.as_str()) {
            Some(record) if record.independent => {}
            Some(_) => errors.push(OracleQualityError {
                case_id: case_id.to_owned(),
                message: format!(
                    "{field_name} references evidence id {id:?}, but it is not independent"
                ),
            }),
            None => errors.push(OracleQualityError {
                case_id: case_id.to_owned(),
                message: format!("{field_name} references missing evidence id {id:?}"),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::{EvidenceKind, OracleExpectations, ReviewEntry, ReviewStatus};

    fn evidence(id: &str, kind: EvidenceKind, independent: bool) -> Evidence {
        Evidence {
            id: id.to_owned(),
            kind,
            citation: "fixture".to_owned(),
            locator: None,
            url: None,
            supports: "fixture".to_owned(),
            independent,
            notes: None,
        }
    }

    fn case_with(evidence_ids: Vec<&str>, status: ReviewStatus) -> OracleCase {
        OracleCase {
            id: "case".to_owned(),
            syntax_row_ids: vec!["fixture.syntax".to_owned()],
            category: "fixture".to_owned(),
            source_utf8: "本文".to_owned(),
            evidence_ids: evidence_ids.into_iter().map(str::to_owned).collect(),
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
    fn rejects_case_evidence_ids_that_do_not_exist() {
        let cases = OracleCases {
            aat_version: 1,
            evidence: Vec::new(),
            case: vec![case_with(vec!["missing"], ReviewStatus::Draft)],
        };

        let errors = validate_oracle_quality(&cases);

        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("missing evidence id \"missing\""))
        );
    }

    #[test]
    fn rejects_non_independent_oracle_evidence() {
        let cases = OracleCases {
            aat_version: 1,
            evidence: vec![evidence(
                "not-independent",
                EvidenceKind::ReferenceTable,
                false,
            )],
            case: vec![case_with(vec!["not-independent"], ReviewStatus::Draft)],
        };

        let errors = validate_oracle_quality(&cases);

        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("not independent"))
        );
    }

    #[test]
    fn reviewed_cases_require_non_curator_evidence() {
        let cases = OracleCases {
            aat_version: 1,
            evidence: vec![evidence("curated", EvidenceKind::CuratorNote, true)],
            case: vec![case_with(vec!["curated"], ReviewStatus::Reviewed)],
        };

        let errors = validate_oracle_quality(&cases);

        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("non-curator"))
        );
    }

    #[test]
    fn duplicate_evidence_ids_are_reported() {
        let cases = OracleCases {
            aat_version: 1,
            evidence: vec![
                evidence("duplicate", EvidenceKind::ReferenceTable, true),
                evidence("duplicate", EvidenceKind::Unicode, true),
            ],
            case: Vec::new(),
        };

        let errors = validate_oracle_quality(&cases);

        assert!(errors.iter().any(|error| {
            error
                .message
                .contains("duplicate evidence id \"duplicate\"")
        }));
    }
}
