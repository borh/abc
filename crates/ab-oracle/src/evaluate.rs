use ab_ir::aat_view::AatDocument;
use serde_json::Value;

use crate::data::{OracleCase, UpstreamObservations};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseEvaluation {
    pub case_id: String,
    pub schema_status: String,
    pub upstream_status: String,
    pub oracle_status: String,
    pub failures: Vec<String>,
}

pub fn schema_status(aat: &Value) -> String {
    if ab_check::check::validate_aat_value(aat).is_ok() {
        "pass".to_owned()
    } else {
        "fail".to_owned()
    }
}

pub fn visible_text(document: &AatDocument) -> String {
    document.visible_text()
}

pub fn evaluate_case(
    case: &OracleCase,
    observations: &UpstreamObservations,
    adapter: &str,
    aat: Value,
) -> CaseEvaluation {
    let schema_status = schema_status(&aat);
    let document = AatDocument::from_value(aat);
    let mut failures = Vec::new();

    if let Some(expected) = &case.oracle.visible_text {
        let actual = document.visible_text();
        if &actual != expected {
            failures.push(format!(
                "visible_text expected {:?}, got {:?}",
                expected, actual
            ));
        }
    }

    evaluate_node_assertions(case, &document, &mut failures);
    evaluate_sequence_assertions(case, &document, &mut failures);
    evaluate_gaiji_assertions(case, &document, &mut failures);

    let upstream_status = upstream_status(case, observations, adapter, &document);
    let oracle_status = if failures.is_empty() { "pass" } else { "fail" }.to_owned();

    CaseEvaluation {
        case_id: case.id.clone(),
        schema_status,
        upstream_status,
        oracle_status,
        failures,
    }
}

fn upstream_status(
    case: &OracleCase,
    observations: &UpstreamObservations,
    adapter: &str,
    document: &AatDocument,
) -> String {
    let Some(observation) = observations
        .observation
        .iter()
        .find(|observation| observation.case_id == case.id && observation.adapter == adapter)
    else {
        return "no_observation".to_owned();
    };

    let Ok(nodes) = document.select(&observation.selector) else {
        return "wrapper_mismatch".to_owned();
    };

    if nodes.iter().any(|node| {
        node.get("kind").and_then(Value::as_str) == Some(observation.kind.as_str())
            && observation
                .fields
                .iter()
                .all(|(field, expected)| field_matches_toml(node.get(field), expected))
    }) {
        "faithful".to_owned()
    } else {
        "wrapper_mismatch".to_owned()
    }
}

fn evaluate_node_assertions(case: &OracleCase, document: &AatDocument, failures: &mut Vec<String>) {
    for assertion in &case.oracle.nodes {
        let nodes = match document.select(&assertion.selector) {
            Ok(nodes) => nodes,
            Err(error) => {
                failures.push(format!(
                    "node selector {} failed: {}",
                    error.selector, error.message
                ));
                continue;
            }
        };
        let matches = nodes
            .iter()
            .filter(|node| {
                node.get("kind").and_then(Value::as_str) == Some(assertion.kind.as_str())
            })
            .filter(|node| {
                assertion
                    .fields
                    .iter()
                    .all(|(field, expected)| field_matches_toml(node.get(field), expected))
            })
            .count();

        if assertion.absent.unwrap_or(false) {
            if matches > 0 {
                failures.push(format!(
                    "expected no {} nodes at {}, found {}",
                    assertion.kind, assertion.selector, matches
                ));
            }
            continue;
        }

        if let Some(expected) = assertion.count {
            if matches != expected as usize {
                failures.push(format!(
                    "expected {} {} nodes at {}, found {}",
                    expected, assertion.kind, assertion.selector, matches
                ));
            }
        }
        if let Some(expected) = assertion.min_count {
            if matches < expected as usize {
                failures.push(format!(
                    "expected at least {} {} nodes at {}, found {}",
                    expected, assertion.kind, assertion.selector, matches
                ));
            }
        }
    }
}

fn evaluate_sequence_assertions(
    case: &OracleCase,
    document: &AatDocument,
    failures: &mut Vec<String>,
) {
    for assertion in &case.oracle.sequence {
        let nodes = match document.select(&assertion.selector) {
            Ok(nodes) => nodes,
            Err(error) => {
                failures.push(format!(
                    "sequence selector {} failed: {}",
                    error.selector, error.message
                ));
                continue;
            }
        };

        let matched = nodes.iter().any(|node| {
            let Some(items) = node.as_array() else {
                return false;
            };
            let kinds = items
                .iter()
                .filter_map(|item| item.get("kind").and_then(Value::as_str))
                .collect::<Vec<_>>();
            kinds == assertion.kinds
        });

        if !matched {
            failures.push(format!(
                "expected kind sequence {:?} at {}",
                assertion.kinds, assertion.selector
            ));
        }
    }
}

fn evaluate_gaiji_assertions(
    case: &OracleCase,
    document: &AatDocument,
    failures: &mut Vec<String>,
) {
    for assertion in &case.oracle.gaiji {
        let nodes = match document.select(&assertion.selector) {
            Ok(nodes) => nodes,
            Err(error) => {
                failures.push(format!(
                    "gaiji selector {} failed: {}",
                    error.selector, error.message
                ));
                continue;
            }
        };

        let matched = nodes.iter().any(|node| {
            node.get("kind").and_then(Value::as_str) == Some("gaiji")
                && node.get("description").and_then(Value::as_str)
                    == Some(assertion.description.as_str())
                && optional_string_matches(node.get("resolved"), assertion.resolved.as_deref())
                && optional_string_matches(node.get("jis_code"), assertion.jis_code.as_deref())
                && optional_string_matches(
                    node.get("unresolved_reason"),
                    assertion.unresolved_reason.as_deref(),
                )
        });

        if !matched {
            failures.push(format!(
                "expected gaiji {:?} at {}",
                assertion.description, assertion.selector
            ));
        }
    }
}

fn field_matches_toml(actual: Option<&Value>, expected: &toml::Value) -> bool {
    match expected {
        toml::Value::String(expected) => optional_string_matches(actual, Some(expected.as_str())),
        toml::Value::Integer(expected) => actual.and_then(Value::as_i64) == Some(*expected),
        toml::Value::Float(expected) => actual.and_then(Value::as_f64) == Some(*expected),
        toml::Value::Boolean(expected) => actual.and_then(Value::as_bool) == Some(*expected),
        _ => false,
    }
}

fn optional_string_matches(actual: Option<&Value>, expected: Option<&str>) -> bool {
    let actual = actual.and_then(Value::as_str).unwrap_or("");
    let expected = expected.unwrap_or("");
    actual == expected
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::{
        GaijiAssertion, OracleCase, OracleExpectations, SequenceAssertion, UpstreamObservation,
        UpstreamObservations,
    };
    use serde_json::json;

    #[test]
    fn schema_status_uses_ab_check() {
        assert_eq!(schema_status(&json!({"version": 1})), "fail");
    }

    #[test]
    fn visible_text_uses_ab_ir_aat_view() {
        let aat = json!({
            "version": 1,
            "work_id": "fixture",
            "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}],
            "meta": {
                "adapter": "fixture",
                "adapter_version": "fixture",
                "source_encoding": "utf-8",
                "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
                "parse_complete": true,
                "warnings": []
            }
        });
        let doc = AatDocument::from_value(aat);

        assert_eq!(visible_text(&doc), "本文");
    }

    #[test]
    fn evaluate_case_reports_all_axes() {
        let case = OracleCase {
            id: "gaiji.jis.2-13-47".to_owned(),
            syntax_row_ids: vec!["gaiji.jis_code".to_owned()],
            category: "gaiji".to_owned(),
            source_utf8: "耳朶を※［＃「てへん＋掌」、第4水準2-13-47］えて".to_owned(),
            notes: None,
            oracle: OracleExpectations {
                visible_text: Some("耳朶を撑えて".to_owned()),
                sequence: vec![SequenceAssertion {
                    selector: "blocks.*.content".to_owned(),
                    kinds: vec!["text".to_owned(), "gaiji".to_owned(), "text".to_owned()],
                }],
                gaiji: vec![GaijiAssertion {
                    selector: "blocks.*.content.*".to_owned(),
                    description: "「てへん＋掌」、第4水準2-13-47".to_owned(),
                    resolved: Some("撑".to_owned()),
                    jis_code: Some("2-13-47".to_owned()),
                    unresolved_reason: Some(String::new()),
                    source: None,
                }],
                ..OracleExpectations::default()
            },
        };
        let observations = UpstreamObservations {
            observation: vec![UpstreamObservation {
                case_id: case.id.clone(),
                adapter: "fixture".to_owned(),
                status: "normalised".to_owned(),
                summary: "fixture".to_owned(),
                selector: "blocks.*.content.*".to_owned(),
                kind: "gaiji".to_owned(),
                fields: [("resolved".to_owned(), toml::Value::String("撑".to_owned()))]
                    .into_iter()
                    .collect(),
                evidence: None,
            }],
        };
        let aat = json!({
            "version": 1,
            "work_id": "fixture",
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "耳朶を"},
                    {"kind": "gaiji", "description": "「てへん＋掌」、第4水準2-13-47", "resolved": "撑", "jis_code": "2-13-47", "unresolved_reason": ""},
                    {"kind": "text", "value": "えて"}
                ]
            }],
            "meta": {
                "adapter": "fixture",
                "adapter_version": "fixture",
                "source_encoding": "utf-8",
                "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
                "parse_complete": true,
                "warnings": []
            }
        });

        let result = evaluate_case(&case, &observations, "fixture", aat);

        assert_eq!(result.schema_status, "pass");
        assert_eq!(result.upstream_status, "faithful");
        assert_eq!(result.oracle_status, "pass");
        assert!(result.failures.is_empty());
    }
}
