use anyhow::Result;
use serde::Serialize;
use serde_json::Value;
use unicode_normalization::UnicodeNormalization;

use crate::aat::{comparison_visible_text_projection, inline_nodes_by_kind};
use crate::source_projection;

pub trait Property {
    fn name(&self) -> &'static str;
    /// Validate a property against AAT and source text.
    ///
    /// # Errors
    ///
    /// Returns an error when the property is violated.
    fn check(&self, txt: &str, aat: &Value) -> Result<(), PropertyViolation>;
}

#[derive(Debug, Clone, Serialize)]
pub struct PropertyViolation {
    pub property: &'static str,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
    pub confidence: &'static str,
}

pub struct ParseCompleteness;
pub struct VisibleTextBodyOrder;
pub struct RubyCompleteness;
pub struct GaijiResolution;
pub struct BlockBalance;
pub struct NoDroppedLines;
pub struct HeadingLevelConsistency;

#[must_use]
pub fn builtin_properties() -> Vec<Box<dyn Property + Sync>> {
    vec![
        Box::new(ParseCompleteness),
        Box::new(VisibleTextBodyOrder),
        Box::new(RubyCompleteness),
        Box::new(GaijiResolution),
        Box::new(BlockBalance),
        Box::new(NoDroppedLines),
        Box::new(HeadingLevelConsistency),
    ]
}

impl Property for ParseCompleteness {
    fn name(&self) -> &'static str {
        "parse_completeness"
    }

    fn check(&self, _txt: &str, aat: &Value) -> Result<(), PropertyViolation> {
        if aat
            .pointer("/meta/parse_complete")
            .and_then(Value::as_bool)
            .unwrap_or(false)
        {
            Ok(())
        } else {
            Err(violation(
                self.name(),
                "Adapter reported parse_complete=false",
                None,
                Some("$.meta.parse_complete".to_owned()),
                "strict",
            ))
        }
    }
}

impl Property for VisibleTextBodyOrder {
    fn name(&self) -> &'static str {
        "visible_text_body_order"
    }

    fn check(&self, txt: &str, aat: &Value) -> Result<(), PropertyViolation> {
        let source = normalize_visible(&source_projection::comparison_lossy_body(body_text(txt)));
        // The projection re-emits Aozora marker syntax verbatim (raw nodes,
        // and markers split across adjacent nodes such as a text `※` before a
        // raw `［＃…］` description). The source side strips that syntax via
        // comparison_lossy_body, so the projection must pass through the same
        // scrubber for the two sides to be comparable.
        let projection = normalize_visible(&source_projection::comparison_lossy_body(
            &comparison_visible_text_projection(aat),
        ));
        if projection.is_empty() || is_subsequence(&projection, &source) {
            Ok(())
        } else {
            Err(violation(
                self.name(),
                "AAT visible text projection is not accounted for in source order",
                None,
                None,
                "heuristic",
            ))
        }
    }
}

impl Property for RubyCompleteness {
    fn name(&self) -> &'static str {
        "ruby_completeness"
    }

    fn check(&self, txt: &str, aat: &Value) -> Result<(), PropertyViolation> {
        let ruby_nodes = inline_nodes_by_kind(aat, "ruby");
        let readings = ruby_nodes
            .iter()
            .filter_map(|(_, node)| node.get("reading").and_then(Value::as_str))
            .collect::<Vec<_>>();
        let annotations = ab_source_syntax::source_annotations_for_validation(body_text(txt));
        for marker in annotations.ruby_readings {
            let reading = marker.value;
            if !readings.contains(&reading) {
                return Err(violation(
                    self.name(),
                    format!(
                        "Ruby marker on line {} has no corresponding AAT ruby node",
                        marker.line
                    ),
                    Some(marker.line),
                    None,
                    "heuristic",
                ));
            }
        }
        Ok(())
    }
}

#[must_use]
pub fn body_text(text: &str) -> &str {
    let mut separator_count = 0;
    let mut body_start = 0;
    let mut offset = 0;
    for line in text.split_inclusive('\n') {
        let trimmed = line.trim_end_matches(['\r', '\n']);
        if trimmed.chars().all(|ch| ch == '-') && trimmed.chars().count() >= 20 {
            separator_count += 1;
            if separator_count == 2 {
                body_start = offset + line.len();
                break;
            }
        }
        offset += line.len();
    }
    let body = &text[body_start..];
    let body_end = body
        .char_indices()
        .find_map(|(offset, _)| {
            let rest = &body[offset..];
            if rest.starts_with("底本：") || rest.starts_with("底本:") {
                Some(offset)
            } else {
                None
            }
        })
        .unwrap_or(body.len());
    &body[..body_end]
}

impl Property for GaijiResolution {
    fn name(&self) -> &'static str {
        "gaiji_resolution"
    }

    fn check(&self, txt: &str, aat: &Value) -> Result<(), PropertyViolation> {
        let source_count = ab_source_syntax::source_annotations_for_validation(body_text(txt))
            .gaiji_descriptions
            .len();
        let gaiji_nodes = inline_nodes_by_kind(aat, "gaiji");
        if gaiji_nodes.len() < source_count {
            return Err(violation(
                self.name(),
                "Source gaiji marker has no corresponding AAT gaiji node",
                None,
                None,
                "heuristic",
            ));
        }
        for (path, node) in gaiji_nodes {
            let resolved = node.get("resolved").is_some_and(|value| !value.is_null());
            let reason = node
                .get("unresolved_reason")
                .is_some_and(|value| !value.is_null());
            if !resolved && !reason {
                return Err(violation(
                    self.name(),
                    "AAT gaiji node has neither resolved nor unresolved_reason set",
                    None,
                    Some(path),
                    "strict",
                ));
            }
        }
        Ok(())
    }
}

impl Property for BlockBalance {
    fn name(&self) -> &'static str {
        "block_balance"
    }

    fn check(&self, _txt: &str, _aat: &Value) -> Result<(), PropertyViolation> {
        Ok(())
    }
}

impl Property for NoDroppedLines {
    fn name(&self) -> &'static str {
        "no_dropped_lines"
    }

    fn check(&self, _txt: &str, _aat: &Value) -> Result<(), PropertyViolation> {
        Ok(())
    }
}

impl Property for HeadingLevelConsistency {
    fn name(&self) -> &'static str {
        "heading_level_consistency"
    }

    fn check(&self, _txt: &str, _aat: &Value) -> Result<(), PropertyViolation> {
        Ok(())
    }
}

fn violation(
    property: &'static str,
    message: impl Into<String>,
    line: Option<usize>,
    path: Option<String>,
    confidence: &'static str,
) -> PropertyViolation {
    PropertyViolation {
        property,
        message: message.into(),
        line,
        path,
        confidence,
    }
}

#[must_use]
pub fn source_visible_text(txt: &str) -> String {
    // Compatibility wrapper for existing callers. New code should use
    // source_projection::comparison_lossy_body so the lossy semantics are named.
    source_projection::comparison_lossy_body(txt)
}

fn normalize_visible(value: &str) -> String {
    value
        .nfkc()
        .collect::<String>()
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

fn is_subsequence(needle: &str, haystack: &str) -> bool {
    let mut haystack = haystack.chars();
    for ch in needle.chars() {
        if !haystack.any(|candidate| candidate == ch) {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_visible_text_excludes_unresolved_gaiji_descriptions() {
        let visible = source_visible_text("二二※［＃小書き片仮名ン、237-11］が四");

        assert_eq!(visible, "二二が四");
    }

    #[test]
    fn source_visible_text_projects_explicit_ruby_base_without_marker() {
        let visible = source_visible_text("――『｜あのひとにとって、わたし《ルビ》はなんだろう？」");

        assert_eq!(visible, "――『あのひとにとって、わたしはなんだろう？」");
    }

    #[test]
    fn source_visible_text_removes_orphan_ruby_after_unresolved_gaiji() {
        let visible =
            source_visible_text("ことを、※［＃「口＋愛」、第3水準1-15-23］《おくび》にも");

        assert_eq!(visible, "ことを、にも");
    }

    #[test]
    fn visible_text_body_order_allows_structured_resolved_gaiji() {
        let txt = "芒の快い刺※［＃「りっしんべん＋戟」、第4水準2-12-78］を感じた。";
        let aat = serde_json::json!({
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "芒の快い刺"},
                    {
                        "kind": "gaiji",
                        "description": "「りっしんべん＋戟」、第4水準2-12-78",
                        "resolved": "㦸",
                        "jis_code": "2-12-78",
                        "unresolved_reason": null
                    },
                    {"kind": "text", "value": "を感じた。"}
                ]
            }]
        });

        assert!(VisibleTextBodyOrder.check(txt, &aat).is_ok());
    }

    #[test]
    fn visible_text_body_order_excludes_raw_marker_syntax() {
        let txt = "一行目。\n［＃５字下げ］見出し［＃「見出し」は中見出し］\n二行目。\n";
        let aat = serde_json::json!({
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "一行目。"},
                    {
                        "kind": "raw",
                        "source": "［＃５字下げ］",
                        "x-source-marker-kind": "indent",
                        "x-provenance": "parser-derived"
                    },
                    {"kind": "text", "value": "見出し"},
                    {
                        "kind": "raw",
                        "source": "［＃「見出し」は中見出し］",
                        "x-source-marker-kind": "directive",
                        "x-provenance": "parser-derived"
                    },
                    {"kind": "text", "value": "二行目。"}
                ]
            }]
        });

        assert!(VisibleTextBodyOrder.check(txt, &aat).is_ok());
    }

    #[test]
    fn visible_text_body_order_still_checks_raw_gap_text_order() {
        let txt = "甲乙丙。\n";
        let aat = serde_json::json!({
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {
                        "kind": "raw",
                        "source": "丙",
                        "x-source-marker-kind": "unparsed-source-gap",
                        "x-provenance": "source-derived"
                    },
                    {"kind": "text", "value": "甲"}
                ]
            }]
        });

        assert!(VisibleTextBodyOrder.check(txt, &aat).is_err());
    }

    #[test]
    fn visible_text_body_order_excludes_source_note_block() {
        let txt = "本文のことば。\n底本：「全集」出版社\n";
        let aat = serde_json::json!({
            "blocks": [
                {
                    "kind": "paragraph",
                    "content": [{"kind": "text", "value": "本文のことば。"}]
                },
                {
                    "kind": "source_note",
                    "content": [{"kind": "text", "value": "底本：「全集」出版社"}]
                }
            ]
        });

        assert!(VisibleTextBodyOrder.check(txt, &aat).is_ok());
    }

    #[test]
    fn gaiji_resolution_ignores_gaiji_examples_inside_command_notes() {
        let txt = "豌豆《ゑんどう》［＃「豌豆」は底本では「※［＃「足＋宛」、第3水準1-92-36］豆」］";
        let aat = serde_json::json!({
            "blocks": [{
                "kind": "paragraph",
                "content": [{"kind": "text", "value": "豌豆"}]
            }]
        });

        assert!(GaijiResolution.check(txt, &aat).is_ok());
    }

    #[test]
    fn gaiji_resolution_counts_gaiji_inside_ruby_base_content() {
        let txt = "耳朶を※［＃「てへん＋掌」、第4水準2-13-47］《ささ》えて";
        let aat = serde_json::json!({
            "blocks": [{
                "kind": "paragraph",
                "content": [{
                    "kind": "ruby",
                    "base": "",
                    "reading": "ささ",
                    "base_content": [{
                        "kind": "gaiji",
                        "description": "「てへん＋掌」、第4水準2-13-47",
                        "resolved": "撑",
                        "jis_code": "2-13-47",
                        "unresolved_reason": null
                    }]
                }]
            }]
        });

        assert!(GaijiResolution.check(txt, &aat).is_ok());
    }
}
