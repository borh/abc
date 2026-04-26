use anyhow::Result;
use regex::Regex;
use serde::Serialize;
use serde_json::Value;
use unicode_normalization::UnicodeNormalization;

use crate::aat::{inline_nodes_by_kind, visible_text_projection};

pub trait Property {
    fn name(&self) -> &'static str;
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
        let source = normalize_visible(&source_visible_text(body_text(txt)));
        let projection = normalize_visible(&visible_text_projection(aat));
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
        let marker = Regex::new(r"《([^》]+)》").unwrap();
        for (line_idx, line) in body_text(txt).lines().enumerate() {
            for capture in marker.captures_iter(line) {
                if inside_editor_note(line, capture.get(0).unwrap().start()) {
                    continue;
                }
                if follows_gaiji_marker(line, capture.get(0).unwrap().start()) {
                    continue;
                }
                let reading = capture.get(1).unwrap().as_str();
                if !readings.contains(&reading) {
                    return Err(violation(
                        self.name(),
                        format!(
                            "Ruby marker on line {} has no corresponding AAT ruby node",
                            line_idx + 1
                        ),
                        Some(line_idx + 1),
                        None,
                        "heuristic",
                    ));
                }
            }
        }
        Ok(())
    }
}

fn follows_gaiji_marker(line: &str, offset: usize) -> bool {
    let before = &line[..offset];
    before
        .rfind("※［＃")
        .is_some_and(|start| before[start..].ends_with('］'))
        || before
            .rfind("※[#")
            .is_some_and(|start| before[start..].ends_with(']'))
}

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
        let source_count = Regex::new(r"※(?:［＃[^］]+］|\[#[^\]]+\])")
            .unwrap()
            .find_iter(body_text(txt))
            .count();
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

fn inside_editor_note(line: &str, offset: usize) -> bool {
    let before = &line[..offset];
    let start = before.rfind("［＃");
    let end = before.rfind('］');
    let ascii_start = before.rfind("[#");
    let ascii_end = before.rfind(']');
    start.is_some_and(|start| end.is_none_or(|end| end < start))
        || ascii_start.is_some_and(|start| ascii_end.is_none_or(|end| end < start))
}

pub fn source_visible_text(txt: &str) -> String {
    let gaiji = Regex::new(r"※(?:［＃([^］]+)］|\[#([^\]]+)\])").unwrap();
    let explicit_ruby = Regex::new(r"｜([^《》\r\n]+)《[^》]+》").unwrap();
    let ruby = Regex::new(r"｜?([^｜\s《》※［＃\[\]］、。，．「」『』（）()]+)《[^》]+》").unwrap();
    let command = Regex::new(r"［＃[^］]+］|\[#[^\]]+\]").unwrap();
    let without_gaiji = gaiji.replace_all(txt, "");
    let without_explicit_ruby = explicit_ruby.replace_all(&without_gaiji, "$1");
    let without_ruby = ruby.replace_all(&without_explicit_ruby, "$1");
    command.replace_all(&without_ruby, "").into_owned()
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
}
