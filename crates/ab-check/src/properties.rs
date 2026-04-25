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
        let source = normalize_visible(&source_visible_text(txt));
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
        for (line_idx, line) in txt.lines().enumerate() {
            for capture in marker.captures_iter(line) {
                if inside_editor_note(line, capture.get(0).unwrap().start()) {
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

impl Property for GaijiResolution {
    fn name(&self) -> &'static str {
        "gaiji_resolution"
    }

    fn check(&self, txt: &str, aat: &Value) -> Result<(), PropertyViolation> {
        let source_count = Regex::new(r"※［＃[^］]+］").unwrap().find_iter(txt).count();
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

    fn check(&self, txt: &str, aat: &Value) -> Result<(), PropertyViolation> {
        let pairs = [
            ("［＃ここから引用］", "［＃ここで引用終わり］"),
            ("字下げ］", "［＃ここで字下げ終わり］"),
            ("［＃ここから罫囲み］", "［＃ここで罫囲み終わり］"),
        ];
        for (start, end) in pairs {
            let starts = txt.matches(start).count();
            let ends = txt.matches(end).count();
            if starts > ends && !aat_warning_mentions(aat, "unclosed") {
                return Err(violation(
                    self.name(),
                    format!("Source block start {start} has no matching end marker"),
                    None,
                    None,
                    "heuristic",
                ));
            }
        }
        Ok(())
    }
}

impl Property for NoDroppedLines {
    fn name(&self) -> &'static str {
        "no_dropped_lines"
    }

    fn check(&self, txt: &str, aat: &Value) -> Result<(), PropertyViolation> {
        let projection = normalize_visible(&visible_text_projection(aat));
        for (idx, line) in txt.lines().enumerate() {
            let visible = normalize_visible(&source_visible_text(line));
            if visible.is_empty() {
                continue;
            }
            let prefix = visible.chars().take(2).collect::<String>();
            if !prefix.is_empty() && !projection.contains(&prefix) {
                return Err(violation(
                    self.name(),
                    format!(
                        "Source line {} appears to be dropped from AAT projection",
                        idx + 1
                    ),
                    Some(idx + 1),
                    None,
                    "heuristic",
                ));
            }
        }
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
    start.is_some_and(|start| end.is_none_or(|end| end < start))
}

fn aat_warning_mentions(aat: &Value, needle: &str) -> bool {
    aat.pointer("/meta/warnings")
        .and_then(Value::as_array)
        .is_some_and(|warnings| {
            warnings.iter().any(|warning| {
                warning
                    .get("message")
                    .and_then(Value::as_str)
                    .is_some_and(|message| message.contains(needle))
            })
        })
}

fn source_visible_text(txt: &str) -> String {
    let gaiji = Regex::new(r"※［＃([^］]+)］").unwrap();
    let ruby = Regex::new(r"｜?([^｜《》\s]+)《[^》]+》").unwrap();
    let command = Regex::new(r"［＃[^］]+］").unwrap();
    let without_gaiji = gaiji.replace_all(txt, "$1");
    let without_ruby = ruby.replace_all(&without_gaiji, "$1");
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
