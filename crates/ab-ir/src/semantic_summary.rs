use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::{block_content, walk_inline, Inline, InlineVisitor, GaijiRef, Block, BreakKind, ProjectionWarning};

/// Parser-neutral semantic summary serialized into AAT `meta.semantic_summary`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SemanticSummary {
    pub syntax: BTreeMap<String, Vec<SemanticSummaryNode>>,
}

/// One matrix-keyed semantic summary entry.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SemanticSummaryNode {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_span: Option<SourceSpan>,
    pub kind: String,
    pub value: serde_json::Value,
    pub provenance: String,
}

/// Byte offsets into decoded UTF-8 source text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
}

#[must_use]
pub fn semantic_summary(blocks: &[Block], warnings: &[ProjectionWarning]) -> SemanticSummary {
    let mut summary = SemanticSummary {
        syntax: BTreeMap::new(),
    };
    for block in blocks {
        collect_block(block, &mut summary);
        collect_inlines(block_content(block), &mut summary);
    }
    for warning in warnings {
        push_node(
            &mut summary,
            "projection.warning",
            SemanticSummaryNode {
                source_span: None,
                kind: "projection_warning".to_owned(),
                value: json!({
                    "syntax_id": warning.syntax_id,
                    "message": warning.message,
                }),
                provenance: "projection".to_owned(),
            },
        );
    }
    summary
}

fn collect_block(block: &Block, summary: &mut SemanticSummary) {
    match block {
        Block::Paragraph { .. } | Block::Heading { .. } | Block::CaptionBlock { .. } => {}
        Block::Jisage { level, content } => {
            push_node(
                summary,
                "indentation.jisage_block",
                SemanticSummaryNode {
                    source_span: None,
                    kind: "jisage_block".to_owned(),
                    value: json!({
                        "indent": level,
                        "text": inline_visible_text(content),
                    }),
                    provenance: "parser".to_owned(),
                },
            );
        }
        Block::Warichu { content } => {
            push_node(
                summary,
                "warichu.basic",
                SemanticSummaryNode {
                    source_span: None,
                    kind: "warichu".to_owned(),
                    value: json!({
                        "text": inline_visible_text(content),
                    }),
                    provenance: "parser".to_owned(),
                },
            );
        }
        Block::Figure { source, content } => {
            let key = if content.is_empty() {
                "figure.image_inline"
            } else {
                "figure.image_caption"
            };
            push_node(
                summary,
                key,
                SemanticSummaryNode {
                    source_span: None,
                    kind: "figure".to_owned(),
                    value: json!({
                        "source": source,
                        "caption": inline_visible_text(content),
                    }),
                    provenance: "parser".to_owned(),
                },
            );
        }
        Block::Break { kind, .. } => {
            let key = match kind {
                BreakKind::Page => "break.page_line",
                BreakKind::Line => "break.line_explicit",
            };
            push_node(
                summary,
                key,
                SemanticSummaryNode {
                    source_span: None,
                    kind: "break".to_owned(),
                    value: json!({"break_kind": kind.as_str()}),
                    provenance: "parser".to_owned(),
                },
            );
        }
    }
}

fn collect_inlines(nodes: &[Inline], summary: &mut SemanticSummary) {
    let mut collector = SummaryCollector {
        summary,
        ruby_reading: Vec::new(),
    };
    for node in nodes {
        walk_inline(&mut collector, node);
    }
}

struct SummaryCollector<'a> {
    summary: &'a mut SemanticSummary,
    ruby_reading: Vec<String>,
}

impl<'a> InlineVisitor for SummaryCollector<'a> {
    fn enter_ruby(
        &mut self,
        base: &[Inline],
        reading: &str,
        placement: &crate::RubyPlacement,
        provenance: &crate::Provenance,
    ) {
        push_node(
            self.summary,
            "ruby.basic",
            SemanticSummaryNode {
                source_span: None,
                kind: "ruby".to_owned(),
                value: json!({
                    "base_projection": inline_visible_text(base),
                    "reading": reading,
                    "placement": placement.as_str(),
                }),
                provenance: provenance.as_str().to_owned(),
            },
        );
        if contains_gaiji(base) {
            push_node(
                self.summary,
                "gaiji_ruby.inline_base",
                SemanticSummaryNode {
                    source_span: None,
                    kind: "gaiji_ruby".to_owned(),
                    value: json!({
                        "base_projection": inline_visible_text(base),
                        "reading": reading,
                        "placement": placement.as_str(),
                    }),
                    provenance: provenance.as_str().to_owned(),
                },
            );
        }
        self.ruby_reading.push(reading.to_owned());
    }

    fn leave_ruby(&mut self) {
        self.ruby_reading.pop();
    }

    fn enter_gaiji_ref(&mut self, gaiji: &GaijiRef) {
        let ruby_reading = self.ruby_reading.last().map(String::as_str);
        collect_gaiji(gaiji, ruby_reading, self.summary);
    }
}

struct ContainsGaiji {
    pub found: bool,
}

impl InlineVisitor for ContainsGaiji {
    fn enter_gaiji_ref(&mut self, _gaiji: &GaijiRef) {
        self.found = true;
    }
}

fn collect_gaiji(gaiji: &GaijiRef, ruby_reading: Option<&str>, summary: &mut SemanticSummary) {
    push_node(
        summary,
        "gaiji.marker",
        SemanticSummaryNode {
            source_span: None,
            kind: "gaiji".to_owned(),
            value: json!({
                "source": &gaiji.source,
                "description": &gaiji.description,
                "description_format": &gaiji.description_format,
                "kind": format!("{:?}", &gaiji.kind),
                "resolved": &gaiji.resolved,
                "ruby_reading": ruby_reading,
            }),
            provenance: gaiji.provenance.as_str().to_owned(),
        },
    );
}

fn push_node(summary: &mut SemanticSummary, syntax_id: &str, node: SemanticSummaryNode) {
    summary
        .syntax
        .entry(syntax_id.to_owned())
        .or_default()
        .push(node);
}

fn contains_gaiji(content: &[Inline]) -> bool {
    let mut visitor = ContainsGaiji { found: false };
    for node in content {
        walk_inline(&mut visitor, node);
        if visitor.found {
            return true;
        }
    }
    false
}

fn inline_visible_text(content: &[Inline]) -> String {
    super::inline_visible_text(content)
}
