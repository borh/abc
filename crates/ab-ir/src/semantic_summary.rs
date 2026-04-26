use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::{Block, GaijiRef, Inline, ProjectionWarning, block_content};

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

pub fn semantic_summary(blocks: &[Block], warnings: &[ProjectionWarning]) -> SemanticSummary {
    let mut summary = SemanticSummary {
        syntax: BTreeMap::new(),
    };
    for block in blocks {
        for child in block_content(block) {
            collect_inline(child, None, &mut summary);
        }
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

fn collect_inline(node: &Inline, ruby_reading: Option<&str>, summary: &mut SemanticSummary) {
    match node {
        Inline::Ruby {
            base,
            reading,
            placement,
            provenance,
        } => {
            push_node(
                summary,
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
                    summary,
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
            for child in base {
                collect_inline(child, Some(reading), summary);
            }
        }
        Inline::GaijiRef(gaiji) => {
            collect_gaiji(gaiji, ruby_reading, summary);
        }
        Inline::Style { content, .. } => {
            for child in content {
                collect_inline(child, ruby_reading, summary);
            }
        }
        Inline::Text { .. } => {}
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
    content.iter().any(|node| match node {
        Inline::GaijiRef(_) => true,
        Inline::Ruby { base, .. } => contains_gaiji(base),
        Inline::Style { content, .. } => contains_gaiji(content),
        Inline::Text { .. } => false,
    })
}

fn inline_visible_text(content: &[Inline]) -> String {
    let mut out = String::new();
    for child in content {
        collect_visible(child, &mut out);
    }
    out
}

fn collect_visible(value: &Inline, out: &mut String) {
    match value {
        Inline::Text { value, .. } => out.push_str(value),
        Inline::Ruby { base, .. } => {
            for child in base {
                collect_visible(child, out);
            }
        }
        Inline::GaijiRef(gaiji) => {
            if let Some(resolved) = &gaiji.resolved {
                out.push_str(resolved);
            }
        }
        Inline::Style { content, .. } => {
            for child in content {
                collect_visible(child, out);
            }
        }
    }
}
