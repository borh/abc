use serde_json::json;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Block {
    Paragraph {
        content: Vec<Inline>,
    },
    Heading {
        level: u8,
        style: &'static str,
        content: Vec<Inline>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Inline {
    Text {
        value: String,
        provenance: Provenance,
    },
    Ruby {
        base: String,
        reading: String,
        provenance: Provenance,
    },
    Gaiji {
        description: String,
        resolved: String,
        description_format: Option<&'static str>,
        provenance: Provenance,
    },
    Style {
        style_type: &'static str,
        content: Vec<Inline>,
        provenance: Provenance,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Provenance {
    Parser,
    ParserNormalized,
    RegexSupplement,
    RegexFallback,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectedText {
    pub visible_text: String,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct ProvenanceCounts {
    pub parser: usize,
    pub parser_normalized: usize,
    pub regex_supplement: usize,
    pub regex_fallback: usize,
}

impl Inline {
    pub fn text(value: impl Into<String>) -> Self {
        Self::text_with_provenance(value, Provenance::Parser)
    }

    pub fn text_with_provenance(value: impl Into<String>, provenance: Provenance) -> Self {
        Self::Text {
            value: value.into(),
            provenance,
        }
    }

    pub fn ruby(base: impl Into<String>, reading: impl Into<String>) -> Self {
        Self::ruby_with_provenance(base, reading, Provenance::Parser)
    }

    pub fn ruby_with_provenance(
        base: impl Into<String>,
        reading: impl Into<String>,
        provenance: Provenance,
    ) -> Self {
        Self::Ruby {
            base: base.into(),
            reading: reading.into(),
            provenance,
        }
    }

    pub fn gaiji(
        description: impl Into<String>,
        resolved: impl Into<String>,
        description_format: Option<&'static str>,
    ) -> Self {
        Self::gaiji_with_provenance(
            description,
            resolved,
            description_format,
            Provenance::Parser,
        )
    }

    pub fn gaiji_with_provenance(
        description: impl Into<String>,
        resolved: impl Into<String>,
        description_format: Option<&'static str>,
        provenance: Provenance,
    ) -> Self {
        Self::Gaiji {
            description: description.into(),
            resolved: resolved.into(),
            description_format,
            provenance,
        }
    }

    pub fn style(style_type: &'static str, content: Vec<Inline>) -> Self {
        Self::Style {
            style_type,
            content,
            provenance: Provenance::Parser,
        }
    }
}

pub fn blocks_to_aat_json(blocks: &[Block]) -> Vec<serde_json::Value> {
    blocks
        .iter()
        .map(|block| match block {
            Block::Paragraph { content } => json!({
                "kind": "paragraph",
                "content": inline_to_aat_json(content)
            }),
            Block::Heading {
                level,
                style,
                content,
            } => json!({
                "kind": "heading",
                "level": level,
                "style": style,
                "content": inline_to_aat_json(content)
            }),
        })
        .collect()
}

pub fn visible_projection(blocks: &[Block]) -> ProjectedText {
    let mut visible_text = String::new();
    for block in blocks {
        for child in block_content(block) {
            collect_visible(child, &mut visible_text);
        }
    }
    ProjectedText { visible_text }
}

fn inline_to_aat_json(content: &[Inline]) -> Vec<serde_json::Value> {
    content
        .iter()
        .map(|node| match node {
            Inline::Text { value, provenance } => {
                with_provenance(json!({ "kind": "text", "value": value }), *provenance)
            }
            Inline::Ruby {
                base,
                reading,
                provenance,
            } => with_provenance(
                json!({ "kind": "ruby", "base": base, "reading": reading }),
                *provenance,
            ),
            Inline::Gaiji {
                description,
                resolved,
                description_format,
                provenance,
            } => {
                let mut value = json!({
                    "kind": "gaiji",
                    "description": description,
                    "resolved": resolved,
                    "jis_code": null,
                    "unresolved_reason": null
                });
                if let Some(format) = description_format {
                    value["x-description-format"] = serde_json::Value::String((*format).to_owned());
                }
                with_provenance(value, *provenance)
            }
            Inline::Style {
                style_type,
                content,
                provenance,
            } => with_provenance(
                json!({
                    "kind": "style",
                    "style_type": style_type,
                    "content": inline_to_aat_json(content)
                }),
                *provenance,
            ),
        })
        .collect()
}

fn collect_visible(value: &Inline, out: &mut String) {
    match value {
        Inline::Text { value, .. } => out.push_str(value),
        Inline::Ruby { base, .. } => out.push_str(base),
        Inline::Gaiji { resolved, .. } => out.push_str(resolved),
        Inline::Style { content, .. } => {
            for child in content {
                collect_visible(child, out);
            }
        }
    }
}

pub fn provenance_counts(blocks: &[Block]) -> ProvenanceCounts {
    let mut counts = ProvenanceCounts::default();
    for block in blocks {
        for child in block_content(block) {
            collect_provenance(child, &mut counts);
        }
    }
    counts
}

fn collect_provenance(value: &Inline, counts: &mut ProvenanceCounts) {
    match inline_provenance(value) {
        Provenance::Parser => counts.parser += 1,
        Provenance::ParserNormalized => counts.parser_normalized += 1,
        Provenance::RegexSupplement => counts.regex_supplement += 1,
        Provenance::RegexFallback => counts.regex_fallback += 1,
    }
    if let Inline::Style { content, .. } = value {
        for child in content {
            collect_provenance(child, counts);
        }
    }
}

fn inline_provenance(value: &Inline) -> Provenance {
    match value {
        Inline::Text { provenance, .. }
        | Inline::Ruby { provenance, .. }
        | Inline::Gaiji { provenance, .. }
        | Inline::Style { provenance, .. } => *provenance,
    }
}

fn with_provenance(mut value: serde_json::Value, provenance: Provenance) -> serde_json::Value {
    if provenance != Provenance::Parser {
        value["x-provenance"] = serde_json::Value::String(provenance.as_str().to_owned());
    }
    value
}

impl Provenance {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Parser => "parser",
            Self::ParserNormalized => "parser_normalized",
            Self::RegexSupplement => "regex_supplement",
            Self::RegexFallback => "regex_fallback",
        }
    }
}

pub fn block_content(block: &Block) -> &[Inline] {
    match block {
        Block::Paragraph { content } | Block::Heading { content, .. } => content,
    }
}

pub fn block_content_mut(block: &mut Block) -> &mut Vec<Inline> {
    match block {
        Block::Paragraph { content } | Block::Heading { content, .. } => content,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn projects_blocks_to_aat_json_shape() {
        let blocks = vec![Block::Paragraph {
            content: vec![
                Inline::ruby("吾輩", "わがはい"),
                Inline::text("は猫である。"),
            ],
        }];

        let json = blocks_to_aat_json(&blocks);

        assert_eq!(json[0]["kind"], "paragraph");
        assert_eq!(json[0]["content"][0]["kind"], "ruby");
        assert_eq!(json[0]["content"][0]["base"], "吾輩");
        assert_eq!(json[0]["content"][1]["value"], "は猫である。");
    }

    #[test]
    fn visible_projection_uses_ruby_base_and_resolved_gaiji() {
        let blocks = vec![Block::Paragraph {
            content: vec![
                Inline::ruby("吾輩", "わがはい"),
                Inline::gaiji("「口＋世」、U+546D", "呻", None),
            ],
        }];

        assert_eq!(visible_projection(&blocks).visible_text, "吾輩呻");
    }

    #[test]
    fn non_parser_provenance_is_projected_as_extension_metadata() {
        let blocks = vec![Block::Paragraph {
            content: vec![Inline::ruby_with_provenance(
                "",
                "わがはい",
                Provenance::RegexSupplement,
            )],
        }];

        let json = blocks_to_aat_json(&blocks);

        assert_eq!(json[0]["content"][0]["x-provenance"], "regex_supplement");
        assert_eq!(provenance_counts(&blocks).regex_supplement, 1);
    }
}
