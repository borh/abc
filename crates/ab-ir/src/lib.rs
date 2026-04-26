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
    Text(String),
    Ruby {
        base: String,
        reading: String,
    },
    Gaiji {
        description: String,
        resolved: String,
        description_format: Option<&'static str>,
    },
    Style {
        style_type: &'static str,
        content: Vec<Inline>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectedText {
    pub visible_text: String,
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
            Inline::Text(value) => json!({ "kind": "text", "value": value }),
            Inline::Ruby { base, reading } => {
                json!({ "kind": "ruby", "base": base, "reading": reading })
            }
            Inline::Gaiji {
                description,
                resolved,
                description_format,
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
                value
            }
            Inline::Style {
                style_type,
                content,
            } => json!({
                "kind": "style",
                "style_type": style_type,
                "content": inline_to_aat_json(content)
            }),
        })
        .collect()
}

fn collect_visible(value: &Inline, out: &mut String) {
    match value {
        Inline::Text(value) => out.push_str(value),
        Inline::Ruby { base, .. } => out.push_str(base),
        Inline::Gaiji { resolved, .. } => out.push_str(resolved),
        Inline::Style { content, .. } => {
            for child in content {
                collect_visible(child, out);
            }
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
                Inline::Ruby {
                    base: "吾輩".to_owned(),
                    reading: "わがはい".to_owned(),
                },
                Inline::Text("は猫である。".to_owned()),
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
                Inline::Ruby {
                    base: "吾輩".to_owned(),
                    reading: "わがはい".to_owned(),
                },
                Inline::Gaiji {
                    description: "「口＋世」、U+546D".to_owned(),
                    resolved: "呻".to_owned(),
                    description_format: None,
                },
            ],
        }];

        assert_eq!(visible_projection(&blocks).visible_text, "吾輩呻");
    }
}
