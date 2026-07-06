use serde_json::Value;

#[derive(Debug, Clone)]
pub enum VisibleFragment<'a> {
    Text {
        value: String,
        path: String,
        node: &'a Value,
    },
    Gaiji {
        resolved: Option<String>,
        description: String,
        has_unresolved_reason: bool,
        path: String,
        node: &'a Value,
    },
}

#[must_use]
pub fn visible_text_projection(aat: &Value) -> String {
    ab_plaintext::visible_text_projection(aat)
}

pub fn comparison_visible_text_projection(aat: &Value) -> String {
    let mut out = String::new();
    if let Some(blocks) = aat.get("blocks").and_then(Value::as_array) {
        for block in blocks {
            collect_comparison_block(block, &mut out);
        }
    }
    out
}

pub fn visible_text_fragments<'a>(aat: &'a Value) -> Vec<VisibleFragment<'a>> {
    let mut fragments = Vec::new();
    if let Some(blocks) = aat.get("blocks").and_then(Value::as_array) {
        for (idx, block) in blocks.iter().enumerate() {
            collect_block(block, &format!("$.blocks[{idx}]"), &mut fragments);
        }
    }
    fragments
}

pub fn inline_nodes_by_kind<'a>(aat: &'a Value, kind: &str) -> Vec<(String, &'a Value)> {
    let mut nodes = Vec::new();
    if let Some(blocks) = aat.get("blocks").and_then(Value::as_array) {
        for (idx, block) in blocks.iter().enumerate() {
            collect_inline_kind(block, &format!("$.blocks[{idx}]"), kind, &mut nodes);
        }
    }
    nodes
}

#[must_use]
pub fn node_line(node: &Value) -> Option<usize> {
    node.get("span")?
        .get("line_start")?
        .as_u64()
        .and_then(|line| usize::try_from(line).ok())
}

fn collect_comparison_block(node: &Value, out: &mut String) {
    if let Some(content) = node.get("content").and_then(Value::as_array) {
        for inline in content {
            collect_comparison_inline(inline, out);
        }
    }
    if let Some(children) = node.get("children").and_then(Value::as_array) {
        for child in children {
            collect_comparison_block(child, out);
        }
    }
}

fn collect_comparison_inline(node: &Value, out: &mut String) {
    match node.get("kind").and_then(Value::as_str).unwrap_or("") {
        "text" => push_string_field(node, "value", out),
        "ruby" => {
            if let Some(base_content) = node.get("base_content").and_then(Value::as_array) {
                for inline in base_content {
                    collect_comparison_inline(inline, out);
                }
            } else {
                push_string_field(node, "base", out);
            }
        }
        "gaiji" => {}
        "style" if node.get("style_type").and_then(Value::as_str) == Some("notes") => {}
        "accent" => {
            if let Some(resolved) = node
                .get("resolved")
                .and_then(Value::as_str)
                .filter(|value| !value.is_empty())
            {
                out.push_str(resolved);
            } else {
                push_string_field(node, "name", out);
            }
        }
        "raw" => push_string_field(node, "source", out),
        "warigaki" => {
            for key in ["upper", "lower"] {
                if let Some(content) = node.get(key).and_then(Value::as_array) {
                    for inline in content {
                        collect_comparison_inline(inline, out);
                    }
                }
            }
        }
        "figure" => {
            if let Some(caption) = node.get("caption").and_then(Value::as_array) {
                for inline in caption {
                    collect_comparison_inline(inline, out);
                }
            }
        }
        _ => {
            if let Some(content) = node.get("content").and_then(Value::as_array) {
                for inline in content {
                    collect_comparison_inline(inline, out);
                }
            }
        }
    }
}

fn push_string_field(node: &Value, key: &str, out: &mut String) {
    if let Some(value) = node.get(key).and_then(Value::as_str) {
        out.push_str(value);
    }
}

fn collect_block<'a>(node: &'a Value, path: &str, fragments: &mut Vec<VisibleFragment<'a>>) {
    if let Some(content) = node.get("content").and_then(Value::as_array) {
        for (idx, inline) in content.iter().enumerate() {
            collect_inline(inline, &format!("{path}.content[{idx}]"), fragments);
        }
    }
    if let Some(children) = node.get("children").and_then(Value::as_array) {
        for (idx, child) in children.iter().enumerate() {
            collect_block(child, &format!("{path}.children[{idx}]"), fragments);
        }
    }
}

fn collect_inline<'a>(node: &'a Value, path: &str, fragments: &mut Vec<VisibleFragment<'a>>) {
    let kind = node.get("kind").and_then(Value::as_str).unwrap_or("");
    match kind {
        "text" => fragments.push(VisibleFragment::Text {
            value: node
                .get("value")
                .and_then(Value::as_str)
                .unwrap_or_default()
                .to_owned(),
            path: path.to_owned(),
            node,
        }),
        "ruby" => fragments.push(VisibleFragment::Text {
            value: node
                .get("base")
                .and_then(Value::as_str)
                .unwrap_or_default()
                .to_owned(),
            path: path.to_owned(),
            node,
        }),
        "gaiji" => fragments.push(VisibleFragment::Gaiji {
            resolved: node
                .get("resolved")
                .and_then(Value::as_str)
                .map(ToOwned::to_owned),
            description: node
                .get("description")
                .and_then(Value::as_str)
                .unwrap_or_default()
                .to_owned(),
            has_unresolved_reason: node
                .get("unresolved_reason")
                .is_some_and(|value| !value.is_null()),
            path: path.to_owned(),
            node,
        }),
        "raw" => fragments.push(VisibleFragment::Text {
            value: node
                .get("source")
                .and_then(Value::as_str)
                .unwrap_or_default()
                .to_owned(),
            path: path.to_owned(),
            node,
        }),
        "warigaki" => {
            for key in ["upper", "lower"] {
                if let Some(content) = node.get(key).and_then(Value::as_array) {
                    for (idx, inline) in content.iter().enumerate() {
                        collect_inline(inline, &format!("{path}.{key}[{idx}]"), fragments);
                    }
                }
            }
        }
        _ => {
            if let Some(content) = node.get("content").and_then(Value::as_array) {
                for (idx, inline) in content.iter().enumerate() {
                    collect_inline(inline, &format!("{path}.content[{idx}]"), fragments);
                }
            }
        }
    }
}

fn collect_inline_kind<'a>(
    node: &'a Value,
    path: &str,
    kind: &str,
    nodes: &mut Vec<(String, &'a Value)>,
) {
    if node.get("kind").and_then(Value::as_str) == Some(kind) {
        nodes.push((path.to_owned(), node));
    }
    if let Some(content) = node.get("content").and_then(Value::as_array) {
        for (idx, inline) in content.iter().enumerate() {
            let inline_path = format!("{path}.content[{idx}]");
            collect_inline_kind(inline, &inline_path, kind, nodes);
        }
    }
    for key in ["children", "upper", "lower", "base_content", "caption"] {
        if let Some(children) = node.get(key).and_then(Value::as_array) {
            for (idx, child) in children.iter().enumerate() {
                collect_inline_kind(child, &format!("{path}.{key}[{idx}]"), kind, nodes);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn fragments_to_text(aat: &Value) -> String {
        visible_text_fragments(aat)
            .into_iter()
            .map(|fragment| match fragment {
                VisibleFragment::Text { value, .. } => value,
                VisibleFragment::Gaiji {
                    resolved,
                    description,
                    has_unresolved_reason,
                    ..
                } => resolved.unwrap_or_else(|| {
                    if has_unresolved_reason {
                        String::new()
                    } else {
                        description
                    }
                }),
            })
            .collect()
    }

    #[test]
    fn fragment_projection_matches_plaintext_projection() {
        let aat = json!({
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "A"},
                    {"kind": "ruby", "base": "B", "reading": "ビー"},
                    {"kind": "gaiji", "description": "desc", "resolved": "C"},
                    {"kind": "gaiji", "description": "empty", "resolved": ""},
                    {
                        "kind": "gaiji",
                        "description": "null",
                        "resolved": null,
                        "unresolved_reason": "image_fallback"
                    },
                    {"kind": "gaiji", "description": "D"},
                    {"kind": "raw", "source": "E"},
                    {
                        "kind": "warigaki",
                        "upper": [{"kind": "text", "value": "F"}],
                        "lower": [{"kind": "text", "value": "G"}]
                    },
                    {"kind": "style", "content": [{"kind": "text", "value": "H"}]}
                ]
            }]
        });

        assert_eq!(
            fragments_to_text(&aat),
            ab_plaintext::visible_text_projection(&aat)
        );
        assert_eq!(fragments_to_text(&aat), "ABCDEFGH");
    }

    #[test]
    fn visible_projection_excludes_unresolved_gaiji_descriptions() {
        let aat = json!({
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "二二"},
                    {"kind": "gaiji", "description": "小書き片仮名ン、237-11", "resolved": ""},
                    {"kind": "text", "value": "が四"}
                ]
            }]
        });

        assert_eq!(visible_text_projection(&aat), "二二が四");
    }
}
