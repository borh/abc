use anyhow::{Result, bail};
use serde_json::Value;

/// Unknown glyphs occupy one object-replacement character; descriptions remain metadata.
pub(crate) fn unicode_or_placeholder(unicode: Option<&str>) -> &str {
    unicode.unwrap_or("\u{fffc}")
}

/// Visible text for analysis consumers, derived from structured children when present.
pub(crate) fn parser_ir_node_visible_text(node: &Value) -> Result<String> {
    if let Some(children) = inline_children(node) {
        let mut text = String::new();
        for child in children {
            text.push_str(&parser_ir_node_visible_text(child)?);
        }
        return Ok(text);
    }
    if node_type(node) == "warichu" {
        let mut text = String::new();
        for key in ["upper_children", "lower_children"] {
            if let Some(children) = node.get(key).and_then(Value::as_array) {
                for child in children {
                    text.push_str(&parser_ir_node_visible_text(child)?);
                }
            }
        }
        return Ok(text);
    }
    let node_type = node_type(node);
    let text = match node_type {
        "text" | "iteration-mark" | "supplied-diacritic" | "quote" | "emphasis" | "layout-span"
        | "heading" | "source-note" => node.get("text").and_then(Value::as_str).unwrap_or(""),
        "ruby" => node
            .pointer("/ruby/base")
            .and_then(Value::as_str)
            .unwrap_or(""),
        "gaiji" => unicode_or_placeholder(node.pointer("/gaiji/unicode").and_then(Value::as_str)),
        "line-break" => "\n",
        "page-break" | "image" | "editor-note" | "kunten" | "indentation" => "",
        other => bail!("unsupported parser-IR node type for visible-text projection: {other}"),
    };
    Ok(text.to_owned())
}

/// The non-empty `inline_children` array of an inline container, if present.
fn inline_children(node: &Value) -> Option<&Vec<Value>> {
    node.get("inline_children")
        .and_then(Value::as_array)
        .filter(|children| !children.is_empty())
}

fn node_type(node: &Value) -> &str {
    node.get("type")
        .and_then(Value::as_str)
        .unwrap_or("unknown")
}
