use serde_json::Value;

use crate::{PlainTextDocument, PlainTextError, SourceFormat, canonicalize_line_endings};

pub fn from_aat_value(aat: &Value) -> Result<PlainTextDocument, PlainTextError> {
    let text_id = aat
        .get("work_id")
        .and_then(Value::as_str)
        .ok_or(PlainTextError::MissingAatWorkId)?
        .to_owned();

    Ok(PlainTextDocument {
        text_id,
        source_format: SourceFormat::AatVisibleText,
        text: visible_text_projection(aat),
    })
}

pub fn visible_text_projection(aat: &Value) -> String {
    let mut out = String::new();
    if let Some(blocks) = aat.get("blocks").and_then(Value::as_array) {
        for block in blocks {
            collect_block(block, &mut out);
        }
    }
    canonicalize_line_endings(out)
}

fn collect_block(node: &Value, out: &mut String) {
    if let Some(content) = node.get("content").and_then(Value::as_array) {
        for inline in content {
            collect_inline(inline, out);
        }
    }
    if let Some(children) = node.get("children").and_then(Value::as_array) {
        for child in children {
            collect_block(child, out);
        }
    }
}

fn collect_inline(node: &Value, out: &mut String) {
    match node.get("kind").and_then(Value::as_str).unwrap_or("") {
        "text" => push_string_field(node, "value", out),
        "ruby" => push_string_field(node, "base", out),
        "gaiji" => {
            if let Some(resolved) = node.get("resolved").and_then(Value::as_str) {
                out.push_str(resolved);
            } else {
                push_string_field(node, "description", out);
            }
        }
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
                        collect_inline(inline, out);
                    }
                }
            }
        }
        "figure" => {
            if let Some(caption) = node.get("caption").and_then(Value::as_array) {
                for inline in caption {
                    collect_inline(inline, out);
                }
            }
        }
        "style" if node.get("style_type").and_then(Value::as_str) == Some("notes") => {}
        _ => {
            if let Some(content) = node.get("content").and_then(Value::as_array) {
                for inline in content {
                    collect_inline(inline, out);
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

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    #[test]
    fn builds_document_from_work_id_and_visible_text() {
        let aat = json!({
            "work_id": "w1",
            "blocks": [{
                "kind": "paragraph",
                "content": [{"kind": "text", "value": "本文"}]
            }]
        });

        let doc = from_aat_value(&aat).unwrap();
        assert_eq!(doc.text_id, "w1");
        assert_eq!(doc.source_format, SourceFormat::AatVisibleText);
        assert_eq!(doc.text, "本文");
    }

    #[test]
    fn projects_nested_visible_text() {
        let aat = json!({
            "work_id": "w2",
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "A"},
                    {"kind": "ruby", "base": "B", "reading": "ビー"},
                    {"kind": "gaiji", "description": "desc", "resolved": "C"},
                    {"kind": "gaiji", "description": "empty", "resolved": ""},
                    {"kind": "gaiji", "description": "D"},
                    {"kind": "accent", "code": "1-09-63", "name": "e acute", "resolved": "é"},
                    {"kind": "accent", "code": "missing", "name": "fallback", "resolved": ""},
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

        assert_eq!(visible_text_projection(&aat), "ABCDéfallbackEFGH");
    }

    #[test]
    fn projects_figure_caption_text() {
        let aat = json!({
            "work_id": "w-figure",
            "blocks": [{
                "kind": "paragraph",
                "content": [{
                    "kind": "figure",
                    "filename": "fig00001_01.png",
                    "alt": "猫の図",
                    "css_class": "",
                    "width": 321,
                    "height": 123,
                    "caption": [{"kind": "text", "value": "猫の図"}]
                }]
            }]
        });

        assert_eq!(visible_text_projection(&aat), "猫の図");
    }

    #[test]
    fn projection_excludes_aozora_input_notes() {
        let aat = json!({
            "work_id": "w-notes",
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "前"},
                    {
                        "kind": "style",
                        "style_type": "notes",
                        "content": [{"kind": "text", "value": "［＃改丁］"}]
                    },
                    {"kind": "text", "value": "後"}
                ]
            }]
        });

        assert_eq!(visible_text_projection(&aat), "前後");
    }

    #[test]
    fn projection_canonicalizes_line_endings() {
        let aat = json!({
            "work_id": "w-line-endings",
            "blocks": [{
                "kind": "paragraph",
                "content": [{"kind": "text", "value": "A\r\nB\rC\nD"}]
            }]
        });

        assert_eq!(visible_text_projection(&aat), "A\nB\nC\nD");
    }

    #[test]
    fn empty_resolved_gaiji_is_empty_and_missing_resolved_falls_back() {
        let aat = json!({
            "work_id": "w",
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "gaiji", "description": "desc", "resolved": ""},
                    {"kind": "gaiji", "description": "fallback"}
                ]
            }]
        });

        assert_eq!(visible_text_projection(&aat), "fallback");
    }

    #[test]
    fn missing_work_id_is_error() {
        let aat = json!({"blocks": []});
        assert_eq!(from_aat_value(&aat), Err(PlainTextError::MissingAatWorkId));
    }
}
