#[cfg(test)]
use ab_source_syntax as source_syntax;
use anyhow::Result;
use aozora_core::{Node, RubyDirection};
use encoding_rs::SHIFT_JIS;
use serde_json::json;
use sha2::{Digest, Sha256};

pub const VERSION: &str = "aozora2-adapter 0.1.0 93420b53c7d52579a0ca3fde466cef8ce6d89879";

#[derive(Debug)]
pub struct DecodedSource {
    pub text: String,
    pub encoding: &'static str,
    pub source_hash: String,
}

pub fn decode_source_bytes(bytes: &[u8]) -> Result<DecodedSource> {
    let source_hash = format!("sha256:{}", hex_sha256(bytes));
    if bytes.starts_with(&[0xef, 0xbb, 0xbf]) {
        return Ok(DecodedSource {
            text: std::str::from_utf8(&bytes[3..])?.to_owned(),
            encoding: "utf-8-bom",
            source_hash,
        });
    }
    if let Ok(text) = std::str::from_utf8(bytes) {
        return Ok(DecodedSource {
            text: text.to_owned(),
            encoding: "utf-8",
            source_hash,
        });
    }
    let (cow, _, had_errors) = SHIFT_JIS.decode(bytes);
    Ok(DecodedSource {
        text: cow.into_owned(),
        encoding: if had_errors {
            "windows-31j-lossy"
        } else {
            "windows-31j"
        },
        source_hash,
    })
}

pub fn build_aat(decoded: &DecodedSource) -> serde_json::Value {
    let content = parse_inline_content(body_text(&decoded.text));
    json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": [
            {
                "kind": "paragraph",
                "content": content
            }
        ],
        "meta": {
            "adapter": "aozora2",
            "adapter_version": VERSION,
            "source_encoding": decoded.encoding,
            "source_hash": decoded.source_hash,
            "parse_complete": true,
            "warnings": []
        }
    })
}

pub fn aat_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let aat = build_aat(&decoded);
    let mut out = Vec::new();
    serde_json::to_writer(&mut out, &aat)?;
    out.push(b'\n');
    Ok(out)
}

pub fn body_text(text: &str) -> &str {
    let mut separator_count = 0;
    let mut body_start = 0;
    let mut offset = 0;
    for line in text.split_inclusive('\n') {
        if line
            .trim_end_matches(['\r', '\n'])
            .chars()
            .all(|ch| ch == '-')
            && line.trim_end_matches(['\r', '\n']).chars().count() >= 20
        {
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

fn parse_inline_content(text: &str) -> Vec<serde_json::Value> {
    let tokens = aozora_core::tokenize(text);
    let nodes = aozora_core::parse(&tokens);
    aozora_nodes_to_aat_content(&nodes)
}

fn aozora_nodes_to_aat_content(nodes: &[Node]) -> Vec<serde_json::Value> {
    let mut content = Vec::new();
    for node in nodes {
        append_aozora_node(&mut content, node);
    }
    content
}

fn append_aozora_node(content: &mut Vec<serde_json::Value>, node: &Node) {
    match node {
        Node::Text(text) => push_text_node(content, text),
        Node::Gaiji {
            description,
            unicode,
            jis_code,
        } => content.push(gaiji_json(
            description,
            unicode.as_deref(),
            jis_code.as_deref(),
        )),
        Node::Ruby {
            children,
            ruby,
            direction,
        } => content.push(json!({
            "kind": "ruby",
            "base": aozora_nodes_visible_text(children),
            "reading": aozora_nodes_visible_text(ruby),
            "direction": ruby_direction_name(*direction),
            "base_content": aozora_nodes_to_aat_content(children),
            "reading_content": aozora_nodes_to_aat_content(ruby)
        })),
        Node::Style {
            children,
            style_type,
            ..
        } => content.push(json!({
            "kind": "style",
            "style_type": format!("{style_type:?}"),
            "content": aozora_nodes_to_aat_content(children)
        })),
        Node::Tcy { children }
        | Node::Keigakomi { children }
        | Node::Yokogumi { children }
        | Node::Caption { children }
        | Node::FontSize { children, .. }
        | Node::Midashi { children, .. } => {
            for child in children {
                append_aozora_node(content, child);
            }
        }
        Node::Warigaki { upper, lower } => content.push(json!({
            "kind": "warigaki",
            "upper": aozora_nodes_to_aat_content(upper),
            "lower": aozora_nodes_to_aat_content(lower)
        })),
        Node::Accent { name, unicode, .. } => {
            push_text_node(content, unicode.as_deref().unwrap_or(name));
        }
        Node::Img {
            filename,
            alt,
            css_class,
            width,
            height,
        } => content.push(json!({
            "kind": "image",
            "filename": filename,
            "alt": alt,
            "css_class": css_class,
            "width": width,
            "height": height
        })),
        Node::DakutenKatakana { .. }
        | Node::Kaeriten(_)
        | Node::Okurigana(_)
        | Node::UnresolvedReference { .. } => {
            let visible = node.to_text();
            if !visible.is_empty() {
                push_text_node(content, &visible);
            }
        }
        Node::AnnotationEnd {
            prefix,
            content: annotation,
            suffix,
        } => {
            push_text_node(content, prefix);
            for child in annotation {
                append_aozora_node(content, child);
            }
            push_text_node(content, suffix);
        }
        Node::BlockStart { .. } | Node::BlockEnd { .. } | Node::Note(_) => {}
    }
}

fn push_text_node(content: &mut Vec<serde_json::Value>, text: &str) {
    if text.is_empty() {
        return;
    }
    if let Some(last) = content.last_mut()
        && let Some(object) = last.as_object_mut()
        && object.get("kind").and_then(serde_json::Value::as_str) == Some("text")
        && let Some(serde_json::Value::String(value)) = object.get_mut("value")
    {
        value.push_str(text);
        return;
    }
    content.push(json!({
        "kind": "text",
        "value": text
    }));
}

fn gaiji_json(
    description: &str,
    resolved: Option<&str>,
    jis_code: Option<&str>,
) -> serde_json::Value {
    json!({
        "kind": "gaiji",
        "description": description,
        "resolved": resolved.unwrap_or(""),
        "jis_code": jis_code,
        "unresolved_reason": if resolved.is_some() { None::<&str> } else { Some("unresolved") }
    })
}

fn aozora_nodes_visible_text(nodes: &[Node]) -> String {
    nodes.iter().map(Node::to_text).collect()
}

fn ruby_direction_name(direction: RubyDirection) -> &'static str {
    match direction {
        RubyDirection::Right => "right",
        RubyDirection::Left => "left",
    }
}

#[cfg(test)]
fn source_visible_text(text: &str) -> String {
    source_syntax::comparison_lossy_body(text).into_owned()
}

fn hex_sha256(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("{:x}", hasher.finalize())
}

pub fn html_escape(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
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
    fn parse_inline_content_emits_nested_gaiji_inside_ruby_reading() {
        let content = parse_inline_content("淡絹《※［＃濁点付き片仮名ヱ、1-7-84］エル》");
        let ruby = content
            .iter()
            .find(|node| node["kind"] == "ruby")
            .expect("ruby node");
        let reading_content = ruby["reading_content"].as_array().expect("reading content");

        assert_eq!(ruby["base"], "淡絹");
        assert_eq!(ruby["reading"], "ヹエル");
        assert_eq!(
            reading_content
                .iter()
                .filter(|node| node["kind"] == "gaiji")
                .count(),
            1
        );
        assert_eq!(reading_content[0]["resolved"], "ヹ");
    }

    #[test]
    fn parse_inline_content_preserves_aozora2_resolved_jis_gaiji() {
        let content = parse_inline_content("耳朶を※［＃「てへん＋掌」、第4水準2-13-47］えて");

        assert_eq!(content[0]["kind"], "text");
        assert_eq!(content[0]["value"], "耳朶を");
        assert_eq!(content[1]["kind"], "gaiji");
        assert_eq!(content[1]["description"], "「てへん＋掌」、第4水準2-13-47");
        assert_eq!(content[1]["resolved"], "撑");
        assert_eq!(content[1]["jis_code"], "2-13-47");
        assert_eq!(content[2]["kind"], "text");
        assert_eq!(content[2]["value"], "えて");
    }
}
