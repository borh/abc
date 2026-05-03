#[cfg(test)]
use ab_source_syntax as source_syntax;
use anyhow::Result;
use aozora_core::node::FontSizeType;
use aozora_core::{MidashiLevel, MidashiStyle, Node, RubyDirection};
use encoding_rs::SHIFT_JIS;
use serde_json::json;
use sha2::{Digest, Sha256};

pub const VERSION: &str = "aozora2-adapter 0.1.0 aozora-core-0.7.1";

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
    let blocks = parse_blocks(body_text(&decoded.text));
    json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": blocks,
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

pub fn html_from_bytes(_bytes: &[u8]) -> Result<Vec<u8>> {
    anyhow::bail!(
        "aozora2-adapter --mode html is unsupported because aozora-core does not expose an upstream HTML renderer"
    );
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

#[cfg(test)]
fn parse_inline_content(text: &str) -> Vec<serde_json::Value> {
    let tokens = aozora_core::tokenize(text);
    let nodes = aozora_core::parse(&tokens);
    aozora_nodes_to_aat_content(&nodes)
}

fn parse_blocks(text: &str) -> Vec<serde_json::Value> {
    let tokens = aozora_core::tokenize(text);
    let nodes = aozora_core::parse(&tokens);
    aozora_nodes_to_aat_blocks(&nodes)
}

#[derive(Debug)]
struct BlockFrame {
    block_type: Option<aozora_core::BlockType>,
    children: Vec<serde_json::Value>,
    content: Vec<serde_json::Value>,
}

impl BlockFrame {
    fn root() -> Self {
        Self {
            block_type: None,
            children: Vec::new(),
            content: Vec::new(),
        }
    }

    fn block(block_type: aozora_core::BlockType) -> Self {
        Self {
            block_type: Some(block_type),
            children: Vec::new(),
            content: Vec::new(),
        }
    }

    fn flush_paragraph(&mut self) {
        if self.content.is_empty() {
            return;
        }
        self.children.push(json!({
            "kind": "paragraph",
            "content": std::mem::take(&mut self.content)
        }));
    }

    fn into_block(mut self) -> serde_json::Value {
        self.flush_paragraph();
        let block_type = self.block_type.expect("non-root block frame");
        if let Some(kind) = block_kind(block_type) {
            json!({
                "kind": kind,
                "children": self.children
            })
        } else {
            let mut content = vec![raw_json(format!("BlockStart({block_type:?})"))];
            for child in self.children {
                content.push(child);
            }
            content.push(raw_json(format!("BlockEnd({block_type:?})")));
            json!({
                "kind": "paragraph",
                "content": content
            })
        }
    }
}

fn aozora_nodes_to_aat_blocks(nodes: &[Node]) -> Vec<serde_json::Value> {
    let mut stack = vec![BlockFrame::root()];
    for node in nodes {
        match node {
            Node::BlockStart { block_type, .. } if block_kind(*block_type).is_some() => {
                stack.last_mut().expect("root frame").flush_paragraph();
                stack.push(BlockFrame::block(*block_type));
            }
            Node::BlockEnd { block_type, .. } => {
                if stack.len() > 1
                    && stack.last().and_then(|frame| frame.block_type) == Some(*block_type)
                {
                    let block = stack.pop().expect("block frame").into_block();
                    stack.last_mut().expect("parent frame").children.push(block);
                } else {
                    append_raw_to_current_frame(&mut stack, format!("BlockEnd({block_type:?})"));
                }
            }
            Node::Text(text) => append_text_to_current_frame(&mut stack, text),
            _ => append_node_to_current_frame(&mut stack, node),
        }
    }

    while stack.len() > 1 {
        let mut frame = stack.pop().expect("block frame");
        let block_type = frame.block_type.expect("block type");
        stack
            .last_mut()
            .expect("parent frame")
            .content
            .push(raw_json(format!("BlockStart({block_type:?})")));
        frame.flush_paragraph();
        for child in frame.children {
            stack.last_mut().expect("parent frame").children.push(child);
        }
    }

    let mut root = stack.pop().expect("root frame");
    root.flush_paragraph();
    root.children
}

fn append_node_to_current_frame(stack: &mut [BlockFrame], node: &Node) {
    append_aozora_node(&mut stack.last_mut().expect("current frame").content, node);
}

fn append_raw_to_current_frame(stack: &mut [BlockFrame], source: impl Into<String>) {
    stack
        .last_mut()
        .expect("current frame")
        .content
        .push(raw_json(source));
}

fn append_text_to_current_frame(stack: &mut [BlockFrame], text: &str) {
    let parts = text.split("\n\n").collect::<Vec<_>>();
    let split_count = parts.len();
    for (idx, part) in parts.into_iter().enumerate() {
        if !part.trim().is_empty() {
            push_text_node(&mut stack.last_mut().expect("current frame").content, part);
        }
        if idx + 1 < split_count {
            stack.last_mut().expect("current frame").flush_paragraph();
        }
    }
}

fn block_kind(block_type: aozora_core::BlockType) -> Option<&'static str> {
    match block_type {
        aozora_core::BlockType::Jisage => Some("jisage_block"),
        aozora_core::BlockType::Keigakomi => Some("keigakomi_block"),
        aozora_core::BlockType::Yokogumi => Some("yokogumi_block"),
        aozora_core::BlockType::Caption => Some("caption_block"),
        _ => None,
    }
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
        Node::Tcy { children } => content.push(inline_container_json("tcy", children)),
        Node::Keigakomi { children } => content.push(inline_container_json("keigakomi", children)),
        Node::Yokogumi { children } => content.push(inline_container_json("yokogumi", children)),
        Node::Caption { children } => content.push(inline_container_json("caption", children)),
        Node::FontSize {
            children,
            size_type,
            level,
        } => content.push(json!({
            "kind": "font_size",
            "size_type": font_size_type_name(*size_type),
            "level": level,
            "content": aozora_nodes_to_aat_content(children)
        })),
        Node::Midashi {
            children,
            level,
            style,
        } => content.push(json!({
            "kind": "style",
            "style_type": "midashi",
            "level": midashi_level_number(*level),
            "class_name": midashi_style_name(*style),
            "content": aozora_nodes_to_aat_content(children)
        })),
        Node::Warigaki { upper, lower } => content.push(json!({
            "kind": "warigaki",
            "upper": aozora_nodes_to_aat_content(upper),
            "lower": aozora_nodes_to_aat_content(lower)
        })),
        Node::Accent {
            code,
            name,
            unicode,
        } => content.push(json!({
            "kind": "accent",
            "code": code,
            "name": name,
            "resolved": unicode
        })),
        Node::Img {
            filename,
            alt,
            css_class,
            width,
            height,
        } => content.push(json!({
            "kind": "figure",
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
        Node::BlockStart { block_type, .. } => {
            content.push(raw_json(format!("BlockStart({block_type:?})")))
        }
        Node::BlockEnd { block_type, .. } => {
            content.push(raw_json(format!("BlockEnd({block_type:?})")))
        }
        Node::Note(note) => content.push(raw_json(note)),
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

fn inline_container_json(kind: &str, children: &[Node]) -> serde_json::Value {
    json!({
        "kind": kind,
        "content": aozora_nodes_to_aat_content(children)
    })
}

fn raw_json(source: impl Into<String>) -> serde_json::Value {
    json!({
        "kind": "raw",
        "source": source.into()
    })
}

fn aozora_nodes_visible_text(nodes: &[Node]) -> String {
    nodes.iter().map(Node::to_text).collect()
}

fn font_size_type_name(size_type: FontSizeType) -> &'static str {
    match size_type {
        FontSizeType::Dai => "dai",
        FontSizeType::Sho => "sho",
    }
}

fn midashi_level_number(level: MidashiLevel) -> u8 {
    match level {
        MidashiLevel::O => 1,
        MidashiLevel::Naka => 2,
        MidashiLevel::Ko => 3,
    }
}

fn midashi_style_name(style: MidashiStyle) -> &'static str {
    match style {
        MidashiStyle::Normal => "normal",
        MidashiStyle::Dogyo => "dogyo",
        MidashiStyle::Mado => "mado",
    }
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
    use aozora_core::node::FontSizeType;
    use aozora_core::{BlockParams, BlockType, MidashiLevel, MidashiStyle};

    fn project_node(node: Node) -> Vec<serde_json::Value> {
        let mut content = Vec::new();
        append_aozora_node(&mut content, &node);
        content
    }

    #[test]
    fn version_names_crates_io_aozora_core_version() {
        assert_eq!(VERSION, "aozora2-adapter 0.1.0 aozora-core-0.7.1");
    }

    #[test]
    fn html_mode_is_not_reported_as_upstream_renderer() {
        assert!(html_from_bytes("本文".as_bytes()).is_err());
    }

    #[test]
    fn projection_preserves_accent_node() {
        let content = project_node(Node::Accent {
            code: "E9".to_owned(),
            name: "e acute".to_owned(),
            unicode: Some("é".to_owned()),
        });

        assert_eq!(content[0]["kind"], "accent");
        assert_eq!(content[0]["code"], "E9");
        assert_eq!(content[0]["name"], "e acute");
        assert_eq!(content[0]["resolved"], "é");
    }

    #[test]
    fn projection_preserves_image_as_figure_node() {
        let content = project_node(Node::Img {
            filename: "fig01.png".to_owned(),
            alt: "挿絵".to_owned(),
            css_class: "width-400 height-300".to_owned(),
            width: Some(400),
            height: Some(300),
        });

        assert_eq!(content[0]["kind"], "figure");
        assert_eq!(content[0]["filename"], "fig01.png");
        assert_eq!(content[0]["alt"], "挿絵");
        assert_eq!(content[0]["css_class"], "width-400 height-300");
        assert_eq!(content[0]["width"], 400);
        assert_eq!(content[0]["height"], 300);
    }

    #[test]
    fn projection_preserves_inline_wrapper_nodes() {
        let cases = [
            (
                Node::Tcy {
                    children: vec![Node::Text("12".to_owned())],
                },
                "tcy",
            ),
            (
                Node::Yokogumi {
                    children: vec![Node::Text("abc".to_owned())],
                },
                "yokogumi",
            ),
            (
                Node::Caption {
                    children: vec![Node::Text("説明".to_owned())],
                },
                "caption",
            ),
            (
                Node::Keigakomi {
                    children: vec![Node::Text("囲み".to_owned())],
                },
                "keigakomi",
            ),
        ];

        for (node, kind) in cases {
            let content = project_node(node);
            assert_eq!(content[0]["kind"], kind);
            assert_eq!(content[0]["content"][0]["kind"], "text");
        }
    }

    #[test]
    fn projection_preserves_font_size_and_midashi_semantics() {
        let font_size = project_node(Node::FontSize {
            children: vec![Node::Text("大".to_owned())],
            size_type: FontSizeType::Dai,
            level: 2,
        });
        assert_eq!(font_size[0]["kind"], "font_size");
        assert_eq!(font_size[0]["size_type"], "dai");
        assert_eq!(font_size[0]["level"], 2);

        let midashi = project_node(Node::Midashi {
            children: vec![Node::Text("章".to_owned())],
            level: MidashiLevel::O,
            style: MidashiStyle::Dogyo,
        });
        assert_eq!(midashi[0]["kind"], "style");
        assert_eq!(midashi[0]["style_type"], "midashi");
        assert_eq!(midashi[0]["level"], 1);
        assert_eq!(midashi[0]["class_name"], "dogyo");
    }

    #[test]
    fn projection_preserves_notes_and_block_boundaries_as_raw_nodes() {
        let note = project_node(Node::Note("注記".to_owned()));
        assert_eq!(note[0]["kind"], "raw");
        assert_eq!(note[0]["source"], "注記");

        let block = project_node(Node::BlockStart {
            block_type: BlockType::Jisage,
            params: BlockParams::default(),
        });
        assert_eq!(block[0]["kind"], "raw");
        assert_eq!(block[0]["source"], "BlockStart(Jisage)");
    }

    #[test]
    fn aozora_core_emits_jisage_block_markers() {
        // Verified against aozora-core 0.7.1: tokenize(&str) and parse(&[Token])
        // return public Node::BlockStart/BlockEnd variants.
        let tokens =
            aozora_core::tokenize("［＃ここから2字下げ］\n字下げ\n［＃ここで字下げ終わり］");
        let nodes = aozora_core::parse(&tokens);

        assert!(nodes.iter().any(|node| {
            matches!(
                node,
                Node::BlockStart {
                    block_type: BlockType::Jisage,
                    ..
                }
            )
        }));
        assert!(nodes.iter().any(|node| {
            matches!(
                node,
                Node::BlockEnd {
                    block_type: BlockType::Jisage,
                    ..
                }
            )
        }));
    }

    #[test]
    fn build_aat_reconstructs_jisage_block_container() {
        let decoded = DecodedSource {
            text: "前\n［＃ここから2字下げ］\n字下げ\n［＃ここで字下げ終わり］\n後".to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                .to_owned(),
        };

        let aat = build_aat(&decoded);
        let blocks = aat["blocks"].as_array().expect("blocks");

        assert_eq!(blocks[0]["kind"], "paragraph");
        assert_eq!(blocks[1]["kind"], "jisage_block");
        assert_eq!(blocks[1]["children"][0]["kind"], "paragraph");
        assert_eq!(blocks[2]["kind"], "paragraph");
    }

    #[test]
    fn build_aat_reconstructs_nested_block_containers() {
        let decoded = DecodedSource {
            text: "［＃ここから2字下げ］\n［＃ここから罫囲み］\n囲み\n［＃ここで罫囲み終わり］\n［＃ここで字下げ終わり］"
                .to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                .to_owned(),
        };

        let aat = build_aat(&decoded);
        let blocks = aat["blocks"].as_array().expect("blocks");

        assert_eq!(blocks[0]["kind"], "jisage_block");
        assert_eq!(blocks[0]["children"][0]["kind"], "keigakomi_block");
        assert_eq!(blocks[0]["children"][0]["children"][0]["kind"], "paragraph");
    }

    #[test]
    fn unmatched_block_end_remains_raw_inline() {
        let decoded = DecodedSource {
            text: "本文［＃ここで字下げ終わり］".to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                .to_owned(),
        };

        let aat = build_aat(&decoded);
        let content = aat["blocks"][0]["content"].as_array().expect("content");

        assert!(
            content
                .iter()
                .any(|node| { node["kind"] == "raw" && node["source"] == "BlockEnd(Jisage)" })
        );
    }

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
