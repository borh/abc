#[cfg(test)]
use ab_source_syntax as source_syntax;
use anyhow::Result;
use aozora_core::node::{FontSizeType, StyleType};
use aozora_core::{MidashiLevel, MidashiStyle, Node, RubyDirection};
use encoding_rs::SHIFT_JIS;
use serde_json::Value;
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
    params: aozora_core::BlockParams,
    children: Vec<serde_json::Value>,
    content: Vec<serde_json::Value>,
}

impl BlockFrame {
    fn root() -> Self {
        Self {
            block_type: None,
            params: aozora_core::BlockParams::default(),
            children: Vec::new(),
            content: Vec::new(),
        }
    }

    fn block(block_type: aozora_core::BlockType, params: aozora_core::BlockParams) -> Self {
        Self {
            block_type: Some(block_type),
            params,
            children: Vec::new(),
            content: Vec::new(),
        }
    }

    fn flush_paragraph(&mut self) {
        normalize_inline_content(&mut self.content);
        trim_content_boundary_line_breaks(&mut self.content);
        if self.content.is_empty() {
            return;
        }
        push_paragraphs_with_page_breaks(&mut self.children, std::mem::take(&mut self.content));
    }

    fn flush_heading(&mut self, level: MidashiLevel, style: MidashiStyle) {
        normalize_inline_content(&mut self.content);
        trim_content_boundary_line_breaks(&mut self.content);
        if self.content.is_empty() {
            return;
        }
        push_headings_with_page_breaks(
            &mut self.children,
            level,
            style,
            std::mem::take(&mut self.content),
        );
    }

    fn wrap_content_in_line_style(
        &mut self,
        block_type: aozora_core::BlockType,
        params: &aozora_core::BlockParams,
    ) {
        normalize_inline_content(&mut self.content);
        trim_content_boundary_line_breaks(&mut self.content);
        if self.content.is_empty() {
            return;
        }
        let Some(style_type) = line_style_type(block_type) else {
            return;
        };
        let mut style = json!({
            "kind": "style",
            "style_type": style_type,
            "content": std::mem::take(&mut self.content)
        });
        apply_style_params(&mut style, style_type, params);
        self.content.push(style);
    }

    fn into_blocks(mut self) -> Vec<serde_json::Value> {
        self.flush_paragraph();
        let block_type = self.block_type.expect("non-root block frame");
        if let Some(kind) = block_kind(block_type) {
            let mut block = json!({
                "kind": kind,
                "children": self.children
            });
            apply_block_params(&mut block, block_type, &self.params);
            vec![block]
        } else if let Some(style_type) = style_block_type(block_type) {
            let mut style = json!({
                "kind": "style",
                "style_type": style_type,
                "content": inline_content_from_blocks(self.children)
            });
            apply_style_params(&mut style, style_type, &self.params);
            paragraph_blocks_from_inline_nodes(vec![style])
        } else if inline_scope_block_type(block_type) {
            let inline = inline_scope_block(block_type, &self.params, self.children)
                .expect("inline scope block");
            paragraph_blocks_from_inline_nodes(vec![inline])
        } else {
            let mut content = vec![raw_json(format!("BlockStart({block_type:?})"))];
            for child in self.children {
                content.push(child);
            }
            content.push(raw_json(format!("BlockEnd({block_type:?})")));
            vec![json!({
                "kind": "paragraph",
                "content": content
            })]
        }
    }
}

fn aozora_nodes_to_aat_blocks(nodes: &[Node]) -> Vec<serde_json::Value> {
    let mut stack = vec![BlockFrame::root()];
    for node in nodes {
        match node {
            Node::BlockStart { block_type, params }
                if *block_type == aozora_core::BlockType::Midashi =>
            {
                stack.last_mut().expect("root frame").flush_heading(
                    params.level.unwrap_or(MidashiLevel::O),
                    params.midashi_style.unwrap_or_default(),
                );
            }
            Node::BlockStart { block_type, params }
                if !params.is_block && line_style_type(*block_type).is_some() =>
            {
                stack
                    .last_mut()
                    .expect("current frame")
                    .wrap_content_in_line_style(*block_type, params);
            }
            Node::BlockStart { block_type, params }
                if params.is_block && block_kind(*block_type).is_some() =>
            {
                stack.last_mut().expect("root frame").flush_paragraph();
                stack.push(BlockFrame::block(*block_type, params.clone()));
            }
            Node::BlockStart { block_type, params } if style_block_type(*block_type).is_some() => {
                stack.last_mut().expect("root frame").flush_paragraph();
                stack.push(BlockFrame::block(*block_type, params.clone()));
            }
            Node::BlockStart { block_type, params } if inline_scope_block_type(*block_type) => {
                stack.last_mut().expect("root frame").flush_paragraph();
                stack.push(BlockFrame::block(*block_type, params.clone()));
            }
            Node::BlockEnd { block_type, .. } => {
                if stack.len() > 1
                    && stack
                        .last()
                        .and_then(|frame| frame.block_type)
                        .is_some_and(|start| block_end_matches(start, *block_type))
                {
                    let blocks = stack.pop().expect("block frame").into_blocks();
                    stack
                        .last_mut()
                        .expect("parent frame")
                        .children
                        .extend(blocks);
                } else {
                    append_raw_to_current_frame(&mut stack, format!("BlockEnd({block_type:?})"));
                }
            }
            Node::Text(text) => append_text_to_current_frame(&mut stack, text),
            Node::Midashi {
                children,
                level,
                style,
            } => {
                let current = stack.last_mut().expect("current frame");
                current.flush_paragraph();
                current.content = aozora_nodes_to_aat_content(children);
                current.flush_heading(*level, *style);
            }
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

fn style_block_type(block_type: aozora_core::BlockType) -> Option<&'static str> {
    match block_type {
        aozora_core::BlockType::Jizume => Some("jizume"),
        aozora_core::BlockType::Burasage => Some("burasage"),
        _ => None,
    }
}

fn inline_scope_block_type(block_type: aozora_core::BlockType) -> bool {
    matches!(
        block_type,
        aozora_core::BlockType::Tcy
            | aozora_core::BlockType::Futoji
            | aozora_core::BlockType::Shatai
            | aozora_core::BlockType::FontDai
            | aozora_core::BlockType::FontSho
    )
}

fn inline_scope_block(
    block_type: aozora_core::BlockType,
    params: &aozora_core::BlockParams,
    children: Vec<serde_json::Value>,
) -> Option<serde_json::Value> {
    let content = inline_content_from_blocks(children);
    match block_type {
        aozora_core::BlockType::Tcy => Some(json!({
            "kind": "tcy",
            "content": content
        })),
        aozora_core::BlockType::Futoji => Some(json!({
            "kind": "style",
            "style_type": "bold",
            "content": content
        })),
        aozora_core::BlockType::Shatai => Some(json!({
            "kind": "style",
            "style_type": "italic",
            "content": content
        })),
        aozora_core::BlockType::FontDai => Some(json!({
            "kind": "font_size",
            "size_type": "larger",
            "level": params.font_size.unwrap_or(1),
            "content": content
        })),
        aozora_core::BlockType::FontSho => Some(json!({
            "kind": "font_size",
            "size_type": "smaller",
            "level": params.font_size.unwrap_or(1),
            "content": content
        })),
        _ => None,
    }
}

fn line_style_type(block_type: aozora_core::BlockType) -> Option<&'static str> {
    match block_type {
        aozora_core::BlockType::Jisage => Some("jisage_line"),
        aozora_core::BlockType::Chitsuki => Some("chitsuki"),
        _ => None,
    }
}

fn block_end_matches(start: aozora_core::BlockType, end: aozora_core::BlockType) -> bool {
    start == end
        || (start == aozora_core::BlockType::Burasage && end == aozora_core::BlockType::Jisage)
}

fn apply_block_params(
    block: &mut serde_json::Value,
    block_type: aozora_core::BlockType,
    params: &aozora_core::BlockParams,
) {
    let Some(object) = block.as_object_mut() else {
        return;
    };
    if block_type == aozora_core::BlockType::Jisage
        && let Some(width) = params.width
    {
        object.insert("x-indent".to_owned(), json!(width));
    }
}

fn apply_style_params(
    style: &mut serde_json::Value,
    style_type: &str,
    params: &aozora_core::BlockParams,
) {
    let Some(object) = style.as_object_mut() else {
        return;
    };
    match style_type {
        "jisage_line" => {
            if let Some(width) = params.width {
                object.insert("x-indent".to_owned(), json!(width));
            }
        }
        "chitsuki" => {
            object.insert("x-align".to_owned(), json!("right"));
            if let Some(width) = params.width {
                object.insert("x-width".to_owned(), json!(width));
            }
        }
        "jizume" => {
            if let Some(width) = params.width {
                object.insert("x-width".to_owned(), json!(width));
            }
        }
        "burasage" => {
            if let Some(width) = params.width {
                object.insert("x-indent-first".to_owned(), json!(width));
            }
            if let Some(wrap_width) = params.wrap_width {
                object.insert("x-indent-rest".to_owned(), json!(wrap_width));
            }
        }
        _ => {}
    }
}

fn inline_content_from_blocks(blocks: Vec<serde_json::Value>) -> Vec<serde_json::Value> {
    let mut content = Vec::new();
    for block in blocks {
        if block.get("kind").and_then(serde_json::Value::as_str) == Some("paragraph")
            && block.get("x-break-kind").and_then(Value::as_str) == Some("page")
        {
            content.push(json!({"kind": "_page_break"}));
        } else if block.get("kind").and_then(serde_json::Value::as_str) == Some("paragraph")
            && let Some(items) = block.get("content").and_then(serde_json::Value::as_array)
        {
            content.extend(items.iter().cloned());
        }
    }
    trim_content_boundary_line_breaks(&mut content);
    content
}

fn paragraph_blocks_from_inline_nodes(content: Vec<Value>) -> Vec<Value> {
    let mut blocks = Vec::new();
    push_paragraphs_with_page_breaks(&mut blocks, content);
    blocks
}

fn push_headings_with_page_breaks(
    children: &mut Vec<Value>,
    level: MidashiLevel,
    style: MidashiStyle,
    content: Vec<Value>,
) {
    let mut heading_content = Vec::new();
    for node in content {
        for fragment in split_inline_node_on_page_breaks(node) {
            match fragment {
                InlineFragment::Node(node) => heading_content.push(node),
                InlineFragment::PageBreak => {
                    push_heading(children, level, style, &mut heading_content);
                    push_page_break_block(children);
                }
            }
        }
    }
    push_heading(children, level, style, &mut heading_content);
}

fn push_heading(
    children: &mut Vec<Value>,
    level: MidashiLevel,
    style: MidashiStyle,
    content: &mut Vec<Value>,
) {
    trim_content_boundary_line_breaks(content);
    if content.is_empty() {
        return;
    }
    children.push(json!({
        "kind": "heading",
        "level": midashi_level_number(level),
        "style": midashi_style_name(style),
        "content": std::mem::take(content)
    }));
}

fn push_paragraphs_with_page_breaks(children: &mut Vec<Value>, content: Vec<Value>) {
    let mut paragraph = Vec::new();
    for node in content {
        push_inline_with_page_breaks(children, &mut paragraph, node);
    }
    trim_content_boundary_line_breaks(&mut paragraph);
    if !paragraph.is_empty() {
        children.push(json!({
            "kind": "paragraph",
            "content": paragraph
        }));
    }
}

fn push_inline_with_page_breaks(
    children: &mut Vec<Value>,
    paragraph: &mut Vec<Value>,
    node: Value,
) {
    for fragment in split_inline_node_on_page_breaks(node) {
        match fragment {
            InlineFragment::Node(node) => paragraph.push(node),
            InlineFragment::PageBreak => push_page_break_paragraph(children, paragraph),
        }
    }
}

enum InlineFragment {
    Node(Value),
    PageBreak,
}

fn split_inline_node_on_page_breaks(node: Value) -> Vec<InlineFragment> {
    if is_page_break_marker(&node) {
        return vec![InlineFragment::PageBreak];
    }

    if node.get("kind").and_then(Value::as_str) == Some("ruby")
        && let Some(items) = node.get("base_content").and_then(Value::as_array).cloned()
    {
        let (saw_page_break, fragments) = split_inline_items_on_page_breaks(items);
        if saw_page_break {
            return fragments;
        }
    }

    let Some(items) = node.get("content").and_then(Value::as_array).cloned() else {
        return vec![InlineFragment::Node(node)];
    };

    let mut fragments = Vec::new();
    let mut segment = Vec::new();
    let mut saw_page_break = false;

    for child in items {
        for fragment in split_inline_node_on_page_breaks(child) {
            match fragment {
                InlineFragment::Node(node) => segment.push(node),
                InlineFragment::PageBreak => {
                    saw_page_break = true;
                    push_wrapped_content_fragment(&mut fragments, &node, &mut segment);
                    fragments.push(InlineFragment::PageBreak);
                }
            }
        }
    }

    if saw_page_break {
        push_wrapped_content_fragment(&mut fragments, &node, &mut segment);
        fragments
    } else {
        vec![InlineFragment::Node(node)]
    }
}

fn split_inline_items_on_page_breaks(items: Vec<Value>) -> (bool, Vec<InlineFragment>) {
    let mut fragments = Vec::new();
    let mut saw_page_break = false;
    for child in items {
        for fragment in split_inline_node_on_page_breaks(child) {
            if matches!(fragment, InlineFragment::PageBreak) {
                saw_page_break = true;
            }
            fragments.push(fragment);
        }
    }
    (saw_page_break, fragments)
}

fn push_wrapped_content_fragment(
    fragments: &mut Vec<InlineFragment>,
    wrapper: &Value,
    segment: &mut Vec<Value>,
) {
    trim_content_boundary_line_breaks(segment);
    if segment.is_empty() {
        return;
    }
    let mut wrapped = wrapper.clone();
    if let Some(object) = wrapped.as_object_mut() {
        object.insert("content".to_owned(), Value::Array(std::mem::take(segment)));
        fragments.push(InlineFragment::Node(wrapped));
    }
}

fn is_page_break_marker(node: &Value) -> bool {
    node.get("kind").and_then(Value::as_str) == Some("_page_break")
}

fn push_page_break_paragraph(children: &mut Vec<Value>, paragraph: &mut Vec<Value>) {
    trim_content_boundary_line_breaks(paragraph);
    if !paragraph.is_empty() {
        children.push(json!({
            "kind": "paragraph",
            "content": std::mem::take(paragraph)
        }));
    }
    push_page_break_block(children);
}

fn push_page_break_block(children: &mut Vec<Value>) {
    children.push(json!({
        "kind": "paragraph",
        "content": [],
        "x-break-kind": "page"
    }));
}

fn normalize_inline_content(content: &mut Vec<Value>) {
    let original = std::mem::take(content);
    let mut normalized = Vec::new();
    let mut idx = 0;
    while idx < original.len() {
        let mut node = original[idx].clone();
        normalize_node_children(&mut node);

        if let Some(value) = text_node_value(&node) {
            if value == "／＼" {
                normalized.push(gaiji_json("くの字点", Some("〳〵"), None));
                idx += 1;
                continue;
            }
            if let Some(figure) = parse_plain_figure_text(value) {
                normalized.push(figure);
                idx += 1;
                continue;
            }
        }

        if node.get("kind").and_then(Value::as_str) == Some("figure")
            && idx + 1 < original.len()
            && original[idx + 1].get("kind").and_then(Value::as_str) == Some("caption")
        {
            if let Some(caption) = original[idx + 1].get("content").cloned()
                && let Some(object) = node.as_object_mut()
            {
                if let Some(caption_text) = caption_visible_text(&caption) {
                    object.insert("alt".to_owned(), json!(caption_text));
                }
                object.insert("caption".to_owned(), caption);
                normalized.push(node);
                idx += 2;
                continue;
            }
        }

        if let Some(source) = raw_node_source(&node) {
            if is_warigaki_start(source) {
                let (warigaki, next_idx) = collect_warigaki(&original, idx + 1);
                normalized.push(warigaki);
                idx = next_idx;
                continue;
            }
            if let Some((target, reading)) = parse_left_ruby_note(source) {
                if target.contains('《') || target.contains('》') {
                    let mut raw = raw_json(source.to_owned());
                    raw.as_object_mut()
                        .expect("raw object")
                        .insert("x-error-kind".to_owned(), json!("nested_ruby_forbidden"));
                    normalized.push(raw);
                    idx += 1;
                    continue;
                }
                if apply_left_ruby(&mut normalized, target, reading) {
                    idx += 1;
                    continue;
                }
            }
            if let Some((target, frontref)) = parse_frontref_boten_note(source) {
                if apply_frontref_boten(&mut normalized, target, frontref) {
                    idx += 1;
                    continue;
                }
            }
            if source == "改行" {
                apply_line_break(&mut normalized, original.get(idx + 1));
                if original.get(idx + 1).and_then(text_node_value).is_some() {
                    idx += 2;
                } else {
                    idx += 1;
                }
                continue;
            }
            if source == "改ページ" {
                normalized.push(json!({"kind": "_page_break"}));
                idx += 1;
                continue;
            }
            if source == "左頁" {
                normalized.push(json!({
                    "kind": "text",
                    "value": "",
                    "x-editor-note": "左頁"
                }));
                idx += 1;
                continue;
            }
            if let Some(marker) = source.strip_prefix("返り点") {
                normalized.push(json!({
                    "kind": "style",
                    "style_type": "kaeriten",
                    "content": [],
                    "x-marker": marker
                }));
                idx += 1;
                continue;
            }
            if let Some(reading) = parse_quoted_arg(source, "訓点送り仮名") {
                normalized.push(json!({
                    "kind": "ruby",
                    "base": "",
                    "reading": reading,
                    "direction": "right",
                    "base_content": [],
                    "reading_content": [{"kind": "text", "value": reading}],
                    "x-annotation-type": "okurigana"
                }));
                idx += 1;
                continue;
            }
        }

        normalized.push(node);
        idx += 1;
    }
    *content = normalized;
}

fn normalize_node_children(node: &mut Value) {
    for key in [
        "content",
        "base_content",
        "reading_content",
        "upper",
        "lower",
        "caption",
    ] {
        if let Some(children) = node.get_mut(key).and_then(Value::as_array_mut) {
            normalize_inline_content(children);
        }
    }
}

fn trim_content_boundary_line_breaks(content: &mut Vec<serde_json::Value>) {
    if let Some(first) = content.first_mut() {
        trim_text_node(first, true);
    }
    while content
        .first()
        .is_some_and(|node| text_node_value(node).is_some_and(str::is_empty))
    {
        content.remove(0);
    }
    if let Some(last) = content.last_mut() {
        trim_text_node(last, false);
    }
    while content
        .last()
        .is_some_and(|node| text_node_value(node).is_some_and(str::is_empty))
    {
        content.pop();
    }
}

fn trim_text_node(node: &mut serde_json::Value, leading: bool) {
    let Some(object) = node.as_object_mut() else {
        return;
    };
    if object.get("kind").and_then(serde_json::Value::as_str) != Some("text") {
        return;
    }
    let Some(serde_json::Value::String(value)) = object.get_mut("value") else {
        return;
    };
    let trimmed = if leading {
        value.trim_start_matches(['\r', '\n'])
    } else {
        value.trim_end_matches(['\r', '\n'])
    };
    *value = trimmed.to_owned();
}

fn text_node_value(node: &serde_json::Value) -> Option<&str> {
    node.as_object()
        .filter(|object| object.get("kind").and_then(serde_json::Value::as_str) == Some("text"))
        .and_then(|object| object.get("value"))
        .and_then(serde_json::Value::as_str)
}

fn raw_node_source(node: &Value) -> Option<&str> {
    node.as_object()
        .filter(|object| object.get("kind").and_then(Value::as_str) == Some("raw"))
        .and_then(|object| object.get("source"))
        .and_then(Value::as_str)
}

fn aozora_nodes_to_aat_content(nodes: &[Node]) -> Vec<serde_json::Value> {
    let mut content = Vec::new();
    for node in nodes {
        append_aozora_node(&mut content, node);
    }
    normalize_inline_content(&mut content);
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
        } => content.push(ruby_json(
            aozora_nodes_visible_text(children),
            aozora_nodes_visible_text(ruby),
            *direction,
            aozora_nodes_to_aat_content(children),
            aozora_nodes_to_aat_content(ruby),
        )),
        Node::Style {
            children,
            style_type,
            ..
        } => content.push(style_json(
            *style_type,
            aozora_nodes_to_aat_content(children),
        )),
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
        "jis_code": jis_code.map(normalize_jis_code),
        "unresolved_reason": if resolved.is_some() { None::<&str> } else { Some("unresolved") }
    })
}

fn normalize_jis_code(jis_code: &str) -> String {
    jis_code
        .split('-')
        .map(|part| {
            let trimmed = part.trim_start_matches('0');
            if trimmed.is_empty() { "0" } else { trimmed }
        })
        .collect::<Vec<_>>()
        .join("-")
}

fn inline_container_json(kind: &str, children: &[Node]) -> serde_json::Value {
    json!({
        "kind": kind,
        "content": aozora_nodes_to_aat_content(children)
    })
}

fn ruby_json(
    base: String,
    reading: String,
    direction: RubyDirection,
    base_content: Vec<Value>,
    mut reading_content: Vec<Value>,
) -> Value {
    let mut reading = reading;
    let mut annotation_type = None;
    if reading.contains('\u{a0}') {
        reading = reading.replace('\u{a0}', "");
        reading_content = vec![json!({"kind": "text", "value": reading})];
        annotation_type = Some("bouki");
    } else if reading == "ママ" {
        annotation_type = Some("chuuki");
    }

    let mut ruby = json!({
        "kind": "ruby",
        "base": base,
        "reading": reading,
        "direction": ruby_direction_name(direction),
        "base_content": base_content,
        "reading_content": reading_content
    });
    if let Some(annotation_type) = annotation_type {
        ruby.as_object_mut()
            .expect("ruby object")
            .insert("x-annotation-type".to_owned(), json!(annotation_type));
    }
    ruby
}

fn style_json(style_type: StyleType, content: Vec<Value>) -> Value {
    let mut style = json!({
        "kind": "style",
        "style_type": style_type_name(style_type),
        "content": content
    });
    let object = style.as_object_mut().expect("style object");
    if let Some(kind) = boten_kind(style_type) {
        object.insert("x-boten-kind".to_owned(), json!(kind));
    }
    if let Some(kind) = line_kind(style_type) {
        object.insert("x-line-kind".to_owned(), json!(kind));
    }
    if style_is_left_placement(style_type) {
        object.insert("x-placement".to_owned(), json!("left"));
    }
    style
}

fn raw_json(source: impl Into<String>) -> serde_json::Value {
    json!({
        "kind": "raw",
        "source": source.into()
    })
}

fn is_warigaki_start(source: &str) -> bool {
    source == "割書" || source == "割り注" || source == "BlockStart(Warigaki)"
}

fn is_warigaki_end(source: &str) -> bool {
    source == "割書終わり" || source == "割り注終わり" || source == "BlockEnd(Warigaki)"
}

fn collect_warigaki(nodes: &[Value], start_idx: usize) -> (Value, usize) {
    let mut upper = Vec::new();
    let mut idx = start_idx;
    while idx < nodes.len() {
        if let Some(source) = raw_node_source(&nodes[idx])
            && is_warigaki_end(source)
        {
            normalize_inline_content(&mut upper);
            return (
                json!({
                    "kind": "warigaki",
                    "upper": upper,
                    "lower": []
                }),
                idx + 1,
            );
        }
        upper.push(nodes[idx].clone());
        idx += 1;
    }
    normalize_inline_content(&mut upper);
    (
        json!({
            "kind": "warigaki",
            "upper": upper,
            "lower": []
        }),
        idx,
    )
}

fn parse_left_ruby_note(source: &str) -> Option<(&str, &str)> {
    let body = source.strip_prefix('「')?;
    let (target, rest) = body.split_once("」の左に「")?;
    let reading = rest.strip_suffix("」のルビ")?;
    Some((target, reading))
}

fn apply_left_ruby(content: &mut Vec<Value>, target: &str, reading: &str) -> bool {
    if let Some(last) = content.last_mut()
        && last.get("kind").and_then(Value::as_str) == Some("ruby")
        && last.get("base").and_then(Value::as_str) == Some(target)
        && let Some(object) = last.as_object_mut()
    {
        object.insert("x-left-reading".to_owned(), json!(reading));
        return true;
    }

    let Some(last) = content.last_mut() else {
        return false;
    };
    let Some(text) = text_node_value(last) else {
        return false;
    };
    let Some(prefix) = text.strip_suffix(target) else {
        return false;
    };
    let prefix = prefix.to_owned();
    if let Some(object) = last.as_object_mut()
        && let Some(Value::String(value)) = object.get_mut("value")
    {
        *value = prefix;
    }
    if text_node_value(last).is_some_and(str::is_empty) {
        content.pop();
    }
    content.push(json!({
        "kind": "ruby",
        "base": target,
        "reading": reading,
        "direction": "left",
        "base_content": [{"kind": "text", "value": target}],
        "reading_content": [{"kind": "text", "value": reading}]
    }));
    true
}

fn parse_frontref_boten_note(source: &str) -> Option<(&str, &str)> {
    let body = source.strip_prefix('「')?;
    let (target, rest) = body.split_once("」に「")?;
    let frontref = rest.strip_suffix("」の傍点")?;
    Some((target, frontref))
}

fn apply_frontref_boten(content: &mut Vec<Value>, target: &str, frontref: &str) -> bool {
    let Some(last) = content.last_mut() else {
        return false;
    };
    let Some(text) = text_node_value(last) else {
        return false;
    };
    let Some(prefix) = text.strip_suffix(target) else {
        return false;
    };
    let prefix = prefix.to_owned();
    if let Some(object) = last.as_object_mut()
        && let Some(Value::String(value)) = object.get_mut("value")
    {
        *value = prefix;
    }
    if text_node_value(last).is_some_and(str::is_empty) {
        content.pop();
    }
    content.push(json!({
        "kind": "style",
        "style_type": "boten",
        "content": [{"kind": "text", "value": target}],
        "x-frontref": frontref
    }));
    true
}

fn apply_line_break(content: &mut Vec<Value>, next: Option<&Value>) {
    let Some(last) = content.last_mut() else {
        content.push(json!({
            "kind": "text",
            "value": "\n",
            "x-break-kind": "line"
        }));
        return;
    };
    if let Some(object) = last.as_object_mut()
        && object.get("kind").and_then(Value::as_str) == Some("text")
        && let Some(Value::String(value)) = object.get_mut("value")
    {
        value.push('\n');
        if let Some(next_text) = next.and_then(text_node_value) {
            value.push_str(next_text);
        }
        object.insert("x-break-kind".to_owned(), json!("line"));
        return;
    }
    content.push(json!({
        "kind": "text",
        "value": "\n",
        "x-break-kind": "line"
    }));
}

fn parse_quoted_arg<'a>(source: &'a str, prefix: &str) -> Option<&'a str> {
    let body = source.strip_prefix(prefix)?;
    body.strip_prefix('「')?.strip_suffix('」')
}

fn parse_plain_figure_text(text: &str) -> Option<Value> {
    let (alt, rest) = text.split_once('（')?;
    let args = rest.strip_suffix("）入る")?;
    figure_from_args(args, alt, None)
}

fn figure_from_args(args: &str, alt: &str, caption: Option<&str>) -> Option<Value> {
    let (filename, rest) = args.split_once("、横")?;
    let (width, rest) = rest.split_once('×')?;
    let height = rest.strip_prefix('縦')?;
    let width = width.parse::<u32>().ok()?;
    let height = height.parse::<u32>().ok()?;
    let mut figure = json!({
        "kind": "figure",
        "filename": filename,
        "alt": alt,
        "css_class": "",
        "width": width,
        "height": height
    });
    if let Some(caption) = caption {
        figure.as_object_mut().expect("figure object").insert(
            "caption".to_owned(),
            json!([{"kind": "text", "value": caption}]),
        );
    }
    Some(figure)
}

fn caption_visible_text(caption: &Value) -> Option<String> {
    let items = caption.as_array()?;
    let text = items.iter().filter_map(text_node_value).collect::<String>();
    if text.is_empty() { None } else { Some(text) }
}

fn aozora_nodes_visible_text(nodes: &[Node]) -> String {
    nodes.iter().map(Node::to_text).collect()
}

fn font_size_type_name(size_type: FontSizeType) -> &'static str {
    match size_type {
        FontSizeType::Dai => "larger",
        FontSizeType::Sho => "smaller",
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

fn style_type_name(style_type: StyleType) -> &'static str {
    match style_type {
        StyleType::SesameDot
        | StyleType::WhiteSesameDot
        | StyleType::BlackCircle
        | StyleType::WhiteCircle
        | StyleType::BlackTriangle
        | StyleType::WhiteTriangle
        | StyleType::Bullseye
        | StyleType::Fisheye
        | StyleType::Saltire
        | StyleType::SesameDotAfter
        | StyleType::WhiteSesameDotAfter
        | StyleType::BlackCircleAfter
        | StyleType::WhiteCircleAfter
        | StyleType::BlackTriangleAfter
        | StyleType::WhiteTriangleAfter
        | StyleType::BullseyeAfter
        | StyleType::FisheyeAfter
        | StyleType::SaltireAfter => "boten",
        StyleType::UnderlineSolid
        | StyleType::UnderlineDouble
        | StyleType::UnderlineDotted
        | StyleType::UnderlineDashed
        | StyleType::UnderlineWave
        | StyleType::OverlineSolid
        | StyleType::OverlineDouble
        | StyleType::OverlineDotted
        | StyleType::OverlineDashed
        | StyleType::OverlineWave => "bousen",
        StyleType::Bold => "bold",
        StyleType::Italic => "italic",
        StyleType::Subscript => "subscript",
        StyleType::Superscript => "superscript",
    }
}

fn boten_kind(style_type: StyleType) -> Option<&'static str> {
    match style_type {
        StyleType::SesameDot | StyleType::SesameDotAfter => Some("sesame"),
        StyleType::WhiteSesameDot | StyleType::WhiteSesameDotAfter => Some("white_sesame"),
        StyleType::BlackCircle | StyleType::BlackCircleAfter => Some("black_circle"),
        StyleType::WhiteCircle | StyleType::WhiteCircleAfter => Some("white_circle"),
        StyleType::BlackTriangle | StyleType::BlackTriangleAfter => Some("black_triangle"),
        StyleType::WhiteTriangle | StyleType::WhiteTriangleAfter => Some("white_triangle"),
        StyleType::Bullseye | StyleType::BullseyeAfter => Some("bullseye"),
        StyleType::Fisheye | StyleType::FisheyeAfter => Some("fisheye"),
        StyleType::Saltire | StyleType::SaltireAfter => Some("saltire"),
        _ => None,
    }
}

fn line_kind(style_type: StyleType) -> Option<&'static str> {
    match style_type {
        StyleType::UnderlineSolid | StyleType::OverlineSolid => Some("solid"),
        StyleType::UnderlineDouble | StyleType::OverlineDouble => Some("double"),
        StyleType::UnderlineDotted | StyleType::OverlineDotted => Some("dotted"),
        StyleType::UnderlineDashed | StyleType::OverlineDashed => Some("dashed"),
        StyleType::UnderlineWave | StyleType::OverlineWave => Some("wave"),
        _ => None,
    }
}

fn style_is_left_placement(style_type: StyleType) -> bool {
    matches!(
        style_type,
        StyleType::SesameDotAfter
            | StyleType::WhiteSesameDotAfter
            | StyleType::BlackCircleAfter
            | StyleType::WhiteCircleAfter
            | StyleType::BlackTriangleAfter
            | StyleType::WhiteTriangleAfter
            | StyleType::BullseyeAfter
            | StyleType::FisheyeAfter
            | StyleType::SaltireAfter
            | StyleType::OverlineSolid
            | StyleType::OverlineDouble
            | StyleType::OverlineDotted
            | StyleType::OverlineDashed
            | StyleType::OverlineWave
    )
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
        assert_eq!(font_size[0]["size_type"], "larger");
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
        assert_eq!(blocks[1]["x-indent"], 2);
        assert_eq!(blocks[1]["children"][0]["kind"], "paragraph");
        assert_eq!(blocks[1]["children"][0]["content"][0]["value"], "字下げ");
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
        assert_eq!(blocks[0]["x-indent"], 2);
        assert_eq!(blocks[0]["children"][0]["kind"], "keigakomi_block");
        assert_eq!(blocks[0]["children"][0]["children"][0]["kind"], "paragraph");
        assert_eq!(
            blocks[0]["children"][0]["children"][0]["content"][0]["value"],
            "囲み"
        );
    }

    #[test]
    fn build_aat_reconstructs_heading_block_from_marker() {
        let decoded = DecodedSource {
            text: "序章［＃「序章」の大見出し］".to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                .to_owned(),
        };

        let aat = build_aat(&decoded);
        let blocks = aat["blocks"].as_array().expect("blocks");

        assert_eq!(blocks[0]["kind"], "heading");
        assert_eq!(blocks[0]["level"], 1);
        assert_eq!(blocks[0]["style"], "normal");
        assert_eq!(blocks[0]["content"][0]["value"], "序章");
    }

    #[test]
    fn build_aat_reconstructs_heading_block_from_midashi_node() {
        let decoded = DecodedSource {
            text: "同行見出し［＃「同行見出し」は同行中見出し］".to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                .to_owned(),
        };

        let aat = build_aat(&decoded);
        let blocks = aat["blocks"].as_array().expect("blocks");

        assert_eq!(blocks[0]["kind"], "heading");
        assert_eq!(blocks[0]["level"], 2);
        assert_eq!(blocks[0]["style"], "dogyo");
        assert_eq!(blocks[0]["content"][0]["value"], "同行見出し");
    }

    #[test]
    fn build_aat_reconstructs_line_jisage_as_style() {
        let decoded = DecodedSource {
            text: "字下げ行［＃この行2字下げ］".to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                .to_owned(),
        };

        let aat = build_aat(&decoded);
        let style = &aat["blocks"][0]["content"][0];

        assert_eq!(style["kind"], "style");
        assert_eq!(style["style_type"], "jisage_line");
        assert_eq!(style["x-indent"], 2);
        assert_eq!(style["content"][0]["value"], "字下げ行");
    }

    #[test]
    fn build_aat_splits_page_break_after_line_jisage_style() {
        let decoded = DecodedSource {
            text: "字下げ行［＃この行2字下げ］\n［＃改ページ］\n次".to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                .to_owned(),
        };

        let aat = build_aat(&decoded);
        let blocks = aat["blocks"].as_array().expect("blocks");

        assert_eq!(blocks[0]["content"][0]["kind"], "style");
        assert_eq!(blocks[0]["content"][0]["style_type"], "jisage_line");
        assert_eq!(blocks[0]["content"][0]["content"][0]["value"], "字下げ行");
        assert_eq!(blocks[1]["kind"], "paragraph");
        assert_eq!(blocks[1]["x-break-kind"], "page");
        assert_eq!(blocks[2]["content"][0]["value"], "次");
        assert!(
            !serde_json::to_string(&aat)
                .unwrap()
                .contains(r#""_page_break""#)
        );
    }

    #[test]
    fn page_break_splitter_splits_nested_style_content() {
        let mut children = Vec::new();
        push_paragraphs_with_page_breaks(
            &mut children,
            vec![json!({
                "kind": "style",
                "style_type": "jisage_line",
                "x-indent": 3,
                "content": [
                    { "kind": "text", "value": "前" },
                    { "kind": "_page_break" }
                ]
            })],
        );

        assert_eq!(children[0]["content"][0]["kind"], "style");
        assert_eq!(children[0]["content"][0]["content"][0]["value"], "前");
        assert_eq!(children[1]["kind"], "paragraph");
        assert_eq!(children[1]["x-break-kind"], "page");
        assert!(
            !serde_json::to_string(&children)
                .unwrap()
                .contains(r#""_page_break""#)
        );
    }

    #[test]
    fn page_break_splitter_splits_deeply_nested_style_content() {
        let mut children = Vec::new();
        push_paragraphs_with_page_breaks(
            &mut children,
            vec![json!({
                "kind": "style",
                "style_type": "chitsuki",
                "x-align": "right",
                "x-width": 2,
                "content": [
                    { "kind": "text", "value": "前" },
                    {
                        "kind": "style",
                        "style_type": "boten",
                        "x-boten-kind": "sesame",
                        "content": [
                            { "kind": "text", "value": "傍点" },
                            { "kind": "_page_break" }
                        ]
                    },
                    { "kind": "text", "value": "後" }
                ]
            })],
        );

        assert_eq!(children[0]["content"][0]["kind"], "style");
        assert_eq!(children[0]["content"][0]["style_type"], "chitsuki");
        assert_eq!(
            children[0]["content"][0]["content"][1]["content"][0]["value"],
            "傍点"
        );
        assert_eq!(children[1]["kind"], "paragraph");
        assert_eq!(children[1]["x-break-kind"], "page");
        assert_eq!(children[2]["content"][0]["content"][0]["value"], "後");
        assert!(
            !serde_json::to_string(&children)
                .unwrap()
                .contains(r#""_page_break""#)
        );
    }

    #[test]
    fn page_break_splitter_projects_ruby_base_content_across_page_break() {
        let mut children = Vec::new();
        push_paragraphs_with_page_breaks(
            &mut children,
            vec![json!({
                "kind": "ruby",
                "base": "前後",
                "reading": "ぜんご",
                "direction": "right",
                "base_content": [
                    { "kind": "text", "value": "前" },
                    { "kind": "_page_break" },
                    { "kind": "text", "value": "後" }
                ],
                "reading_content": [{ "kind": "text", "value": "ぜんご" }]
            })],
        );

        assert_eq!(children[0]["content"][0]["value"], "前");
        assert_eq!(children[1]["kind"], "paragraph");
        assert_eq!(children[1]["x-break-kind"], "page");
        assert_eq!(children[2]["content"][0]["value"], "後");
        assert!(
            !serde_json::to_string(&children)
                .unwrap()
                .contains(r#""_page_break""#)
        );
    }

    #[test]
    fn build_aat_reconstructs_line_chitsuki_as_style() {
        let decoded = DecodedSource {
            text: "右寄せ［＃この行地付き］".to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                .to_owned(),
        };

        let aat = build_aat(&decoded);
        let style = &aat["blocks"][0]["content"][0];

        assert_eq!(style["kind"], "style");
        assert_eq!(style["style_type"], "chitsuki");
        assert_eq!(style["x-align"], "right");
        assert_eq!(style["content"][0]["value"], "右寄せ");
    }

    #[test]
    fn build_aat_reconstructs_jizume_block_as_style_scope() {
        let decoded = DecodedSource {
            text: "［＃ここから字詰め4］\n本文\n［＃ここで字詰め終わり］".to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                .to_owned(),
        };

        let aat = build_aat(&decoded);
        let style = &aat["blocks"][0]["content"][0];

        assert_eq!(style["kind"], "style");
        assert_eq!(style["style_type"], "jizume");
        assert_eq!(style["x-width"], 4);
        assert_eq!(style["content"][0]["value"], "本文");
    }

    #[test]
    fn build_aat_splits_page_break_inside_style_block_scope() {
        let decoded = DecodedSource {
            text: "［＃ここから字詰め4］\n前\n［＃改ページ］\n後\n［＃ここで字詰め終わり］"
                .to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                .to_owned(),
        };

        let aat = build_aat(&decoded);
        let blocks = aat["blocks"].as_array().expect("blocks");

        assert_eq!(blocks[0]["content"][0]["style_type"], "jizume");
        assert_eq!(blocks[0]["content"][0]["content"][0]["value"], "前");
        assert_eq!(blocks[1]["kind"], "paragraph");
        assert_eq!(blocks[1]["x-break-kind"], "page");
        assert_eq!(blocks[2]["content"][0]["style_type"], "jizume");
        assert_eq!(blocks[2]["content"][0]["content"][0]["value"], "後");
        assert!(
            !serde_json::to_string(&aat)
                .unwrap()
                .contains(r#""_page_break""#)
        );
    }

    #[test]
    fn heading_flush_moves_page_break_marker_after_heading() {
        let mut frame = BlockFrame::root();
        frame
            .content
            .push(json!({"kind": "text", "value": "見出し"}));
        frame.content.push(json!({"kind": "_page_break"}));

        frame.flush_heading(MidashiLevel::Naka, MidashiStyle::default());

        assert_eq!(frame.children[0]["kind"], "heading");
        assert_eq!(frame.children[0]["content"][0]["value"], "見出し");
        assert_eq!(frame.children[1]["kind"], "paragraph");
        assert_eq!(frame.children[1]["x-break-kind"], "page");
        assert!(
            !serde_json::to_string(&frame.children)
                .unwrap()
                .contains(r#""_page_break""#)
        );
    }

    #[test]
    fn build_aat_reconstructs_burasage_block_as_style_scope() {
        let decoded = DecodedSource {
            text: "［＃ここから2字下げ、折り返して4字下げ］\n本文\n［＃ここで字下げ終わり］"
                .to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                .to_owned(),
        };

        let aat = build_aat(&decoded);
        let style = &aat["blocks"][0]["content"][0];

        assert_eq!(style["kind"], "style");
        assert_eq!(style["style_type"], "burasage");
        assert_eq!(style["x-indent-first"], 2);
        assert_eq!(style["x-indent-rest"], 4);
        assert_eq!(style["content"][0]["value"], "本文");
    }

    #[test]
    fn build_aat_reconstructs_tcy_block_as_inline_scope() {
        let decoded = DecodedSource {
            text: "［＃ここから縦中横］\n12\n［＃ここで縦中横終わり］".to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                .to_owned(),
        };

        let aat = build_aat(&decoded);
        let tcy = &aat["blocks"][0]["content"][0];

        assert_eq!(tcy["kind"], "tcy");
        assert_eq!(tcy["content"][0]["value"], "12");
    }

    #[test]
    fn build_aat_reconstructs_bold_and_italic_blocks_as_style_scopes() {
        let decoded = DecodedSource {
            text: "［＃ここから太字］\n太字\n［＃ここで太字終わり］\n［＃ここから斜体］\n斜体\n［＃ここで斜体終わり］"
                .to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                .to_owned(),
        };

        let aat = build_aat(&decoded);
        let bold = &aat["blocks"][0]["content"][0];
        let italic = &aat["blocks"][1]["content"][0];

        assert_eq!(bold["kind"], "style");
        assert_eq!(bold["style_type"], "bold");
        assert_eq!(bold["content"][0]["value"], "太字");
        assert_eq!(italic["kind"], "style");
        assert_eq!(italic["style_type"], "italic");
        assert_eq!(italic["content"][0]["value"], "斜体");
    }

    #[test]
    fn build_aat_reconstructs_font_size_blocks_as_font_size_scopes() {
        let decoded = DecodedSource {
            text: "［＃ここから2段階大きな文字］\n大\n［＃ここで大きな文字終わり］\n［＃ここから3段階小さな文字］\n小\n［＃ここで小さな文字終わり］"
                .to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                .to_owned(),
        };

        let aat = build_aat(&decoded);
        let larger = &aat["blocks"][0]["content"][0];
        let smaller = &aat["blocks"][1]["content"][0];

        assert_eq!(larger["kind"], "font_size");
        assert_eq!(larger["size_type"], "larger");
        assert_eq!(larger["level"], 2);
        assert_eq!(larger["content"][0]["value"], "大");
        assert_eq!(smaller["kind"], "font_size");
        assert_eq!(smaller["size_type"], "smaller");
        assert_eq!(smaller["level"], 3);
        assert_eq!(smaller["content"][0]["value"], "小");
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

    #[test]
    fn parse_inline_content_reconstructs_left_ruby_notes() {
        let content =
            parse_inline_content("青空文庫［＃「青空文庫」の左に「あおぞらぶんこ」のルビ］");

        assert_eq!(content.len(), 1);
        assert_eq!(content[0]["kind"], "ruby");
        assert_eq!(content[0]["base"], "青空文庫");
        assert_eq!(content[0]["reading"], "あおぞらぶんこ");
        assert_eq!(content[0]["direction"], "left");

        let content = parse_inline_content(
            "青空文庫《あおぞらぶんこ》［＃「青空文庫」の左に「aozora bunko」のルビ］",
        );

        assert_eq!(content.len(), 1);
        assert_eq!(content[0]["kind"], "ruby");
        assert_eq!(content[0]["base"], "青空文庫");
        assert_eq!(content[0]["reading"], "あおぞらぶんこ");
        assert_eq!(content[0]["direction"], "right");
        assert_eq!(content[0]["x-left-reading"], "aozora bunko");
    }

    #[test]
    fn parse_inline_content_marks_nested_left_ruby_target_as_raw_error() {
        let content = parse_inline_content(
            "青空文庫《あおぞらぶんこ》［＃「青空文庫《あおぞらぶんこ》」の左に「aozora bunko」のルビ］",
        );

        assert_eq!(content[0]["kind"], "ruby");
        assert_eq!(content[1]["kind"], "raw");
        assert_eq!(content[1]["x-error-kind"], "nested_ruby_forbidden");
    }

    #[test]
    fn parse_inline_content_marks_annotation_ruby_kinds() {
        let content = parse_inline_content("吹喋［＃「喋」の「ママ」の注記］");
        let ruby = content
            .iter()
            .find(|node| node["kind"] == "ruby")
            .expect("chuuki ruby");

        assert_eq!(ruby["base"], "喋");
        assert_eq!(ruby["reading"], "ママ");
        assert_eq!(ruby["x-annotation-type"], "chuuki");

        let content = parse_inline_content("血が流れ［＃「血が流れ」に「×」の傍記］");
        let ruby = content
            .iter()
            .find(|node| node["kind"] == "ruby")
            .expect("bouki ruby");

        assert_eq!(ruby["base"], "血が流れ");
        assert_eq!(ruby["reading"], "××××");
        assert_eq!(ruby["x-annotation-type"], "bouki");
    }

    #[test]
    fn parse_inline_content_normalizes_style_variants() {
        let content = parse_inline_content("おやじ［＃「おやじ」に白ゴマ傍点］");
        assert_eq!(content[0]["kind"], "style");
        assert_eq!(content[0]["style_type"], "boten");
        assert_eq!(content[0]["x-boten-kind"], "white_sesame");

        let content = parse_inline_content("傍線［＃「傍線」に二重傍線］");
        assert_eq!(content[0]["kind"], "style");
        assert_eq!(content[0]["style_type"], "bousen");
        assert_eq!(content[0]["x-line-kind"], "double");

        let content = parse_inline_content("強調［＃「強調」は太字］と斜体［＃「斜体」は斜体］");
        assert_eq!(content[0]["style_type"], "bold");
        assert_eq!(content[2]["style_type"], "italic");

        let content = parse_inline_content("左点［＃「左点」に左に傍点］");
        assert_eq!(content[0]["kind"], "style");
        assert_eq!(content[0]["style_type"], "boten");
        assert_eq!(content[0]["x-placement"], "left");

        let content = parse_inline_content("参照［＃「参照」に「強調」の傍点］");
        assert_eq!(content[0]["kind"], "style");
        assert_eq!(content[0]["style_type"], "boten");
        assert_eq!(content[0]["x-frontref"], "強調");
    }

    #[test]
    fn parse_inline_content_normalizes_misc_note_fallbacks() {
        let content = parse_inline_content("／＼");
        assert_eq!(content[0]["kind"], "gaiji");
        assert_eq!(content[0]["description"], "くの字点");
        assert_eq!(content[0]["resolved"], "〳〵");

        let content = parse_inline_content("※［＃濁点付き片仮名ヱ、1-7-84］エル");
        assert_eq!(content[0]["kind"], "gaiji");
        assert_eq!(content[0]["jis_code"], "1-7-84");

        let content = parse_inline_content("漢［＃返り点一］文");
        assert_eq!(content[0]["value"], "漢");
        assert_eq!(content[1]["kind"], "style");
        assert_eq!(content[1]["style_type"], "kaeriten");
        assert_eq!(content[1]["x-marker"], "一");
        assert_eq!(content[2]["value"], "文");

        let content = parse_inline_content("漢［＃訓点送り仮名「読」］文");
        assert_eq!(content[1]["kind"], "ruby");
        assert_eq!(content[1]["base"], "");
        assert_eq!(content[1]["reading"], "読");
        assert_eq!(content[1]["x-annotation-type"], "okurigana");
    }

    #[test]
    fn parse_blocks_normalizes_warigaki_figures_editor_notes_and_breaks() {
        let blocks = parse_blocks("本文［＃割書］注［＃割書終わり］続き");
        let content = blocks[0]["content"].as_array().expect("content");
        assert_eq!(content[1]["kind"], "warigaki");
        assert_eq!(content[1]["upper"][0]["value"], "注");

        let blocks = parse_blocks("猫の図（fig00001_01.png、横321×縦123）入る");
        let content = blocks[0]["content"].as_array().expect("content");
        assert_eq!(content[0]["kind"], "figure");
        assert_eq!(content[0]["filename"], "fig00001_01.png");
        assert_eq!(content[0]["width"], 321);
        assert_eq!(content[0]["height"], 123);

        let blocks = parse_blocks("本文［＃左頁］続き");
        let content = blocks[0]["content"].as_array().expect("content");
        assert_eq!(content[1]["kind"], "text");
        assert_eq!(content[1]["value"], "");
        assert_eq!(content[1]["x-editor-note"], "左頁");

        let blocks = parse_blocks("前［＃改行］後");
        let content = blocks[0]["content"].as_array().expect("content");
        assert_eq!(content[0]["kind"], "text");
        assert_eq!(content[0]["value"], "前\n後");
        assert_eq!(content[0]["x-break-kind"], "line");

        let blocks = parse_blocks("前の段落。\n［＃改ページ］\n後の段落。");
        assert_eq!(blocks[0]["content"][0]["value"], "前の段落。");
        assert_eq!(blocks[1]["kind"], "paragraph");
        assert_eq!(blocks[1]["x-break-kind"], "page");
        assert_eq!(blocks[2]["content"][0]["value"], "後の段落。");
    }

    #[test]
    fn parse_blocks_normalizes_captioned_figure_notes() {
        let blocks = parse_blocks(
            "［＃「猫の図」のキャプション付きの図（fig00001_01.png、横321×縦123）入る］\n猫の図［＃「猫の図」はキャプション］",
        );
        let figure = &blocks[0]["content"][0];

        assert_eq!(figure["kind"], "figure");
        assert_eq!(figure["filename"], "fig00001_01.png");
        assert_eq!(figure["alt"], "猫の図");
        assert_eq!(figure["width"], 321);
        assert_eq!(figure["height"], 123);
        assert_eq!(figure["caption"][0]["value"], "猫の図");
    }
}
