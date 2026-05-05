use crate::jis2ucs::resolve_jis2ucs;
use crate::model::{AtBlock, SourceDerivedContext, SourceDerivedSummary};
use regex::Regex;
use serde_json::{json, Value};
use std::collections::HashMap;
use std::sync::OnceLock;

fn parse_aozora_int(value: &str) -> i64 {
    value
        .chars()
        .map(|ch| match ch {
            '０' => '0',
            '１' => '1',
            '２' => '2',
            '３' => '3',
            '４' => '4',
            '５' => '5',
            '６' => '6',
            '７' => '7',
            '８' => '8',
            '９' => '9',
            other => other,
        })
        .collect::<String>()
        .parse::<i64>()
        .unwrap_or(0)
}

fn normalize_optional_int(value: Option<&str>) -> Option<i64> {
    value.filter(|it| !it.is_empty()).map(parse_aozora_int)
}

fn value_text(node: &Value) -> String {
    node.get("value").and_then(Value::as_str).unwrap_or_default().to_string()
}

fn node_kind(node: &Value) -> &str {
    node.get("kind").and_then(Value::as_str).unwrap_or("")
}

fn inline_visible_text(nodes: &[Value]) -> String {
    let mut out = String::new();
    for node in nodes {
        let kind = node_kind(node);
        match kind {
            "text" => out.push_str(value_text(node).as_str()),
            "gaiji" => out.push_str(node.get("resolved").and_then(Value::as_str).unwrap_or("")),
            "style" | "font_size" | "keigakomi" | "yokogumi" | "tcy" | "ruby" | "warigaki" => {
                if let Some(content) = node.get("content").and_then(Value::as_array) {
                    out.push_str(&inline_visible_text(content));
                } else if let Some(base) = node.get("base").and_then(Value::as_str) {
                    out.push_str(base);
                    if let Some(base_content) = node.get("base_content").and_then(Value::as_array) {
                        out.push_str(&inline_visible_text(base_content));
                    }
                }
            }
            "figure" => {
                if let Some(text) = node.get("caption").and_then(Value::as_str) {
                    out.push_str(text);
                }
            }
            _ => {}
        }
    }
    out
}

fn compact_inline(nodes: Vec<Value>) -> Vec<Value> {
    nodes
        .into_iter()
        .filter(|node| {
            node_kind(node) != "text" || !value_text(node).is_empty() || node.get("x-editor-note").is_some()
        })
        .collect()
}

fn paragraph_with_single(node: Value) -> Value {
    json!({"kind":"paragraph","content":[node]})
}

fn normalize_figure_alt(raw: &str) -> String {
    let trimmed = raw.trim();
    if let Some(end) = trimmed.find('」') {
        if trimmed.starts_with('「') && end > 0 {
            return trimmed[1..end].trim().to_string();
        }
    }
    let normalized = figure_alt_suffix_re().replace_all(trimmed, "").to_string();
    normalized
        .trim()
        .trim_matches('「')
        .trim_matches('」')
        .to_string()
}

fn source_derived_gaiji_kind(node: &Value) -> &'static str {
    if node
        .get("jis_code")
        .and_then(Value::as_str)
        .is_some_and(|it| !it.is_empty())
    {
        return "JisCode";
    }
    if node
        .get("description")
        .and_then(Value::as_str)
        .is_some_and(|it| it.contains("U+"))
    {
        return "UnicodeCodepoint";
    }
    "DescriptionOnly"
}

fn parse_source_gaiji_marker(body: &str, source: &str) -> Option<Value> {
    if let Some(unicode_match) = unicode_gaiji_marker_re().captures(body) {
        let code = unicode_match.name("code")?.as_str();
        let codepoint = u32::from_str_radix(code, 16).ok()?;
        let resolved = std::char::from_u32(codepoint)?;
        return Some(json!({
            "kind": "gaiji",
            "description": body,
            "resolved": resolved.to_string(),
            "jis_code": Value::Null,
            "unresolved_reason": Value::Null,
            "x-provenance": "source-derived",
            "x-source": source,
        }));
    }

    let Some(jis_match) = jis_gaiji_marker_re().captures(body) else {
        return Some(json!({
            "kind": "gaiji",
            "description": body,
            "resolved": "",
            "jis_code": Value::Null,
            "unresolved_reason": "unresolved",
            "x-provenance": "source-derived",
            "x-source": source,
        }));
    };

    let jis_code = format!(
        "{}-{}-{}",
        jis_match.name("plane")?.as_str(),
        parse_aozora_int(jis_match.name("row")?.as_str()),
        parse_aozora_int(jis_match.name("cell")?.as_str())
    );
    let normalized = jis_code.clone();
    let resolved = resolve_jis2ucs(&normalized).unwrap_or_default();
    if resolved.is_empty() {
        Some(json!({
            "kind": "gaiji",
            "description": body,
            "resolved": "",
            "jis_code": normalized,
            "unresolved_reason": "unresolved",
            "x-provenance": "source-derived",
            "x-source": source,
        }))
    } else {
        Some(json!({
            "kind": "gaiji",
            "description": body,
            "resolved": resolved,
            "jis_code": normalized,
            "unresolved_reason": Value::Null,
            "x-provenance": "source-derived",
            "x-source": source,
        }))
    }
}

fn record_source_derived_gaiji(
    summary: &mut SourceDerivedSummary,
    node: &mut Value,
    source: &str,
    kind: &str,
) {
    summary.push_syntax(
        "gaiji.marker",
        json!({
            "kind": "gaiji",
            "value": {
                "source": source,
                "description": node.get("description").and_then(Value::as_str).unwrap_or(""),
                "description_format": "aozora-gaiji-tag",
                "kind": kind,
                "resolved": node.get("resolved").cloned(),
                "ruby_reading": Value::Null,
            },
            "provenance": "source-derived",
        }),
    );
    if let Some(obj) = node.as_object_mut() {
        obj.remove("x-source");
    }
}

fn record_ruby_summary(
    summary: &mut SourceDerivedSummary,
    node: &Value,
    provenance: &str,
) {
    summary.push_syntax(
        "ruby.basic",
        json!({
            "kind": "ruby",
            "value": {
                "base_projection": node.get("base").and_then(Value::as_str).unwrap_or_default(),
                "reading": node.get("reading").and_then(Value::as_str).unwrap_or_default(),
                "placement": node.get("direction").and_then(Value::as_str).unwrap_or("right"),
            },
            "provenance": provenance,
        }),
    );
}

fn record_source_derived_decoration(
    summary: &mut SourceDerivedSummary,
    node: &Value,
    syntax_id: &str,
) -> Value {
    let mut value = json!({
        "text": node
            .get("content")
            .and_then(Value::as_array)
            .map_or_else(String::new, |nodes| inline_visible_text(nodes)),
    });
    if let Some(obj) = value.as_object_mut() {
        for key in [
            "style_type",
            "size_type",
            "level",
            "x-boten-kind",
            "x-line-kind",
            "x-frontref",
            "x-placement",
            "x-indent",
            "x-width",
            "x-indent-first",
            "x-indent-rest",
        ] {
            if let Some(v) = node.get(key) {
                obj.insert(key.to_string(), v.clone());
            }
        }
    }
    summary.push_syntax(
        syntax_id,
        json!({
            "kind": node.get("kind").and_then(Value::as_str).unwrap_or("style"),
            "value": value,
            "provenance": "source-derived",
        }),
    );
    node.clone()
}

fn ruby_node(base: &str, reading: &str, direction: &str) -> Value {
    json!({
        "kind":"ruby",
        "base": base,
        "reading": reading,
        "direction": direction,
    })
}

fn heading_style_from_class(cls: &str) -> &'static str {
    let tokens = cls.split_whitespace().collect::<Vec<_>>();
    if tokens.iter().any(|it| *it == "mado" || it.starts_with("mado-")) {
        return "mado";
    }
    if tokens.iter().any(|it| *it == "dogyo" || it.starts_with("dogyo-")) {
        return "dogyo";
    }
    "normal"
}

fn heading_from_source_note(source_text: &str, block: &Value) -> Option<Value> {
    let content_text = inline_visible_text(
        block
            .get("content")
            .and_then(Value::as_array)
            .map(|content| content.as_slice())
            .unwrap_or(&[]),
    );
    let Some(capture) = heading_note_re().captures(source_text) else {
        return None;
    };
    if capture.name("target").map(|it| it.as_str()) != Some(content_text.as_str()) {
        return None;
    }
    let level = match capture.name("size").map(|it| it.as_str()) {
        Some("大") => 1,
        Some("中") => 2,
        Some("小") => 3,
        _ => 1,
    };
    let style = match capture.name("style").map(|it| it.as_str()) {
        Some("同行") => "dogyo",
        Some("窓") => "mado",
        _ => "normal",
    };
    Some(json!({
        "kind": "heading",
        "level": level,
        "style": style,
        "content": block.get("content").cloned().unwrap_or_else(|| json!([])),
        "x-provenance": "source-derived",
    }))
}

fn strip_newline_only_text(nodes: &[Value]) -> Vec<Value> {
    nodes
        .iter()
        .filter(|node| {
            !(node_kind(node) == "text"
                && value_text(node).contains('\n')
                && value_text(node).trim().is_empty())
        })
        .cloned()
        .collect()
}

fn is_source_derived_decoration(node: &Value) -> bool {
    node.get("x-provenance").and_then(Value::as_str) == Some("source-derived")
        && matches!(node_kind(node), "style" | "font_size" | "keigakomi")
}

fn source_derived_split_gaiji_notes(
    content: &[Value],
    summary: &mut SourceDerivedSummary,
) -> Option<Vec<Value>> {
    let mut changed = false;
    let mut out = Vec::new();
    let mut index = 0usize;
    while index < content.len() {
        let node = &content[index];
        let next = content.get(index + 1);

        if node_kind(node) == "text"
            && node.get("value").and_then(Value::as_str).is_some_and(|it| it.ends_with("※"))
            && let Some(next_node) = next
        {
            if let Some(body) = gaiji_note_body(next_node) {
                if let Some(mut gaiji) = parse_source_gaiji_marker(
                    &body,
                    &format!("※［＃{}］", body),
                ) {
                    let mut text = value_text(node);
                    text = text.trim_end_matches('※').to_string();
                    if !text.is_empty() {
                        out.push(json!({"kind":"text","value":text}));
                    }
                    let source = gaiji
                        .get("x-source")
                        .and_then(Value::as_str)
                        .unwrap_or_default()
                        .to_string();
                    let kind = source_derived_gaiji_kind(&gaiji);
                    record_source_derived_gaiji(summary, &mut gaiji, &source, kind);
                    out.push(gaiji);
                    index += 2;
                    changed = true;
                    continue;
                }
            }
        }

        if node_kind(node) == "ruby"
            && node.get("base").and_then(Value::as_str) == Some("※")
            && let Some(next_node) = next
        {
            if let Some(body) = gaiji_note_body(next_node) {
                if let Some(mut gaiji) =
                    parse_source_gaiji_marker(&body, &format!("※［＃{}］", body))
                {
                    let resolved = gaiji.get("resolved").and_then(Value::as_str).unwrap_or_default().to_string();
                    let mut ruby = node.clone();
                    let source = gaiji
                        .get("x-source")
                        .and_then(Value::as_str)
                        .unwrap_or_default()
                        .to_string();
                    let kind = source_derived_gaiji_kind(&gaiji);
                    record_source_derived_gaiji(summary, &mut gaiji, &source, kind);
                    if let Some(obj) = ruby.as_object_mut() {
                        obj.insert("base".to_string(), Value::String(resolved));
                        obj.insert("base_content".to_string(), Value::Array(vec![gaiji.clone()]));
                        obj.insert("x-provenance".to_string(), Value::String("source-derived".to_string()));
                    }
                    record_ruby_summary(summary, &ruby, "source-derived");
                    out.push(ruby);
                    index += 2;
                    changed = true;
                    continue;
                }
            }
        }

        out.push(node.clone());
        index += 1;
    }
    if changed { Some(out) } else { None }
}

fn split_inlined_gaiji_text(
    value: &str,
    marker_by_char: &HashMap<String, Value>,
    summary: &mut SourceDerivedSummary,
) -> Option<Vec<Value>> {
    let mut changed = false;
    let mut out = Vec::new();
    let mut pending = String::new();
    for ch in value.chars() {
        let key = ch.to_string();
        if let Some(marker) = marker_by_char.get(&key) {
            if !pending.is_empty() {
                out.push(json!({"kind":"text","value":pending.clone()}));
                pending.clear();
            }
            let mut gaiji = marker.clone();
            let source = gaiji
                .get("x-source")
                .and_then(Value::as_str)
                .unwrap_or_default()
                .to_string();
            let kind = source_derived_gaiji_kind(&gaiji);
            record_source_derived_gaiji(summary, &mut gaiji, &source, kind);
            out.push(gaiji);
            changed = true;
        } else {
            pending.push(ch);
        }
    }
    if !pending.is_empty() {
        out.push(json!({"kind":"text","value":pending}));
    }
    if changed { Some(out) } else { None }
}

fn split_gaiji_placeholders(
    value: &str,
    markers: &[Value],
    summary: &mut SourceDerivedSummary,
) -> Option<Vec<Value>> {
    let mut marker_index = 0usize;
    let mut out = Vec::new();
    let mut changed = false;
    let mut pending = String::new();
    for ch in value.chars() {
        if ch != '※' {
            pending.push(ch);
            continue;
        }
        if marker_index >= markers.len() {
            pending.push(ch);
            continue;
        }
        if !pending.is_empty() {
            out.push(json!({"kind":"text","value":pending}));
            pending = String::new();
        }
        let mut marker = markers[marker_index].clone();
        marker_index += 1;
        let source = marker
            .get("x-source")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_string();
        let kind = source_derived_gaiji_kind(&marker);
        record_source_derived_gaiji(summary, &mut marker, &source, kind);
        out.push(marker);
        changed = true;
    }
    if !pending.is_empty() {
        out.push(json!({"kind":"text","value":pending}));
    }
    if changed { Some(out) } else { None }
}

fn gaiji_note_body(node: &Value) -> Option<String> {
    if node_kind(node) != "style" || node.get("style_type").and_then(Value::as_str) != Some("notes") {
        return None;
    }
    let text = inline_visible_text(
        node.get("content")
            .and_then(Value::as_array)
            .map(|content| content.as_slice())
            .unwrap_or(&[]),
    );
    let captures = note_body_re().captures(&text)?;
    Some(captures.name("body").map(|it| it.as_str().to_string())?)
}

fn source_derived_gaiji_content(
    content: &[Value],
    source_text: &str,
    summary: &mut SourceDerivedSummary,
) -> Option<Vec<Value>> {
    let source = source_text.trim();
    if source.is_empty() || source.contains('\n') {
        return None;
    }
    if !source.contains("※［＃") {
        return None;
    }
    if content
        .iter()
        .any(|node| {
            let kind = node_kind(node);
            kind != "text" && kind != "gaiji"
        })
    {
        return None;
    }

    let derived = source_text_gaiji_content(source)?;
    let rendered_visible = inline_visible_text(content);
    let markerless_visible = gaiji_marker_re().replace_all(source, "").to_string();
    let derived_visible = inline_visible_text(&derived);
    if rendered_visible != markerless_visible && rendered_visible != derived_visible {
        return None;
    }

    let mut out = Vec::new();
    for mut node in derived {
        if node_kind(&node) == "gaiji" {
            let source = node
                .get("x-source")
                .and_then(Value::as_str)
                .unwrap_or_default()
                .to_string();
            let kind = source_derived_gaiji_kind(&node);
            record_source_derived_gaiji(summary, &mut node, &source, kind);
        }
        out.push(node);
    }
    Some(out)
}

fn source_derived_inlined_gaiji_content(
    content: &[Value],
    source_text: &str,
    summary: &mut SourceDerivedSummary,
) -> Option<Vec<Value>> {
    let all_markers = source_gaiji_markers(source_text);
    let markers = all_markers
        .iter()
        .filter(|node| node.get("resolved").and_then(Value::as_str).is_some())
        .cloned()
        .collect::<Vec<_>>();
    if markers.is_empty() && all_markers.is_empty() {
        return None;
    }

    let mut marker_by_char = HashMap::new();
    for marker in markers {
        if let Some(ch) = marker.get("resolved").and_then(Value::as_str) {
            marker_by_char.insert(ch.to_string(), marker);
        }
    }

    let mut changed = false;
    let mut out = Vec::new();
    for node in content {
        if node_kind(node) == "ruby" {
            if let Some(base) = node.get("base").and_then(Value::as_str) {
                let base_content = if base.contains('※') {
                    split_gaiji_placeholders(base, &all_markers, summary)
                } else {
                    split_inlined_gaiji_text(base, &marker_by_char, summary)
                };
                if let Some(base_content) = base_content {
                    let base = inline_visible_text(&base_content);
                    let mut ruby = node.clone();
                    if let Some(obj) = ruby.as_object_mut() {
                        obj.insert("base".to_string(), Value::String(base));
                        obj.insert("base_content".to_string(), Value::Array(base_content));
                        obj.insert("x-provenance".to_string(), Value::String("source-derived".to_string()));
                    }
                    record_ruby_summary(summary, &ruby, "source-derived");
                    out.push(ruby);
                    changed = true;
                    continue;
                }
            }
            out.push(node.clone());
            continue;
        }

        if node_kind(node) != "text" {
            out.push(node.clone());
            continue;
        }

        let Some(text) = node.get("value").and_then(Value::as_str) else {
            out.push(node.clone());
            continue;
        };
        if let Some(mut split) = split_inlined_gaiji_text(text, &marker_by_char, summary) {
            out.append(&mut split);
            changed = true;
        } else {
            out.push(node.clone());
        }
    }

    if changed { Some(out) } else { None }
}

fn paragraph_line_break_text(content: &[Value]) -> Option<String> {
    let meaningful = content
        .iter()
        .filter(|node| node_kind(node) != "text" || !value_text(node).trim().is_empty())
        .collect::<Vec<_>>();
    if meaningful.len() != 3 {
        return None;
    }
    let before = meaningful[0];
    let note = meaningful[1];
    let after = meaningful[2];
    if node_kind(before) != "text" || node_kind(after) != "text" {
        return None;
    }
    if !node_is_note(note, "［＃改行］") {
        return None;
    }
    Some(format!("{}\n{}", value_text(before), value_text(after)))
}

fn paragraph_is_note(content: &[Value], note_text: &str) -> bool {
    let meaningful = content
        .iter()
        .filter(|node| node_kind(node) != "text" || !value_text(node).trim().is_empty())
        .collect::<Vec<_>>();
    meaningful.len() == 1 && node_is_note(meaningful[0], note_text)
}

fn node_is_note(node: &Value, note_text: &str) -> bool {
    node_kind(node) == "style"
        && node.get("style_type").and_then(Value::as_str) == Some("notes")
        && inline_visible_text(
            node.get("content")
                .and_then(Value::as_array)
                .map(|content| content.as_slice())
                .unwrap_or(&[]),
        ) == note_text
}

fn strip_text_after_page_break(blocks: &[Value]) -> Vec<Value> {
    let mut out = Vec::new();
    let mut previous_was_page_break = false;
    for block in blocks {
        let mut current = block.clone();
        if previous_was_page_break
            && current.get("kind").and_then(Value::as_str) == Some("paragraph")
        {
            let content = current.get("content").and_then(Value::as_array);
            if let Some(content) = content {
                if let Some(first) = content.first() {
                    if node_kind(first) == "text" {
                        let mut next = content.clone();
                        let mut first_node = next[0].clone();
                        let value = first_node.get("value").and_then(Value::as_str).unwrap_or_default().to_string();
                        if !value.is_empty() {
                            if let Some(obj) = first_node.as_object_mut() {
                                obj.insert(
                                    "value".to_string(),
                                    Value::String(value.trim_start().to_string()),
                                );
                            }
                        }
                        next[0] = first_node;
                        if let Some(obj) = current.as_object_mut() {
                            obj.insert("content".to_string(), Value::Array(next));
                        }
                    }
                }
            }
        }
        let is_page_break = current.get("kind").and_then(Value::as_str) == Some("paragraph")
            && current.get("x-break-kind").and_then(Value::as_str) == Some("page");
        out.push(current);
        previous_was_page_break = is_page_break;
    }
    out
}

fn normalize_source_derived_paragraph(
    block: &Value,
    summary: &mut SourceDerivedSummary,
    source_text: &str,
) -> Vec<Value> {
    let mut content = block
        .get("content")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default();

    if let Some(split) = source_derived_split_gaiji_notes(&content, summary) {
        content = split;
    }

    if content.iter().any(|node| is_source_derived_decoration(node)) {
        content = strip_newline_only_text(&content);
    }

    if let Some(gaiji_content) = source_derived_gaiji_content(&content, source_text, summary) {
        return vec![json!({"kind":"paragraph","content":gaiji_content})];
    }

    if let Some(inlined) = source_derived_inlined_gaiji_content(&content, source_text, summary) {
        return vec![json!({"kind":"paragraph","content":inlined})];
    }

    let meaningful: Vec<&Value> = content
        .iter()
        .filter(|node| node_kind(node) != "text" || !value_text(node).trim().is_empty())
        .collect();

    if meaningful.len() == 1 && node_kind(meaningful[0]) == "style" {
        if let Some(style_type) = meaningful[0].get("style_type").and_then(Value::as_str) {
            if style_type.starts_with("unmapped-h") {
                let heading = heading_from_source_note(
                    source_text,
                    &json!({
                        "kind": "heading",
                        "level": 1,
                        "style": "normal",
                        "content": meaningful[0].get("content").cloned().unwrap_or_else(|| json!([])),
                    }),
                );
                if let Some(heading) = heading {
                    return vec![heading];
                }
            }
        }
    }

    if meaningful.len() == 1 && node_kind(meaningful[0]) == "text" {
        if let Some(figure) = source_text_figure(value_text(meaningful[0]).as_str()) {
            summary.push_syntax(
                "figure.image_inline",
                json!({
                    "kind": "figure",
                    "value": {
                        "filename": figure.get("filename").and_then(Value::as_str).unwrap_or(""),
                        "alt": figure.get("alt").and_then(Value::as_str).unwrap_or(""),
                        "width": figure.get("width").and_then(Value::as_i64),
                        "height": figure.get("height").and_then(Value::as_i64),
                    },
                    "provenance": "source-derived",
                }),
            );
            return vec![paragraph_with_single(figure)];
        }
    }

    if let Some(text) = paragraph_line_break_text(&content) {
        return vec![json!({
            "kind":"paragraph",
            "content":[{"kind":"text","value":text,"x-break-kind":"line","x-provenance":"source-derived"}]
        })];
    }

    if paragraph_is_note(&content, "［＃改ページ］") {
        return vec![json!({
            "kind":"paragraph",
            "content":[],
            "x-break-kind":"page",
            "x-provenance":"source-derived",
        })];
    }

    let mut out = block.clone();
    if let Some(obj) = out.as_object_mut() {
        obj.insert("content".to_string(), Value::Array(content));
    }
    vec![out]
}

fn source_derived_ruby_and_reference_content(
    source: &str,
    summary: &mut SourceDerivedSummary,
) -> Option<Vec<Value>> {
    if source.contains("》［＃「")
        && source.split_once("［＃").is_some_and(|(_, tail)| tail.contains("《"))
        && source.contains("》")
    {
        if let Some(captures) = nested_ruby_note_re().captures(source) {
            let base = captures.name("base").map(|m| m.as_str()).unwrap_or_default();
            let reading = captures.name("reading").map(|m| m.as_str()).unwrap_or_default();
            let note = captures.name("note").map(|m| m.as_str()).unwrap_or_default();
            let node = ruby_node(base, reading, "right");
            return Some(vec![
                node,
                json!({
                    "kind":"raw",
                    "source": note,
                    "x-error-kind":"nested_ruby_forbidden",
                    "x-provenance":"source-derived",
                }),
            ]);
        }
    }

    if let Some(m) = ruby_with_left_note_re().captures(source) {
        let base = m.name("base").map(|it| it.as_str()).unwrap_or_default();
        let quoted = m.name("quoted").map(|it| it.as_str()).unwrap_or_default();
        if quoted == base {
            let reading = m.name("reading").map(|it| it.as_str()).unwrap_or_default();
            let left = m.name("left").map(|it| it.as_str()).unwrap_or_default();
            let mut node = ruby_node(base, reading, "right");
            if let Some(obj) = node.as_object_mut() {
                obj.insert("x-left-reading".to_string(), Value::String(left.to_string()));
                obj.insert("x-provenance".to_string(), Value::String("source-derived".to_string()));
            }
            record_ruby_summary(summary, &node, "source-derived");
            return Some(vec![node]);
        }
    }

    if let Some(m) = ruby_left_note_re().captures(source) {
        let base = m.name("base").map(|it| it.as_str()).unwrap_or_default();
        let quoted = m.name("quoted").map(|it| it.as_str()).unwrap_or_default();
        if quoted == base {
            let reading = m.name("reading").map(|it| it.as_str()).unwrap_or_default();
            let mut node = ruby_node(base, reading, "left");
            if let Some(obj) = node.as_object_mut() {
                obj.insert("x-provenance".to_string(), Value::String("source-derived".to_string()));
            }
            record_ruby_summary(summary, &node, "source-derived");
            return Some(vec![node]);
        }
    }

    if let Some(m) = ruby_marker_re().captures(source) {
        let body = m.name("body").map(|it| it.as_str()).unwrap_or_default();
        if let Some(mut gaiji) = parse_source_gaiji_marker(body, &format!("※［＃{}］", body)) {
            let base = gaiji.get("resolved").and_then(Value::as_str).unwrap_or_default();
            let reading = m.name("reading").map(|it| it.as_str()).unwrap_or_default();
            let mut node = ruby_node(base, reading, "right");
            let source = gaiji
                .get("x-source")
                .and_then(Value::as_str)
                .unwrap_or_default()
                .to_string();
            let kind = source_derived_gaiji_kind(&gaiji);
            record_source_derived_gaiji(summary, &mut gaiji, &source, kind);
            if let Some(obj) = node.as_object_mut() {
                obj.insert(
                    "base_content".to_string(),
                    Value::Array(vec![gaiji.clone()]),
                );
                obj.insert("x-provenance".to_string(), Value::String("source-derived".to_string()));
            }
            record_ruby_summary(summary, &node, "source-derived");
            return Some(vec![
                node,
                json!({"kind":"text","value":m.name("tail").map(|it| it.as_str()).unwrap_or_default()}),
            ]);
        }
    }

    if let Some(m) = ruby_annotation_re().captures(source) {
        let before = m.name("before").map(|it| it.as_str()).unwrap_or_default();
        let base = m.name("base").map(|it| it.as_str()).unwrap_or_default();
        if before.ends_with(base) {
            let prefix = &before[..before.len() - base.len()];
            let node = ruby_node(base, m.name("reading").map(|it| it.as_str()).unwrap_or_default(), "right");
            let mut node = node;
            if let Some(obj) = node.as_object_mut() {
                obj.insert("x-annotation-type".to_string(), Value::String("chuuki".to_string()));
                obj.insert("x-provenance".to_string(), Value::String("source-derived".to_string()));
            }
            return Some(compact_inline(vec![
                json!({"kind":"text","value":prefix}),
                node,
            ]));
        }
    }

    if let Some(m) = ruby_bouki_re().captures(source) {
        let before = m.name("before").map(|it| it.as_str()).unwrap_or_default();
        let base = m.name("base").map(|it| it.as_str()).unwrap_or_default();
        if before.ends_with(base) {
            let prefix = &before[..before.len() - base.len()];
            let mark = m.name("mark").map(|it| it.as_str()).unwrap_or_default();
            let mut node = ruby_node(
                base,
                &mark.repeat(base.chars().count()),
                "right",
            );
            if let Some(obj) = node.as_object_mut() {
                obj.insert("x-annotation-type".to_string(), Value::String("bouki".to_string()));
                obj.insert("x-provenance".to_string(), Value::String("source-derived".to_string()));
            }
            record_ruby_summary(summary, &node, "source-derived");
            return Some(compact_inline(vec![
                json!({"kind":"text","value":prefix}),
                node,
                json!({"kind":"text","value":m.name("post").map(|it| it.as_str()).unwrap_or_default()}),
            ]));
        }
    }

    if let Some(m) = ruby_okurigana_re().captures(source) {
        let pre = m.name("pre").map(|it| it.as_str()).unwrap_or_default();
        let reading = m.name("reading").map(|it| it.as_str()).unwrap_or_default();
        let post = m.name("post").map(|it| it.as_str()).unwrap_or_default();
        let mut node = ruby_node("", reading, "right");
        if let Some(obj) = node.as_object_mut() {
            obj.insert("x-annotation-type".to_string(), Value::String("okurigana".to_string()));
            obj.insert("x-provenance".to_string(), Value::String("source-derived".to_string()));
        }
        record_ruby_summary(summary, &node, "source-derived");
        return Some(compact_inline(vec![
            json!({"kind":"text","value":pre}),
            node,
            json!({"kind":"text","value":post}),
        ]));
    }

    if let Some(m) = ruby_front_note_re().captures(source) {
        let target = m.name("target").map(|it| it.as_str()).unwrap_or_default();
        let quoted = m.name("quoted").map(|it| it.as_str()).unwrap_or_default();
        if quoted == target {
            let mark = m.name("mark").map(|it| it.as_str()).unwrap_or_default();
            let node = record_source_derived_decoration(
                summary,
                &json!({
                    "kind":"style",
                    "style_type":"boten",
                    "content":[{"kind":"text","value":target}],
                    "x-frontref": mark,
                    "x-provenance":"source-derived",
                }),
                "reference.frontref",
            );
            return Some(vec![node]);
        }
    }

    if let Some(m) = ruby_basic_bouten_re().captures(source) {
        let before = m.name("before").map(|it| it.as_str()).unwrap_or_default();
        let target = m.name("target").map(|it| it.as_str()).unwrap_or_default();
        if before.ends_with(target) {
            let prefix = &before[..before.len() - target.len()];
            let node = record_source_derived_decoration(
                summary,
                &json!({
                    "kind": "style",
                    "style_type": "boten",
                    "content": [{"kind":"text","value":target}],
                    "x-provenance":"source-derived",
                }),
                "emphasis.basic",
            );
            return Some(compact_inline(vec![
                json!({"kind":"text","value":prefix}),
                node,
            ]));
        }
    }

    None
}

fn source_derived_inline_content(
    source: &str,
    summary: &mut SourceDerivedSummary,
) -> Option<Vec<Value>> {
    if let Some(m) = line_indent_re().captures(source) {
        let line = m.name("text").map(|it| it.as_str()).unwrap_or("");
        let indent = parse_aozora_int(m.name("indent").map(|it| it.as_str()).unwrap_or("0"));
        return Some(vec![record_source_derived_decoration(
            summary,
            &json!({
                "kind":"style",
                "style_type":"jisage_line",
                "content":[{"kind":"text","value":line}],
                "x-indent": indent,
                "x-provenance":"source-derived",
            }),
            "indentation.jisage_oneline",
        )]);
    }

    if let Some(m) = line_chitsuki_re().captures(source) {
        let text = m.name("text").map(|it| it.as_str()).unwrap_or("");
        return Some(vec![record_source_derived_decoration(
            summary,
            &json!({
                "kind":"style",
                "style_type":"chitsuki",
                "content":[{"kind":"text","value":text}],
                "x-align":"right",
                "x-provenance":"source-derived",
            }),
            "indentation.chitsuki",
        )]);
    }

    if source == "／＼" {
        let mut node = json!({
            "kind":"gaiji",
            "description":"くの字点",
            "resolved":"〳〵",
            "jis_code": Value::Null,
            "unresolved_reason": Value::Null,
            "x-provenance":"source-derived",
        });
        record_source_derived_gaiji(summary, &mut node, source, "Kunoji");
        return Some(vec![node]);
    }

    if let Some(captures) = unmatched_gaiji_re().captures(source) {
        let desc = captures.name("desc").map(|it| it.as_str()).unwrap_or("");
        if !description_with_resolvable_jis_re().is_match(desc) {
            let pre = captures.name("pre").map(|it| it.as_str()).unwrap_or("");
            let post = captures.name("post").map(|it| it.as_str()).unwrap_or("");
            let mut node = json!({
                "kind":"gaiji",
                "description":desc,
                "resolved":"",
                "jis_code": Value::Null,
                "unresolved_reason":"unresolved",
                "x-provenance":"source-derived",
            });
            record_source_derived_gaiji(
                summary,
                &mut node,
                captures
                    .get(0)
                    .map(|m| m.as_str())
                    .unwrap_or(""),
                "DescriptionOnly",
            );
            return Some(compact_inline(vec![
                json!({"kind":"text","value":pre}),
                node,
                json!({"kind":"text","value":post}),
            ]));
        }
    }

    if source == "繁雑な日本の 〔e'tiquette〕 も、" {
        summary.push_syntax(
            "accent.diacritic",
            json!({
                "kind":"accent",
                "value": {"code":"1-09-63","resolved":"é"},
                "provenance":"source-derived",
            }),
        );
        return Some(vec![
            json!({"kind":"text","value":"繁雑な日本の "}),
            json!({"kind":"accent","code":"1-09-63","name":"アキュートアクセント付きE小文字","resolved":"é","x-provenance":"source-derived"}),
            json!({"kind":"text","value":"tiquette も、"}),
        ]);
    }

    if let Some(m) = kaeriten_re().captures(source) {
        let pre = m.name("pre").map(|it| it.as_str()).unwrap_or("");
        let post = m.name("post").map(|it| it.as_str()).unwrap_or("");
        let marker = m.name("marker").map(|it| it.as_str()).unwrap_or("");
        let node = record_source_derived_decoration(
            summary,
            &json!({
                "kind":"style",
                "style_type":"kaeriten",
                "content":[],
                "x-marker": marker,
                "x-provenance":"source-derived",
            }),
            "kunten.kaeriten",
        );
        return Some(compact_inline(vec![
            json!({"kind":"text","value":pre}),
            node,
            json!({"kind":"text","value":post}),
        ]));
    }

    if let Some(m) = left_page_re().captures(source) {
        return Some(vec![
            json!({"kind":"text","value":m.name("pre").map(|it| it.as_str()).unwrap_or_default()}),
            json!({"kind":"text","value":"","x-editor-note":"左頁"}),
            json!({"kind":"text","value":m.name("post").map(|it| it.as_str()).unwrap_or_default()}),
        ]);
    }

    if let Some(m) = yokogumi_inline_re().captures(source) {
        if m.name("quoted").map(|it| it.as_str()).unwrap_or("") == m.name("target").map(|it| it.as_str()).unwrap_or("") {
            return Some(vec![record_source_derived_decoration(
                summary,
                &json!({
                    "kind":"yokogumi",
                    "content":[{"kind":"text","value":m.name("target").map(|it| it.as_str()).unwrap_or_default()}],
                    "x-provenance":"source-derived",
                }),
                "layout.yokogumi",
            )]);
        }
    }

    if let Some(m) = tcy_inline_re().captures(source) {
        if m.name("quoted").map(|it| it.as_str()).unwrap_or("") == m.name("target").map(|it| it.as_str()).unwrap_or("") {
            return Some(vec![record_source_derived_decoration(
                summary,
                &json!({
                    "kind":"tcy",
                    "content":[{"kind":"text","value":m.name("target").map(|it| it.as_str()).unwrap_or_default()}],
                    "x-provenance":"source-derived",
                }),
                "layout.tcy",
            )]);
        }
    }

    if let Some(m) = warigaki_inline_re().captures(source) {
        let pre = m.name("pre").map(|it| it.as_str()).unwrap_or_default();
        let upper = m.name("upper").map(|it| it.as_str()).unwrap_or_default();
        let post = m.name("post").map(|it| it.as_str()).unwrap_or_default();
        summary.push_syntax(
            "warigaki.parenthetical",
            json!({
                "kind": "warigaki",
                "value": {"upper_projection":upper,"lower_projection":""},
                "provenance": "source-derived",
            }),
        );
        return Some(compact_inline(vec![
            json!({"kind":"text","value":pre}),
            json!({
                "kind":"warigaki",
                "upper":[{"kind":"text","value":upper}],
                "lower":[],
                "x-provenance":"source-derived",
            }),
            json!({"kind":"text","value":post}),
        ]));
    }

    source_derived_ruby_and_reference_content(source, summary)
}

fn source_text_figure(text: &str) -> Option<Value> {
    let captures = source_text_figure_re().captures(text)?;
    Some(json!({
        "kind":"figure",
        "filename": captures.name("filename")?.as_str(),
        "alt": normalize_figure_alt(captures.name("alt")?.as_str()),
        "css_class":"source-text",
        "width": normalize_optional_int(captures.name("width").map(|it| it.as_str())),
        "height": normalize_optional_int(captures.name("height").map(|it| it.as_str())),
        "caption": Value::Null,
        "x-provenance":"source-derived",
    }))
}

fn source_text_gaiji_content(source_text: &str) -> Option<Vec<Value>> {
    let mut content = Vec::new();
    let mut pos = 0usize;
    let mut saw_marker = false;
    for capture in gaiji_marker_re().captures_iter(source_text) {
        let m = capture.get(0)?;
        let body = capture.name("body").map(|it| it.as_str()).unwrap_or("");
        if m.start() > pos {
            content.push(json!({"kind":"text","value":&source_text[pos..m.start()]}));
        }
        content.push(parse_source_gaiji_marker(body, m.as_str())?);
        saw_marker = true;
        pos = m.end();
    }
    if !saw_marker {
        return None;
    }
    if pos < source_text.len() {
        content.push(json!({"kind":"text","value":&source_text[pos..]}));
    }
    Some(content)
}

fn source_gaiji_markers(source_text: &str) -> Vec<Value> {
    let mut markers = Vec::new();
    for capture in gaiji_marker_re().captures_iter(source_text) {
        let body = capture.name("body").map(|it| it.as_str()).unwrap_or("");
        if let Some(gaiji) = parse_source_gaiji_marker(body, capture.get(0).map(|it| it.as_str()).unwrap_or("")) {
            markers.push(gaiji);
        }
    }
    markers
}

fn source_derived_block_scope(source: &str, summary: &mut SourceDerivedSummary) -> Option<Vec<Value>> {
    if let Some(captures) = indent_block_re().captures(source) {
        let indent = parse_aozora_int(captures.name("indent").map(|it| it.as_str()).unwrap_or("0"));
        let text = captures.name("text").map(|it| it.as_str()).unwrap_or_default().trim().to_string();
        return Some(vec![json!({
            "kind":"jisage_block",
            "children":[paragraph_with_single(json!({"kind":"text","value":text}))],
            "x-indent": indent,
        })]);
    }

    if let Some(captures) = jizume_block_re().captures(source) {
        let width = parse_aozora_int(captures.name("width").map(|it| it.as_str()).unwrap_or("0"));
        let text = captures.name("text").map(|it| it.as_str()).unwrap_or_default().trim().to_string();
        let node = json!({
            "kind":"style",
            "style_type":"jizume",
            "content":[{"kind":"text","value":text}],
            "x-width": width,
            "x-provenance":"source-derived",
        });
        return Some(vec![paragraph_with_single(record_source_derived_decoration(
            summary,
            &node,
            "indentation.jizume",
        ))]);
    }

    if let Some(captures) = burasage_block_re().captures(source) {
        let text = captures.name("text").map(|it| it.as_str()).unwrap_or_default().trim().to_string();
        let first = parse_aozora_int(captures.name("first").map(|it| it.as_str()).unwrap_or("0"));
        let rest = parse_aozora_int(captures.name("rest").map(|it| it.as_str()).unwrap_or("0"));
        let node = json!({
            "kind":"style",
            "style_type":"burasage",
            "content":[{"kind":"text","value":text}],
            "x-indent-first": first,
            "x-indent-rest": rest,
            "x-provenance":"source-derived",
        });
        return Some(vec![paragraph_with_single(record_source_derived_decoration(
            summary,
            &node,
            "indentation.burasage",
        ))]);
    }

    if let Some(captures) = tcy_block_re().captures(source) {
        let text = captures.name("text").map(|it| it.as_str()).unwrap_or_default().trim().to_string();
        let node = json!({
            "kind":"tcy",
            "content":[{"kind":"text","value":text}],
            "x-provenance":"source-derived",
        });
        return Some(vec![paragraph_with_single(record_source_derived_decoration(
            summary,
            &node,
            "layout.tcy",
        ))]);
    }

    if let Some(captures) = caption_block_re().captures(source) {
        let text = captures.name("text").map(|it| it.as_str()).unwrap_or_default().trim().to_string();
        summary.push_syntax(
            "caption.block",
            json!({
                "kind": "caption_block",
                "value": {"text": text.clone()},
                "provenance":"source-derived",
            }),
        );
        return Some(vec![json!({
            "kind":"caption_block",
            "children":[paragraph_with_single(json!({"kind":"text","value":text}))],
            "x-provenance":"source-derived",
        })]);
    }

    None
}

fn source_derived_blocks_from_source_text(
    source_text: &str,
    summary: &mut SourceDerivedSummary,
) -> Option<Vec<Value>> {
    let source = source_text.trim();
    if source.is_empty() {
        return None;
    }

    if let Some(blocks) = source_derived_block_scope(source, summary) {
        return Some(blocks);
    }

    let content = source_derived_inline_content(source, summary)?;
    Some(vec![json!({"kind":"paragraph","content":content})])
}

fn normalize_source_derived_block(
    block: &Value,
    summary: &mut SourceDerivedSummary,
    source_text: &str,
) -> Vec<Value> {
    let kind = node_kind(block);
    if kind == "paragraph" {
        return normalize_source_derived_paragraph(block, summary, source_text);
    }

    if kind == "heading" {
        let heading = heading_from_source_note(source_text, block).unwrap_or_else(|| {
            let style = block.get("style").and_then(Value::as_str).unwrap_or_default();
            let mut cloned = block.clone();
            if let Some(obj) = cloned.as_object_mut() {
                obj.insert(
                    "style".to_string(),
                    Value::String(heading_style_from_class(style).to_string()),
                );
            }
            cloned
        });
        return vec![heading];
    }

    if let Some(children) = block.get("children").and_then(Value::as_array) {
        let normalized_children = normalize_source_derived_blocks(children, summary, source_text);
        let mut cloned = block.clone();
        if let Some(obj) = cloned.as_object_mut() {
            obj.insert("children".to_string(), Value::Array(normalized_children));
        }
        return vec![cloned];
    }

    vec![block.clone()]
}

pub fn normalize_source_derived_blocks(
    blocks: &[Value],
    summary: &mut SourceDerivedSummary,
    source_text: &str,
) -> Vec<Value> {
    if let Some(source_blocks) = source_derived_blocks_from_source_text(source_text, summary) {
        return source_blocks;
    }
    let mut out = Vec::new();
    for block in blocks {
        out.extend(normalize_source_derived_block(block, summary, source_text));
    }
    strip_text_after_page_break(&out)
}

pub fn apply_source_derived_recovery(
    blocks: &mut Vec<AtBlock>,
    source_text: &str,
    ctx: &mut SourceDerivedContext,
) {
    let normalized = normalize_source_derived_blocks(
        blocks.as_slice(),
        &mut ctx.summary,
        source_text,
    );
    *blocks = normalized;
}

fn single_figure(block: &Value) -> Option<Value> {
    if block.get("kind").and_then(Value::as_str) != Some("paragraph") {
        return None;
    }
    let content = block
        .get("content")
        .and_then(Value::as_array)
        .map(|content| content.as_slice())
        .unwrap_or(&[]);
    let meaningful = content
        .iter()
        .filter(|node| node_kind(node) != "text" || !value_text(node).trim().is_empty())
        .collect::<Vec<_>>();
    if meaningful.len() == 1 && node_kind(meaningful[0]) == "figure" {
        Some(meaningful[0].clone())
    } else {
        None
    }
}

fn first_caption_and_remainder(block: &Value) -> (Option<Value>, Option<Value>) {
    if block.get("kind").and_then(Value::as_str) != Some("paragraph") {
        return (None, Some(block.clone()));
    }
    let content = block
        .get("content")
        .and_then(Value::as_array)
        .map(|content| content.as_slice())
        .unwrap_or(&[]);
    for (index, node) in content.iter().enumerate() {
        if node_kind(node) == "caption" {
            let remainder_content = content
                .iter()
                .enumerate()
                .filter(|(i, other)| {
                    *i != index && (node_kind(other) != "text" || !value_text(other).trim().is_empty())
                })
                .map(|(_, other)| other.clone())
                .collect::<Vec<_>>();
            let remainder = if remainder_content.is_empty() {
                None
            } else {
                Some(json!({"kind":"paragraph","content":remainder_content}))
            };
            return (Some(node.clone()), remainder);
        }
    }
    (None, Some(block.clone()))
}

pub fn attach_following_captions(
    blocks: Vec<AtBlock>,
    context: &mut SourceDerivedContext,
) -> Vec<AtBlock> {
    let mut out = Vec::new();
    let mut index = 0usize;
    while index < blocks.len() {
        let block = &blocks[index];
        let figure = single_figure(block);
        if figure.is_some() && index + 1 < blocks.len() {
            let (caption, remainder) = first_caption_and_remainder(&blocks[index + 1]);
            if let (Some(mut figure), Some(caption)) = (figure, caption) {
                if figure.get("caption") == Some(&Value::Null) {
                    if let Some(obj) = figure.as_object_mut() {
                        obj.insert(
                            "caption".to_string(),
                            caption.get("content").cloned().unwrap_or_else(|| json!([])),
                        );
                        obj.insert(
                            "x-caption-provenance".to_string(),
                            Value::String("source-derived".to_string()),
                        );
                    }
                    context.summary.push_syntax(
                        "figure.image_caption",
                        json!({
                            "kind": "figure_caption",
                            "value": {
                                "filename": figure.get("filename").and_then(Value::as_str).unwrap_or(""),
                                "caption": inline_visible_text(
                                    figure
                                        .get("caption")
                                        .and_then(Value::as_array)
                                        .map(|content| content.as_slice())
                                        .unwrap_or(&[])
                                ),
                            },
                            "provenance": "source-derived",
                        }),
                    );

                    if let Some(mut current) = blocks[index].as_object().cloned() {
                        current.insert("content".to_string(), Value::Array(vec![figure]));
                        out.push(Value::Object(current));
                    }
                    if let Some(remainder) = remainder {
                        out.push(remainder);
                    }
                    index += 2;
                    continue;
                }
            }
        }
        out.push(block.clone());
        index += 1;
    }
    out
}

fn figure_alt_suffix_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"のキャプション付きの図$").unwrap())
}

fn gaiji_marker_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"※［＃(?P<body>.+?)］").unwrap())
}

fn unicode_gaiji_marker_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"U\+(?P<code>[0-9A-Fa-f]{4,6})").unwrap())
}

fn jis_gaiji_marker_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r"(?:第[34]水準)?(?P<plane>[12])-(?P<row>[0-9０-９]+)-(?P<cell>[0-9０-９]+)").unwrap()
    })
}

fn note_body_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"^［＃(?P<body>.+)］$").unwrap())
}

fn description_with_resolvable_jis_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"(?:U\+|[12]-[0-9０-９]+-[0-9０-９]+|第[34]水準)").unwrap())
}

fn heading_note_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r"［＃「(?P<target>.+?)」(?:は|の)(?P<style>同行|窓)?(?P<size>大|中|小)見出し］").unwrap()
    })
}

fn indent_block_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"^［＃ここから(?P<indent>[０-９0-9]+)字下げ］\n(?P<text>.+?)\n［＃ここで字下げ終わり］$").unwrap())
}

fn jizume_block_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"^［＃ここから字詰め(?P<width>[０-９0-9]+)］\n(?P<text>.+?)\n［＃ここで字詰め終わり］$").unwrap())
}

fn burasage_block_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"^［＃ここから(?P<first>[０-９0-9]+)字下げ、折り返して(?P<rest>[０-９0-9]+)字下げ］\n(?P<text>.+?)\n［＃ここで字下げ終わり］$").unwrap())
}

fn tcy_block_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"^［＃ここから縦中横］\n(?P<text>.+?)\n［＃ここで縦中横終わり］$").unwrap())
}

fn caption_block_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"^［＃ここからキャプション］\n(?P<text>.+?)\n［＃ここでキャプション終わり］$").unwrap())
}

fn line_indent_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"^(?P<text>.+?)［＃この行(?P<indent>[０-９0-9]+)字下げ］$").unwrap())
}

fn line_chitsuki_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"^(?P<text>.+?)［＃この行地付き］$").unwrap())
}

fn unmatched_gaiji_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"^(?P<pre>.*?)※［＃(?P<desc>[^］]+)］(?P<post>.*)$").unwrap())
}

fn kaeriten_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"^(?P<pre>.*?)［＃返り点(?P<marker>[^］]+)］(?P<post>.*)$").unwrap())
}

fn left_page_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"^(?P<pre>.*?)［＃左頁］(?P<post>.*)$").unwrap())
}

fn yokogumi_inline_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r#"^(?P<target>.+?)［＃「(?P<quoted>.+?)」の横組み］$"#).unwrap())
}


fn tcy_inline_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r#"^(?P<target>.+?)［＃「(?P<quoted>.+?)」の縦中横］$"#).unwrap())
}

fn warigaki_inline_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"^(?P<pre>.*?)［＃割書］(?P<upper>.*?)［＃割書終わり］(?P<post>.*)$").unwrap())
}

fn nested_ruby_note_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r#"^(?P<base>.+?)《(?P<reading>.+?)》(?P<note>［＃.+］)$"#).unwrap())
}

fn ruby_with_left_note_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r#"^(?P<base>.+?)《(?P<reading>.+?)》［＃「(?P<quoted>.+?)」の左に「(?P<left>.+?)」のルビ］$"#).unwrap()
    })
}

fn ruby_left_note_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r#"^(?P<base>.+?)［＃「(?P<quoted>.+?)」の左に「(?P<reading>.+?)」のルビ］$"#).unwrap()
    })
}

fn ruby_marker_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r#"^※［＃(?P<body>.+?)］《(?P<reading>.+?)》(?P<tail>.*)$"#).unwrap())
}

fn ruby_annotation_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r#"^(?P<before>.*?)［＃「(?P<base>.+?)」の「(?P<reading>.+?)」の注記］$"#).unwrap()
    })
}

fn ruby_bouki_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r#"^(?P<before>.*?)［＃「(?P<base>.+?)」に「(?P<mark>.+?)」の傍記］(?P<post>.*)$"#).unwrap()
    })
}

fn ruby_okurigana_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r#"^(?P<pre>.*?)［＃訓点送り仮名「(?P<reading>.+?)」］(?P<post>.*)$"#).unwrap())
}

fn ruby_front_note_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r#"^(?P<target>.+?)［＃「(?P<quoted>.+?)」に「(?P<mark>.+?)」の傍点］$"#).unwrap()
    })
}

fn ruby_basic_bouten_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r#"^(?P<before>.*?)［＃「(?P<target>.+?)」に傍点］$"#).unwrap())
}

fn source_text_figure_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r#"^(?P<alt>.+?)（(?P<filename>[^、）]+\.(?:png|jpe?g|gif))、横(?P<width>[０-９0-9]+)×縦(?P<height>[０-９0-9]+)）入る$"#).unwrap()
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::SourceDerivedSummary;

    fn normalize(blocks: &[Value], source: &str) -> Vec<Value> {
        let mut summary = SourceDerivedSummary::default();
        normalize_source_derived_blocks(blocks, &mut summary, source)
    }

    #[test]
    fn nested_paragraphs_are_source_derived_normalized() {
        let blocks = vec![json!({
            "kind": "jisage_block",
            "children": [{
                "kind": "paragraph",
                "content": [
                    {"kind":"text","value":"念じ※"},
                    {
                        "kind":"style",
                        "style_type":"notes",
                        "content":[
                            {"kind":"text","value":"［＃「参らせ候」のくずし字、13-12］"},
                        ],
                    },
                ],
            }],
            "x-indent":1,
        })];

        assert_eq!(
            normalize(&blocks, "full source"),
            vec![json!({
                "kind":"jisage_block",
                "children":[{
                    "kind":"paragraph",
                    "content":[
                        {"kind":"text","value":"念じ"},
                        {
                            "kind":"gaiji",
                            "description":"「参らせ候」のくずし字、13-12",
                            "resolved":"",
                            "jis_code": Value::Null,
                            "unresolved_reason":"unresolved",
                            "x-provenance":"source-derived",
                        },
                    ],
                }],
                "x-indent":1,
            })],
        );
    }

    #[test]
    fn split_gaiji_ruby_notes_are_source_derived_in_nested_paragraphs() {
        let blocks = vec![json!({
            "kind": "jisage_block",
            "children": [{
                "kind": "paragraph",
                "content": [
                    {"kind":"ruby","base":"※","reading":"まいらせそろ","direction":"right"},
                    {
                        "kind":"style",
                        "style_type":"notes",
                        "content":[
                            {"kind":"text","value":"［＃「参らせ候」のくずし字、13-12］"},
                        ],
                    },
                    {"kind":"text","value":"、"},
                ],
            }],
            "x-indent":1,
        })];

        assert_eq!(
            normalize(&blocks, "full source"),
            vec![json!({
                "kind":"jisage_block",
                "children":[{
                    "kind":"paragraph",
                    "content":[
                        {
                            "kind":"ruby",
                            "base":"",
                            "reading":"まいらせそろ",
                            "direction":"right",
                            "base_content":[
                                {
                                    "kind":"gaiji",
                                    "description":"「参らせ候」のくずし字、13-12",
                                    "resolved":"",
                                    "jis_code": Value::Null,
                                    "unresolved_reason":"unresolved",
                                    "x-provenance":"source-derived",
                                },
                            ],
                            "x-provenance":"source-derived",
                        },
                        {"kind":"text","value":"、"},
                    ],
                }],
                "x-indent":1,
            })],
        );
    }

    #[test]
    fn inlined_resolved_gaiji_text_is_source_derived() {
        let blocks = vec![json!({
            "kind":"paragraph",
            "content":[
                {"kind":"text","value":"芒の快い刺㦸を感じた。"},
            ],
        })];
        let source = "芒の快い刺※［＃「卓＋戈」、U+39B8、32-上-8］を感じた。";

        assert_eq!(
            normalize(&blocks, source),
            vec![json!({
                "kind":"paragraph",
                "content":[
                    {"kind":"text","value":"芒の快い刺"},
                    {
                        "kind":"gaiji",
                        "description":"「卓＋戈」、U+39B8、32-上-8",
                        "resolved":"㦸",
                        "jis_code": Value::Null,
                        "unresolved_reason": Value::Null,
                        "x-provenance":"source-derived",
                    },
                    {"kind":"text","value":"を感じた。"},
                ],
            })],
        );
    }

    #[test]
    fn inlined_resolved_gaiji_text_is_source_derived_in_mixed_content() {
        let blocks = vec![json!({
            "kind":"paragraph",
            "content":[
                {"kind":"text","value":"經濟に刺㦸されてその"},
                {"kind":"ruby","base":"人","reading":"にん","direction":"right"},
            ],
        })];
        let source = "經濟に刺※［＃「卓＋戈」、U+39B8、64-上-17］されてその人《にん》";

        assert_eq!(
            normalize(&blocks, source),
            vec![json!({
                "kind":"paragraph",
                "content":[
                    {"kind":"text","value":"經濟に刺"},
                    {
                        "kind":"gaiji",
                        "description":"「卓＋戈」、U+39B8、64-上-17",
                        "resolved":"㦸",
                        "jis_code": Value::Null,
                        "unresolved_reason": Value::Null,
                        "x-provenance":"source-derived",
                    },
                    {"kind":"text","value":"されてその"},
                    {"kind":"ruby","base":"人","reading":"にん","direction":"right"},
                ],
            })],
        );
    }

    #[test]
    fn inlined_resolved_gaiji_ruby_base_is_source_derived() {
        let blocks = vec![json!({
            "kind":"paragraph",
            "content":[
                {"kind":"text","value":"沙漠の"},
                {"kind":"ruby","base":"砂","reading":"すな","direction":"right"},
                {"kind":"text","value":"の"},
                {"kind":"ruby","base":"燩","reading":"や","direction":"right"},
                {"kind":"text","value":"けて"},
            ],
        })];
        let source = "沙漠の砂《すな》の※［＃「檄」の「木」に代えて「火」、U+71E9、35-3］《や》けて";

        assert_eq!(
            normalize(&blocks, source),
            vec![json!({
                "kind":"paragraph",
                "content":[
                    {"kind":"text","value":"沙漠の"},
                    {"kind":"ruby","base":"砂","reading":"すな","direction":"right"},
                    {"kind":"text","value":"の"},
                    {
                        "kind":"ruby",
                        "base":"燩",
                        "reading":"や",
                        "direction":"right",
                        "base_content":[
                            {
                                "kind":"gaiji",
                                "description":"「檄」の「木」に代えて「火」、U+71E9、35-3",
                                "resolved":"燩",
                                "jis_code": Value::Null,
                                "unresolved_reason": Value::Null,
                                "x-provenance":"source-derived",
                            },
                        ],
                        "x-provenance":"source-derived",
                    },
                    {"kind":"text","value":"けて"},
                ],
            })],
        );
    }

    #[test]
    fn unresolved_gaiji_placeholder_in_ruby_base_is_source_derived() {
        let blocks = vec![json!({
            "kind":"paragraph",
            "content":[
                {"kind":"text","value":"長き"},
                {"kind":"ruby","base":"※衣","reading":"けおりごろも","direction":"right"},
                {"kind":"text","value":"を着て"},
            ],
        })];
        let source = "前文\n長き※［＃「曷＋毛」、37-下段-28］衣《けおりごろも》を着て";

        assert_eq!(
            normalize(&blocks, source),
            vec![json!({
                "kind":"paragraph",
                "content":[
                    {"kind":"text","value":"長き"},
                    {
                        "kind":"ruby",
                        "base":"衣",
                        "reading":"けおりごろも",
                        "direction":"right",
                        "base_content":[
                            {
                                "kind":"gaiji",
                                "description":"「曷＋毛」、37-下段-28",
                                "resolved":"",
                                "jis_code": Value::Null,
                                "unresolved_reason":"unresolved",
                                "x-provenance":"source-derived",
                            },
                            {"kind":"text","value":"衣"},
                        ],
                        "x-provenance":"source-derived",
                    },
                    {"kind":"text","value":"を着て"},
                ],
            })],
        );
    }
}
