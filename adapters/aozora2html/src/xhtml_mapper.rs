use crate::model::{AtBlock, MappingError, SourceDerivedSummary};
use regex::Regex;
use roxmltree::{Document, Node};
use serde_json::{json, Value};
use std::sync::OnceLock;

fn parser_text(value: &str) -> String {
    value.replace('｜', "").replace('|', "")
}

fn node_name<'a>(node: Node<'a, 'a>) -> &'a str {
    node.tag_name().name()
}

fn node_class<'a>(node: Node<'a, 'a>) -> &'a str {
    node.attribute("class").unwrap_or("")
}

fn text_only<'a>(node: Node<'a, 'a>) -> String {
    node.descendants()
        .filter_map(|node| node.text())
        .collect::<String>()
}

fn inline_visible_text(nodes: &[Value]) -> String {
    let mut out = String::new();
    for node in nodes {
        let kind = node.get("kind").and_then(Value::as_str);
        match kind {
            Some("text") => out.push_str(node.get("value").and_then(Value::as_str).unwrap_or_default()),
            Some("gaiji") => out.push_str(node.get("resolved").and_then(Value::as_str).unwrap_or_default()),
            Some("style") => {
                let style_type = node.get("style_type").and_then(Value::as_str);
                if style_type == Some("notes") || style_type == Some("kaeriten") {
                    continue;
                }
                if let Some(content) = node.get("content").and_then(Value::as_array) {
                    out.push_str(&inline_visible_text(content));
                }
            }
            _ => {
                if let Some(content) = node.get("content").and_then(Value::as_array) {
                    out.push_str(&inline_visible_text(content));
                }
            }
        }
    }
    out
}

fn paragraph_has_content(nodes: &[Value]) -> bool {
    nodes.iter().any(|node| {
        node.get("kind").and_then(Value::as_str) != Some("text")
            || !node.get("value").and_then(Value::as_str).unwrap_or_default().trim().is_empty()
    })
}

fn walk_inline_children<'a>(
    node: Node<'a, 'a>,
    warnings: &mut Vec<Value>,
    summary: &mut SourceDerivedSummary,
    ruby_reading: Option<&str>,
) -> Vec<AtBlock> {
    let mut out = Vec::new();

    if let Some(text) = node.text() {
        let value = parser_text(text);
        if !value.is_empty() {
            out.push(json!({"kind": "text", "value": value}));
        }
    }

    for child in node.children().filter(|child| child.is_element()) {
        out.extend(map_inline(child, warnings, summary, ruby_reading));
        if let Some(tail) = child.tail() {
            let value = parser_text(tail);
            if !value.is_empty() {
                out.push(json!({"kind": "text", "value": value}));
            }
        }
    }

    out
}

fn source_derived_decoration_node(
    class: &str,
    content: Vec<Value>,
    summary: &mut SourceDerivedSummary,
) -> Option<Value> {
    let tokens: Vec<_> = class.split_whitespace().collect();
    if tokens.is_empty() {
        return None;
    }

    let primary = tokens[0];
    let content = clean_decoration_content(content);
    let node = match primary {
        "white_sesame_dot" => {
            json!({
                "kind": "style",
                "style_type": "boten",
                "content": content,
                "x-boten-kind": "white_sesame",
                "x-provenance": "source-derived",
            })
        }
        "sesame_dot_after" => {
            json!({
                "kind": "style",
                "style_type": "boten",
                "content": content,
                "x-placement": "left",
                "x-provenance": "source-derived",
            })
        }
        "underline_double" => {
            json!({
                "kind": "style",
                "style_type": "bousen",
                "content": content,
                "x-line-kind": "double",
                "x-provenance": "source-derived",
            })
        }
        "futoji" => {
            json!({
                "kind": "style",
                "style_type": "bold",
                "content": content,
                "x-provenance": "source-derived",
            })
        }
        "shatai" => {
            json!({
                "kind": "style",
                "style_type": "italic",
                "content": content,
                "x-provenance": "source-derived",
            })
        }
        "keigakomi" => {
            json!({
                "kind": "keigakomi",
                "content": content,
                "x-provenance": "source-derived",
            })
        }
        _ => {
            if let Some(font) = font_size_from_class(primary, content.clone()) {
                return Some(record_source_derived_decoration(summary, "decoration.font_size", font));
            }
            return None;
        }
    };

    let syntax = match primary {
        "white_sesame_dot" => "decoration.boten",
        "sesame_dot_after" => "decoration.direction_override",
        "underline_double" => "decoration.bousen",
        "futoji" | "shatai" => "decoration.bold_italic",
        "keigakomi" => "decoration.keigakomi",
        _ => "decoration.font_size",
    };
    Some(record_source_derived_decoration(summary, syntax, node))
}

fn font_size_from_class(class: &str, content: Vec<Value>) -> Option<Value> {
    let re = font_size_re();
    let captures = re.captures(class)?;
    let kind = captures.name("kind").map(|m| m.as_str())?;
    let level = captures
        .name("level")
        .and_then(|m| m.as_str().parse::<i64>().ok())?;
    Some(json!({
        "kind": "font_size",
        "size_type": if kind == "dai" { "larger" } else { "smaller" },
        "level": level,
        "content": content,
        "x-provenance": "source-derived",
    }))
}

fn record_source_derived_decoration(
    summary: &mut SourceDerivedSummary,
    syntax_id: &str,
    node: Value,
) -> Value {
    let mut value = json!({
        "text": node
            .get("content")
            .and_then(Value::as_array)
            .map_or_else(String::new, |nodes| inline_visible_text(nodes)),
    });
    if let Some(container) = value.as_object_mut() {
        for key in [
            "style_type",
            "size_type",
            "level",
            "x-boten-kind",
            "x-line-kind",
            "x-placement",
        ] {
            if let Some(v) = node.get(key) {
                container.insert(key.to_string(), v.clone());
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
    node
}

fn clean_decoration_content(nodes: Vec<Value>) -> Vec<Value> {
    let mut out = Vec::new();
    for mut node in nodes {
        let kind = node.get("kind").and_then(Value::as_str).unwrap_or_default();
        if kind == "raw" && node.get("source").and_then(Value::as_str) == Some("<br/>") {
            continue;
        }
        if kind == "text" {
            let mut value = node.get("value").and_then(Value::as_str).unwrap_or_default().to_string();
            if value.contains('\n') {
                value = value.trim().to_string();
            }
            if value.is_empty() {
                continue;
            }
            if let Some(obj) = node.as_object_mut() {
                obj.insert("value".to_string(), Value::String(value));
            }
            out.push(node);
            continue;
        }
        if let Some(content) = node.get("content").and_then(Value::as_array).map(|nodes| nodes.to_vec()) {
            if let Some(obj) = node.as_object_mut() {
                obj.insert(
                    "content".to_string(),
                    Value::Array(clean_decoration_content(content)),
                );
            }
        }
        out.push(node);
    }
    out
}

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

fn normalize_optional_int(value: &str) -> Option<i64> {
    if value.is_empty() {
        None
    } else {
        Some(parse_aozora_int(value))
    }
}

fn normalize_figure_alt(raw: &str) -> String {
    let mut text = raw.trim();
    if text.starts_with('「') && text.ends_with('」') {
        if let Some(idx) = text.find('」') {
            return text[1..idx].to_string();
        }
    }
    text = text.trim();
    let re = figure_alt_suffix_re();
    let normalized = re.replace_all(text, "").to_string();
    normalized.trim().trim_matches('「').trim_matches('」').to_string()
}

fn source_note_figure(note: &str) -> Option<Value> {
    let re = source_note_figure_re();
    let captures = re.captures(note)?;
    let width = captures.name("width").map(|m| parse_aozora_int(m.as_str()));
    let height = captures.name("height").map(|m| parse_aozora_int(m.as_str()));
    Some(json!({
        "kind": "figure",
        "filename": captures.name("filename")?.as_str().to_string(),
        "alt": normalize_figure_alt(captures.name("alt")?.as_str()),
        "css_class": "source-note",
        "width": width,
        "height": height,
        "caption": Value::Null,
        "x-provenance": "source-derived",
    }))
}

fn source_note_inline_caption(note: &str) -> Option<Value> {
    let re = source_note_inline_caption_re();
    let captures = re.captures(note)?;
    Some(json!({
        "kind": "caption",
        "content": [{"kind":"text","value":captures.name("caption")?.as_str()}],
        "x-provenance": "source-derived",
    }))
}

fn map_warichu(node: Node<'_, '_>, warnings: &mut Vec<Value>, summary: &mut SourceDerivedSummary) -> Value {
    let _ = warnings;
    let text = text_only(node).trim().to_string();
    let inner = if text.starts_with('（') && text.ends_with('）') && text.len() >= 2 {
        text[1..text.len() - 1].to_string()
    } else {
        text
    };
    let (upper, lower) = if let Some((u, l)) = inner.split_once('／') {
        (u.to_string(), l.to_string())
    } else if let Some((u, l)) = inner.split_once('/') {
        (u.to_string(), l.to_string())
    } else {
        (inner.clone(), String::new())
    };

    let node = json!({
        "kind": "warichu",
        "upper": if upper.is_empty() { Value::Array(Vec::new()) } else { json!([{"kind":"text","value":upper.clone()}]) },
        "lower": if lower.is_empty() { Value::Array(Vec::new()) } else { json!([{"kind":"text","value":lower.clone()}]) },
    });

    summary.push_syntax(
        "warichu.basic",
        json!({
            "kind": "warigaki",
            "value": {
                "upper_projection": upper,
                "lower_projection": lower,
            },
            "provenance": "parser",
        }),
    );

    node
}

fn map_source_note(
    node: Node<'_, '_>,
    warnings: &mut Vec<Value>,
    summary: &mut SourceDerivedSummary,
) -> Vec<Value> {
    let _ = warnings;
    let note = text_only(node);
    if let Some(figure) = source_note_figure(&note) {
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
        return vec![figure];
    }

    if let Some(caption) = source_note_inline_caption(&note) {
        summary.push_syntax(
            "caption.inline",
            json!({
                "kind": "caption",
                "value": {
                    "text": caption
                        .get("content")
                        .and_then(Value::as_array)
                        .map_or_else(String::new, |content| inline_visible_text(content)),
                },
                "provenance": "source-derived",
            }),
        );
        return vec![caption];
    }

    vec![json!({
        "kind": "style",
        "style_type": "notes",
        "content": [{"kind": "text", "value": note}],
    })]
}

fn map_caption_span(node: Node<'_, '_>, warnings: &mut Vec<Value>, summary: &mut SourceDerivedSummary) -> Value {
    let _ = warnings;
    let content = walk_inline_children(node, warnings, summary, None);
    summary.push_syntax(
        "caption.inline",
        json!({
            "kind": "caption",
            "value": {"text": inline_visible_text(&content)},
            "provenance": "parser",
        }),
    );
    json!({
        "kind": "caption",
        "content": content,
    })
}

fn map_sub_kaeriten(node: Node<'_, '_>, summary: &mut SourceDerivedSummary) -> Value {
    let marker = text_only(node).trim().to_string();
    summary.push_syntax(
        "kunten.kaeriten",
        json!({
            "kind": "style",
            "value": {"marker": marker.clone()},
            "provenance": "parser",
        }),
    );
    json!({
        "kind": "style",
        "style_type": "kaeriten",
        "content": [],
        "x-marker": marker,
        "x-provenance": "parser",
    })
}

fn map_span_gaiji(
    node: Node<'_, '_>,
    _warnings: &mut Vec<Value>,
    summary: &mut SourceDerivedSummary,
    ruby_reading: Option<&str>,
) -> Value {
    let cls = node_class(node);
    let text = text_only(node);
    let (resolved, unresolved_reason) = if text.is_empty() {
        (Value::Null, Value::String("unresolved_in_span".to_string()))
    } else {
        (Value::String(text.clone()), Value::Null)
    };

    summary.push_syntax(
        "gaiji.marker",
        json!({
            "kind": "gaiji",
            "value": {
                "source": "",
                "description": cls,
                "description_format": Value::Null,
                "kind": if resolved.is_null() { "Unknown" } else { "UnicodeCodepoint" },
                "resolved": if resolved.is_null() { Value::Null } else { resolved.clone() },
                "ruby_reading": ruby_reading,
            },
            "provenance": "parser",
        }),
    );

    json!({
        "kind": "gaiji",
        "description": cls,
        "resolved": resolved,
        "jis_code": Value::Null,
        "unresolved_reason": unresolved_reason,
    })
}

fn map_img_gaiji(
    node: Node<'_, '_>,
    _warnings: &mut Vec<Value>,
    summary: &mut SourceDerivedSummary,
    _ruby_reading: Option<&str>,
) -> Value {
    let alt = node.attribute("alt").unwrap_or("");
    let src = node.attribute("src").unwrap_or("");
    let is_gaiji = src.contains("gaiji");
    if is_gaiji {
        let description = if alt.is_empty() { "unknown" } else { alt };
        summary.push_syntax(
            "gaiji.marker",
            json!({
                "kind": "gaiji",
                "value": {
                    "source": "",
                    "description": description,
                    "description_format": Value::Null,
                    "kind": "Image",
                    "resolved": Value::Null,
                    "ruby_reading": Value::Null,
                },
                "provenance": "parser",
            }),
        );
        return json!({
            "kind": "gaiji",
            "description": description,
            "resolved": Value::Null,
            "jis_code": Value::Null,
            "unresolved_reason": "image_fallback",
        });
    }

    let filename = src
        .rsplit('/')
        .next()
        .filter(|it| !it.is_empty())
        .unwrap_or(src);
    let figure = json!({
        "kind": "figure",
        "filename": filename,
        "alt": normalize_figure_alt(alt),
        "css_class": node_class(node),
        "width": node.attribute("width").and_then(|value| normalize_optional_int(value)),
        "height": node.attribute("height").and_then(|value| normalize_optional_int(value)),
        "caption": Value::Null,
    });
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
            "provenance": "parser",
        }),
    );
    figure
}

fn map_ruby(
    node: Node<'_, '_>,
    warnings: &mut Vec<Value>,
    summary: &mut SourceDerivedSummary,
    _ruby_reading: Option<&str>,
) -> Value {
    let mut rb_nodes = Vec::new();
    let mut rt_nodes = Vec::new();
    for child in node.children().filter(|child| child.is_element()) {
        match node_name(child) {
            "rb" => rb_nodes.push(child),
            "rt" => rt_nodes.push(child),
            _ => {}
        }
    }

    let mut reading = String::new();
    let mut placement = "right";
    if let Some(rt) = rt_nodes.first() {
        reading = text_only(*rt);
        if node_class(*rt).split_whitespace().any(|token| token == "left") {
            placement = "left";
        }
    }

    let mut base_text_parts = Vec::new();
    let mut base_inline = Vec::new();
    let mut has_gaiji_base = false;
    let mut has_structured_base = false;

    if let Some(rb) = rb_nodes.first().copied() {
        if let Some(text) = rb.text() {
            let base_text = parser_text(text);
            base_text_parts.push(base_text.clone());
            base_inline.push(json!({"kind":"text","value":base_text}));
        }

        for child in rb.children().filter(|child| child.is_element()) {
            let chunks = map_inline(child, warnings, summary, Some(&reading));
            for chunk in &chunks {
                if chunk.get("kind").and_then(Value::as_str) == Some("gaiji") {
                    has_gaiji_base = true;
                    has_structured_base = true;
                }
                if chunk.get("kind").and_then(Value::as_str) == Some("style")
                    && chunk.get("style_type").and_then(Value::as_str) == Some("kaeriten")
                {
                    has_structured_base = true;
                }
                let projection = inline_visible_text(std::slice::from_ref(chunk));
                if !projection.is_empty() {
                    base_text_parts.push(projection);
                }
            }
            base_inline.extend(chunks);
            if let Some(tail) = child.tail() {
                let text = parser_text(tail);
                if !text.is_empty() {
                    base_text_parts.push(text.clone());
                    base_inline.push(json!({"kind":"text","value":text}));
                }
            }
        }
    } else if let Some(text) = node.text() {
        let value = parser_text(text);
        if !value.is_empty() {
            base_text_parts.push(value.clone());
            base_inline.push(json!({"kind":"text","value":value}));
        }
    }

    let base_projection = base_text_parts.concat();
    summary.push_syntax(
        "ruby.basic",
        json!({
            "kind": "ruby",
            "value": {
                "base_projection": base_projection.clone(),
                "reading": reading,
                "placement": placement,
            },
            "provenance": "parser",
        }),
    );

    if has_gaiji_base {
        let mut proj = String::new();
        for node in &base_inline {
            match node.get("kind").and_then(Value::as_str) {
                Some("text") => proj.push_str(node.get("value").and_then(Value::as_str).unwrap_or_default()),
                Some("gaiji") => {
                    if let Some(resolved) = node.get("resolved").and_then(Value::as_str) {
                        proj.push_str(resolved);
                    }
                }
                _ => {}
            }
        }
        summary.push_syntax(
            "gaiji_ruby.inline_base",
            json!({
                "kind": "gaiji_ruby",
                "value": {
                    "base_projection": proj,
                    "reading": reading,
                    "placement": placement,
                },
                "provenance": "parser",
            }),
        );
    }

    let mut out = json!({
        "kind": "ruby",
        "base": base_projection,
        "reading": reading,
        "direction": placement,
    });
    if has_structured_base {
        if let Some(obj) = out.as_object_mut() {
            obj.insert("base_content".to_string(), Value::Array(base_inline));
        }
    }
    out
}

fn map_inline<'a>(
    node: Node<'a, 'a>,
    warnings: &mut Vec<Value>,
    summary: &mut SourceDerivedSummary,
    ruby_reading: Option<&str>,
) -> Vec<AtBlock> {
    if node.is_text() {
        if let Some(value) = node.text() {
            let value = parser_text(value);
            if value.is_empty() {
                return Vec::new();
            }
            return vec![json!({"kind":"text","value":value})];
        }
        return Vec::new();
    }

    if !node.is_element() {
        return Vec::new();
    }

    let name = node_name(node);
    let class = node_class(node);

    if name == "ruby" {
        return vec![map_ruby(node, warnings, summary, ruby_reading)];
    }

    if name == "img" {
        return vec![map_img_gaiji(node, warnings, summary, ruby_reading)];
    }

    if name == "sub" {
        return vec![map_sub_kaeriten(node, summary)];
    }

    if name == "span" && class.starts_with("gaiji") {
        return vec![map_span_gaiji(node, warnings, summary, ruby_reading)];
    }

    if (name == "div" || name == "em" || name == "span") && !class.is_empty() {
        let children = walk_inline_children(node, warnings, summary, ruby_reading);
        if let Some(decoration) = source_derived_decoration_node(class, children.clone(), summary) {
            return vec![decoration];
        }
        if name == "em" {
            return vec![json!({
                "kind": "style",
                "style_type": class,
                "content": children,
            })];
        }
        if name == "span" && class.split_whitespace().any(|token| token == "notes") {
            return map_source_note(node, warnings, summary);
        }
        if name == "span" && class.split_whitespace().any(|token| token == "caption") {
            return vec![map_caption_span(node, warnings, summary)];
        }
        if name == "span" && class.split_whitespace().any(|token| token == "warichu") {
            return vec![map_warichu(node, warnings, summary)];
        }
        if name == "span" {
            return vec![json!({
                "kind": "style",
                "style_type": class,
                "content": children,
            })];
        }
    }

    if name == "em" {
        return vec![json!({
            "kind": "style",
            "style_type": if class.is_empty() { "em" } else { class },
            "content": walk_inline_children(node, warnings, summary, ruby_reading),
        })];
    }

    if name == "span" && class.split_whitespace().any(|token| token == "warichu") {
        return vec![map_warichu(node, warnings, summary)];
    }
    if name == "span" && class.split_whitespace().any(|token| token == "notes") {
        return map_source_note(node, warnings, summary);
    }
    if name == "span" && class.split_whitespace().any(|token| token == "caption") {
        return vec![map_caption_span(node, warnings, summary)];
    }
    if name == "br" {
        return vec![json!({"kind":"raw","source":"<br/>"})];
    }
    if name == "hr" {
        return vec![json!({"kind":"raw","source":"<hr/>"})];
    }
    if name == "span" {
        return walk_inline_children(node, warnings, summary, ruby_reading);
    }
    if name == "a" {
        return walk_inline_children(node, warnings, summary, ruby_reading);
    }

    warnings.push(json!({
        "message": format!("unmapped XHTML element <{name}>"),
        "path": format!("/blocks/.../{name}"),
    }));

    let children = walk_inline_children(node, warnings, summary, ruby_reading);
    let content = if children.is_empty() {
        vec![json!({"kind":"text","value":text_only(node)})]
    } else {
        children
    };
    vec![json!({
        "kind": "style",
        "style_type": format!("unmapped-{name}"),
        "content": content,
        "x-aozora2html-unmapped": name,
    })]
}

fn map_block(
    node: Node<'_, '_>,
    warnings: &mut Vec<Value>,
    summary: &mut SourceDerivedSummary,
) -> Vec<AtBlock> {
    let name = node_name(node);
    let class = node_class(node);

    if matches!(name, "h1" | "h2" | "h3") {
        let level = name[1..].parse::<i64>().unwrap_or(1);
        return vec![json!({
            "kind": "heading",
            "level": level,
            "style": if class.is_empty() { "normal" } else { class },
            "content": walk_inline_children(node, warnings, summary, None),
        })];
    }

    if name == "div" {
        if let Some(captures) = map_jisage_re().captures(class) {
            let indent = captures
                .name("indent")
                .and_then(|it| it.as_str().parse::<i64>().ok())
                .unwrap_or(0);
            return vec![json!({
                "kind": "jisage_block",
                "children": map_from_container(node, warnings, summary),
                "x-indent": indent,
            })];
        }
        if let Some(kind) = midashi_kind_from_class(class) {
            let level = match kind {
                "o" => 1,
                "naka" => 2,
                _ => 3,
            };
            return vec![json!({
                "kind": "heading",
                "level": level,
                "style": heading_style_from_class(class),
                "content": walk_inline_children(node, warnings, summary, None),
            })];
        }
        return Vec::new();
    }

    if name == "p" {
        return vec![json!({
            "kind": "paragraph",
            "content": walk_inline_children(node, warnings, summary, None),
        })];
    }
    if name == "br" {
        return Vec::new();
    }

    let content = walk_inline_children(node, warnings, summary, None);
    if content.is_empty() {
        return Vec::new();
    }
    warnings.push(json!({
        "message": format!("unmapped block element <{name}>"),
        "path": format!("/blocks/.../{name}"),
    }));
    vec![json!({
        "kind": "paragraph",
        "content": content,
    })]
}

fn map_from_container(
    container: Node<'_, '_>,
    warnings: &mut Vec<Value>,
    summary: &mut SourceDerivedSummary,
) -> Vec<AtBlock> {
    let mut blocks = Vec::new();
    let mut current: Vec<AtBlock> = Vec::new();

    let flush = |blocks: &mut Vec<Value>, current: &mut Vec<AtBlock>| {
        if !current.is_empty() {
            blocks.push(json!({"kind":"paragraph","content":current.clone()}));
            current.clear();
        }
    };

    if let Some(text) = container.text() {
        let value = parser_text(text);
        if !value.is_empty() {
            current.push(json!({"kind":"text","value":value}));
        }
    }

    for child in container.children().filter(|node| node.is_element()) {
        let name = node_name(child);
        let class = node_class(child);
        let is_block = matches!(name, "h1" | "h2" | "h3" | "p")
            || (name == "div" && (map_jisage_re().is_match(class) || midashi_kind_from_class(class).is_some()));
        if is_block {
            flush(&mut blocks, &mut current);
            blocks.extend(map_block(child, warnings, summary));
        } else if name == "br" {
            flush(&mut blocks, &mut current);
        } else {
            current.extend(map_inline(child, warnings, summary, None));
        }

        if let Some(tail) = child.tail() {
            let value = parser_text(tail);
            if !value.is_empty() {
                current.push(json!({"kind":"text","value":value}));
            }
        }
    }

    flush(&mut blocks, &mut current);
    blocks
        .into_iter()
        .filter(|block| {
            if block.get("kind").and_then(Value::as_str) == Some("paragraph") {
                block
                    .get("content")
                    .and_then(Value::as_array)
                    .is_some_and(|content| paragraph_has_content(content))
            } else {
                true
            }
        })
        .collect()
}

fn find_main_text<'a>(root: Node<'a, 'a>) -> Option<Node<'a, 'a>> {
    root.descendants().find(|node| {
        node.is_element()
            && node.tag_name().name() == "div"
            && node_class(*node).split_whitespace().any(|token| token == "main_text")
    })
}

fn midashi_kind_from_class(class: &str) -> Option<&str> {
    for token in class.split_whitespace() {
        if token == "o-midashi" {
            return Some("o");
        }
        if token == "naka-midashi" {
            return Some("naka");
        }
        if token == "ko-midashi" {
            return Some("ko");
        }
    }
    None
}

fn heading_style_from_class(class: &str) -> &'static str {
    let tokens = class.split_whitespace().collect::<Vec<_>>();
    if tokens.iter().any(|token| *token == "mado" || token.starts_with("mado-")) {
        "mado"
    } else if tokens.iter().any(|token| *token == "dogyo" || token.starts_with("dogyo-")) {
        "dogyo"
    } else {
        "normal"
    }
}

fn map_blocks_from_main(
    main: Node<'_, '_>,
    warnings: &mut Vec<Value>,
    summary: &mut SourceDerivedSummary,
) -> Vec<AtBlock> {
    map_from_container(main, warnings, summary)
}

fn map_blocks_from_container(
    container: Node<'_, '_>,
    warnings: &mut Vec<Value>,
    summary: &mut SourceDerivedSummary,
) -> Vec<AtBlock> {
    map_from_container(container, warnings, summary)
}

fn map_jisage_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"^jisage_(?P<indent>\d+)$").unwrap())
}

fn font_size_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"^(?P<kind>dai|sho)(?P<level>[0-9]+)$").unwrap())
}

fn source_note_figure_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r"^［＃(?P<alt>.+?)（(?P<filename>[^、）]+\.(?:png|jpe?g|gif))、横(?P<width>[０-９0-9]+)×縦(?P<height>[０-９0-9]+)）入る］$")
            .unwrap()
    })
}

fn source_note_inline_caption_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"^［＃「(?P<caption>.+?)」のキャプション］$").unwrap())
}

fn figure_alt_suffix_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"のキャプション付きの図$").unwrap())
}

pub fn map_blocks_from_xhtml_bytes(
    xhtml: &[u8],
    warnings: &mut Vec<Value>,
    summary: &mut SourceDerivedSummary,
) -> Result<(Vec<AtBlock>, bool), MappingError> {
    let xml = std::str::from_utf8(xhtml)
        .map_err(|err| MappingError::parse_error(format!("invalid XHTML bytes: {err}"), Vec::new(), false))?;
    let doc = Document::parse(xml)
        .map_err(|err| MappingError::parse_error(format!("invalid XHTML: {err}"), Vec::new(), false))?;
    let root = doc.root_element();

    if let Some(main) = find_main_text(root) {
        return Ok((map_blocks_from_main(main, warnings, summary), true));
    }

    warnings.push(json!({"message":"no <div class=\"main_text\"> found"}));
    let body = root
        .descendants()
        .find(|node| node.is_element() && node.tag_name().name() == "body")
        .unwrap_or(root);
    let blocks = map_blocks_from_container(body, warnings, summary);
    Ok((blocks, false))
}
