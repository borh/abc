//! Source-derived recovery pass.
//!
//! Operates on the AAT `blocks` produced by `xhtml_mapper` together with the
//! raw work `source_text`. Recovery reconstructs information that AozoraEpub3
//! either renders ambiguously, drops, or emits malformed HTML for, by reading
//! the original Aozora markup markers.
//!
//! The two implemented passes — gaiji and figure recovery — emit nodes tagged
//! `x-provenance = "source-derived"`. The warichu pass is intentionally a
//! no-op: AozoraEpub3 1.3.4-jdk21 emits malformed HTML for warichu blocks

//! schema's `warigaki` kind requires upper/lower rows that cannot be reliably
//! reconstructed from rendered split spans here. Recovery is left as a
//! documented gap rather than emitting schema-invalid nodes.

use std::sync::LazyLock;

use regex::Regex;
use serde_json::Value;

static GAIJI_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"※［＃(?P<desc>[^］]+)、U\+(?P<code>[0-9A-Fa-f]{4,6})］").unwrap()
});
static FIGURE_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(
        r"挿絵（(?P<filename>[^、]+)、横(?P<width>[０-９0-9]+)×縦(?P<height>[０-９0-9]+)）入る",
    )
    .unwrap()
});

/// Run all recovery passes over `blocks` using `source_text`.
pub fn apply_source_derived_recovery(blocks: &mut Vec<Value>, source_text: &str) {
    recover_gaiji(blocks, source_text);
    recover_figures(blocks, source_text);
    recover_warichu(blocks, source_text);
}

fn recover_gaiji(blocks: &mut [Value], source_text: &str) {
    // Matches markers like `※［＃「口＋世」、U+546D］`. Captures the inner
    // description and the resolved code point `U+XXXX`.
    for caps in GAIJI_RE.captures_iter(source_text) {
        let desc = caps
            .name("desc")
            .map(|m| m.as_str())
            .unwrap_or("")
            .to_string();
        let Some(code_u) = caps
            .name("code")
            .and_then(|m| u32::from_str_radix(m.as_str(), 16).ok())
            .and_then(char::from_u32)
        else {
            continue;
        };
        let resolved = code_u.to_string();
        let jis_code = caps
            .name("code")
            .map(|m| format!("U+{}", m.as_str().to_ascii_uppercase()));
        let gaiji = serde_json::json!({
            "kind": "gaiji",
            "description": desc,
            "resolved": resolved,
            "jis_code": jis_code,
            "unresolved_reason": Value::Null,
            "x-provenance": "source-derived",
        });
        // Swap the first lone text node whose value equals the resolved char.
        swap_first_matching_text(blocks, &resolved, gaiji);
    }
}

fn swap_first_matching_text(blocks: &mut [Value], needle: &str, replacement: Value) {
    for block in blocks.iter_mut() {
        let kind = block.get("kind").and_then(Value::as_str).unwrap_or("");
        if (kind == "paragraph" || kind == "heading")
            && let Some(content) = block.get_mut("content").and_then(Value::as_array_mut)
            && content.iter_mut().any(|n| {
                if n.get("kind").and_then(Value::as_str) == Some("text")
                    && n.get("value").and_then(Value::as_str) == Some(needle)
                {
                    *n = replacement.clone();
                    true
                } else {
                    false
                }
            })
        {
            return;
        }
    }
}

fn recover_figures(blocks: &mut Vec<Value>, source_text: &str) {
    // Note: takes &mut Vec (not &[Value]) because the figure-insert path may push a new paragraph onto blocks.
    // Matches `挿絵（filename.png、横123×縦456）入る` style markers.
    for caps in FIGURE_RE.captures_iter(source_text) {
        let filename = caps.name("filename").map(|m| m.as_str()).unwrap_or("");
        let width = caps.name("width").map(|m| parse_aozora_int(m.as_str()));
        let height = caps.name("height").map(|m| parse_aozora_int(m.as_str()));
        let width = width.unwrap_or(0);
        let height = height.unwrap_or(0);
        enrich_or_insert_figure(blocks, filename, width, height);
    }
}

fn enrich_or_insert_figure(blocks: &mut Vec<Value>, filename: &str, width: i64, height: i64) {
    // Walk all inline content arrays looking for a matching figure node.
    let mut found = false;
    for block in blocks.iter_mut() {
        if let Some(content) = block.get_mut("content").and_then(Value::as_array_mut)
            && enrich_figure_in_arr(content, filename, width, height)
        {
            found = true;
        }
    }
    if !found {
        // Insert a figure node carrying source-derived provenance.
        let fig = serde_json::json!({
            "kind": "figure",
            "filename": filename,
            "alt": "",
            "css_class": "",
            "caption": Value::Null,
            "width": width,
            "height": height,
            "x-provenance": "source-derived",
        });
        blocks.push(serde_json::json!({
            "kind": "paragraph",
            "content": [fig],
            "x-provenance": "source-derived",
        }));
    }
}

fn enrich_figure_in_arr(content: &mut [Value], filename: &str, width: i64, height: i64) -> bool {
    let mut found = false;
    for node in content.iter_mut() {
        if node.get("kind").and_then(Value::as_str) == Some("figure") {
            let existing = node
                .get("filename")
                .and_then(Value::as_str)
                .unwrap_or_default();
            let basename = existing.rsplit('/').next().unwrap_or(existing);
            if basename == filename
                && let Some(obj) = node.as_object_mut()
            {
                if !obj.contains_key("width") {
                    obj.insert("width".to_string(), Value::from(width));
                }
                if !obj.contains_key("height") {
                    obj.insert("height".to_string(), Value::from(height));
                }
                obj.insert(
                    "x-provenance".to_string(),
                    Value::String("source-derived".to_string()),
                );
                found = true;
            }
        } else if let Some(children) = node.get_mut("children").and_then(Value::as_array_mut)
            && enrich_figure_in_arr(children, filename, width, height)
        {
            found = true;
        }
    }
    found
}

fn parse_aozora_int(value: &str) -> i64 {
    value
        .chars()
        .map(|c| match c {
            '０'..='９' => char::from_u32(c as u32 - '０' as u32 + '0' as u32).unwrap_or(c),
            other => other,
        })
        .collect::<String>()
        .parse::<i64>()
        .unwrap_or(0)
}

fn recover_warichu(_blocks: &mut Vec<Value>, _source_text: &str) {
    // Intentionally a no-op (see module docs). AozoraEpub3 1.3.4-jdk21 emits
    // malformed HTML for warichu and reports `[ERROR] 割り注終わりなし`; the
    // schema's `warigaki` kind requires `upper`/`lower` rows that cannot be
    // reconstructed from rendered split spans here, and we refuse to emit
    // schema-invalid nodes instead.
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn recovers_inline_gaiji_from_text_node() {
        let source = "本文中に※［＃「口＋世」、U+546D］が登場する。";
        let mut blocks = vec![json!({
            "kind": "paragraph",
            "content": [
                {"kind": "text", "value": "本文中に"},
                {"kind": "text", "value": "\u{546D}"},
                {"kind": "text", "value": "が登場する。"}
            ],
        })];
        apply_source_derived_recovery(&mut blocks, source);
        let content = &blocks[0]["content"];
        let gaiji = content
            .as_array()
            .unwrap()
            .iter()
            .find(|n| n.get("kind").and_then(Value::as_str) == Some("gaiji"))
            .expect("gaiji node recovered");
        assert_eq!(gaiji["resolved"], "\u{546D}");
        assert_eq!(gaiji["x-provenance"], "source-derived");
        assert_eq!(gaiji["description"], "「口＋世」");
    }

    #[test]
    fn no_op_warichu_preserves_blocks() {
        let source = "［＃ここから割り注］上／下［＃ここで割り注終わり］";
        let mut blocks = vec![json!({"kind": "paragraph", "content": [
            {"kind": "text", "value": source}
        ]})];
        let before = blocks.clone();
        apply_source_derived_recovery(&mut blocks, source);
        assert_eq!(blocks, before, "warichu recovery must be a no-op");
    }
}
