//! Per-row detectors. Most rows count occurrences of one or more AAT inline
//! kinds; a small minority falls back to scanning the raw source bytes.
//!
//! The default detector counts AAT nodes whose `kind` matches one of the
//! row's `aat_nodes`. A row may opt into a hand-written detector by setting
//! `corpus_prevalence.detector_id`; the registry below resolves the id to a
//! function that consults AAT attributes (`style_type`, ruby `direction`,
//! gaiji `jis_code`, container kind, …) so rows sharing an `aat_nodes`
//! signature produce different counts. Source-side regexes from
//! `source_patterns` are always added on top.

use std::collections::HashMap;

use regex::Regex;
use serde_json::Value;

use crate::matrix::Row;

#[derive(Debug)]
pub struct DetectorRegistry {
    detectors: HashMap<String, Detector>,
}

impl DetectorRegistry {
    pub fn from_matrix(rows: &[Row]) -> Self {
        let mut detectors = HashMap::new();
        for row in rows {
            let detector = build_detector_for_row(row);
            detectors.insert(row.id.clone(), detector);
        }
        Self { detectors }
    }

    pub fn detect(&self, row_id: &str, ctx: &DetectorContext<'_>) -> u64 {
        match self.detectors.get(row_id) {
            Some(d) => d.run(ctx),
            None => 0,
        }
    }

    pub fn rows(&self) -> impl Iterator<Item = &str> {
        self.detectors.keys().map(|s| s.as_str())
    }
}

pub struct DetectorContext<'a> {
    pub aat: &'a Value,
    pub source: &'a str,
}

#[derive(Debug)]
struct Detector {
    rules: Vec<Rule>,
}

#[derive(Debug)]
enum Rule {
    /// Count AAT inline / block nodes whose `kind` is in the list.
    AatKindCount(Vec<String>),
    /// Match a regex against the decoded source text.
    SourceRegex(Regex),
    /// Hand-written AAT predicate; resolved from `corpus_prevalence.detector_id`.
    Named(NamedFn),
}

type NamedFn = fn(&Value) -> u64;

impl Detector {
    fn run(&self, ctx: &DetectorContext<'_>) -> u64 {
        let mut total = 0u64;
        for rule in &self.rules {
            total += match rule {
                Rule::AatKindCount(kinds) => count_aat_kinds(ctx.aat, kinds),
                Rule::SourceRegex(re) => re.find_iter(ctx.source).count() as u64,
                Rule::Named(f) => f(ctx.aat),
            };
        }
        total
    }
}

fn build_detector_for_row(row: &Row) -> Detector {
    let mut rules = Vec::new();
    let detector_id = row
        .corpus_prevalence
        .as_ref()
        .map(|p| p.detector_id.as_str())
        .unwrap_or("");

    // Named detector takes precedence over the generic `aat_nodes` matcher.
    // Source patterns are always added on top.
    if !detector_id.is_empty()
        && let Some(f) = lookup_named_detector(detector_id)
    {
        rules.push(Rule::Named(f));
    } else {
        // Default: count any AAT node whose kind matches the row's `aat_nodes`.
        // Generic kinds like "text" / "paragraph" are dropped because they match
        // the structural backbone of every work and would dwarf real signal; for
        // those rows the source-side regex is the authoritative detector.
        let kinds = row
            .aat_nodes
            .iter()
            .map(|s| s.to_string())
            .filter(|s| !s.is_empty() && !is_generic_aat_kind(s))
            .collect::<Vec<_>>();
        if !kinds.is_empty() {
            rules.push(Rule::AatKindCount(kinds));
        }
    }

    for pat in &row.source_patterns {
        if let Ok(re) = Regex::new(pat) {
            rules.push(Rule::SourceRegex(re));
        }
    }
    Detector { rules }
}

fn is_generic_aat_kind(kind: &str) -> bool {
    matches!(kind, "text" | "paragraph")
}

fn count_aat_kinds(aat: &Value, kinds: &[String]) -> u64 {
    let mut count = 0u64;
    walk(aat, &mut |node| {
        if let Some(kind) = node.get("kind").and_then(Value::as_str)
            && kinds.iter().any(|k| k == kind)
        {
            count += 1;
        }
    });
    count
}

fn walk<F>(node: &Value, visit: &mut F)
where
    F: FnMut(&Value),
{
    match node {
        Value::Object(map) => {
            visit(node);
            for (_, child) in map {
                walk(child, visit);
            }
        }
        Value::Array(arr) => {
            for child in arr {
                walk(child, visit);
            }
        }
        _ => {}
    }
}

// ---------------------------------------------------------------------------
// Named detectors. Each one receives the raw AAT JSON and returns the count
// of nodes that match the row's intent. They are referenced by
// `corpus_prevalence.detector_id` in `data/aozora-syntax-coverage.toml`.
// Convention: detector id is the row id with `.` replaced by `_`.
// ---------------------------------------------------------------------------

fn lookup_named_detector(id: &str) -> Option<NamedFn> {
    Some(match id {
        // ---- gaiji.* group ----
        "gaiji_marker" => d_gaiji_any,
        "gaiji_unicode_codepoint" => d_gaiji_unicode_codepoint,
        "gaiji_jis_code" => d_gaiji_with_jis_code,
        "gaiji_dakuten_katakana" => d_gaiji_dakuten_katakana,
        "gaiji_un_embed" => d_gaiji_un_embed,
        "iteration_kunoji" => d_source_only,
        "accent_diacritic" => d_accent_kind,
        "figure_image_inline" => d_figure_image,

        // ---- ruby.* + annotation.* + kunten.okurigana group ----
        "ruby_basic" => d_ruby_any,
        "ruby_double" => d_source_only,
        "ruby_placement_directional" => d_ruby_directional,
        "annotation_chuuki" => d_source_only,
        "annotation_bouki" => d_source_only,
        "kunten_okurigana" => d_source_only,

        // ---- heading.* group ----
        "heading_basic" => d_heading_any,
        "heading_inline_form" => d_source_only,
        "heading_dogyo" => d_source_only,
        "heading_mado" => d_source_only,

        // ---- caption.* group ----
        "caption_inline" => d_caption_inline,
        "caption_block" => d_caption_block,

        // ---- style group: decoration / indentation / layout / etc. ----
        "decoration_boten" => d_style_boten,
        "decoration_bousen" => d_style_bousen,
        "decoration_bold_italic" => d_style_bold_italic,
        "decoration_font_size" => d_font_size,
        "decoration_keigakomi" => d_keigakomi,
        "decoration_direction_override" => d_source_only,
        "indentation_basic" => d_source_only,
        "indentation_jisage_block" => d_jisage_block,
        "indentation_jisage_oneline" => d_source_only,
        "indentation_chitsuki" => d_source_only,
        "indentation_jizume" => d_source_only,
        "indentation_burasage" => d_source_only,
        "layout_yokogumi" => d_yokogumi,
        "layout_tcy" => d_tcy,
        "warigaki_parenthetical" => d_warigaki,
        "warichu_basic" => d_warichu,
        "kunten_kaeriten" => d_source_only,
        "reference_frontref" => d_source_only,
        "emphasis_basic" => d_emphasis_any,

        // ---- text-anchor group: source-only ----
        "break_page_line" | "break_line_explicit" | "editor_note_unmapped" => d_source_only,

        // ---- composite ad-hoc rows ----
        "gaiji_ruby_inline_base" => d_gaiji_ruby_inline_base,
        "figure_image_caption" => d_figure_image_caption,
        "ruby_nested_forbidden" => d_source_only,

        _ => return None,
    })
}

fn d_source_only(_aat: &Value) -> u64 {
    0
}

fn count_kind(aat: &Value, kind: &str) -> u64 {
    let mut n = 0u64;
    walk(aat, &mut |node| {
        if node.get("kind").and_then(Value::as_str) == Some(kind) {
            n += 1;
        }
    });
    n
}

fn count_with<F: FnMut(&Value) -> bool>(aat: &Value, mut pred: F) -> u64 {
    let mut n = 0u64;
    walk(aat, &mut |node| {
        if pred(node) {
            n += 1;
        }
    });
    n
}

fn style_type_of(node: &Value) -> Option<&str> {
    node.get("style_type").and_then(Value::as_str)
}

// ---- gaiji ----

fn d_gaiji_any(aat: &Value) -> u64 {
    count_kind(aat, "gaiji")
}

fn d_gaiji_unicode_codepoint(aat: &Value) -> u64 {
    // adapter encodes either via `value` (a single char) or by U+ in description.
    count_with(aat, |n| {
        if n.get("kind").and_then(Value::as_str) != Some("gaiji") {
            return false;
        }
        if let Some(desc) = n.get("description").and_then(Value::as_str)
            && desc.contains("U+")
        {
            return true;
        }
        n.get("value")
            .and_then(Value::as_str)
            .is_some_and(|v| v.chars().next().map(|c| (c as u32) > 0x7F).unwrap_or(false))
            && n.get("jis_code").map(|j| j.is_null()).unwrap_or(true)
    })
}

fn d_gaiji_with_jis_code(aat: &Value) -> u64 {
    count_with(aat, |n| {
        if n.get("kind").and_then(Value::as_str) != Some("gaiji") {
            return false;
        }
        // Either a structured `jis_code` field, or a 1-NN-NN reference inside the description.
        if n.get("jis_code")
            .and_then(Value::as_str)
            .is_some_and(|s| !s.is_empty())
        {
            return true;
        }
        n.get("description")
            .and_then(Value::as_str)
            .is_some_and(|d| {
                d.contains("第3水準")
                    || d.contains("第4水準")
                    || d.contains("第1水準")
                    || d.contains("第2水準")
            })
    })
}

fn d_gaiji_dakuten_katakana(aat: &Value) -> u64 {
    count_with(aat, |n| {
        if n.get("kind").and_then(Value::as_str) != Some("gaiji") {
            return false;
        }
        n.get("description")
            .and_then(Value::as_str)
            .is_some_and(|d| {
                d.contains("濁点")
                    || d.contains("半濁点")
                    || d.contains("小書き片仮名")
                    || (d.contains("1-07-8") || d.contains("1-7-8"))
            })
    })
}

fn d_gaiji_un_embed(aat: &Value) -> u64 {
    // unresolved gaiji marker: kind=gaiji and resolved is empty/null AND no jis code.
    count_with(aat, |n| {
        if n.get("kind").and_then(Value::as_str) != Some("gaiji") {
            return false;
        }
        let resolved_empty = match n.get("resolved") {
            None => true,
            Some(Value::Null) => true,
            Some(Value::String(s)) => s.is_empty(),
            _ => false,
        };
        let no_jis = n
            .get("jis_code")
            .map(|j| j.is_null() || j.as_str().is_some_and(str::is_empty))
            .unwrap_or(true);
        resolved_empty && no_jis
    })
}

fn d_accent_kind(aat: &Value) -> u64 {
    count_kind(aat, "accent")
}

fn d_figure_image(aat: &Value) -> u64 {
    // Image kind in aozora2html output uses capitalized "Image"; aozora2 emits
    // a `figure` kind. Match either.
    count_with(aat, |n| {
        matches!(
            n.get("kind").and_then(Value::as_str),
            Some("Image") | Some("figure")
        )
    })
}

// ---- ruby ----

fn d_ruby_any(aat: &Value) -> u64 {
    count_kind(aat, "ruby")
}

fn d_ruby_directional(aat: &Value) -> u64 {
    count_with(aat, |n| {
        if n.get("kind").and_then(Value::as_str) != Some("ruby") {
            return false;
        }
        matches!(
            n.get("direction").and_then(Value::as_str),
            Some("left") | Some("below")
        )
    })
}

// ---- heading ----

fn d_heading_any(aat: &Value) -> u64 {
    count_kind(aat, "heading")
}

// ---- caption ----

fn d_caption_inline(aat: &Value) -> u64 {
    count_with(aat, |n| {
        n.get("kind").and_then(Value::as_str) == Some("caption")
            || (n.get("kind").and_then(Value::as_str) == Some("style")
                && style_type_of(n) == Some("caption"))
    })
}

fn d_caption_block(aat: &Value) -> u64 {
    count_kind(aat, "caption_block")
}

// ---- style group ----

const BOTEN_TYPES: &[&str] = &[
    "boten",
    "sesame_dot",
    "white_circle",
    "black_circle",
    "bullseye",
    "white_sesame_dot",
    "black_sesame_dot",
    "fisheye",
];

const BOUSEN_TYPES: &[&str] = &[
    "bousen",
    "underline_solid",
    "underline_double",
    "underline_dashed",
    "underline_wave",
    "sidedot",
];

const BOLD_ITALIC_TYPES: &[&str] = &["bold", "italic", "shatai", "futoji"];

fn count_style_with(aat: &Value, types: &[&str]) -> u64 {
    count_with(aat, |n| {
        if n.get("kind").and_then(Value::as_str) != Some("style") {
            return false;
        }
        style_type_of(n).is_some_and(|t| types.contains(&t))
    })
}

fn d_style_boten(aat: &Value) -> u64 {
    count_style_with(aat, BOTEN_TYPES)
}

fn d_style_bousen(aat: &Value) -> u64 {
    count_style_with(aat, BOUSEN_TYPES)
}

fn d_style_bold_italic(aat: &Value) -> u64 {
    count_style_with(aat, BOLD_ITALIC_TYPES)
}

fn d_font_size(aat: &Value) -> u64 {
    // Either a dedicated kind=font_size container, or a style with size markers.
    count_with(aat, |n| {
        let kind = n.get("kind").and_then(Value::as_str);
        if kind == Some("font_size") {
            return true;
        }
        if kind == Some("style")
            && let Some(t) = style_type_of(n)
        {
            return t.contains("smaller")
                || t.contains("larger")
                || t.starts_with("size_")
                || t.starts_with("sho")
                || t.starts_with("dai");
        }
        false
    })
}

fn d_keigakomi(aat: &Value) -> u64 {
    count_with(aat, |n| {
        let kind = n.get("kind").and_then(Value::as_str);
        kind == Some("keigakomi_block")
            || (kind == Some("style") && style_type_of(n) == Some("keigakomi"))
    })
}

fn d_jisage_block(aat: &Value) -> u64 {
    count_kind(aat, "jisage_block")
}

fn d_yokogumi(aat: &Value) -> u64 {
    count_with(aat, |n| {
        let kind = n.get("kind").and_then(Value::as_str);
        kind == Some("yokogumi")
            || kind == Some("yokogumi_block")
            || (kind == Some("style") && style_type_of(n) == Some("yokogumi"))
    })
}

fn d_tcy(aat: &Value) -> u64 {
    count_with(aat, |n| {
        let kind = n.get("kind").and_then(Value::as_str);
        kind == Some("tcy") || (kind == Some("style") && style_type_of(n) == Some("tcy"))
    })
}

fn d_warigaki(aat: &Value) -> u64 {
    count_kind(aat, "warigaki")
}

fn d_warichu(aat: &Value) -> u64 {
    count_with(aat, |n| {
        let kind = n.get("kind").and_then(Value::as_str);
        kind == Some("warichu") || (kind == Some("style") && style_type_of(n) == Some("warichu"))
    })
}

fn d_gaiji_ruby_inline_base(aat: &Value) -> u64 {
    // ruby whose base contains a gaiji marker. The adapter typically lifts
    // the gaiji into a sibling node, so check whether a ruby's `value` (raw
    // base text) carries the ※［＃ marker, falling back to looking for a
    // gaiji-kind sibling immediately preceding a ruby in any content array.
    let mut count = 0u64;
    walk(aat, &mut |node| {
        if let Some(arr) = node
            .get("content")
            .and_then(Value::as_array)
            .or_else(|| node.get("children").and_then(Value::as_array))
        {
            let kinds: Vec<&str> = arr
                .iter()
                .map(|n| n.get("kind").and_then(Value::as_str).unwrap_or(""))
                .collect();
            for w in kinds.windows(2) {
                if w[0] == "gaiji" && w[1] == "ruby" {
                    count += 1;
                }
            }
        }
        if node.get("kind").and_then(Value::as_str) == Some("ruby")
            && node
                .get("value")
                .and_then(Value::as_str)
                .is_some_and(|v| v.contains("※［＃"))
        {
            count += 1;
        }
    });
    count
}

fn d_figure_image_caption(aat: &Value) -> u64 {
    // image followed (or contained) by a caption. Approximated as: any
    // figure/Image node that has a caption sibling, or any caption_block.
    count_with(aat, |n| {
        let kind = n.get("kind").and_then(Value::as_str);
        kind == Some("caption_block")
            || (matches!(kind, Some("Image") | Some("figure"))
                && n.get("caption").is_some_and(|v| !v.is_null()))
    })
}

fn d_emphasis_any(aat: &Value) -> u64 {
    // Umbrella row: count any `style` container that carries one of the
    // recognised decoration types (boten/bousen/bold/italic/...).
    count_with(aat, |n| {
        if n.get("kind").and_then(Value::as_str) != Some("style") {
            return false;
        }
        let Some(t) = style_type_of(n) else {
            return false;
        };
        BOTEN_TYPES.contains(&t) || BOUSEN_TYPES.contains(&t) || BOLD_ITALIC_TYPES.contains(&t)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn ctx<'a>(aat: &'a Value, source: &'a str) -> DetectorContext<'a> {
        DetectorContext { aat, source }
    }

    #[test]
    fn shared_aat_nodes_rows_diverge_with_named_detectors() {
        // Two style nodes — one boten, one shatai (italic). Without per-row
        // detectors both `decoration.boten` and `decoration.bold_italic` would
        // match every style node identically. With named detectors, each row
        // sees only its own subset.
        let aat = json!({
            "kind": "paragraph",
            "content": [
                {"kind": "style", "style_type": "boten", "content": []},
                {"kind": "style", "style_type": "shatai", "content": []},
            ]
        });
        assert_eq!(d_style_boten(&aat), 1, "boten counts only boten styles");
        assert_eq!(
            d_style_bold_italic(&aat),
            1,
            "bold_italic counts only italic styles"
        );
        assert_eq!(d_style_bousen(&aat), 0);
    }

    #[test]
    fn ruby_directional_excludes_right_placement() {
        let aat = json!({
            "kind": "paragraph",
            "content": [
                {"kind": "ruby", "base": "山", "reading": "やま", "direction": "right"},
                {"kind": "ruby", "base": "山", "reading": "やま", "direction": "left"},
            ]
        });
        assert_eq!(d_ruby_directional(&aat), 1);
        assert_eq!(d_ruby_any(&aat), 2);
    }

    #[test]
    fn gaiji_jis_code_matches_jis_field_or_description() {
        let aat = json!({
            "kind": "paragraph",
            "content": [
                {"kind": "gaiji", "description": "「廴＋囘」、第4水準2-12-11", "jis_code": null, "resolved": null, "unresolved_reason": null},
                {"kind": "gaiji", "description": "ローマ数字1", "jis_code": "1-13-21", "resolved": null, "unresolved_reason": null},
                {"kind": "gaiji", "description": "ふつう", "jis_code": null, "resolved": null, "unresolved_reason": null},
            ]
        });
        assert_eq!(d_gaiji_with_jis_code(&aat), 2);
        assert_eq!(d_gaiji_any(&aat), 3);
    }

    #[test]
    fn keigakomi_matches_block_and_style_forms() {
        let aat = json!({
            "kind": "paragraph",
            "content": [
                {"kind": "keigakomi_block", "children": []},
                {"kind": "style", "style_type": "keigakomi", "content": []},
                {"kind": "style", "style_type": "boten", "content": []},
            ]
        });
        assert_eq!(d_keigakomi(&aat), 2);
    }

    #[test]
    fn registry_falls_back_to_aat_kinds_when_detector_id_unset() {
        // Build a minimal row with no detector_id; default rule applies.
        let row = serde_json::from_str::<crate::matrix::Row>(
            r#"{
            "id":"r1","priority":1,"category":"c","feature_keys":[],"reference_sources":[],
            "source_examples":[],"source_patterns":[],"ir_nodes":[],"aat_nodes":["gaiji"],
            "tei_projection":"x","plaintext_projection":"x","comparison_projection":"x",
            "validation_properties":[],"adapter_expectations":[],"status":"covered",
            "status_reason":"x"
        }"#,
        )
        .unwrap();
        let det = build_detector_for_row(&row);
        let aat = json!({"kind":"paragraph","content":[{"kind":"gaiji"},{"kind":"gaiji"}]});
        assert_eq!(det.run(&ctx(&aat, "")), 2);
    }
}
