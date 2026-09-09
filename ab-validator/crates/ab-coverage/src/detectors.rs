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

use std::collections::BTreeMap;

use regex::Regex;
use serde_json::Value;

use crate::matrix::Row;

#[derive(Debug)]
pub struct DetectorRegistry {
    detectors: BTreeMap<String, Detector>,
}

impl DetectorRegistry {
    #[must_use]
    pub fn from_matrix(rows: &[Row]) -> Self {
        let mut detectors = BTreeMap::new();
        for row in rows {
            let detector = build_detector_for_row(row);
            detectors.insert(row.id.clone(), detector);
        }
        Self { detectors }
    }

    #[must_use]
    pub fn detect(&self, row_id: &str, ctx: &DetectorContext<'_>) -> u64 {
        match self.detectors.get(row_id) {
            Some(d) => d.run(ctx),
            None => 0,
        }
    }

    pub fn rows(&self) -> impl Iterator<Item = &str> {
        self.detectors.keys().map(|s| s.as_str())
    }

    #[must_use]
    pub fn detect_all<'a>(&'a self, ctx: &DetectorContext<'_>) -> BTreeMap<&'a str, u64> {
        let mut counts = vec![0u64; self.detectors.len()];

        walk(ctx.aat, &mut |node| {
            for (count, detector) in counts.iter_mut().zip(self.detectors.values()) {
                for rule in &detector.rules {
                    let matched = match rule {
                        Rule::AatKindCount(kinds) => node
                            .get("kind")
                            .and_then(Value::as_str)
                            .is_some_and(|kind| kinds.iter().any(|candidate| candidate == kind)),
                        Rule::AatNode(predicate) => predicate(node),
                        Rule::SourceRegex(_) | Rule::WholeAat(_) => false,
                    };
                    if matched {
                        *count += 1;
                    }
                }
            }
        });

        for (count, detector) in counts.iter_mut().zip(self.detectors.values()) {
            for rule in &detector.rules {
                *count += match rule {
                    Rule::SourceRegex(regex) => regex.find_iter(ctx.source).count() as u64,
                    Rule::WholeAat(detect) => detect(ctx.aat),
                    Rule::AatKindCount(_) | Rule::AatNode(_) => 0,
                };
            }
        }
        self.detectors
            .keys()
            .map(String::as_str)
            .zip(counts)
            .collect()
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
    AatNode(NodePredicate),
    WholeAat(WholeAatFn),
}

type NodePredicate = fn(&Value) -> bool;
type WholeAatFn = fn(&Value) -> u64;

impl Detector {
    fn run(&self, ctx: &DetectorContext<'_>) -> u64 {
        let mut total = 0u64;
        for rule in &self.rules {
            total += match rule {
                Rule::AatKindCount(kinds) => count_aat_kinds(ctx.aat, kinds),
                Rule::SourceRegex(re) => re.find_iter(ctx.source).count() as u64,
                Rule::AatNode(predicate) => count_with(ctx.aat, *predicate),
                Rule::WholeAat(detect) => detect(ctx.aat),
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
        && let Some(rule) = lookup_named_detector(detector_id)
    {
        rules.push(rule);
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

fn lookup_named_detector(id: &str) -> Option<Rule> {
    let node = |predicate| Rule::AatNode(predicate);
    Some(match id {
        // ---- gaiji.* group ----
        "gaiji_marker" => node(d_gaiji_any),
        "gaiji_unicode_codepoint" => node(d_gaiji_unicode_codepoint),
        "gaiji_jis_code" => node(d_gaiji_with_jis_code),
        "gaiji_dakuten_katakana" => node(d_gaiji_dakuten_katakana),
        "gaiji_un_embed" => node(d_gaiji_un_embed),
        "iteration_kunoji" => node(d_source_only),
        "accent_diacritic" => node(d_accent_kind),
        "figure_image_inline" => node(d_figure_image),
        "figure_insertion_declaration" => node(d_figure_insertion_declaration),

        // ---- ruby.* + annotation.* + kunten.okurigana group ----
        "ruby_basic" => node(d_ruby_any),
        "ruby_double" => node(d_source_only),
        "ruby_placement_directional" => node(d_ruby_directional),
        "annotation_chuuki" => node(d_source_only),
        "annotation_bouki" => node(d_source_only),
        "kunten_okurigana" => node(d_source_only),

        // ---- heading.* group ----
        "heading_basic" => node(d_heading_any),
        "heading_inline_form" => node(d_source_only),
        "heading_dogyo" => node(d_source_only),
        "heading_mado" => node(d_source_only),

        // ---- caption.* group ----
        "caption_inline" => node(d_caption_inline),
        "caption_block" => node(d_caption_block),

        // ---- style group: decoration / indentation / layout / etc. ----
        "decoration_boten" => node(d_style_boten),
        "decoration_bousen" => node(d_style_bousen),
        "decoration_bold_italic" => node(d_style_bold_italic),
        "decoration_font_size" => node(d_font_size),
        "decoration_keigakomi" => node(d_keigakomi),
        "decoration_direction_override" => node(d_source_only),
        "indentation_basic" => node(d_source_only),
        "indentation_jisage_block" => node(d_jisage_block),
        "indentation_jisage_oneline" => node(d_source_only),
        "indentation_chitsuki" => node(d_source_only),
        "indentation_jizume" => node(d_source_only),
        "indentation_burasage" => node(d_source_only),
        "layout_yokogumi" => node(d_yokogumi),
        "layout_tcy" => node(d_tcy),
        "warigaki_parenthetical" => node(d_warigaki),
        "warichu_basic" => node(d_warichu),
        "kunten_kaeriten" => node(d_source_only),
        "reference_frontref" => node(d_source_only),
        "emphasis_basic" => node(d_emphasis_any),

        // ---- text-anchor group: source-only ----
        "break_page_line" | "break_line_explicit" | "editor_note_unmapped" => node(d_source_only),

        // ---- composite ad-hoc rows ----
        "gaiji_ruby_inline_base" => Rule::WholeAat(d_gaiji_ruby_inline_base),
        "figure_image_caption" => Rule::WholeAat(d_figure_image_caption),
        "ruby_nested_forbidden" => node(d_source_only),

        _ => return None,
    })
}

fn d_source_only(_node: &Value) -> bool {
    false
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

fn d_gaiji_any(node: &Value) -> bool {
    node.get("kind").and_then(Value::as_str) == Some("gaiji")
}

fn d_gaiji_unicode_codepoint(n: &Value) -> bool {
    // adapter encodes either via `value` (a single char) or by U+ in description.
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
}

fn d_gaiji_with_jis_code(n: &Value) -> bool {
    if n.get("kind").and_then(Value::as_str) != Some("gaiji") {
        return false;
    }
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
}

fn d_gaiji_dakuten_katakana(n: &Value) -> bool {
    n.get("kind").and_then(Value::as_str) == Some("gaiji")
        && n.get("description")
            .and_then(Value::as_str)
            .is_some_and(|d| {
                d.contains("濁点")
                    || d.contains("半濁点")
                    || d.contains("小書き片仮名")
                    || (d.contains("1-07-8") || d.contains("1-7-8"))
            })
}

fn d_gaiji_un_embed(n: &Value) -> bool {
    // unresolved gaiji marker: kind=gaiji and resolved is empty/null AND no jis code.
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
}

fn d_accent_kind(node: &Value) -> bool {
    node.get("kind").and_then(Value::as_str) == Some("accent")
}

fn d_figure_insertion_declaration(n: &Value) -> bool {
    // A figure the source places without supplying it: an editorial note, not a
    // figure node, because there is no image to carry.
    n.get("kind").and_then(Value::as_str) == Some("editorial_note")
        && n.get("note_kind").and_then(Value::as_str) == Some("figure-insertion")
}

fn d_figure_image(n: &Value) -> bool {
    // Image kind in aozora2html output uses capitalized "Image"; aozora2 emits
    // a `figure` kind. Match either.
    matches!(
        n.get("kind").and_then(Value::as_str),
        Some("Image" | "figure")
    )
}

// ---- ruby ----

fn d_ruby_any(node: &Value) -> bool {
    node.get("kind").and_then(Value::as_str) == Some("ruby")
}

fn d_ruby_directional(n: &Value) -> bool {
    n.get("kind").and_then(Value::as_str) == Some("ruby")
        && matches!(
            n.get("direction").and_then(Value::as_str),
            Some("left" | "below")
        )
}

// ---- heading ----

fn d_heading_any(node: &Value) -> bool {
    node.get("kind").and_then(Value::as_str) == Some("heading")
}

// ---- caption ----

fn d_caption_inline(n: &Value) -> bool {
    n.get("kind").and_then(Value::as_str) == Some("caption")
        || (n.get("kind").and_then(Value::as_str) == Some("style")
            && style_type_of(n) == Some("caption"))
}

fn d_caption_block(node: &Value) -> bool {
    node.get("kind").and_then(Value::as_str) == Some("caption_block")
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

fn is_style_with(node: &Value, types: &[&str]) -> bool {
    node.get("kind").and_then(Value::as_str) == Some("style")
        && style_type_of(node).is_some_and(|style| types.contains(&style))
}

fn d_style_boten(node: &Value) -> bool {
    is_style_with(node, BOTEN_TYPES)
}

fn d_style_bousen(node: &Value) -> bool {
    is_style_with(node, BOUSEN_TYPES)
}

fn d_style_bold_italic(node: &Value) -> bool {
    is_style_with(node, BOLD_ITALIC_TYPES)
}

fn d_font_size(n: &Value) -> bool {
    // Either a dedicated kind=font_size container, or a style with size markers.
    let kind = n.get("kind").and_then(Value::as_str);
    if kind == Some("font_size") {
        return true;
    }
    kind == Some("style")
        && style_type_of(n).is_some_and(|style| {
            style.contains("smaller")
                || style.contains("larger")
                || style.starts_with("size_")
                || style.starts_with("sho")
                || style.starts_with("dai")
        })
}

fn d_keigakomi(n: &Value) -> bool {
    let kind = n.get("kind").and_then(Value::as_str);
    (kind == Some("keigakomi") && n.get("content").is_some())
        || (kind == Some("typography_block") && n["formatting"]["kind"] == "keigakomi")
        || (kind == Some("style") && style_type_of(n) == Some("keigakomi"))
}

fn d_jisage_block(node: &Value) -> bool {
    node.get("kind").and_then(Value::as_str) == Some("jisage_block")
        || (node["kind"] == "layout_block" && node.get("indent").is_some())
}

fn d_yokogumi(n: &Value) -> bool {
    let kind = n.get("kind").and_then(Value::as_str);
    (kind == Some("yokogumi") && n.get("content").is_some())
        || (kind == Some("typography_block") && n["formatting"]["kind"] == "yokogumi")
        || (kind == Some("style") && style_type_of(n) == Some("yokogumi"))
}

fn d_tcy(n: &Value) -> bool {
    let kind = n.get("kind").and_then(Value::as_str);
    kind == Some("tcy") || (kind == Some("style") && style_type_of(n) == Some("tcy"))
}

fn d_warigaki(node: &Value) -> bool {
    node.get("kind").and_then(Value::as_str) == Some("warigaki")
}

fn d_warichu(n: &Value) -> bool {
    let kind = n.get("kind").and_then(Value::as_str);
    kind == Some("warichu") || (kind == Some("style") && style_type_of(n) == Some("warichu"))
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
            || (matches!(kind, Some("Image" | "figure"))
                && n.get("caption").is_some_and(|v| !v.is_null()))
    })
}

fn d_emphasis_any(n: &Value) -> bool {
    // Umbrella row: count any `style` container that carries one of the
    // recognised decoration types (boten/bousen/bold/italic/...).
    if n.get("kind").and_then(Value::as_str) != Some("style") {
        return false;
    }
    let Some(style) = style_type_of(n) else {
        return false;
    };
    BOTEN_TYPES.contains(&style)
        || BOUSEN_TYPES.contains(&style)
        || BOLD_ITALIC_TYPES.contains(&style)
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
        assert_eq!(
            count_with(&aat, d_style_boten),
            1,
            "boten counts only boten styles"
        );
        assert_eq!(
            count_with(&aat, d_style_bold_italic),
            1,
            "bold_italic counts only italic styles"
        );
        assert_eq!(count_with(&aat, d_style_bousen), 0);
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
        assert_eq!(count_with(&aat, d_ruby_directional), 1);
        assert_eq!(count_with(&aat, d_ruby_any), 2);
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
        assert_eq!(count_with(&aat, d_gaiji_with_jis_code), 2);
        assert_eq!(count_with(&aat, d_gaiji_any), 3);
    }

    #[test]
    fn keigakomi_matches_block_and_style_forms() {
        let aat = json!({
            "kind": "paragraph",
            "content": [
                {"kind": "typography_block", "formatting": {"kind": "keigakomi"}, "children": []},
                {"kind": "style", "style_type": "keigakomi", "content": []},
                {"kind": "style", "style_type": "boten", "content": []},
            ]
        });
        assert_eq!(count_with(&aat, d_keigakomi), 2);
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
