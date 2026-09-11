//! Rust↔Python mirror test + property-test target for the bare-toggle
//! inline-container classifier (`pair_bare_toggles` in `src/lib.rs`,
//! applied AFTER block classification to every built content array via
//! `pair_bare_toggles_in_blocks`.
//!
//! `pair_bare_toggles` itself is `pub(crate)` — this integration test
//! reaches it only indirectly, through the crate's public
//! `ab_aat::aat_json_from_bytes` entry point (same call `goldens.rs`
//! uses), which suffices for every property here EXCEPT the two that need
//! a DIRECT pass-level call (zero-adoption structural identity,
//! determinism at the pass level) — those live in `src/lib.rs`'s
//! `mod tests` instead (`bare_toggle_zero_adoption_is_structurally_unchanged`,
//! `bare_toggle_pass_is_deterministic`), the natural home for anything
//! that must call a `pub(crate)` function directly.
//! The mirror test reads the vector file
//! `tests/fixtures/bare-toggle-model-vectors.json`. It asserts the
//! OBSERVABLE adapter outcome only — container counts and raw-node
//! survivors — not the internal per-line counters (`orphan_open`/
//! `orphan_close`/`reopen`/`interleave_events`/`proper_nestings`/
//! `rollback_markers`); the Python side owns counter-level assertions.
//!
//! The properties test five invariants over the same
//! `classify_tokens`-equivalent grammar, generated via a "token soup" of
//! the four bare-toggle marker literals interleaved with short ASCII/
//! hiragana filler.

use std::collections::HashMap;
use std::fs;

use proptest::prelude::*;
use serde::Deserialize;
use serde_json::Value;

/// One entry of `bare-toggle-model-vectors.json`. Only the fields this
/// (Rust-side, observable-outcome) mirror needs are named; the per-line
/// counter fields (`orphan_open`, `orphan_close`, `reopen`,
/// `interleave_events`, `proper_nestings`, `rollback_markers`) are left
/// for serde to ignore — the Python side owns those assertions.
#[derive(Debug, Deserialize)]
struct Vector {
    name: String,
    line: String,
    adopted: HashMap<String, u64>,
    #[serde(default)]
    invalid: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct VectorFile {
    vectors: Vec<Vector>,
}

/// The two marker literals for one construct, in `[open, close]` order.
fn tokens_for(construct: &str) -> [&'static str; 2] {
    match construct {
        "yokogumi" => ["［＃横組み］", "［＃横組み終わり］"],
        "keigakomi" => ["［＃罫囲み］", "［＃罫囲み終わり］"],
        other => panic!("unknown bare-toggle construct: {other}"),
    }
}

/// `(token, construct)` for all four marker literals — the fixed
/// vocabulary `pair_bare_toggles`'s `bare_toggle_marker` recognizes.
const MARKER_TOKENS: [(&str, &str); 4] = [
    ("［＃横組み］", "yokogumi"),
    ("［＃横組み終わり］", "yokogumi"),
    ("［＃罫囲み］", "keigakomi"),
    ("［＃罫囲み終わり］", "keigakomi"),
];

/// Parse `src` through the full public `aat_json_from_bytes` path — same
/// helper name/shape as `src/lib.rs`'s `mod tests` copy (not reusable
/// across the crate boundary, so duplicated here).
fn aat_value_for(src: &str) -> Value {
    serde_json::from_slice(&ab_aat::aat_json_from_bytes(src.as_bytes()).unwrap()).unwrap()
}

/// Depth-first collect of every `"raw"` node's `source` string, in
/// document order. Mirrors `src/lib.rs`'s `mod tests` helper of the same
/// name.
fn collect_raw_sources(v: &Value, out: &mut Vec<String>) {
    if let Some(obj) = v.as_object() {
        if obj.get("kind").and_then(Value::as_str) == Some("raw")
            && let Some(s) = obj.get("source").and_then(Value::as_str)
        {
            out.push(s.to_owned());
        }
        for key in ["blocks", "content", "children"] {
            if let Some(arr) = obj.get(key).and_then(Value::as_array) {
                for item in arr {
                    collect_raw_sources(item, out);
                }
            }
        }
    }
}

/// Depth-first collect of every node whose `"kind"` equals `kind`, in
/// document order. Mirrors `src/lib.rs`'s `mod tests` helper of the same
/// name.
fn collect_nodes<'a>(v: &'a Value, kind: &str, out: &mut Vec<&'a Value>) {
    match v {
        Value::Object(map) => {
            if map.get("kind").and_then(Value::as_str) == Some(kind) {
                out.push(v);
            }
            for key in ["blocks", "content", "children"] {
                if let Some(child) = map.get(key) {
                    collect_nodes(child, kind, out);
                }
            }
        }
        Value::Array(items) => {
            for item in items {
                collect_nodes(item, kind, out);
            }
        }
        _ => {}
    }
}

#[test]
fn bare_toggle_model_matches_shared_vectors() {
    let raw = fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/bare-toggle-model-vectors.json"
    ))
    .expect("shared bare-toggle vector file must be readable");
    let file: VectorFile =
        serde_json::from_str(&raw).expect("shared bare-toggle vector file must parse");
    assert!(!file.vectors.is_empty(), "vector file has no vectors");

    for vector in &file.vectors {
        let doc = aat_value_for(&format!("{}\n", vector.line));
        let mut raws = Vec::new();
        collect_raw_sources(&doc, &mut raws);

        for construct in ["yokogumi", "keigakomi"] {
            let expected = vector.adopted.get(construct).copied().unwrap_or(0);
            let mut containers = Vec::new();
            collect_nodes(&doc, construct, &mut containers);
            assert_eq!(
                containers.len() as u64,
                expected,
                "{}: {construct} adopted-container count",
                vector.name
            );
            if expected > 0 {
                let [open, close] = tokens_for(construct);
                assert!(
                    raws.iter().all(|s| s != open && s != close),
                    "{}: adopted {construct} must consume every {construct} marker; raw \
                     survivors: {raws:?}",
                    vector.name
                );
            }
        }

        for construct in &vector.invalid {
            for token in tokens_for(construct) {
                let expected_occurrences = vector.line.matches(token).count();
                if expected_occurrences == 0 {
                    continue;
                }
                let actual = raws.iter().filter(|s| s.as_str() == token).count();
                assert_eq!(
                    actual, expected_occurrences,
                    "{}: every {construct} token {token:?} on the line must survive as a raw \
                     node; raw survivors: {raws:?}",
                    vector.name
                );
            }
        }
    }
}

/// One token: a bare-toggle marker literal, or a short run of ASCII/
/// hiragana filler standing in for ordinary text.
fn bare_toggle_token() -> impl Strategy<Value = String> {
    prop_oneof![
        Just("［＃横組み］".to_owned()),
        Just("［＃横組み終わり］".to_owned()),
        Just("［＃罫囲み］".to_owned()),
        Just("［＃罫囲み終わり］".to_owned()),
        "[a-zあ-ん]{1,4}",
    ]
}

/// One generated line: 0-11 tokens concatenated directly (no separator —
/// Aozora text carries no mandatory inter-token whitespace, and none of
/// the marker literals is a substring of another or of itself repeated,
/// so concatenation never manufactures a spurious extra marker
/// occurrence).
fn bare_toggle_line() -> impl Strategy<Value = String> {
    prop::collection::vec(bare_toggle_token(), 0..12).prop_map(|tokens| tokens.concat())
}

/// 1-3 generated lines joined with `\n` — the shared "token soup"
/// generator.
fn bare_toggle_doc() -> impl Strategy<Value = String> {
    prop::collection::vec(bare_toggle_line(), 1..=3).prop_map(|lines| lines.join("\n"))
}

/// `(line_start, line_end)` of a node's `"span"`, or `None` if it has no
/// span (e.g. a `paragraph`/block wrapper node).
fn span_lines(v: &Value) -> Option<(u64, u64)> {
    let span = v.get("span")?;
    Some((
        span.get("line_start")?.as_u64()?,
        span.get("line_end")?.as_u64()?,
    ))
}

/// `(byte_start, byte_end)` of a node's `"span"`.
fn span_bytes(v: &Value) -> Option<(u64, u64)> {
    let span = v.get("span")?;
    Some((
        span.get("byte_start")?.as_u64()?,
        span.get("byte_end")?.as_u64()?,
    ))
}

/// Reduce a node to only `kind`/`value`/`source` plus recursively reduced
/// `content`/`children` — drops every span (byte offsets and line numbers
/// alike), so two structurally-equal subtrees compare equal regardless of
/// where they sit in their respective documents.
fn canonicalize(v: &Value) -> Value {
    match v {
        Value::Object(map) => {
            let mut out = serde_json::Map::new();
            for key in ["kind", "value", "source"] {
                if let Some(field) = map.get(key) {
                    out.insert(key.to_owned(), field.clone());
                }
            }
            for key in ["content", "children"] {
                if let Some(arr) = map.get(key).and_then(Value::as_array) {
                    out.insert(
                        key.to_owned(),
                        Value::Array(arr.iter().map(canonicalize).collect()),
                    );
                }
            }
            Value::Object(out)
        }
        Value::Array(items) => Value::Array(items.iter().map(canonicalize).collect()),
        other => other.clone(),
    }
}

/// Depth-first collect of every `"raw"` node whose `source` is one of the
/// four marker literals AND whose span lies ENTIRELY on `line`, in
/// document order.
///
/// Collection is restricted to marker-literal raw nodes rather than generic
/// text or gap nodes. A marker always becomes its own parser node with a tight
/// span (`bare_toggle_marker`'s subject), so it never straddles a line
/// boundary. In contrast, a generic filler or gap `text` node can straddle a
/// boundary (`push_source_gap` merges a maximal run with no recognized node
/// in between, spanning the embedded `\n` when neither line contains a
/// marker). Using marker-literal raw nodes as the isolation witness avoids
/// that merging entirely, isolating the bare-toggle classifier's decisions
/// from unrelated gap-merging behavior for `line_isolation` testing.
fn collect_marker_raw_on_line(v: &Value, line: u64, out: &mut Vec<String>) {
    if let Some(obj) = v.as_object() {
        if obj.get("kind").and_then(Value::as_str) == Some("raw")
            && let Some(s) = obj.get("source").and_then(Value::as_str)
            && MARKER_TOKENS.iter().any(|(token, _)| *token == s)
            && span_lines(v) == Some((line, line))
        {
            out.push(s.to_owned());
        }
        for key in ["blocks", "content", "children"] {
            if let Some(arr) = obj.get(key).and_then(Value::as_array) {
                for item in arr {
                    collect_marker_raw_on_line(item, line, out);
                }
            }
        }
    }
}

/// Depth-first collect of every `yokogumi`/`keigakomi` container node
/// whose span lies entirely on `line`, in document order. Containers
/// never straddle a line boundary by construction (`pair_bare_toggles`
/// rejects a marker whose own span crosses lines before pairing even
/// starts), so this filter is exact.
fn collect_containers_on_line<'a>(v: &'a Value, line: u64, out: &mut Vec<&'a Value>) {
    match v {
        Value::Object(map) => {
            if matches!(
                map.get("kind").and_then(Value::as_str),
                Some("yokogumi" | "keigakomi")
            ) && span_lines(v) == Some((line, line))
            {
                out.push(v);
            }
            for key in ["blocks", "content", "children"] {
                if let Some(child) = map.get(key) {
                    collect_containers_on_line(child, line, out);
                }
            }
        }
        Value::Array(items) => {
            for item in items {
                collect_containers_on_line(item, line, out);
            }
        }
        _ => {}
    }
}

/// Recursively assert every `yokogumi`/`keigakomi` container's `content`
/// children lie byte-span-wise within the container's own span (Property
/// 5, "nesting well-formed").
fn assert_nesting_well_formed(v: &Value) {
    match v {
        Value::Object(map) => {
            if matches!(
                map.get("kind").and_then(Value::as_str),
                Some("yokogumi" | "keigakomi")
            ) {
                let (parent_start, parent_end) =
                    span_bytes(v).expect("bare-toggle container must carry a span");
                if let Some(children) = map.get("content").and_then(Value::as_array) {
                    for child in children {
                        if let Some((child_start, child_end)) = span_bytes(child) {
                            // STRICT containment on both ends: the parent
                            // span runs from its open marker's byte_start to
                            // its close marker's byte_end, and children sit
                            // strictly between the two markers, so equality
                            // on either end would mean a child overlapping a
                            // consumed marker — a construction bug.
                            assert!(
                                parent_start < child_start && child_end < parent_end,
                                "child span [{child_start},{child_end}) not strictly inside \
                                 parent span [{parent_start},{parent_end}): parent={v}, \
                                 child={child}"
                            );
                        }
                    }
                }
            }
            for key in ["blocks", "content", "children"] {
                if let Some(child) = map.get(key) {
                    assert_nesting_well_formed(child);
                }
            }
        }
        Value::Array(items) => {
            for item in items {
                assert_nesting_well_formed(item);
            }
        }
        _ => {}
    }
}

proptest! {
    #![proptest_config(ProptestConfig { cases: 512, ..ProptestConfig::default() })]

    /// Property 1: for every marker literal, its input occurrence count
    /// equals its raw-survivor count plus its construct's adopted-
    /// container count (each container consumes exactly one open-token
    /// occurrence and one close-token occurrence of its construct) — no
    /// marker is ever duplicated or silently dropped.
    #[test]
    fn every_marker_consumed_or_preserved_exactly_once(doc in bare_toggle_doc()) {
        let value = aat_value_for(&format!("{doc}\n"));
        let mut raws = Vec::new();
        collect_raw_sources(&value, &mut raws);
        for (token, construct) in MARKER_TOKENS {
            let input_count = doc.matches(token).count();
            let raw_count = raws.iter().filter(|s| s.as_str() == token).count();
            let mut containers = Vec::new();
            collect_nodes(&value, construct, &mut containers);
            prop_assert_eq!(
                input_count,
                raw_count + containers.len(),
                "token {:?}: input={} raw={} containers={}",
                token,
                input_count,
                raw_count,
                containers.len()
            );
        }
    }

    /// Property 4: line 1's bare-toggle classification (which markers
    /// adopt into containers, which stay raw) is identical whether line 1
    /// is followed by a second line or stands alone.
    #[test]
    fn line_isolation(line1 in bare_toggle_line(), line2 in bare_toggle_line()) {
        let two_line = aat_value_for(&format!("{line1}\n{line2}\n"));
        let one_line = aat_value_for(&format!("{line1}\n"));

        let mut two_line_raws = Vec::new();
        collect_marker_raw_on_line(&two_line, 1, &mut two_line_raws);
        let mut one_line_raws = Vec::new();
        collect_marker_raw_on_line(&one_line, 1, &mut one_line_raws);
        prop_assert_eq!(two_line_raws, one_line_raws);

        let mut two_line_containers = Vec::new();
        collect_containers_on_line(&two_line, 1, &mut two_line_containers);
        let mut one_line_containers = Vec::new();
        collect_containers_on_line(&one_line, 1, &mut one_line_containers);
        prop_assert_eq!(two_line_containers.len(), one_line_containers.len());
        for (a, b) in two_line_containers.iter().zip(one_line_containers.iter()) {
            prop_assert_eq!(canonicalize(a), canonicalize(b));
        }
    }

    /// Property 5: every `yokogumi`/`keigakomi` container's children lie
    /// strictly within the container's own byte span.
    #[test]
    fn nesting_well_formed(doc in bare_toggle_doc()) {
        let value = aat_value_for(&format!("{doc}\n"));
        assert_nesting_well_formed(&value);
    }

    /// Property 3 (ADAPTER-level determinism): running
    /// the whole public pipeline (`aat_json_from_bytes` — decode, sanitize,
    /// parse, classify, serialize) twice on the same input yields identical
    /// output bytes, hence identical JSON. Byte equality is stronger
    /// than `Value` equality (it also verifies key order under the
    /// default `BTreeMap` map).
    /// The pass-level determinism property
    /// (`bare_toggle_pass_is_deterministic`, `src/lib.rs` `mod tests`)
    /// remains as a narrower, direct `pair_bare_toggles` check.
    #[test]
    fn deterministic(doc in bare_toggle_doc()) {
        let src = format!("{doc}\n");
        let first = ab_aat::aat_json_from_bytes(src.as_bytes()).unwrap();
        let second = ab_aat::aat_json_from_bytes(src.as_bytes()).unwrap();
        prop_assert_eq!(&first, &second, "adapter output must be byte-identical across runs");
        // Redundant given byte equality, but asserts the spec's literal
        // phrasing ("identical JSON") at the Value level too.
        let first_value: Value = serde_json::from_slice(&first).unwrap();
        let second_value: Value = serde_json::from_slice(&second).unwrap();
        prop_assert_eq!(first_value, second_value);
    }
}
