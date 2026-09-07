//! Native JSON projections for diagnostics, nodes, ruby pairs, container pairs,
//! annotation slugs and gaiji. Each result has a `{schemaVersion, data}` envelope.
//!
//! Versions belong to individual envelopes: adding a container kind changes
//! the container-pair contract without changing diagnostic records or ruby pairs.
//! Consumers validate the version of the projection they read.

use serde::Serialize;

use crate::encoding::gaiji::{self, gaiji_resolutions};
#[cfg(feature = "json")]
use crate::encoding::gaiji::{find_span, resolve_at};
use crate::{DiagnosticSource, NodeRef, RubySide, Severity, Tree};

/// Diagnostic records and their envelope.
pub const DIAGNOSTICS_SCHEMA_VERSION: u32 = 3;
/// Source-keyed node entries and their envelope.
pub const NODES_SCHEMA_VERSION: u32 = 3;
/// Ruby pair entries and their envelope.
pub const PAIRS_SCHEMA_VERSION: u32 = 3;
/// Container pairs, including scoped TCY's `combineUprightRange` tag.
pub const CONTAINER_PAIRS_SCHEMA_VERSION: u32 = 4;
/// Canonical annotation slug entries and their envelope.
pub const SLUGS_SCHEMA_VERSION: u32 = 3;
/// Gaiji resolution entries and their envelope.
pub const GAIJI_SCHEMA_VERSION: u32 = 3;

/// Project a slice of [`crate::Diagnostic`] into a `{ schemaVersion, data }`
/// JSON envelope. Every entry has the shape
/// `{ kind, span: { start, end }, codepoint? }`.
///
/// Empty input → `{"schemaVersion":3,"data":[]}`.
#[cfg(feature = "json")]
#[cfg_attr(docsrs, doc(cfg(feature = "json")))]
#[must_use]
pub fn diagnostics(diagnostics: &[crate::Diagnostic]) -> String {
    serialize_envelope(DIAGNOSTICS_SCHEMA_VERSION, &diagnostic_entries(diagnostics))
}

/// The structured `Diagnostic` records that back `diagnostics()` —
/// prefer this to re-parsing the JSON when a caller needs the values
/// directly (e.g. a Wasm binding building JS objects).
#[must_use]
pub fn diagnostic_entries(diagnostics: &[crate::Diagnostic]) -> Vec<Diagnostic> {
    diagnostics.iter().map(Diagnostic::from).collect()
}

/// Project an [`Tree`]'s source-keyed node side-table into a
/// `{ schemaVersion, data }` JSON envelope.
///
/// Every entry has the shape `{ kind, span: { start, end } }`,
/// source-coordinate, sorted by `span.start`. Empty parse →
/// `{"schemaVersion":3,"data":[]}`.
#[cfg(feature = "json")]
#[cfg_attr(docsrs, doc(cfg(feature = "json")))]
#[must_use]
pub fn nodes(tree: &Tree<'_>) -> String {
    serialize_envelope(NODES_SCHEMA_VERSION, &node_entries(tree))
}

/// The structured `Node` records that back `nodes()` — prefer this to
/// re-parsing the JSON when a caller needs the values directly.
#[must_use]
pub fn node_entries(tree: &Tree<'_>) -> Vec<Node> {
    tree.source_nodes()
        .iter()
        .map(|sn| Node {
            kind: sn.node.kind().as_json_tag(),
            span: sn.source_span.into(),
        })
        .collect()
}

/// Project an [`Tree`]'s pair table into a
/// `{ schemaVersion, data }` JSON envelope. Every entry has the shape
/// `{ kind, open: { start, end }, close: { start, end } }`.
///
/// One entry per matched open/close pair; unmatched closes and
/// unclosed opens are excluded (they have no partner span and would
/// only confuse editor surfaces). Useful for LSP requests like
/// `textDocument/linkedEditingRange` and
/// `textDocument/documentHighlight`.
///
/// Empty parse → `{"schemaVersion":3,"data":[]}`.
#[cfg(feature = "json")]
#[cfg_attr(docsrs, doc(cfg(feature = "json")))]
#[must_use]
pub fn pairs(tree: &Tree<'_>) -> String {
    serialize_envelope(PAIRS_SCHEMA_VERSION, &pair_entries(tree))
}

/// The structured `Pair` records that back `pairs()` — prefer this to
/// re-parsing the JSON when a caller needs the values directly.
#[must_use]
pub fn pair_entries(tree: &Tree<'_>) -> Vec<Pair> {
    tree.pairs()
        .iter()
        .map(|link| Pair {
            kind: link.kind.as_json_tag(),
            open: link.open.into(),
            close: link.close.into(),
        })
        .collect()
}

/// Project an [`Tree`]'s container open/close pair table into a
/// `{ schemaVersion, data }` JSON envelope.
///
/// Each entry has the shape
/// `{ kind, open: { offset }, close: { offset } }` where `kind` is
/// the [`crate::RegionFormat`] discriminant (one of `"indent"` /
/// `"warichu"` / `"framed"` / `"alignEnd"`) and the offsets are
/// **normalized-coordinate** byte positions that index the PUA
/// sentinel positions — not the source span the user wrote.
///
/// Coordinate-system distinction matters: editor surfaces that want
/// source-coordinate container pairs must translate through
/// [`Tree::source_nodes`].
///
/// Empty parse → `{"schemaVersion":4,"data":[]}`.
#[cfg(feature = "json")]
#[cfg_attr(docsrs, doc(cfg(feature = "json")))]
#[must_use]
pub fn container_pairs(tree: &Tree<'_>) -> String {
    serialize_envelope(
        CONTAINER_PAIRS_SCHEMA_VERSION,
        &container_pair_entries(tree),
    )
}

/// The structured `ContainerPair` records that back
/// `container_pairs()` — prefer this to re-parsing the JSON when a caller
/// needs the values directly.
#[must_use]
pub fn container_pair_entries(tree: &Tree<'_>) -> Vec<ContainerPair> {
    tree.container_pairs()
        .iter()
        .map(|pair| ContainerPair {
            kind: pair.kind.as_json_tag(),
            open: Offset {
                offset: pair.open.get(),
            },
            close: Offset {
                offset: pair.close.get(),
            },
        })
        .collect()
}

/// Project the canonical slug catalogue ([`crate::SLUGS`]) into a
/// `{ schemaVersion, data }` JSON envelope.
///
/// Each entry has the shape `{ canonical, family, accepts_param, doc,
/// partner }`: `family` is the camelCase form of the
/// [`crate::SlugFamily`] variant, `partner` is `null` for non-paired
/// families. The catalogue is independent of any parse. Consumers can
/// use it to complete annotations without duplicating the parser's
/// canonical spelling and pairing tables.
#[cfg(feature = "json")]
#[cfg_attr(docsrs, doc(cfg(feature = "json")))]
#[must_use]
pub fn slugs() -> String {
    serialize_envelope(SLUGS_SCHEMA_VERSION, &slug_entries())
}

/// The structured `Slug` records that back `slugs()` — prefer this to
/// re-parsing the JSON when a caller needs the catalogue directly.
#[must_use]
pub fn slug_entries() -> Vec<Slug> {
    crate::SLUGS
        .iter()
        .map(|s| Slug {
            canonical: s.canonical,
            family: s.family.as_json_tag(),
            accepts_param: s.accepts_param,
            doc: s.doc,
            partner: s.partner,
        })
        .collect()
}

/// Project resolved `※［＃…］` gaiji references from `source` into a
/// `{ schemaVersion, data }` JSON envelope.
///
/// Each entry is
/// `{ span: { start, end }, description, mencode, codepoint, resolved }`
/// in source-byte coordinates; `mencode` / `codepoint` / `resolved` are
/// `null` when absent or unresolved. Walks the source once — `O(source)`.
///
/// Powers inlay-hint UIs (`→GLYPH` after each reference) and batch gaiji
/// audits. The scan + resolution are the single authority in
/// [`crate::encoding::gaiji`]; this is only their wire projection.
///
/// Empty / gaiji-free source → `{"schemaVersion":3,"data":[]}`.
#[cfg(feature = "json")]
#[cfg_attr(docsrs, doc(cfg(feature = "json")))]
#[must_use]
pub fn gaiji(source: &str) -> String {
    serialize_envelope(GAIJI_SCHEMA_VERSION, &gaiji_entries(source))
}

/// The structured `GaijiResolution` records that back `gaiji()` —
/// prefer this to re-parsing the JSON when a caller needs the values
/// directly.
#[must_use]
pub fn gaiji_entries(source: &str) -> Vec<GaijiResolution> {
    gaiji_resolutions(source)
        .into_iter()
        .map(Into::into)
        .collect()
}

/// The structured ruby (furigana) records projected from an [`Tree`]'s nodes.
///
/// Prefer this to re-parsing JSON when a caller needs the values directly
/// (e.g. the `ab-aozora-aat` adapter, which retired its `RUBY_RE` regex
/// reparse in favor of this typed projection).
///
/// Each entry has the shape `{ span: { start, end }, base, reading, side }`
/// in source-byte coordinates, where `side` is `"right"` or `"left"`.
///
/// Mirrors [`gaiji_entries`]'s shape: walks [`Tree::source_nodes`], matches
/// the nodes whose payload is [`crate::Node::Ruby`], and resolves the
/// `base`/`reading` `ContentRange`s against the tree's backing
/// [`crate::ast::NodeStore`] the same way [`crate::splice`]'s
/// `coupled_target_text` resolves a split-ownership node's target (via
/// `NodeStore::content_range_as_plain`). An entry is omitted when its base or
/// reading is not a single plain run (mixed/segmented content) — the caller
/// falls back to its own raw-node handling for that case, same as an
/// unresolved gaiji reference.
#[must_use]
pub fn ruby_entries(tree: &Tree<'_>) -> Vec<RubyEntry> {
    let store = &tree.lex_output().store;
    tree.source_nodes()
        .iter()
        .filter_map(|sn| {
            let (NodeRef::Inline(leaf) | NodeRef::BlockLeaf(leaf)) = sn.node else {
                return None;
            };
            let crate::Node::Ruby(ruby) = leaf else {
                return None;
            };
            let base = store.content_range_as_plain(ruby.base)?.to_owned();
            let reading = store.content_range_as_plain(ruby.reading)?.to_owned();
            Some(RubyEntry {
                span: sn.source_span.into(),
                base,
                reading,
                // `RubySide` is `#[non_exhaustive]` upstream — the wildcard
                // arm covers any future variant by defaulting to "right"
                // (the same defensive convention as `severity_str`).
                side: match ruby.side {
                    RubySide::Left => "left",
                    RubySide::Right | _ => "right",
                },
            })
        })
        .collect()
}

/// Resolve the gaiji reference at `byte_offset` (cursor-local).
///
/// Serialises the single
/// `{ span, description, mencode, codepoint, resolved }` object — or the
/// literal `"null"` when the offset is not inside a `※［＃…］` span.
///
/// For editor cursor-hover: the scan is bounded to a window around the
/// cursor, so cost is independent of document size (unlike
/// [`gaiji()`], which walks the whole source).
#[cfg(feature = "json")]
#[cfg_attr(docsrs, doc(cfg(feature = "json")))]
#[must_use]
pub fn gaiji_at(source: &str, byte_offset: usize) -> String {
    find_span(source, byte_offset)
        .and_then(|(start, end)| resolve_at(source, start, end))
        .map_or_else(
            || "null".to_owned(),
            |g| {
                serde_json::to_string(&GaijiResolution::from(g))
                    .unwrap_or_else(|_| "null".to_owned())
            },
        )
}

// ────────────────────────────────────────────────────────────────────
// Internal: envelope + wire structs
// ────────────────────────────────────────────────────────────────────

#[cfg(feature = "json")]
#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct Envelope<'a, T> {
    schema_version: u32,
    data: &'a [T],
}

// ────────────────────────────────────────────────────────────────────
// JSON Schema introspection
// ────────────────────────────────────────────────────────────────────

/// JSON Schema (draft 2020-12) describing the
/// [`diagnostics`] envelope output.
///
/// Schema-feature only. Used by `xtask schema dump` to commit the
/// schema artefact under `crates/aozora-book/src/json/`, and by the
/// `aozora schema` CLI subcommand for ad-hoc introspection.
#[cfg(feature = "schema")]
#[cfg_attr(docsrs, doc(cfg(feature = "schema")))]
#[must_use]
pub fn schema_diagnostics() -> serde_json::Value {
    envelope_schema(
        DIAGNOSTICS_SCHEMA_VERSION,
        "AozoraDiagnosticsEnvelope",
        "Envelope returned by aozora::json::diagnostics.",
        schemars::schema_for!(Diagnostic),
    )
}

/// JSON Schema for the [`nodes`] envelope output.
#[cfg(feature = "schema")]
#[cfg_attr(docsrs, doc(cfg(feature = "schema")))]
#[must_use]
pub fn schema_nodes() -> serde_json::Value {
    envelope_schema(
        NODES_SCHEMA_VERSION,
        "AozoraNodesEnvelope",
        "Envelope returned by aozora::json::nodes.",
        schemars::schema_for!(Node),
    )
}

/// JSON Schema for the [`pairs`] envelope output.
#[cfg(feature = "schema")]
#[cfg_attr(docsrs, doc(cfg(feature = "schema")))]
#[must_use]
pub fn schema_pairs() -> serde_json::Value {
    envelope_schema(
        PAIRS_SCHEMA_VERSION,
        "AozoraPairsEnvelope",
        "Envelope returned by aozora::json::pairs.",
        schemars::schema_for!(Pair),
    )
}

/// JSON Schema for the [`container_pairs`] envelope output.
#[cfg(feature = "schema")]
#[cfg_attr(docsrs, doc(cfg(feature = "schema")))]
#[must_use]
pub fn schema_container_pairs() -> serde_json::Value {
    envelope_schema(
        CONTAINER_PAIRS_SCHEMA_VERSION,
        "AozoraContainerPairsEnvelope",
        "Envelope returned by aozora::json::container_pairs.",
        schemars::schema_for!(ContainerPair),
    )
}

/// Wrap the per-entry schema in the canonical
/// `{schemaVersion, data: […]}` envelope. The envelope shape is
/// shared by all four wire functions; only the inner item schema
/// varies.
#[cfg(feature = "schema")]
fn envelope_schema(
    version: u32,
    title: &str,
    description: &str,
    item_schema: schemars::Schema,
) -> serde_json::Value {
    // `schema_for!(ItemWire)` returns a self-contained document: its
    // shared sub-types (e.g. `Span`) live under a root `$defs` and
    // are referenced as `#/$defs/…`. Embedding it verbatim as `items`
    // would bury that `$defs` under `properties/data/items`, leaving the
    // `#/$defs/…` refs — which resolve against the *document* root —
    // dangling, so strict resolvers (quicktype, and any other consumer
    // of the published schema) reject it. Hoist the item schema's
    // `$defs` to the envelope root and drop its redundant per-item
    // `$schema` dialect marker so the refs resolve against the root.
    let mut item = item_schema.to_value();
    let defs = item.as_object_mut().and_then(|obj| {
        obj.remove("$schema");
        obj.remove("$defs")
    });
    let mut root = serde_json::json!({
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "title": title,
        "description": description,
        "type": "object",
        "additionalProperties": false,
        "required": ["schemaVersion", "data"],
        "properties": {
            "schemaVersion": {
                "description": "Version of this envelope's wire contract.",
                "type": "integer",
                "const": version,
            },
            "data": {
                "description": "Per-entry payload array; one item per emitted diagnostic / node / pair.",
                "type": "array",
                "items": item,
            },
        },
    });
    if let Some(defs) = defs {
        root.as_object_mut()
            .expect("envelope root is a JSON object literal")
            .insert("$defs".to_owned(), defs);
    }
    root
}

#[cfg(feature = "json")]
fn serialize_envelope<T: Serialize>(version: u32, data: &[T]) -> String {
    let env = Envelope {
        schema_version: version,
        data,
    };
    serde_json::to_string(&env)
        .unwrap_or_else(|_| format!(r#"{{"schemaVersion":{version},"data":[]}}"#))
}

/// One half-open `[start, end)` byte span in a wire envelope.
#[derive(Debug, Clone, Copy, Serialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub struct Span {
    start: u32,
    end: u32,
}

impl From<crate::Span> for Span {
    fn from(s: crate::Span) -> Self {
        Self {
            start: s.start,
            end: s.end,
        }
    }
}

/// One `diagnostics` envelope entry — a projected [`crate::Diagnostic`].
#[derive(Debug, Clone, Serialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub struct Diagnostic {
    kind: &'static str,
    /// Stable kebab-case diagnostic code — the conformance-contract
    /// identity of this diagnostic. Always the
    /// 1:1 kebab form of `kind`.
    code: String,
    severity: &'static str,
    source: &'static str,
    span: Span,
}

impl From<&crate::Diagnostic> for Diagnostic {
    fn from(d: &crate::Diagnostic) -> Self {
        let kind = d.code().rsplit("::").next().unwrap_or("unknown");
        let code = kind.replace('_', "-");
        Self {
            kind,
            code,
            severity: severity_str(d.severity()),
            source: source_str(d.source()),
            span: d.span().into(),
        }
    }
}

const fn severity_str(s: Severity) -> &'static str {
    // `Severity` is `#[non_exhaustive]` upstream — the wildcard arm
    // covers any future variant by defaulting to "error" so consumers
    // err on the side of surfacing it until they upgrade.
    match s {
        Severity::Warning => "warning",
        Severity::Note => "note",
        Severity::Error | _ => "error",
    }
}

const fn source_str(s: DiagnosticSource) -> &'static str {
    // `DiagnosticSource` is `#[non_exhaustive]` upstream — the
    // wildcard arm covers any future variant by defaulting to
    // "internal" so consumers filtering library-bug diagnostics
    // catch it until they upgrade.
    match s {
        DiagnosticSource::Source => "source",
        DiagnosticSource::Internal | _ => "internal",
    }
}

/// One `nodes` envelope entry — a classified node span in source coords.
#[derive(Debug, Clone, Copy, Serialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub struct Node {
    kind: &'static str,
    span: Span,
}

/// One `pairs` envelope entry — a matched bracket pair.
#[derive(Debug, Clone, Copy, Serialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub struct Pair {
    kind: &'static str,
    open: Span,
    close: Span,
}

/// One `container_pairs` envelope entry — a paired container, open/close
/// in normalized coordinates.
#[derive(Debug, Clone, Copy, Serialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub struct ContainerPair {
    kind: &'static str,
    open: Offset,
    close: Offset,
}

/// A single byte offset (a `container_pairs` open/close in normalized
/// coordinates).
#[derive(Debug, Clone, Copy, Serialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub struct Offset {
    offset: u32,
}

/// One `slugs` envelope entry — a row of the annotation slug catalogue.
#[derive(Debug, Clone, Copy, Serialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub struct Slug {
    canonical: &'static str,
    family: &'static str,
    accepts_param: bool,
    doc: &'static str,
    // No `skip_serializing_if`: non-paired families emit `partner:null`
    // (byte-identical to the prior aozora-wasm `slugs_json` shape).
    partner: Option<&'static str>,
}

/// Source-byte span carried by [`GaijiResolution`].
///
/// Distinct from [`Span`] (whose `u32` fields cover sanitized-source
/// spans): gaiji offsets are raw `usize` byte positions into the original
/// source, kept as-is to stay byte-identical to the prior aozora-wasm
/// projection.
#[derive(Debug, Clone, Copy, Serialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub struct ByteSpan {
    start: usize,
    end: usize,
}

/// One `gaiji` envelope entry — a resolved `※［＃…］` reference.
#[derive(Debug, Clone, Serialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub struct GaijiResolution {
    span: ByteSpan,
    description: String,
    // Nulls (not skipped) so the shape is fixed across entries.
    mencode: Option<String>,
    codepoint: Option<u32>,
    resolved: Option<String>,
}

impl From<gaiji::GaijiResolution> for GaijiResolution {
    fn from(g: gaiji::GaijiResolution) -> Self {
        Self {
            span: ByteSpan {
                start: g.start,
                end: g.end,
            },
            description: g.description,
            mencode: g.mencode,
            codepoint: g.codepoint,
            resolved: g.resolved,
        }
    }
}

/// One [`ruby_entries`] entry — a ruby (furigana) annotation resolved to its
/// plain base/reading text, in source-byte coordinates.
#[derive(Debug, Clone, Serialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub struct RubyEntry {
    span: Span,
    base: String,
    reading: String,
    /// `"right"` (`｜base《reading》`) or `"left"`
    /// (`［＃「base」の左に「reading」のルビ］`).
    side: &'static str,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Document;

    #[test]
    #[cfg(feature = "json")]
    fn slugs_envelope_lists_catalogue_with_known_families() {
        let json = slugs();
        assert!(json.contains(r#""schemaVersion":3"#));
        assert!(json.contains(r#""canonical":"#));
        assert!(json.contains(r#""family":"#));
        // Guard against the silent `_ => "unknown"` degrade: every
        // shipped slug must map to an explicit camelCase family.
        assert!(
            !json.contains(r#""family":"unknown""#),
            "shipped catalogue leaked an unknown family: {json}"
        );
    }

    #[test]
    #[cfg(feature = "json")]
    fn gaiji_resolutions_empty_envelope_for_plain_text() {
        assert_eq!(gaiji("no gaiji here"), r#"{"schemaVersion":3,"data":[]}"#);
    }

    #[test]
    #[cfg(feature = "json")]
    fn gaiji_resolutions_emits_resolved_entry_in_source_coords() {
        let json = gaiji("※［＃「々」］");
        assert!(json.contains(r#""schemaVersion":3"#));
        assert!(
            json.contains(r#""span":{"start":0,"end":21}"#),
            "json: {json}"
        );
        assert!(json.contains(r#""description":"々""#), "json: {json}");
        assert!(json.contains(r#""resolved":"々""#), "json: {json}");
        // Unresolved/absent fields serialise as null, not skipped.
        assert!(json.contains(r#""mencode":null"#), "json: {json}");
    }

    #[test]
    #[cfg(feature = "json")]
    fn gaiji_resolution_at_returns_object_inside_span_else_null() {
        let src = "あ※［＃「々」］い";
        let inside = src.find('※').unwrap() + "※".len();
        let at = gaiji_at(src, inside);
        assert!(at.contains(r#""description":"々""#), "{at}");
        assert!(at.contains(r#""resolved":"々""#), "{at}");
        // A cursor outside any reference resolves to the literal "null".
        assert_eq!(gaiji_at(src, 0), "null");
    }

    #[test]
    fn scoped_tcy_changes_only_the_container_pair_contract() {
        assert_eq!(CONTAINER_PAIRS_SCHEMA_VERSION, 4);
        assert_eq!(DIAGNOSTICS_SCHEMA_VERSION, 3);
        assert_eq!(NODES_SCHEMA_VERSION, 3);
        assert_eq!(PAIRS_SCHEMA_VERSION, 3);
        assert_eq!(SLUGS_SCHEMA_VERSION, 3);
        assert_eq!(GAIJI_SCHEMA_VERSION, 3);
    }

    #[test]
    #[cfg(feature = "json")]
    fn empty_diagnostics_round_trip_envelope() {
        let json = diagnostics(&[]);
        assert_eq!(json, r#"{"schemaVersion":3,"data":[]}"#);
    }

    #[test]
    #[cfg(feature = "json")]
    fn empty_nodes_round_trip_envelope() {
        let doc = Document::new("plain");
        let tree = doc.parse();
        let json = nodes(&tree);
        assert_eq!(json, r#"{"schemaVersion":3,"data":[]}"#);
    }

    #[test]
    #[cfg(feature = "json")]
    fn empty_pairs_round_trip_envelope() {
        let doc = Document::new("plain");
        let tree = doc.parse();
        let json = pairs(&tree);
        assert_eq!(json, r#"{"schemaVersion":3,"data":[]}"#);
    }

    #[test]
    #[cfg(feature = "json")]
    fn diagnostic_entries_carry_kebab_code() {
        // Any source producing an unclosed_bracket diagnostic:
        let doc = Document::new("あ［＃ここから".to_owned());
        let tree = doc.parse();
        let entries = diagnostic_entries(tree.diagnostics());
        assert!(!entries.is_empty());
        let value = serde_json::to_value(&entries).unwrap();
        let entry = &value[0];
        let code = entry["code"].as_str().unwrap();
        assert_eq!(code, entry["kind"].as_str().unwrap().replace('_', "-"));
        assert!(!code.contains("::"));
    }

    #[test]
    #[cfg(feature = "json")]
    fn ruby_serialises_with_kind_ruby_in_nodes() {
        let doc = Document::new("｜青梅《おうめ》");
        let tree = doc.parse();
        let json = nodes(&tree);
        assert!(json.contains(r#""kind":"ruby""#));
        assert!(json.contains(r#""schemaVersion":3"#));
    }

    #[test]
    #[cfg(feature = "json")]
    fn ruby_serialises_in_pairs() {
        let doc = Document::new("｜青梅《おうめ》");
        let tree = doc.parse();
        let json = pairs(&tree);
        assert!(json.contains(r#""kind":"ruby""#));
        assert!(json.contains(r#""open":"#));
        assert!(json.contains(r#""close":"#));
    }

    #[test]
    fn ruby_entries_exposes_side_base_reading() {
        let src = "｜漢字《かんじ》\n名［＃「名」の左に「な」のルビ］\n";
        let doc = Document::new(src);
        let tree = doc.parse();
        let entries = ruby_entries(&tree);
        assert!(
            entries
                .iter()
                .any(|e| e.base == "漢字" && e.reading == "かんじ" && e.side == "right"),
            "no right ruby entry: {entries:?}"
        );
        assert!(
            entries
                .iter()
                .any(|e| e.base == "名" && e.reading == "な" && e.side == "left"),
            "no left ruby entry: {entries:?}"
        );
    }

    #[test]
    fn ruby_entries_skips_segments_base_ruby() {
        // A gaiji-reference base (※［＃…］) is segmented content, not a single
        // plain run; content_range_as_plain returns None and the entry must be
        // omitted (the AAT adapter then keeps its own typed emission — pinned
        // adapter-side by gaiji_base_ruby_keeps_v1_typed_emission).
        let src = "※［＃「木＋吶のつくり」、第3水準1-85-57］《かい》\n";
        let doc = Document::new(src);
        let tree = doc.parse();
        let entries = ruby_entries(&tree);
        assert!(
            entries.iter().all(|e| e.reading != "かい"),
            "Segments-base ruby must not project into ruby_entries: {entries:?}"
        );
    }

    #[test]
    fn pair_kind_camel_case_covers_all_known_kinds() {
        use crate::PairKind;
        assert_eq!(PairKind::Bracket.as_json_tag(), "bracket");
        assert_eq!(PairKind::Ruby.as_json_tag(), "ruby");
        assert_eq!(PairKind::AngleQuote.as_json_tag(), "angleQuote");
        assert_eq!(PairKind::Tortoise.as_json_tag(), "tortoise");
        assert_eq!(PairKind::Quote.as_json_tag(), "quote");
    }

    #[test]
    fn container_kind_wire_tags_via_as_json_tag() {
        use ab_aozora_syntax::{BoutenKind, BoutenPosition, RegionFormat};
        // `RegionFormat::as_json_tag` is the single authority on the
        // container-pairs wire tag (no `_ => "unknown"` fallback —
        // exhaustiveness is enforced in aozora-syntax). The scope-specific
        // `boutenRange` / `combineUprightRange` strings are preserved verbatim
        // so unrelated region tags stay byte-stable.
        assert_eq!(RegionFormat::Bold { padded: false }.as_json_tag(), "bold");
        assert_eq!(RegionFormat::Bold { padded: true }.as_json_tag(), "bold");
        assert_eq!(
            RegionFormat::Italic { padded: false }.as_json_tag(),
            "italic"
        );
        assert_eq!(
            RegionFormat::Italic { padded: true }.as_json_tag(),
            "italic"
        );
        assert_eq!(
            RegionFormat::Bouten {
                kind: BoutenKind::Goma,
                position: BoutenPosition::Right,
            }
            .as_json_tag(),
            "boutenRange"
        );
    }
}
