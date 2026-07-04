# Source Authority Representability Gate Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace parser-count evidence as the basis for AAT representability claims with a source-backed Aozora marker inventory that proves each source body marker is either typed in AAT, preserved as raw/source-derived evidence, out of scope, or explicitly unsupported.

**Architecture:** `ab-source-syntax` owns one lexical source-marker recognition engine. Existing `source_events` and new `source_markers` must both be derived from that same scan, so inventory and projection cannot diverge. `ab-coverage` owns corpus-scale classification, reporting, allowlist handling, and matrix/gate evaluation. Parser and adapter measurements remain supporting evidence, but source inventory is the authority for "can we represent Aozora markdown?" claims.

**Tech Stack:** Rust workspace crates `ab-source-syntax`, `ab-coverage`, `ab-encoding`; TOML matrix `data/aozora-syntax-coverage.toml`; JSON/Markdown reports under `docs/superpowers/reports/`; shell smokes under `tests/`; `just` and flake checks for repeatable gates.

## Global Constraints

- Parser outputs are supporting evidence only; raw Aozora source markers are the authority for representability.
- Source inventory must not invoke parsers or adapters.
- Full-corpus source inventory may read the local corpus/index; flake checks must use fixtures and must not require `/db` or network.
- Unknown source marker classes are a gate failure unless listed in a reviewed allowlist with a reason.
- AAT representability is not the same as typed semantic support: `raw_preserved` is acceptable for v1 representability but must be counted separately from `typed`.
- Do not weaken existing parser-IR mapping gates; this plan adds a source-authority gate beside them.
- Use canonical source decoding from `ab-encoding::decode_source_bytes`, not ad hoc decoding.
- Keep generated full-corpus artifacts out of `data/`; commit durable summaries under `docs/superpowers/reports/`.

---

## File Structure

- Modify `crates/ab-source-syntax/src/lib.rs`
  - Add a lexical `source_markers` API that returns raw source marker spans, including ruby, gaiji, commands, accent notation, and malformed marker starts.
- Modify `crates/ab-coverage/Cargo.toml`
  - Add `ab-source-syntax` and `ab-encoding` dependencies.
  - Add a new `[[bin]]` named `ab-source-inventory`.
- Create `crates/ab-coverage/src/source_corpus.rs`
  - Shared index loading and source byte reading for source-only corpus scans.
- Create `crates/ab-coverage/src/source_inventory.rs`
  - Classify source markers against `data/aozora-syntax-coverage.toml` rows and summarize unknowns.
- Create `crates/ab-coverage/src/bin/source_inventory.rs`
  - CLI for fixture/full-corpus inventory runs.
- Modify `crates/ab-coverage/src/lib.rs`
  - Export `source_corpus` and `source_inventory`.
- Modify `data/aozora-syntax-coverage.schema.json`
  - Add per-row `representability` table.
- Modify `crates/ab-coverage/src/matrix.rs`
  - Parse the new `representability` table.
- Modify `crates/ab-coverage/src/schema.rs`
  - Validate representability statuses and source-inventory row links.
- Modify `data/aozora-syntax-coverage.toml`
  - Add reviewed representability status for priority-1 rows first.
- Create `data/aozora-source-inventory-allowlist.toml`
  - Reviewed unknown/source-only allowlist for markers that must not fail the gate.
- Create `data/aozora-source-inventory-allowlist.schema.json`
  - Schema for reviewed allowlist entries.
- Create `data/aozora-source-inventory.schema.json`
  - Schema for inventory JSON summaries.
- Create `tests/source-inventory-smoke.sh`
  - Fixture smoke that exercises known and unknown markers.
- Modify `justfile`
  - Add `source-inventory-smoke` and `source-inventory-full`.
- Modify `flake.nix`
  - Add a fixture-only `source-inventory-smoke` check.
- Create `docs/superpowers/reports/2026-07-04-source-authority-representability.md`
  - Durable report after the first full run.

---

### Task 1: Shared Source Marker Recognition Engine

**Files:**
- Modify: `crates/ab-source-syntax/src/lib.rs`

**Interfaces:**
- Produces:
  - internal `enum RawMarkerKind`
  - internal `struct RawMarker<'a>`
  - internal `fn scan_markers(txt: &str) -> Vec<RawMarker<'_>>`
  - `pub enum SourceMarkerKind`
  - `pub struct SourceMarker<'a>`
  - `pub fn source_markers(txt: &str) -> Vec<SourceMarker<'_>>`
- Consumes: existing `SourceSpan`, `marker_end_on_same_line`, `command_end_on_same_line`, `explicit_ruby_bounds`, and editorial-note helpers.
- Changes: `source_events(txt)` must consume `scan_markers(txt)` rather than keep an independent marker recognizer.

- [ ] **Step 1: Add the public marker model**

Add these definitions next to `SourceEventKind`:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SourceMarkerKind {
    RubyExplicit,
    RubyImplicit,
    GaijiFullwidth,
    GaijiAscii,
    CommandFullwidth,
    CommandAscii,
    AccentNotation,
    EditorialNoteRubyCorrection,
    EditorialNoteBottomTextCorrection,
    SegmentBoundaryTerminalProvenance,
    MalformedGaiji,
    MalformedGaijiAscii,
    MalformedCommand,
    MalformedCommandAscii,
    MalformedRuby,
    MalformedImplicitRuby,
    MalformedAccentNotation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceMarker<'a> {
    pub span: SourceSpan,
    pub raw: &'a str,
    pub body: &'a str,
    pub kind: SourceMarkerKind,
}
```

- [ ] **Step 2: Add marker extraction tests**

Add tests in `crates/ab-source-syntax/src/lib.rs`:

```rust
#[test]
fn source_markers_return_raw_marker_body_and_span() {
    let text = "吾輩《わがはい》\n※［＃「口＋世」、U+546D］\n［＃ここから横組み］\n〔e'tude〕\n底本：fixture";
    let markers = source_markers(text);

    assert_eq!(markers.len(), 5);
    assert_eq!(markers[0].kind, SourceMarkerKind::RubyImplicit);
    assert_eq!(markers[0].raw, "《わがはい》");
    assert_eq!(markers[0].body, "わがはい");
    assert_eq!(markers[0].span.line, 1);

    assert_eq!(markers[1].kind, SourceMarkerKind::GaijiFullwidth);
    assert_eq!(markers[1].raw, "※［＃「口＋世」、U+546D］");
    assert_eq!(markers[1].body, "「口＋世」、U+546D");
    assert_eq!(markers[1].span.line, 2);

    assert_eq!(markers[2].kind, SourceMarkerKind::CommandFullwidth);
    assert_eq!(markers[2].body, "ここから横組み");

    assert_eq!(markers[3].kind, SourceMarkerKind::AccentNotation);
    assert_eq!(markers[3].raw, "〔e'tude〕");
    assert_eq!(markers[3].body, "e'tude");

    assert_eq!(
        markers[4].kind,
        SourceMarkerKind::SegmentBoundaryTerminalProvenance
    );
    assert_eq!(markers[4].raw, "底本：fixture");
}

#[test]
fn source_markers_surface_malformed_starts() {
    let text = "※［＃未完了\n※[#broken\n［＃ここから割り注\n[#broken\n｜未完了\n《未完了\n〔未完了";
    let markers = source_markers(text);

    assert_eq!(markers.len(), 7);
    assert_eq!(markers[0].kind, SourceMarkerKind::MalformedGaiji);
    assert_eq!(markers[0].raw, "※［＃");
    assert_eq!(markers[1].kind, SourceMarkerKind::MalformedGaijiAscii);
    assert_eq!(markers[1].raw, "※[#");
    assert_eq!(markers[2].kind, SourceMarkerKind::MalformedCommand);
    assert_eq!(markers[2].raw, "［＃");
    assert_eq!(markers[3].kind, SourceMarkerKind::MalformedCommandAscii);
    assert_eq!(markers[3].raw, "[#");
    assert_eq!(markers[4].kind, SourceMarkerKind::MalformedRuby);
    assert_eq!(markers[4].raw, "｜");
    assert_eq!(markers[5].kind, SourceMarkerKind::MalformedImplicitRuby);
    assert_eq!(markers[5].raw, "《");
    assert_eq!(markers[6].kind, SourceMarkerKind::MalformedAccentNotation);
    assert_eq!(markers[6].raw, "〔");
}

#[test]
fn source_markers_share_recognition_with_source_events() {
    let text = "｜吾輩《わがはい》\n※［＃「口＋世」、U+546D］\n［＃「おもて」のルビは「うら」］";
    let markers = source_markers(text);
    let events = source_events(text);

    assert_eq!(markers.len(), 3);
    assert!(events.iter().any(|event| matches!(
        event.kind,
        SourceEventKind::Ruby { .. }
    )));
    assert!(events.iter().any(|event| matches!(
        event.kind,
        SourceEventKind::Gaiji { .. }
    )));
    assert!(events.iter().any(|event| matches!(
        event.kind,
        SourceEventKind::EditorialNote { .. }
    )));
}
```

- [ ] **Step 3: Implement the shared scanner**

Refactor marker recognition into an internal `scan_markers(txt)` function and make both public APIs consume it:

```rust
fn scan_markers(txt: &str) -> Vec<RawMarker<'_>> {
    // Single recognition pass for source_events and source_markers.
}

pub fn source_markers(txt: &str) -> Vec<SourceMarker<'_>> {
    scan_markers(txt)
        .into_iter()
        .filter_map(SourceMarker::from_raw_marker)
        .collect()
}

pub fn source_events(txt: &str) -> Vec<SourceEvent<'_>> {
    let markers = scan_markers(txt);
    // Rebuild the existing SourceEvent stream from marker spans plus intervening text.
}
```

`scan_markers` must:

- emit `RubyExplicit` for `｜base《reading》`, with `raw` equal to the full `｜...《...》` marker and `body` equal to `base《reading》`;
- emit `RubyImplicit` for `《reading》`, with `body` equal to `reading`;
- emit `GaijiFullwidth` for `※［＃...］`;
- emit `GaijiAscii` for `※[#...]`;
- emit `CommandFullwidth` for `［＃...］`;
- emit `CommandAscii` for `[#...]`;
- emit `AccentNotation` for `〔...〕`;
- emit editorial-note markers that currently become `SourceEventKind::EditorialNote`;
- emit terminal-provenance segment-boundary markers that currently become `SourceEventKind::SegmentBoundary`;
- use `command_end_on_same_line` for command markers so nested gaiji/command delimiters behave exactly as today;
- skip bare `※` and `｜` as text-control characters, matching current `source_events` behavior;
- emit malformed starts for unterminated fullwidth/ascii gaiji, fullwidth/ascii command, explicit ruby, implicit ruby, and accent notation.

The existing public behavior of `comparison_lossy_body`, `source_annotations`, and `source_events` must be unchanged except that malformed marker starts are now observable through `source_markers`.

- [ ] **Step 4: Verify**

Run:

```bash
cargo test -p ab-source-syntax source_markers -- --nocapture
```

Expected: all three new tests pass.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-source-syntax/src/lib.rs
git commit -m "feat: expose source marker inventory"
```

---

### Task 2: Source-Only Corpus Reader

**Files:**
- Modify: `crates/ab-coverage/Cargo.toml`
- Modify: `crates/ab-coverage/src/lib.rs`
- Create: `crates/ab-coverage/src/source_corpus.rs`

**Interfaces:**
- Produces:
  - `pub struct SourceWork { pub work_id: String, pub indexed_path: String, pub bytes: Vec<u8>, pub decoded: ab_encoding::DecodedSource }`
  - `pub fn load_index(path: &Path) -> Result<BTreeMap<String, String>>`
  - `pub fn read_source_work(corpus_root: &Path, work_id: &str, indexed_path: &str) -> Result<SourceWork>`
- Consumes: `ab_encoding::decode_source_bytes`.

- [ ] **Step 1: Add dependencies**

In `crates/ab-coverage/Cargo.toml`, add:

```toml
ab-encoding.workspace = true
ab-source-syntax.workspace = true
```

- [ ] **Step 2: Export the module**

In `crates/ab-coverage/src/lib.rs`, add:

```rust
pub mod source_corpus;
```

- [ ] **Step 3: Add fixture tests first**

Create `source_corpus.rs` with tests for:

```rust
#[test]
fn load_index_accepts_txt_path_and_indexed_path() {
    let json = serde_json::json!({
        "works": [
            {"id": "w1", "txt_path": "cards/1/files/1.txt"},
            {"id": "w2", "indexed_path": "cards/2/files/2.zip::2.txt"}
        ]
    });
    let path = temp_file("source-index.json");
    std::fs::write(&path, serde_json::to_vec(&json).unwrap()).unwrap();

    let index = load_index(&path).unwrap();
    assert_eq!(index["w1"], "cards/1/files/1.txt");
    assert_eq!(index["w2"], "cards/2/files/2.zip::2.txt");
}

#[test]
fn read_source_work_decodes_with_canonical_decoder() {
    let root = temp_dir("source-work");
    let source = root.join("cards/1/files");
    std::fs::create_dir_all(&source).unwrap();
    std::fs::write(source.join("1.txt"), b"\xef\xbb\xbfabc").unwrap();

    let work = read_source_work(&root, "w1", "cards/1/files/1.txt").unwrap();
    assert_eq!(work.work_id, "w1");
    assert_eq!(work.decoded.text, "abc");
    assert_eq!(work.decoded.encoding, "utf-8-bom");
}
```

- [ ] **Step 4: Implement the reader**

Move the index parsing and `read_indexed_source_bytes` logic from `crates/ab-coverage/src/prevalence.rs` into `source_corpus.rs`. Keep zip support for `archive.zip::entry.txt`. Call `ab_encoding::decode_source_bytes(&bytes)` in `read_source_work`.

- [ ] **Step 5: Refactor prevalence**

Update `crates/ab-coverage/src/prevalence.rs` to use `source_corpus::read_source_work` and remove its local `decode_source` and `read_indexed_source_bytes` copies.

- [ ] **Step 6: Verify**

Run:

```bash
cargo test -p ab-coverage source_corpus -- --nocapture
cargo test -p ab-coverage prevalence::decode_source_tests -- --nocapture
```

Expected: source corpus tests pass; the old prevalence decode test module no longer exists or has been replaced by source-corpus tests.

- [ ] **Step 7: Commit**

```bash
git add crates/ab-coverage/Cargo.toml crates/ab-coverage/src/lib.rs crates/ab-coverage/src/source_corpus.rs crates/ab-coverage/src/prevalence.rs
git commit -m "refactor: share source corpus reader"
```

---

### Task 3: Source Inventory Classifier

**Files:**
- Create: `crates/ab-coverage/src/source_inventory.rs`
- Modify: `crates/ab-coverage/src/lib.rs`

**Interfaces:**
- Produces:
  - `pub struct SourceInventoryPattern { pub row_id: String, pub source_patterns: Vec<String> }`
  - `pub struct SourceInventorySummary`
  - `pub struct MarkerClassSummary`
  - `pub struct UnknownMarkerExample`
  - `pub fn patterns_from_rows(rows: &[Row]) -> Vec<SourceInventoryPattern>`
  - `pub fn inventory_document(work_id: &str, text: &str, patterns: &[SourceInventoryPattern]) -> SourceInventorySummary`
- Consumes: `ab_source_syntax::source_markers`, `crate::matrix::Row`.

- [ ] **Step 1: Export the module**

In `crates/ab-coverage/src/lib.rs`, add:

```rust
pub mod source_inventory;
```

- [ ] **Step 2: Add classifier tests**

Create `crates/ab-coverage/src/source_inventory.rs` with these tests:

```rust
#[test]
fn classifies_known_rows_from_source_patterns() {
    let patterns = vec![
        pattern("layout.yokogumi", vec![r"［＃ここから横組み］"]),
        pattern("gaiji.marker", vec![r"※［＃[^］]+］"]),
    ];
    let summary = inventory_document(
        "w1",
        "※［＃「口＋世」、U+546D］\n［＃ここから横組み］",
        &patterns,
    );

    assert_eq!(summary.markers_total, 2);
    assert_eq!(summary.row_counts["gaiji.marker"].occurrences, 1);
    assert_eq!(summary.row_counts["layout.yokogumi"].occurrences, 1);
    assert!(summary.unknown_examples.is_empty());
}

#[test]
fn unknown_command_is_reported_with_raw_body_and_span() {
    let patterns = vec![pattern("layout.yokogumi", vec![r"［＃ここから横組み］"])];
    let summary = inventory_document("w1", "［＃謎の注記］", &patterns);

    assert_eq!(summary.markers_total, 1);
    assert_eq!(summary.unknown_examples.len(), 1);
    assert_eq!(summary.unknown_examples[0].work_id, "w1");
    assert_eq!(summary.unknown_examples[0].raw, "［＃謎の注記］");
    assert_eq!(summary.unknown_examples[0].body, "謎の注記");
}

#[test]
fn editorial_and_segment_markers_are_inventory_visible() {
    let summary = inventory_document(
        "w1",
        "［＃「おもて」のルビは「うら」］\n底本：fixture",
        &[],
    );

    assert_eq!(summary.markers_total, 2);
    assert_eq!(summary.unknown_examples.len(), 2);
    assert!(summary.unknown_examples.iter().any(|example| {
        example.kind == "EditorialNoteRubyCorrection"
            && example.raw == "［＃「おもて」のルビは「うら」］"
    }));
    assert!(summary.unknown_examples.iter().any(|example| {
        example.kind == "SegmentBoundaryTerminalProvenance" && example.raw == "底本：fixture"
    }));
}

fn pattern(row_id: &str, source_patterns: Vec<&str>) -> SourceInventoryPattern {
    SourceInventoryPattern {
        row_id: row_id.to_owned(),
        source_patterns: source_patterns.into_iter().map(str::to_owned).collect(),
    }
}
```

- [ ] **Step 3: Implement classification**

Rules:

- For each marker from `source_markers(text)`, first try every row's `source_patterns` regex against `marker.raw`.
- If no row matches and marker kind is `RubyExplicit` or `RubyImplicit`, count row `ruby.basic` when present.
- If no row matches and marker kind is `GaijiFullwidth` or `GaijiAscii`, count row `gaiji.marker` when present.
- Editorial-note and segment-boundary markers are inventory-visible. They should be classified by a matching source pattern or reported as unknown for allowlist/review.
- If no row matches, append an `UnknownMarkerExample` with `work_id`, `line`, `raw`, `body`, and `kind`.
- A marker may increment multiple rows when multiple source patterns intentionally overlap; record `matched_row_ids` in per-marker debug output but summarize counts per row.

- [ ] **Step 4: Verify**

Run:

```bash
cargo test -p ab-coverage source_inventory -- --nocapture
```

Expected: all new classifier tests pass.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-coverage/src/lib.rs crates/ab-coverage/src/source_inventory.rs
git commit -m "feat: classify source marker inventory"
```

---

### Task 4: Source Inventory CLI, Schema, and Smoke

**Files:**
- Modify: `crates/ab-coverage/Cargo.toml`
- Create: `crates/ab-coverage/src/bin/source_inventory.rs`
- Create: `data/aozora-source-inventory.schema.json`
- Create: `data/aozora-source-inventory-allowlist.toml`
- Create: `data/aozora-source-inventory-allowlist.schema.json`
- Create: `tests/source-inventory-smoke.sh`
- Modify: `justfile`
- Modify: `flake.nix`

**Interfaces:**
- Produces CLI:
  - `cargo run -p ab-coverage --bin ab-source-inventory -- --matrix ... --index ... --corpus ... --allowlist ... --output-json ... --report-md ... --unknown-workset ... --fail-on-unknown`

- [ ] **Step 1: Add the bin target**

In `crates/ab-coverage/Cargo.toml`:

```toml
[[bin]]
name = "ab-source-inventory"
path = "src/bin/source_inventory.rs"
```

- [ ] **Step 2: Write CLI fixture smoke**

Create `tests/source-inventory-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${TMPDIR:-/tmp}/ab-validator-source-inventory-smoke"
rm -rf "$out_dir"
mkdir -p "$out_dir/corpus/cards/000001/files" "$out_dir/corpus/cards/000002/files"

cat > "$out_dir/corpus/cards/000001/files/1.txt" <<'TXT'
吾輩《わがはい》
※［＃「口＋世」、U+546D］
［＃ここから横組み］
TXT

cat > "$out_dir/corpus/cards/000002/files/2.txt" <<'TXT'
［＃未知の注記］
TXT

cat > "$out_dir/index.json" <<'JSON'
{
  "works": [
    {"id": "000001_1", "txt_path": "cards/000001/files/1.txt"},
    {"id": "000002_2", "txt_path": "cards/000002/files/2.txt"}
  ]
}
JSON

cargo run -p ab-coverage --bin ab-source-inventory -- \
  --matrix "$repo_root/data/aozora-syntax-coverage.toml" \
  --index "$out_dir/index.json" \
  --corpus "$out_dir/corpus" \
  --output-json "$out_dir/source-inventory.json" \
  --report-md "$out_dir/source-inventory.md" \
  --unknown-workset "$out_dir/unknown-workset.json"

jq -e '.works_scanned == 2' "$out_dir/source-inventory.json"
jq -e '.markers_total == 4' "$out_dir/source-inventory.json"
jq -e '.unknown_markers_total == 1' "$out_dir/source-inventory.json"
jq -e '.unallowlisted_unknown_markers_total == 1' "$out_dir/source-inventory.json"
jq -e '.allowlisted_unknown_markers_total == 0' "$out_dir/source-inventory.json"
jq -e 'any(.unknown_examples[]; .raw == "［＃未知の注記］")' "$out_dir/source-inventory.json"
jq -e '.rows["ruby.basic"].occurrences >= 1' "$out_dir/source-inventory.json"
jq -e '.rows["gaiji.marker"].occurrences >= 1' "$out_dir/source-inventory.json"
jq -e '.rows["layout.yokogumi"].occurrences >= 1' "$out_dir/source-inventory.json"
jq -e '. == ["000002_2"]' "$out_dir/unknown-workset.json"
rg -n "Unknown Source Markers|未知の注記" "$out_dir/source-inventory.md"

if cargo run -p ab-coverage --bin ab-source-inventory -- \
  --matrix "$repo_root/data/aozora-syntax-coverage.toml" \
  --index "$out_dir/index.json" \
  --corpus "$out_dir/corpus" \
  --output-json "$out_dir/fail.json" \
  --report-md "$out_dir/fail.md" \
  --fail-on-unknown
then
  echo "expected --fail-on-unknown to fail" >&2
  exit 1
fi
```

Make it executable.

- [ ] **Step 3: Implement CLI output**

`source_inventory.rs` must:

- load matrix rows with `CoverageMatrix::from_toml`;
- load allowlist rows from `--allowlist` when provided;
- load index with `source_corpus::load_index`;
- scan all works or a JSON `--work-ids` array;
- read each source with `read_source_work`;
- aggregate row occurrence counts, works-with-row counts, top sample works, unknown examples, allowlisted unknown counts, decode failures, and total markers;
- write JSON matching `data/aozora-source-inventory.schema.json`;
- write Markdown with sections: Summary, Rows, Unknown Source Markers, Decode Failures, Inputs;
- write `--unknown-workset` as a JSON array of work IDs with at least one unknown marker;
- exit non-zero when `--fail-on-unknown` and unallowlisted unknown count is non-zero.

- [ ] **Step 4: Add JSON schema**

Create `data/aozora-source-inventory.schema.json` with required top-level fields:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://example.com/ab-validator/aozora-source-inventory.schema.json",
  "type": "object",
  "required": [
    "works_scanned",
    "works_failed",
    "markers_total",
    "unknown_markers_total",
    "unallowlisted_unknown_markers_total",
    "allowlisted_unknown_markers_total",
    "rows",
    "unknown_examples",
    "inputs"
  ],
  "properties": {
    "works_scanned": { "type": "integer", "minimum": 0 },
    "works_failed": { "type": "integer", "minimum": 0 },
    "markers_total": { "type": "integer", "minimum": 0 },
    "unknown_markers_total": { "type": "integer", "minimum": 0 },
    "unallowlisted_unknown_markers_total": { "type": "integer", "minimum": 0 },
    "allowlisted_unknown_markers_total": { "type": "integer", "minimum": 0 },
    "rows": {
      "type": "object",
      "additionalProperties": {
        "type": "object",
        "required": ["works_with_marker", "occurrences", "sample_works"],
        "properties": {
          "works_with_marker": { "type": "integer", "minimum": 0 },
          "occurrences": { "type": "integer", "minimum": 0 },
          "sample_works": {
            "type": "array",
            "items": { "type": "string" },
            "maxItems": 5
          }
        },
        "additionalProperties": false
      }
    },
    "unknown_examples": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["work_id", "line", "kind", "raw", "body"],
        "properties": {
          "work_id": { "type": "string" },
          "line": { "type": "integer", "minimum": 1 },
          "kind": { "type": "string" },
          "raw": { "type": "string" },
          "body": { "type": "string" }
        },
        "additionalProperties": false
      }
    },
    "inputs": { "type": "object" }
  },
  "additionalProperties": true
}
```

- [ ] **Step 5: Add allowlist format**

Create `data/aozora-source-inventory-allowlist.toml`:

```toml
# Reviewed source markers that are inventory-visible but do not block the v1
# representability gate. Each entry must match by kind plus either body_pattern
# or raw_pattern. Use this only for out-of-body, malformed corpus noise, or
# intentionally unsupported-v1 markers.

[[allow]]
id = "terminal-provenance-note"
kind = "SegmentBoundaryTerminalProvenance"
body_pattern = "^底本[：:]"
scope = "out_of_body"
reason = "Terminal bibliography/provenance metadata is not body AAT."
evidence = "docs/aat-contract.md"
```

Create `data/aozora-source-inventory-allowlist.schema.json`:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://example.com/ab-validator/aozora-source-inventory-allowlist.schema.json",
  "type": "object",
  "required": ["allow"],
  "properties": {
    "allow": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["id", "kind", "scope", "reason", "evidence"],
        "properties": {
          "id": { "type": "string", "minLength": 1 },
          "kind": { "type": "string", "minLength": 1 },
          "body_pattern": { "type": "string" },
          "raw_pattern": { "type": "string" },
          "scope": {
            "type": "string",
            "enum": ["out_of_body", "malformed_noise", "unsupported_v1"]
          },
          "reason": { "type": "string", "minLength": 1 },
          "evidence": { "type": "string", "minLength": 1 }
        },
        "anyOf": [
          { "required": ["body_pattern"] },
          { "required": ["raw_pattern"] }
        ],
        "additionalProperties": false
      }
    }
  },
  "additionalProperties": false
}
```

Matching rule: an unknown marker is allowlisted only when `kind` is equal and at least one configured regex (`body_pattern` against `body`, `raw_pattern` against `raw`) matches. Invalid regexes are CLI errors.

- [ ] **Step 6: Wire just and flake**

Add just targets:

```just
source-inventory-smoke:
    @bash "{{repo_root}}/tests/source-inventory-smoke.sh"

source-inventory-full JOBS="24" INDEX="" CORPUS="":
    @cargo run -p ab-coverage --bin ab-source-inventory -- \
        --matrix "{{repo_root}}/data/aozora-syntax-coverage.toml" \
        --index "{{INDEX}}" \
        --corpus "{{CORPUS}}" \
        --allowlist "{{repo_root}}/data/aozora-source-inventory-allowlist.toml" \
        --output-json "{{repo_root}}/docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json" \
        --report-md "{{repo_root}}/docs/superpowers/reports/2026-07-04-source-authority-representability.md" \
        --unknown-workset "{{ab_db_root}}/source-inventory/unknown-workset.json"
```

Add a flake check that runs only `tests/source-inventory-smoke.sh`.

- [ ] **Step 7: Verify**

Run:

```bash
bash tests/source-inventory-smoke.sh
just source-inventory-smoke
cargo test -p ab-coverage
git diff --check
```

Expected: smoke passes, `--fail-on-unknown` failure path is exercised, tests pass.

- [ ] **Step 8: Commit**

```bash
git add crates/ab-coverage/Cargo.toml crates/ab-coverage/src/bin/source_inventory.rs data/aozora-source-inventory.schema.json data/aozora-source-inventory-allowlist.toml data/aozora-source-inventory-allowlist.schema.json tests/source-inventory-smoke.sh justfile flake.nix
git commit -m "feat: add source inventory gate"
```

---

### Task 5: Representability Matrix Contract

**Files:**
- Modify: `data/aozora-syntax-coverage.schema.json`
- Modify: `crates/ab-coverage/src/matrix.rs`
- Modify: `crates/ab-coverage/src/schema.rs`
- Modify: `crates/ab-coverage/tests/schema_matrix.rs`
- Modify: `data/aozora-syntax-coverage.toml`

**Interfaces:**
- Produces a new optional per-row table:

```toml
[syntax.representability]
source_inventory_row = "ruby.basic"
status = "typed"
aat_nodes = ["ruby"]
raw_fallback = true
evidence = "docs/aat-contract.md:119"
notes = "AAT has typed ruby; malformed or unsupported variants can still be preserved as raw."
```

Status enum:

- `typed`
- `raw_preserved`
- `out_of_body`
- `unsupported`
- `needs_research`

- [ ] **Step 1: Add schema test**

In `crates/ab-coverage/tests/schema_matrix.rs`, add a test fixture row with `representability` and assert it parses.

- [ ] **Step 2: Extend Rust matrix types**

In `crates/ab-coverage/src/matrix.rs`, add:

```rust
#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct RepresentabilityCell {
    pub source_inventory_row: String,
    pub status: RepresentabilityStatus,
    #[serde(default)]
    pub aat_nodes: Vec<String>,
    pub raw_fallback: bool,
    #[serde(default)]
    pub evidence: String,
    #[serde(default)]
    pub notes: String,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum RepresentabilityStatus {
    Typed,
    RawPreserved,
    OutOfBody,
    Unsupported,
    NeedsResearch,
}
```

Add to `Row`:

```rust
#[serde(default)]
pub representability: Option<RepresentabilityCell>,
```

- [ ] **Step 3: Extend JSON schema**

Add `representability` to `SyntaxRow.properties` in `data/aozora-syntax-coverage.schema.json`, with required fields `source_inventory_row`, `status`, `raw_fallback`.

- [ ] **Step 4: Validate the contract**

In `crates/ab-coverage/src/schema.rs`, add validation:

- if `status = "typed"`, `aat_nodes` must be non-empty;
- if `status = "unsupported"`, `raw_fallback` must be false;
- `source_inventory_row` must be non-empty and must name an existing matrix row id;
- matrix validation must not use `corpus_prevalence.total_occurrences` to decide whether representability is required, because `corpus_prevalence` is parser/adapter-detector evidence, not source-authority evidence;
- strict source-count enforcement belongs to `ab-source-inventory --strict-representability` in Task 8, where the CLI has the actual source-inventory occurrence counts.

- [ ] **Step 5: Populate priority-1 rows**

Update `data/aozora-syntax-coverage.toml` for priority-1 rows:

- `ruby.basic`: `typed`, raw fallback true.
- `gaiji.marker`: `typed`, raw fallback true.
- `gaiji_ruby.inline_base`: `typed`, raw fallback true.
- `heading.basic`: `typed`, raw fallback true.
- `caption.inline`: `typed`, raw fallback true.
- `figure.image.inline`: `typed`, raw fallback true.
- `warichu.basic` / `warigaki.parenthetical`: `typed` if AAT warigaki rows are recoverable, otherwise `raw_preserved`.
- Any priority-1 row not settled in this task: `needs_research`, raw fallback true, with evidence pointing to this plan and a note explaining the remaining decision.

- [ ] **Step 6: Verify**

Run:

```bash
cargo test -p ab-coverage schema_matrix -- --nocapture
cargo test -p ab-coverage
bash tests/adapter-fidelity-preflight.sh
```

Expected: schema parser accepts representability cells; matrix validation checks representability shape but does not infer source occurrence requirements from parser-detector prevalence.

- [ ] **Step 7: Commit**

```bash
git add data/aozora-syntax-coverage.schema.json data/aozora-syntax-coverage.toml crates/ab-coverage/src/matrix.rs crates/ab-coverage/src/schema.rs crates/ab-coverage/tests/schema_matrix.rs
git commit -m "feat: add source representability matrix"
```

---

### Task 6: AAT Raw Preservation Contract

**Files:**
- Modify: `docs/aat-contract.md`
- Modify: `data/aat-schema.json`
- Modify: `tests/adapter-fidelity-smoke.sh` or add a focused schema smoke if existing smoke is too broad.

**Interfaces:**
- Produces a documented invariant:

> Every body source marker that is not represented as a typed AAT node must be represented as an AAT `raw` node with the original marker text, provenance, and source span when the adapter has access to source bytes.

- [ ] **Step 1: Add contract text**

In `docs/aat-contract.md`, add a section after the inline node table:

```markdown
### Source Marker Preservation

AAT v1 has two levels of source representability:

1. Typed representation: a source marker maps to a semantic AAT node such as
   `ruby`, `gaiji`, `figure`, `style`, `tcy`, `keigakomi`, or `warigaki`.
2. Raw preservation: a source marker that cannot be typed must be represented
   as `raw` with the original marker text in `source`, `x-provenance =
   "source-derived"` when recovered from source text, and a source span when the
   adapter has source bytes.

Parser agreement is not sufficient evidence for AAT representability. The
source-inventory gate over raw Aozora text is the authority.
```

- [ ] **Step 2: Tighten raw node schema**

In `data/aat-schema.json`, keep `raw.source` required and add optional extension fields under `raw.properties`:

```json
"x-provenance": {
  "type": "string",
  "enum": ["parser-derived", "source-derived", "adapter-derived"]
},
"x-source-marker-kind": { "type": "string" }
```

Do not require these fields yet; adapters will be hardened incrementally.

- [ ] **Step 3: Add schema fixture**

Create or update a fixture that validates:

```json
{
  "kind": "raw",
  "source": "［＃未知の注記］",
  "x-provenance": "source-derived",
  "x-source-marker-kind": "CommandFullwidth",
  "span": {
    "line_start": 1,
    "line_end": 1,
    "byte_start": 0,
    "byte_end": 24
  }
}
```

- [ ] **Step 4: Verify**

Run:

```bash
bash tests/adapter-fidelity-smoke.sh
cargo test -p ab-check
git diff --check
```

Expected: schema validation still accepts existing AAT and accepts the explicit raw preservation fixture.

- [ ] **Step 5: Commit**

```bash
git add docs/aat-contract.md data/aat-schema.json tests
git commit -m "docs: define source marker raw preservation"
```

---

### Task 7: Operator Full-Corpus Source Inventory Run and Decision Report

**Files:**
- Create: `docs/superpowers/reports/2026-07-04-source-authority-representability.md`
- Create: `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`
- Modify: `docs/superpowers/reports/2026-07-04-post-parser-ir-conversion-sync.md`

**Interfaces:**
- Consumes: `just source-inventory-full`.
- Produces: durable source-authority evidence and worksets for unknown marker review.
- Classification: operator measurement task. It is intentionally not a flake check because it reads the local Aozora corpus and may write `/db` worksets.

- [ ] **Step 1: Run source inventory over the full local corpus**

Use the current ab-index output. The expected index schema is a JSON object with `.works[]`; each work object must have string `id` and one of `txt_path`, `indexed_path`, or `source_path`. Zip selectors use the existing `archive.zip::entry.txt` form.

If no durable index exists, first run the existing index target used by the AAT corpus pipeline, then record the exact index path, corpus root, work count, and command in the report.

Run:

```bash
just source-inventory-full 24 /path/to/index.json /path/to/aozorabunko
```

Expected:

- JSON summary is written to `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`.
- Markdown report is written to `docs/superpowers/reports/2026-07-04-source-authority-representability.md`.
- Unknown marker workset is written under `/db/ab-validator/source-inventory/unknown-workset.json`.
- The target runs with `--strict-representability`: it exits nonzero after writing outputs while unallowlisted unknown markers, reached `needs_research` rows, or reached rows without representability remain.

- [ ] **Step 2: Review unknown marker classes**

Open the Markdown report. For each unknown marker class:

- classify it into an existing matrix row if a source pattern is missing;
- add a new matrix row if it is a real Aozora feature not represented today;
- add an allowlist entry only if it is out-of-body, malformed corpus noise, or intentionally unsupported.

- [ ] **Step 3: Re-run until unknown classes are reviewed**

Run:

```bash
just source-inventory-full 24 /path/to/index.json /path/to/aozorabunko
```

Expected: unknown markers are either zero or match reviewed allowlist entries, reached source-inventory rows have non-`needs_research` representability, and the report says `SOURCE_AUTHORITY_GATE_PASS`. Until then the report must say `SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED`.

- [ ] **Step 4: Update post-parser-IR sync report**

In `docs/superpowers/reports/2026-07-04-post-parser-ir-conversion-sync.md`, add:

```markdown
## Source Authority Caveat

Parser-IR conversion evidence over four adapters is not a proof that AAT can
represent the Aozora source language. The source-authority inventory at
`docs/superpowers/reports/2026-07-04-source-authority-representability.md`
is the representability gate, and the current run is failing:
`SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED`. Parser evidence remains
triangulation only until the source inventory has no unallowlisted unknown
markers and no reached `needs_research` representability rows.
```

- [ ] **Step 5: Verify**

Run:

```bash
bash tests/source-inventory-smoke.sh
cargo test -p ab-source-syntax
cargo test -p ab-coverage
git diff --check
```

- [ ] **Step 6: Commit**

```bash
git add docs/superpowers/reports/2026-07-04-source-authority-representability.md docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json docs/superpowers/reports/2026-07-04-post-parser-ir-conversion-sync.md data/aozora-syntax-coverage.toml data/aozora-source-inventory-allowlist.toml
git commit -m "docs: add source-authority representability evidence"
```

---

### Task 8: Strict Gate for Durable Claims

**Files:**
- Modify: `tests/source-inventory-smoke.sh`
- Create: `tests/source-representability-gate-smoke.sh`
- Modify: `justfile`
- Modify: `flake.nix`
- Modify: `crates/ab-coverage/src/bin/source_inventory.rs`
- Modify: `data/aozora-source-inventory-allowlist.toml`

**Interfaces:**
- Produces:
  - `just source-representability-gate-smoke`
  - flake check `checks.<system>.source-representability-gate`

- [ ] **Step 1: Add strict gate mode**

Extend `ab-source-inventory`:

```text
--strict-representability
```

When set, the CLI exits non-zero if:

- `unallowlisted_unknown_markers_total` is non-zero;
- any source inventory row in the current run has `occurrences > 0` and the corresponding matrix row has no `representability` table;
- any source inventory row in the current run has `occurrences > 0` and the corresponding matrix row has `representability.status = "needs_research"`;
- any matrix row reached by current source inventory counts has `representability.status = "typed"` but no `aat_nodes`;
- raw-preserved rows are not counted separately in the report.

The strict gate must use the source inventory summary produced by this CLI invocation. It must not use `corpus_prevalence.total_occurrences`, because that field is parser-detector prevalence and can miss features that parsers drop.

- [ ] **Step 2: Add smoke**

Create `tests/source-representability-gate-smoke.sh` with a fixture corpus containing:

- one typed ruby marker;
- one raw-preserved unknown marker that is allowlisted by kind + body regex;
- one unallowlisted unknown marker in a second run that must fail.

Assertions:

```bash
jq -e '.representability.typed_occurrences >= 1' "$out_dir/pass.json"
jq -e '.representability.raw_preserved_occurrences >= 1' "$out_dir/pass.json"
jq -e '.allowlisted_unknown_markers_total >= 1' "$out_dir/pass.json"
jq -e '.unallowlisted_unknown_markers_total == 0' "$out_dir/pass.json"
```

The pass fixture must write an allowlist TOML containing:

```toml
[[allow]]
id = "fixture-raw-preserved"
kind = "CommandFullwidth"
body_pattern = "^fixture raw preserved$"
scope = "unsupported_v1"
reason = "Fixture marker exercises raw-preserved strict gate accounting."
evidence = "tests/source-representability-gate-smoke.sh"
```

- [ ] **Step 3: Wire just and flake**

Add:

```just
source-representability-gate-smoke:
    @bash "{{repo_root}}/tests/source-representability-gate-smoke.sh"
```

Add the same smoke as a flake check.

- [ ] **Step 4: Verify**

Run:

```bash
bash tests/source-representability-gate-smoke.sh
just source-representability-gate-smoke
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).source-representability-gate --print-build-logs
```

- [ ] **Step 5: Commit**

```bash
git add crates/ab-coverage/src/bin/source_inventory.rs tests/source-representability-gate-smoke.sh justfile flake.nix data/aozora-source-inventory-allowlist.toml
git commit -m "test: gate source representability claims"
```

---

## Blindspot Controls

This plan handles the known blindspots as follows:

| Blindspot | Control |
|---|---|
| All parsers drop the same feature | Source inventory scans raw source, not parser output. |
| Unknown free-text `［＃...］` spellings | Unknown marker classes fail strict mode until classified or allowlisted. |
| Parser DNF/timeouts skew evidence | Source inventory does not run parsers; parser DNF remains adapter-performance evidence only. |
| Visible-text success hides structural loss | Representability separates `typed` from `raw_preserved` and reports both counts. |
| Raw escape hatch hides semantic gaps | Raw-preserved counts are first-class report metrics and cannot be called typed support. |
| Body/out-of-body confusion | Inventory report records inputs and scan scope; out-of-body markers require explicit representability status. |
| Corpus drift | Full report records index path, corpus path, work count, and source hashes where available. |
| Encoding drift | Source reader uses `ab-encoding::decode_source_bytes`. |
| Matrix drift | Source-inventory smokes and strict representability gate validate row links and required statuses. |

## Self-Review

- Spec coverage: The plan makes source text authoritative, preserves parser evidence as support only, adds unknown-marker gates, adds representability statuses, and reports blindspots explicitly.
- Red-flag scan: No task uses defer-the-work language. The plan does contain implementation choices that require coding, but each task defines interfaces, files, tests, and verification commands.
- Type consistency: Task 3 uses `SourceInventoryPattern` instead of constructing future `Row` fields, and Task 5 introduces `representability` before strict representability checks consume it.
