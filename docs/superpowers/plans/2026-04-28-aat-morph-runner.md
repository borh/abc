# AAT Morph Runner Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a small AAT-first morph runner that converts checked AAT JSON into plaintext documents, runs selected morph analyzers, and writes analysis/comparison JSONL.

**Architecture:** `ab-plaintext` becomes the single owner of AAT visible-text projection. `ab-check` reuses that projection instead of maintaining a duplicate walker. A new binary crate `ab-morph-run` reads checked AAT files, resolves requested analyzers through `ab-morph-analyzers`, serializes `Analysis` rows, and optionally serializes `Comparison` rows using `ab-morph-diff`.

**Tech Stack:** Rust edition 2024, `serde`, `serde_json`, `clap`, `anyhow`, `ab-plaintext`, `ab-check`, `ab-morph-analyzers`, `ab-morph-diff`, checked AAT JSON from `ab-check --aat-output`.

---

## File Structure

```text
Cargo.toml
Cargo.lock
crates/ab-plaintext/src/lib.rs
crates/ab-plaintext/src/aat.rs
crates/ab-check/Cargo.toml
crates/ab-check/src/aat.rs
crates/ab-morph-run/Cargo.toml
crates/ab-morph-run/src/lib.rs
crates/ab-morph-run/src/main.rs
```

---

### Task 1: Move Shared AAT Projection Into `ab-plaintext`

**Files:**
- Modify: `crates/ab-plaintext/Cargo.toml`
- Modify: `crates/ab-plaintext/src/lib.rs`
- Create: `crates/ab-plaintext/src/aat.rs`
- Modify: `crates/ab-check/Cargo.toml`
- Modify: `crates/ab-check/src/aat.rs`

- [ ] **Step 1: Add dependencies**

Add to `crates/ab-plaintext/Cargo.toml`:

```toml
serde_json.workspace = true
```

Add to `crates/ab-check/Cargo.toml`:

```toml
ab-plaintext.workspace = true
```

- [ ] **Step 2: Extend `ab-plaintext` public API**

Update `crates/ab-plaintext/src/lib.rs`:

```rust
mod aozora;
mod aat;

use std::error::Error;
use std::fmt;

use serde::Serialize;

pub use aozora::from_aozora_honbun_bytes;
pub use aat::{from_aat_value, visible_text_projection};

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PlainTextDocument {
    pub text_id: String,
    pub source_format: SourceFormat,
    pub text: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum SourceFormat {
    AozoraHonbun,
    AatVisibleText,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PlainTextError {
    MissingAatWorkId,
}

impl fmt::Display for PlainTextError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PlainTextError::MissingAatWorkId => write!(f, "AAT is missing string work_id"),
        }
    }
}

impl Error for PlainTextError {}
```

- [ ] **Step 3: Implement shared projection**

Create `crates/ab-plaintext/src/aat.rs`:

```rust
use serde_json::Value;

use crate::{PlainTextDocument, PlainTextError, SourceFormat};

pub fn from_aat_value(aat: &Value) -> Result<PlainTextDocument, PlainTextError> {
    let text_id = aat
        .get("work_id")
        .and_then(Value::as_str)
        .ok_or(PlainTextError::MissingAatWorkId)?
        .to_owned();

    Ok(PlainTextDocument {
        text_id,
        source_format: SourceFormat::AatVisibleText,
        text: visible_text_projection(aat),
    })
}

pub fn visible_text_projection(aat: &Value) -> String {
    let mut out = String::new();
    if let Some(blocks) = aat.get("blocks").and_then(Value::as_array) {
        for block in blocks {
            collect_block(block, &mut out);
        }
    }
    out
}

fn collect_block(node: &Value, out: &mut String) {
    if let Some(content) = node.get("content").and_then(Value::as_array) {
        for inline in content {
            collect_inline(inline, out);
        }
    }
    if let Some(children) = node.get("children").and_then(Value::as_array) {
        for child in children {
            collect_block(child, out);
        }
    }
}

fn collect_inline(node: &Value, out: &mut String) {
    match node.get("kind").and_then(Value::as_str).unwrap_or("") {
        "text" => push_string_field(node, "value", out),
        "ruby" => push_string_field(node, "base", out),
        "gaiji" => {
            if let Some(resolved) = node.get("resolved").and_then(Value::as_str) {
                out.push_str(resolved);
            } else {
                push_string_field(node, "description", out);
            }
        }
        "raw" => push_string_field(node, "source", out),
        "warigaki" => {
            for key in ["upper", "lower"] {
                if let Some(content) = node.get(key).and_then(Value::as_array) {
                    for inline in content {
                        collect_inline(inline, out);
                    }
                }
            }
        }
        _ => {
            if let Some(content) = node.get("content").and_then(Value::as_array) {
                for inline in content {
                    collect_inline(inline, out);
                }
            }
        }
    }
}

fn push_string_field(node: &Value, key: &str, out: &mut String) {
    if let Some(value) = node.get(key).and_then(Value::as_str) {
        out.push_str(value);
    }
}
```

- [ ] **Step 4: Add projection tests**

Append tests to `crates/ab-plaintext/src/aat.rs` covering `work_id`, text/ruby/gaiji/raw/warigaki/style projection, empty resolved gaiji, missing resolved fallback to description, and missing work id.

Use this key assertion for gaiji behavior:

```rust
let aat = serde_json::json!({
    "work_id": "w",
    "blocks": [{"kind": "paragraph", "content": [
        {"kind": "gaiji", "description": "desc", "resolved": ""},
        {"kind": "gaiji", "description": "fallback"}
    ]}]
});
assert_eq!(visible_text_projection(&aat), "fallback");
```

- [ ] **Step 5: Make `ab-check` reuse `ab-plaintext`**

In `crates/ab-check/src/aat.rs`, change the existing `visible_text_projection` function body to delegate:

```rust
pub fn visible_text_projection(aat: &Value) -> String {
    ab_plaintext::visible_text_projection(aat)
}
```

Keep `visible_text_fragments`, `VisibleFragment`, and path/node helper functions in `ab-check` because validation still uses them.

- [ ] **Step 6: Verify and commit**

Run:

```bash
cargo test -p ab-plaintext
cargo test -p ab-check aat::tests::visible_projection_excludes_unresolved_gaiji_descriptions
```

Commit:

```bash
git add crates/ab-plaintext crates/ab-check Cargo.lock
git commit -m "feat: share AAT visible text projection"
```

---

### Task 2: Add `ab-morph-run` Skeleton

**Files:**
- Modify: `Cargo.toml`
- Create: `crates/ab-morph-run/Cargo.toml`
- Create: `crates/ab-morph-run/src/lib.rs`
- Create: `crates/ab-morph-run/src/main.rs`

- [ ] **Step 1: Register crate**

Add `"crates/ab-morph-run"` to workspace members and add this dependency to `[workspace.dependencies]`:

```toml
ab-morph-run = { path = "crates/ab-morph-run" }
```

- [ ] **Step 2: Create manifest**

Create `crates/ab-morph-run/Cargo.toml` with dependencies on `ab-morph-analyzers`, `ab-morph-diff`, `ab-plaintext`, `anyhow`, `clap`, `serde`, and `serde_json` from the workspace.

- [ ] **Step 3: Add CLI wrapper**

Create `crates/ab-morph-run/src/main.rs` with an `analyze-aat` subcommand accepting exactly one of `--aat` or `--aat-dir`, one or more `--analyzer`, required `--analyses-output`, and optional `--comparisons-output`.

- [ ] **Step 4: Add skeleton validation in library**

Create `crates/ab-morph-run/src/lib.rs` with `run_analyze_aat(...)` that rejects missing/both inputs and empty analyzer lists.

- [ ] **Step 5: Verify and commit**

Run:

```bash
cargo test -p ab-morph-run
```

Commit:

```bash
git add Cargo.toml Cargo.lock crates/ab-morph-run
git commit -m "feat: add morph runner CLI skeleton"
```

---

### Task 3: Implement AAT Reading, Analyzer Resolution, and Analysis JSONL

**Files:**
- Modify: `crates/ab-morph-run/src/lib.rs`

- [ ] **Step 1: Discover AAT inputs**

Implement `discover_aat_inputs(aat, aat_dir)`:

- single file returns that file;
- directory returns sorted `*.json` files;
- empty directory returns an error containing `no AAT JSON files`.

- [ ] **Step 2: Parse and dedupe analyzer specs**

Implement accepted CLI ids: `vibrato`, `sudachi-a`, `sudachi-b`, `sudachi-c`.

Deduplicate exact duplicate CLI ids while preserving first occurrence. Unknown ids return an error containing `unknown analyzer`.

- [ ] **Step 3: Resolve analyzers**

Use:

- `VibratoAnalyzer::unidic_cwj_default()` for `vibrato`;
- `SudachiAnalyzer::from_dictionary_path(mode, $AB_SUDACHI_DICT)` for Sudachi ids;
- missing `AB_SUDACHI_DICT` returns an error containing `AB_SUDACHI_DICT`.

- [ ] **Step 4: Write analysis JSONL**

Open `--analyses-output` with create/truncate semantics. For every AAT and analyzer, write:

```rust
#[derive(serde::Serialize)]
struct AnalysisRow {
    text_id: String,
    analyzer: String,
    analysis: ab_morph_diff::Analysis,
}
```

The wrapper duplicates nested fields intentionally for grep-friendly JSONL.

- [ ] **Step 5: Add tests without dictionaries**

Test analyzer parsing, dedupe, unknown analyzer error, empty directory error, and sorted discovery. Do not load real analyzers in unit tests.

- [ ] **Step 6: Verify and commit**

Run:

```bash
cargo test -p ab-morph-run
```

Commit:

```bash
git add crates/ab-morph-run Cargo.lock
git commit -m "feat: write morph analysis JSONL from AAT"
```

---

### Task 4: Add Pairwise Comparison JSONL

**Files:**
- Modify: `crates/ab-morph-run/src/lib.rs`

- [ ] **Step 1: Add comparison rows**

Use create/truncate semantics for `--comparisons-output` when provided. Row shape:

```rust
#[derive(serde::Serialize)]
struct ComparisonRow {
    text_id: String,
    from_analyzer: String,
    to_analyzer: String,
    comparison: ab_morph_diff::Comparison,
}
```

- [ ] **Step 2: Emit deterministic pairs**

For each text, compare analyses in deduplicated CLI order using all `i < j` pairs:

```rust
ab_morph_diff::compare_pair(&analyses[i], &analyses[j], &[])
```

- [ ] **Step 3: Add valid analysis helper test**

Use helpers that construct valid `Analysis` values with `source_text: "今日"`, `surface: "今日"`, `byte_span: 0..6`, and `char_span: 0..2`. Confirm two analyses produce exactly one comparison row. Do not hand-wave span construction.

- [ ] **Step 4: Verify and commit**

Run:

```bash
cargo test -p ab-morph-run
```

Commit:

```bash
git add crates/ab-morph-run Cargo.lock
git commit -m "feat: write morph comparison JSONL"
```

---

### Task 5: Final Verification

**Files:**
- No source changes expected.

- [ ] **Step 1: Run targeted checks**

```bash
cargo fmt --all -- --check
cargo test -p ab-plaintext
cargo test -p ab-check aat
cargo test -p ab-morph-run
cargo check -p ab-morph-analyzers
```

Expected: all pass.

- [ ] **Step 2: Run dictionary-backed smoke command**

Create `/tmp/aat.json`:

```json
{"version":1,"work_id":"smoke","blocks":[{"kind":"paragraph","content":[{"kind":"text","value":"吾輩は猫である。"}]}],"meta":{"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}}
```

Run:

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  cargo run -p ab-morph-run -- analyze-aat \
  --aat /tmp/aat.json \
  --analyzer vibrato \
  --analyzer sudachi-c \
  --analyses-output /tmp/analyses.jsonl \
  --comparisons-output /tmp/comparisons.jsonl
```

Expected: command exits 0 and both output files are non-empty.

## Self-Review

Spec coverage:

- Shared AAT projection ownership is Task 1.
- `ab-check` dedupe/reuse is Task 1.
- AAT-file runner CLI is Task 2.
- Analysis JSONL is Task 3.
- Comparison JSONL is Task 4.
- Validation and Nix Sudachi smoke path are Task 5.

Ambiguity scan:

- Gaiji missing-vs-empty `resolved` behavior is explicit.
- Output files truncate rather than append.
- Duplicate analyzer ids are deduped.
- Empty AAT directories error.
- Comparison tests require valid spans.
