# AAT Morph Runner Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a small AAT-first morph runner that converts checked AAT JSON into plaintext documents, runs selected morph analyzers, and writes analysis/comparison JSONL.

**Architecture:** `ab-plaintext` gains AAT visible-text projection and `PlainTextDocument` construction. A new binary crate `ab-morph-run` reads AAT files, resolves requested analyzers through `ab-morph-analyzers`, serializes `Analysis` rows, and optionally serializes `Comparison` rows using `ab-morph-diff`.

**Tech Stack:** Rust edition 2024, `serde`, `serde_json`, `clap`, `anyhow`, `ab-plaintext`, `ab-morph-analyzers`, `ab-morph-diff`, checked AAT JSON from `ab-check --aat-output`.

---

## File Structure

```text
Cargo.toml
Cargo.lock
crates/ab-plaintext/src/lib.rs
crates/ab-plaintext/src/aat.rs
crates/ab-morph-run/Cargo.toml
crates/ab-morph-run/src/lib.rs
crates/ab-morph-run/src/main.rs
```

---

### Task 1: Add AAT Projection to `ab-plaintext`

**Files:**
- Modify: `crates/ab-plaintext/Cargo.toml`
- Modify: `crates/ab-plaintext/src/lib.rs`
- Create: `crates/ab-plaintext/src/aat.rs`

- [ ] **Step 1: Add dependency**

Add to `crates/ab-plaintext/Cargo.toml`:

```toml
serde_json.workspace = true
```

- [ ] **Step 2: Extend public API**

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

- [ ] **Step 3: Implement AAT projection**

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

- [ ] **Step 4: Add tests**

Append to `crates/ab-plaintext/src/aat.rs`:

```rust
#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    #[test]
    fn builds_document_from_work_id_and_visible_text() {
        let aat = json!({
            "work_id": "w1",
            "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}]
        });

        let doc = from_aat_value(&aat).unwrap();
        assert_eq!(doc.text_id, "w1");
        assert_eq!(doc.source_format, SourceFormat::AatVisibleText);
        assert_eq!(doc.text, "本文");
    }

    #[test]
    fn projects_nested_visible_text() {
        let aat = json!({
            "work_id": "w2",
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "A"},
                    {"kind": "ruby", "base": "B", "reading": "ビー"},
                    {"kind": "gaiji", "description": "desc", "resolved": "C"},
                    {"kind": "gaiji", "description": "unresolved", "resolved": ""},
                    {"kind": "raw", "source": "D"},
                    {"kind": "warigaki", "upper": [{"kind": "text", "value": "E"}], "lower": [{"kind": "text", "value": "F"}]},
                    {"kind": "style", "content": [{"kind": "text", "value": "G"}]}
                ]
            }]
        });

        assert_eq!(visible_text_projection(&aat), "ABCDEFG");
    }

    #[test]
    fn missing_work_id_is_error() {
        let aat = json!({"blocks": []});
        assert_eq!(from_aat_value(&aat), Err(PlainTextError::MissingAatWorkId));
    }
}
```

- [ ] **Step 5: Verify and commit**

Run:

```bash
cargo test -p ab-plaintext
```

Commit:

```bash
git add crates/ab-plaintext Cargo.toml Cargo.lock
git commit -m "feat: add AAT plaintext projection"
```

---

### Task 2: Add `ab-morph-run` Skeleton

**Files:**
- Modify: `Cargo.toml`
- Create: `crates/ab-morph-run/Cargo.toml`
- Create: `crates/ab-morph-run/src/lib.rs`
- Create: `crates/ab-morph-run/src/main.rs`

- [ ] **Step 1: Register crate**

Add `"crates/ab-morph-run"` to workspace members and this dependency to `[workspace.dependencies]`:

```toml
ab-morph-run = { path = "crates/ab-morph-run" }
```

- [ ] **Step 2: Create manifest**

Create `crates/ab-morph-run/Cargo.toml`:

```toml
[package]
name = "ab-morph-run"
version.workspace = true
edition.workspace = true
license.workspace = true

[dependencies]
ab-morph-analyzers.workspace = true
ab-morph-diff.workspace = true
ab-plaintext.workspace = true
anyhow.workspace = true
clap.workspace = true
serde.workspace = true
serde_json.workspace = true
```

- [ ] **Step 3: Add CLI wrapper**

Create `crates/ab-morph-run/src/main.rs`:

```rust
use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, Subcommand};

#[derive(Debug, Parser)]
#[command(version, about = "Run morph analyzers over checked AAT JSON")]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    AnalyzeAat {
        #[arg(long, conflicts_with = "aat_dir")]
        aat: Option<PathBuf>,
        #[arg(long, conflicts_with = "aat")]
        aat_dir: Option<PathBuf>,
        #[arg(long, required = true)]
        analyzer: Vec<String>,
        #[arg(long)]
        analyses_output: PathBuf,
        #[arg(long)]
        comparisons_output: Option<PathBuf>,
    },
}

fn main() -> Result<()> {
    let args = Args::parse();
    match args.command {
        Command::AnalyzeAat {
            aat,
            aat_dir,
            analyzer,
            analyses_output,
            comparisons_output,
        } => ab_morph_run::run_analyze_aat(
            aat.as_deref(),
            aat_dir.as_deref(),
            &analyzer,
            &analyses_output,
            comparisons_output.as_deref(),
        ),
    }
}
```

- [ ] **Step 4: Add placeholder library with argument validation**

Create `crates/ab-morph-run/src/lib.rs`:

```rust
use std::path::Path;

use anyhow::{Result, bail};

pub fn run_analyze_aat(
    aat: Option<&Path>,
    aat_dir: Option<&Path>,
    analyzer_ids: &[String],
    _analyses_output: &Path,
    _comparisons_output: Option<&Path>,
) -> Result<()> {
    if aat.is_none() == aat_dir.is_none() {
        bail!("provide exactly one of --aat or --aat-dir");
    }
    if analyzer_ids.is_empty() {
        bail!("provide at least one --analyzer");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;

    #[test]
    fn rejects_missing_input() {
        let err = run_analyze_aat(None, None, &["vibrato".to_owned()], Path::new("out.jsonl"), None)
            .unwrap_err();
        assert!(err.to_string().contains("exactly one"));
    }
}
```

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

### Task 3: Implement AAT Reading and Analysis JSONL

**Files:**
- Modify: `crates/ab-morph-run/src/lib.rs`

- [ ] **Step 1: Add row types and AAT file discovery**

Add serializable row structs and functions to read either one `--aat` file or all `*.json` files in `--aat-dir`, sorted by path.

Use this row shape:

```rust
#[derive(serde::Serialize)]
struct AnalysisRow {
    work_id: String,
    analyzer: String,
    analysis: ab_morph_diff::Analysis,
}
```

- [ ] **Step 2: Add analyzer resolution**

Implement `AnalyzerSpec` parsing:

```rust
enum AnalyzerSpec {
    Vibrato,
    Sudachi(ab_morph_analyzers::SudachiMode),
}
```

Accepted ids: `vibrato`, `sudachi-a`, `sudachi-b`, `sudachi-c`. Unknown ids return an error containing `unknown analyzer`.

- [ ] **Step 3: Run analyzers and write analysis rows**

For each AAT file:

1. parse JSON as `serde_json::Value`;
2. convert with `ab_plaintext::from_aat_value`;
3. run each analyzer;
4. write one JSON line per analysis.

Use `VibratoAnalyzer::unidic_cwj_default()` for `vibrato`. Use `AB_SUDACHI_DICT` for Sudachi and fail if missing.

- [ ] **Step 4: Add tests without dictionaries**

Add tests for:

- `parse_analyzer_spec("vibrato")` succeeds;
- `parse_analyzer_spec("sudachi-c")` succeeds;
- `parse_analyzer_spec("x")` errors;
- AAT discovery sorts two files by path.

- [ ] **Step 5: Verify and commit**

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

Use this row shape:

```rust
#[derive(serde::Serialize)]
struct ComparisonRow {
    work_id: String,
    from_analyzer: String,
    to_analyzer: String,
    comparison: ab_morph_diff::Comparison,
}
```

- [ ] **Step 2: Emit all requested analyzer pairs**

After collecting analyses for one work, if `comparisons_output` is set, call:

```rust
ab_morph_diff::compare_pair(&analyses[i], &analyses[j], &[])
```

for all `i < j` and write each comparison as one JSONL row.

- [ ] **Step 3: Add unit helper test**

Add a pure helper test that builds two tiny `Analysis` values for source text `今日` and confirms one comparison row is produced for two analyses. This test must not load dictionaries.

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
cargo test -p ab-morph-run
cargo check -p ab-morph-analyzers
```

Expected: all pass.

- [ ] **Step 2: Run optional dictionary-backed smoke command**

Use one checked AAT fixture or create a temp JSON file with a paragraph text node, then run:

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

- AAT-to-plaintext extraction is Task 1.
- AAT-file runner CLI is Task 2.
- Analysis JSONL is Task 3.
- Comparison JSONL is Task 4.
- Validation and Nix Sudachi smoke path are Task 5.

Placeholder scan:

- No task relies on an undefined type name without defining its shape.
- No step says to add generic error handling without specifying the behavior.

Scope check:

- The plan does not invoke adapters or traverse corpus indexes. That stays in `ab-check`.
