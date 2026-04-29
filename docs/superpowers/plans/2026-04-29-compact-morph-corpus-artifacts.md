# Compact Morph Corpus Artifacts Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make complete corpus-scale morph comparisons practical by writing compact, scan-friendly artifacts instead of persisting full repeated `Analysis` and `Comparison` JSON for every row.

**Architecture:** Keep the existing full JSONL behavior for small/debug runs. Add a compact corpus profile that writes value-oriented summary rows keyed by both `source_id` and `text_id`, plus bounded examples for inspection. Treat zstd as an output transport detail selected by file extension, not as the core storage model.

**Tech Stack:** Rust 2024, `serde`, `serde_json`, existing `zstd` workspace dependency, `ab-morph-run`, `ab-morph-diff`, `ab-plaintext`, `clap`.

**Storage Principle:** Compact row shape is the fix. Zstd is a multiplier on top of compact rows, not a substitute for changing the persisted schema.

---

## Rich Hickey Review

### Simplicity vs ease

The tempting easy fix is to zstd-compress the current full JSONL. That is familiar and cheap, but it preserves the complex shape: every row still entangles source text, full analyzer output, full comparison regions, and summary metrics. The simple fix is to separate facts by purpose: compact summary values for corpus scans, bounded examples for evidence, and full detail only for debug runs.

### Values over places

The corpus artifact should be a set of immutable rows that can be streamed, sorted, grouped, and re-read without hidden state. Do not require a mutable run directory with implicit relationships between files. Every row that leaves `ab-morph-run` must carry the identity needed to interpret it: `source_id`, `text_id`, analyzer ids, and stats.

### Time and identity

`text_id` is not a unique dataset row key. It identifies a logical Aozora work/card-ish entity. The actual dataset row is the AAT file/source record. Add `source_id` as the stable row identity and keep `text_id` as the grouping identity. This makes duplicate logical works explicit rather than accidental.

### Protocols

The compact artifact schema is the protocol. Compression, sharding, and output paths are implementation details. Design summary rows so downstream tools can operate without knowing how `ab-morph-run` internally tokenized, chunked, or parallelized inputs.

### Constraint

For phase 1, do not implement an `explain` command, a database, or arbitrary profile plugins. The useful constraint is two profiles only: `full` and `compact`. `full` preserves current behavior. `compact` writes summaries and bounded examples only.

---

## File Structure

- Modify `crates/ab-morph-run/src/main.rs`: add CLI flags for `--output-profile`, compact output paths, and example budget.
- Modify `crates/ab-morph-run/src/lib.rs`: route full vs compact execution, define compact row structs, derive source ids, write compact rows, make resume source-id aware in compact mode, propagate compact/zstd behavior through `--jobs`, and add zstd-aware writers.
- Create `crates/ab-morph-run/src/output.rs`: small output-writer abstraction for plain and `.zst` paths.
- Create `crates/ab-morph-run/src/compact.rs`: compact row structs and conversion helpers from `Analysis`/`Comparison` to summary/example rows.
- Modify `crates/ab-morph-run/Cargo.toml`: ensure `zstd.workspace = true` is present if not already inherited.
- Create or modify tests in `crates/ab-morph-run/src/lib.rs`, `output.rs`, and `compact.rs` unit-test modules.
- Create `docs/superpowers/reports/2026-04-29-morph-artifact-storage.md`: document the new artifact contract and why compact is the corpus default.

---

### Task 1: Add output writer abstraction with zstd-by-extension

**Files:**
- Create: `crates/ab-morph-run/src/output.rs`
- Modify: `crates/ab-morph-run/src/lib.rs`
- Modify: `crates/ab-morph-run/Cargo.toml`

- [ ] **Step 1: Add the module declaration**

Modify `crates/ab-morph-run/src/lib.rs` near the top:

```rust
mod compact;
mod output;
```

Expected: compile fails because the files do not exist yet.

- [ ] **Step 2: Ensure zstd is available to ab-morph-run**

Modify `crates/ab-morph-run/Cargo.toml` dependencies:

```toml
zstd.workspace = true
```

Expected: no behavior change yet.

- [ ] **Step 3: Write failing tests for plain and zstd output**

Create `crates/ab-morph-run/src/output.rs` with this test module first:

```rust
use std::fs::{self, File, OpenOptions};
use std::io::{self, BufWriter, Write};
use std::path::Path;

use anyhow::{Context, Result};

#[cfg(test)]
mod tests {
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::*;

    #[test]
    fn writes_plain_output_for_jsonl_path() {
        let dir = temp_dir("plain");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("rows.jsonl");

        {
            let mut writer = open_output_writer(&path, false).unwrap();
            writer.write_all(b"one\n").unwrap();
            writer.flush().unwrap();
        }

        assert_eq!(fs::read_to_string(&path).unwrap(), "one\n");
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn writes_zstd_output_for_zst_path() {
        let dir = temp_dir("zst");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("rows.jsonl.zst");

        {
            let mut writer = open_output_writer(&path, false).unwrap();
            writer.write_all(b"one\n").unwrap();
            writer.flush().unwrap();
        }

        let bytes = fs::read(&path).unwrap();
        let decoded = zstd::decode_all(bytes.as_slice()).unwrap();
        assert_eq!(decoded, b"one\n");
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn appends_zstd_output_as_concatenated_frames() {
        let dir = temp_dir("zst-append");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("rows.jsonl.zst");

        {
            let mut writer = open_output_writer(&path, false).unwrap();
            writer.write_all(b"one\n").unwrap();
            writer.flush().unwrap();
        }
        {
            let mut writer = open_output_writer(&path, true).unwrap();
            writer.write_all(b"two\n").unwrap();
            writer.flush().unwrap();
        }

        let bytes = fs::read(&path).unwrap();
        let decoded = zstd::decode_all(bytes.as_slice()).unwrap();
        assert_eq!(decoded, b"one\ntwo\n");
        let _ = fs::remove_dir_all(dir);
    }

    fn temp_dir(label: &str) -> std::path::PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!(
            "ab-morph-run-output-{label}-{}-{unique}",
            std::process::id()
        ))
    }
}
```

- [ ] **Step 4: Run tests and verify they fail**

Run:

```bash
cargo test -p ab-morph-run output::tests::writes_plain_output_for_jsonl_path output::tests::writes_zstd_output_for_zst_path
```

Expected: cargo rejects multiple test filters or compilation fails because `open_output_writer` is missing. If cargo rejects multiple filters, run `cargo test -p ab-morph-run output::tests` and expect compile failure for missing `open_output_writer`.

- [ ] **Step 5: Implement output writer**

Add this above the test module in `crates/ab-morph-run/src/output.rs`:

```rust
pub(crate) fn open_output_writer(path: &Path, append: bool) -> Result<Box<dyn Write + Send>> {
    if let Some(parent) = path.parent().filter(|parent| !parent.as_os_str().is_empty()) {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }

    let file = if append {
        OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .with_context(|| format!("failed to open {}", path.display()))?
    } else {
        File::create(path).with_context(|| format!("failed to create {}", path.display()))?
    };

    if path.extension().and_then(|ext| ext.to_str()) == Some("zst") {
        Ok(Box::new(zstd::stream::write::Encoder::new(file, 3)?.auto_finish()))
    } else {
        Ok(Box::new(BufWriter::new(file)))
    }
}
```

- [ ] **Step 6: Run output tests**

Run:

```bash
cargo test -p ab-morph-run output::tests
```

Expected: all output tests pass, including append-to-zstd as concatenated frames.

- [ ] **Step 7: Commit**

Run:

```bash
git add crates/ab-morph-run/Cargo.toml crates/ab-morph-run/src/lib.rs crates/ab-morph-run/src/output.rs
git commit -m "feat: add compressed morph output writer"
```

---

### Task 2: Add source identity helpers

**Files:**
- Create/modify: `crates/ab-morph-run/src/compact.rs`
- Modify: `crates/ab-morph-run/src/lib.rs`

- [ ] **Step 1: Write failing tests for `source_id` derivation**

Create `crates/ab-morph-run/src/compact.rs` with:

```rust
use std::path::Path;

use serde::Serialize;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_id_uses_aat_file_stem_without_hash_suffix() {
        let path = Path::new("aats/aozora-rs-adapter/000013_542-f2b43aeff7df.json");
        assert_eq!(source_id_from_aat_path(path), "000013_542-f2b43aeff7df");
    }

    #[test]
    fn source_id_preserves_distinct_duplicate_logical_ids() {
        let first = source_id_from_aat_path(Path::new("000013_542-f2b43aeff7df.json"));
        let second = source_id_from_aat_path(Path::new("000013_542-9dc5cea740ac.json"));
        assert_ne!(first, second);
    }
}
```

- [ ] **Step 2: Run tests and verify failure**

Run:

```bash
cargo test -p ab-morph-run compact::tests::source_id
```

Expected: compile fails because `source_id_from_aat_path` is missing.

- [ ] **Step 3: Implement `source_id_from_aat_path`**

Add above tests in `compact.rs`:

```rust
pub(crate) fn source_id_from_aat_path(path: &Path) -> String {
    path.file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("unknown")
        .to_owned()
}
```

This deliberately uses the full AAT file stem, including the path hash suffix. It is a file/record identity, not the logical `text_id`.

- [ ] **Step 4: Run tests**

Run:

```bash
cargo test -p ab-morph-run compact::tests::source_id
```

Expected: both tests pass.

- [ ] **Step 5: Commit**

Run:

```bash
git add crates/ab-morph-run/src/compact.rs crates/ab-morph-run/src/lib.rs
git commit -m "feat: add morph source identity helper"
```

---

### Task 3: Add compact summary row schema

**Files:**
- Modify: `crates/ab-morph-run/src/compact.rs`

- [ ] **Step 1: Write failing test for compact conversion**

Append this test to `compact.rs` tests:

```rust
use ab_morph_diff::{Analysis, Comparison, ComparisonStats, FeatureMap, Morpheme};

#[test]
fn builds_compact_rows_without_source_text_or_regions() {
    let analysis = Analysis {
        analyzer: "vibrato".to_owned(),
        text_id: "t1".to_owned(),
        source_text: "今日".to_owned(),
        morphemes: vec![Morpheme {
            surface: "今日".to_owned(),
            byte_span: 0..6,
            char_span: 0..2,
            features: FeatureMap::new(),
        }],
    };
    let comparison = Comparison {
        text_id: "t1".to_owned(),
        from_analyzer: "vibrato".to_owned(),
        to_analyzer: "sudachi-c".to_owned(),
        regions: Vec::new(),
        feature_diffs: Vec::new(),
        stats: ComparisonStats {
            from_morphemes: 1,
            to_morphemes: 1,
            one_to_one_regions: 1,
            one_to_one_with_feature_differences: 0,
            segmentation_regions: 0,
            split_regions: 0,
            merge_regions: 0,
            resegment_regions: 0,
            coverage_mismatch_regions: 0,
            from_morphemes_in_segmentation: 0,
            to_morphemes_in_segmentation: 0,
            boundary_precision: Some(1.0),
            boundary_recall: Some(1.0),
            boundary_f1: Some(1.0),
        },
    };

    let analysis_row = AnalysisSummaryRow::from_analysis("source-a".to_owned(), &analysis);
    let comparison_row = ComparisonSummaryRow::from_comparison("source-a".to_owned(), &comparison);
    let encoded = serde_json::to_string(&comparison_row).unwrap();

    assert_eq!(analysis_row.source_id, "source-a");
    assert_eq!(analysis_row.text_id, "t1");
    assert_eq!(analysis_row.analyzer, "vibrato");
    assert_eq!(analysis_row.morpheme_count, 1);
    assert_eq!(comparison_row.coverage_mismatch_regions, 0);
    assert_eq!(comparison_row.boundary_f1, Some(1.0));
    assert!(!encoded.contains("source_text"));
    assert!(!encoded.contains("regions"));
}
```

- [ ] **Step 2: Run test and verify failure**

Run:

```bash
cargo test -p ab-morph-run compact::tests::builds_compact_rows_without_source_text_or_regions
```

Expected: compile fails because `AnalysisSummaryRow` and `ComparisonSummaryRow` are missing.

- [ ] **Step 3: Implement compact row structs**

Add above tests in `compact.rs`:

```rust
use ab_morph_diff::{Analysis, Comparison};

#[derive(Debug, Clone, Serialize)]
pub(crate) struct AnalysisSummaryRow {
    pub source_id: String,
    pub text_id: String,
    pub analyzer: String,
    pub morpheme_count: usize,
    pub source_bytes: usize,
    pub source_chars: usize,
}

impl AnalysisSummaryRow {
    pub(crate) fn from_analysis(source_id: String, analysis: &Analysis) -> Self {
        Self {
            source_id,
            text_id: analysis.text_id.clone(),
            analyzer: analysis.analyzer.clone(),
            morpheme_count: analysis.morphemes.len(),
            source_bytes: analysis.source_text.len(),
            source_chars: analysis.source_text.chars().count(),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct ComparisonSummaryRow {
    pub source_id: String,
    pub text_id: String,
    pub from_analyzer: String,
    pub to_analyzer: String,
    pub from_morphemes: usize,
    pub to_morphemes: usize,
    pub one_to_one_regions: usize,
    pub one_to_one_with_feature_differences: usize,
    pub segmentation_regions: usize,
    pub split_regions: usize,
    pub merge_regions: usize,
    pub resegment_regions: usize,
    pub coverage_mismatch_regions: usize,
    pub from_morphemes_in_segmentation: usize,
    pub to_morphemes_in_segmentation: usize,
    pub boundary_precision: Option<f64>,
    pub boundary_recall: Option<f64>,
    pub boundary_f1: Option<f64>,
}

impl ComparisonSummaryRow {
    pub(crate) fn from_comparison(source_id: String, comparison: &Comparison) -> Self {
        let stats = &comparison.stats;
        Self {
            source_id,
            text_id: comparison.text_id.clone(),
            from_analyzer: comparison.from_analyzer.clone(),
            to_analyzer: comparison.to_analyzer.clone(),
            from_morphemes: stats.from_morphemes,
            to_morphemes: stats.to_morphemes,
            one_to_one_regions: stats.one_to_one_regions,
            one_to_one_with_feature_differences: stats.one_to_one_with_feature_differences,
            segmentation_regions: stats.segmentation_regions,
            split_regions: stats.split_regions,
            merge_regions: stats.merge_regions,
            resegment_regions: stats.resegment_regions,
            coverage_mismatch_regions: stats.coverage_mismatch_regions,
            from_morphemes_in_segmentation: stats.from_morphemes_in_segmentation,
            to_morphemes_in_segmentation: stats.to_morphemes_in_segmentation,
            boundary_precision: stats.boundary_precision,
            boundary_recall: stats.boundary_recall,
            boundary_f1: stats.boundary_f1,
        }
    }
}
```

- [ ] **Step 4: Run compact tests**

Run:

```bash
cargo test -p ab-morph-run compact::tests
```

Expected: all compact tests pass.

- [ ] **Step 5: Commit**

Run:

```bash
git add crates/ab-morph-run/src/compact.rs
git commit -m "feat: add compact morph summary rows"
```

---

### Task 4: Add compact output profile to the CLI

**Files:**
- Modify: `crates/ab-morph-run/src/main.rs`
- Modify: `crates/ab-morph-run/src/lib.rs`

- [ ] **Step 1: Add CLI enum and flag**

Modify `main.rs` imports only if needed; do not add a separate CLI-only enum. Add to `AnalyzeAat` args:

```rust
#[arg(long, value_enum, default_value_t = ab_morph_run::OutputProfile::Full)]
output_profile: ab_morph_run::OutputProfile,
```

Pass `output_profile` directly into `run_analyze_aat`.

Expected: compile fails until `OutputProfile` exists and `run_analyze_aat` accepts it.

- [ ] **Step 2: Add public profile enum**

Add near the top of `lib.rs`:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum OutputProfile {
    Full,
    Compact,
}
```

Add an `output_profile: OutputProfile` argument to `run_analyze_aat` after `jobs: usize`.

Update existing tests and call sites to pass `OutputProfile::Full`.

- [ ] **Step 3: Run tests and verify compile is restored**

Run:

```bash
cargo test -p ab-morph-run
```

Expected: tests pass with full-profile behavior unchanged.

- [ ] **Step 4: Commit**

Run:

```bash
git add crates/ab-morph-run/src/main.rs crates/ab-morph-run/src/lib.rs
git commit -m "feat: add morph output profile flag"
```

---

### Task 5: Implement compact profile writing

**Files:**
- Modify: `crates/ab-morph-run/src/lib.rs`
- Modify: `crates/ab-morph-run/src/compact.rs`
- Modify: `crates/ab-morph-run/src/output.rs`

- [ ] **Step 1: Write failing integration-style unit test for compact rows**

In `lib.rs` test module, add:

```rust
#[test]
fn compact_profile_writes_summary_rows_without_full_regions() {
    let dir = temp_dir("compact-profile");
    let aat_dir = dir.join("aat");
    fs::create_dir_all(&aat_dir).unwrap();
    fs::write(
        aat_dir.join("source-a.json"),
        r#"{"version":1,"work_id":"source-a","blocks":[{"kind":"paragraph","content":[{"kind":"text","value":"吾輩は猫である。"}]}],"meta":{"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}}"#,
    )
    .unwrap();

    let analyses = dir.join("analyses.jsonl");
    let comparisons = dir.join("comparisons.jsonl");
    let errors = dir.join("errors.jsonl");

    run_analyze_aat(
        None,
        Some(&aat_dir),
        &["vibrato".to_owned()],
        &analyses,
        Some(&comparisons),
        Some(&errors),
        false,
        1,
        OutputProfile::Compact,
    )
    .unwrap();

    let analysis_text = fs::read_to_string(&analyses).unwrap();
    assert!(analysis_text.contains("\"source_id\":\"source-a\""));
    assert!(analysis_text.contains("\"morpheme_count\""));
    assert!(!analysis_text.contains("source_text"));

    let _ = fs::remove_dir_all(dir);
}
```

This uses only Vibrato so it does not require `AB_SUDACHI_DICT`.

- [ ] **Step 2: Run test and verify failure**

Run:

```bash
cargo test -p ab-morph-run compact_profile_writes_summary_rows_without_full_regions
```

Expected: fails because compact profile still writes full rows or is unimplemented.

- [ ] **Step 3: Route analysis row writing by profile**

In the main per-input loop in `run_analyze_aat`, replace full-only `AnalysisRow` writing with:

```rust
match output_profile {
    OutputProfile::Full => {
        let row = AnalysisRow {
            text_id: analysis.text_id.clone(),
            analyzer: analysis.analyzer.clone(),
            analysis: analysis.clone(),
        };
        write_jsonl_row(&mut analyses_writer, &row)?;
    }
    OutputProfile::Compact => {
        let row = compact::AnalysisSummaryRow::from_analysis(source_id.clone(), &analysis);
        write_jsonl_row(&mut analyses_writer, &row)?;
    }
}
```

Derive `source_id` once per input and keep it alongside `document.text_id`:

```rust
let source_id = compact::source_id_from_aat_path(&input);
```

This is intentionally file-stem based. Do not derive compact-row identity from `document.text_id`, because duplicate logical `text_id` values are valid corpus records.

- [ ] **Step 4: Route comparison row writing by profile**

Change `write_comparison_rows` signature to:

```rust
fn write_comparison_rows(
    writer: &mut impl Write,
    analyses: &[Analysis],
    source_id: &str,
    output_profile: OutputProfile,
) -> Result<()>
```

Inside the comparison loop:

```rust
match output_profile {
    OutputProfile::Full => {
        let row = ComparisonRow {
            text_id: comparison.text_id.clone(),
            from_analyzer: comparison.from_analyzer.clone(),
            to_analyzer: comparison.to_analyzer.clone(),
            comparison,
        };
        write_jsonl_row(writer, &row)?;
    }
    OutputProfile::Compact => {
        let row = compact::ComparisonSummaryRow::from_comparison(source_id.to_owned(), &comparison);
        write_jsonl_row(writer, &row)?;
    }
}
```

Update all callers and tests.

- [ ] **Step 5: Run compact profile test**

Run:

```bash
cargo test -p ab-morph-run compact_profile_writes_summary_rows_without_full_regions
```

Expected: pass.

- [ ] **Step 6: Run full test suite for ab-morph-run**

Run:

```bash
cargo test -p ab-morph-run
```

Expected: all tests pass.

- [ ] **Step 7: Commit**

Run:

```bash
git add crates/ab-morph-run/src/lib.rs crates/ab-morph-run/src/compact.rs
git commit -m "feat: write compact morph corpus summaries"
```

---


### Task 6: Propagate compact profile through parallel jobs and resume

**Files:**
- Modify: `crates/ab-morph-run/src/lib.rs`
- Modify: `crates/ab-morph-run/src/output.rs`

- [ ] **Step 1: Write failing real-shape test for compact parallel output**

In `lib.rs` test module, add a test that uses two tiny AAT files, `--jobs 2`, `OutputProfile::Compact`, and `.jsonl.zst` output paths. Use only Vibrato so no Sudachi dictionary is required:

```rust
#[test]
fn compact_parallel_profile_writes_compressed_summary_rows() {
    let dir = temp_dir("compact-parallel");
    let aat_dir = dir.join("aat");
    fs::create_dir_all(&aat_dir).unwrap();
    for name in ["source-a", "source-b"] {
        fs::write(
            aat_dir.join(format!("{name}.json")),
            format!(
                r#"{{"version":1,"work_id":"{name}","blocks":[{{"kind":"paragraph","content":[{{"kind":"text","value":"吾輩は猫である。"}}]}}],"meta":{{"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}}}}"#
            ),
        )
        .unwrap();
    }

    let analyses = dir.join("analyses.jsonl.zst");
    let comparisons = dir.join("comparisons.jsonl.zst");
    let errors = dir.join("errors.jsonl.zst");
    run_analyze_aat(
        None,
        Some(&aat_dir),
        &["vibrato".to_owned()],
        &analyses,
        Some(&comparisons),
        Some(&errors),
        false,
        2,
        OutputProfile::Compact,
    )
    .unwrap();

    let analysis_bytes = fs::read(&analyses).unwrap();
    let analysis_text = String::from_utf8(zstd::decode_all(analysis_bytes.as_slice()).unwrap()).unwrap();
    assert_eq!(analysis_text.lines().count(), 2);
    assert!(analysis_text.contains("\"source_id\":\"source-a\""));
    assert!(analysis_text.contains("\"source_id\":\"source-b\""));
    assert!(!analysis_text.contains("source_text"));

    let error_bytes = fs::read(&errors).unwrap();
    let error_text = String::from_utf8(zstd::decode_all(error_bytes.as_slice()).unwrap()).unwrap();
    assert_eq!(error_text.lines().count(), 0);

    let _ = fs::remove_dir_all(dir);
}
```

- [ ] **Step 2: Run test and verify failure**

Run:

```bash
cargo test -p ab-morph-run compact_parallel_profile_writes_compressed_summary_rows
```

Expected before implementation: failure because `run_analyze_aat_parallel` still hardcodes full-profile shard runs and plain `.jsonl` shard paths.

- [ ] **Step 3: Mirror final extensions in shard paths**

Change `run_analyze_aat_parallel` so shard paths preserve the final extension:

```rust
fn shard_output_path(output_dir: &Path, final_path: &Path, stem: &str) -> PathBuf {
    let file_name = final_path.file_name().and_then(|name| name.to_str()).unwrap_or(stem);
    if file_name.ends_with(".jsonl.zst") {
        output_dir.join(format!("{stem}.jsonl.zst"))
    } else {
        output_dir.join(format!("{stem}.jsonl"))
    }
}
```

Use it for analyses, comparisons, and errors shard files. Do not hardcode `analyses.jsonl`, `comparisons.jsonl`, or `errors.jsonl` in the parallel path.

- [ ] **Step 4: Thread output profile through shard calls**

Update `run_analyze_aat_parallel` to accept and pass through:

```rust
output_profile: OutputProfile,
```

Inside each worker call `run_analyze_aat` with `output_profile` instead of hardcoding `OutputProfile::Full`. At this point in the plan, examples and manifest output do not exist yet; later tasks extend the same shard plumbing for those outputs.

- [ ] **Step 5: Merge compressed shard files by raw concatenation**

Keep `merge_shard_files` as raw byte concatenation, but make the behavior explicit. For `.zst`, every shard file is already a complete zstd frame because it was created via `open_output_writer`; concatenating frames is valid zstd multi-frame output and is covered by `appends_zstd_output_as_concatenated_frames` from Task 1.

Open final merge targets with `OpenOptions`/`File`, not `open_output_writer`, because the shard bytes are already encoded. If `append == true`, append raw shard frames to the existing file. If `append == false`, truncate/create the file.

- [ ] **Step 6: Make compact resume source-id based**

Replace `read_resume_text_ids` with a function that reads both compact and full rows:

```rust
fn read_resume_ids(path: &Path, prefer_source_id: bool, ids: &mut BTreeSet<String>) -> Result<()> {
    if !path.exists() {
        return Ok(());
    }
    let content = read_jsonl_or_zst_to_string(path)?;
    for line in content.lines().filter(|line| !line.trim().is_empty()) {
        let value: Value = serde_json::from_str(line)?;
        let key = if prefer_source_id {
            value.get("source_id").and_then(Value::as_str)
        } else {
            value.get("text_id").and_then(Value::as_str)
        };
        if let Some(key) = key {
            ids.insert(key.to_owned());
        }
    }
    Ok(())
}
```

For `OutputProfile::Compact`, filter resume inputs by `compact::source_id_from_aat_path(&input)`. For `OutputProfile::Full`, keep the existing `text_id` behavior for backward compatibility.

- [ ] **Step 7: Run tests**

Run:

```bash
cargo test -p ab-morph-run compact_parallel_profile_writes_compressed_summary_rows
cargo test -p ab-morph-run
```

Expected: all tests pass.

- [ ] **Step 8: Commit**

Run:

```bash
git add crates/ab-morph-run/src/lib.rs crates/ab-morph-run/src/output.rs
git commit -m "feat: support compact compressed parallel morph output"
```

---

### Task 7: Add bounded examples file for compact profile

**Files:**
- Modify: `crates/ab-morph-run/src/main.rs`
- Modify: `crates/ab-morph-run/src/lib.rs`
- Modify: `crates/ab-morph-run/src/compact.rs`

- [ ] **Step 1: Add CLI options**

Add to `AnalyzeAat` in `main.rs`:

```rust
#[arg(long)]
examples_output: Option<PathBuf>,
#[arg(long, default_value_t = 10)]
max_examples_per_comparison: usize,
```

Pass both into `run_analyze_aat`.

- [ ] **Step 2: Define example row**

Add to `compact.rs`:

```rust
#[derive(Debug, Clone, Serialize)]
pub(crate) struct ComparisonExampleRow {
    pub source_id: String,
    pub text_id: String,
    pub from_analyzer: String,
    pub to_analyzer: String,
    pub region_index: usize,
    pub kind: String,
    pub byte_start: usize,
    pub byte_end: usize,
    pub char_start: usize,
    pub char_end: usize,
    pub source_excerpt: String,
    pub from_surfaces: Vec<String>,
    pub to_surfaces: Vec<String>,
}
```

- [ ] **Step 3: Write failing test for bounded examples**

Add a compact unit test that builds a `Comparison` with more segmentation regions than budget and asserts only `budget` rows are returned:

```rust
#[test]
fn example_rows_are_limited_by_budget() {
    let comparison = fixture_comparison_with_segmentation_regions(12);
    let rows = example_rows_from_comparison("source-a".to_owned(), "今日は晴れです。", &comparison, 3);
    assert_eq!(rows.len(), 3);
    assert!(rows.iter().all(|row| row.source_id == "source-a"));
}
```

Define `fixture_comparison_with_segmentation_regions` in the test module with actual `Region::Split`, `Region::Merge`, or `Region::Resegment` values matching the current `ab-morph-diff` model. Copy constructors from existing `ab-morph-diff` tests rather than inventing fields.

- [ ] **Step 4: Implement `example_rows_from_comparison`**

Implement:

```rust
pub(crate) fn example_rows_from_comparison(
    source_id: String,
    source_text: &str,
    comparison: &Comparison,
    max_examples: usize,
) -> Vec<ComparisonExampleRow> {
    comparison
        .regions
        .iter()
        .enumerate()
        .filter_map(|(region_index, region)| {
            example_row_from_region(&source_id, source_text, comparison, region_index, region)
        })
        .take(max_examples)
        .collect()
}
```

Implement `example_row_from_region` for segmentation and coverage mismatch regions. Use the region `text_span` to slice `source_text` for `source_excerpt`; clamp to UTF-8 boundaries by using spans produced by `ab-morph-diff`, which are validated byte spans. Also emit example rows for `comparison.feature_diffs` after segmentation/coverage rows, with `kind: "feature_diff"`, so `one_to_one_with_feature_differences` has a drill-down path. Return `None` only for one-to-one regions that do not have feature differences.

- [ ] **Step 5: Wire examples writer in compact profile**

In `run_analyze_aat`, open `examples_output` if present. When writing each comparison in compact mode, also write:

```rust
let source_text = analyses
    .first()
    .map(|analysis| analysis.source_text.as_str())
    .unwrap_or("");
for row in compact::example_rows_from_comparison(
    source_id.to_owned(),
    source_text,
    &comparison,
    max_examples_per_comparison,
) {
    write_jsonl_row(examples_writer, &row)?;
}
```

If `examples_output` is provided with `OutputProfile::Full`, still allow it. Examples are compact evidence and useful beside full rows. The example budget applies per comparison, not per logical text ID.

Update `run_analyze_aat_parallel` in the same task: add an optional shard `examples` path to `ShardOutput`, mirror the final examples extension with `shard_output_path`, pass `examples_output.map(|_| examples.as_path())` to shard workers, and merge example shard files with `merge_shard_files` after comparisons.

- [ ] **Step 6: Run tests**

Run:

```bash
cargo test -p ab-morph-run compact::tests::example_rows_are_limited_by_budget
cargo test -p ab-morph-run
```

Expected: all tests pass.

- [ ] **Step 7: Commit**

Run:

```bash
git add crates/ab-morph-run/src/main.rs crates/ab-morph-run/src/lib.rs crates/ab-morph-run/src/compact.rs
git commit -m "feat: write bounded morph comparison examples"
```

---

### Task 8: Add manifest output

**Files:**
- Modify: `crates/ab-morph-run/src/main.rs`
- Modify: `crates/ab-morph-run/src/lib.rs`
- Modify: `crates/ab-morph-run/src/compact.rs`

- [ ] **Step 1: Add CLI flag**

Add to `AnalyzeAat`:

```rust
#[arg(long)]
manifest_output: Option<PathBuf>,
```

Pass it into `run_analyze_aat`.

- [ ] **Step 2: Define manifest row**

Add to `compact.rs`:

```rust
#[derive(Debug, Clone, Serialize)]
pub(crate) struct RunManifest {
    pub version: u32,
    pub output_profile: String,
    pub analyzer_args: Vec<String>,
    pub jobs: usize,
    pub input_mode: String,
    pub input_path: String,
    pub input_file_count: usize,
    pub analyses_output: String,
    pub comparisons_output: Option<String>,
    pub examples_output: Option<String>,
    pub errors_output: Option<String>,
}
```

- [ ] **Step 3: Write manifest at end of run**

After all writers flush, if `manifest_output` is present, write pretty JSON:

```rust
let manifest = compact::RunManifest {
    version: 1,
    output_profile: output_profile.as_str().to_owned(),
    analyzer_args: analyzer_ids.to_vec(),
    jobs,
    input_mode: if aat.is_some() { "aat" } else { "aat_dir" }.to_owned(),
    input_path: aat.or(aat_dir).map(|path| path.display().to_string()).unwrap_or_default(),
    input_file_count: inputs.len(),
    analyses_output: analyses_output.display().to_string(),
    comparisons_output: comparisons_output.map(|path| path.display().to_string()),
    examples_output: examples_output.map(|path| path.display().to_string()),
    errors_output: errors_output.map(|path| path.display().to_string()),
};
serde_json::to_writer_pretty(File::create(path)?, &manifest)?;
```

Add `OutputProfile::as_str`:

```rust
impl OutputProfile {
    fn as_str(self) -> &'static str {
        match self {
            OutputProfile::Full => "full",
            OutputProfile::Compact => "compact",
        }
    }
}
```

- [ ] **Step 4: Test manifest content**

Add a unit test around a one-file Vibrato compact run:

```rust
#[test]
fn writes_manifest_for_compact_run() {
    let dir = temp_dir("manifest");
    let aat_dir = dir.join("aat");
    fs::create_dir_all(&aat_dir).unwrap();
    fs::write(
        aat_dir.join("source-a.json"),
        r#"{"version":1,"work_id":"source-a","blocks":[{"kind":"paragraph","content":[{"kind":"text","value":"吾輩は猫である。"}]}],"meta":{"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}}"#,
    )
    .unwrap();

    let manifest = dir.join("manifest.json");
    run_analyze_aat(
        None,
        Some(&aat_dir),
        &["vibrato".to_owned()],
        &dir.join("analyses.jsonl"),
        Some(&dir.join("comparisons.jsonl")),
        Some(&dir.join("errors.jsonl")),
        false,
        1,
        OutputProfile::Compact,
        None,
        10,
        Some(&manifest),
    )
    .unwrap();

    let value: serde_json::Value = serde_json::from_str(&fs::read_to_string(&manifest).unwrap()).unwrap();
    assert_eq!(value["output_profile"], "compact");
    assert_eq!(value["jobs"], 1);
    assert_eq!(value["input_file_count"], 1);

    let _ = fs::remove_dir_all(dir);
}
```

- [ ] **Step 5: Run tests**

Run:

```bash
cargo test -p ab-morph-run writes_manifest_for_compact_run
cargo test -p ab-morph-run
```

Expected: all tests pass.

- [ ] **Step 6: Commit**

Run:

```bash
git add crates/ab-morph-run/src/main.rs crates/ab-morph-run/src/lib.rs crates/ab-morph-run/src/compact.rs
git commit -m "feat: write morph run manifests"
```

---

### Task 9: Real corpus smoke and documentation

**Files:**
- Create: `docs/superpowers/reports/2026-04-29-morph-artifact-storage.md`

- [ ] **Step 1: Run compact smoke on existing 10-file sample**

Run:

```bash
out=scratch/morph-compact-smoke
rm -rf "$out"
mkdir -p "$out"
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  cargo run -p ab-morph-run -- analyze-aat \
  --aat-dir scratch/morph-real-sample/aats/aozora-rs-adapter \
  --analyzer vibrato \
  --analyzer sudachi-c \
  --output-profile compact \
  --analyses-output "$out/analyses.jsonl.zst" \
  --comparisons-output "$out/comparisons.jsonl.zst" \
  --examples-output "$out/examples.jsonl.zst" \
  --errors-output "$out/errors.jsonl.zst" \
  --manifest-output "$out/manifest.json" \
  --jobs 2
```

Expected: command exits 0.

- [ ] **Step 2: Verify compact smoke counts**

Run:

```bash
python - <<'PY'
import zstandard as zstd
from pathlib import Path
for name in ['analyses', 'comparisons', 'examples', 'errors']:
    path = Path('scratch/morph-compact-smoke') / f'{name}.jsonl.zst'
    data = zstd.ZstdDecompressor().decompress(path.read_bytes())
    rows = [line for line in data.splitlines() if line]
    print(name, len(rows))
PY
```

If the Python `zstandard` module is unavailable, use `zstd -dc scratch/morph-compact-smoke/analyses.jsonl.zst | wc -l` for each file.

Expected:

```text
analyses 20
comparisons 10
errors 0
```

`examples` may vary by segmentation differences but should be bounded by `max_examples_per_comparison * comparison_count`.

- [ ] **Step 3: Compare compact vs full size on smoke sample**

Run:

```bash
du -h scratch/morph-real-sample/chunked-verify/analyses.jsonl \
      scratch/morph-real-sample/chunked-verify/comparisons.jsonl \
      scratch/morph-compact-smoke/analyses.jsonl.zst \
      scratch/morph-compact-smoke/comparisons.jsonl.zst \
      scratch/morph-compact-smoke/examples.jsonl.zst
```

Expected: compact compressed files are substantially smaller than full JSONL.

- [ ] **Step 4: Write storage report**

Create `docs/superpowers/reports/2026-04-29-morph-artifact-storage.md`:

```markdown
# Morph artifact storage report

Date: 2026-04-29

## Problem

Full corpus morph rows are too large because full `Analysis` and `Comparison` rows repeat source text and region detail.

## New contract

Corpus runs should use `--output-profile compact` with `.jsonl.zst` outputs.

Required identities:

- `source_id`: unique AAT/source record key.
- `text_id`: logical work grouping key.

## Artifacts

- `analyses.jsonl.zst`: compact analysis summaries.
- `comparisons.jsonl.zst`: compact comparison summaries.
- `examples.jsonl.zst`: bounded evidence rows.
- `errors.jsonl.zst`: per-file/per-analyzer errors.
- `manifest.json`: run metadata.

## Debug path

Use `--output-profile full` on selected AAT files when full detail is needed. Sudachi may internally chunk large documents, but chunking is reassembled into one `Analysis` per `source_id`; it must not change `text_id` or `source_id`.

## Result

Paste the exact `du -h` output from Step 3 in a fenced `text` block, then add one sentence stating whether compact compressed files are smaller than the full JSONL files for this smoke sample.
```

Replace the final `Record...` sentence with actual size numbers from Step 3.

- [ ] **Step 5: Final verification**

Run:

```bash
cargo fmt --all -- --check
cargo test -p ab-morph-run
```

Expected: format check passes and all tests pass.

- [ ] **Step 6: Commit**

Run:

```bash
git add docs/superpowers/reports/2026-04-29-morph-artifact-storage.md
git commit -m "docs: document compact morph artifacts"
```

---

## Self-Review

### Spec coverage

This plan covers the requested goal: complete corpus comparison without ballooning storage. It adds compact summaries, source identity, bounded examples including feature-diff evidence, zstd output, compact-aware parallel execution, source-id-based compact resume, and a manifest. It deliberately leaves full-detail regeneration as a later command because existing `--output-profile full` on selected AAT files already provides the debug path.

### Placeholder scan

No task uses placeholder markers or unspecified error handling. Task 7 requires copying exact region constructors from current `ab-morph-diff` tests because those model fields must match the current code; the plan explicitly says not to invent fields. The previous invalid boundary-metrics fixture has been removed; boundary metrics live only in `ComparisonStats`.

### Type consistency

The plan consistently uses `source_id`, `text_id`, `OutputProfile::{Full, Compact}`, `AnalysisSummaryRow`, `ComparisonSummaryRow`, and `ComparisonExampleRow`. Existing tests must be updated to pass `OutputProfile::Full` after the signature change. Compact resume is keyed by `source_id`; full-profile resume remains text-id keyed for backward compatibility.

### Hickey alignment

The implementation separates identity, summaries, examples, compression, and manifest concerns. It does not make compression a substitute for a simpler data model.
