# Compact Morph Workflow Tools Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the smallest usable workflow around compact morph outputs: summarize worst cases, rerun selected sources in full mode, emit periodic progress, and document source-id/text-id policy.

**Architecture:** Keep `analyze-aat` as the engine. Add focused library modules for compact summary loading/ranking and source-id input selection, then expose them through two CLI subcommands: `summarize-compact` and `rerun-full`. Periodic progress stays in `main.rs` as CLI orchestration around the existing library call.

**Tech Stack:** Rust 2024, `clap`, `serde`/`serde_json`, existing zstd reader in `ab-morph-run::output`, existing compact JSONL row schemas.

---

## File Map

- Modify: `crates/ab-morph-run/src/lib.rs` — expose new modules/functions and add source-id filtering support to the runner.
- Modify: `crates/ab-morph-run/src/main.rs` — add CLI subcommands and periodic progress orchestration.
- Modify: `crates/ab-morph-run/src/compact.rs` — make compact row types usable by sibling modules and tests; keep schema unchanged.
- Create: `crates/ab-morph-run/src/summary.rs` — load compact comparison JSONL, aggregate/rank by explicit grouping key, and serialize summary rows.
- Create: `crates/ab-morph-run/src/select.rs` — resolve AAT file paths by `source_id` for targeted full reruns.
- Create: `docs/morph-corpus-workflow.md` — standard corpus workflow and identity policy.
- Modify: `docs/superpowers/reports/2026-04-29-morph-compact-artifacts.md` — add a short pointer to the workflow doc and tools.

## Public CLI Shape

```bash
ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst \
  --sort-by boundary-f1 \
  --group-by source-id \
  --limit 20

ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst \
  --sort-by segmentation-regions \
  --group-by text-id \
  --limit 20 \
  --json

ab-morph-run rerun-full \
  --aat-dir scratch/morph-full-corpus/aats \
  --source-id 001529_50685-dd3b2fe4e5bf \
  --analyzer vibrato \
  --analyzer sudachi-c \
  --output-dir scratch/morph-targeted-full/001529_50685-dd3b2fe4e5bf

ab-morph-run analyze-aat \
  --aat-dir scratch/morph-full-corpus/aats \
  --analyzer vibrato \
  --analyzer sudachi-c \
  --output-profile compact \
  --analyses-output scratch/out/analyses.jsonl.zst \
  --comparisons-output scratch/out/comparisons.jsonl.zst \
  --progress \
  --progress-interval-seconds 30
```

## Identity Policy

- `source_id` is the stable row/run identity. It is derived from the AAT file stem and distinguishes duplicate corpus index records.
- `text_id` is a logical grouping key. It can intentionally collapse multiple source records.
- Summary tooling must require explicit grouping semantics through `--group-by source-id|text-id`; default is `source-id`.
- Targeted full rerun selects by `source_id`, not `text_id`, to avoid accidentally rerunning or skipping duplicate logical works.

---

### Task 1: Compact Summary Library

**Files:**
- Create: `crates/ab-morph-run/src/summary.rs`
- Modify: `crates/ab-morph-run/src/lib.rs`
- Modify: `crates/ab-morph-run/src/compact.rs`

- [ ] **Step 1: Make compact comparison row fields comparable in tests**

In `crates/ab-morph-run/src/compact.rs`, change this derive:

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct ComparisonSummaryRow {
```

to:

```rust
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub(crate) struct ComparisonSummaryRow {
```

- [ ] **Step 2: Add the summary module declaration**

In `crates/ab-morph-run/src/lib.rs`, add the module near the existing modules:

```rust
mod compact;
mod output;
mod summary;
```

Then add these re-exports near `OutputProfile`:

```rust
pub use summary::{
    CompactSummaryGroupBy, CompactSummaryOptions, CompactSummaryRow, CompactSummarySort,
    summarize_compact_comparisons,
};
```

- [ ] **Step 3: Write failing summary tests**

Create `crates/ab-morph-run/src/summary.rs` with this initial content:

```rust
use std::path::Path;

use anyhow::Result;
use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompactSummaryGroupBy {
    SourceId,
    TextId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompactSummarySort {
    BoundaryF1,
    SegmentationRegions,
    FeatureDifferences,
    CoverageMismatchRegions,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompactSummaryOptions {
    pub group_by: CompactSummaryGroupBy,
    pub sort_by: CompactSummarySort,
    pub limit: usize,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CompactSummaryRow {
    pub key: String,
    pub source_ids: Vec<String>,
    pub text_ids: Vec<String>,
    pub comparisons: usize,
    pub worst_boundary_f1: Option<f64>,
    pub total_segmentation_regions: usize,
    pub total_feature_difference_regions: usize,
    pub total_coverage_mismatch_regions: usize,
    pub max_segmentation_regions: usize,
    pub max_feature_difference_regions: usize,
    pub max_coverage_mismatch_regions: usize,
}

pub fn summarize_compact_comparisons(
    _comparisons_path: &Path,
    _options: CompactSummaryOptions,
) -> Result<Vec<CompactSummaryRow>> {
    unimplemented!("summary implementation follows failing tests")
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::*;

    #[test]
    fn summarize_groups_by_source_id_and_sorts_lowest_boundary_f1_first() {
        let dir = temp_dir("source-boundary");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("comparisons.jsonl");
        fs::write(
            &path,
            concat!(
                r#"{"source_id":"src-a","text_id":"t1","from_analyzer":"vibrato","to_analyzer":"sudachi-c","from_morphemes":10,"to_morphemes":11,"one_to_one_regions":8,"one_to_one_with_feature_differences":1,"segmentation_regions":2,"coverage_mismatch_regions":0,"split_regions":1,"merge_regions":1,"resegment_regions":0,"from_morphemes_in_segmentation":2,"to_morphemes_in_segmentation":3,"boundary_precision":0.9,"boundary_recall":0.8,"boundary_f1":0.847}"#, "\n",
                r#"{"source_id":"src-b","text_id":"t1","from_analyzer":"vibrato","to_analyzer":"sudachi-c","from_morphemes":10,"to_morphemes":10,"one_to_one_regions":10,"one_to_one_with_feature_differences":0,"segmentation_regions":0,"coverage_mismatch_regions":0,"split_regions":0,"merge_regions":0,"resegment_regions":0,"from_morphemes_in_segmentation":0,"to_morphemes_in_segmentation":0,"boundary_precision":1.0,"boundary_recall":1.0,"boundary_f1":1.0}"#, "\n",
                r#"{"source_id":"src-c","text_id":"t2","from_analyzer":"vibrato","to_analyzer":"sudachi-c","from_morphemes":5,"to_morphemes":5,"one_to_one_regions":5,"one_to_one_with_feature_differences":0,"segmentation_regions":0,"coverage_mismatch_regions":0,"split_regions":0,"merge_regions":0,"resegment_regions":0,"from_morphemes_in_segmentation":0,"to_morphemes_in_segmentation":0,"boundary_precision":null,"boundary_recall":null,"boundary_f1":null}"#, "\n",
            ),
        )
        .unwrap();

        let rows = summarize_compact_comparisons(
            &path,
            CompactSummaryOptions {
                group_by: CompactSummaryGroupBy::SourceId,
                sort_by: CompactSummarySort::BoundaryF1,
                limit: 2,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0].key, "src-a");
        assert_eq!(rows[0].worst_boundary_f1, Some(0.847));
        assert_eq!(rows[1].key, "src-b");
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn summarize_can_group_duplicate_text_ids_explicitly() {
        let dir = temp_dir("text-group");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("comparisons.jsonl");
        fs::write(
            &path,
            concat!(
                r#"{"source_id":"src-a","text_id":"same","from_analyzer":"vibrato","to_analyzer":"sudachi-c","from_morphemes":10,"to_morphemes":11,"one_to_one_regions":8,"one_to_one_with_feature_differences":2,"segmentation_regions":3,"coverage_mismatch_regions":1,"split_regions":1,"merge_regions":1,"resegment_regions":1,"from_morphemes_in_segmentation":3,"to_morphemes_in_segmentation":4,"boundary_precision":0.9,"boundary_recall":0.8,"boundary_f1":0.847}"#, "\n",
                r#"{"source_id":"src-b","text_id":"same","from_analyzer":"vibrato","to_analyzer":"sudachi-c","from_morphemes":12,"to_morphemes":13,"one_to_one_regions":9,"one_to_one_with_feature_differences":4,"segmentation_regions":5,"coverage_mismatch_regions":0,"split_regions":3,"merge_regions":1,"resegment_regions":1,"from_morphemes_in_segmentation":5,"to_morphemes_in_segmentation":6,"boundary_precision":0.95,"boundary_recall":0.9,"boundary_f1":0.924}"#, "\n",
            ),
        )
        .unwrap();

        let rows = summarize_compact_comparisons(
            &path,
            CompactSummaryOptions {
                group_by: CompactSummaryGroupBy::TextId,
                sort_by: CompactSummarySort::SegmentationRegions,
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].key, "same");
        assert_eq!(rows[0].source_ids, vec!["src-a", "src-b"]);
        assert_eq!(rows[0].comparisons, 2);
        assert_eq!(rows[0].total_segmentation_regions, 8);
        assert_eq!(rows[0].max_segmentation_regions, 5);
        assert_eq!(rows[0].total_feature_difference_regions, 6);
        assert_eq!(rows[0].total_coverage_mismatch_regions, 1);
        let _ = fs::remove_dir_all(dir);
    }

    fn temp_dir(label: &str) -> std::path::PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!(
            "ab-morph-run-summary-{label}-{}-{unique}",
            std::process::id()
        ))
    }
}
```

- [ ] **Step 4: Run summary tests and verify red**

Run:

```bash
cargo test -p ab-morph-run summary::tests
```

Expected: tests compile and fail with the `unimplemented!` panic from `summarize_compact_comparisons`.

- [ ] **Step 5: Implement summary loading, grouping, and sorting**

Replace the body of `crates/ab-morph-run/src/summary.rs` above the test module with:

```rust
use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use anyhow::Result;
use serde::Serialize;

use crate::compact::ComparisonSummaryRow;
use crate::output::read_jsonl_or_zst_to_string;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompactSummaryGroupBy {
    SourceId,
    TextId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompactSummarySort {
    BoundaryF1,
    SegmentationRegions,
    FeatureDifferences,
    CoverageMismatchRegions,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompactSummaryOptions {
    pub group_by: CompactSummaryGroupBy,
    pub sort_by: CompactSummarySort,
    pub limit: usize,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CompactSummaryRow {
    pub key: String,
    pub source_ids: Vec<String>,
    pub text_ids: Vec<String>,
    pub comparisons: usize,
    pub worst_boundary_f1: Option<f64>,
    pub total_segmentation_regions: usize,
    pub total_feature_difference_regions: usize,
    pub total_coverage_mismatch_regions: usize,
    pub max_segmentation_regions: usize,
    pub max_feature_difference_regions: usize,
    pub max_coverage_mismatch_regions: usize,
}

#[derive(Debug, Default)]
struct Accumulator {
    source_ids: BTreeSet<String>,
    text_ids: BTreeSet<String>,
    comparisons: usize,
    worst_boundary_f1: Option<f64>,
    total_segmentation_regions: usize,
    total_feature_difference_regions: usize,
    total_coverage_mismatch_regions: usize,
    max_segmentation_regions: usize,
    max_feature_difference_regions: usize,
    max_coverage_mismatch_regions: usize,
}

pub fn summarize_compact_comparisons(
    comparisons_path: &Path,
    options: CompactSummaryOptions,
) -> Result<Vec<CompactSummaryRow>> {
    let text = read_jsonl_or_zst_to_string(comparisons_path)?;
    let mut groups = BTreeMap::<String, Accumulator>::new();

    for line in text.lines().filter(|line| !line.trim().is_empty()) {
        let row: ComparisonSummaryRow = serde_json::from_str(line)?;
        let key = match options.group_by {
            CompactSummaryGroupBy::SourceId => row.source_id.clone(),
            CompactSummaryGroupBy::TextId => row.text_id.clone(),
        };
        groups.entry(key).or_default().push(row);
    }

    let mut rows = groups
        .into_iter()
        .map(|(key, accumulator)| accumulator.into_row(key))
        .collect::<Vec<_>>();
    rows.sort_by(|left, right| compare_rows(left, right, options.sort_by));
    rows.truncate(options.limit);
    Ok(rows)
}

impl Accumulator {
    fn push(&mut self, row: ComparisonSummaryRow) {
        self.source_ids.insert(row.source_id);
        self.text_ids.insert(row.text_id);
        self.comparisons += 1;
        self.worst_boundary_f1 = match (self.worst_boundary_f1, row.boundary_f1) {
            (Some(left), Some(right)) => Some(left.min(right)),
            (None, Some(value)) => Some(value),
            (value, None) => value,
        };
        self.total_segmentation_regions += row.segmentation_regions;
        self.total_feature_difference_regions += row.one_to_one_with_feature_differences;
        self.total_coverage_mismatch_regions += row.coverage_mismatch_regions;
        self.max_segmentation_regions = self.max_segmentation_regions.max(row.segmentation_regions);
        self.max_feature_difference_regions = self
            .max_feature_difference_regions
            .max(row.one_to_one_with_feature_differences);
        self.max_coverage_mismatch_regions = self
            .max_coverage_mismatch_regions
            .max(row.coverage_mismatch_regions);
    }

    fn into_row(self, key: String) -> CompactSummaryRow {
        CompactSummaryRow {
            key,
            source_ids: self.source_ids.into_iter().collect(),
            text_ids: self.text_ids.into_iter().collect(),
            comparisons: self.comparisons,
            worst_boundary_f1: self.worst_boundary_f1,
            total_segmentation_regions: self.total_segmentation_regions,
            total_feature_difference_regions: self.total_feature_difference_regions,
            total_coverage_mismatch_regions: self.total_coverage_mismatch_regions,
            max_segmentation_regions: self.max_segmentation_regions,
            max_feature_difference_regions: self.max_feature_difference_regions,
            max_coverage_mismatch_regions: self.max_coverage_mismatch_regions,
        }
    }
}

fn compare_rows(
    left: &CompactSummaryRow,
    right: &CompactSummaryRow,
    sort_by: CompactSummarySort,
) -> std::cmp::Ordering {
    match sort_by {
        CompactSummarySort::BoundaryF1 => compare_boundary_f1(left, right),
        CompactSummarySort::SegmentationRegions => right
            .total_segmentation_regions
            .cmp(&left.total_segmentation_regions)
            .then_with(|| left.key.cmp(&right.key)),
        CompactSummarySort::FeatureDifferences => right
            .total_feature_difference_regions
            .cmp(&left.total_feature_difference_regions)
            .then_with(|| left.key.cmp(&right.key)),
        CompactSummarySort::CoverageMismatchRegions => right
            .total_coverage_mismatch_regions
            .cmp(&left.total_coverage_mismatch_regions)
            .then_with(|| left.key.cmp(&right.key)),
    }
}

fn compare_boundary_f1(left: &CompactSummaryRow, right: &CompactSummaryRow) -> std::cmp::Ordering {
    match (left.worst_boundary_f1, right.worst_boundary_f1) {
        (Some(left), Some(right)) => left
            .partial_cmp(&right)
            .unwrap_or(std::cmp::Ordering::Equal),
        (Some(_), None) => std::cmp::Ordering::Less,
        (None, Some(_)) => std::cmp::Ordering::Greater,
        (None, None) => left.key.cmp(&right.key),
    }
    .then_with(|| left.key.cmp(&right.key))
}
```

Keep the test module from Step 3 below this implementation.

- [ ] **Step 6: Run summary tests and verify green**

Run:

```bash
cargo test -p ab-morph-run summary::tests
```

Expected: PASS.

- [ ] **Step 7: Commit Task 1**

```bash
git add crates/ab-morph-run/src/lib.rs crates/ab-morph-run/src/compact.rs crates/ab-morph-run/src/summary.rs
git commit -m "feat: summarize compact morph comparisons"
```

---

### Task 2: `summarize-compact` CLI

**Files:**
- Modify: `crates/ab-morph-run/src/main.rs`
- Test: `crates/ab-morph-run/src/main.rs`

- [ ] **Step 1: Write failing parser tests for the new subcommand enums**

In `crates/ab-morph-run/src/main.rs`, add the enum types above `Command`:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum SummaryGroupByArg {
    SourceId,
    TextId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum SummarySortArg {
    BoundaryF1,
    SegmentationRegions,
    FeatureDifferences,
    CoverageMismatchRegions,
}
```

Then add a failing test inside the existing `#[cfg(test)] mod tests`:

```rust
#[test]
fn parses_summarize_compact_command() {
    let args = Args::parse_from([
        "ab-morph-run",
        "summarize-compact",
        "--comparisons",
        "comparisons.jsonl.zst",
        "--group-by",
        "text-id",
        "--sort-by",
        "segmentation-regions",
        "--limit",
        "25",
        "--json",
    ]);

    let Command::SummarizeCompact {
        comparisons,
        group_by,
        sort_by,
        limit,
        json,
    } = args.command
    else {
        panic!("expected summarize-compact command");
    };

    assert_eq!(comparisons, PathBuf::from("comparisons.jsonl.zst"));
    assert_eq!(group_by, SummaryGroupByArg::TextId);
    assert_eq!(sort_by, SummarySortArg::SegmentationRegions);
    assert_eq!(limit, 25);
    assert!(json);
}
```

- [ ] **Step 2: Run parser test and verify red**

Run:

```bash
cargo test -p ab-morph-run parses_summarize_compact_command
```

Expected: FAIL because `Command::SummarizeCompact` is not defined.

- [ ] **Step 3: Add the CLI variant and mapping helpers**

In `Command`, add this variant after `AnalyzeAat`:

```rust
    SummarizeCompact {
        #[arg(long)]
        comparisons: PathBuf,
        #[arg(long, value_enum, default_value_t = SummaryGroupByArg::SourceId)]
        group_by: SummaryGroupByArg,
        #[arg(long, value_enum, default_value_t = SummarySortArg::BoundaryF1)]
        sort_by: SummarySortArg,
        #[arg(long, default_value_t = 20)]
        limit: usize,
        #[arg(long)]
        json: bool,
    },
```

Add mapping helpers below `Command`:

```rust
impl SummaryGroupByArg {
    fn into_library(self) -> ab_morph_run::CompactSummaryGroupBy {
        match self {
            Self::SourceId => ab_morph_run::CompactSummaryGroupBy::SourceId,
            Self::TextId => ab_morph_run::CompactSummaryGroupBy::TextId,
        }
    }
}

impl SummarySortArg {
    fn into_library(self) -> ab_morph_run::CompactSummarySort {
        match self {
            Self::BoundaryF1 => ab_morph_run::CompactSummarySort::BoundaryF1,
            Self::SegmentationRegions => ab_morph_run::CompactSummarySort::SegmentationRegions,
            Self::FeatureDifferences => ab_morph_run::CompactSummarySort::FeatureDifferences,
            Self::CoverageMismatchRegions => ab_morph_run::CompactSummarySort::CoverageMismatchRegions,
        }
    }
}
```

- [ ] **Step 4: Implement CLI execution and table formatting**

In the `match args.command` block, add this arm after `AnalyzeAat`:

```rust
        Command::SummarizeCompact {
            comparisons,
            group_by,
            sort_by,
            limit,
            json,
        } => {
            let rows = ab_morph_run::summarize_compact_comparisons(
                &comparisons,
                ab_morph_run::CompactSummaryOptions {
                    group_by: group_by.into_library(),
                    sort_by: sort_by.into_library(),
                    limit,
                },
            )?;
            if json {
                serde_json::to_writer_pretty(std::io::stdout(), &rows)?;
                println!();
            } else {
                print_summary_table(&rows);
            }
            Ok(())
        }
```

Add this helper near `emit_progress_summary`:

```rust
fn print_summary_table(rows: &[ab_morph_run::CompactSummaryRow]) {
    println!(
        "key\tcomparisons\tworst_boundary_f1\ttotal_segmentation_regions\ttotal_feature_difference_regions\ttotal_coverage_mismatch_regions"
    );
    for row in rows {
        println!(
            "{}\t{}\t{}\t{}\t{}\t{}",
            row.key,
            row.comparisons,
            row.worst_boundary_f1
                .map(|value| value.to_string())
                .unwrap_or_else(|| "null".to_owned()),
            row.total_segmentation_regions,
            row.total_feature_difference_regions,
            row.total_coverage_mismatch_regions,
        );
    }
}
```

- [ ] **Step 5: Run parser test and CLI unit tests**

Run:

```bash
cargo test -p ab-morph-run parses_summarize_compact_command
cargo test -p ab-morph-run --lib summary::tests
```

Expected: PASS.

- [ ] **Step 6: Commit Task 2**

```bash
git add crates/ab-morph-run/src/main.rs
git commit -m "feat: add compact summary CLI"
```

---

### Task 3: Source-ID Selection and `rerun-full` CLI

**Files:**
- Create: `crates/ab-morph-run/src/select.rs`
- Modify: `crates/ab-morph-run/src/lib.rs`
- Modify: `crates/ab-morph-run/src/main.rs`

- [ ] **Step 1: Add source-id selector module and failing tests**

Create `crates/ab-morph-run/src/select.rs`:

```rust
use std::path::{Path, PathBuf};

use anyhow::Result;

use crate::compact::source_id_from_aat_path;

pub fn resolve_source_id_aat_paths(
    _aat_dir: &Path,
    _source_ids: &[String],
) -> Result<Vec<PathBuf>> {
    unimplemented!("selector implementation follows failing tests")
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::*;

    #[test]
    fn resolves_source_ids_recursively_in_requested_order() {
        let dir = temp_dir("resolve");
        let nested = dir.join("aozora-rs-adapter");
        fs::create_dir_all(&nested).unwrap();
        fs::write(nested.join("src-b.json"), "{}").unwrap();
        fs::write(nested.join("src-a.json"), "{}").unwrap();

        let paths = resolve_source_id_aat_paths(
            &dir,
            &["src-a".to_owned(), "src-b".to_owned()],
        )
        .unwrap();

        assert_eq!(paths.len(), 2);
        assert_eq!(source_id_from_aat_path(&paths[0]), "src-a");
        assert_eq!(source_id_from_aat_path(&paths[1]), "src-b");
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn missing_source_id_is_an_error() {
        let dir = temp_dir("missing");
        fs::create_dir_all(&dir).unwrap();

        let error = resolve_source_id_aat_paths(&dir, &["missing".to_owned()])
            .unwrap_err()
            .to_string();

        assert!(error.contains("missing source_id"));
        let _ = fs::remove_dir_all(dir);
    }

    fn temp_dir(label: &str) -> std::path::PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!(
            "ab-morph-run-select-{label}-{}-{unique}",
            std::process::id()
        ))
    }
}
```

- [ ] **Step 2: Wire module declaration**

In `crates/ab-morph-run/src/lib.rs`, add:

```rust
mod select;
```

and re-export:

```rust
pub use select::resolve_source_id_aat_paths;
```

- [ ] **Step 3: Run selector tests and verify red**

Run:

```bash
cargo test -p ab-morph-run select::tests
```

Expected: FAIL with `unimplemented!` panic.

- [ ] **Step 4: Implement recursive source-id resolution**

Replace `select.rs` above the test module with:

```rust
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};

use crate::compact::source_id_from_aat_path;

pub fn resolve_source_id_aat_paths(aat_dir: &Path, source_ids: &[String]) -> Result<Vec<PathBuf>> {
    if source_ids.is_empty() {
        bail!("provide at least one --source-id");
    }
    if !aat_dir.is_dir() {
        bail!("--aat-dir must point to an existing directory: {}", aat_dir.display());
    }

    let requested = source_ids.iter().cloned().collect::<BTreeSet<_>>();
    let mut discovered = BTreeMap::<String, PathBuf>::new();
    collect_matching_json(aat_dir, &requested, &mut discovered)?;

    let mut paths = Vec::new();
    for source_id in source_ids {
        let Some(path) = discovered.get(source_id) else {
            bail!("missing source_id `{source_id}` under {}", aat_dir.display());
        };
        paths.push(path.clone());
    }
    Ok(paths)
}

fn collect_matching_json(
    dir: &Path,
    requested: &BTreeSet<String>,
    discovered: &mut BTreeMap<String, PathBuf>,
) -> Result<()> {
    for entry in fs::read_dir(dir).with_context(|| format!("failed to read {}", dir.display()))? {
        let path = entry?.path();
        if path.is_dir() {
            collect_matching_json(&path, requested, discovered)?;
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("json") {
            let source_id = source_id_from_aat_path(&path);
            if requested.contains(&source_id) {
                if discovered.insert(source_id.clone(), path).is_some() {
                    bail!("duplicate source_id `{source_id}` under {}", dir.display());
                }
            }
        }
    }
    Ok(())
}
```

Keep the test module from Step 1 below this implementation.

- [ ] **Step 5: Run selector tests and verify green**

Run:

```bash
cargo test -p ab-morph-run select::tests
```

Expected: PASS.

- [ ] **Step 6: Add `rerun-full` parser test**

In `crates/ab-morph-run/src/main.rs`, add this test inside the existing test module:

```rust
#[test]
fn parses_rerun_full_command() {
    let args = Args::parse_from([
        "ab-morph-run",
        "rerun-full",
        "--aat-dir",
        "aats",
        "--source-id",
        "src-a",
        "--source-id",
        "src-b",
        "--analyzer",
        "vibrato",
        "--analyzer",
        "sudachi-c",
        "--output-dir",
        "full-out",
    ]);

    let Command::RerunFull {
        aat_dir,
        source_id,
        analyzer,
        output_dir,
    } = args.command
    else {
        panic!("expected rerun-full command");
    };

    assert_eq!(aat_dir, PathBuf::from("aats"));
    assert_eq!(source_id, vec!["src-a".to_owned(), "src-b".to_owned()]);
    assert_eq!(analyzer, vec!["vibrato".to_owned(), "sudachi-c".to_owned()]);
    assert_eq!(output_dir, PathBuf::from("full-out"));
}
```

- [ ] **Step 7: Run parser test and verify red**

Run:

```bash
cargo test -p ab-morph-run parses_rerun_full_command
```

Expected: FAIL because `Command::RerunFull` is not defined.

- [ ] **Step 8: Implement the `rerun-full` CLI command**

In `Command`, add this variant after `SummarizeCompact`:

```rust
    RerunFull {
        #[arg(long)]
        aat_dir: PathBuf,
        #[arg(long, required = true)]
        source_id: Vec<String>,
        #[arg(long, required = true)]
        analyzer: Vec<String>,
        #[arg(long)]
        output_dir: PathBuf,
    },
```

Add this match arm after `SummarizeCompact`:

```rust
        Command::RerunFull {
            aat_dir,
            source_id,
            analyzer,
            output_dir,
        } => {
            let inputs = ab_morph_run::resolve_source_id_aat_paths(&aat_dir, &source_id)?;
            let input_dir = output_dir.join("inputs");
            if input_dir.exists() {
                fs::remove_dir_all(&input_dir)?;
            }
            fs::create_dir_all(&input_dir)?;
            for input in inputs {
                let target = input_dir.join(input.file_name().expect("AAT path has file name"));
                fs::copy(&input, &target)?;
            }
            ab_morph_run::run_analyze_aat(
                None,
                Some(&input_dir),
                &analyzer,
                &output_dir.join("analyses.jsonl"),
                Some(&output_dir.join("comparisons.jsonl")),
                Some(&output_dir.join("errors.jsonl")),
                false,
                1,
                ab_morph_run::OutputProfile::Full,
                None,
                0,
                Some(&output_dir.join("manifest.json")),
            )
        }
```

This copies selected AAT files into an output-local `inputs/` directory before running. That avoids extending `run_analyze_aat` with an input-list API in this increment.

- [ ] **Step 9: Run selector and parser tests**

Run:

```bash
cargo test -p ab-morph-run select::tests
cargo test -p ab-morph-run parses_rerun_full_command
```

Expected: both PASS.

- [ ] **Step 10: Commit Task 3**

```bash
git add crates/ab-morph-run/src/lib.rs crates/ab-morph-run/src/main.rs crates/ab-morph-run/src/select.rs
git commit -m "feat: rerun full morph details by source id"
```

---

### Task 4: Periodic Progress Output

**Files:**
- Modify: `crates/ab-morph-run/src/main.rs`

- [ ] **Step 1: Write failing parser test for progress interval**

Add this test inside `crates/ab-morph-run/src/main.rs` test module:

```rust
#[test]
fn parses_progress_interval_seconds() {
    let args = Args::parse_from([
        "ab-morph-run",
        "analyze-aat",
        "--aat",
        "one.json",
        "--analyzer",
        "vibrato",
        "--analyses-output",
        "analyses.jsonl",
        "--progress",
        "--progress-interval-seconds",
        "5",
    ]);

    let Command::AnalyzeAat {
        progress,
        progress_interval_seconds,
        ..
    } = args.command
    else {
        panic!("expected analyze-aat command");
    };

    assert!(progress);
    assert_eq!(progress_interval_seconds, 5);
}
```

- [ ] **Step 2: Run parser test and verify red**

Run:

```bash
cargo test -p ab-morph-run parses_progress_interval_seconds
```

Expected: FAIL because `progress_interval_seconds` is not a field.

- [ ] **Step 3: Add the CLI field**

In the `AnalyzeAat` variant, add after `progress`:

```rust
        #[arg(long, default_value_t = 30)]
        progress_interval_seconds: u64,
```

Add `progress_interval_seconds` to the `Command::AnalyzeAat` destructuring in `main()`.

- [ ] **Step 4: Implement periodic progress thread**

Replace the direct `run_analyze_aat` block in the `AnalyzeAat` arm with:

```rust
            let start = Instant::now();
            let progress_stop = if progress {
                let interval = std::time::Duration::from_secs(progress_interval_seconds.max(1));
                Some(spawn_progress_thread(start, input_count, interval))
            } else {
                None
            };
            let result = ab_morph_run::run_analyze_aat(
                aat.as_deref(),
                aat_dir.as_deref(),
                &analyzer,
                &analyses_output,
                comparisons_output.as_deref(),
                errors_output.as_deref(),
                resume,
                jobs,
                output_profile,
                examples_output.as_deref(),
                max_examples_per_comparison,
                manifest_output.as_deref(),
            );
            if let Some(stop) = progress_stop {
                stop.stop();
            }
            if progress {
                emit_progress_summary(start, input_count);
            }
            result
```

Add this struct and helper near `emit_progress_summary`:

```rust
struct ProgressStop {
    stop: Option<std::sync::mpsc::Sender<()>>,
    handle: Option<std::thread::JoinHandle<()>>,
}

impl ProgressStop {
    fn stop(mut self) {
        if let Some(stop) = self.stop.take() {
            let _ = stop.send(());
        }
        if let Some(handle) = self.handle.take() {
            let _ = handle.join();
        }
    }
}

fn spawn_progress_thread(
    start: Instant,
    input_count: Option<usize>,
    interval: std::time::Duration,
) -> ProgressStop {
    let (stop, stopped) = std::sync::mpsc::channel();
    let handle = std::thread::spawn(move || {
        while stopped.recv_timeout(interval).is_err() {
            emit_progress_summary(start, input_count);
        }
    });
    ProgressStop {
        stop: Some(stop),
        handle: Some(handle),
    }
}
```

This emits elapsed/memory periodically, retains the existing final summary, and stops promptly when the run finishes. It does not count completed inputs in this increment because the runner does not expose cross-thread progress counters yet.

- [ ] **Step 5: Run progress parser test**

Run:

```bash
cargo test -p ab-morph-run parses_progress_interval_seconds
```

Expected: PASS.

- [ ] **Step 6: Commit Task 4**

```bash
git add crates/ab-morph-run/src/main.rs
git commit -m "feat: emit periodic morph run progress"
```

---

### Task 5: Workflow Documentation and Report Pointer

**Files:**
- Create: `docs/morph-corpus-workflow.md`
- Modify: `docs/superpowers/reports/2026-04-29-morph-compact-artifacts.md`

- [ ] **Step 1: Write workflow documentation**

Create `docs/morph-corpus-workflow.md`:

```markdown
# Morph Corpus Workflow

This is the standard workflow for complete morpheme comparison runs over checked AAT JSON.

## 1. Build tools

```bash
cargo build --release -p ab-check -p ab-morph-run
cargo build --release --manifest-path adapters/aozora-rs/Cargo.toml
```

## 2. Generate checked AAT

```bash
target/release/ab-check \
  --index scratch/ab-index.json \
  --corpus references/aozorabunko \
  --adapter adapters/aozora-rs/target/release/aozora-rs-adapter \
  --output scratch/morph-full-corpus/reports \
  --aat-output scratch/morph-full-corpus/aats \
  --jobs 16
```

## 3. Run compact morph comparison

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/morph-full-corpus/aats \
    --analyzer vibrato \
    --analyzer sudachi-c \
    --output-profile compact \
    --analyses-output scratch/morph-full-corpus-compact-canonical/analyses.jsonl.zst \
    --comparisons-output scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst \
    --examples-output scratch/morph-full-corpus-compact-canonical/examples.jsonl.zst \
    --errors-output scratch/morph-full-corpus-compact-canonical/errors.jsonl.zst \
    --manifest-output scratch/morph-full-corpus-compact-canonical/manifest.json \
    --jobs 8 \
    --progress \
    --progress-interval-seconds 30
```

Compact output is the default artifact for comprehensive runs. Full-detail output is available for targeted debugging, but it is too large as a default corpus artifact.

## 4. Summarize worst cases

Lowest boundary F1 by source record:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst \
  --group-by source-id \
  --sort-by boundary-f1 \
  --limit 20
```

Most segmentation regions by logical text id:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst \
  --group-by text-id \
  --sort-by segmentation-regions \
  --limit 20 \
  --json
```

## 5. Rerun full detail for selected sources

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  target/release/ab-morph-run rerun-full \
    --aat-dir scratch/morph-full-corpus/aats \
    --source-id 001529_50685-dd3b2fe4e5bf \
    --analyzer vibrato \
    --analyzer sudachi-c \
    --output-dir scratch/morph-targeted-full/001529_50685-dd3b2fe4e5bf
```

The rerun writes full `analyses.jsonl`, `comparisons.jsonl`, `errors.jsonl`, and `manifest.json` into the output directory.

## Identity policy

`source_id` is the stable row/run identity. It is derived from the AAT file stem and distinguishes duplicate records in the corpus index.

`text_id` is the logical work id. It is useful for grouped reporting, but multiple `source_id` values can share one `text_id`.

Reporting tools must make grouping explicit. Use `--group-by source-id` when investigating a concrete AAT file or rerunning full details. Use `--group-by text-id` when asking logical-work questions.
```

- [ ] **Step 2: Add report pointer**

Append this to `docs/superpowers/reports/2026-04-29-morph-compact-artifacts.md`:

```markdown
## Workflow tooling

The standard corpus workflow is documented in `docs/morph-corpus-workflow.md`. It covers checked AAT generation, compact corpus comparison, compact summary ranking, targeted full-detail reruns by `source_id`, and the `source_id` vs `text_id` grouping policy.
```

- [ ] **Step 3: Commit Task 5**

```bash
git add docs/morph-corpus-workflow.md docs/superpowers/reports/2026-04-29-morph-compact-artifacts.md
git commit -m "docs: document morph corpus workflow"
```

---

### Task 6: Final Verification

**Files:**
- No new source files beyond prior tasks.

- [ ] **Step 1: Run format check**

```bash
cargo fmt --all -- --check
```

Expected: PASS.

- [ ] **Step 2: Run `ab-morph-run` tests**

```bash
cargo test -p ab-morph-run
```

Expected: PASS.

- [ ] **Step 3: Build release binary**

```bash
cargo build --release -p ab-morph-run
```

Expected: PASS.

- [ ] **Step 4: Smoke compact summary command**

If `scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst` exists, run:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst \
  --group-by source-id \
  --sort-by boundary-f1 \
  --limit 5
```

Expected: PASS and print a tab-separated header plus at most 5 rows.

If the scratch artifact does not exist, skip this smoke and record that it was skipped because scratch artifacts are regenerable and not committed.

- [ ] **Step 5: Smoke JSON summary command**

If `scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst` exists, run:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst \
  --group-by text-id \
  --sort-by segmentation-regions \
  --limit 3 \
  --json
```

Expected: PASS and print a JSON array.

- [ ] **Step 6: Check worktree**

```bash
git status --short
```

Expected: no uncommitted source/doc changes. Scratch artifacts may be ignored and should not be committed.
