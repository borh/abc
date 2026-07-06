# Compact Morph Workflow Tools Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the smallest usable workflow around compact morph outputs: summarize worst cases, rerun selected sources in full mode, emit periodic progress, and document source-id/text-id policy.

**Architecture:** Keep `analyze-aat` as the main engine, but factor the runner so it can also accept an explicit selected AAT path list with separate manifest provenance. Add streaming compact-summary loading so comprehensive artifacts do not need to be decompressed into a single `String`. Expose the workflow through `summarize-compact`, `rerun-full`, and periodic progress on `analyze-aat`.

**Tech Stack:** Rust 2024, `clap`, `serde`/`serde_json`, `zstd`, existing compact JSONL row schemas.

---

## File Map

- Modify: `crates/ab-morph-run/src/output.rs` — add streaming JSONL / JSONL.zst line reading.
- Modify: `crates/ab-morph-run/src/lib.rs` — add selected-input runner entry point and preserve manifest provenance independently from selected file paths.
- Modify: `crates/ab-morph-run/src/main.rs` — add CLI subcommands and periodic progress orchestration.
- Create: `crates/ab-morph-run/src/summary.rs` — stream compact comparison JSONL, aggregate/rank by explicit grouping key, and serialize summary rows.
- Create: `crates/ab-morph-run/src/select.rs` — resolve AAT file paths by `source_id` for targeted full reruns.
- Create: `docs/morph-corpus-workflow.md` — standard corpus workflow and identity policy.
- Modify: `docs/superpowers/reports/2026-04-29-morph-compact-artifacts.md` — add a pointer to the workflow doc and tools.

## Public CLI Shape

```bash
ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst \
  --sort-by boundary-f1 \
  --group-by source-id \
  --limit 20

ab-morph-run rerun-full \
  --aat-dir scratch/morph-full-corpus/aats \
  --source-id 001529_50685-dd3b2fe4e5bf \
  --analyzer vibrato \
  --analyzer sudachi-c \
  --output-dir scratch/morph-targeted-full/001529_50685-dd3b2fe4e5bf \
  --jobs 2 \
  --examples-output scratch/morph-targeted-full/001529_50685-dd3b2fe4e5bf/examples.jsonl \
  --max-examples-per-comparison 100

ab-morph-run analyze-aat \
  --aat-dir scratch/morph-full-corpus/aats \
  --analyzer vibrato \
  --analyzer sudachi-c \
  --output-profile compact \
  --analyses-output scratch/out/analyses.jsonl.zst \
  --comparisons-output scratch/out/comparisons.jsonl.zst \
  --progress-interval-seconds 30
```

## Identity Policy

- `source_id` is the stable row/run identity. It is derived from the AAT file stem and distinguishes duplicate corpus index records.
- `text_id` is a logical grouping key. It can intentionally collapse multiple source records.
- Summary tooling defaults to `--group-by source-id`; `--group-by text-id` is explicit logical-work grouping.
- Targeted full rerun selects by `source_id`, not `text_id`, to avoid accidentally rerunning or skipping duplicate logical works.
- `boundary_f1 = null` is sorted as worse than any numeric value when sorting by `boundary-f1`, because undefined boundary metrics are inspection-worthy.

---

### Task 1: Streaming JSONL Input Helper

**Files:**
- Modify: `crates/ab-morph-run/src/output.rs`

- [ ] **Step 1: Add failing streaming-reader tests**

In `crates/ab-morph-run/src/output.rs`, add these tests inside the existing test module:

```rust
#[test]
fn streams_plain_jsonl_lines_without_materializing_file() {
    let dir = temp_dir("stream-plain");
    fs::create_dir_all(&dir).unwrap();
    let path = dir.join("rows.jsonl");
    fs::write(&path, "one\n\n two \n").unwrap();

    let mut rows = Vec::new();
    for_each_jsonl_or_zst_line(&path, |line| {
        rows.push(line.to_owned());
        Ok(())
    })
    .unwrap();

    assert_eq!(rows, vec!["one", " two "]);
    let _ = fs::remove_dir_all(dir);
}

#[test]
fn streams_zstd_jsonl_lines() {
    let dir = temp_dir("stream-zst");
    fs::create_dir_all(&dir).unwrap();
    let path = dir.join("rows.jsonl.zst");
    {
        let mut writer = open_output_writer(&path, false).unwrap();
        writer.write_all(b"one\ntwo\n").unwrap();
        writer.flush().unwrap();
    }

    let mut rows = Vec::new();
    for_each_jsonl_or_zst_line(&path, |line| {
        rows.push(line.to_owned());
        Ok(())
    })
    .unwrap();

    assert_eq!(rows, vec!["one", "two"]);
    let _ = fs::remove_dir_all(dir);
}
```

- [ ] **Step 2: Run tests and verify red**

```bash
cargo test -p ab-morph-run output::tests::streams_plain_jsonl_lines_without_materializing_file
cargo test -p ab-morph-run output::tests::streams_zstd_jsonl_lines
```

Expected: FAIL because `for_each_jsonl_or_zst_line` is not defined.

- [ ] **Step 3: Implement streaming reader**

In `crates/ab-morph-run/src/output.rs`, change imports to include `BufRead` and `BufReader`:

```rust
use std::io::{BufRead, BufReader, BufWriter, Write};
```

Add this function below `read_jsonl_or_zst_to_string`:

```rust
pub(crate) fn for_each_jsonl_or_zst_line(
    path: &Path,
    mut visit: impl FnMut(&str) -> Result<()>,
) -> Result<()> {
    let file = File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    let mut reader: Box<dyn BufRead> = if path.extension().and_then(|ext| ext.to_str()) == Some("zst") {
        Box::new(BufReader::new(zstd::stream::read::Decoder::new(file)?))
    } else {
        Box::new(BufReader::new(file))
    };

    let mut line = String::new();
    loop {
        line.clear();
        let read = reader.read_line(&mut line)?;
        if read == 0 {
            break;
        }
        let line = line.trim_end_matches(['\r', '\n']);
        if !line.trim().is_empty() {
            visit(line)?;
        }
    }
    Ok(())
}
```

- [ ] **Step 4: Run output tests and verify green**

```bash
cargo test -p ab-morph-run output::tests::streams_plain_jsonl_lines_without_materializing_file
cargo test -p ab-morph-run output::tests::streams_zstd_jsonl_lines
```

Expected: PASS.

- [ ] **Step 5: Commit Task 1**

```bash
git add crates/ab-morph-run/src/output.rs
git commit -m "feat: stream morph JSONL inputs"
```

---

### Task 2: Compact Summary Library

**Files:**
- Create: `crates/ab-morph-run/src/summary.rs`
- Modify: `crates/ab-morph-run/src/lib.rs`

- [ ] **Step 1: Add the summary module declaration**

In `crates/ab-morph-run/src/lib.rs`, add:

```rust
mod summary;
```

Add public re-exports near `OutputProfile`:

```rust
pub use summary::{
    CompactSummaryGroupBy, CompactSummaryOptions, CompactSummaryRow, CompactSummarySort,
    summarize_compact_comparisons,
};
```

- [ ] **Step 2: Write failing summary tests**

Create `crates/ab-morph-run/src/summary.rs`:

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
    fn boundary_f1_sort_puts_null_first_then_lowest_numeric() {
        let dir = temp_dir("boundary");
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

        assert_eq!(rows.iter().map(|row| row.key.as_str()).collect::<Vec<_>>(), vec!["src-c", "src-a"]);
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
        let unique = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
        std::env::temp_dir().join(format!("ab-morph-run-summary-{label}-{}-{unique}", std::process::id()))
    }
}
```

- [ ] **Step 3: Run summary tests and verify red**

```bash
cargo test -p ab-morph-run summary::tests
```

Expected: tests compile and fail with the `unimplemented!` panic.

- [ ] **Step 4: Implement streaming summary loading, grouping, and sorting**

Replace `summary.rs` above the test module with:

```rust
use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use anyhow::Result;
use serde::Serialize;

use crate::compact::ComparisonSummaryRow;
use crate::output::for_each_jsonl_or_zst_line;

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
    saw_null_boundary_f1: bool,
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
    let mut groups = BTreeMap::<String, Accumulator>::new();
    for_each_jsonl_or_zst_line(comparisons_path, |line| {
        let row: ComparisonSummaryRow = serde_json::from_str(line)?;
        let key = match options.group_by {
            CompactSummaryGroupBy::SourceId => row.source_id.clone(),
            CompactSummaryGroupBy::TextId => row.text_id.clone(),
        };
        groups.entry(key).or_default().push(row);
        Ok(())
    })?;

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
        match row.boundary_f1 {
            Some(value) => {
                self.worst_boundary_f1 = Some(self.worst_boundary_f1.map_or(value, |old| old.min(value)));
            }
            None => self.saw_null_boundary_f1 = true,
        }
        self.total_segmentation_regions += row.segmentation_regions;
        self.total_feature_difference_regions += row.one_to_one_with_feature_differences;
        self.total_coverage_mismatch_regions += row.coverage_mismatch_regions;
        self.max_segmentation_regions = self.max_segmentation_regions.max(row.segmentation_regions);
        self.max_feature_difference_regions = self.max_feature_difference_regions.max(row.one_to_one_with_feature_differences);
        self.max_coverage_mismatch_regions = self.max_coverage_mismatch_regions.max(row.coverage_mismatch_regions);
    }

    fn into_row(self, key: String) -> CompactSummaryRow {
        CompactSummaryRow {
            key,
            source_ids: self.source_ids.into_iter().collect(),
            text_ids: self.text_ids.into_iter().collect(),
            comparisons: self.comparisons,
            worst_boundary_f1: if self.saw_null_boundary_f1 { None } else { self.worst_boundary_f1 },
            total_segmentation_regions: self.total_segmentation_regions,
            total_feature_difference_regions: self.total_feature_difference_regions,
            total_coverage_mismatch_regions: self.total_coverage_mismatch_regions,
            max_segmentation_regions: self.max_segmentation_regions,
            max_feature_difference_regions: self.max_feature_difference_regions,
            max_coverage_mismatch_regions: self.max_coverage_mismatch_regions,
        }
    }
}

fn compare_rows(left: &CompactSummaryRow, right: &CompactSummaryRow, sort_by: CompactSummarySort) -> std::cmp::Ordering {
    match sort_by {
        CompactSummarySort::BoundaryF1 => compare_boundary_f1(left, right),
        CompactSummarySort::SegmentationRegions => right.total_segmentation_regions.cmp(&left.total_segmentation_regions).then_with(|| left.key.cmp(&right.key)),
        CompactSummarySort::FeatureDifferences => right.total_feature_difference_regions.cmp(&left.total_feature_difference_regions).then_with(|| left.key.cmp(&right.key)),
        CompactSummarySort::CoverageMismatchRegions => right.total_coverage_mismatch_regions.cmp(&left.total_coverage_mismatch_regions).then_with(|| left.key.cmp(&right.key)),
    }
}

fn compare_boundary_f1(left: &CompactSummaryRow, right: &CompactSummaryRow) -> std::cmp::Ordering {
    match (left.worst_boundary_f1, right.worst_boundary_f1) {
        (None, Some(_)) => std::cmp::Ordering::Less,
        (Some(_), None) => std::cmp::Ordering::Greater,
        (None, None) => left.key.cmp(&right.key),
        (Some(left), Some(right)) => left.partial_cmp(&right).unwrap_or(std::cmp::Ordering::Equal),
    }
    .then_with(|| left.key.cmp(&right.key))
}
```

Keep the test module from Step 2 below this implementation.

- [ ] **Step 5: Run summary tests and verify green**

```bash
cargo test -p ab-morph-run summary::tests
```

Expected: PASS.

- [ ] **Step 6: Commit Task 2**

```bash
git add crates/ab-morph-run/src/lib.rs crates/ab-morph-run/src/summary.rs
git commit -m "feat: summarize compact morph comparisons"
```

---

### Task 3: `summarize-compact` CLI

**Files:**
- Modify: `crates/ab-morph-run/src/main.rs`

- [ ] **Step 1: Write failing parser test for `summarize-compact`**

In `crates/ab-morph-run/src/main.rs`, add the CLI enum types above `Command`:

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

Add this test inside the existing test module:

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

    let Command::SummarizeCompact { comparisons, group_by, sort_by, limit, json } = args.command else {
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

```bash
cargo test -p ab-morph-run parses_summarize_compact_command
```

Expected: FAIL because `Command::SummarizeCompact` is not defined.

- [ ] **Step 3: Add CLI variant and mapper helpers**

Add this `Command` variant after `AnalyzeAat`:

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

Add helpers below `Command`:

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

Add this `match` arm:

```rust
        Command::SummarizeCompact { comparisons, group_by, sort_by, limit, json } => {
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

Add the table helper near `emit_progress_summary`:

```rust
fn print_summary_table(rows: &[ab_morph_run::CompactSummaryRow]) {
    println!(
        "key\tsource_ids\ttext_ids\tcomparisons\tworst_boundary_f1\ttotal_segmentation_regions\ttotal_feature_difference_regions\ttotal_coverage_mismatch_regions"
    );
    for row in rows {
        println!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            row.key,
            row.source_ids.join(","),
            row.text_ids.join(","),
            row.comparisons,
            row.worst_boundary_f1.map(|value| value.to_string()).unwrap_or_else(|| "null".to_owned()),
            row.total_segmentation_regions,
            row.total_feature_difference_regions,
            row.total_coverage_mismatch_regions,
        );
    }
}
```

- [ ] **Step 5: Run CLI tests and summary tests**

```bash
cargo test -p ab-morph-run parses_summarize_compact_command
cargo test -p ab-morph-run summary::tests
```

Expected: PASS.

- [ ] **Step 6: Commit Task 3**

```bash
git add crates/ab-morph-run/src/main.rs
git commit -m "feat: add compact summary CLI"
```

---

### Task 4: Selected-Input Runner and Source-ID Selection

**Files:**
- Create: `crates/ab-morph-run/src/select.rs`
- Modify: `crates/ab-morph-run/src/lib.rs`

- [ ] **Step 1: Add selector module and failing tests**

Create `crates/ab-morph-run/src/select.rs`:

```rust
use std::path::{Path, PathBuf};

use anyhow::Result;

use crate::compact::source_id_from_aat_path;

pub fn resolve_source_id_aat_paths(_aat_dir: &Path, _source_ids: &[String]) -> Result<Vec<PathBuf>> {
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

        let paths = resolve_source_id_aat_paths(&dir, &["src-a".to_owned(), "src-b".to_owned()]).unwrap();

        assert_eq!(paths.len(), 2);
        assert_eq!(source_id_from_aat_path(&paths[0]), "src-a");
        assert_eq!(source_id_from_aat_path(&paths[1]), "src-b");
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn duplicate_source_id_is_an_error() {
        let dir = temp_dir("duplicate");
        let one = dir.join("one");
        let two = dir.join("two");
        fs::create_dir_all(&one).unwrap();
        fs::create_dir_all(&two).unwrap();
        fs::write(one.join("same.json"), "{}").unwrap();
        fs::write(two.join("same.json"), "{}").unwrap();

        let error = resolve_source_id_aat_paths(&dir, &["same".to_owned()]).unwrap_err().to_string();

        assert!(error.contains("duplicate source_id"));
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn missing_source_id_is_an_error() {
        let dir = temp_dir("missing");
        fs::create_dir_all(&dir).unwrap();

        let error = resolve_source_id_aat_paths(&dir, &["missing".to_owned()]).unwrap_err().to_string();

        assert!(error.contains("missing source_id"));
        let _ = fs::remove_dir_all(dir);
    }

    fn temp_dir(label: &str) -> std::path::PathBuf {
        let unique = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
        std::env::temp_dir().join(format!("ab-morph-run-select-{label}-{}-{unique}", std::process::id()))
    }
}
```

In `lib.rs`, add:

```rust
mod select;
pub use select::resolve_source_id_aat_paths;
```

- [ ] **Step 2: Run selector tests and verify red**

```bash
cargo test -p ab-morph-run select::tests
```

Expected: FAIL with `unimplemented!` panic.

- [ ] **Step 3: Implement selector**

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
            if requested.contains(&source_id) && discovered.insert(source_id.clone(), path).is_some() {
                bail!("duplicate source_id `{source_id}` under {}", dir.display());
            }
        }
    }
    Ok(())
}
```

- [ ] **Step 4: Refactor runner to preserve provenance for selected inputs**

In `crates/ab-morph-run/src/lib.rs`, introduce this public function next to `run_analyze_aat`:

```rust
#[allow(clippy::too_many_arguments)]
pub fn run_analyze_aat_selected(
    inputs: Vec<PathBuf>,
    input_mode: &str,
    input_path: &str,
    analyzer_ids: &[String],
    analyses_output: &Path,
    comparisons_output: Option<&Path>,
    errors_output: Option<&Path>,
    resume: bool,
    jobs: usize,
    output_profile: OutputProfile,
    examples_output: Option<&Path>,
    max_examples_per_comparison: usize,
    manifest_output: Option<&Path>,
) -> Result<()> {
    if inputs.is_empty() {
        bail!("provide at least one AAT input");
    }
    run_analyze_aat_inputs(
        inputs,
        input_mode,
        input_path,
        analyzer_ids,
        analyses_output,
        comparisons_output,
        errors_output,
        resume,
        jobs,
        output_profile,
        examples_output,
        max_examples_per_comparison,
        manifest_output,
    )
}
```

Then change `run_analyze_aat` so after validating input mode, analyzer list, and jobs it calls a new private helper:

```rust
let inputs = discover_aat_inputs(aat, aat_dir)?;
let input_mode = if aat.is_some() { "aat" } else { "aat_dir" };
let input_path = aat.or(aat_dir).map(|path| path.display().to_string()).unwrap_or_default();
run_analyze_aat_inputs(
    inputs,
    input_mode,
    &input_path,
    analyzer_ids,
    analyses_output,
    comparisons_output,
    errors_output,
    resume,
    jobs,
    output_profile,
    examples_output,
    max_examples_per_comparison,
    manifest_output,
)
```

Move the existing analyzer loading, serial/parallel execution, and manifest writing body into a private helper:

```rust
#[allow(clippy::too_many_arguments)]
fn run_analyze_aat_inputs(
    inputs: Vec<PathBuf>,
    input_mode: &str,
    input_path: &str,
    analyzer_ids: &[String],
    analyses_output: &Path,
    comparisons_output: Option<&Path>,
    errors_output: Option<&Path>,
    resume: bool,
    jobs: usize,
    output_profile: OutputProfile,
    examples_output: Option<&Path>,
    max_examples_per_comparison: usize,
    manifest_output: Option<&Path>,
) -> Result<()> {
    if analyzer_ids.is_empty() {
        bail!("provide at least one --analyzer");
    }
    if jobs == 0 {
        bail!("--jobs must be at least 1");
    }

    let input_file_count = inputs.len();
    let specs = parse_analyzer_specs(analyzer_ids)?;
    let analyzers = load_analyzers(&specs)?;

    if jobs > 1 {
        run_analyze_aat_parallel(
            inputs,
            analyzers,
            analyses_output,
            comparisons_output,
            errors_output,
            resume,
            jobs,
            output_profile,
            examples_output,
            max_examples_per_comparison,
        )?;
    } else {
        run_analyze_aat_serial(
            inputs,
            &analyzers,
            analyses_output,
            comparisons_output,
            errors_output,
            resume,
            output_profile,
            examples_output,
            max_examples_per_comparison,
        )?;
    }

    if let Some(path) = manifest_output {
        write_manifest(
            path,
            output_profile,
            analyzer_ids,
            jobs,
            input_mode,
            input_path,
            input_file_count,
            analyses_output,
            comparisons_output,
            examples_output,
            errors_output,
        )?;
    }

    Ok(())
}
```

Change `write_manifest` parameters from `aat: Option<&Path>, aat_dir: Option<&Path>` to `input_mode: &str, input_path: &str`, and set:

```rust
input_mode: input_mode.to_owned(),
input_path: input_path.to_owned(),
```

- [ ] **Step 5: Add runner provenance regression test**

In `lib.rs` tests, add:

```rust
#[test]
fn selected_runner_manifest_preserves_original_input_path() {
    let dir = temp_dir("selected-manifest");
    let aat_dir = dir.join("aats");
    fs::create_dir_all(&aat_dir).unwrap();
    let input = aat_dir.join("source-a.json");
    fs::write(&input, TINY_AAT).unwrap();

    let out = dir.join("out");
    fs::create_dir_all(&out).unwrap();
    run_analyze_aat_selected(
        vec![input],
        "aat_dir",
        &aat_dir.display().to_string(),
        &["vibrato".to_owned()],
        &out.join("analyses.jsonl"),
        None,
        Some(&out.join("errors.jsonl")),
        false,
        1,
        OutputProfile::Compact,
        None,
        10,
        Some(&out.join("manifest.json")),
    )
    .unwrap();

    let manifest: serde_json::Value = serde_json::from_str(&fs::read_to_string(out.join("manifest.json")).unwrap()).unwrap();
    assert_eq!(manifest["input_path"], aat_dir.display().to_string());
    assert_eq!(manifest["input_file_count"], 1);
    let _ = fs::remove_dir_all(dir);
}
```

- [ ] **Step 6: Run selector and provenance tests**

```bash
cargo test -p ab-morph-run select::tests
cargo test -p ab-morph-run selected_runner_manifest_preserves_original_input_path
```

Expected: PASS.

- [ ] **Step 7: Commit Task 4**

```bash
git add crates/ab-morph-run/src/lib.rs crates/ab-morph-run/src/select.rs
git commit -m "feat: select morph AAT inputs by source id"
```

---

### Task 5: `rerun-full` CLI

**Files:**
- Modify: `crates/ab-morph-run/src/main.rs`

- [ ] **Step 1: Add failing parser test for `rerun-full` options**

Add this test to `main.rs` tests:

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
        "--jobs",
        "2",
        "--examples-output",
        "examples.jsonl",
        "--max-examples-per-comparison",
        "50",
    ]);

    let Command::RerunFull { aat_dir, source_id, analyzer, output_dir, jobs, examples_output, max_examples_per_comparison } = args.command else {
        panic!("expected rerun-full command");
    };

    assert_eq!(aat_dir, PathBuf::from("aats"));
    assert_eq!(source_id, vec!["src-a".to_owned(), "src-b".to_owned()]);
    assert_eq!(analyzer, vec!["vibrato".to_owned(), "sudachi-c".to_owned()]);
    assert_eq!(output_dir, PathBuf::from("full-out"));
    assert_eq!(jobs, 2);
    assert_eq!(examples_output, Some(PathBuf::from("examples.jsonl")));
    assert_eq!(max_examples_per_comparison, 50);
}
```

- [ ] **Step 2: Run parser test and verify red**

```bash
cargo test -p ab-morph-run parses_rerun_full_command
```

Expected: FAIL because `Command::RerunFull` is not defined.

- [ ] **Step 3: Add CLI variant and a testable helper**

Add this variant after `SummarizeCompact`:

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
        #[arg(long, default_value_t = 1)]
        jobs: usize,
        #[arg(long)]
        examples_output: Option<PathBuf>,
        #[arg(long, default_value_t = 10)]
        max_examples_per_comparison: usize,
    },
```

Add this helper near `print_summary_table`:

```rust
#[allow(clippy::too_many_arguments)]
fn run_rerun_full(
    aat_dir: &Path,
    source_ids: &[String],
    analyzer: &[String],
    output_dir: &Path,
    jobs: usize,
    examples_output: Option<&Path>,
    max_examples_per_comparison: usize,
) -> Result<()> {
    let inputs = ab_morph_run::resolve_source_id_aat_paths(aat_dir, source_ids)?;
    ab_morph_run::run_analyze_aat_selected(
        inputs,
        "aat_dir",
        &aat_dir.display().to_string(),
        analyzer,
        &output_dir.join("analyses.jsonl"),
        Some(&output_dir.join("comparisons.jsonl")),
        Some(&output_dir.join("errors.jsonl")),
        false,
        jobs,
        ab_morph_run::OutputProfile::Full,
        examples_output,
        max_examples_per_comparison,
        Some(&output_dir.join("manifest.json")),
    )
}
```

Add this match arm:

```rust
        Command::RerunFull {
            aat_dir,
            source_id,
            analyzer,
            output_dir,
            jobs,
            examples_output,
            max_examples_per_comparison,
        } => run_rerun_full(
            &aat_dir,
            &source_id,
            &analyzer,
            &output_dir,
            jobs,
            examples_output.as_deref(),
            max_examples_per_comparison,
        ),
```

- [ ] **Step 4: Add end-to-end helper test**

Add this test to `main.rs` tests:

```rust
#[test]
fn rerun_full_writes_manifest_with_original_aat_dir() {
    const TINY_AAT: &str = r#"{"version":1,"work_id":"source-a","blocks":[{"kind":"paragraph","content":[{"kind":"text","value":"吾輩は猫である。"}]}],"meta":{"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}}"#;
    let dir = temp_dir("rerun-full");
    let aat_dir = dir.join("aats");
    let out = dir.join("out");
    fs::create_dir_all(&aat_dir).unwrap();
    fs::create_dir_all(&out).unwrap();
    fs::write(aat_dir.join("source-a.json"), TINY_AAT).unwrap();

    run_rerun_full(
        &aat_dir,
        &["source-a".to_owned()],
        &["vibrato".to_owned()],
        &out,
        1,
        None,
        10,
    )
    .unwrap();

    let manifest: serde_json::Value = serde_json::from_str(&fs::read_to_string(out.join("manifest.json")).unwrap()).unwrap();
    assert_eq!(manifest["input_path"], aat_dir.display().to_string());
    assert!(out.join("analyses.jsonl").exists());
    let _ = fs::remove_dir_all(dir);
}
```

If `main.rs` tests do not already have `temp_dir`, add:

```rust
fn temp_dir(label: &str) -> PathBuf {
    let unique = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    std::env::temp_dir().join(format!("ab-morph-run-main-{label}-{}-{unique}", std::process::id()))
}
```

- [ ] **Step 5: Run parser and end-to-end tests**

```bash
cargo test -p ab-morph-run parses_rerun_full_command
cargo test -p ab-morph-run rerun_full_writes_manifest_with_original_aat_dir
```

Expected: PASS.

- [ ] **Step 6: Commit Task 5**

```bash
git add crates/ab-morph-run/src/main.rs
git commit -m "feat: add targeted full morph rerun CLI"
```

---

### Task 6: Periodic Progress Output

**Files:**
- Modify: `crates/ab-morph-run/src/main.rs`

- [ ] **Step 1: Write failing parser test for progress interval implying progress**

Add this test to `main.rs` tests:

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
        "--progress-interval-seconds",
        "5",
    ]);

    let Command::AnalyzeAat { progress, progress_interval_seconds, .. } = args.command else {
        panic!("expected analyze-aat command");
    };

    assert!(!progress);
    assert_eq!(progress_interval_seconds, Some(5));
}
```

- [ ] **Step 2: Run parser test and verify red**

```bash
cargo test -p ab-morph-run parses_progress_interval_seconds
```

Expected: FAIL because `progress_interval_seconds` is not a field.

- [ ] **Step 3: Add optional progress interval field**

In `AnalyzeAat`, add:

```rust
        #[arg(long)]
        progress_interval_seconds: Option<u64>,
```

Add it to the `Command::AnalyzeAat` destructuring. Replace the existing `input_count` setup with:

```rust
let progress_enabled = progress || progress_interval_seconds.is_some();
let progress_interval_seconds = progress_interval_seconds.unwrap_or(30).max(1);
let input_count = if progress_enabled {
    Some(count_aat_json_inputs(aat.as_deref(), aat_dir.as_deref())?)
} else {
    None
};
```

Use `progress_enabled` anywhere the old code used `progress`.

- [ ] **Step 4: Implement immediate and periodic progress thread**

Replace the direct progress setup with:

```rust
let start = Instant::now();
let progress_stop = if progress_enabled {
    emit_progress_summary(start, input_count);
    Some(spawn_progress_thread(
        start,
        input_count,
        std::time::Duration::from_secs(progress_interval_seconds),
    ))
} else {
    None
};
```

Add this helper near `emit_progress_summary`:

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
    ProgressStop { stop: Some(stop), handle: Some(handle) }
}
```

After `run_analyze_aat` returns, keep:

```rust
if let Some(stop) = progress_stop {
    stop.stop();
}
if progress_enabled {
    emit_progress_summary(start, input_count);
}
```

- [ ] **Step 5: Run progress parser test**

```bash
cargo test -p ab-morph-run parses_progress_interval_seconds
```

Expected: PASS.

- [ ] **Step 6: Commit Task 6**

```bash
git add crates/ab-morph-run/src/main.rs
git commit -m "feat: emit periodic morph run progress"
```

---

### Task 7: Workflow Documentation and Report Pointer

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
    --progress-interval-seconds 30
```

Compact output is the default artifact for comprehensive runs. Full-detail output is available for targeted debugging, but it is too large as a default corpus artifact.

## 4. Summarize worst cases

Summary commands are dictionary-free. They read compact JSONL artifacts and do not load Vibrato or Sudachi.

Lowest boundary F1 by source record. Rows with undefined boundary F1 (`null`) sort before numeric scores because they need explicit inspection:

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

The tabular output includes `source_ids` and `text_ids`. Use `--json` when membership lists are too long for comfortable terminal reading.

## 5. Rerun full detail for selected sources

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  target/release/ab-morph-run rerun-full \
    --aat-dir scratch/morph-full-corpus/aats \
    --source-id 001529_50685-dd3b2fe4e5bf \
    --analyzer vibrato \
    --analyzer sudachi-c \
    --output-dir scratch/morph-targeted-full/001529_50685-dd3b2fe4e5bf \
    --jobs 2 \
    --examples-output scratch/morph-targeted-full/001529_50685-dd3b2fe4e5bf/examples.jsonl \
    --max-examples-per-comparison 100
```

The rerun writes full `analyses.jsonl`, `comparisons.jsonl`, `errors.jsonl`, and `manifest.json` into the output directory. If `--examples-output` is provided, it also writes bounded example rows for quick inspection.

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

- [ ] **Step 3: Commit Task 7**

```bash
git add docs/morph-corpus-workflow.md docs/superpowers/reports/2026-04-29-morph-compact-artifacts.md
git commit -m "docs: document morph corpus workflow"
```

---

### Task 8: Final Verification

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

- [ ] **Step 4: Smoke compact summary command if scratch artifact exists**

If `scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst` exists, run:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst \
  --group-by source-id \
  --sort-by boundary-f1 \
  --limit 5
```

Expected: PASS and print a tab-separated header plus at most 5 rows.

- [ ] **Step 5: Smoke JSON summary command if scratch artifact exists**

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst \
  --group-by text-id \
  --sort-by segmentation-regions \
  --limit 3 \
  --json
```

Expected: PASS and print a JSON array. If scratch artifact does not exist, record that both smoke steps were skipped because scratch artifacts are regenerable and not committed.

- [ ] **Step 6: Check worktree**

```bash
git status --short
```

Expected: no uncommitted source/doc changes. Scratch artifacts may be ignored and should not be committed.
