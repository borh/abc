# Morph Results Warehouse Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a canonical Parquet-backed morph result warehouse that persists comprehensive analyzer, morpheme, feature, and N-way disagreement facts without using JSONL as the durable artifact.

**Architecture:** Warehouse mode is an output sink inside the existing `ab-morph-run` analysis loop, not a second analyzer runner. A run is written to `.staging/<run_id>.<pid>/` and published by atomic rename to `runs/<run_id>/`; every row carries `run_id`, while the path is only a publication boundary. DuckDB reads Parquet directly through generated SQL helper files; JSONL remains a deprecated/debug path in phase 1 (status: deprecated) and is not extended as the canonical store.

**Tech Stack:** Rust 2024, `ab-morph-run`, `ab-morph-diff`, Apache Arrow/Parquet Rust crates, existing Vibrato/Sudachi analyzers, DuckDB SQL over Parquet.

---

## Review Decisions Incorporated

- JSONL is not the warehouse format. Phase 1 does not delete the existing JSONL writer because existing summary commands and targeted-debug workflows depend on it, but warehouse mode is mutually exclusive with JSONL outputs and no new canonical facts are added only to JSONL.
- `source_id` and `text_id` have explicit cardinality. A `source_id` is one AAT source record/file within a run. A `text_id` is the logical work id. Many `source_id` values may share one `text_id`; every fact table is keyed by `source_id`, while `text_id` is a grouping attribute.
- N-way region storage is collapsed to three tables: `nway_regions`, `nway_region_analyzers`, and `nway_feature_diffs`. Segmentation patterns are derived from analyzer surface sequences, not stored in a separate pattern table.
- `nway_region_analyzers.surfaces` uses Parquet `list<utf8>`. There is no tab-separated or JSON-in-string micro-format.
- `boundary_count` is removed. It is derivable from morphemes and was ambiguous in the prior plan.
- `source_excerpt`, `source_script_category`, and region `script_category` are removed from canonical tables. Region whitespace is retained as `is_nonempty_whitespace`: non-empty region text whose characters are all whitespace. Empty regions are not whitespace evidence.
- `runs.status` is removed. Presence under `runs/<run_id>/` plus a readable `runs.parquet` row is the completion signal.
- Analyzer metadata fails closed. `analyzer_family` is `vibrato` or `sudachi` in phase 1; unsupported analyzer specs are already errors.
- Errors include stable `error_code` plus controlled `stage`; consumers do not need to parse free-form messages for grouping.
- Warehouse mode is serial in phase 1, rejects `--jobs != 1` before doing I/O, and rejects `--resume`. Resume over immutable Parquet runs is deferred. Warehouse writers are append-style so the shared loop flushes per-source batches instead of accumulating the full corpus in memory.
- Stale staging directories for the same `run_id` are cleaned on writer creation before new staging begins.
- Arrow/Parquet compile cost is accepted as the cost of making Parquet canonical. Keep all Arrow-family crates on one major version.
- DuckDB is the only warehouse query surface in phase 1. Rust warehouse readers and summary commands over Parquet are deferred. Generated `views.sql` is sealed into each run directory and uses absolute paths; if a run is moved, regenerate views or query Parquet paths directly.

---

## File Map

- Modify `Cargo.toml`
  - Add Arrow/Parquet workspace dependencies.
- Modify `crates/ab-morph-run/Cargo.toml`
  - Depend on Arrow/Parquet workspace crates.
- Create `crates/ab-morph-run/src/warehouse/mod.rs`
  - Warehouse module boundary.
- Create `crates/ab-morph-run/src/warehouse/schema.rs`
  - Table names, schema version, row structs, path helpers, scope invariants.
- Create `crates/ab-morph-run/src/warehouse/writer.rs`
  - Append-style Parquet writers, empty-table schemas, stale-staging cleanup, atomic finalization.
- Create `crates/ab-morph-run/src/warehouse/rows.rs`
  - Mapping from `Analysis`, morphemes, features, and N-way regions into warehouse rows.
- Create `crates/ab-morph-run/src/warehouse/sql.rs`
  - Generated SQL helper text and per-run SQL path writing.
- Modify `crates/ab-morph-run/src/lib.rs`
  - Refactor existing serial analysis loop to write through either JSONL outputs or warehouse outputs.
- Modify `crates/ab-morph-run/src/main.rs`
  - Add `--warehouse-dir` and `--run-id`; validate warehouse incompatibilities early.
- Create `crates/ab-morph-run/sql/schema.sql`
  - Human/query-tool schema descriptor and invariants.
- Create `crates/ab-morph-run/sql/morph_views.sql`
  - Template DuckDB views over canonical Parquet tables.
- Modify `docs/morph-corpus-workflow.md`
  - Document warehouse mode as the preferred comprehensive artifact.
- Create `docs/superpowers/reports/2026-05-01-morph-warehouse-smoke.md`
  - Record smoke command, table row counts, DuckDB checks, and the no-JSONL warehouse invariant.

---

## Data Protocol

### Sealed run layout

Warehouse mode writes to staging first:

```text
<warehouse-dir>/.staging/<run-id>.<pid>/
  runs.parquet
  sources.parquet
  run_analyzers.parquet
  analyses.parquet
  morphemes.parquet
  morpheme_features.parquet
  nway_regions.parquet
  nway_region_analyzers.parquet
  nway_feature_diffs.parquet
  errors.parquet
  views.sql
```

Finalization writes `views.sql` into the staging directory, then renames the complete staging directory to:

```text
<warehouse-dir>/runs/<run-id>/
```

Readers ignore `.staging`. A run is complete iff its directory exists under `runs/` and contains a readable `runs.parquet` row for that `run_id`. `views.sql` is part of the sealed per-run artifact and contains absolute paths for that run; if the run directory is moved, regenerate the view file or query the Parquet files directly.

### Canonical tables

All fact rows include `run_id`. `source_id` is the primary source key. `text_id` is copied into rows to make logical-work grouping cheap and explicit.

`runs.parquet`

```text
schema_version: u32
run_id: utf8
created_at_utc: utf8          -- RFC3339
input_mode: utf8              -- aat | aat_dir
input_path: utf8
source_count: u64
analyzer_count: u64
error_count: u64
```

Schema-version reader policy: phase-1 SQL helpers and future Rust readers support only `schema_version = 1` and must reject any other value.

`run_analyzers.parquet`

```text
run_id: utf8
analyzer_id: utf8             -- runtime analyzer id, e.g. vibrato:unidic-cwj-...
analyzer_arg: utf8            -- CLI arg, e.g. vibrato | sudachi-a | sudachi-c
analyzer_family: utf8         -- vibrato | sudachi
```

`analyzer_family` is fail-closed. If an analyzer arg cannot be classified, warehouse creation returns an error rather than writing `unknown`.

`sources.parquet`

```text
run_id: utf8
source_id: utf8               -- AAT file stem, unique within run
text_id: utf8                 -- logical work id, many source_id can share one text_id
aat_path: utf8
source_bytes: u64              -- projected plaintext byte length, not AAT file size
source_chars: u64              -- projected plaintext char count
```

`analyses.parquet`

```text
run_id: utf8
source_id: utf8
text_id: utf8
analyzer_id: utf8
morpheme_count: u64
```

This table preserves successful zero-morpheme analyses. Non-zero `morpheme_count` is derivable from `morphemes.parquet`, but a source/analyzer pair with zero morphemes would otherwise disappear from the fact set.

`morphemes.parquet`

```text
run_id: utf8
source_id: utf8
text_id: utf8
analyzer_id: utf8
morpheme_index: u64
byte_start: u64
byte_end: u64
char_start: u64
char_end: u64
surface: utf8
```

`morpheme_features.parquet`

```text
run_id: utf8
source_id: utf8
text_id: utf8
analyzer_id: utf8
morpheme_index: u64
feature_key: utf8
feature_value: utf8 nullable
```

`nway_regions.parquet`

```text
run_id: utf8
source_id: utf8
text_id: utf8
region_index: u64
byte_start: u64
byte_end: u64
char_start: u64
char_end: u64
is_nonempty_whitespace: bool
is_agreement: bool
has_coverage_mismatch: bool
has_segmentation_disagreement: bool
has_feature_disagreement: bool
```

`nway_region_analyzers.parquet`

```text
run_id: utf8
source_id: utf8
text_id: utf8
region_index: u64
analyzer_id: utf8
covers_exactly: bool
morpheme_start: u64
morpheme_end: u64
surfaces: list<utf8>
```

`surfaces` is ordered by morpheme order in that analyzer for the region. The list is part of one region/analyzer value, so Parquet list type is appropriate.

`nway_feature_diffs.parquet`

```text
run_id: utf8
source_id: utf8
text_id: utf8
region_index: u64
feature_key: utf8
scope_type: utf8             -- whole_region | token_position | surface
scope_position: u64 nullable
scope_surface: utf8 nullable
feature_value: utf8 nullable
analyzer_id: utf8
```

Scope invariant:

```text
scope_type = 'whole_region'   => scope_position IS NULL AND scope_surface IS NULL
scope_type = 'token_position' => scope_position IS NOT NULL AND scope_surface IS NULL
scope_type = 'surface'        => scope_position IS NULL AND scope_surface IS NOT NULL
```

`errors.parquet`

```text
run_id: utf8
source_id: utf8 nullable
text_id: utf8 nullable
analyzer_id: utf8 nullable
stage: utf8                  -- read_aat | project_aat | analyze | compare_nway | write
error_code: utf8             -- stable grouping code
message: utf8
```

---

## Task 1: Add warehouse schema and CLI surface

**Files:**
- Modify: `Cargo.toml`
- Modify: `crates/ab-morph-run/Cargo.toml`
- Modify: `crates/ab-morph-run/src/lib.rs`
- Modify: `crates/ab-morph-run/src/main.rs`
- Create: `crates/ab-morph-run/src/warehouse/mod.rs`
- Create: `crates/ab-morph-run/src/warehouse/schema.rs`

- [ ] **Step 1: Add failing CLI tests**

Add these tests to `crates/ab-morph-run/src/main.rs` inside the existing test module:

```rust
#[test]
fn parses_warehouse_mode_without_jsonl_outputs() {
    let args = Args::try_parse_from([
        "ab-morph-run",
        "analyze-aat",
        "--aat-dir",
        "scratch/aats",
        "--analyzer",
        "vibrato",
        "--warehouse-dir",
        "scratch/morph-warehouse",
        "--run-id",
        "smoke-2026-05-01",
    ])
    .unwrap();

    let Command::AnalyzeAat {
        warehouse_dir,
        run_id,
        output_dir,
        analyses_output,
        ..
    } = args.command
    else {
        panic!("expected analyze-aat");
    };

    assert_eq!(warehouse_dir, Some(PathBuf::from("scratch/morph-warehouse")));
    assert_eq!(run_id, Some("smoke-2026-05-01".to_owned()));
    assert_eq!(output_dir, None);
    assert_eq!(analyses_output, None);
}

#[test]
fn rejects_warehouse_with_jsonl_output_dir() {
    let err = Args::try_parse_from([
        "ab-morph-run",
        "analyze-aat",
        "--aat-dir",
        "scratch/aats",
        "--analyzer",
        "vibrato",
        "--warehouse-dir",
        "scratch/morph-warehouse",
        "--run-id",
        "smoke-2026-05-01",
        "--output-dir",
        "scratch/jsonl",
    ])
    .unwrap_err();

    assert_eq!(err.kind(), clap::error::ErrorKind::ArgumentConflict);
}

#[test]
fn warehouse_validation_rejects_resume_and_parallel_jobs() {
    let err = validate_warehouse_cli(Some(&PathBuf::from("scratch/warehouse")), Some("run-a"), true, 1)
        .unwrap_err()
        .to_string();
    assert!(err.contains("does not support --resume"));

    let err = validate_warehouse_cli(Some(&PathBuf::from("scratch/warehouse")), Some("run-a"), false, 2)
        .unwrap_err()
        .to_string();
    assert!(err.contains("requires --jobs 1"));
}
```

- [ ] **Step 2: Run the failing tests**

Run:

```bash
cargo test -p ab-morph-run --bin ab-morph-run warehouse
```

Expected: FAIL because the warehouse CLI fields and `validate_warehouse_cli` do not exist.

- [ ] **Step 3: Add dependencies**

In root `Cargo.toml`, add workspace dependencies:

```toml
arrow-array = "56"
arrow-schema = "56"
parquet = { version = "56", default-features = false, features = ["arrow", "zstd"] }
```

In `crates/ab-morph-run/Cargo.toml`, add:

```toml
arrow-array.workspace = true
arrow-schema.workspace = true
parquet.workspace = true
```

Keep all Arrow-family crates on the same major version. This is the first Arrow dependency in the workspace and will increase compile time; do not add Polars or DuckDB Rust dependencies in this phase.

- [ ] **Step 4: Add module skeleton**

Create `crates/ab-morph-run/src/warehouse/mod.rs`:

```rust
pub(crate) mod schema;
```

Modify `crates/ab-morph-run/src/lib.rs`:

```rust
mod warehouse;
```

- [ ] **Step 5: Add schema types and path helpers**

Create `crates/ab-morph-run/src/warehouse/schema.rs`:

```rust
use std::path::{Path, PathBuf};

pub(crate) const SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum WarehouseTable {
    Runs,
    RunAnalyzers,
    Sources,
    Analyses,
    Morphemes,
    MorphemeFeatures,
    NwayRegions,
    NwayRegionAnalyzers,
    NwayFeatureDiffs,
    Errors,
}

impl WarehouseTable {
    pub(crate) const ALL: &'static [Self] = &[
        Self::Runs,
        Self::RunAnalyzers,
        Self::Sources,
        Self::Analyses,
        Self::Morphemes,
        Self::MorphemeFeatures,
        Self::NwayRegions,
        Self::NwayRegionAnalyzers,
        Self::NwayFeatureDiffs,
        Self::Errors,
    ];

    pub(crate) fn file_name(self) -> &'static str {
        match self {
            Self::Runs => "runs.parquet",
            Self::RunAnalyzers => "run_analyzers.parquet",
            Self::Sources => "sources.parquet",
            Self::Analyses => "analyses.parquet",
            Self::Morphemes => "morphemes.parquet",
            Self::MorphemeFeatures => "morpheme_features.parquet",
            Self::NwayRegions => "nway_regions.parquet",
            Self::NwayRegionAnalyzers => "nway_region_analyzers.parquet",
            Self::NwayFeatureDiffs => "nway_feature_diffs.parquet",
            Self::Errors => "errors.parquet",
        }
    }

    pub(crate) fn column_names(self) -> &'static [&'static str] {
        match self {
            Self::Runs => &[
                "schema_version",
                "run_id",
                "created_at_utc",
                "input_mode",
                "input_path",
                "source_count",
                "analyzer_count",
                "error_count",
            ],
            Self::RunAnalyzers => &["run_id", "analyzer_id", "analyzer_arg", "analyzer_family"],
            Self::Sources => &["run_id", "source_id", "text_id", "aat_path", "source_bytes", "source_chars"],
            Self::Analyses => &["run_id", "source_id", "text_id", "analyzer_id", "morpheme_count"],
            Self::Morphemes => &[
                "run_id",
                "source_id",
                "text_id",
                "analyzer_id",
                "morpheme_index",
                "byte_start",
                "byte_end",
                "char_start",
                "char_end",
                "surface",
            ],
            Self::MorphemeFeatures => &[
                "run_id",
                "source_id",
                "text_id",
                "analyzer_id",
                "morpheme_index",
                "feature_key",
                "feature_value",
            ],
            Self::NwayRegions => &[
                "run_id",
                "source_id",
                "text_id",
                "region_index",
                "byte_start",
                "byte_end",
                "char_start",
                "char_end",
                "is_nonempty_whitespace",
                "is_agreement",
                "has_coverage_mismatch",
                "has_segmentation_disagreement",
                "has_feature_disagreement",
            ],
            Self::NwayRegionAnalyzers => &[
                "run_id",
                "source_id",
                "text_id",
                "region_index",
                "analyzer_id",
                "covers_exactly",
                "morpheme_start",
                "morpheme_end",
                "surfaces",
            ],
            Self::NwayFeatureDiffs => &[
                "run_id",
                "source_id",
                "text_id",
                "region_index",
                "feature_key",
                "scope_type",
                "scope_position",
                "scope_surface",
                "feature_value",
                "analyzer_id",
            ],
            Self::Errors => &[
                "run_id",
                "source_id",
                "text_id",
                "analyzer_id",
                "stage",
                "error_code",
                "message",
            ],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct WarehousePaths {
    pub(crate) warehouse_dir: PathBuf,
    pub(crate) run_id: String,
    pub(crate) staging_dir: PathBuf,
    pub(crate) final_dir: PathBuf,
}

impl WarehousePaths {
    pub(crate) fn new(warehouse_dir: impl AsRef<Path>, run_id: impl Into<String>) -> Self {
        let warehouse_dir = warehouse_dir.as_ref().to_path_buf();
        let run_id = run_id.into();
        let staging_dir = warehouse_dir
            .join(".staging")
            .join(format!("{}.{}", run_id, std::process::id()));
        let final_dir = warehouse_dir.join("runs").join(&run_id);
        Self {
            warehouse_dir,
            run_id,
            staging_dir,
            final_dir,
        }
    }

    pub(crate) fn staging_table_path(&self, table: WarehouseTable) -> PathBuf {
        self.staging_dir.join(table.file_name())
    }

    pub(crate) fn final_table_path(&self, table: WarehouseTable) -> PathBuf {
        self.final_dir.join(table.file_name())
    }
}
```

- [ ] **Step 6: Add core row structs**

Append the row structs from the Data Protocol to `schema.rs`. Use these exact Rust names:

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RunRow {
    pub(crate) schema_version: u32,
    pub(crate) run_id: String,
    pub(crate) created_at_utc: String,
    pub(crate) input_mode: String,
    pub(crate) input_path: String,
    pub(crate) source_count: u64,
    pub(crate) analyzer_count: u64,
    pub(crate) error_count: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RunAnalyzerRow {
    pub(crate) run_id: String,
    pub(crate) analyzer_id: String,
    pub(crate) analyzer_arg: String,
    pub(crate) analyzer_family: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SourceRow {
    pub(crate) run_id: String,
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) aat_path: String,
    pub(crate) source_bytes: u64,
    pub(crate) source_chars: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct AnalysisRow {
    pub(crate) run_id: String,
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) analyzer_id: String,
    pub(crate) morpheme_count: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct MorphemeRow {
    pub(crate) run_id: String,
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) analyzer_id: String,
    pub(crate) morpheme_index: u64,
    pub(crate) byte_start: u64,
    pub(crate) byte_end: u64,
    pub(crate) char_start: u64,
    pub(crate) char_end: u64,
    pub(crate) surface: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct MorphemeFeatureRow {
    pub(crate) run_id: String,
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) analyzer_id: String,
    pub(crate) morpheme_index: u64,
    pub(crate) feature_key: String,
    pub(crate) feature_value: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct NwayRegionRow {
    pub(crate) run_id: String,
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) region_index: u64,
    pub(crate) byte_start: u64,
    pub(crate) byte_end: u64,
    pub(crate) char_start: u64,
    pub(crate) char_end: u64,
    pub(crate) is_nonempty_whitespace: bool,
    pub(crate) is_agreement: bool,
    pub(crate) has_coverage_mismatch: bool,
    pub(crate) has_segmentation_disagreement: bool,
    pub(crate) has_feature_disagreement: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct NwayRegionAnalyzerRow {
    pub(crate) run_id: String,
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) region_index: u64,
    pub(crate) analyzer_id: String,
    pub(crate) covers_exactly: bool,
    pub(crate) morpheme_start: u64,
    pub(crate) morpheme_end: u64,
    pub(crate) surfaces: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct NwayFeatureDiffRow {
    pub(crate) run_id: String,
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) region_index: u64,
    pub(crate) feature_key: String,
    pub(crate) scope_type: String,
    pub(crate) scope_position: Option<u64>,
    pub(crate) scope_surface: Option<String>,
    pub(crate) feature_value: Option<String>,
    pub(crate) analyzer_id: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ErrorRow {
    pub(crate) run_id: String,
    pub(crate) source_id: Option<String>,
    pub(crate) text_id: Option<String>,
    pub(crate) analyzer_id: Option<String>,
    pub(crate) stage: String,
    pub(crate) error_code: String,
    pub(crate) message: String,
}
```

- [ ] **Step 7: Add schema tests**

Add to `schema.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn warehouse_paths_stage_then_publish_by_run_id() {
        let paths = WarehousePaths::new("scratch/morph-warehouse", "run-a");

        assert_eq!(
            paths.final_table_path(WarehouseTable::Morphemes),
            PathBuf::from("scratch/morph-warehouse/runs/run-a/morphemes.parquet")
        );
        assert!(
            paths
                .staging_table_path(WarehouseTable::Morphemes)
                .starts_with("scratch/morph-warehouse/.staging/run-a.")
        );
    }

    #[test]
    fn every_table_has_a_parquet_file_name() {
        let names: Vec<_> = WarehouseTable::ALL
            .iter()
            .map(|table| table.file_name())
            .collect();

        assert_eq!(names.len(), 10);
        assert!(names.iter().all(|name| name.ends_with(".parquet")));
        assert!(names.contains(&"nway_region_analyzers.parquet"));
        assert!(!names.contains(&"nway_segmentation_groups.parquet"));
    }

    #[test]
    fn every_table_has_documented_columns() {
        for table in WarehouseTable::ALL {
            assert!(!table.column_names().is_empty(), "missing columns for {:?}", table);
        }
        assert!(WarehouseTable::NwayRegionAnalyzers.column_names().contains(&"surfaces"));
        assert!(WarehouseTable::NwayRegions.column_names().contains(&"is_nonempty_whitespace"));
    }
}
```

- [ ] **Step 8: Add CLI fields and validation helper**

In `Command::AnalyzeAat`, add:

```rust
#[arg(long, conflicts_with_all = [
    "output_dir",
    "analyses_output",
    "comparisons_output",
    "examples_output",
    "errors_output",
    "manifest_output",
    "nway_output",
    "nway_pattern_counts_output",
])]
warehouse_dir: Option<PathBuf>,
#[arg(long, requires = "warehouse_dir")]
run_id: Option<String>,
```

Add this helper near the existing output resolver helpers in `main.rs`:

```rust
fn validate_warehouse_cli(
    warehouse_dir: Option<&PathBuf>,
    run_id: Option<&str>,
    resume: bool,
    jobs: usize,
) -> Result<()> {
    if warehouse_dir.is_none() {
        return Ok(());
    }
    if run_id.is_none() {
        bail!("--warehouse-dir requires --run-id");
    }
    if resume {
        bail!("warehouse mode does not support --resume in phase 1");
    }
    if jobs != 1 {
        bail!("warehouse mode requires --jobs 1 in phase 1");
    }
    Ok(())
}
```

In the `AnalyzeAat` match arm, call this helper before resolving any output paths. For this task only, if `warehouse_dir` is present after validation, return:

```rust
bail!("warehouse mode is introduced in this task and wired in Task 4");
```

- [ ] **Step 9: Run tests**

Run:

```bash
cargo test -p ab-morph-run warehouse::schema parses_warehouse_mode_without_jsonl_outputs rejects_warehouse_with_jsonl_output_dir warehouse_validation_rejects_resume_and_parallel_jobs
```

Expected: PASS.

- [ ] **Step 10: Commit**

```bash
git add Cargo.toml crates/ab-morph-run/Cargo.toml crates/ab-morph-run/src/lib.rs crates/ab-morph-run/src/main.rs crates/ab-morph-run/src/warehouse
git commit -m "feat: add morph warehouse schema shell"
```

---

## Task 2: Implement Parquet writer, empty-table schemas, and sealed finalization

**Files:**
- Create: `crates/ab-morph-run/src/warehouse/writer.rs`
- Modify: `crates/ab-morph-run/src/warehouse/mod.rs`
- Modify: `crates/ab-morph-run/src/warehouse/schema.rs`

- [ ] **Step 1: Add writer module and finalization tests**

Create `crates/ab-morph-run/src/warehouse/writer.rs`:

```rust
use std::path::Path;

use anyhow::{Result, bail};

use super::schema::{WarehousePaths, WarehouseTable};

pub(crate) struct WarehouseWriter {
    paths: WarehousePaths,
}

impl WarehouseWriter {
    pub(crate) fn create(paths: WarehousePaths) -> Result<Self> {
        bail!("red-phase warehouse writer")
    }

    pub(crate) fn finalize(self) -> Result<()> {
        bail!("red-phase warehouse finalization")
    }
}

pub(crate) fn parquet_file_exists(dir: &Path, table: WarehouseTable) -> bool {
    dir.join(table.file_name()).is_file()
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::*;

    #[test]
    fn create_removes_stale_staging_for_same_run_id() {
        let root = temp_dir("stale");
        let stale = root.join(".staging/run-a.12345");
        fs::create_dir_all(&stale).unwrap();
        fs::write(stale.join("sentinel"), b"stale").unwrap();

        let paths = WarehousePaths::new(&root, "run-a");
        let writer = WarehouseWriter::create(paths.clone()).unwrap();

        assert!(!stale.exists());
        assert!(paths.staging_dir.exists());
        drop(writer);
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn finalize_moves_complete_run_out_of_staging() {
        let root = temp_dir("finalize");
        let paths = WarehousePaths::new(&root, "run-a");
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer.finalize().unwrap();

        assert!(!paths.staging_dir.exists());
        assert!(paths.final_dir.exists());
        assert!(parquet_file_exists(&paths.final_dir, WarehouseTable::Runs));
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn finalize_refuses_to_overwrite_existing_run() {
        let root = temp_dir("overwrite");
        let paths = WarehousePaths::new(&root, "run-a");
        fs::create_dir_all(&paths.final_dir).unwrap();

        let err = WarehouseWriter::create(paths).unwrap_err().to_string();

        assert!(err.contains("already exists"));
        let _ = fs::remove_dir_all(root);
    }

    fn temp_dir(label: &str) -> std::path::PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!(
            "ab-morph-warehouse-{label}-{}-{unique}",
            std::process::id()
        ))
    }
}
```

Modify `warehouse/mod.rs`:

```rust
pub(crate) mod schema;
pub(crate) mod writer;
```

- [ ] **Step 2: Run failing tests**

Run:

```bash
cargo test -p ab-morph-run warehouse::writer
```

Expected: FAIL with the red-phase writer error.

- [ ] **Step 3: Implement staging cleanup and finalization**

In `writer.rs`, implement:

```rust
use std::fs;

use anyhow::Context;

fn cleanup_stale_staging(paths: &WarehousePaths) -> Result<()> {
    let Some(staging_root) = paths.staging_dir.parent() else {
        return Ok(());
    };
    if !staging_root.exists() {
        return Ok(());
    }
    let prefix = format!("{}.", paths.run_id);
    for entry in fs::read_dir(staging_root)? {
        let entry = entry?;
        let file_name = entry.file_name();
        let file_name = file_name.to_string_lossy();
        if file_name.starts_with(&prefix) && entry.path() != paths.staging_dir {
            fs::remove_dir_all(entry.path()).with_context(|| {
                format!("failed to remove stale staging directory {}", entry.path().display())
            })?;
        }
    }
    Ok(())
}

fn finalize_staging_run(paths: &WarehousePaths) -> Result<()> {
    if paths.final_dir.exists() {
        bail!("warehouse run {} already exists", paths.run_id);
    }
    if let Some(parent) = paths.final_dir.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::rename(&paths.staging_dir, &paths.final_dir).with_context(|| {
        format!(
            "failed to publish warehouse run from {} to {}",
            paths.staging_dir.display(),
            paths.final_dir.display()
        )
    })?;
    Ok(())
}
```

`WarehouseWriter::create` must:

- fail if `final_dir` already exists,
- call `cleanup_stale_staging`,
- remove its own staging dir if present,
- create the staging dir.

`WarehouseWriter::finalize` must call `finalize_staging_run`.

- [ ] **Step 4: Add Arrow/Parquet append helpers**

Implement private helpers in `writer.rs`:

```rust
use std::fs::File;
use std::sync::Arc;

use arrow_array::{ArrayRef, BooleanArray, ListArray, RecordBatch, StringArray, UInt32Array, UInt64Array};
use arrow_array::builder::{ListBuilder, StringBuilder};
use arrow_schema::{DataType, Field, Schema};
use parquet::arrow::ArrowWriter;
use parquet::basic::{Compression, ZstdLevel};
use parquet::file::properties::WriterProperties;

fn writer_properties() -> WriterProperties {
    WriterProperties::builder()
        .set_compression(Compression::ZSTD(ZstdLevel::try_new(3).expect("valid zstd level")))
        .build()
}

fn write_batch<W: std::io::Write + Send>(
    writer: &mut ArrowWriter<W>,
    schema: Arc<Schema>,
    columns: Vec<ArrayRef>,
) -> Result<()> {
    let batch = RecordBatch::try_new(schema.clone(), columns)?;
    writer.write(&batch)?;
    Ok(())
}
```

Use `ListBuilder<StringBuilder>` for `surfaces: list<utf8>`. `WarehouseWriter::create` opens one `ArrowWriter<File>` per table and keeps them alive until `finalize`; append methods write one record batch at a time.

- [ ] **Step 5: Add empty-table schema test**

Add this test to `writer.rs`:

```rust
#[test]
fn empty_tables_are_valid_parquet_files() {
    let root = temp_dir("empty-tables");
    let paths = WarehousePaths::new(&root, "run-a");
    let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
    writer.finalize().unwrap();

    for table in WarehouseTable::ALL {
        assert!(paths.final_table_path(*table).is_file(), "missing {:?}", table);
    }

    let _ = fs::remove_dir_all(root);
}
```

Add a companion schema test that opens each empty Parquet file and compares its Arrow column names to `WarehouseTable::column_names()`:

```rust
#[test]
fn empty_parquet_schemas_match_documented_columns() {
    use std::fs::File;
    use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;

    let root = temp_dir("empty-schema");
    let paths = WarehousePaths::new(&root, "run-a");
    let writer = WarehouseWriter::create(paths.clone()).unwrap();
    writer.finalize().unwrap();

    for table in WarehouseTable::ALL {
        let file = File::open(paths.final_table_path(*table)).unwrap();
        let builder = ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
        let actual: Vec<_> = builder
            .schema()
            .fields()
            .iter()
            .map(|field| field.name().as_str())
            .collect();
        assert_eq!(actual, table.column_names(), "schema mismatch for {:?}", table);
    }

    let _ = fs::remove_dir_all(root);
}
```

- [ ] **Step 6: Implement append-style row writers**

Add writer methods:

```rust
pub(crate) fn append_runs(&mut self, rows: &[RunRow]) -> Result<()>;
pub(crate) fn append_run_analyzers(&mut self, rows: &[RunAnalyzerRow]) -> Result<()>;
pub(crate) fn append_sources(&mut self, rows: &[SourceRow]) -> Result<()>;
pub(crate) fn append_analyses(&mut self, rows: &[AnalysisRow]) -> Result<()>;
pub(crate) fn append_morphemes(&mut self, rows: &[MorphemeRow]) -> Result<()>;
pub(crate) fn append_morpheme_features(&mut self, rows: &[MorphemeFeatureRow]) -> Result<()>;
pub(crate) fn append_nway_regions(&mut self, rows: &[NwayRegionRow]) -> Result<()>;
pub(crate) fn append_nway_region_analyzers(&mut self, rows: &[NwayRegionAnalyzerRow]) -> Result<()>;
pub(crate) fn append_nway_feature_diffs(&mut self, rows: &[NwayFeatureDiffRow]) -> Result<()>;
pub(crate) fn append_errors(&mut self, rows: &[ErrorRow]) -> Result<()>;
```

Each method writes a record batch to the already-open Parquet writer for its table. Empty slices are no-ops. `finalize` closes every table writer; if an `ArrowWriter` does not emit a valid empty file without a batch, write a zero-row `RecordBatch` immediately before closing that table. This keeps warehouse memory bounded by one source's rows plus Arrow writer buffers.

- [ ] **Step 7: Add non-empty Parquet test**

Add a test that writes one `RunRow` and one `NwayRegionAnalyzerRow` with `surfaces = vec!["今".to_owned(), "日".to_owned()]`, finalizes, and asserts both files exist.

- [ ] **Step 8: Run tests**

Run:

```bash
cargo test -p ab-morph-run warehouse::writer
```

Expected: PASS.

- [ ] **Step 9: Commit**

```bash
git add crates/ab-morph-run/src/warehouse
git commit -m "feat: add sealed parquet warehouse writer"
```

---

## Task 3: Map analyses and N-way regions into warehouse rows

**Files:**
- Create: `crates/ab-morph-run/src/warehouse/rows.rs`
- Modify: `crates/ab-morph-run/src/warehouse/mod.rs`
- Modify: `crates/ab-morph-run/src/warehouse/schema.rs`

- [ ] **Step 1: Create row mapping module with failing tests**

Create `crates/ab-morph-run/src/warehouse/rows.rs`:

```rust
use ab_morph_diff::{Analysis, MorphDiffError};

use super::schema::{AnalysisRow, MorphemeFeatureRow, MorphemeRow, NwayFeatureDiffRow, NwayRegionAnalyzerRow, NwayRegionRow, SourceRow};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub(crate) struct NwayFactRows {
    pub(crate) regions: Vec<NwayRegionRow>,
    pub(crate) region_analyzers: Vec<NwayRegionAnalyzerRow>,
    pub(crate) feature_diffs: Vec<NwayFeatureDiffRow>,
}

pub(crate) fn source_row(
    _run_id: &str,
    _source_id: &str,
    _aat_path: &str,
    _analysis: &Analysis,
) -> SourceRow {
    panic!("red-phase source row mapping")
}

pub(crate) fn analysis_row(_run_id: &str, _source_id: &str, _analysis: &Analysis) -> AnalysisRow {
    panic!("red-phase analysis row mapping")
}

pub(crate) fn morpheme_rows(
    _run_id: &str,
    _source_id: &str,
    _analysis: &Analysis,
) -> Vec<MorphemeRow> {
    panic!("red-phase morpheme row mapping")
}

pub(crate) fn morpheme_feature_rows(
    _run_id: &str,
    _source_id: &str,
    _analysis: &Analysis,
) -> Vec<MorphemeFeatureRow> {
    panic!("red-phase feature row mapping")
}

pub(crate) fn nway_fact_rows(
    _run_id: &str,
    _source_id: &str,
    _source_text: &str,
    _analyses: &[Analysis],
) -> Result<NwayFactRows, MorphDiffError> {
    panic!("red-phase nway row mapping")
}
```

Modify `warehouse/mod.rs`:

```rust
pub(crate) mod rows;
pub(crate) mod schema;
pub(crate) mod writer;
```

- [ ] **Step 2: Add analysis mapping test**

Add to `rows.rs`:

```rust
#[cfg(test)]
mod tests {
    use std::ops::Range;

    use ab_morph_diff::{Analysis, FeatureMap, Morpheme};

    use super::*;

    #[test]
    fn maps_source_analysis_morphemes_and_features() {
        let analysis = Analysis {
            text_id: "work-a".to_owned(),
            analyzer: "vibrato:unidic".to_owned(),
            source_text: "今日".to_owned(),
            morphemes: vec![m(
                "今日",
                0..6,
                0..2,
                [("pos1", Some("名詞")), ("lemma", Some("今日"))],
            )],
        };

        assert_eq!(source_row("run-a", "source-a", "scratch/a.json", &analysis).text_id, "work-a");
        assert_eq!(analysis_row("run-a", "source-a", &analysis).morpheme_count, 1);

        let morphemes = morpheme_rows("run-a", "source-a", &analysis);
        assert_eq!(morphemes[0].surface, "今日");
        assert_eq!(morphemes[0].byte_start, 0);
        assert_eq!(morphemes[0].byte_end, 6);

        let features = morpheme_feature_rows("run-a", "source-a", &analysis);
        assert_eq!(features.len(), 2);
        assert!(features.iter().any(|row| row.feature_key == "pos1" && row.feature_value.as_deref() == Some("名詞")));
    }

    fn m(
        surface: &str,
        byte_span: Range<usize>,
        char_span: Range<usize>,
        features: impl IntoIterator<Item = (&'static str, Option<&'static str>)>,
    ) -> Morpheme {
        let mut map = FeatureMap::new();
        for (key, value) in features {
            map.insert(key.into(), value.map(Into::into));
        }
        Morpheme {
            surface: surface.to_owned(),
            byte_span,
            char_span,
            features: map,
        }
    }
}
```

- [ ] **Step 3: Add N-way mapping test with exact rows**

Add to `rows.rs` tests:

```rust
#[test]
fn maps_nway_regions_to_three_fact_tables() {
    let analyses = vec![
        analysis("work-a", "vibrato", "今日", vec![m("今日", 0..6, 0..2, [("pos1", Some("名詞"))])]),
        analysis("work-a", "sudachi-a", "今日", vec![m("今日", 0..6, 0..2, [("pos1", Some("名詞"))])]),
        analysis(
            "work-a",
            "sudachi-c",
            "今日",
            vec![
                m("今", 0..3, 0..1, [("pos1", Some("名詞"))]),
                m("日", 3..6, 1..2, [("pos1", Some("名詞"))]),
            ],
        ),
    ];

    let facts = nway_fact_rows("run-a", "source-a", "今日", &analyses).unwrap();

    assert_eq!(facts.regions.len(), 1);
    assert!(facts.regions[0].has_segmentation_disagreement);
    assert_eq!(facts.region_analyzers.len(), 3);
    assert!(facts.region_analyzers.iter().any(|row| row.analyzer_id == "vibrato" && row.surfaces == vec!["今日"]));
    assert!(facts.region_analyzers.iter().any(|row| row.analyzer_id == "sudachi-c" && row.surfaces == vec!["今", "日"]));
    assert!(facts.feature_diffs.is_empty());
}

fn analysis(text_id: &str, analyzer: &str, source_text: &str, morphemes: Vec<Morpheme>) -> Analysis {
    Analysis {
        text_id: text_id.to_owned(),
        analyzer: analyzer.to_owned(),
        source_text: source_text.to_owned(),
        morphemes,
    }
}
```

- [ ] **Step 4: Run failing tests**

Run:

```bash
cargo test -p ab-morph-run warehouse::rows
```

Expected: FAIL with red-phase mapping panics.

- [ ] **Step 5: Implement analysis row mapping**

Implement `source_row`, `analysis_row`, `morpheme_rows`, and `morpheme_feature_rows`. Do not compute `boundary_count`.

- [ ] **Step 6: Implement N-way row mapping**

Implementation requirements:

- Use `ab_morph_diff::visit_nway_regions_with_source_text`.
- Compute byte spans from char spans once per region using a helper equivalent to compact example byte-span conversion.
- `is_nonempty_whitespace` is `!excerpt.is_empty() && excerpt.chars().all(char::is_whitespace)`. Empty regions return `false` because this field represents observed whitespace evidence, not vacuous truth.
- `NwayRegionAnalyzerRow.surfaces` preserves morpheme order for that analyzer.
- `NwayFeatureDiffRow` emits only feature groups with at least two distinct values.
- Scope conversion enforces the `scope_type` nullability invariant.

- [ ] **Step 7: Run tests**

Run:

```bash
cargo test -p ab-morph-run warehouse::rows
```

Expected: PASS.

- [ ] **Step 8: Commit**

```bash
git add crates/ab-morph-run/src/warehouse
git commit -m "feat: map morph facts to warehouse rows"
```

---

## Task 4: Wire warehouse sink into the existing serial analysis loop

**Files:**
- Modify: `crates/ab-morph-run/src/lib.rs`
- Modify: `crates/ab-morph-run/src/main.rs`
- Modify: `crates/ab-morph-run/src/warehouse/writer.rs`
- Modify: `crates/ab-morph-run/src/warehouse/rows.rs`

- [ ] **Step 1: Add integration test for warehouse run**

Add a test in `crates/ab-morph-run/src/lib.rs` near existing run tests:

```rust
#[test]
fn warehouse_mode_writes_sealed_parquet_without_jsonl_outputs() {
    let dir = temp_dir("warehouse-run");
    let aat_dir = dir.join("aats");
    let warehouse_dir = dir.join("warehouse");
    fs::create_dir_all(&aat_dir).unwrap();
    fs::write(aat_dir.join("source-a.json"), tiny_aat_json("work-a", "今日")).unwrap();

    run_analyze_aat_warehouse(
        None,
        Some(&aat_dir),
        &["test:single".to_owned(), "test:split".to_owned()],
        &warehouse_dir,
        "run-a",
        1,
    )
    .unwrap();

    let run_dir = warehouse_dir.join("runs/run-a");
    assert!(run_dir.join("runs.parquet").is_file());
    assert!(run_dir.join("morphemes.parquet").is_file());
    assert!(run_dir.join("nway_regions.parquet").is_file());
    assert!(!run_dir.join("analyses.jsonl").exists());
    assert!(!warehouse_dir.join(".staging").exists() || fs::read_dir(warehouse_dir.join(".staging")).unwrap().next().is_none());

    let _ = fs::remove_dir_all(dir);
}
```

- [ ] **Step 2: Add test analyzer design**

Add a `#[cfg(test)]` variant to the existing loaded analyzer path instead of loading real dictionaries:

```rust
#[cfg(test)]
enum TestAnalyzerKind {
    Single,
    Split,
}
```

Wire `test:single` and `test:split` only under `#[cfg(test)]` in analyzer-spec parsing/loading. `test:single` emits one morpheme covering the whole text. `test:split` splits `"今日"` into `"今"` and `"日"`; for other text it emits one whole-text morpheme. Both analyzers must populate `pos1 = 名詞`.

Byte spans for test analyzers must be computed from UTF-8 byte offsets, not char indexes. For `"今日"`, `test:split` emits `今` as `byte_span = 0..3, char_span = 0..1` and `日` as `byte_span = 3..6, char_span = 1..2`.

- [ ] **Step 3: Refactor output options, not the analysis loop**

Replace the growing serial-output option set with an internal enum:

```rust
enum SerialOutputMode<'a> {
    Jsonl(JsonlSerialOutputs<'a>),
    Warehouse(WarehouseSerialOutputs),
}

struct JsonlSerialOutputs<'a> {
    analyses_output: &'a Path,
    comparisons_output: Option<&'a Path>,
    examples_output: Option<&'a Path>,
    errors_output: Option<&'a Path>,
    manifest_output: Option<&'a Path>,
    nway_output: Option<&'a Path>,
    nway_pattern_counts_output: Option<&'a Path>,
}

struct WarehouseSerialOutputs {
    paths: warehouse::schema::WarehousePaths,
    input_mode: String,
    input_path: String,
}
```

`run_analyze_aat_inputs` should keep one source-processing loop. Inside that loop, route facts to either JSONL writers or the warehouse writer. Do not add a second loop that reads AAT, projects plaintext, and runs analyzers independently.

- [ ] **Step 4: Add public warehouse wrapper**

Add:

```rust
pub fn run_analyze_aat_warehouse(
    aat: Option<&Path>,
    aat_dir: Option<&Path>,
    analyzer_ids: &[String],
    warehouse_dir: &Path,
    run_id: &str,
    jobs: usize,
) -> Result<()> {
    if jobs != 1 {
        bail!("warehouse mode requires --jobs 1 in phase 1");
    }
    if aat.is_none() == aat_dir.is_none() {
        bail!("provide exactly one of --aat or --aat-dir");
    }
    let inputs = discover_aat_inputs(aat, aat_dir)?;
    let input_mode = if aat.is_some() { "aat" } else { "aat_dir" };
    let input_path = aat.or(aat_dir).unwrap().display().to_string();
    run_analyze_aat_inputs_with_mode(
        inputs,
        analyzer_ids,
        SerialOutputMode::Warehouse(WarehouseSerialOutputs {
            paths: warehouse::schema::WarehousePaths::new(warehouse_dir, run_id),
            input_mode: input_mode.to_owned(),
            input_path,
        }),
    )
}
```

`run_analyze_aat_inputs_with_mode` is the refactored form of the existing `run_analyze_aat_inputs`; do not add a second AAT/analyzer loop. Adapt names to the final internal helper, but preserve the rule: this wrapper delegates to the shared loop.

- [ ] **Step 5: Implement warehouse sink behavior in the shared loop**

For warehouse output mode:

- Create one `WarehouseWriter` before the loop.
- Append `RunAnalyzerRow` values before processing sources.
- For each source, build only that source's `SourceRow`, `AnalysisRow`, `MorphemeRow`, `MorphemeFeatureRow`, `NwayRegionRow`, `NwayRegionAnalyzerRow`, and `NwayFeatureDiffRow` batches, append them, then drop them before moving to the next source.
- On each source failure, append `ErrorRow` facts instead of JSONL error rows.
- Keep only scalar counters across the run: `source_count`, `error_count`, and analyzer count.
- After the loop, append one `RunRow`, then finalize.
- `RunRow.error_count` is the number of appended `ErrorRow` values.
- Do not write pairwise comparison rows in warehouse mode; pairwise is derived from N-way when needed.

- [ ] **Step 6: Wire CLI to warehouse wrapper**

In `main.rs`, after `validate_warehouse_cli`, route warehouse mode directly:

```rust
if let Some(warehouse_dir) = warehouse_dir {
    ab_morph_run::run_analyze_aat_warehouse(
        aat.as_deref(),
        aat_dir.as_deref(),
        &analyzer,
        &warehouse_dir,
        run_id.as_deref().expect("clap requires --run-id"),
        jobs,
    )?;
    return Ok(());
}
```

- [ ] **Step 7: Run tests**

Run:

```bash
cargo test -p ab-morph-run warehouse_mode_writes_sealed_parquet_without_jsonl_outputs parses_warehouse_mode_without_jsonl_outputs
```

Expected: PASS.

- [ ] **Step 8: Commit**

```bash
git add crates/ab-morph-run/src/lib.rs crates/ab-morph-run/src/main.rs crates/ab-morph-run/src/warehouse
git commit -m "feat: write morph warehouse from shared runner"
```

---

## Task 5: Add schema descriptor and DuckDB view helpers

**Files:**
- Create: `crates/ab-morph-run/sql/schema.sql`
- Create: `crates/ab-morph-run/sql/morph_views.sql`
- Create: `crates/ab-morph-run/src/warehouse/sql.rs`
- Modify: `crates/ab-morph-run/src/warehouse/mod.rs`
- Modify: `crates/ab-morph-run/src/warehouse/writer.rs`

- [ ] **Step 1: Create schema descriptor**

Create `crates/ab-morph-run/sql/schema.sql` with DDL comments matching the Data Protocol. Include these invariant comments exactly:

```sql
-- Morph warehouse schema version 1.
-- Readers must reject runs.schema_version values other than 1.
-- source_id is one AAT source record/file within a run.
-- text_id is the logical work id; many source_id values may share one text_id.
-- analyzer_family is closed in v1: vibrato | sudachi.
-- analyses preserves successful zero-morpheme analyzer runs; non-zero morpheme counts are derivable.
-- nway_feature_diffs scope invariant:
--   whole_region   => scope_position IS NULL AND scope_surface IS NULL
--   token_position => scope_position IS NOT NULL AND scope_surface IS NULL
--   surface        => scope_position IS NULL AND scope_surface IS NOT NULL
```

Use `CREATE TABLE` statements for the 10 canonical tables. This file is a human/tooling schema descriptor, not the mechanism used to create Parquet files. DuckDB type for `surfaces` is `VARCHAR[]`.

- [ ] **Step 2: Create view template**

Create `crates/ab-morph-run/sql/morph_views.sql`:

```sql
-- This file is a template. Warehouse finalization writes per-run views.sql with absolute paths.

CREATE OR REPLACE VIEW warehouse_runs AS
SELECT * FROM read_parquet('__RUN_DIR__/runs.parquet')
WHERE schema_version = 1;

CREATE OR REPLACE VIEW warehouse_nway_regions AS
SELECT * FROM read_parquet('__RUN_DIR__/nway_regions.parquet');

CREATE OR REPLACE VIEW warehouse_nway_region_analyzers AS
SELECT * FROM read_parquet('__RUN_DIR__/nway_region_analyzers.parquet');

CREATE OR REPLACE VIEW warehouse_nway_feature_diffs AS
SELECT * FROM read_parquet('__RUN_DIR__/nway_feature_diffs.parquet');

CREATE OR REPLACE VIEW top_segmentation_patterns AS
WITH region_patterns AS (
  SELECT
    run_id,
    source_id,
    region_index,
    list(struct_pack(analyzer_id := analyzer_id, surfaces := surfaces) ORDER BY analyzer_id) AS pattern
  FROM warehouse_nway_region_analyzers
  GROUP BY run_id, source_id, region_index
)
SELECT
  run_id,
  pattern,
  count(*) AS regions,
  count(DISTINCT source_id) AS sources
FROM region_patterns
GROUP BY run_id, pattern
ORDER BY regions DESC;

CREATE OR REPLACE VIEW top_feature_differences AS
SELECT
  run_id,
  feature_key,
  scope_type,
  coalesce(scope_surface, cast(scope_position AS VARCHAR), '') AS scope,
  feature_value,
  analyzer_id,
  count(*) AS occurrences,
  count(DISTINCT source_id || ':' || cast(region_index AS VARCHAR)) AS regions
FROM warehouse_nway_feature_diffs
GROUP BY run_id, feature_key, scope_type, scope, feature_value, analyzer_id
ORDER BY regions DESC;
```

The segmentation `pattern` column is a typed `LIST<STRUCT<analyzer_id VARCHAR, surfaces VARCHAR[]>>`. Do not collapse `surfaces` into an empty-joined string: `['今', '日']` and `['今日']` must remain distinct.

- [ ] **Step 3: Add SQL helper module**

Create `crates/ab-morph-run/src/warehouse/sql.rs`:

```rust
use std::fs;
use std::path::Path;

use anyhow::{Context, Result};

pub(crate) const SCHEMA_SQL: &str = include_str!("../../sql/schema.sql");
pub(crate) const MORPH_VIEWS_SQL_TEMPLATE: &str = include_str!("../../sql/morph_views.sql");

pub(crate) fn write_schema_sql(warehouse_dir: &Path) -> Result<()> {
    fs::write(warehouse_dir.join("schema.sql"), SCHEMA_SQL)
        .with_context(|| format!("failed to write {}", warehouse_dir.join("schema.sql").display()))?;
    Ok(())
}

pub(crate) fn write_run_views_sql(output_run_dir: &Path, final_run_dir: &Path) -> Result<()> {
    let final_run_dir = final_run_dir.canonicalize().unwrap_or_else(|_| {
        if final_run_dir.is_absolute() {
            final_run_dir.to_path_buf()
        } else {
            std::env::current_dir()
                .expect("current directory is available")
                .join(final_run_dir)
        }
    });
    let final_run_dir = final_run_dir
        .to_string_lossy()
        .replace('\\', "\\\\")
        .replace('\'', "''");
    let views = MORPH_VIEWS_SQL_TEMPLATE.replace("__RUN_DIR__", &final_run_dir);
    fs::write(output_run_dir.join("views.sql"), views).with_context(|| {
        format!(
            "failed to write {}",
            output_run_dir.join("views.sql").display()
        )
    })?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sql_mentions_parquet_not_jsonl_and_has_version_policy() {
        assert!(SCHEMA_SQL.contains("Readers must reject runs.schema_version values other than 1"));
        assert!(MORPH_VIEWS_SQL_TEMPLATE.contains("__RUN_DIR__/nway_regions.parquet"));
        assert!(MORPH_VIEWS_SQL_TEMPLATE.contains("struct_pack"));
        assert!(!MORPH_VIEWS_SQL_TEMPLATE.contains("jsonl"));
    }

    #[test]
    fn schema_sql_mentions_every_documented_column() {
        for table in crate::warehouse::schema::WarehouseTable::ALL {
            for column in table.column_names() {
                assert!(
                    SCHEMA_SQL.contains(column),
                    "schema.sql does not mention {:?}.{}",
                    table,
                    column
                );
            }
        }
    }
}
```

Modify `warehouse/mod.rs`:

```rust
pub(crate) mod rows;
pub(crate) mod schema;
pub(crate) mod sql;
pub(crate) mod writer;
```

Unit tests intentionally do not invoke DuckDB. SQL syntax and semantics are verified in Task 7 against the smoke warehouse using the DuckDB CLI so `ab-morph-run` does not gain a DuckDB Rust dependency in phase 1.

- [ ] **Step 4: Write SQL helpers during finalization**

In `WarehouseWriter::create`, call `crate::warehouse::sql::write_schema_sql(&paths.warehouse_dir)?` after creating the warehouse directory. In `WarehouseWriter::finalize`, write `views.sql` into staging before publishing, but substitute the final run directory into the SQL:

```rust
crate::warehouse::sql::write_run_views_sql(&self.paths.staging_dir, &self.paths.final_dir)?;
```

If consuming `self` makes this awkward, move the call into a private finalization helper before returning.

- [ ] **Step 5: Run tests**

Run:

```bash
cargo test -p ab-morph-run warehouse::sql warehouse::writer
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-run/sql crates/ab-morph-run/src/warehouse
git commit -m "feat: add morph warehouse duckdb helpers"
```

---

## Task 6: Document warehouse workflow and limitations

**Files:**
- Modify: `docs/morph-corpus-workflow.md`

- [ ] **Step 1: Add warehouse workflow section**

Add near the current compact-run section:

```markdown
## Warehouse mode: canonical comprehensive artifact

Warehouse mode writes sealed Parquet fact tables. It is the preferred format for complete corpus analysis. It does not write JSONL outputs.

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/morph-full-corpus/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --warehouse-dir scratch/morph-warehouse \
    --run-id full-2026-05-01 \
    --jobs 1
```

Phase-1 warehouse mode is serial and does not support `--resume`. Interrupted runs leave staging directories under `.staging/`; the next run with the same `--run-id` removes stale staging before starting. Published runs under `runs/<run-id>/` are immutable.

Query with DuckDB:

```bash
duckdb -c ".read scratch/morph-warehouse/runs/full-2026-05-01/views.sql" \
       -c "SELECT * FROM top_segmentation_patterns LIMIT 50;"
```

The existing JSONL `--output-dir` mode remains for compatibility and targeted debugging, but it is not the canonical comprehensive store.
```

- [ ] **Step 2: Run doc grep sanity check**

Run:

```bash
rg -n "Warehouse mode|canonical comprehensive|does not support `--resume`" docs/morph-corpus-workflow.md
```

Expected: all three phrases appear.

- [ ] **Step 3: Commit**

```bash
git add docs/morph-corpus-workflow.md
git commit -m "docs: document morph warehouse workflow"
```

---

## Task 7: Smoke test warehouse mode with real AAT input and DuckDB

**Files:**
- Create: `docs/superpowers/reports/2026-05-01-morph-warehouse-smoke.md`

- [ ] **Step 1: Build release binary**

Run:

```bash
cargo build --release -p ab-morph-run
```

Expected: PASS.

- [ ] **Step 2: Run one-source warehouse smoke**

Run:

```bash
rm -rf scratch/morph-warehouse-smoke
mkdir -p scratch/morph-warehouse-smoke/aats
smallest=$(python - <<'PY'
from pathlib import Path
files=list(Path('scratch/morph-full-corpus/aats').rglob('*.json'))
if not files:
    raise SystemExit('no AAT files found under scratch/morph-full-corpus/aats')
print(min(files, key=lambda p: p.stat().st_size))
PY
)
ln -s "$(realpath "$smallest")" "scratch/morph-warehouse-smoke/aats/$(basename "$smallest")"
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/morph-warehouse-smoke/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --warehouse-dir scratch/morph-warehouse-smoke/warehouse \
    --run-id smoke-2026-05-01 \
    --jobs 1
find scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01 -maxdepth 1 -type f -name '*.parquet' | sort
```

Expected: all 10 canonical Parquet files and `views.sql` are present.

- [ ] **Step 3: Query with DuckDB and assert rows**

Run:

```bash
duckdb -c ".read scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01/views.sql" \
       -c "SELECT count(*) AS regions FROM warehouse_nway_regions;" \
       -c "SELECT count(*) AS rows FROM top_segmentation_patterns;"
```

Expected: both counts are non-zero. If `duckdb` is not installed, install it through the local Nix shell or run `nix shell nixpkgs#duckdb -c duckdb ...` with the same SQL commands.

- [ ] **Step 4: Confirm no JSONL in warehouse run**

Run:

```bash
find scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01 -type f | sort
```

Expected: only `.parquet` files plus `views.sql`; no `.jsonl` or `.jsonl.zst` files.

- [ ] **Step 5: Write smoke report**

Create `docs/superpowers/reports/2026-05-01-morph-warehouse-smoke.md`:

```markdown
# Morph Warehouse Smoke Report — 2026-05-01

## Command

Record the exact command from Task 7 Step 2, including the selected AAT file and `AB_SUDACHI_DICT` value.

## Artifact

- Warehouse path: `scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01`
- Canonical files: record the sorted `find` output, including `views.sql`.

## DuckDB checks

- `warehouse_nway_regions` row count: record the observed count.
- `top_segmentation_patterns` row count: record the observed count.

## Notes

The smoke verifies that warehouse mode writes comprehensive Parquet facts directly. No JSONL files are produced by the warehouse run.
```

- [ ] **Step 6: Commit**

```bash
git add docs/superpowers/reports/2026-05-01-morph-warehouse-smoke.md
git commit -m "docs: record morph warehouse smoke"
```

---

## Task 8: Final verification

**Files:**
- No new files unless verification finds issues.

- [ ] **Step 1: Format check**

Run:

```bash
cargo fmt --all -- --check
```

Expected: PASS.

- [ ] **Step 2: Test target crate**

Run:

```bash
cargo test -p ab-morph-run
```

Expected: PASS.

- [ ] **Step 3: Workspace check**

Run:

```bash
cargo check --workspace
```

Expected: PASS.

- [ ] **Step 4: Clippy**

Run:

```bash
cargo clippy --workspace --all-targets -- -D warnings
```

Expected: PASS.

- [ ] **Step 5: Confirm warehouse smoke has no JSONL**

Run:

```bash
find scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01 -type f | sort
```

Expected: only `.parquet` files plus `views.sql`; no `.jsonl` or `.jsonl.zst` files.

- [ ] **Step 6: Commit verification fixes if any**

If verification required fixes:

```bash
git add <fixed-files>
git commit -m "fix: polish morph warehouse implementation"
```

If no fixes were required, do not create an empty commit.

---

## Deferred Work

- Deleting the JSONL write path (status: deprecated). Trigger: after `ab-morph-run report` commands can read warehouse facts and produce equivalents for `summarize-compact`, `summarize-examples`, `summarize-differences`, `summarize-nway`, and `summarize-nway-patterns`. Target phase: warehouse reporting phase, immediately after this persistence phase.
- `export-jsonl` generated from warehouse facts. Add only when a concrete downstream consumer needs JSONL.
- Warehouse resume. Phase 1 restarts interrupted runs from scratch; stale staging cleanup prevents partial data from becoming visible.
- Parallel warehouse writes. Phase 1 is serial because schema correctness is the first constraint.
- Materialized pattern-count Parquet tables. Start with DuckDB views; materialize only after measuring query cost.
- Rust readers and summary commands over Parquet. Phase 1 query surface is DuckDB CLI plus generated SQL helpers.
- DuckDB database file. DuckDB queries Parquet directly; a `.duckdb` file is not canonical storage.
- Extracting the runner from `crates/ab-morph-run/src/lib.rs` into a focused `run.rs`. Do this if the shared-loop refactor makes `lib.rs` materially harder to navigate during implementation.

---

## Self-Review

- Spec coverage: The revised plan keeps sealed runs, Parquet facts, no canonical JSONL, explicit source/text identity, structural pattern derivation, stale-staging cleanup, schema descriptors, SQL views, and DuckDB smoke checks.
- Review incorporation: The plan accepts the high-impact feedback on duplicate loops, over-normalized N-way tables, custom surface encoding, ambiguous identity, wrong `boundary_count`, decorative status, stringly unknown analyzer family, missing error codes, CWD-coupled SQL, no empty-table tests, no resume story, stale-staging cleanup, structural segmentation-pattern SQL, nonempty whitespace naming, append-style warehouse writers, per-run `views.sql`, schema-drift tests, and a concrete JSONL deletion trigger. It partially defers JSONL deletion because current report commands still depend on it.
- Placeholder scan: No `TBD`, `TODO`, or undefined future fields are used as requirements. Red-phase panic strings are intentional test-first scaffolding.
- Type consistency: `WarehouseTable`, row structs, `WarehouseWriter`, `SerialOutputMode`, and `run_analyze_aat_warehouse` are introduced before later tasks reference them.
- Scope check: This remains one subsystem: canonical morph-result persistence and DuckDB querying. It does not attempt to replace all report commands in the same phase.
