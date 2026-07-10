//! DuckDB collection engine for the interestingness ranker.
//!
//! At full-corpus scale (~165M regions, ~495M region-analyzer rows, ~17.8B
//! feature-diff rows) the in-memory engine cannot hold the fact tables, so
//! aggregation runs in the DuckDB CLI (same discovery and settings as the
//! existing materializers) and Rust reads back one bounded row per pattern.
//!
//! Grouping happens on canonical signature strings built with `list_sort`
//! (deterministic, input-order independent — verified including NULL struct
//! fields). Rust re-canonicalizes parsed groups before hashing, so
//! `pattern_id` is identical across engines; the path-equality test pins
//! full-summary agreement.

use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use arrow_array::{Array, BooleanArray, Float64Array, RecordBatch, StringArray, UInt64Array};
use arrow_schema::{DataType, Field, Schema};
use parquet::arrow::ArrowWriter;
use parquet::basic::{Compression, ZstdLevel};
use parquet::file::properties::WriterProperties;

use super::interesting::{
    AnomalyRow, InterestingTextFilter, PatternAccumulator, PatternKind, PatternStats, RarityConfig,
    RegionExampleOut, RegionOccurrence, SqlSignature, WarehouseInterestingOptions,
};
use super::pattern_id::pattern_id;
use super::summary_body::{
    FeatureDiffsShape, NwayPatternKey, duckdb_table_path_literal, nway_feature_diffs_shape,
    read_warehouse_parquet_file, run_duckdb_statement, sql_literal,
};
use super::summary_body::{canonicalize_feature_values, canonicalize_segmentation_groups};
use crate::nway::{NwayFeatureScopeRow, NwayFeatureValueGroupRow, NwaySegmentationGroupRow};
use crate::summary::WarehouseFeatureProfile;
use crate::warehouse::schema::WarehouseTable;

/// Matches the in-memory predicate: punctuation (`\p{P}`), separators
/// (`\p{Z}`), ASCII whitespace (`\s` in RE2), plus U+000B and U+0085 which
/// `char::is_whitespace` accepts but RE2's `\s` does not.
const PUNCT_ONLY_REGEX: &str = "^[\\p{P}\\p{Z}\\s\u{000B}\u{0085}]*$";

#[derive(serde::Deserialize)]
struct JsonSurfaceGroup {
    surfaces: Vec<String>,
    analyzers: Vec<String>,
}

fn temp_output_path(run_dir: &Path, label: &str) -> PathBuf {
    super::summary_body::duckdb_temp_dir(run_dir).join(format!(
        "interesting-{}-{label}.parquet",
        std::process::id()
    ))
}

/// Removes `interesting-*.parquet` intermediates left behind by earlier
/// crashed or killed invocations (this process only deletes its own
/// outputs on success).
fn remove_stale_temp_outputs(run_dir: &Path) {
    let prefix = format!("interesting-{}-", std::process::id());
    let Ok(entries) = fs::read_dir(super::summary_body::duckdb_temp_dir(run_dir)) else {
        return;
    };
    for entry in entries.flatten() {
        let name = entry.file_name();
        let Some(name) = name.to_str() else { continue };
        if name.starts_with("interesting-")
            && name.ends_with(".parquet")
            && !name.starts_with(&prefix)
        {
            let _ = fs::remove_file(entry.path());
        }
    }
}

/// `base_regions` CTE body: region flags with the text filter applied.
/// Under `lexical-only`, punctuation-only regions are excluded via the
/// analyzers' concatenated surfaces (LEFT JOIN so regions with no analyzer
/// rows stay included, matching the in-memory engine).
fn base_regions_cte(
    regions: &str,
    analyzers: &str,
    filter: InterestingTextFilter,
    extra_predicate: &str,
) -> String {
    match filter {
        InterestingTextFilter::All => format!(
            r"base_regions AS (
    SELECT r.source_id, r.text_id, r.region_index, r.char_start, r.char_end,
           r.is_nonempty_whitespace, r.is_agreement, r.has_coverage_mismatch,
           r.has_segmentation_disagreement, r.has_feature_disagreement
    FROM read_parquet({regions}) r
    WHERE {extra_predicate}
)"
        ),
        InterestingTextFilter::LexicalOnly => format!(
            r"region_chars AS (
    SELECT source_id, text_id, region_index,
           array_to_string(list(array_to_string(surfaces, '')), '') AS all_chars
    FROM read_parquet({analyzers})
    GROUP BY source_id, text_id, region_index
),
base_regions AS (
    SELECT r.source_id, r.text_id, r.region_index, r.char_start, r.char_end,
           r.is_nonempty_whitespace, r.is_agreement, r.has_coverage_mismatch,
           r.has_segmentation_disagreement, r.has_feature_disagreement
    FROM read_parquet({regions}) r
    LEFT JOIN region_chars c USING (source_id, text_id, region_index)
    WHERE {extra_predicate}
      AND NOT r.is_nonempty_whitespace
      AND NOT (length(coalesce(c.all_chars, '')) > 0
               AND regexp_matches(coalesce(c.all_chars, ''), {punct}))
)",
            punct = sql_literal(PUNCT_ONLY_REGEX),
        ),
    }
}

/// Rarity-key expression and optional `works` join clause.
fn rarity_sql(run_dir: &Path, rarity: &RarityConfig) -> (String, String) {
    if rarity.work_by_source.is_some() {
        let works = sql_literal(&run_dir.join("aozora_works.parquet").display().to_string());
        (
            format!(
                "LEFT JOIN (SELECT DISTINCT source_id, work_id FROM read_parquet({works})) w \
                 ON w.source_id = b.source_id"
            ),
            "coalesce(w.work_id, b.source_id)".to_owned(),
        )
    } else {
        (String::new(), "b.source_id".to_owned())
    }
}

/// Per-pattern rollup over an occurrence CTE (`src`), keyed by
/// `key_cols`. Distinct counts and bounded sample lists are computed from
/// pre-deduplicated subqueries: grouped `list(...)`/`count(DISTINCT ...)`
/// states over raw occurrence rows blew the DuckDB memory limit on the
/// full corpus (162M feature-pattern occurrences), while `min_by(.., n)`
/// keeps a bounded per-group heap and `quantile_disc` holds one integer
/// per occurrence.
/// `carry_cols` may contain NULLs (e.g. `scope_position`); `join_key`
/// must be non-null columns only — SQL joins never match NULL = NULL, so
/// joining rollups on nullable scope columns silently dropped every
/// whole-region feature pattern.
fn pattern_rollup_sql(
    src: &str,
    carry_cols: &str,
    join_key: &str,
    max_region_examples: usize,
) -> String {
    let carry_prefix = if carry_cols.is_empty() {
        String::new()
    } else {
        format!("{carry_cols}, ")
    };
    format!(
        r"stats AS (
    SELECT {carry_prefix}{join_key},
        CAST(count(*) AS UBIGINT) AS examples,
        CAST(sum(CASE WHEN has_coverage_mismatch THEN 1 ELSE 0 END) AS UBIGINT) AS coverage_regions,
        CAST(quantile_disc(char_end - char_start, 0.9) AS DOUBLE) AS span_p90,
        CAST(to_json(min_by(
            struct_pack(source_id := source_id, text_id := text_id, region_index := region_index,
                        char_start := char_start, char_end := char_end),
            struct_pack(s := source_id, t := text_id, r := region_index),
            {max_region_examples})) AS VARCHAR) AS region_examples
    FROM {src}
    GROUP BY {carry_prefix}{join_key}
),
source_rollup AS (
    SELECT {join_key},
        CAST(count(*) AS UBIGINT) AS source_count,
        CAST(to_json(list_slice(list_sort(list(source_id)), 1, 5)) AS VARCHAR) AS sample_source_ids
    FROM (SELECT DISTINCT {join_key}, source_id FROM {src})
    GROUP BY {join_key}
),
text_rollup AS (
    SELECT {join_key},
        CAST(count(*) AS UBIGINT) AS text_count,
        CAST(to_json(list_slice(list_sort(list(text_id)), 1, 5)) AS VARCHAR) AS sample_text_ids
    FROM (SELECT DISTINCT {join_key}, text_id FROM {src})
    GROUP BY {join_key}
),
rarity_rollup AS (
    SELECT {join_key}, CAST(count(*) AS UBIGINT) AS rarity_count
    FROM (SELECT DISTINCT {join_key}, rarity_key FROM {src})
    GROUP BY {join_key}
)
SELECT {carry_prefix}{join_key},
    examples, source_count, text_count, rarity_count, coverage_regions, span_p90,
    sample_source_ids, sample_text_ids, region_examples
FROM stats
JOIN source_rollup USING ({join_key})
JOIN text_rollup USING ({join_key})
JOIN rarity_rollup USING ({join_key})"
    )
}

/// DuckDB settings for the interestingness engine. The shared
/// `duckdb_settings_sql` pins 16GB/4 threads, which OOMs on the
/// full-corpus feature aggregation; this workload gets a larger,
/// env-overridable budget (`AB_DUCKDB_MEMORY_LIMIT`, `AB_DUCKDB_THREADS`).
///
/// The default budget is sized against *currently available* memory, not
/// total RAM: hosts running `earlyoom -m10` SIGTERM any process once
/// MemAvailable drops under 10%, so the budget leaves that watermark plus
/// slack untouched (observed: a fixed 48GB default got duckdb killed on a
/// 93GB box with 33GB already in use).
fn default_memory_limit_gb() -> u64 {
    let available_kb = std::fs::read_to_string("/proc/meminfo")
        .ok()
        .and_then(|meminfo| {
            meminfo.lines().find_map(|line| {
                line.strip_prefix("MemAvailable:")?
                    .trim()
                    .split(' ')
                    .next()?
                    .parse::<u64>()
                    .ok()
            })
        })
        .unwrap_or(16 * 1024 * 1024);
    // Keep 40% of available memory (min 8GB) free for the OS, other
    // processes, and the earlyoom watermark; clamp to a sane range.
    let available_gb = available_kb / 1024 / 1024;
    (available_gb * 6 / 10).clamp(8, 48)
}

fn interesting_settings_sql(run_dir: &Path) -> String {
    let memory_limit = std::env::var("AB_DUCKDB_MEMORY_LIMIT")
        .unwrap_or_else(|_| format!("{}GB", default_memory_limit_gb()));
    let threads = std::env::var("AB_DUCKDB_THREADS").unwrap_or_else(|_| "8".to_owned());
    format!(
        "SET temp_directory = {};\nSET threads = {};\nSET preserve_insertion_order = false;\nSET memory_limit = {};",
        sql_literal(
            &super::summary_body::duckdb_temp_dir(run_dir)
                .display()
                .to_string()
        ),
        threads,
        sql_literal(&memory_limit),
    )
}

/// `region_sig` CTE shared by segmentation/coverage aggregation and the
/// anomaly exclusion: one canonical surface-group signature per region.
fn region_sig_cte(analyzers: &str, region_predicate: &str) -> String {
    format!(
        r"per_surface AS (
    SELECT a.source_id, a.text_id, a.region_index, a.surfaces,
           list_sort(list(a.analyzer_id)) AS analyzers
    FROM read_parquet({analyzers}) a
    JOIN (SELECT source_id, text_id, region_index FROM base_regions
          WHERE {region_predicate}) b USING (source_id, text_id, region_index)
    GROUP BY a.source_id, a.text_id, a.region_index, a.surfaces
),
region_sig AS (
    SELECT source_id, text_id, region_index,
           CAST(to_json(list_sort(list(struct_pack(surfaces := surfaces, analyzers := analyzers)))) AS VARCHAR) AS sig,
           count(*) AS group_count
    FROM per_surface
    GROUP BY source_id, text_id, region_index
)"
    )
}

fn feature_key_in_predicate(keys: &[String]) -> String {
    let list = keys
        .iter()
        .map(|key| sql_literal(key))
        .collect::<Vec<_>>()
        .join(", ");
    format!("f.feature_key IN ({list})")
}

/// The feature keys to aggregate: the fixed core set for `core`, the
/// data's distinct keys (one cheap single-column scan) for `raw`. Returns
/// `None` when no duckdb binary is available.
fn discover_feature_keys(
    run_dir: &Path,
    features: &str,
    profile: WarehouseFeatureProfile,
) -> Result<Option<Vec<String>>> {
    if profile == WarehouseFeatureProfile::Core {
        return Ok(Some(
            crate::summary::WAREHOUSE_CORE_FEATURE_KEYS
                .iter()
                .map(|key| (*key).to_owned())
                .collect(),
        ));
    }
    let out = temp_output_path(run_dir, "feature-keys");
    let sql = format!(
        "{settings}\nCOPY (SELECT DISTINCT feature_key FROM read_parquet({features}) ORDER BY feature_key) TO {out} (FORMAT PARQUET, COMPRESSION ZSTD);",
        settings = interesting_settings_sql(run_dir),
        out = sql_literal(&out.display().to_string()),
    );
    if !run_duckdb_statement(run_dir, sql, "interestingness feature key discovery")? {
        return Ok(None);
    }
    let mut keys = Vec::new();
    for batch in read_warehouse_parquet_file(&out)? {
        let column = column::<StringArray>(&batch, "feature_key")?;
        for row in 0..batch.num_rows() {
            keys.push(column.value(row).to_owned());
        }
    }
    let _ = fs::remove_file(&out);
    Ok(Some(keys))
}

/// Feature keys aggregated per duckdb invocation. Even the fixed-state
/// stage query spills the external sort to disk; on the full corpus a
/// 6-key batch overflowed the ~110GiB free temp volume, while one key at
/// a time peaks around 50GB. Results are identical across batch sizes
/// because patterns never span feature keys; raise via
/// AB_INTERESTING_FEATURE_KEY_BATCH on hosts with more disk.
fn feature_key_batch_size() -> usize {
    std::env::var("AB_INTERESTING_FEATURE_KEY_BATCH")
        .ok()
        .and_then(|value| value.parse().ok())
        .filter(|batch: &usize| *batch > 0)
        .unwrap_or(1)
}

fn seg_coverage_query(
    regions: &str,
    analyzers: &str,
    filter: InterestingTextFilter,
    works_join: &str,
    rarity_key: &str,
    max_region_examples: usize,
) -> String {
    let base = base_regions_cte(
        regions,
        analyzers,
        filter,
        "(r.has_segmentation_disagreement OR r.has_coverage_mismatch)",
    );
    let region_sig = region_sig_cte(analyzers, "TRUE");
    let rollup = pattern_rollup_sql("kind_source", "", "kind, sig", max_region_examples);
    format!(
        r"WITH {base},
{region_sig},
kind_source AS (
    SELECT 'segmentation' AS kind, s.sig,
           b.source_id, b.text_id, b.region_index, b.char_start, b.char_end,
           b.has_coverage_mismatch, {rarity_key} AS rarity_key
    FROM region_sig s
    JOIN base_regions b USING (source_id, text_id, region_index)
    {works_join}
    WHERE b.has_segmentation_disagreement AND s.group_count > 1
    UNION ALL
    SELECT 'coverage' AS kind, s.sig,
           b.source_id, b.text_id, b.region_index, b.char_start, b.char_end,
           b.has_coverage_mismatch, {rarity_key} AS rarity_key
    FROM region_sig s
    JOIN base_regions b USING (source_id, text_id, region_index)
    {works_join}
    WHERE b.has_coverage_mismatch
),
{rollup}"
    )
}

/// Stage query for the feature kind: dedupe to one row per
/// `(region, feature_key, scope, value)` with a fixed-size per-analyzer
/// `bool_or` pivot, externally sorted so Rust can stream contiguous
/// groups. Grouped `list(...)`/ordered aggregates cannot spill in DuckDB
/// and OOM'd on the full corpus (~162M feature regions); `bool_or` states
/// and `ORDER BY` both go to disk cleanly.
fn feature_stage_query(
    regions: &str,
    analyzers: &str,
    features: &str,
    filter: InterestingTextFilter,
    feature_predicate: &str,
    analyzer_ids: &[String],
    shape: FeatureDiffsShape,
) -> String {
    let base = base_regions_cte(regions, analyzers, filter, "r.has_feature_disagreement");
    let source = crate::summary::summary_body::nway_feature_diffs_expanded_source(features, shape);
    let analyzer_flags = analyzer_ids
        .iter()
        .enumerate()
        .map(|(index, analyzer_id)| {
            format!(
                "bool_or(f.analyzer_id = {}) AS analyzer_{index}",
                sql_literal(analyzer_id)
            )
        })
        .collect::<Vec<_>>()
        .join(",\n       ");
    format!(
        r"WITH {base}
SELECT f.feature_key, f.scope_type, f.scope_position, f.scope_surface,
       f.source_id, f.text_id, f.region_index,
       b.char_start, b.char_end, b.has_coverage_mismatch,
       f.feature_value,
       {analyzer_flags}
FROM {source} f
JOIN base_regions b USING (source_id, text_id, region_index)
WHERE {feature_predicate}
GROUP BY ALL
ORDER BY f.feature_key, f.scope_type, f.scope_position, f.scope_surface,
         f.source_id, f.text_id, f.region_index, f.feature_value"
    )
}

fn column<'a, T: 'static>(batch: &'a RecordBatch, name: &str) -> Result<&'a T> {
    let index = batch
        .schema()
        .index_of(name)
        .with_context(|| format!("missing column {name}"))?;
    batch
        .column(index)
        .as_any()
        .downcast_ref::<T>()
        .with_context(|| format!("column {name} has unexpected type"))
}

struct CommonAggregates {
    examples: usize,
    source_count: usize,
    text_count: usize,
    rarity_count: usize,
    coverage_region_count: usize,
    span_p90: f64,
    sample_source_ids: Vec<String>,
    sample_text_ids: Vec<String>,
    region_examples: Vec<RegionExampleOut>,
}

fn read_common_aggregates(batch: &RecordBatch, row: usize) -> Result<CommonAggregates> {
    let examples = column::<UInt64Array>(batch, "examples")?.value(row) as usize;
    let source_count = column::<UInt64Array>(batch, "source_count")?.value(row) as usize;
    let text_count = column::<UInt64Array>(batch, "text_count")?.value(row) as usize;
    let rarity_count = column::<UInt64Array>(batch, "rarity_count")?.value(row) as usize;
    let coverage_region_count =
        column::<UInt64Array>(batch, "coverage_regions")?.value(row) as usize;
    let span_p90 = column::<Float64Array>(batch, "span_p90")?.value(row);
    let sample_source_ids: Vec<String> =
        serde_json::from_str(column::<StringArray>(batch, "sample_source_ids")?.value(row))
            .context("failed to parse sample_source_ids JSON")?;
    let sample_text_ids: Vec<String> =
        serde_json::from_str(column::<StringArray>(batch, "sample_text_ids")?.value(row))
            .context("failed to parse sample_text_ids JSON")?;
    let mut region_examples: Vec<RegionExampleOut> =
        serde_json::from_str(column::<StringArray>(batch, "region_examples")?.value(row))
            .context("failed to parse region_examples JSON")?;
    region_examples.sort();
    Ok(CommonAggregates {
        examples,
        source_count,
        text_count,
        rarity_count,
        coverage_region_count,
        span_p90,
        sample_source_ids,
        sample_text_ids,
        region_examples,
    })
}

fn scope_from_columns(
    scope_type: &str,
    scope_position: Option<u64>,
    scope_surface: Option<String>,
) -> Result<NwayFeatureScopeRow> {
    match scope_type {
        "whole_region" => Ok(NwayFeatureScopeRow::WholeRegion),
        "token_position" => Ok(NwayFeatureScopeRow::TokenPosition {
            position: scope_position.context("token_position scope missing scope_position")?
                as usize,
        }),
        "surface" => Ok(NwayFeatureScopeRow::Surface {
            surface: scope_surface.context("surface scope missing scope_surface")?,
        }),
        other => anyhow::bail!("unknown warehouse feature scope_type `{other}`"),
    }
}

fn parse_seg_coverage_rows(batches: &[RecordBatch]) -> Result<Vec<PatternStats>> {
    let mut patterns = Vec::new();
    for batch in batches {
        let kinds = column::<StringArray>(batch, "kind")?;
        let sigs = column::<StringArray>(batch, "sig")?;
        for row in 0..batch.num_rows() {
            let sig = sigs.value(row).to_owned();
            let groups: Vec<JsonSurfaceGroup> =
                serde_json::from_str(&sig).context("failed to parse surface-group JSON")?;
            let mut segmentation_groups = groups
                .into_iter()
                .map(|group| NwaySegmentationGroupRow {
                    surfaces: group.surfaces,
                    analyzers: group.analyzers,
                })
                .collect::<Vec<_>>();
            canonicalize_segmentation_groups(&mut segmentation_groups);
            let kind = match kinds.value(row) {
                "segmentation" => PatternKind::Segmentation,
                _ => PatternKind::Coverage,
            };
            let key = NwayPatternKey {
                kind: kind.as_str().to_owned(),
                segmentation_groups,
                feature_key: None,
                feature_scope: None,
                feature_values: Vec::new(),
            };
            let common = read_common_aggregates(batch, row)?;
            patterns.push(PatternStats {
                pattern_id: pattern_id(&key),
                key,
                kind,
                examples: common.examples,
                source_count: common.source_count,
                text_count: common.text_count,
                sample_source_ids: common.sample_source_ids,
                sample_text_ids: common.sample_text_ids,
                rarity_count: common.rarity_count,
                coverage_region_count: common.coverage_region_count,
                span_p90: common.span_p90,
                region_examples: common.region_examples,
                sql_signature: Some(SqlSignature::Groups { sig }),
            });
        }
    }
    Ok(patterns)
}

/// One fully-buffered `(region, feature_key, scope)` group from the
/// sorted stage output.
struct FeatureGroup {
    feature_key: String,
    scope_type: String,
    scope_position: Option<u64>,
    scope_surface: Option<String>,
    source_id: String,
    text_id: String,
    region_index: u64,
    char_start: u64,
    char_end: u64,
    has_coverage_mismatch: bool,
    values: Vec<(Option<String>, Vec<String>)>,
}

impl FeatureGroup {
    fn matches_row(&self, row: &FeatureStageRowRef<'_>) -> bool {
        self.feature_key == row.feature_key
            && self.scope_type == row.scope_type
            && self.scope_position == row.scope_position
            && self.scope_surface.as_deref() == row.scope_surface
            && self.source_id == row.source_id
            && self.text_id == row.text_id
            && self.region_index == row.region_index
    }

    fn into_key(self) -> Result<Option<(NwayPatternKey, FeatureOccurrence)>> {
        if self.values.len() < 2 {
            return Ok(None);
        }
        let mut feature_values = self
            .values
            .into_iter()
            .map(|(value, analyzers)| NwayFeatureValueGroupRow { value, analyzers })
            .collect::<Vec<_>>();
        canonicalize_feature_values(&mut feature_values);
        let key = NwayPatternKey {
            kind: "feature".to_owned(),
            segmentation_groups: Vec::new(),
            feature_key: Some(self.feature_key.clone()),
            feature_scope: Some(scope_from_columns(
                &self.scope_type,
                self.scope_position,
                self.scope_surface.clone(),
            )?),
            feature_values,
        };
        Ok(Some((
            key,
            FeatureOccurrence {
                source_id: self.source_id,
                text_id: self.text_id,
                region_index: self.region_index,
                char_start: self.char_start,
                char_end: self.char_end,
                has_coverage_mismatch: self.has_coverage_mismatch,
            },
        )))
    }
}

struct FeatureOccurrence {
    source_id: String,
    text_id: String,
    region_index: u64,
    char_start: u64,
    char_end: u64,
    has_coverage_mismatch: bool,
}

struct FeatureStageRowRef<'a> {
    feature_key: &'a str,
    scope_type: &'a str,
    scope_position: Option<u64>,
    scope_surface: Option<&'a str>,
    source_id: &'a str,
    text_id: &'a str,
    region_index: u64,
}

/// Streams a sorted stage file, invoking `sink` once per completed
/// `(region, feature_key, scope)` group that has more than one value
/// group. Bounded memory: one group buffered at a time.
fn stream_feature_stage(
    path: &Path,
    analyzer_ids: &[String],
    sink: &mut impl FnMut(NwayPatternKey, FeatureOccurrence) -> Result<()>,
) -> Result<()> {
    let mut current: Option<FeatureGroup> = None;
    for batch in read_warehouse_parquet_file(path)? {
        let feature_keys = column::<StringArray>(&batch, "feature_key")?;
        let scope_types = column::<StringArray>(&batch, "scope_type")?;
        let scope_positions = column::<UInt64Array>(&batch, "scope_position")?;
        let scope_surfaces = column::<StringArray>(&batch, "scope_surface")?;
        let source_ids = column::<StringArray>(&batch, "source_id")?;
        let text_ids = column::<StringArray>(&batch, "text_id")?;
        let region_indices = column::<UInt64Array>(&batch, "region_index")?;
        let char_starts = column::<UInt64Array>(&batch, "char_start")?;
        let char_ends = column::<UInt64Array>(&batch, "char_end")?;
        let coverage = column::<BooleanArray>(&batch, "has_coverage_mismatch")?;
        let feature_values = column::<StringArray>(&batch, "feature_value")?;
        let analyzer_flags = analyzer_ids
            .iter()
            .enumerate()
            .map(|(index, _)| column::<BooleanArray>(&batch, &format!("analyzer_{index}")))
            .collect::<Result<Vec<_>>>()?;
        for row in 0..batch.num_rows() {
            let row_ref = FeatureStageRowRef {
                feature_key: feature_keys.value(row),
                scope_type: scope_types.value(row),
                scope_position: (!scope_positions.is_null(row)).then(|| scope_positions.value(row)),
                scope_surface: (!scope_surfaces.is_null(row)).then(|| scope_surfaces.value(row)),
                source_id: source_ids.value(row),
                text_id: text_ids.value(row),
                region_index: region_indices.value(row),
            };
            let group_matches = current
                .as_ref()
                .is_some_and(|group| group.matches_row(&row_ref));
            if !group_matches {
                if let Some(group) = current.take()
                    && let Some((key, occurrence)) = group.into_key()?
                {
                    sink(key, occurrence)?;
                }
                current = Some(FeatureGroup {
                    feature_key: row_ref.feature_key.to_owned(),
                    scope_type: row_ref.scope_type.to_owned(),
                    scope_position: row_ref.scope_position,
                    scope_surface: row_ref.scope_surface.map(str::to_owned),
                    source_id: row_ref.source_id.to_owned(),
                    text_id: row_ref.text_id.to_owned(),
                    region_index: row_ref.region_index,
                    char_start: char_starts.value(row),
                    char_end: char_ends.value(row),
                    has_coverage_mismatch: coverage.value(row),
                    values: Vec::new(),
                });
            }
            let value =
                (!feature_values.is_null(row)).then(|| feature_values.value(row).to_owned());
            let analyzers = analyzer_flags
                .iter()
                .enumerate()
                .filter(|(_, flags)| flags.value(row))
                .map(|(index, _)| analyzer_ids[index].clone())
                .collect::<Vec<_>>();
            current
                .as_mut()
                .expect("group buffered")
                .values
                .push((value, analyzers));
        }
    }
    if let Some(group) = current.take()
        && let Some((key, occurrence)) = group.into_key()?
    {
        sink(key, occurrence)?;
    }
    Ok(())
}

/// Writes the `(source_id, text_id, region_index)` exclusion set for the
/// anomaly channel's top-feature-pattern anti-join.
fn write_region_exclusions(path: &Path, regions: &BTreeSet<(String, String, u64)>) -> Result<()> {
    let schema = std::sync::Arc::new(Schema::new(vec![
        Field::new("source_id", DataType::Utf8, false),
        Field::new("text_id", DataType::Utf8, false),
        Field::new("region_index", DataType::UInt64, false),
    ]));
    let batch = RecordBatch::try_new(
        schema.clone(),
        vec![
            std::sync::Arc::new(StringArray::from(
                regions
                    .iter()
                    .map(|(source_id, _, _)| source_id.as_str())
                    .collect::<Vec<_>>(),
            )),
            std::sync::Arc::new(StringArray::from(
                regions
                    .iter()
                    .map(|(_, text_id, _)| text_id.as_str())
                    .collect::<Vec<_>>(),
            )),
            std::sync::Arc::new(UInt64Array::from(
                regions
                    .iter()
                    .map(|(_, _, region_index)| *region_index)
                    .collect::<Vec<_>>(),
            )),
        ],
    )?;
    let properties = WriterProperties::builder()
        .set_compression(Compression::ZSTD(ZstdLevel::try_new(3)?))
        .build();
    let file = std::fs::File::create(path)
        .with_context(|| format!("failed to create {}", path.display()))?;
    let mut writer = ArrowWriter::try_new(file, schema, Some(properties))?;
    writer.write(&batch)?;
    writer.close()?;
    Ok(())
}

/// Collects pattern statistics via the DuckDB CLI. Returns `None` when no
/// `duckdb` binary is available (callers fall back to the in-memory
/// engine).
pub(super) fn collect_patterns_duckdb(
    run_dir: &Path,
    options: &WarehouseInterestingOptions,
    rarity: &RarityConfig,
    analyzer_ids: &std::collections::BTreeSet<String>,
) -> Result<Option<Vec<PatternStats>>> {
    let regions = duckdb_table_path_literal(run_dir, WarehouseTable::NwayRegions);
    let analyzers = duckdb_table_path_literal(run_dir, WarehouseTable::NwayRegionAnalyzers);
    let features = duckdb_table_path_literal(run_dir, WarehouseTable::NwayFeatureDiffs);
    let shape = nway_feature_diffs_shape(run_dir)?;
    let (works_join, rarity_key) = rarity_sql(run_dir, rarity);
    let analyzer_ids = analyzer_ids.iter().cloned().collect::<Vec<_>>();

    remove_stale_temp_outputs(run_dir);
    let Some(feature_keys) = discover_feature_keys(run_dir, &features, options.feature_profile)?
    else {
        return Ok(None);
    };

    let seg_out = temp_output_path(run_dir, "seg");
    let seg_body = seg_coverage_query(
        &regions,
        &analyzers,
        options.filter,
        &works_join,
        &rarity_key,
        options.max_region_examples,
    );
    let sql = format!(
        "{settings}\nCOPY ({seg_body}) TO {seg_out} (FORMAT PARQUET, COMPRESSION ZSTD);",
        settings = interesting_settings_sql(run_dir),
        seg_out = sql_literal(&seg_out.display().to_string()),
    );
    if !run_duckdb_statement(run_dir, sql, "interestingness pattern aggregation")? {
        return Ok(None);
    }
    let mut patterns = parse_seg_coverage_rows(&read_warehouse_parquet_file(&seg_out)?)?;
    let _ = fs::remove_file(&seg_out);

    let mut accumulator = PatternAccumulator::default();
    let work_by_source = rarity.work_by_source.as_ref();
    for (index, batch_keys) in feature_keys.chunks(feature_key_batch_size()).enumerate() {
        let feat_out = temp_output_path(run_dir, &format!("feat-{index}"));
        let feat_body = feature_stage_query(
            &regions,
            &analyzers,
            &features,
            options.filter,
            &feature_key_in_predicate(batch_keys),
            &analyzer_ids,
            shape,
        );
        let sql = format!(
            "{settings}\nCOPY ({feat_body}) TO {feat_out} (FORMAT PARQUET, COMPRESSION ZSTD);",
            settings = interesting_settings_sql(run_dir),
            feat_out = sql_literal(&feat_out.display().to_string()),
        );
        if !run_duckdb_statement(run_dir, sql, "interestingness feature aggregation")? {
            return Ok(None);
        }
        stream_feature_stage(&feat_out, &analyzer_ids, &mut |key, occurrence| {
            let rarity_key = work_by_source
                .and_then(|map| map.get(&occurrence.source_id))
                .map_or(occurrence.source_id.as_str(), String::as_str);
            accumulator.record(
                key,
                PatternKind::Feature,
                &RegionOccurrence {
                    source_id: &occurrence.source_id,
                    text_id: &occurrence.text_id,
                    region_index: occurrence.region_index,
                    char_start: occurrence.char_start,
                    char_end: occurrence.char_end,
                    has_coverage_mismatch: occurrence.has_coverage_mismatch,
                    rarity_key,
                },
                options.max_region_examples,
            );
            Ok(())
        })?;
        let _ = fs::remove_file(&feat_out);
    }
    patterns.extend(accumulator.finalize());

    // Deterministic pattern order regardless of DuckDB's group emission
    // order (ranking re-sorts, but rank tie-breaks read pattern order via
    // pattern_id, and the path-equality test compares full summaries).
    patterns.sort_by(|left, right| left.pattern_id.cmp(&right.pattern_id));
    Ok(Some(patterns))
}

/// Computes the anomaly channel via DuckDB: disagreement regions passing
/// the filter whose patterns all fell below the emitted top set, scored by
/// `w_cov * has_coverage_mismatch + log2(1 + char_length)`.
pub(super) fn anomalies_duckdb(
    run_dir: &Path,
    options: &WarehouseInterestingOptions,
    top: &[&PatternStats],
    analyzer_ids: &std::collections::BTreeSet<String>,
) -> Result<Vec<AnomalyRow>> {
    if options.anomalies == 0 {
        return Ok(Vec::new());
    }
    let regions = duckdb_table_path_literal(run_dir, WarehouseTable::NwayRegions);
    let analyzers = duckdb_table_path_literal(run_dir, WarehouseTable::NwayRegionAnalyzers);
    let features = duckdb_table_path_literal(run_dir, WarehouseTable::NwayFeatureDiffs);

    let mut seg_sigs = BTreeSet::new();
    let mut cov_sigs = BTreeSet::new();
    let mut top_feature_keys = BTreeSet::new();
    for stats in top {
        match (&stats.sql_signature, stats.kind) {
            (Some(SqlSignature::Groups { sig }), PatternKind::Segmentation) => {
                seg_sigs.insert(sig.clone());
            }
            (Some(SqlSignature::Groups { sig }), _) => {
                cov_sigs.insert(sig.clone());
            }
            (None, PatternKind::Feature) => {
                top_feature_keys.insert(stats.key.clone());
            }
            (None, _) => anyhow::bail!(
                "pattern {} has no SQL signature; anomaly exclusion cannot be built",
                stats.pattern_id
            ),
        }
    }

    // Top-feature-pattern regions: re-run the sorted stage query for just
    // the top patterns' feature keys (a handful at most), stream it to
    // find every region owning a top pattern, and write those region keys
    // to an exclusion parquet for the anti-join.
    let feature_exclusion_path = if top_feature_keys.is_empty() {
        None
    } else {
        let key_names = top_feature_keys
            .iter()
            .filter_map(|key| key.feature_key.clone())
            .collect::<Vec<_>>();
        let analyzer_ids = analyzer_ids.iter().cloned().collect::<Vec<_>>();
        let shape = nway_feature_diffs_shape(run_dir)?;
        let stage_out = temp_output_path(run_dir, "top-feature-stage");
        let stage_body = feature_stage_query(
            &regions,
            &analyzers,
            &features,
            options.filter,
            &feature_key_in_predicate(&key_names),
            &analyzer_ids,
            shape,
        );
        let sql = format!(
            "{settings}\nCOPY ({stage_body}) TO {stage_out} (FORMAT PARQUET, COMPRESSION ZSTD);",
            settings = interesting_settings_sql(run_dir),
            stage_out = sql_literal(&stage_out.display().to_string()),
        );
        if !run_duckdb_statement(run_dir, sql, "interestingness anomaly feature exclusion")? {
            anyhow::bail!("duckdb binary disappeared between pattern and anomaly queries");
        }
        let mut excluded = BTreeSet::new();
        stream_feature_stage(&stage_out, &analyzer_ids, &mut |key, occurrence| {
            if top_feature_keys.contains(&key) {
                excluded.insert((
                    occurrence.source_id,
                    occurrence.text_id,
                    occurrence.region_index,
                ));
            }
            Ok(())
        })?;
        let _ = fs::remove_file(&stage_out);
        let path = temp_output_path(run_dir, "feature-exclusions");
        write_region_exclusions(&path, &excluded)?;
        Some(path)
    };

    let base = base_regions_cte(&regions, &analyzers, options.filter, "TRUE");
    let mut ctes = vec![base];
    let mut exclusions = Vec::new();
    if !seg_sigs.is_empty() || !cov_sigs.is_empty() {
        ctes.push(region_sig_cte(
            &analyzers,
            "(has_segmentation_disagreement OR has_coverage_mismatch)",
        ));
        if !seg_sigs.is_empty() {
            let list = sig_in_list(&seg_sigs);
            exclusions.push(format!(
                "NOT (b.has_segmentation_disagreement AND EXISTS (SELECT 1 FROM region_sig s \
                 WHERE s.source_id = b.source_id AND s.text_id = b.text_id \
                 AND s.region_index = b.region_index AND s.group_count > 1 AND s.sig IN ({list})))"
            ));
        }
        if !cov_sigs.is_empty() {
            let list = sig_in_list(&cov_sigs);
            exclusions.push(format!(
                "NOT (b.has_coverage_mismatch AND EXISTS (SELECT 1 FROM region_sig s \
                 WHERE s.source_id = b.source_id AND s.text_id = b.text_id \
                 AND s.region_index = b.region_index AND s.sig IN ({list})))"
            ));
        }
    }
    if let Some(path) = &feature_exclusion_path {
        let exclusion_table = sql_literal(&path.display().to_string());
        exclusions.push(format!(
            "NOT EXISTS (SELECT 1 FROM read_parquet({exclusion_table}) e \
             WHERE e.source_id = b.source_id AND e.text_id = b.text_id \
             AND e.region_index = b.region_index)"
        ));
    }
    let exclusion_clause = if exclusions.is_empty() {
        "TRUE".to_owned()
    } else {
        exclusions.join("\n  AND ")
    };
    let out = temp_output_path(run_dir, "anomalies");
    let body = format!(
        r"WITH {ctes}
SELECT b.source_id, b.text_id, b.region_index, b.char_start, b.char_end,
       b.has_coverage_mismatch,
       round(CASE WHEN b.has_coverage_mismatch THEN {w_cov} ELSE 0.0 END
             + log2(1.0 + (b.char_end - b.char_start)), 6) AS anomaly_score
FROM base_regions b
WHERE NOT b.is_agreement
  AND {exclusion_clause}
ORDER BY anomaly_score DESC, b.source_id, b.text_id, b.region_index
LIMIT {limit}",
        ctes = ctes.join(",\n"),
        w_cov = options.anomaly_w_cov,
        limit = options.anomalies,
    );
    let sql = format!(
        "{settings}\nCOPY ({body}) TO {out} (FORMAT PARQUET, COMPRESSION ZSTD);",
        settings = interesting_settings_sql(run_dir),
        out = sql_literal(&out.display().to_string()),
    );
    if !run_duckdb_statement(run_dir, sql, "interestingness anomaly channel")? {
        anyhow::bail!("duckdb binary disappeared between pattern and anomaly queries");
    }

    let mut rows = Vec::new();
    for batch in read_warehouse_parquet_file(&out)? {
        let source_ids = column::<StringArray>(&batch, "source_id")?;
        let text_ids = column::<StringArray>(&batch, "text_id")?;
        let region_indices = column::<UInt64Array>(&batch, "region_index")?;
        let char_starts = column::<UInt64Array>(&batch, "char_start")?;
        let char_ends = column::<UInt64Array>(&batch, "char_end")?;
        let coverage = column::<arrow_array::BooleanArray>(&batch, "has_coverage_mismatch")?;
        let scores = column::<Float64Array>(&batch, "anomaly_score")?;
        for row in 0..batch.num_rows() {
            rows.push(AnomalyRow {
                source_id: source_ids.value(row).to_owned(),
                text_id: text_ids.value(row).to_owned(),
                region_index: region_indices.value(row),
                char_start: char_starts.value(row),
                char_end: char_ends.value(row),
                has_coverage_mismatch: coverage.value(row),
                anomaly_score: scores.value(row),
            });
        }
    }
    let _ = fs::remove_file(&out);
    if let Some(path) = &feature_exclusion_path {
        let _ = fs::remove_file(path);
    }
    Ok(rows)
}

fn sig_in_list(values: &BTreeSet<String>) -> String {
    values
        .iter()
        .map(|value| sql_literal(value))
        .collect::<Vec<_>>()
        .join(", ")
}
