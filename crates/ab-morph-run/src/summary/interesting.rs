//! Interestingness ranking over v1 warehouse pattern data.
//!
//! Implements `summarize-warehouse-interesting`: per-pattern Reciprocal Rank
//! Fusion over four signals (coverage, rarity, impact, span), ranked
//! within-kind, with an anomaly channel for high-signal regions whose
//! patterns fall below the cutoff. Reads only existing v1 tables; degrades
//! honestly (`rarity_basis = "source"`) when `aozora_works.parquet` is
//! absent. See `docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md`.
//!
//! Two collection engines produce identical results: a DuckDB CLI
//! aggregation (required at full-corpus scale — the full Aozora warehouse
//! holds ~165M regions and ~17.8B feature-diff rows, far beyond what the
//! in-memory path can hold) and a pure in-memory path used for small runs,
//! tests, and environments without a `duckdb` binary.

use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::io::Write;
use std::path::Path;

use anyhow::{Context, Result, bail};
use arrow_array::{StringArray, UInt32Array};
use clap::ValueEnum;
use serde::Serialize;
use unicode_properties::{GeneralCategoryGroup, UnicodeGeneralCategory};

use super::interesting_sql;
use super::pattern_id::{PATTERN_ID_VERSION, pattern_id};
use super::summary_body::{
    NwayPatternKey, WarehouseRegionAnalyzerFact, WarehouseRegionFlags, WarehouseRegionKey,
    canonicalize_segmentation_groups, nway_pattern_display, read_warehouse_feature_diffs,
    read_warehouse_parquet_file, read_warehouse_region_analyzers, read_warehouse_region_flags,
    read_warehouse_table, string_column, warehouse_feature_facts_for_profile,
    warehouse_feature_key_in_profile, warehouse_feature_pattern_key,
    warehouse_segmentation_pattern_key,
};
use crate::nway::NwaySegmentationGroupRow;
use crate::summary::WarehouseFeatureProfile;
use crate::warehouse::schema::WarehouseTable;

pub(crate) const SCORE_VERSION: u32 = 1;
pub(crate) const READER_MAX_SCHEMA_VERSION: u32 = 1;
pub(crate) const RRF_K: f64 = 60.0;
pub(crate) const ANOMALY_W_COV: f64 = 5.0;
const MAX_SAMPLE_IDS: usize = 5;

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum InterestingTextFilter {
    All,
    LexicalOnly,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum InterestingOutputFormat {
    Table,
    Json,
}

/// Collection engine. `Auto` prefers DuckDB (necessary at full-corpus
/// scale) and falls back to the in-memory path when no `duckdb` binary is
/// available.
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum InterestingEngine {
    Auto,
    InMemory,
    Duckdb,
}

#[derive(Debug, Clone)]
pub struct WarehouseInterestingOptions {
    pub limit: usize,
    pub filter: InterestingTextFilter,
    pub anomalies: usize,
    pub explain: Option<String>,
    pub max_region_examples: usize,
    pub engine: InterestingEngine,
    pub feature_profile: WarehouseFeatureProfile,
}

impl Default for WarehouseInterestingOptions {
    fn default() -> Self {
        Self {
            limit: 50,
            filter: InterestingTextFilter::All,
            anomalies: 10,
            explain: None,
            max_region_examples: 5,
            engine: InterestingEngine::Auto,
            feature_profile: WarehouseFeatureProfile::Raw,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct InterestingSummary {
    pub score_version: ScoreVersionBlock,
    pub run_id: String,
    pub rows: Vec<InterestingRow>,
    pub anomalies: Vec<AnomalyRow>,
}

/// Every knob affecting cross-run comparability. Two runs are comparable
/// only when all fields match (spec §Score Versioning).
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ScoreVersionBlock {
    pub score_version: u32,
    pub pattern_id_version: u32,
    pub rrf_k: u32,
    /// `λ_missing` is not a fixed constant: a fixed value breaks the
    /// missing-signal monotonicity invariant for ranks past `1/λ - k`. The
    /// rank-floor policy sets `λ = 1/(k + N_kind + 1)` — "just below the
    /// worst-ranked observed pattern of the kind" — which preserves it.
    pub lambda_missing_policy: String,
    pub anomaly_w_cov: f64,
    pub signal_profile: Vec<String>,
    /// Which feature keys were admitted to feature-pattern collection
    /// (`raw` = all, `core` = pos1..pos4). Changes which patterns exist,
    /// so it gates comparability like `granularity_profile`.
    pub feature_profile: String,
    pub rarity_basis: String,
    pub granularity_profile: String,
    pub cause_classification_profile: String,
    pub literal_context_policy: Option<String>,
    pub surprise: String,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct InterestingRow {
    pub pattern_id: String,
    pub pattern: String,
    pub kind: String,
    pub rrf_score: f64,
    pub signal_profile: Vec<String>,
    pub signals: Vec<SignalExplain>,
    pub examples: usize,
    pub source_count: usize,
    pub text_count: usize,
    pub sample_source_ids: Vec<String>,
    pub sample_text_ids: Vec<String>,
    pub region_examples: Vec<RegionExampleOut>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SignalExplain {
    pub signal: String,
    pub status: String,
    pub raw_value: Option<f64>,
    pub rank: Option<u64>,
    pub rrf_term: f64,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, serde::Deserialize)]
pub struct RegionExampleOut {
    pub source_id: String,
    pub text_id: String,
    pub region_index: u64,
    pub char_start: u64,
    pub char_end: u64,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct AnomalyRow {
    pub source_id: String,
    pub text_id: String,
    pub region_index: u64,
    pub char_start: u64,
    pub char_end: u64,
    pub has_coverage_mismatch: bool,
    pub anomaly_score: f64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum PatternKind {
    Feature,
    Segmentation,
    Coverage,
}

impl PatternKind {
    pub(super) fn as_str(self) -> &'static str {
        match self {
            Self::Feature => "feature",
            Self::Segmentation => "segmentation",
            Self::Coverage => "coverage",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Signal {
    Coverage,
    Rarity,
    Impact,
    Span,
}

impl Signal {
    fn name(self) -> &'static str {
        match self {
            Self::Coverage => "coverage",
            Self::Rarity => "rarity",
            Self::Impact => "impact",
            Self::Span => "span",
        }
    }

    /// Applicability is kind-level (spec §Scoring): impact only fires for
    /// feature patterns. `|S_applicable|` is constant per kind.
    fn applicable(kind: PatternKind) -> &'static [Signal] {
        match kind {
            PatternKind::Feature => &[Self::Coverage, Self::Rarity, Self::Impact, Self::Span],
            PatternKind::Segmentation | PatternKind::Coverage => {
                &[Self::Coverage, Self::Rarity, Self::Span]
            }
        }
    }
}

/// Rounds to 6 decimal places, half away from zero (half-up for the
/// non-negative values used here). Applied before serialization so golden
/// outputs are byte-stable (spec §Tie-Breaking and Determinism).
pub(crate) fn round6(value: f64) -> f64 {
    (value * 1e6).round() / 1e6
}

fn impact_weight(feature_key: &str) -> f64 {
    match feature_key {
        "pos1" => 4.0,
        "lemma" => 3.5,
        "ctype" => 2.5,
        "cform" => 2.0,
        "kana" | "kana_base" | "pron" | "pron_base" | "lform" => 1.5,
        _ => 1.0,
    }
}

/// Nearest-rank 90th percentile of an unsorted, non-empty sample. Matches
/// DuckDB `quantile_disc(x, 0.9)` so both engines agree exactly.
fn percentile_90(lengths: &[u64]) -> f64 {
    let mut sorted = lengths.to_vec();
    sorted.sort_unstable();
    let n = sorted.len();
    let rank = (0.9 * n as f64).ceil() as usize;
    sorted[rank.max(1) - 1] as f64
}

/// The `λ_missing` rank-floor policy: an applicable signal with missing
/// data scores as if ranked just below the worst observed pattern of the
/// kind.
fn lambda_missing(kind_pattern_count: usize) -> f64 {
    1.0 / (RRF_K + kind_pattern_count as f64 + 1.0)
}

fn rrf_term(rank: Option<usize>, lambda: f64) -> f64 {
    match rank {
        Some(rank) => 1.0 / (RRF_K + rank as f64),
        None => lambda,
    }
}

/// Normalized RRF fusion (spec §Scoring): sum of per-signal terms (missing
/// applicable signals contribute `lambda`) divided by the kind-constant
/// applicable-signal count.
fn fuse(ranks: &[Option<usize>], applicable_count: usize, lambda: f64) -> f64 {
    debug_assert_eq!(ranks.len(), applicable_count);
    ranks.iter().map(|rank| rrf_term(*rank, lambda)).sum::<f64>() / applicable_count as f64
}

/// Every char is punctuation (`P*`), a separator (`Z*`), or whitespace
/// (`char::is_whitespace`, covering the `Cc` whitespace controls). Empty
/// input is not punctuation-only: a region where every analyzer emitted
/// zero morphemes must stay rankable under `--filter lexical-only`.
fn is_punctuation_only(chars: impl Iterator<Item = char>) -> bool {
    let mut saw_any = false;
    for ch in chars {
        saw_any = true;
        let group = ch.general_category_group();
        let allowed = ch.is_whitespace()
            || matches!(
                group,
                GeneralCategoryGroup::Punctuation | GeneralCategoryGroup::Separator
            );
        if !allowed {
            return false;
        }
    }
    saw_any
}

fn region_is_punctuation_only(facts: &[WarehouseRegionAnalyzerFact]) -> bool {
    is_punctuation_only(
        facts
            .iter()
            .flat_map(|fact| fact.surfaces.iter())
            .flat_map(|surface| surface.chars()),
    )
}

fn region_passes_filter(
    filter: InterestingTextFilter,
    flags: &WarehouseRegionFlags,
    punctuation_only: bool,
) -> bool {
    match filter {
        InterestingTextFilter::All => true,
        InterestingTextFilter::LexicalOnly => !flags.is_nonempty_whitespace && !punctuation_only,
    }
}

/// Coverage-kind pattern key: the segmentation-style surface-group key over
/// a coverage-mismatch region. Unlike segmentation, a single surface group
/// is allowed — the coverage mismatch itself is the finding even when all
/// analyzers agree on surfaces.
fn coverage_pattern_key(facts: &[WarehouseRegionAnalyzerFact]) -> Option<NwayPatternKey> {
    if facts.is_empty() {
        return None;
    }
    let mut surface_groups = BTreeMap::<Vec<String>, Vec<String>>::new();
    for fact in facts {
        surface_groups
            .entry(fact.surfaces.clone())
            .or_default()
            .push(fact.analyzer_id.clone());
    }
    let mut segmentation_groups = surface_groups
        .into_iter()
        .map(|(surfaces, analyzers)| NwaySegmentationGroupRow {
            surfaces,
            analyzers,
        })
        .collect::<Vec<_>>();
    canonicalize_segmentation_groups(&mut segmentation_groups);
    Some(NwayPatternKey {
        kind: "coverage".to_owned(),
        segmentation_groups,
        feature_key: None,
        feature_scope: None,
        feature_values: Vec::new(),
    })
}

/// The raw SQL grouping columns a pattern was aggregated under (DuckDB
/// engine only). Reused verbatim to build the anomaly channel's
/// top-pattern exclusion predicates, so the exclusion matches the
/// aggregation byte-for-byte.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum SqlSignature {
    /// Segmentation/coverage kinds: the canonical surface-group JSON.
    /// Feature-kind patterns from the DuckDB engine carry no SQL
    /// signature; their anomaly exclusion re-streams the sorted stage
    /// files instead.
    Groups { sig: String },
}

/// Finalized per-pattern statistics, engine-agnostic. Both collection
/// engines must produce identical values here — pinned by the
/// path-equality test.
#[derive(Debug, Clone)]
pub(super) struct PatternStats {
    pub(super) key: NwayPatternKey,
    pub(super) pattern_id: String,
    pub(super) kind: PatternKind,
    pub(super) examples: usize,
    pub(super) source_count: usize,
    pub(super) text_count: usize,
    pub(super) sample_source_ids: Vec<String>,
    pub(super) sample_text_ids: Vec<String>,
    pub(super) rarity_count: usize,
    pub(super) coverage_region_count: usize,
    pub(super) span_p90: f64,
    pub(super) region_examples: Vec<RegionExampleOut>,
    pub(super) sql_signature: Option<SqlSignature>,
}

/// Rarity configuration shared by both engines: basis and denominator are
/// always computed in Rust from `sources` plus the optional
/// `aozora_works.parquet` projection.
pub(super) struct RarityConfig {
    pub(super) work_by_source: Option<BTreeMap<String, String>>,
    pub(super) basis: &'static str,
    pub(super) total: usize,
}

#[derive(Debug, Default)]
struct PatternAccum {
    examples: usize,
    source_ids: BTreeSet<String>,
    text_ids: BTreeSet<String>,
    rarity_keys: BTreeSet<String>,
    coverage_region_count: usize,
    span_lengths: Vec<u64>,
    region_examples: Vec<RegionExampleOut>,
}

/// One pattern occurrence, engine-agnostic. Both the in-memory engine and
/// the DuckDB streaming feature path feed these into
/// [`PatternAccumulator`], so per-pattern statistics are computed by one
/// code path regardless of engine.
pub(super) struct RegionOccurrence<'a> {
    pub(super) source_id: &'a str,
    pub(super) text_id: &'a str,
    pub(super) region_index: u64,
    pub(super) char_start: u64,
    pub(super) char_end: u64,
    pub(super) has_coverage_mismatch: bool,
    pub(super) rarity_key: &'a str,
}

#[derive(Default)]
pub(super) struct PatternAccumulator {
    index_of: BTreeMap<NwayPatternKey, usize>,
    keys: Vec<(NwayPatternKey, PatternKind)>,
    accums: Vec<PatternAccum>,
}

impl PatternAccumulator {
    /// Records one occurrence and returns the pattern's dense index.
    pub(super) fn record(
        &mut self,
        key: NwayPatternKey,
        kind: PatternKind,
        occurrence: &RegionOccurrence<'_>,
        max_region_examples: usize,
    ) -> usize {
        let next_index = self.accums.len();
        let index = *self.index_of.entry(key.clone()).or_insert(next_index);
        if index == next_index {
            self.keys.push((key, kind));
            self.accums.push(PatternAccum::default());
        }
        let accum = &mut self.accums[index];
        accum.examples += 1;
        if !accum.source_ids.contains(occurrence.source_id) {
            accum.source_ids.insert(occurrence.source_id.to_owned());
        }
        if !accum.text_ids.contains(occurrence.text_id) {
            accum.text_ids.insert(occurrence.text_id.to_owned());
        }
        if !accum.rarity_keys.contains(occurrence.rarity_key) {
            accum.rarity_keys.insert(occurrence.rarity_key.to_owned());
        }
        if occurrence.has_coverage_mismatch {
            accum.coverage_region_count += 1;
        }
        accum
            .span_lengths
            .push(occurrence.char_end - occurrence.char_start);
        if accum.region_examples.len() < max_region_examples {
            accum.region_examples.push(RegionExampleOut {
                source_id: occurrence.source_id.to_owned(),
                text_id: occurrence.text_id.to_owned(),
                region_index: occurrence.region_index,
                char_start: occurrence.char_start,
                char_end: occurrence.char_end,
            });
        }
        index
    }

    pub(super) fn finalize(self) -> Vec<PatternStats> {
        self.keys
            .into_iter()
            .zip(self.accums)
            .map(|((key, kind), accum)| PatternStats {
                pattern_id: pattern_id(&key),
                key,
                kind,
                examples: accum.examples,
                source_count: accum.source_ids.len(),
                text_count: accum.text_ids.len(),
                sample_source_ids: accum
                    .source_ids
                    .iter()
                    .take(MAX_SAMPLE_IDS)
                    .cloned()
                    .collect(),
                sample_text_ids: accum
                    .text_ids
                    .iter()
                    .take(MAX_SAMPLE_IDS)
                    .cloned()
                    .collect(),
                rarity_count: accum.rarity_keys.len(),
                coverage_region_count: accum.coverage_region_count,
                span_p90: percentile_90(&accum.span_lengths),
                region_examples: accum.region_examples,
                sql_signature: None,
            })
            .collect()
    }
}

enum AnomalySource {
    InMemory {
        memberships: BTreeMap<WarehouseRegionKey, BTreeSet<usize>>,
        region_flags: BTreeMap<WarehouseRegionKey, WarehouseRegionFlags>,
        punctuation_only: BTreeSet<WarehouseRegionKey>,
    },
    /// DuckDB engine: anomalies are computed by a follow-up query once the
    /// top pattern set is known. Carries the sorted feature-stage parquet
    /// files retained for the top-pattern region exclusion pass.
    Deferred {
        feature_stage_files: Vec<std::path::PathBuf>,
    },
}

struct Collected {
    patterns: Vec<PatternStats>,
    anomaly_source: AnomalySource,
}

struct InMemoryAccumulators<'a> {
    patterns: PatternAccumulator,
    memberships: BTreeMap<WarehouseRegionKey, BTreeSet<usize>>,
    work_by_source: Option<&'a BTreeMap<String, String>>,
    max_region_examples: usize,
}

impl InMemoryAccumulators<'_> {
    fn record(
        &mut self,
        key: NwayPatternKey,
        kind: PatternKind,
        region: &WarehouseRegionKey,
        flags: &WarehouseRegionFlags,
    ) {
        let rarity_key = self
            .work_by_source
            .and_then(|map| map.get(&region.source_id))
            .map_or(region.source_id.as_str(), String::as_str);
        let index = self.patterns.record(
            key,
            kind,
            &RegionOccurrence {
                source_id: &region.source_id,
                text_id: &region.text_id,
                region_index: region.region_index,
                char_start: flags.char_start,
                char_end: flags.char_end,
                has_coverage_mismatch: flags.has_coverage_mismatch,
                rarity_key,
            },
            self.max_region_examples,
        );
        self.memberships
            .entry(region.clone())
            .or_default()
            .insert(index);
    }

    fn finalize(self) -> (Vec<PatternStats>, BTreeMap<WarehouseRegionKey, BTreeSet<usize>>) {
        (self.patterns.finalize(), self.memberships)
    }
}

fn read_run_meta(run_dir: &Path) -> Result<(u32, String)> {
    let batches = read_warehouse_table(run_dir, WarehouseTable::Runs)
        .with_context(|| format!("{} is not a warehouse run directory", run_dir.display()))?;
    for batch in batches {
        if batch.num_rows() == 0 {
            continue;
        }
        let schema_version = batch
            .column(0)
            .as_any()
            .downcast_ref::<UInt32Array>()
            .context("runs.schema_version is not a UInt32Array")?;
        let run_id = string_column(&batch, 1)?;
        return Ok((schema_version.value(0), run_id.value(0).to_owned()));
    }
    bail!("runs.parquet has no rows in {}", run_dir.display());
}

fn read_distinct_column(
    run_dir: &Path,
    table: WarehouseTable,
    column: usize,
) -> Result<BTreeSet<String>> {
    let mut values = BTreeSet::new();
    for batch in read_warehouse_table(run_dir, table)? {
        let array = string_column(&batch, column)?;
        for row in 0..batch.num_rows() {
            values.insert(array.value(row).to_owned());
        }
    }
    Ok(values)
}

/// Optional v2-forward probe: when `aozora_works.parquet` is present the
/// rarity signal deduplicates by `work_id` (spec §Per-Signal Definitions).
/// Column lookup is by name so this reader does not depend on a column
/// order the v2 schema has not pinned yet.
fn read_optional_work_map(run_dir: &Path) -> Result<Option<BTreeMap<String, String>>> {
    let path = run_dir.join("aozora_works.parquet");
    if !path.exists() {
        return Ok(None);
    }
    let mut map = BTreeMap::new();
    for batch in read_warehouse_parquet_file(&path)? {
        let schema = batch.schema();
        let work_index = schema
            .index_of("work_id")
            .context("aozora_works.parquet is missing a work_id column")?;
        let source_index = schema
            .index_of("source_id")
            .context("aozora_works.parquet is missing a source_id column")?;
        let work_ids = batch
            .column(work_index)
            .as_any()
            .downcast_ref::<StringArray>()
            .context("aozora_works.work_id is not a StringArray")?;
        let source_ids = batch
            .column(source_index)
            .as_any()
            .downcast_ref::<StringArray>()
            .context("aozora_works.source_id is not a StringArray")?;
        for row in 0..batch.num_rows() {
            map.insert(
                source_ids.value(row).to_owned(),
                work_ids.value(row).to_owned(),
            );
        }
    }
    Ok(Some(map))
}

fn rarity_config(run_dir: &Path, source_ids: &BTreeSet<String>) -> Result<RarityConfig> {
    let work_by_source = read_optional_work_map(run_dir)?;
    let (basis, total) = match &work_by_source {
        Some(map) => {
            let works: BTreeSet<&String> = source_ids
                .iter()
                .filter_map(|source_id| map.get(source_id))
                .collect();
            ("work", works.len())
        }
        None => ("source", source_ids.len()),
    };
    Ok(RarityConfig {
        work_by_source,
        basis,
        total,
    })
}

fn collect_in_memory(
    run_dir: &Path,
    options: &WarehouseInterestingOptions,
    rarity: &RarityConfig,
) -> Result<Collected> {
    let region_flags = read_warehouse_region_flags(run_dir)?;

    let mut by_region = BTreeMap::<WarehouseRegionKey, Vec<WarehouseRegionAnalyzerFact>>::new();
    for fact in read_warehouse_region_analyzers(run_dir)? {
        by_region.entry(fact.key.clone()).or_default().push(fact);
    }
    let mut punctuation_only = BTreeSet::new();
    for (region, facts) in &by_region {
        if region_is_punctuation_only(facts) {
            punctuation_only.insert(region.clone());
        }
    }

    let mut accumulators = InMemoryAccumulators {
        patterns: PatternAccumulator::default(),
        memberships: BTreeMap::new(),
        work_by_source: rarity.work_by_source.as_ref(),
        max_region_examples: options.max_region_examples,
    };

    for (region, facts) in &by_region {
        let Some(flags) = region_flags.get(region) else {
            continue;
        };
        if !region_passes_filter(options.filter, flags, punctuation_only.contains(region)) {
            continue;
        }
        if flags.has_segmentation_disagreement
            && let Some(key) = warehouse_segmentation_pattern_key(facts)
        {
            accumulators.record(key, PatternKind::Segmentation, region, flags);
        }
        if flags.has_coverage_mismatch
            && let Some(key) = coverage_pattern_key(facts)
        {
            accumulators.record(key, PatternKind::Coverage, region, flags);
        }
    }

    let mut by_feature_group = BTreeMap::<_, Vec<_>>::new();
    for fact in read_warehouse_feature_diffs(run_dir)? {
        if !warehouse_feature_key_in_profile(&fact.key.feature_key, options.feature_profile) {
            continue;
        }
        by_feature_group
            .entry(fact.key.clone())
            .or_default()
            .push(fact);
    }
    for (group, facts) in by_feature_group {
        let Some(flags) = region_flags.get(&group.region) else {
            continue;
        };
        if !region_passes_filter(
            options.filter,
            flags,
            punctuation_only.contains(&group.region),
        ) {
            continue;
        }
        let facts = warehouse_feature_facts_for_profile(options.feature_profile, facts);
        if facts.is_empty() {
            continue;
        }
        let key = warehouse_feature_pattern_key(&group, &facts)?;
        if key.feature_values.len() > 1 {
            let region = group.region.clone();
            accumulators.record(key, PatternKind::Feature, &region, flags);
        }
    }

    let (patterns, memberships) = accumulators.finalize();
    Ok(Collected {
        patterns,
        anomaly_source: AnomalySource::InMemory {
            memberships,
            region_flags,
            punctuation_only,
        },
    })
}

fn raw_signal_value(signal: Signal, stats: &PatternStats, rarity_total: usize) -> Option<f64> {
    match signal {
        Signal::Coverage => Some((1.0 + stats.coverage_region_count as f64).log2()),
        Signal::Rarity => {
            Some(((rarity_total as f64 + 1.0) / (stats.rarity_count as f64 + 1.0)).log2())
        }
        Signal::Impact => stats.key.feature_key.as_deref().map(impact_weight),
        Signal::Span => Some(stats.span_p90),
    }
}

#[derive(Debug, Clone)]
struct PatternScore {
    /// Aligned with `Signal::applicable(kind)`.
    signals: Vec<(Signal, Option<f64>, Option<usize>)>,
    lambda: f64,
    rrf_score: f64,
}

/// Ranks patterns per kind per signal (raw desc, `pattern_id` asc — the
/// deterministic tie-break) and fuses. Returns scores aligned with
/// `patterns`.
fn score_patterns(patterns: &[PatternStats], rarity_total: usize) -> Vec<PatternScore> {
    let mut scores = patterns
        .iter()
        .map(|stats| {
            let applicable = Signal::applicable(stats.kind);
            PatternScore {
                signals: applicable
                    .iter()
                    .map(|signal| {
                        (*signal, raw_signal_value(*signal, stats, rarity_total), None)
                    })
                    .collect(),
                lambda: 0.0,
                rrf_score: 0.0,
            }
        })
        .collect::<Vec<_>>();

    for kind in [
        PatternKind::Feature,
        PatternKind::Segmentation,
        PatternKind::Coverage,
    ] {
        let kind_indices = patterns
            .iter()
            .enumerate()
            .filter(|(_, stats)| stats.kind == kind)
            .map(|(index, _)| index)
            .collect::<Vec<_>>();
        if kind_indices.is_empty() {
            continue;
        }
        let lambda = lambda_missing(kind_indices.len());
        for index in &kind_indices {
            scores[*index].lambda = lambda;
        }
        for (signal_slot, _signal) in Signal::applicable(kind).iter().enumerate() {
            let mut present = kind_indices
                .iter()
                .filter_map(|index| {
                    scores[*index].signals[signal_slot]
                        .1
                        .map(|raw| (*index, raw))
                })
                .collect::<Vec<_>>();
            present.sort_by(|left, right| {
                right
                    .1
                    .partial_cmp(&left.1)
                    .unwrap_or(Ordering::Equal)
                    .then_with(|| {
                        patterns[left.0]
                            .pattern_id
                            .cmp(&patterns[right.0].pattern_id)
                    })
            });
            for (rank_zero, (index, _)) in present.iter().enumerate() {
                scores[*index].signals[signal_slot].2 = Some(rank_zero + 1);
            }
        }
        for index in kind_indices {
            let ranks = scores[index]
                .signals
                .iter()
                .map(|(_, _, rank)| *rank)
                .collect::<Vec<_>>();
            scores[index].rrf_score = round6(fuse(&ranks, ranks.len(), lambda));
        }
    }
    scores
}

fn build_row(stats: &PatternStats, score: &PatternScore) -> InterestingRow {
    InterestingRow {
        pattern_id: stats.pattern_id.clone(),
        pattern: nway_pattern_display(&stats.key),
        kind: stats.kind.as_str().to_owned(),
        rrf_score: score.rrf_score,
        signal_profile: Signal::applicable(stats.kind)
            .iter()
            .map(|signal| signal.name().to_owned())
            .collect(),
        signals: score
            .signals
            .iter()
            .map(|(signal, raw, rank)| SignalExplain {
                signal: signal.name().to_owned(),
                status: if raw.is_some() { "present" } else { "missing" }.to_owned(),
                raw_value: raw.map(round6),
                rank: rank.map(|rank| rank as u64),
                rrf_term: round6(rrf_term(*rank, score.lambda)),
            })
            .collect(),
        examples: stats.examples,
        source_count: stats.source_count,
        text_count: stats.text_count,
        sample_source_ids: stats.sample_source_ids.clone(),
        sample_text_ids: stats.sample_text_ids.clone(),
        region_examples: stats.region_examples.clone(),
    }
}

/// Final ordering (spec §Tie-Breaking): rounded RRF desc, `source_count`
/// desc, `pattern_id` asc.
fn ranked_order(patterns: &[PatternStats], scores: &[PatternScore]) -> Vec<usize> {
    let mut order = (0..patterns.len()).collect::<Vec<_>>();
    order.sort_by(|left, right| {
        scores[*right]
            .rrf_score
            .partial_cmp(&scores[*left].rrf_score)
            .unwrap_or(Ordering::Equal)
            .then_with(|| patterns[*right].source_count.cmp(&patterns[*left].source_count))
            .then_with(|| patterns[*left].pattern_id.cmp(&patterns[*right].pattern_id))
    });
    order
}

/// Regions worth surfacing individually: disagreement regions passing the
/// filter whose owning patterns all fell below the emitted top set (or
/// which formed no pattern at all). Spec §Anomaly Channel.
fn anomaly_channel_in_memory(
    memberships: &BTreeMap<WarehouseRegionKey, BTreeSet<usize>>,
    region_flags: &BTreeMap<WarehouseRegionKey, WarehouseRegionFlags>,
    punctuation_only: &BTreeSet<WarehouseRegionKey>,
    filter: InterestingTextFilter,
    top_indices: &BTreeSet<usize>,
    k: usize,
) -> Vec<AnomalyRow> {
    let mut rows = Vec::new();
    for (region, flags) in region_flags {
        if flags.is_agreement {
            continue;
        }
        if !region_passes_filter(filter, flags, punctuation_only.contains(region)) {
            continue;
        }
        if memberships
            .get(region)
            .is_some_and(|owners| owners.iter().any(|owner| top_indices.contains(owner)))
        {
            continue;
        }
        let char_length = flags.char_end - flags.char_start;
        let coverage_term = if flags.has_coverage_mismatch {
            ANOMALY_W_COV
        } else {
            0.0
        };
        rows.push(AnomalyRow {
            source_id: region.source_id.clone(),
            text_id: region.text_id.clone(),
            region_index: region.region_index,
            char_start: flags.char_start,
            char_end: flags.char_end,
            has_coverage_mismatch: flags.has_coverage_mismatch,
            anomaly_score: round6(coverage_term + (1.0 + char_length as f64).log2()),
        });
    }
    rows.sort_by(|left, right| {
        right
            .anomaly_score
            .partial_cmp(&left.anomaly_score)
            .unwrap_or(Ordering::Equal)
            .then_with(|| left.source_id.cmp(&right.source_id))
            .then_with(|| left.text_id.cmp(&right.text_id))
            .then_with(|| left.region_index.cmp(&right.region_index))
    });
    rows.truncate(k);
    rows
}

fn feature_profile_name(profile: WarehouseFeatureProfile) -> &'static str {
    match profile {
        WarehouseFeatureProfile::Raw => "raw",
        WarehouseFeatureProfile::Core => "core",
        WarehouseFeatureProfile::Schema => "schema",
    }
}

fn score_version_block(rarity_basis: &str, profile: WarehouseFeatureProfile) -> ScoreVersionBlock {
    ScoreVersionBlock {
        score_version: SCORE_VERSION,
        pattern_id_version: PATTERN_ID_VERSION,
        rrf_k: RRF_K as u32,
        lambda_missing_policy: "rank-floor".to_owned(),
        anomaly_w_cov: ANOMALY_W_COV,
        signal_profile: ["coverage", "rarity", "impact", "span"]
            .iter()
            .map(|name| (*name).to_owned())
            .collect(),
        feature_profile: feature_profile_name(profile).to_owned(),
        rarity_basis: rarity_basis.to_owned(),
        granularity_profile: "none".to_owned(),
        cause_classification_profile: "absent".to_owned(),
        literal_context_policy: None,
        surprise: "absent".to_owned(),
    }
}

/// Ranks warehouse patterns by interestingness (spec MVP v1).
///
/// # Errors
///
/// Returns an error when the run directory is not a warehouse run, the run's
/// `schema_version` exceeds this reader's maximum, the run has fewer than two
/// analyzers, required tables cannot be read, `options.engine` is `Duckdb`
/// but no binary is available, or `options.explain` names an unknown
/// `pattern_id`.
pub fn summarize_warehouse_interesting(
    run_dir: &Path,
    options: WarehouseInterestingOptions,
) -> Result<InterestingSummary> {
    if options.feature_profile == WarehouseFeatureProfile::Schema {
        bail!("--feature-profile schema is not supported for interestingness ranking");
    }
    let (schema_version, run_id) = read_run_meta(run_dir)?;
    if schema_version > READER_MAX_SCHEMA_VERSION {
        bail!(
            "run schema_version {schema_version} exceeds this reader's supported maximum \
             {READER_MAX_SCHEMA_VERSION}"
        );
    }
    let analyzer_ids = read_distinct_column(run_dir, WarehouseTable::RunAnalyzers, 1)?;
    if analyzer_ids.len() < 2 {
        bail!(
            "interestingness ranking requires >= 2 analyzers; run has {}",
            analyzer_ids.len()
        );
    }
    let source_ids = read_distinct_column(run_dir, WarehouseTable::Sources, 1)?;
    if source_ids.is_empty() {
        return Ok(InterestingSummary {
            score_version: score_version_block("source", options.feature_profile),
            run_id,
            rows: Vec::new(),
            anomalies: Vec::new(),
        });
    }

    let rarity = rarity_config(run_dir, &source_ids)?;
    let collected = match options.engine {
        InterestingEngine::InMemory => collect_in_memory(run_dir, &options, &rarity)?,
        InterestingEngine::Duckdb | InterestingEngine::Auto => {
            match interesting_sql::collect_patterns_duckdb(run_dir, &options, &rarity, &analyzer_ids)? {
                Some(collected) => Collected {
                    patterns: collected.patterns,
                    anomaly_source: AnomalySource::Deferred {
                        feature_stage_files: collected.feature_stage_files,
                    },
                },
                None if options.engine == InterestingEngine::Duckdb => {
                    bail!(
                        "no duckdb binary found (set AB_DUCKDB_BIN or install duckdb); \
                         --engine in-memory is not viable for full-corpus warehouses"
                    );
                }
                None => collect_in_memory(run_dir, &options, &rarity)?,
            }
        }
    };

    let scores = score_patterns(&collected.patterns, rarity.total);
    let order = ranked_order(&collected.patterns, &scores);

    if let Some(explain_id) = &options.explain {
        if let AnomalySource::Deferred {
            feature_stage_files,
        } = &collected.anomaly_source
        {
            for path in feature_stage_files {
                let _ = std::fs::remove_file(path);
            }
        }
        let index = collected
            .patterns
            .iter()
            .position(|stats| stats.pattern_id == *explain_id)
            .with_context(|| format!("unknown pattern_id {explain_id}"))?;
        return Ok(InterestingSummary {
            score_version: score_version_block(rarity.basis, options.feature_profile),
            run_id,
            rows: vec![build_row(&collected.patterns[index], &scores[index])],
            anomalies: Vec::new(),
        });
    }

    let top = order
        .iter()
        .take(options.limit)
        .copied()
        .collect::<Vec<_>>();
    let top_set = top.iter().copied().collect::<BTreeSet<_>>();
    let rows = top
        .iter()
        .map(|index| build_row(&collected.patterns[*index], &scores[*index]))
        .collect();
    let anomalies = match &collected.anomaly_source {
        AnomalySource::InMemory {
            memberships,
            region_flags,
            punctuation_only,
        } => anomaly_channel_in_memory(
            memberships,
            region_flags,
            punctuation_only,
            options.filter,
            &top_set,
            options.anomalies,
        ),
        AnomalySource::Deferred {
            feature_stage_files,
        } => {
            let top_stats = top
                .iter()
                .map(|index| &collected.patterns[*index])
                .collect::<Vec<_>>();
            let anomalies = interesting_sql::anomalies_duckdb(
                run_dir,
                &options,
                &top_stats,
                feature_stage_files,
                &analyzer_ids,
            );
            for path in feature_stage_files {
                let _ = std::fs::remove_file(path);
            }
            anomalies?
        }
    };

    Ok(InterestingSummary {
        score_version: score_version_block(rarity.basis, options.feature_profile),
        run_id,
        rows,
        anomalies,
    })
}

/// Writes the summary as TSV: ranked rows, then an `# anomalies` section
/// when present.
///
/// # Errors
///
/// Returns an error when writing to `out` fails.
pub fn write_interesting_tsv(summary: &InterestingSummary, mut out: impl Write) -> Result<()> {
    writeln!(
        out,
        "rank\tkind\trrf_score\texamples\tsource_count\ttext_count\tpattern_id\tpattern"
    )?;
    for (index, row) in summary.rows.iter().enumerate() {
        writeln!(
            out,
            "{}\t{}\t{:.6}\t{}\t{}\t{}\t{}\t{}",
            index + 1,
            row.kind,
            row.rrf_score,
            row.examples,
            row.source_count,
            row.text_count,
            row.pattern_id,
            row.pattern
        )?;
    }
    if !summary.anomalies.is_empty() {
        writeln!(out, "# anomalies")?;
        writeln!(
            out,
            "rank\tsource_id\ttext_id\tregion_index\tchar_start\tchar_end\thas_coverage_mismatch\tanomaly_score"
        )?;
        for (index, row) in summary.anomalies.iter().enumerate() {
            writeln!(
                out,
                "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{:.6}",
                index + 1,
                row.source_id,
                row.text_id,
                row.region_index,
                row.char_start,
                row.char_end,
                row.has_coverage_mismatch,
                row.anomaly_score
            )?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::sync::Arc;

    use arrow_array::RecordBatch;
    use arrow_schema::{DataType, Field, Schema};
    use parquet::arrow::ArrowWriter;

    use super::*;
    use crate::warehouse::schema::{
        NwayFeatureDiffRow, NwayRegionAnalyzerRow, NwayRegionRow, RunAnalyzerRow, RunRow,
        SCHEMA_VERSION, SourceRow, WarehousePaths,
    };
    use crate::warehouse::writer::WarehouseWriter;
    use proptest::prelude::*;

    const RUN: &str = "run-a";

    fn run_row(schema_version: u32, source_count: u64, analyzer_count: u64) -> RunRow {
        RunRow {
            schema_version,
            run_id: RUN.to_owned(),
            created_at_utc: "2026-07-05T00:00:00Z".to_owned(),
            input_mode: "aat_dir".to_owned(),
            input_path: "scratch/aats".to_owned(),
            source_count,
            analyzer_count,
            error_count: 0,
        }
    }

    fn analyzer_row(analyzer_id: &str) -> RunAnalyzerRow {
        RunAnalyzerRow {
            run_id: RUN.to_owned(),
            analyzer_id: analyzer_id.to_owned(),
            analyzer_arg: analyzer_id.to_owned(),
            analyzer_family: analyzer_id.to_owned(),
        }
    }

    fn source_row(source_id: &str, text_id: &str) -> SourceRow {
        SourceRow {
            run_id: RUN.to_owned(),
            source_id: source_id.to_owned(),
            text_id: text_id.to_owned(),
            aat_path: format!("{source_id}.json"),
            source_bytes: 100,
            source_chars: 50,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn region_row(
        source_id: &str,
        text_id: &str,
        region_index: u64,
        char_start: u64,
        char_end: u64,
        coverage: bool,
        segmentation: bool,
        feature: bool,
    ) -> NwayRegionRow {
        NwayRegionRow {
            run_id: RUN.to_owned(),
            source_id: source_id.to_owned(),
            text_id: text_id.to_owned(),
            region_index,
            byte_start: char_start * 3,
            byte_end: char_end * 3,
            char_start,
            char_end,
            is_nonempty_whitespace: false,
            is_agreement: !(coverage || segmentation || feature),
            has_coverage_mismatch: coverage,
            has_segmentation_disagreement: segmentation,
            has_feature_disagreement: feature,
        }
    }

    fn region_analyzer_row(
        source_id: &str,
        text_id: &str,
        region_index: u64,
        analyzer_id: &str,
        surfaces: &[&str],
    ) -> NwayRegionAnalyzerRow {
        NwayRegionAnalyzerRow {
            run_id: RUN.to_owned(),
            source_id: source_id.to_owned(),
            text_id: text_id.to_owned(),
            region_index,
            analyzer_id: analyzer_id.to_owned(),
            covers_exactly: true,
            morpheme_start: 0,
            morpheme_end: surfaces.len() as u64,
            surfaces: surfaces.iter().map(|s| (*s).to_owned()).collect(),
        }
    }

    fn feature_diff_row(
        source_id: &str,
        text_id: &str,
        region_index: u64,
        feature_key: &str,
        feature_value: &str,
        analyzer_id: &str,
    ) -> NwayFeatureDiffRow {
        NwayFeatureDiffRow {
            run_id: RUN.to_owned(),
            source_id: source_id.to_owned(),
            text_id: text_id.to_owned(),
            region_index,
            feature_key: feature_key.to_owned(),
            scope_type: "whole_region".to_owned(),
            scope_position: None,
            scope_surface: None,
            feature_value: Some(feature_value.to_owned()),
            analyzer_id: analyzer_id.to_owned(),
        }
    }

    /// Two analyzers, two sources. Patterns under `--filter all`:
    /// - segmentation: 今日 split (src-a r0 + src-b r0) and a
    ///   punctuation-only split (src-a r3)
    /// - coverage: single-surface-group mismatch (src-a r1, chars 2..5)
    /// - feature: pos1 名詞/動詞 whole-region (src-a r2, chars 5..6)
    fn write_fixture(root: &Path) -> std::path::PathBuf {
        let paths = WarehousePaths::new(root, RUN);
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer.append_runs(&[run_row(SCHEMA_VERSION, 2, 2)]).unwrap();
        writer
            .append_run_analyzers(&[analyzer_row("vibrato"), analyzer_row("sudachi-a")])
            .unwrap();
        writer
            .append_sources(&[source_row("src-a", "txt-a"), source_row("src-b", "txt-b")])
            .unwrap();
        writer
            .append_nway_regions(&[
                region_row("src-a", "txt-a", 0, 0, 2, false, true, false),
                region_row("src-a", "txt-a", 1, 2, 5, true, false, false),
                region_row("src-a", "txt-a", 2, 5, 6, false, false, true),
                region_row("src-a", "txt-a", 3, 6, 8, false, true, false),
                region_row("src-b", "txt-b", 0, 0, 2, false, true, false),
            ])
            .unwrap();
        writer
            .append_nway_region_analyzers(&[
                region_analyzer_row("src-a", "txt-a", 0, "vibrato", &["今日"]),
                region_analyzer_row("src-a", "txt-a", 0, "sudachi-a", &["今", "日"]),
                region_analyzer_row("src-a", "txt-a", 1, "vibrato", &["ゆき"]),
                region_analyzer_row("src-a", "txt-a", 1, "sudachi-a", &["ゆき"]),
                region_analyzer_row("src-a", "txt-a", 2, "vibrato", &["走る"]),
                region_analyzer_row("src-a", "txt-a", 2, "sudachi-a", &["走る"]),
                region_analyzer_row("src-a", "txt-a", 3, "vibrato", &["、、"]),
                region_analyzer_row("src-a", "txt-a", 3, "sudachi-a", &["、", "、"]),
                region_analyzer_row("src-b", "txt-b", 0, "vibrato", &["今日"]),
                region_analyzer_row("src-b", "txt-b", 0, "sudachi-a", &["今", "日"]),
            ])
            .unwrap();
        writer
            .append_nway_feature_diffs(&[
                feature_diff_row("src-a", "txt-a", 2, "pos1", "名詞", "vibrato"),
                feature_diff_row("src-a", "txt-a", 2, "pos1", "動詞", "sudachi-a"),
            ])
            .unwrap();
        writer.finalize().unwrap();
        paths.final_dir
    }

    fn write_aozora_works(run_dir: &Path, rows: &[(&str, &str)]) {
        let schema = Arc::new(Schema::new(vec![
            Field::new("work_id", DataType::Utf8, false),
            Field::new("source_id", DataType::Utf8, false),
        ]));
        let batch = RecordBatch::try_new(
            schema.clone(),
            vec![
                Arc::new(arrow_array::StringArray::from(
                    rows.iter().map(|(work, _)| *work).collect::<Vec<_>>(),
                )),
                Arc::new(arrow_array::StringArray::from(
                    rows.iter().map(|(_, source)| *source).collect::<Vec<_>>(),
                )),
            ],
        )
        .unwrap();
        let file = File::create(run_dir.join("aozora_works.parquet")).unwrap();
        let mut writer = ArrowWriter::try_new(file, schema, None).unwrap();
        writer.write(&batch).unwrap();
        writer.close().unwrap();
    }

    #[test]
    fn collects_all_three_pattern_kinds() {
        let root = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(root.path());
        let summary =
            summarize_warehouse_interesting(&run_dir, WarehouseInterestingOptions::default())
                .unwrap();
        let kinds = summary
            .rows
            .iter()
            .map(|row| (row.kind.clone(), row.examples))
            .collect::<Vec<_>>();
        assert_eq!(summary.rows.len(), 4);
        assert!(kinds.contains(&("segmentation".to_owned(), 2)));
        assert!(kinds.contains(&("segmentation".to_owned(), 1)));
        assert!(kinds.contains(&("coverage".to_owned(), 1)));
        assert!(kinds.contains(&("feature".to_owned(), 1)));
        assert_eq!(summary.score_version.rarity_basis, "source");
        assert_eq!(summary.score_version.granularity_profile, "none");
        // Coverage pattern admitted with a single surface group.
        let coverage = summary.rows.iter().find(|row| row.kind == "coverage").unwrap();
        assert_eq!(coverage.region_examples.len(), 1);
        assert_eq!(coverage.region_examples[0].char_start, 2);
        assert_eq!(coverage.region_examples[0].char_end, 5);
    }

    #[test]
    fn lexical_only_filter_excludes_punctuation_only_patterns() {
        let root = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(root.path());
        let summary = summarize_warehouse_interesting(
            &run_dir,
            WarehouseInterestingOptions {
                filter: InterestingTextFilter::LexicalOnly,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!(summary.rows.len(), 3);
        assert!(summary.rows.iter().all(|row| !row.pattern.contains('、')));
        // With one pattern per kind, every signal ranks 1: RRF = 1/61.
        for row in &summary.rows {
            assert_eq!(row.rrf_score, 0.016393, "row {}", row.pattern_id);
        }
        // Tie on score: source_count desc puts the two-source segmentation
        // pattern first.
        assert_eq!(summary.rows[0].kind, "segmentation");
        assert_eq!(summary.rows[0].source_count, 2);
    }

    #[test]
    fn coverage_signal_counts_mismatch_regions() {
        let root = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(root.path());
        let summary =
            summarize_warehouse_interesting(&run_dir, WarehouseInterestingOptions::default())
                .unwrap();
        let coverage = summary.rows.iter().find(|row| row.kind == "coverage").unwrap();
        let signal = coverage
            .signals
            .iter()
            .find(|signal| signal.signal == "coverage")
            .unwrap();
        assert_eq!(signal.raw_value, Some(1.0)); // log2(1 + 1)
        assert_eq!(signal.status, "present");
        assert_eq!(signal.rank, Some(1));
    }

    #[test]
    fn work_map_switches_rarity_basis_and_dedups() {
        let root = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(root.path());
        write_aozora_works(&run_dir, &[("w1", "src-a"), ("w1", "src-b")]);
        let summary =
            summarize_warehouse_interesting(&run_dir, WarehouseInterestingOptions::default())
                .unwrap();
        assert_eq!(summary.score_version.rarity_basis, "work");
        // Both sources map to one work: every pattern has rarity_count 1 of
        // total 1, so every rarity raw is log2(2/2) = 0.
        for row in &summary.rows {
            let rarity = row.signals.iter().find(|s| s.signal == "rarity").unwrap();
            assert_eq!(rarity.raw_value, Some(0.0), "row {}", row.pattern_id);
        }
    }

    #[test]
    fn anomaly_channel_surfaces_regions_below_cutoff() {
        let root = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(root.path());
        let summary = summarize_warehouse_interesting(
            &run_dir,
            WarehouseInterestingOptions {
                limit: 1,
                filter: InterestingTextFilter::LexicalOnly,
                ..Default::default()
            },
        )
        .unwrap();
        // Top-1 is the two-source segmentation pattern; its regions are
        // excluded. Coverage region (chars 2..5) scores 5 + log2(4) = 7;
        // feature region (chars 5..6) scores log2(2) = 1. The punctuation
        // region is excluded by the filter.
        assert_eq!(summary.rows.len(), 1);
        let scored = summary
            .anomalies
            .iter()
            .map(|row| (row.region_index, row.anomaly_score))
            .collect::<Vec<_>>();
        assert_eq!(scored, vec![(1, 7.0), (2, 1.0)]);
    }

    #[test]
    fn explain_returns_single_row_and_rejects_unknown_ids() {
        let root = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(root.path());
        let ranked =
            summarize_warehouse_interesting(&run_dir, WarehouseInterestingOptions::default())
                .unwrap();
        let wanted = ranked.rows.iter().find(|row| row.kind == "feature").unwrap();

        let explained = summarize_warehouse_interesting(
            &run_dir,
            WarehouseInterestingOptions {
                explain: Some(wanted.pattern_id.clone()),
                limit: 0,
                anomalies: 0,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!(explained.rows.len(), 1);
        assert!(explained.anomalies.is_empty());
        let row = &explained.rows[0];
        assert_eq!(row.pattern_id, wanted.pattern_id);
        assert_eq!(
            row.signal_profile,
            vec!["coverage", "rarity", "impact", "span"]
        );
        let impact = row.signals.iter().find(|s| s.signal == "impact").unwrap();
        assert_eq!(impact.raw_value, Some(4.0)); // pos1

        let error = summarize_warehouse_interesting(
            &run_dir,
            WarehouseInterestingOptions {
                explain: Some("sha256:doesnotexist".to_owned()),
                ..Default::default()
            },
        )
        .unwrap_err();
        assert!(error.to_string().contains("unknown pattern_id"));
    }

    #[test]
    fn future_schema_version_is_rejected() {
        let root = tempfile::tempdir().unwrap();
        let paths = WarehousePaths::new(root.path(), RUN);
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer.append_runs(&[run_row(2, 0, 2)]).unwrap();
        writer
            .append_run_analyzers(&[analyzer_row("vibrato"), analyzer_row("sudachi-a")])
            .unwrap();
        writer.finalize().unwrap();
        let error = summarize_warehouse_interesting(
            &paths.final_dir,
            WarehouseInterestingOptions::default(),
        )
        .unwrap_err();
        assert!(
            error
                .to_string()
                .contains("schema_version 2 exceeds this reader's supported maximum 1"),
            "{error}"
        );
    }

    #[test]
    fn single_analyzer_run_is_rejected() {
        let root = tempfile::tempdir().unwrap();
        let paths = WarehousePaths::new(root.path(), RUN);
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer.append_runs(&[run_row(SCHEMA_VERSION, 0, 1)]).unwrap();
        writer.append_run_analyzers(&[analyzer_row("vibrato")]).unwrap();
        writer.finalize().unwrap();
        let error = summarize_warehouse_interesting(
            &paths.final_dir,
            WarehouseInterestingOptions::default(),
        )
        .unwrap_err();
        assert!(error.to_string().contains(">= 2 analyzers"), "{error}");
    }

    #[test]
    fn empty_run_produces_empty_summary() {
        let root = tempfile::tempdir().unwrap();
        let paths = WarehousePaths::new(root.path(), RUN);
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer.append_runs(&[run_row(SCHEMA_VERSION, 0, 2)]).unwrap();
        writer
            .append_run_analyzers(&[analyzer_row("vibrato"), analyzer_row("sudachi-a")])
            .unwrap();
        writer.finalize().unwrap();
        let summary = summarize_warehouse_interesting(
            &paths.final_dir,
            WarehouseInterestingOptions::default(),
        )
        .unwrap();
        assert!(summary.rows.is_empty());
        assert!(summary.anomalies.is_empty());
        assert_eq!(summary.score_version.rarity_basis, "source");
    }

    #[test]
    fn missing_run_dir_is_a_hard_error() {
        let root = tempfile::tempdir().unwrap();
        let error = summarize_warehouse_interesting(
            &root.path().join("nope"),
            WarehouseInterestingOptions::default(),
        )
        .unwrap_err();
        assert!(
            error.to_string().contains("not a warehouse run directory"),
            "{error}"
        );
    }

    #[test]
    fn json_output_is_byte_stable_across_invocations() {
        let root = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(root.path());
        let first = serde_json::to_string_pretty(
            &summarize_warehouse_interesting(&run_dir, WarehouseInterestingOptions::default())
                .unwrap(),
        )
        .unwrap();
        let second = serde_json::to_string_pretty(
            &summarize_warehouse_interesting(&run_dir, WarehouseInterestingOptions::default())
                .unwrap(),
        )
        .unwrap();
        assert_eq!(first, second);
        assert!(first.contains("\"score_version\": 1"));
        assert!(first.contains("\"lambda_missing_policy\": \"rank-floor\""));
    }

    #[test]
    fn tsv_writer_emits_rows_and_anomaly_section() {
        let root = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(root.path());
        let summary = summarize_warehouse_interesting(
            &run_dir,
            WarehouseInterestingOptions {
                limit: 1,
                filter: InterestingTextFilter::LexicalOnly,
                ..Default::default()
            },
        )
        .unwrap();
        let mut buffer = Vec::new();
        write_interesting_tsv(&summary, &mut buffer).unwrap();
        let text = String::from_utf8(buffer).unwrap();
        assert!(text.starts_with("rank\tkind\trrf_score"));
        assert!(text.contains("1\tsegmentation\t0.016393"));
        assert!(text.contains("# anomalies"));
        assert!(text.contains("7.000000"));
    }

    fn duckdb_available() -> bool {
        let binary = std::env::var("AB_DUCKDB_BIN")
            .unwrap_or_else(|_| std::env::var("DUCKDB").unwrap_or_else(|_| "duckdb".to_owned()));
        std::process::Command::new(binary)
            .arg("-version")
            .output()
            .is_ok()
    }

    #[test]
    fn coverage_pattern_has_display_string() {
        let root = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(root.path());
        let summary = summarize_warehouse_interesting(
            &run_dir,
            WarehouseInterestingOptions {
                engine: InterestingEngine::InMemory,
                ..Default::default()
            },
        )
        .unwrap();
        let coverage = summary.rows.iter().find(|row| row.kind == "coverage").unwrap();
        assert!(coverage.pattern.contains("ゆき"), "{}", coverage.pattern);
    }

    #[test]
    fn duckdb_engine_matches_in_memory_engine() {
        if !duckdb_available() {
            eprintln!("skipping: duckdb binary not available");
            return;
        }
        let root = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(root.path());
        // Work map exercises the rarity join in both engines.
        write_aozora_works(&run_dir, &[("w1", "src-a"), ("w1", "src-b")]);
        for options in [
            WarehouseInterestingOptions::default(),
            WarehouseInterestingOptions {
                limit: 1,
                filter: InterestingTextFilter::LexicalOnly,
                ..Default::default()
            },
            WarehouseInterestingOptions {
                feature_profile: WarehouseFeatureProfile::Core,
                anomalies: 3,
                ..Default::default()
            },
        ] {
            let in_memory = summarize_warehouse_interesting(
                &run_dir,
                WarehouseInterestingOptions {
                    engine: InterestingEngine::InMemory,
                    ..options.clone()
                },
            )
            .unwrap();
            let duckdb = summarize_warehouse_interesting(
                &run_dir,
                WarehouseInterestingOptions {
                    engine: InterestingEngine::Duckdb,
                    ..options
                },
            )
            .unwrap();
            assert_eq!(
                serde_json::to_string_pretty(&in_memory).unwrap(),
                serde_json::to_string_pretty(&duckdb).unwrap()
            );
        }
    }

    #[test]
    fn fuse_missing_signal_uses_lambda_floor() {
        let lambda = lambda_missing(2); // 1/63
        let fused = fuse(&[Some(1), None, Some(2)], 3, lambda);
        let expected = (1.0 / 61.0 + lambda + 1.0 / 62.0) / 3.0;
        assert!((fused - expected).abs() < 1e-12);
    }

    proptest! {
        /// Adding data for an applicable signal never decreases the fused
        /// score; removing it never increases it (spec missing-signal
        /// monotonicity). Holds because the rank floor guarantees
        /// 1/(k+rank) > lambda for every real rank <= N.
        #[test]
        fn adding_signal_data_never_decreases_score(
            n in 1usize..500,
            slot in 0usize..3,
            other_rank in 1usize..500,
        ) {
            let rank = other_rank.min(n);
            let lambda = lambda_missing(n);
            let mut with_missing = [Some(1), Some(2.min(n)), Some(3.min(n))];
            with_missing[slot] = None;
            let mut with_data = with_missing;
            with_data[slot] = Some(rank);
            let missing_score = fuse(&with_missing, 3, lambda);
            let data_score = fuse(&with_data, 3, lambda);
            prop_assert!(data_score >= missing_score);
            // The change is confined to the one term, bounded by
            // (1/(k+1) - lambda) / |S|.
            let bound = (1.0 / (RRF_K + 1.0) - lambda) / 3.0;
            prop_assert!(data_score - missing_score <= bound + 1e-12);
        }

        /// Fused scores stay within (0, 1/(k+1)] regardless of ranks and
        /// missing slots.
        #[test]
        fn fused_score_bounds(
            n in 1usize..500,
            ranks in proptest::collection::vec(proptest::option::of(1usize..500), 1..5),
        ) {
            let lambda = lambda_missing(n);
            let clamped = ranks
                .iter()
                .map(|rank| rank.map(|r| r.min(n)))
                .collect::<Vec<_>>();
            let fused = fuse(&clamped, clamped.len(), lambda);
            prop_assert!(fused > 0.0);
            prop_assert!(fused <= 1.0 / (RRF_K + 1.0) + 1e-12);
        }

        /// Better raw rank always contributes a strictly larger term.
        #[test]
        fn rrf_term_is_monotone_in_rank(a in 1usize..1000, b in 1usize..1000) {
            let term_a = rrf_term(Some(a), 0.0);
            let term_b = rrf_term(Some(b), 0.0);
            prop_assert_eq!(a < b, term_a > term_b);
        }

        /// percentile_90 returns an element of the sample and is monotone
        /// under appending a new maximum.
        #[test]
        fn percentile_90_is_sane(mut lengths in proptest::collection::vec(0u64..10_000, 1..50)) {
            let p90 = percentile_90(&lengths);
            prop_assert!(lengths.iter().any(|len| *len as f64 == p90));
            let max = *lengths.iter().max().unwrap();
            lengths.push(max + 1);
            prop_assert!(percentile_90(&lengths) >= p90);
        }
    }
}
