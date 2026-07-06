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
use super::pattern_id::{PATTERN_ID_VERSION, pattern_digest, pattern_id_from_digest};
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

/// Signal-rank pooling scope (spec §Calibration Plan step 3 A/B). Within-kind
/// is the shipped v1 default; global pools ranks per signal across kinds
/// (applicability unchanged: impact still fires only for feature patterns).
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum RankScope {
    WithinKind,
    Global,
}

impl RankScope {
    fn as_str(self) -> &'static str {
        match self {
            Self::WithinKind => "within-kind",
            Self::Global => "global",
        }
    }
}

/// Missing-signal λ policy (spec §Calibration Plan step 4, adapted per the
/// v1 rank-floor deviation): `rank-floor` (default) or `fixed:<v>` with
/// v ≥ 0. Fixed values break missing-signal monotonicity past rank
/// `1/v − k`; they exist for the calibration sweep, not for production use.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LambdaMissingPolicy {
    RankFloor,
    Fixed(f64),
}

impl std::str::FromStr for LambdaMissingPolicy {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        if input == "rank-floor" {
            return Ok(Self::RankFloor);
        }
        if let Some(value) = input.strip_prefix("fixed:") {
            let value: f64 = value
                .parse()
                .map_err(|_| format!("invalid fixed λ value {value:?}"))?;
            if !value.is_finite() || value < 0.0 {
                return Err(format!("fixed λ must be finite and >= 0, got {value}"));
            }
            return Ok(Self::Fixed(value));
        }
        Err(format!(
            "expected `rank-floor` or `fixed:<value>`, got {input:?}"
        ))
    }
}

impl std::fmt::Display for LambdaMissingPolicy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RankFloor => write!(f, "rank-floor"),
            Self::Fixed(value) => write!(f, "fixed:{value}"),
        }
    }
}

/// Baseline score mode (spec §Calibration Plan step 2): `rrf` (default, the
/// shipped v1 ranking) or one of two non-learned baselines the calibration
/// campaign compares it against.
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum ScoreMode {
    Rrf,
    Frequency,
    Random,
}

impl ScoreMode {
    fn as_str(self) -> &'static str {
        match self {
            Self::Rrf => "rrf",
            Self::Frequency => "frequency",
            Self::Random => "random",
        }
    }
}

/// splitmix64 (Vigna, public domain): stable across Rust/platform versions,
/// which `rand::StdRng` explicitly is not. Used for the random baseline and
/// the labeling-TSV blind shuffle.
pub(crate) fn splitmix64(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9E37_79B9_7F4A_7C15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    z ^ (z >> 31)
}

/// Ordering for the non-RRF baselines (spec §Calibration Plan step 2).
/// Frequency: `examples` desc, `source_count` desc, `pattern_id` asc.
/// Random: Fisher–Yates over indices pre-sorted by `pattern_id`, keyed by
/// the mandatory seed (spec §Tie-Breaking: randomness requires a seed).
fn baseline_order(
    patterns: &[PatternStats],
    mode: ScoreMode,
    sample_seed: Option<u64>,
) -> Result<Vec<usize>> {
    match mode {
        ScoreMode::Rrf => bail!("baseline_order is not for rrf mode"),
        ScoreMode::Frequency => {
            let mut order = (0..patterns.len()).collect::<Vec<_>>();
            order.sort_by(|left, right| {
                patterns[*right]
                    .examples
                    .cmp(&patterns[*left].examples)
                    .then_with(|| patterns[*right].source_count.cmp(&patterns[*left].source_count))
                    .then_with(|| patterns[*left].pattern_id.cmp(&patterns[*right].pattern_id))
            });
            Ok(order)
        }
        ScoreMode::Random => {
            let seed = sample_seed
                .context("--score-mode random requires --sample-seed (determinism contract)")?;
            let mut order = (0..patterns.len()).collect::<Vec<_>>();
            order.sort_by(|left, right| {
                patterns[*left].pattern_id.cmp(&patterns[*right].pattern_id)
            });
            let mut state = seed;
            for i in (1..order.len()).rev() {
                let j = (splitmix64(&mut state) % (i as u64 + 1)) as usize;
                order.swap(i, j);
            }
            Ok(order)
        }
    }
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
    pub rank_scope: RankScope,
    pub lambda_policy: LambdaMissingPolicy,
    pub anomaly_w_cov: f64,
    pub score_mode: ScoreMode,
    pub sample_seed: Option<u64>,
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
            feature_profile: WarehouseFeatureProfile::Core,
            rank_scope: RankScope::WithinKind,
            lambda_policy: LambdaMissingPolicy::RankFloor,
            anomaly_w_cov: ANOMALY_W_COV,
            score_mode: ScoreMode::Rrf,
            sample_seed: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, serde::Deserialize)]
pub struct InterestingSummary {
    pub score_version: ScoreVersionBlock,
    pub run_id: String,
    pub rows: Vec<InterestingRow>,
    pub anomalies: Vec<AnomalyRow>,
}

/// Every knob affecting cross-run comparability. Two runs are comparable
/// only when all fields match (spec §Score Versioning).
///
/// `rank_scope`, `score_mode`, and `sample_seed` were added after v1
/// shipped (the calibration knobs). Pre-calibration artifacts predate these
/// fields and omit them entirely; their `#[serde(default = ...)]`
/// fallbacks encode what those artifacts actually were — within-kind
/// pooling, RRF scoring, no sampling seed — so such artifacts deserialize
/// honestly instead of failing to parse. Serialization is unchanged: these
/// fields are always written for artifacts produced going forward.
#[derive(Debug, Clone, PartialEq, Serialize, serde::Deserialize)]
pub struct ScoreVersionBlock {
    pub score_version: u32,
    pub pattern_id_version: u32,
    pub rrf_k: u32,
    /// `λ_missing` is not a fixed constant: a fixed value breaks the
    /// missing-signal monotonicity invariant for ranks past `1/λ - k`. The
    /// rank-floor policy sets `λ = 1/(k + N_kind + 1)` — "just below the
    /// worst-ranked observed pattern of the kind" — which preserves it.
    pub lambda_missing_policy: String,
    /// Signal-rank pooling scope: `"within-kind"` (v1 default) or `"global"`.
    /// Absent in pre-calibration artifacts, which were always within-kind.
    #[serde(default = "default_rank_scope")]
    pub rank_scope: String,
    /// `"rrf"` (v1 default) or one of the calibration baselines
    /// (`"frequency"`, `"random"`). Absent in pre-calibration artifacts,
    /// which were always RRF.
    #[serde(default = "default_score_mode")]
    pub score_mode: String,
    /// Fisher-Yates seed for `score_mode: "random"`; always `None` (JSON
    /// `null`) otherwise (spec §Score Versioning: an ignored seed would
    /// misdescribe the artifact). Absent in pre-calibration artifacts,
    /// which predate `score_mode: "random"` and so never had a seed.
    #[serde(default)]
    pub sample_seed: Option<u64>,
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

/// Pre-calibration artifacts' actual `rank_scope`: always within-kind.
fn default_rank_scope() -> String {
    RankScope::WithinKind.as_str().to_owned()
}

/// Pre-calibration artifacts' actual `score_mode`: always RRF.
fn default_score_mode() -> String {
    ScoreMode::Rrf.as_str().to_owned()
}

#[derive(Debug, Clone, PartialEq, Serialize, serde::Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Serialize, serde::Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Serialize, serde::Deserialize)]
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
/// DuckDB `quantile_disc(x, 0.9)` so both engines agree exactly. Test
/// oracle for [`percentile_90_histogram`], which production code uses.
#[cfg(test)]
fn percentile_90(lengths: &[u64]) -> f64 {
    let mut sorted = lengths.to_vec();
    sorted.sort_unstable();
    let n = sorted.len();
    let rank = (0.9 * n as f64).ceil() as usize;
    sorted[rank.max(1) - 1] as f64
}

/// Nearest-rank p90 over a sorted `(length, count)` histogram — same
/// result as [`percentile_90`] on the expanded multiset, without storing
/// it.
fn percentile_90_histogram(histogram: &[(u32, u32)]) -> f64 {
    let total: u64 = histogram.iter().map(|(_, count)| u64::from(*count)).sum();
    let rank = (0.9 * total as f64).ceil().max(1.0) as u64;
    let mut cumulative = 0u64;
    for (length, count) in histogram {
        cumulative += u64::from(*count);
        if cumulative >= rank {
            return f64::from(*length);
        }
    }
    0.0
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

/// String interner for source/text/rarity ids: full-corpus accumulation
/// holds tens of millions of patterns, so per-pattern sets store u32
/// symbols instead of cloned strings (a 55GB-RSS lesson).
#[derive(Default)]
struct Interner {
    ids: std::collections::HashMap<String, u32>,
    values: Vec<String>,
}

impl Interner {
    fn intern(&mut self, value: &str) -> u32 {
        if let Some(id) = self.ids.get(value) {
            return *id;
        }
        let id = self.values.len() as u32;
        self.ids.insert(value.to_owned(), id);
        self.values.push(value.to_owned());
        id
    }

    fn resolve(&self, id: u32) -> &str {
        &self.values[id as usize]
    }
}

/// Sorted-unique u32 set. `BTreeSet<u32>` allocates a ~150-byte node
/// even for a singleton; at tens of millions of patterns those nodes were
/// most of a 55GB RSS. Sorted Vec inserts are O(n) shifts only for new
/// distinct values, bounded by real cardinalities (<= source count).
#[derive(Debug, Default)]
struct SortedSet(Vec<u32>);

impl SortedSet {
    fn insert(&mut self, value: u32) {
        if let Err(position) = self.0.binary_search(&value) {
            self.0.insert(position, value);
        }
    }

    fn len(&self) -> usize {
        self.0.len()
    }
}

#[derive(Debug, Default)]
struct PatternAccum {
    examples: usize,
    source_ids: SortedSet,
    text_ids: SortedSet,
    rarity_keys: SortedSet,
    coverage_region_count: usize,
    /// (length, count) pairs, sorted by length; p90 is nearest-rank.
    span_lengths: Vec<(u32, u32)>,
    region_examples: Vec<(u32, u32, u64, u64, u64)>,
}

impl PatternAccum {
    fn push_span(&mut self, length: u32) {
        match self.span_lengths.binary_search_by_key(&length, |(len, _)| *len) {
            Ok(position) => self.span_lengths[position].1 += 1,
            Err(position) => self.span_lengths.insert(position, (length, 1)),
        }
    }
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
    /// Patterns indexed by raw content digest — storing the key again as a
    /// map key doubles key memory at full-corpus scale.
    index_of: std::collections::HashMap<[u8; 32], usize>,
    keys: Vec<(NwayPatternKey, PatternKind, [u8; 32])>,
    accums: Vec<PatternAccum>,
    interner: Interner,
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
        let digest = pattern_digest(&key);
        let next_index = self.accums.len();
        let index = *self.index_of.entry(digest).or_insert(next_index);
        if index == next_index {
            self.keys.push((key, kind, digest));
            self.accums.push(PatternAccum::default());
        }
        let source_id = self.interner.intern(occurrence.source_id);
        let text_id = self.interner.intern(occurrence.text_id);
        let rarity_key = self.interner.intern(occurrence.rarity_key);
        let accum = &mut self.accums[index];
        accum.examples += 1;
        accum.source_ids.insert(source_id);
        accum.text_ids.insert(text_id);
        accum.rarity_keys.insert(rarity_key);
        if occurrence.has_coverage_mismatch {
            accum.coverage_region_count += 1;
        }
        let length = u32::try_from(occurrence.char_end - occurrence.char_start).unwrap_or(u32::MAX);
        accum.push_span(length);
        if accum.region_examples.len() < max_region_examples {
            accum.region_examples.push((
                source_id,
                text_id,
                occurrence.region_index,
                occurrence.char_start,
                occurrence.char_end,
            ));
        }
        index
    }

    pub(super) fn finalize(self) -> Vec<PatternStats> {
        let interner = self.interner;
        let sorted_samples = |ids: &SortedSet| {
            let mut values = ids
                .0
                .iter()
                .map(|id| interner.resolve(*id))
                .collect::<Vec<_>>();
            values.sort_unstable();
            values
                .into_iter()
                .take(MAX_SAMPLE_IDS)
                .map(str::to_owned)
                .collect::<Vec<_>>()
        };
        self.keys
            .into_iter()
            .zip(self.accums)
            .map(|((key, kind, digest), accum)| PatternStats {
                pattern_id: pattern_id_from_digest(&digest),
                key,
                kind,
                examples: accum.examples,
                source_count: accum.source_ids.len(),
                text_count: accum.text_ids.len(),
                sample_source_ids: sorted_samples(&accum.source_ids),
                sample_text_ids: sorted_samples(&accum.text_ids),
                rarity_count: accum.rarity_keys.len(),
                coverage_region_count: accum.coverage_region_count,
                span_p90: percentile_90_histogram(&accum.span_lengths),
                region_examples: accum
                    .region_examples
                    .into_iter()
                    .map(
                        |(source_id, text_id, region_index, char_start, char_end)| {
                            RegionExampleOut {
                                source_id: interner.resolve(source_id).to_owned(),
                                text_id: interner.resolve(text_id).to_owned(),
                                region_index,
                                char_start,
                                char_end,
                            }
                        },
                    )
                    .collect(),
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
    /// DuckDB engine: anomalies are computed by follow-up queries once
    /// the top pattern set is known.
    Deferred,
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

/// Reads `(analyzer_arg, analyzer_family)` for every `run_analyzers` row
/// (columns 2 and 3; see `RunAnalyzerRow` field order in
/// `ab-warehouse::schema`), the input to [`granularity_profile_token`].
fn read_analyzer_arg_family(run_dir: &Path) -> Result<Vec<(String, String)>> {
    let mut pairs = Vec::new();
    for batch in read_warehouse_table(run_dir, WarehouseTable::RunAnalyzers)? {
        let args = string_column(&batch, 2)?;
        let families = string_column(&batch, 3)?;
        for row in 0..batch.num_rows() {
            pairs.push((args.value(row).to_owned(), families.value(row).to_owned()));
        }
    }
    Ok(pairs)
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
    if map.is_empty() {
        // A present-but-empty projection would flip rarity_basis to
        // "work" with total 0 and degenerate the IDF; the importer
        // refuses to create one, and the reader refuses to honor one.
        return Ok(None);
    }
    Ok(Some(map))
}

fn rarity_config(run_dir: &Path, source_ids: &BTreeSet<String>) -> Result<RarityConfig> {
    let work_by_source = read_optional_work_map(run_dir)?;
    let (basis, total) = match &work_by_source {
        Some(map) => {
            let mut works = BTreeSet::new();
            let mut unmapped = 0usize;
            for source_id in source_ids {
                match map.get(source_id) {
                    Some(work_id) => {
                        works.insert(work_id.as_str());
                    }
                    // Unmapped sources contribute per-source rarity keys
                    // (see InMemoryAccumulators::record and the SQL
                    // coalesce), so they belong in the denominator.
                    None => unmapped += 1,
                }
            }
            ("work", works.len() + unmapped)
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

/// Ranks patterns per signal (raw desc, `pattern_id` asc — the
/// deterministic tie-break) and fuses. Returns scores aligned with
/// `patterns`. `rank_scope` selects whether ranking pools are formed
/// within each kind (v1 default) or globally across all patterns.
fn score_patterns(
    patterns: &[PatternStats],
    rarity_total: usize,
    rank_scope: RankScope,
    lambda_policy: LambdaMissingPolicy,
) -> Vec<PatternScore> {
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

    let lambda_for = |pool_size: usize| match lambda_policy {
        LambdaMissingPolicy::RankFloor => lambda_missing(pool_size),
        LambdaMissingPolicy::Fixed(value) => value,
    };

    match rank_scope {
        RankScope::WithinKind => {
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
                let lambda = lambda_for(kind_indices.len());
                for index in &kind_indices {
                    scores[*index].lambda = lambda;
                }
                for (signal_slot, _signal) in Signal::applicable(kind).iter().enumerate() {
                    rank_signal_pool(patterns, &mut scores, &kind_indices, |index| {
                        Some(signal_slot).filter(|_| patterns[index].kind == kind)
                    });
                }
                for index in kind_indices {
                    fuse_into(&mut scores, index, lambda);
                }
            }
        }
        RankScope::Global => {
            let all_indices = (0..patterns.len()).collect::<Vec<_>>();
            if all_indices.is_empty() {
                return scores;
            }
            let lambda = lambda_for(all_indices.len());
            for index in &all_indices {
                scores[*index].lambda = lambda;
            }
            for signal in [Signal::Coverage, Signal::Rarity, Signal::Impact, Signal::Span] {
                rank_signal_pool(patterns, &mut scores, &all_indices, |index| {
                    Signal::applicable(patterns[index].kind)
                        .iter()
                        .position(|candidate| *candidate == signal)
                });
            }
            for index in all_indices {
                fuse_into(&mut scores, index, lambda);
            }
        }
    }
    scores
}

/// Ranks one signal's present raws (desc, `pattern_id` asc tie-break) over
/// `pool`, writing 1-based ranks into each member's slot. `slot_of` maps a
/// pattern index to its slot for this signal (`None` = signal not
/// applicable to that pattern's kind — skipped, stays missing).
fn rank_signal_pool(
    patterns: &[PatternStats],
    scores: &mut [PatternScore],
    pool: &[usize],
    slot_of: impl Fn(usize) -> Option<usize>,
) {
    let mut present = pool
        .iter()
        .filter_map(|index| {
            let slot = slot_of(*index)?;
            scores[*index].signals[slot].1.map(|raw| (*index, slot, raw))
        })
        .collect::<Vec<_>>();
    present.sort_by(|left, right| {
        right
            .2
            .partial_cmp(&left.2)
            .unwrap_or(Ordering::Equal)
            .then_with(|| patterns[left.0].pattern_id.cmp(&patterns[right.0].pattern_id))
    });
    for (rank_zero, (index, slot, _)) in present.iter().enumerate() {
        scores[*index].signals[*slot].2 = Some(rank_zero + 1);
    }
}

fn fuse_into(scores: &mut [PatternScore], index: usize, lambda: f64) {
    let ranks = scores[index]
        .signals
        .iter()
        .map(|(_, _, rank)| *rank)
        .collect::<Vec<_>>();
    scores[index].rrf_score = round6(fuse(&ranks, ranks.len(), lambda));
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
    w_cov: f64,
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
            w_cov
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

/// Classifies a single `run_analyzers` row into a NINJAL segmentation
/// granularity class: `suw` (短単位 / Short Unit Word), `muw` (中単位 /
/// Middle Unit Word), or `luw` (長単位 / Long Unit Word).
///
/// Sudachi mode A (`sudachi-a`) is approximately UniDic short-unit
/// granularity, mode B is middle-unit, and mode C is long-unit. Every other
/// analyzer family (`vibrato` — any dictionary, including kindai/qkana;
/// `vaporetto`; `test`) is a UniDic-短単位 lineage, hence `suw`.
fn granularity_class(analyzer_arg: &str, analyzer_family: &str) -> &'static str {
    if analyzer_family == "sudachi" {
        match analyzer_arg {
            "sudachi-b" => "muw",
            "sudachi-c" => "luw",
            _ => "suw",
        }
    } else {
        "suw"
    }
}

/// Ranks a granularity class into the canonical composition order
/// `suw < muw < luw` (NOT alphabetical — `luw` sorts last despite starting
/// with `l`).
fn granularity_class_rank(class: &str) -> u8 {
    match class {
        "suw" => 0,
        "muw" => 1,
        "luw" => 2,
        _ => 3,
    }
}

/// Derives the run's `granularity_profile` — which segmentation granularity
/// classes this run's analyzers compared — from its `run_analyzers` rows
/// (spec §Granularity Harmonization). Derived at summarize time from
/// `run_analyzers.parquet` metadata rather than stored as a new warehouse
/// column: a single source of truth, no schema change within v1, and
/// retroactively correct for every existing warehouse.
///
/// Each row is classified by [`granularity_class`] into `suw`/`muw`/`luw`;
/// the profile is the deduped set of classes present, sorted in canonical
/// order (`suw < muw < luw`, see [`granularity_class_rank`]) and joined with
/// `"+"` — e.g. `"suw"`, `"suw+luw"`, `"suw+muw+luw"`, or the hypothetical
/// `"muw+luw"`.
///
/// A single-class profile (`"suw"`) means no granularity-policy noise is
/// possible: every analyzer in the run compares at the same segmentation
/// granularity. A multi-class profile means granularity-policy disagreements
/// (e.g. Sudachi mode A vs. mode C splitting differently) are present in the
/// run by design, not by accident. Cross-run comparability on this field is
/// exact string match. Extending the analyzer roster with a non-short-unit
/// analyzer requires extending [`granularity_class`]'s classification, same
/// as before.
fn granularity_profile_token(analyzer_arg_family: &[(String, String)]) -> String {
    let classes: BTreeSet<&'static str> = analyzer_arg_family
        .iter()
        .map(|(arg, family)| granularity_class(arg, family))
        .collect();
    let mut ordered: Vec<&'static str> = classes.into_iter().collect();
    ordered.sort_by_key(|class| granularity_class_rank(class));
    ordered.join("+")
}

fn score_version_block(
    rarity_basis: &str,
    profile: WarehouseFeatureProfile,
    granularity_profile: &str,
    options: &WarehouseInterestingOptions,
) -> ScoreVersionBlock {
    ScoreVersionBlock {
        score_version: SCORE_VERSION,
        pattern_id_version: PATTERN_ID_VERSION,
        rrf_k: RRF_K as u32,
        lambda_missing_policy: options.lambda_policy.to_string(),
        rank_scope: options.rank_scope.as_str().to_owned(),
        score_mode: options.score_mode.as_str().to_owned(),
        sample_seed: options.sample_seed,
        anomaly_w_cov: options.anomaly_w_cov,
        signal_profile: ["coverage", "rarity", "impact", "span"]
            .iter()
            .map(|name| (*name).to_owned())
            .collect(),
        feature_profile: feature_profile_name(profile).to_owned(),
        rarity_basis: rarity_basis.to_owned(),
        granularity_profile: granularity_profile.to_owned(),
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
    if options.score_mode != ScoreMode::Random && options.sample_seed.is_some() {
        bail!("--sample-seed is only meaningful with --score-mode random");
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
    // Derived once and reused for every emitted score_version block (both
    // engines, the empty-run early return, and --explain) so it is always
    // the honest value, never a hardcode.
    let granularity_profile = granularity_profile_token(&read_analyzer_arg_family(run_dir)?);
    let source_ids = read_distinct_column(run_dir, WarehouseTable::Sources, 1)?;
    if source_ids.is_empty() {
        return Ok(InterestingSummary {
            score_version: score_version_block(
                "source",
                options.feature_profile,
                &granularity_profile,
                &options,
            ),
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
                Some(patterns) => Collected {
                    patterns,
                    anomaly_source: AnomalySource::Deferred,
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

    let scores = score_patterns(
        &collected.patterns,
        rarity.total,
        options.rank_scope,
        options.lambda_policy,
    );
    let order = match options.score_mode {
        ScoreMode::Rrf => ranked_order(&collected.patterns, &scores),
        mode => baseline_order(&collected.patterns, mode, options.sample_seed)?,
    };

    if let Some(explain_id) = &options.explain {
        let index = collected
            .patterns
            .iter()
            .position(|stats| stats.pattern_id == *explain_id)
            .with_context(|| format!("unknown pattern_id {explain_id}"))?;
        return Ok(InterestingSummary {
            score_version: score_version_block(
                rarity.basis,
                options.feature_profile,
                &granularity_profile,
                &options,
            ),
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
            options.anomaly_w_cov,
        ),
        AnomalySource::Deferred => {
            let top_stats = top
                .iter()
                .map(|index| &collected.patterns[*index])
                .collect::<Vec<_>>();
            interesting_sql::anomalies_duckdb(run_dir, &options, &top_stats, &analyzer_ids)?
        }
    };

    Ok(InterestingSummary {
        score_version: score_version_block(
            rarity.basis,
            options.feature_profile,
            &granularity_profile,
            &options,
        ),
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

    /// Unlike [`analyzer_row`], sets `analyzer_family` to the real
    /// `"sudachi"` family value (production `analyzer_row` conflates id,
    /// arg, and family, which never happens for real Sudachi rows) so
    /// fixtures can exercise the granularity-profile derivation honestly.
    fn sudachi_analyzer_row(mode_arg: &str) -> RunAnalyzerRow {
        RunAnalyzerRow {
            run_id: RUN.to_owned(),
            analyzer_id: mode_arg.to_owned(),
            analyzer_arg: mode_arg.to_owned(),
            analyzer_family: "sudachi".to_owned(),
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

    /// Minimal `PatternStats` fixture for `score_patterns`-level tests.
    /// Fields not passed in are neutral (one example/source/text, empty
    /// sample vecs and region examples, no SQL signature) since the
    /// scoring tests only need to control kind, rarity, coverage, span,
    /// and (for feature patterns) the impact-driving `feature_key`.
    fn calib_stats(
        kind: PatternKind,
        pattern_id: &str,
        rarity_count: usize,
        coverage_region_count: usize,
        span_p90: f64,
        feature_key: Option<&str>,
    ) -> PatternStats {
        PatternStats {
            key: NwayPatternKey {
                kind: kind.as_str().to_owned(),
                segmentation_groups: Vec::new(),
                feature_key: feature_key.map(str::to_owned),
                feature_scope: None,
                feature_values: Vec::new(),
            },
            pattern_id: pattern_id.to_owned(),
            kind,
            examples: 1,
            source_count: 1,
            text_count: 1,
            sample_source_ids: Vec::new(),
            sample_text_ids: Vec::new(),
            rarity_count,
            coverage_region_count,
            span_p90,
            region_examples: Vec::new(),
            sql_signature: None,
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

    /// Like `write_fixture` but with Aozora-style source ids, for
    /// exercising the real `import-aozora-metadata` producer end-to-end.
    fn write_aozora_id_fixture(root: &Path) -> std::path::PathBuf {
        const SOURCES: [&str; 3] = [
            "000001_10-aaaaaaaaaaaa",
            "000001_10-bbbbbbbbbbbb",
            "000002_20-cccccccccccc",
        ];
        let paths = WarehousePaths::new(root, RUN);
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer.append_runs(&[run_row(SCHEMA_VERSION, 3, 2)]).unwrap();
        writer
            .append_run_analyzers(&[analyzer_row("vibrato"), analyzer_row("sudachi-a")])
            .unwrap();
        writer
            .append_sources(&SOURCES.map(|source_id| source_row(source_id, source_id)))
            .unwrap();
        writer
            .append_nway_regions(&SOURCES.map(|source_id| {
                region_row(source_id, source_id, 0, 0, 2, false, true, false)
            }))
            .unwrap();
        let mut region_analyzers = Vec::new();
        for source_id in SOURCES {
            region_analyzers.push(region_analyzer_row(
                source_id, source_id, 0, "vibrato", &["今日"],
            ));
            region_analyzers.push(region_analyzer_row(
                source_id, source_id, 0, "sudachi-a", &["今", "日"],
            ));
        }
        writer
            .append_nway_region_analyzers(&region_analyzers)
            .unwrap();
        writer.finalize().unwrap();
        paths.final_dir
    }

    /// Minimal valid ABC export record for the importer's consumed fields.
    fn write_abc_export_record(export_dir: &Path, work_id: &str) {
        let works = export_dir.join("works");
        std::fs::create_dir_all(&works).unwrap();
        let record = serde_json::json!({
            "metadata_record_schema_id":
                "https://w3id.org/abc/schemas/metadata-record.schema.json",
            "metadata_record_schema_hash":
                crate::import_aozora::ABC_METADATA_RECORD_SCHEMA_HASH,
            "work": {
                "work_id": work_id,
                "title": "題名",
                "first_published": null,
                "orthographic_style": "新字新仮名",
                "source_editions": []
            },
            "contributors": [
                {"person_id": "000035", "person_record_hash": "sha256:0",
                 "relation_to_work": "著者"}
            ]
        });
        std::fs::write(
            works.join(format!("{work_id}.json")),
            serde_json::to_vec(&record).unwrap(),
        )
        .unwrap();
    }

    #[test]
    fn imported_sidecar_flips_rarity_basis_to_work() {
        let root = tempfile::tempdir().unwrap();
        let run_dir = write_aozora_id_fixture(root.path());
        let export = root.path().join("export");
        write_abc_export_record(&export, "000010");
        write_abc_export_record(&export, "000020");

        crate::run_import_aozora_metadata(&run_dir, &export, false).unwrap();
        let summary =
            summarize_warehouse_interesting(&run_dir, WarehouseInterestingOptions::default())
                .unwrap();

        assert_eq!(summary.score_version.rarity_basis, "work");
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
        // write_fixture's analyzer rows carry no true `analyzer_family ==
        // "sudachi"` row (the naive `analyzer_row` helper sets family to
        // the id, e.g. "sudachi-a", not "sudachi") so every row falls into
        // the default (non-Sudachi) `suw` class.
        assert_eq!(summary.score_version.granularity_profile, "suw");
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
    fn empty_work_map_is_treated_as_absent() {
        let root = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(root.path());
        write_aozora_works(&run_dir, &[]);

        let summary =
            summarize_warehouse_interesting(&run_dir, WarehouseInterestingOptions::default())
                .unwrap();

        assert_eq!(summary.score_version.rarity_basis, "source");
    }

    // `rarity_config` is a private fn of the parent module; this tests
    // submodule shares that module (`use super::*`), so it is callable
    // here with no visibility change.
    #[test]
    fn rarity_denominator_counts_unmapped_sources() {
        let root = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(root.path());
        // src-a mapped to w1; src-b left unmapped (falls back to a
        // per-source rarity key, so it must count in the denominator).
        write_aozora_works(&run_dir, &[("w1", "src-a")]);
        let source_ids: BTreeSet<String> =
            ["src-a", "src-b"].iter().map(|s| (*s).to_owned()).collect();

        let rarity = rarity_config(&run_dir, &source_ids).unwrap();

        assert_eq!(rarity.basis, "work");
        assert_eq!(rarity.total, 2);
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
        // The --explain path must carry the derived profile too, never the
        // old hardcode.
        assert_eq!(explained.score_version.granularity_profile, "suw");
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
        // The empty-run early return must emit the derived profile too,
        // never the old hardcode.
        assert_eq!(summary.score_version.granularity_profile, "suw");
    }

    #[test]
    fn empty_run_reports_suw_profile_for_sudachi_mode_a() {
        let root = tempfile::tempdir().unwrap();
        let paths = WarehousePaths::new(root.path(), RUN);
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer.append_runs(&[run_row(SCHEMA_VERSION, 0, 2)]).unwrap();
        writer
            .append_run_analyzers(&[sudachi_analyzer_row("sudachi-a"), analyzer_row("vibrato")])
            .unwrap();
        writer.finalize().unwrap();
        let summary = summarize_warehouse_interesting(
            &paths.final_dir,
            WarehouseInterestingOptions::default(),
        )
        .unwrap();
        assert_eq!(summary.score_version.granularity_profile, "suw");
    }

    #[test]
    fn empty_run_reports_suw_luw_profile_for_mixed_sudachi_modes() {
        let root = tempfile::tempdir().unwrap();
        let paths = WarehousePaths::new(root.path(), RUN);
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer.append_runs(&[run_row(SCHEMA_VERSION, 0, 2)]).unwrap();
        writer
            .append_run_analyzers(&[
                sudachi_analyzer_row("sudachi-a"),
                sudachi_analyzer_row("sudachi-c"),
            ])
            .unwrap();
        writer.finalize().unwrap();
        let summary = summarize_warehouse_interesting(
            &paths.final_dir,
            WarehouseInterestingOptions::default(),
        )
        .unwrap();
        assert_eq!(summary.score_version.granularity_profile, "suw+luw");
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
                feature_profile: WarehouseFeatureProfile::Raw,
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

    /// (analyzer_arg, analyzer_family) pair builder for
    /// [`granularity_profile_token`] tests.
    fn af(arg: &str, family: &str) -> (String, String) {
        (arg.to_owned(), family.to_owned())
    }

    #[test]
    fn granularity_profile_empty_input_is_empty_string() {
        // Never reached in production (>= 2 analyzers is enforced before
        // this is called) but the honest answer to "which classes did zero
        // rows compare?" is none, not a default.
        assert_eq!(granularity_profile_token(&[]), "");
    }

    #[test]
    fn granularity_profile_non_sudachi_rows_are_suw() {
        assert_eq!(
            granularity_profile_token(&[af("vibrato", "vibrato"), af("vaporetto", "vaporetto")]),
            "suw"
        );
    }

    #[test]
    fn granularity_profile_sudachi_mode_a_only_is_suw() {
        assert_eq!(granularity_profile_token(&[af("sudachi-a", "sudachi")]), "suw");
        // A non-sudachi analyzer alongside mode A adds no additional class.
        assert_eq!(
            granularity_profile_token(&[af("sudachi-a", "sudachi"), af("vibrato", "vibrato")]),
            "suw"
        );
    }

    #[test]
    fn granularity_profile_sudachi_mode_b_is_muw() {
        assert_eq!(granularity_profile_token(&[af("sudachi-b", "sudachi")]), "muw");
    }

    #[test]
    fn granularity_profile_sudachi_mode_c_is_luw() {
        assert_eq!(granularity_profile_token(&[af("sudachi-c", "sudachi")]), "luw");
        assert_eq!(
            granularity_profile_token(&[af("sudachi-c", "sudachi"), af("vaporetto", "vaporetto")]),
            "suw+luw"
        );
    }

    #[test]
    fn granularity_profile_mixed_sudachi_modes_composes_classes() {
        assert_eq!(
            granularity_profile_token(&[af("sudachi-a", "sudachi"), af("sudachi-c", "sudachi")]),
            "suw+luw"
        );
        assert_eq!(
            granularity_profile_token(&[
                af("sudachi-a", "sudachi"),
                af("sudachi-b", "sudachi"),
                af("sudachi-c", "sudachi"),
            ]),
            "suw+muw+luw"
        );
        // Hypothetical: no mode-A analyzer present, muw + luw only.
        assert_eq!(
            granularity_profile_token(&[af("sudachi-b", "sudachi"), af("sudachi-c", "sudachi")]),
            "muw+luw"
        );
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

        /// The histogram p90 equals the expanded-multiset p90 exactly.
        #[test]
        fn histogram_percentile_matches_slice(
            lengths in proptest::collection::vec(0u64..5_000, 1..80),
        ) {
            let mut histogram = BTreeMap::new();
            for length in &lengths {
                *histogram.entry(*length as u32).or_insert(0u32) += 1;
            }
            let histogram = histogram.into_iter().collect::<Vec<_>>();
            prop_assert_eq!(percentile_90_histogram(&histogram), percentile_90(&lengths));
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

    #[test]
    fn lambda_policy_parses_rank_floor_and_fixed() {
        assert_eq!(
            "rank-floor".parse::<LambdaMissingPolicy>().unwrap(),
            LambdaMissingPolicy::RankFloor
        );
        assert_eq!(
            "fixed:0.005".parse::<LambdaMissingPolicy>().unwrap(),
            LambdaMissingPolicy::Fixed(0.005)
        );
        assert_eq!(
            "fixed:0".parse::<LambdaMissingPolicy>().unwrap(),
            LambdaMissingPolicy::Fixed(0.0)
        );
        assert!("fixed:-0.1".parse::<LambdaMissingPolicy>().is_err());
        assert!("fixed:abc".parse::<LambdaMissingPolicy>().is_err());
        assert!("floor".parse::<LambdaMissingPolicy>().is_err());
    }

    #[test]
    fn lambda_policy_display_round_trips_into_score_block_string() {
        assert_eq!(LambdaMissingPolicy::RankFloor.to_string(), "rank-floor");
        assert_eq!(LambdaMissingPolicy::Fixed(0.005).to_string(), "fixed:0.005");
        assert_eq!(LambdaMissingPolicy::Fixed(0.0).to_string(), "fixed:0");
    }

    /// Looks a signal's assigned rank up by `Signal` identity rather than
    /// slot index — segmentation and feature kinds place the same signal
    /// at different slots (e.g. `Span` is slot 3 for feature, slot 2 for
    /// segmentation), so a slot-index bug in pooled ranking would not show
    /// up if tests only ever indexed by position.
    fn rank_by_signal(scores: &[PatternScore], index: usize, signal: Signal) -> Option<usize> {
        scores[index]
            .signals
            .iter()
            .find(|(s, _, _)| *s == signal)
            .and_then(|(_, _, rank)| *rank)
    }

    #[test]
    fn within_kind_default_scoring_matches_hand_computed_ranks() {
        // 3 feature + 2 segmentation patterns, each signal given a
        // deliberately distinct order across coverage/rarity/span/impact
        // so no assertion could pass by accident from correlated inputs.
        let rarity_total = 50;
        let patterns = vec![
            calib_stats(PatternKind::Feature, "f1", 1, 1, 50.0, Some("pos1")), // rarity 1, coverage 1, span 50, impact 4.0
            calib_stats(PatternKind::Feature, "f2", 2, 3, 80.0, Some("lemma")), // rarity 2, coverage 3, span 80, impact 3.5
            calib_stats(PatternKind::Feature, "f3", 4, 5, 20.0, Some("ctype")), // rarity 4, coverage 5, span 20, impact 2.5
            calib_stats(PatternKind::Segmentation, "s1", 1, 2, 15.0, None), // rarity 1, coverage 2, span 15
            calib_stats(PatternKind::Segmentation, "s2", 3, 6, 45.0, None), // rarity 3, coverage 6, span 45
        ];
        let scores = score_patterns(
            &patterns,
            rarity_total,
            RankScope::WithinKind,
            LambdaMissingPolicy::RankFloor,
        );

        // f1: coverage rank3 (raw log2(2)=1.0, lowest), rarity rank1 (raw
        // log2(51/2) highest), impact rank1 (pos1=4.0 highest), span rank2
        // (50.0, between f3's 20.0 and f2's 80.0).
        assert_eq!(rank_by_signal(&scores, 0, Signal::Coverage), Some(3));
        assert_eq!(rank_by_signal(&scores, 0, Signal::Rarity), Some(1));
        assert_eq!(rank_by_signal(&scores, 0, Signal::Impact), Some(1));
        assert_eq!(rank_by_signal(&scores, 0, Signal::Span), Some(2));
        // f2: coverage rank2, rarity rank2, impact rank2 (lemma=3.5), span
        // rank1 (80.0, highest of the three).
        assert_eq!(rank_by_signal(&scores, 1, Signal::Coverage), Some(2));
        assert_eq!(rank_by_signal(&scores, 1, Signal::Rarity), Some(2));
        assert_eq!(rank_by_signal(&scores, 1, Signal::Impact), Some(2));
        assert_eq!(rank_by_signal(&scores, 1, Signal::Span), Some(1));
        // f3: coverage rank1 (log2(6) highest), rarity rank3 (lowest),
        // impact rank3 (ctype=2.5, lowest), span rank3 (20.0, lowest).
        assert_eq!(rank_by_signal(&scores, 2, Signal::Coverage), Some(1));
        assert_eq!(rank_by_signal(&scores, 2, Signal::Rarity), Some(3));
        assert_eq!(rank_by_signal(&scores, 2, Signal::Impact), Some(3));
        assert_eq!(rank_by_signal(&scores, 2, Signal::Span), Some(3));
        // s1: coverage rank2, rarity rank1, span rank2; no impact slot.
        assert_eq!(rank_by_signal(&scores, 3, Signal::Coverage), Some(2));
        assert_eq!(rank_by_signal(&scores, 3, Signal::Rarity), Some(1));
        assert_eq!(rank_by_signal(&scores, 3, Signal::Span), Some(2));
        assert_eq!(rank_by_signal(&scores, 3, Signal::Impact), None);
        assert_eq!(scores[3].signals.len(), 3);
        // s2: coverage rank1, rarity rank2, span rank1; no impact slot.
        assert_eq!(rank_by_signal(&scores, 4, Signal::Coverage), Some(1));
        assert_eq!(rank_by_signal(&scores, 4, Signal::Rarity), Some(2));
        assert_eq!(rank_by_signal(&scores, 4, Signal::Span), Some(1));
        assert_eq!(rank_by_signal(&scores, 4, Signal::Impact), None);
        assert_eq!(scores[4].signals.len(), 3);

        // Feature pool has 3 patterns, segmentation pool has 2.
        let lambda_feature = 1.0 / (RRF_K + 3.0 + 1.0);
        let lambda_segmentation = 1.0 / (RRF_K + 2.0 + 1.0);
        for index in [0, 1, 2] {
            assert!((scores[index].lambda - lambda_feature).abs() < 1e-12);
        }
        for index in [3, 4] {
            assert!((scores[index].lambda - lambda_segmentation).abs() < 1e-12);
        }

        // rrf_score is round6 of the RRF-fusion formula applied to the
        // ranks derived above — computed here from the formula, not by
        // reading the ranks back out of `scores`.
        let expected_rrf = |ranks: &[usize]| -> f64 {
            ranks.iter().map(|rank| 1.0 / (RRF_K + *rank as f64)).sum::<f64>()
                / ranks.len() as f64
        };
        assert_eq!(scores[0].rrf_score, round6(expected_rrf(&[3, 1, 1, 2])));
        assert_eq!(scores[1].rrf_score, round6(expected_rrf(&[2, 2, 2, 1])));
        assert_eq!(scores[2].rrf_score, round6(expected_rrf(&[1, 3, 3, 3])));
        assert_eq!(scores[3].rrf_score, round6(expected_rrf(&[2, 1, 2])));
        assert_eq!(scores[4].rrf_score, round6(expected_rrf(&[1, 2, 1])));
    }

    #[test]
    fn global_rank_scope_pools_signal_ranks_across_kinds() {
        // Two feature + two segmentation patterns with rarity raws ordered
        // feature[0] > seg[0] > feature[1] > seg[1], and span raws ordered
        // feature[1] > seg[0] > seg[1] > feature[0] — an independent order
        // so the span assertion cannot pass by riding on the rarity setup.
        let patterns = vec![
            calib_stats(PatternKind::Feature, "feat-0", 1, 0, 10.0, Some("pos1")),
            calib_stats(PatternKind::Feature, "feat-1", 3, 0, 40.0, Some("pos1")),
            calib_stats(PatternKind::Segmentation, "seg-0", 2, 0, 30.0, None),
            calib_stats(PatternKind::Segmentation, "seg-1", 4, 0, 20.0, None),
        ];
        let within = score_patterns(&patterns, 100, RankScope::WithinKind, LambdaMissingPolicy::RankFloor);
        let global = score_patterns(&patterns, 100, RankScope::Global, LambdaMissingPolicy::RankFloor);
        // seg[0] (index 2): rank 1 within its kind, rank 2 globally.
        assert_eq!(rank_by_signal(&within, 2, Signal::Rarity), Some(1));
        assert_eq!(rank_by_signal(&global, 2, Signal::Rarity), Some(2));
        // Span lives at a different slot per kind (feature slot 3,
        // segmentation slot 2 — see `Signal::applicable`); pooling must
        // still be by signal identity, not slot position. seg[0]'s span
        // (30.0) is within-kind rank 1 (beats seg[1]'s 20.0) but globally
        // rank 2 (feature[1]'s 40.0 pools ahead of it).
        assert_eq!(rank_by_signal(&within, 2, Signal::Span), Some(1));
        assert_eq!(rank_by_signal(&global, 2, Signal::Span), Some(2));
        // Global λ uses N_total = 4 for every pattern.
        assert!((global[0].lambda - 1.0 / (RRF_K + 4.0 + 1.0)).abs() < 1e-12);
        // Impact stays feature-only under global scope: segmentation
        // patterns still have 3 signal slots.
        assert_eq!(global[2].signals.len(), 3);
    }

    #[test]
    fn fixed_lambda_policy_replaces_rank_floor_term() {
        let patterns = vec![
            calib_stats(PatternKind::Feature, "feat-0", 1, 0, 1.0, Some("pos1")),
            calib_stats(PatternKind::Segmentation, "seg-0", 1, 0, 1.0, None),
        ];
        let scores = score_patterns(&patterns, 100, RankScope::WithinKind, LambdaMissingPolicy::Fixed(0.005));
        for score in &scores {
            assert!((score.lambda - 0.005).abs() < 1e-12);
        }
        let zero = score_patterns(&patterns, 100, RankScope::WithinKind, LambdaMissingPolicy::Fixed(0.0));
        for score in &zero {
            assert_eq!(score.lambda, 0.0);
        }
    }

    #[test]
    fn anomaly_w_cov_option_scales_coverage_term() {
        let root = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(root.path());
        let source_ids = read_distinct_column(&run_dir, WarehouseTable::Sources, 1).unwrap();
        let rarity = rarity_config(&run_dir, &source_ids).unwrap();
        let options = WarehouseInterestingOptions {
            engine: InterestingEngine::InMemory,
            ..Default::default()
        };
        let collected = collect_in_memory(&run_dir, &options, &rarity).unwrap();
        let AnomalySource::InMemory {
            memberships,
            region_flags,
            punctuation_only,
        } = &collected.anomaly_source
        else {
            panic!("expected in-memory anomaly source");
        };
        let top_set = BTreeSet::new();
        let high = anomaly_channel_in_memory(
            memberships,
            region_flags,
            punctuation_only,
            InterestingTextFilter::All,
            &top_set,
            10,
            5.0,
        );
        let low = anomaly_channel_in_memory(
            memberships,
            region_flags,
            punctuation_only,
            InterestingTextFilter::All,
            &top_set,
            10,
            2.0,
        );
        let high_score = high
            .iter()
            .find(|row| row.has_coverage_mismatch)
            .unwrap()
            .anomaly_score;
        let low_score = low
            .iter()
            .find(|row| row.has_coverage_mismatch)
            .unwrap()
            .anomaly_score;
        assert!((high_score - low_score - 3.0).abs() < 1e-9);
    }

    #[test]
    fn score_block_records_rank_scope_and_dynamic_knobs() {
        let block = score_version_block(
            "source",
            WarehouseFeatureProfile::Core,
            "suw",
            &WarehouseInterestingOptions {
                rank_scope: RankScope::Global,
                lambda_policy: LambdaMissingPolicy::Fixed(0.01),
                anomaly_w_cov: 2.0,
                ..WarehouseInterestingOptions::default()
            },
        );
        assert_eq!(block.rank_scope, "global");
        assert_eq!(block.lambda_missing_policy, "fixed:0.01");
        assert!((block.anomaly_w_cov - 2.0).abs() < f64::EPSILON);
    }

    #[test]
    fn score_block_records_score_mode_and_sample_seed() {
        let block = score_version_block(
            "source",
            WarehouseFeatureProfile::Core,
            "suw",
            &WarehouseInterestingOptions {
                score_mode: ScoreMode::Random,
                sample_seed: Some(7),
                ..WarehouseInterestingOptions::default()
            },
        );
        assert_eq!(block.score_mode, "random");
        assert_eq!(block.sample_seed, Some(7));
    }

    #[test]
    fn splitmix64_is_pinned_for_cross_version_stability() {
        // Golden: the first three outputs for seed 1234567, computed at
        // plan time from Vigna's public-domain reference algorithm. The
        // test's job is that they never change — shuffle artifacts must be
        // reproducible years later.
        let mut state = 1234567u64;
        let observed = [
            splitmix64(&mut state),
            splitmix64(&mut state),
            splitmix64(&mut state),
        ];
        assert_eq!(
            observed,
            [
                0x599e_d017_fb08_fc85,
                0x2c73_f084_5854_0fa5,
                0x883e_bce5_a3f2_7c77,
            ]
        );
    }

    #[test]
    fn frequency_mode_orders_by_examples_then_source_count_then_id() {
        let mut patterns = vec![
            calib_stats(PatternKind::Feature, "p0", 1, 1, 1.0, Some("pos1")),
            calib_stats(PatternKind::Feature, "p1", 1, 1, 1.0, Some("pos1")),
            calib_stats(PatternKind::Feature, "p2", 1, 1, 1.0, Some("pos1")),
        ];
        patterns[0].examples = 5;
        patterns[0].source_count = 3;
        patterns[1].examples = 9;
        patterns[1].source_count = 1;
        patterns[2].examples = 5;
        patterns[2].source_count = 7;
        let order = baseline_order(&patterns, ScoreMode::Frequency, None).unwrap();
        // examples desc, then source_count desc, then pattern_id asc.
        assert_eq!(order, vec![1, 2, 0]);
    }

    #[test]
    fn random_mode_is_seed_deterministic_and_seed_sensitive() {
        let patterns = (0..8)
            .map(|i| {
                calib_stats(
                    PatternKind::Feature,
                    &format!("p{i}"),
                    1,
                    1,
                    1.0,
                    Some("pos1"),
                )
            })
            .collect::<Vec<_>>();
        let a = baseline_order(&patterns, ScoreMode::Random, Some(42)).unwrap();
        let b = baseline_order(&patterns, ScoreMode::Random, Some(42)).unwrap();
        let c = baseline_order(&patterns, ScoreMode::Random, Some(43)).unwrap();
        assert_eq!(a, b);
        assert_ne!(a, c);
        let mut sorted = a.clone();
        sorted.sort_unstable();
        assert_eq!(sorted, (0..8).collect::<Vec<_>>());
    }

    #[test]
    fn random_mode_without_seed_errors() {
        let patterns = vec![calib_stats(PatternKind::Feature, "p0", 1, 1, 1.0, Some("pos1"))];
        assert!(baseline_order(&patterns, ScoreMode::Random, None).is_err());
    }

    #[test]
    fn sample_seed_with_non_random_score_mode_is_rejected() {
        let root = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(root.path());
        let err = summarize_warehouse_interesting(
            &run_dir,
            WarehouseInterestingOptions {
                score_mode: ScoreMode::Rrf,
                sample_seed: Some(7),
                ..WarehouseInterestingOptions::default()
            },
        )
        .unwrap_err();
        assert!(
            err.to_string()
                .contains("--sample-seed is only meaningful with --score-mode random")
        );
    }
}
