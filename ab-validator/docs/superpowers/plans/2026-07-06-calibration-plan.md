# Calibration Plan (Interestingness v1 Lock) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Execute the governing spec's §Calibration Plan (steps 1–9) against the canonical warehouse: add the four missing summarizer knobs (rank scope, λ policy, anomaly weight, baseline score modes), build the comparison/labeling/metrics tooling, run the sweeps, and hand the owner a blind labeling TSV. **The v1 lock is conditional, not delivered by Tasks 1–9:** it completes only when the owner's labels return and the spec step-8 gate passes (RRF must beat frequency sort at p@50 — otherwise the mandated outcome is "revisit signal definitions", not a lock). Tasks 1–9 deliver everything up to that gate.

**Architecture:** All ranking happens in the shared Rust function `score_patterns` (`crates/ab-morph-run/src/summary/interesting.rs:872`) — both collection engines (in-memory and DuckDB) produce `Vec<PatternStats>` and never rank in SQL, so every scoring knob is a single-site change plus CLI threading plus `score_version` recording. Three new subcommands (`compare-interesting-rankings`, `export-interesting-labels`, `score-interesting-labels`) consume the ranking JSON artifacts. Sweeps run on a new deterministic ~1000-source triage warehouse; method comparisons run on the canonical full run.

**Tech Stack:** Rust (clap, serde, arrow/parquet already in-crate), DuckDB CLI engine (env `AB_DUCKDB_BIN`), `ab-plaintext::from_aat_value` for labeling snippets. No new dependencies: the seeded shuffle uses an in-crate splitmix64 (stable across toolchains, unlike `rand::StdRng` across versions).

## Global Constraints

- ALL test invocations: `cargo test -p ab-morph-run --features test-analyzer` (bare invocation false-fails 2 bin tests).
- Default-knob behavior must be byte-identical to today's ranking output for `rows` and `anomalies`; the only default-path output change is three additive `score_version` fields (`rank_scope`, `score_mode`, `sample_seed`). `SCORE_VERSION` stays `1` (`interesting.rs:41`): default semantics are unchanged; the new fields extend the comparability contract additively.
- Every knob that affects ordering or scores MUST be recorded in the `score_version` block (spec §Score Versioning).
- Determinism: any randomized ordering requires an explicit `--sample-seed` (spec §Tie-Breaking); same seed ⇒ byte-identical output.
- Canonical run: `/db/ab-validator/morph-warehouse/runs/full-2026-07-05_164518-jobs0` (17,885 sources, `rarity_basis="work"` via its `aozora_works.parquet` sidecar). Its files are never modified by this plan (sidecar exception already used; nothing new is written into it).
- Warehouse analyze runs use `jobs=8`; full-corpus summarize takes ~8–10 min (use `run_in_background` for any command that can exceed the 10-minute Bash cap).
- ABC export for `import-aozora-metadata`: `../abc/out/corpus` (i.e. `/home/bor/Projects/abc/out/corpus`).
- Tracked artifact home: `reports/morph-warehouse/calibration/` (scripts) and `reports/morph-warehouse/calibration/2026-07-06/` (this campaign's artifacts). Nothing calibration-related goes to gitignored `scratch/`.
- Worktree gotcha: repo-root `dictionary` is a tracked symlink to `../vibrato-pipe/dictionary` and dangles in worktrees — `ln -sfn /home/bor/Projects/ab-validator/dictionary dictionary` after entering, `git checkout -- dictionary` before finishing. (Only Tasks 6–8 touch analyzers/dictionaries; code tasks 1–5 don't need it.)
- `--explain` interacts with every knob: it reuses the same `score_patterns` output, so no special handling is needed — but never special-case it either.

## Decisions (adaptations of the spec's steps, decided at plan time)

| # | Decision | Rationale |
|---|---|---|
| D1 | Spec step 4's `λ_missing ∈ {0, 0.005, 0.010}` sweep becomes a **policy A/B**: `--lambda-missing-policy rank-floor\|fixed:<v>` with the sweep set {rank-floor, fixed:0, fixed:0.005, fixed:0.010} | v1 deviation 11: a fixed constant breaks missing-signal monotonicity past rank `1/λ − k`; rank-floor is the shipped default. The sweep's *intent* (top-50 stable under missing-data handling) is preserved by comparing policies. |
| D2 | Global rank scope pools ranks **per signal name across kinds**; applicability is unchanged (impact still fires only for feature patterns); `λ_rank-floor` uses `N_total`; the fusion divisor stays each pattern's applicable-signal count | Spec step 3 asks whether the within-kind *partition* is worth its comparability constraint. Only the rank pools may differ; changing applicability or normalization would confound the A/B. |
| D3 | Baselines reorder the same collected patterns: `--score-mode rrf\|frequency\|random`. Frequency = `examples` desc (occurrence count, "most common patterns first"), ties `source_count` desc then `pattern_id` asc. Random = Fisher–Yates over indices pre-sorted by `pattern_id`, splitmix64 keyed by `--sample-seed`. RRF signals are still computed and emitted in every mode | Collection is the expensive part and identical across methods; emitting signals keeps baseline rows fully inspectable. `score_mode` + `sample_seed` land in the score block so no artifact can be mistaken for an RRF ranking. |
| D4 | Labeling is **pooled and blind**: union of every method's top-50, deduped by `pattern_id`, presented in seeded-shuffle order with no method/rank/score columns. One TSV scores all methods | The owner labels each pattern once (~120–170 rows instead of 200); hiding provenance prevents rank-anchoring bias. A separate `mapping.json` carries `label_id → {pattern_id, per-method ranks}` for the metrics step. |
| D5 | Metric definitions: **p@50** counts verdicts in {`bug`, `expected-dictionary`}; **nDCG@50** gains: `bug`=3, `expected-dictionary`=2, `corpus-artifact`=1, `expected-policy`=1, `noise`=0, `unclear`=0; IDCG from the pooled union's best 50 gains (standard pooled evaluation) | The ranker's job is surfacing analyzer defects and dictionary gaps; policy differences and corpus artifacts are known/structural. Constants live in one place in code and the report invites the owner to dispute them — recomputation from the same labels is free. Pooled IDCG is the methodological choice that makes methods surfacing *different* item sets comparable: every method's DCG normalizes against the best achievable ordering of the pooled labeled items, not its own surfaced set, so nDCG implicitly penalizes low-value sets while p@50 keeps the fixed denominator k. |
| D6 | Stability thresholds (spec leaves "within threshold" open): a sweep variant is *stable* vs the default when Kendall **τ-b ≥ 0.9 over the rank intersection** AND **overlap ≥ 45/50** | τ alone is blind to set churn (it only sees the intersection); overlap alone is blind to reordering. Both bounds are recorded in the report and are analysis inputs, not code. |
| D7 | Subset corpora are deterministic **sorted-stride** selections of AAT filenames (stride `⌊N/n⌋`, first `n`), materialized as symlink farms under `/db/ab-validator/aat-corpus/subsets/` | Reproducible without a seed, spreads selection across the card-id/person-id range (the dir is sorted by person prefix), no analyzer code change. |
| D8 | `inheritance_jaccard_threshold` is **not implemented in v1** (grep-verified: no occurrence in the crate); spec step 9 locks it as `n/a — not implemented in v1` | Locking a knob that doesn't exist would fabricate a default. The spec amendment (Task 9) records this. |
| D9 | Snippet extraction for the labeling TSV re-derives each source's projected text via `ab_plaintext::from_aat_value` and slices warehouse char offsets directly | Valid because the canonical run was produced without orthographic normalization (no ortho flags in the `analyze-aat` recipe invocation — Task 4 Step 0 re-verifies this in-task rather than trusting this plan-time claim), so warehouse `char_*` offsets index the unnormalized projection. The exporter hard-fails on out-of-range offsets rather than emitting silently wrong snippets. |
| D10 | Spec step 1's "three warehouse profiles" is read as **three corpus sizes** (~100 / ~1000 / full ~17,885), not the codebase's `WarehouseProfile` enum (which selects *table subsets*: Full vs Triage). Calibration runs always use the full table profile | The spec text itself glosses its "profiles" as source counts ("small smoke corpus (~100 sources)…"); the name collision with `--warehouse-profile` is pre-existing. Recorded so nobody "fixes" a calibration run onto the Triage table subset — it omits `nway_feature_diffs`, which the feature-signal path requires. |

## File Structure

- `crates/ab-morph-run/src/summary/interesting.rs` — knob enums (`RankScope`, `LambdaMissingPolicy`, `ScoreMode`), options fields, `score_patterns` scope/λ generalization, baseline ordering, anomaly W_COV threading, score-block fields (Tasks 1–2)
- `crates/ab-morph-run/src/summary/interesting_sql.rs` — replace the `ANOMALY_W_COV` const reference with the options value (Task 1)
- `crates/ab-morph-run/src/calibration/mod.rs` — NEW: module root; shared `read_ranking` + re-exports only (Tasks 3–5)
- `crates/ab-morph-run/src/calibration/compare.rs` — NEW: Kendall τ-b, `compare_rankings`, block diff (Task 3)
- `crates/ab-morph-run/src/calibration/label_export.rs` — NEW: pooled blind TSV export — union/dedup, blind shuffle, AAT snippet extraction, mapping.json (Task 4)
- `crates/ab-morph-run/src/calibration/label_score.rs` — NEW: TSV parse, p@k, nDCG@k (Task 5)
  (one responsibility per file — the three subcommands share only `read_ranking` and the Task 2 shuffle primitive; do NOT grow a single `calibration.rs`)
- `crates/ab-morph-run/src/lib.rs` — `mod calibration;` + re-exports (Task 3)
- `crates/ab-morph-run/src/main.rs` — CLI flags on `SummarizeWarehouseInteresting`; three new `Command` variants (Tasks 1–5)
- `reports/morph-warehouse/calibration/make-aat-subset.sh` — NEW: sorted-stride symlink-farm builder (Task 6)
- `reports/morph-warehouse/calibration/2026-07-06/` — NEW: sweep/method artifacts, timings, labeling package, report (Tasks 6–9)
- `docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md` — §Calibration Plan amendments (Task 9)

Region-example note for Task 4: `RegionExampleOut` (`interesting.rs:152-159`) carries `source_id, text_id, region_index, char_start, char_end` — char offsets only, no text. AAT files live one-per-source at `<aat-dir>/<source_id>.json`; `ab_plaintext::from_aat_value(&aat) -> PlainTextDocument { text_id, source_format, text }` yields the projected text those offsets index (D9).

---

### Task 1: Scoring knobs — `--rank-scope`, `--lambda-missing-policy`, `--anomaly-w-cov`

**Files:**
- Modify: `crates/ab-morph-run/src/summary/interesting.rs`
- Modify: `crates/ab-morph-run/src/summary/interesting_sql.rs` (one line: W_COV source)
- Modify: `crates/ab-morph-run/src/main.rs` (flags + threading + parse test)
- Tests: in-file `#[cfg(test)]` in `interesting.rs`, parse tests in `main.rs`

**Interfaces:**
- Produces: `pub enum RankScope { WithinKind, Global }` (clap `ValueEnum`), `pub enum LambdaMissingPolicy { RankFloor, Fixed(f64) }` (custom `FromStr`), new `WarehouseInterestingOptions` fields `rank_scope: RankScope`, `lambda_policy: LambdaMissingPolicy`, `anomaly_w_cov: f64`; `ScoreVersionBlock` gains `pub rank_scope: String`; `lambda_missing_policy`/`anomaly_w_cov` become dynamic. Task 2 builds on these options fields; Tasks 3–5 read the block fields from JSON.

- [ ] **Step 1: Write failing tests for the enums and policy parsing**

Append to the `#[cfg(test)] mod tests` in `crates/ab-morph-run/src/summary/interesting.rs`:

```rust
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
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p ab-morph-run --features test-analyzer lambda_policy -- --nocapture`
Expected: FAIL to compile (`LambdaMissingPolicy` not defined).

- [ ] **Step 3: Define the enums and extend the options struct**

In `crates/ab-morph-run/src/summary/interesting.rs`, immediately after the `InterestingEngine` enum (after line 67), insert:

```rust
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
```

Extend `WarehouseInterestingOptions` (lines 69–92) — add three fields and their defaults:

```rust
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
        }
    }
}
```

- [ ] **Step 4: Run the enum tests to verify they pass**

Run: `cargo test -p ab-morph-run --features test-analyzer lambda_policy -- --nocapture`
Expected: PASS (2 tests).

- [ ] **Step 5: Write failing tests for scope/λ-generalized scoring and W_COV threading**

The test module has NO `PatternStats` builders today — the tests near `interesting.rs:2028`/`2113` drive `fuse`/`lambda_missing` with raw rank arrays, and the integration tests go through `write_fixture` + the public entry point. Create exactly ONE minimal helper and use it for every new scoring-unit test in Tasks 1–2:

```rust
fn calib_stats(
    kind: PatternKind,
    pattern_id: &str,
    rarity_count: usize,
    coverage_region_count: usize,
    span_p90: f64,
    feature_key: Option<&str>,
) -> PatternStats
```

Fill the remaining fields neutrally (`examples`/`source_count`/`text_count` = 1, empty sample vecs and `region_examples`, `sql_signature: None`) and build the minimal `NwayPatternKey` the struct requires, threading `feature_key` through — it drives the Impact signal via `raw_signal_value`. Parameters exist only for what the tests vary. Add:

```rust
    #[test]
    fn within_kind_default_scoring_matches_hand_computed_ranks() {
        // Refactor-equivalence pin (write it BEFORE the Step 7 refactor; it
        // must pass identically before and after). Mixed fixture: 3 feature
        // patterns (feature_key Some("pos1"), distinct rarity_counts and
        // span_p90s, varying coverage_region_count) + 2 segmentation
        // patterns, all via calib_stats. Assert with defaults
        // (WithinKind, RankFloor):
        //  (a) exact per-signal ranks for EVERY pattern, looked up by
        //      Signal identity (not slot index);
        //  (b) lambda == 1/(60+3+1) for features, 1/(60+2+1) for
        //      segmentation;
        //  (c) each rrf_score equals round6 of the RRF formula computed in
        //      the test from the EXPECTED ranks (sum of 1/(60+rank) over
        //      applicable signals, missing → lambda, divided by applicable
        //      count) — from the formula, never by calling score_patterns
        //      to produce its own expectation.
    }

    #[test]
    fn global_rank_scope_pools_signal_ranks_across_kinds() {
        // Two feature + two segmentation patterns with rarity raws ordered
        // feature[0] > seg[0] > feature[1] > seg[1]. Within-kind: rarity
        // ranks are 1,2 inside each kind. Global: 1,2,3,4 across kinds.
        // ALSO assert the span signal pools across kinds — span sits at a
        // different slot index per kind (feature slot 3, segmentation slot
        // 2), which is exactly where a pooled-slot lookup bug would hide.
        // Look ranks up by Signal identity.
        let patterns = /* 4 PatternStats via calib_stats: 2 feature,
                          2 segmentation, rarity_count ordering raws as
                          above, span_p90 interleaved across kinds */;
        let within = score_patterns(
            &patterns, 100, RankScope::WithinKind, LambdaMissingPolicy::RankFloor,
        );
        let global = score_patterns(
            &patterns, 100, RankScope::Global, LambdaMissingPolicy::RankFloor,
        );
        let rarity_rank = |scores: &[PatternScore], i: usize| {
            scores[i].signals.iter()
                .find(|(s, _, _)| *s == Signal::Rarity)
                .and_then(|(_, _, rank)| *rank)
        };
        // seg[0] (index 2): rank 1 within its kind, rank 2 globally.
        assert_eq!(rarity_rank(&within, 2), Some(1));
        assert_eq!(rarity_rank(&global, 2), Some(2));
        // Global λ uses N_total = 4 for every pattern.
        assert!((global[0].lambda - 1.0 / (RRF_K + 4.0 + 1.0)).abs() < 1e-12);
        // Impact stays feature-only under global scope: segmentation
        // patterns still have 3 signal slots.
        assert_eq!(global[2].signals.len(), 3);
    }

    #[test]
    fn fixed_lambda_policy_replaces_rank_floor_term() {
        // A feature pattern with a missing impact raw (feature_key = None
        // never happens for feature kind, so use the existing fixture whose
        // impact is present and instead assert on the lambda field the
        // policy sets).
        let patterns = /* one feature + one segmentation pattern via
                          calib_stats */;
        let scores = score_patterns(
            &patterns, 100, RankScope::WithinKind, LambdaMissingPolicy::Fixed(0.005),
        );
        for score in &scores {
            assert!((score.lambda - 0.005).abs() < 1e-12);
        }
        let zero = score_patterns(
            &patterns, 100, RankScope::WithinKind, LambdaMissingPolicy::Fixed(0.0),
        );
        for score in &zero {
            assert_eq!(score.lambda, 0.0);
        }
    }

    #[test]
    fn anomaly_w_cov_option_scales_coverage_term() {
        // Reuse the existing anomaly-channel test fixture; call
        // anomaly_channel_in_memory twice with w_cov 5.0 and 2.0 and assert
        // a has_coverage_mismatch region's score drops by exactly 3.0.
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
```

(The two `/* ... */` fixture expressions are filled with the file's existing builder helpers — the implementer reads the neighboring tests at `interesting.rs:2028`/`2113` first and reuses whatever those use; the assertion structure above is binding, the construction expression is not.)

- [ ] **Step 6: Run new tests to verify they fail**

Run: `cargo test -p ab-morph-run --features test-analyzer global_rank_scope fixed_lambda anomaly_w_cov_option score_block_records -- --nocapture`
Expected: FAIL to compile (`score_patterns` has the old 2-arg signature; `score_version_block` has the old 3-arg signature; `ScoreVersionBlock` lacks `rank_scope`).

- [ ] **Step 7: Generalize `score_patterns`, `anomaly_channel_in_memory`, the SQL W_COV, and the score block**

`score_patterns` (`interesting.rs:872-942`) — new signature and partition logic. The within-kind arm must stay behaviorally identical (same iteration order, same tie-breaks):

```rust
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
```

`anomaly_channel_in_memory` (`interesting.rs:992-1041`): add a `w_cov: f64` parameter after `k: usize`; replace the `ANOMALY_W_COV` use at line 1015-1019 with `w_cov`. Update its call site (`interesting.rs:1242-1249`) to pass `options.anomaly_w_cov`.

`interesting_sql.rs:972`: replace `w_cov = super::interesting::ANOMALY_W_COV,` with `w_cov = options.anomaly_w_cov,` (`anomalies_duckdb` already receives `options`).

`ScoreVersionBlock` (`interesting.rs:104-125`): add after `lambda_missing_policy`:

```rust
    /// Signal-rank pooling scope: `"within-kind"` (v1 default) or `"global"`.
    pub rank_scope: String,
```

`score_version_block` (`interesting.rs:1114-1136`) — take the options and use the dynamic values:

```rust
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
```

Update all three `score_version_block(...)` call sites in `summarize_warehouse_interesting` (`interesting.rs:1175`, `1216`, `1260`) to pass `&options` — note the `:1175` site is the pre-collection empty-run early-return branch (`rows: Vec::new()`); `options` is already in scope there, pass it the same way, no special-casing. Update the `score_patterns` call site (`interesting.rs:1206`) to pass `options.rank_scope, options.lambda_policy`. Fix any other in-crate callers the compiler reports (test helpers included).

- [ ] **Step 8: Run the crate's full test suite**

Run: `cargo test -p ab-morph-run --features test-analyzer`
Expected: PASS. If any golden/engine-equality test asserts an exact serialized score block, update its expectation to include `"rank_scope": "within-kind"` — that is the only sanctioned golden change; any `rows`/`anomalies` diff under default knobs is a regression to fix, not a golden to update.

- [ ] **Step 9: Wire the CLI flags**

In `crates/ab-morph-run/src/main.rs`, extend the `SummarizeWarehouseInteresting` variant (lines 227–252) after `feature_profile`:

```rust
        #[arg(long, value_enum, default_value_t = ab_morph_run::RankScope::WithinKind)]
        rank_scope: ab_morph_run::RankScope,
        #[arg(long, default_value = "rank-floor")]
        lambda_missing_policy: ab_morph_run::LambdaMissingPolicy,
        #[arg(long, default_value_t = 5.0)]
        anomaly_w_cov: f64,
```

Thread them through the match arm (lines 622–645) into the options struct. Export the two enums from `lib.rs` alongside the existing `WarehouseInterestingOptions` re-export (find the existing `pub use` for `summary::interesting` items and extend it). Add a parse test next to the existing `SummarizeWarehouseInteresting` parse test (`main.rs:1682`):

```rust
    #[test]
    fn parses_summarize_interesting_scoring_knobs() {
        let cli = Cli::parse_from([
            "ab-morph-run",
            "summarize-warehouse-interesting",
            "--run-dir", "/tmp/run",
            "--rank-scope", "global",
            "--lambda-missing-policy", "fixed:0.005",
            "--anomaly-w-cov", "2",
        ]);
        let Command::SummarizeWarehouseInteresting {
            rank_scope, lambda_missing_policy, anomaly_w_cov, ..
        } = cli.command else { panic!("wrong variant") };
        assert_eq!(rank_scope, ab_morph_run::RankScope::Global);
        assert_eq!(
            lambda_missing_policy,
            ab_morph_run::LambdaMissingPolicy::Fixed(0.005)
        );
        assert!((anomaly_w_cov - 2.0).abs() < f64::EPSILON);
    }
```

(Match the surrounding parse tests' actual CLI-struct idiom — if they use a helper instead of `Cli::parse_from`, follow it.)

- [ ] **Step 10: Run the full suite and commit**

Run: `cargo test -p ab-morph-run --features test-analyzer`
Expected: PASS.

```bash
git add crates/ab-morph-run/src/summary/interesting.rs crates/ab-morph-run/src/summary/interesting_sql.rs crates/ab-morph-run/src/main.rs crates/ab-morph-run/src/lib.rs
git commit -m "feat(interesting): rank-scope, lambda-policy, anomaly-w-cov calibration knobs"
```

---

### Task 2: Baseline score modes — `--score-mode` + `--sample-seed`

**Files:**
- Modify: `crates/ab-morph-run/src/summary/interesting.rs`
- Modify: `crates/ab-morph-run/src/main.rs`

**Interfaces:**
- Consumes: Task 1's options fields.
- Produces: `pub enum ScoreMode { Rrf, Frequency, Random }` (ValueEnum), options fields `score_mode: ScoreMode`, `sample_seed: Option<u64>`; `ScoreVersionBlock` fields `pub score_mode: String`, `pub sample_seed: Option<u64>`; `pub(crate) fn splitmix64(state: &mut u64) -> u64` (Task 4 reuses it for the labeling shuffle — place it in `interesting.rs` as `pub(crate)`).

- [ ] **Step 1: Write failing tests**

```rust
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
        let patterns = /* three patterns via calib_stats (Task 1's helper);
                          set examples 5, 9, 5 and distinct source_counts on
                          the tied pair by mutating the returned structs —
                          do not widen the helper's signature for this */;
        let order = baseline_order(&patterns, ScoreMode::Frequency, None).unwrap();
        // examples desc, then source_count desc, then pattern_id asc.
        assert_eq!(order, vec![1, /* tied pair by source_count desc */ ...]);
    }

    #[test]
    fn random_mode_is_seed_deterministic_and_seed_sensitive() {
        let patterns = /* 8 patterns via calib_stats */;
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
        let patterns = /* one pattern via calib_stats */;
        assert!(baseline_order(&patterns, ScoreMode::Random, None).is_err());
    }
```

For `splitmix64_matches_reference_vector`: the implementer transcribes the reference algorithm (below), runs it once for seed `1234567`, and pins the three first outputs as the golden — the point is cross-version stability from now on, not conformance to an external vector we can't verify offline. Write the pinned values as plain hex literals (delete the placeholder expression above).

- [ ] **Step 2: Run to verify failure** — `cargo test -p ab-morph-run --features test-analyzer splitmix64 frequency_mode random_mode -- --nocapture` — expected: compile FAIL.

- [ ] **Step 3: Implement**

In `interesting.rs`:

```rust
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
```

Options fields + block fields (`score_mode: String`, `sample_seed: Option<u64>` after `rank_scope`), block builder lines:

```rust
        score_mode: options.score_mode.as_str().to_owned(),
        sample_seed: options.sample_seed,
```

In `summarize_warehouse_interesting`, replace the single `ranked_order` call (`interesting.rs:1207`) with:

```rust
    let order = match options.score_mode {
        ScoreMode::Rrf => ranked_order(&collected.patterns, &scores),
        mode => baseline_order(&collected.patterns, mode, options.sample_seed)?,
    };
```

Validate early (top of the function, next to the `feature_profile == Schema` guard at `interesting.rs:1151`): `if options.score_mode != ScoreMode::Random && options.sample_seed.is_some() { bail!("--sample-seed is only meaningful with --score-mode random"); }` — an ignored seed silently misdescribes the artifact.

CLI: add to the variant

```rust
        #[arg(long, value_enum, default_value_t = ab_morph_run::ScoreMode::Rrf)]
        score_mode: ab_morph_run::ScoreMode,
        #[arg(long)]
        sample_seed: Option<u64>,
```

thread through the match arm, re-export `ScoreMode`, and extend the Task 1 parse test (or add a sibling) covering `--score-mode random --sample-seed 7`.

- [ ] **Step 4: Run the full suite** — `cargo test -p ab-morph-run --features test-analyzer` — expected: PASS (same golden rule as Task 1 Step 8: block-only diffs sanctioned — `"score_mode": "rrf"`, `"sample_seed": null` — row diffs are bugs).

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/summary/interesting.rs crates/ab-morph-run/src/main.rs crates/ab-morph-run/src/lib.rs
git commit -m "feat(interesting): frequency/random baseline score modes with mandatory seed"
```

---

### Task 3: `compare-interesting-rankings` subcommand

**Files:**
- Create: `crates/ab-morph-run/src/calibration/mod.rs` (module docs, shared `read_ranking`, `pub use` of the submodules' entry points)
- Create: `crates/ab-morph-run/src/calibration/compare.rs` (everything else in this task)
- Modify: `crates/ab-morph-run/src/lib.rs` (`mod calibration;` + re-exports)
- Modify: `crates/ab-morph-run/src/main.rs` (new variant + arm + parse test)
- Modify: `crates/ab-morph-run/src/summary/interesting.rs` (derive `Deserialize` on the output structs)

One responsibility per file (design-review requirement): `mod.rs` holds only what Tasks 4–5 share (`read_ranking`); τ/comparison code goes in `compare.rs`. Split the plan's single code block accordingly — the module docs and `read_ranking` into `mod.rs`, the rest into `compare.rs`.

**Interfaces:**
- Consumes: ranking JSON artifacts (serialized `InterestingSummary`).
- Produces: `pub fn run_compare_rankings(left: &Path, right: &Path) -> Result<RankingComparison>`; pure fns `kendall_tau_b(&[(usize, usize)]) -> Option<f64>`, `pub struct RankingComparison { left_len, right_len, overlap, jaccard, kendall_tau_b: Option<f64>, score_version_mismatches: Vec<String> }` (Serialize). Task 7's sweep analysis calls the CLI; Task 5 reuses the JSON-reading helper `read_ranking(path) -> Result<InterestingSummary>`.

- [ ] **Step 1: Derive `Deserialize`**

Add `serde::Deserialize` to the derives of `InterestingSummary`, `ScoreVersionBlock`, `InterestingRow`, `SignalExplain`, `AnomalyRow` in `interesting.rs` (`RegionExampleOut` already has it).

- [ ] **Step 2: Write failing tests for the pure core**

`calibration.rs` test module:

```rust
    #[test]
    fn kendall_tau_b_extremes_and_ties() {
        // Perfect agreement.
        let pairs: Vec<(usize, usize)> = (1..=5).map(|r| (r, r)).collect();
        assert!((kendall_tau_b(&pairs).unwrap() - 1.0).abs() < 1e-12);
        // Perfect reversal.
        let pairs: Vec<(usize, usize)> = (1..=5).map(|r| (r, 6 - r)).collect();
        assert!((kendall_tau_b(&pairs).unwrap() + 1.0).abs() < 1e-12);
        // Hand-computed 4-element example with one discordant pair:
        // left 1,2,3,4 / right 1,2,4,3 → C=5, D=1, τ = 4/6.
        let pairs = vec![(1, 1), (2, 2), (3, 4), (4, 3)];
        assert!((kendall_tau_b(&pairs).unwrap() - 4.0 / 6.0).abs() < 1e-12);
        // Fewer than 2 pairs: undefined.
        assert_eq!(kendall_tau_b(&[(1, 1)]), None);
        // All-tied on one side: denominator 0 → None.
        assert_eq!(kendall_tau_b(&[(1, 1), (1, 2), (1, 3)]), None);
    }

    #[test]
    fn comparison_reports_overlap_and_block_mismatches() {
        let left = fixture_summary(&["p1", "p2", "p3"], "within-kind");
        let right = fixture_summary(&["p2", "p1", "p4"], "global");
        let cmp = compare_rankings(&left, &right);
        assert_eq!(cmp.overlap, 2);
        assert!((cmp.jaccard - 2.0 / 4.0).abs() < 1e-12);
        // p1: ranks (1,2); p2: ranks (2,1) → one discordant pair, τ = -1.
        assert!((cmp.kendall_tau_b.unwrap() + 1.0).abs() < 1e-12);
        assert!(cmp
            .score_version_mismatches
            .iter()
            .any(|m| m.contains("rank_scope")));
    }
```

`fixture_summary(ids, rank_scope)` builds a minimal `InterestingSummary` in-test (rows with only `pattern_id` + defaults; a small local helper is fine here because `InterestingRow` now derives `Deserialize` — build it via `serde_json::from_value` to avoid spelling every field).

- [ ] **Step 3: Run to verify failure** — compile FAIL (module doesn't exist).

- [ ] **Step 4: Implement the module**

`crates/ab-morph-run/src/calibration/mod.rs` (docs + `read_ranking` + `mod compare; pub use compare::...;`), `crates/ab-morph-run/src/calibration/compare.rs` (the rest):

```rust
//! Calibration tooling for the interestingness ranker (spec §Calibration
//! Plan): ranking comparison (Kendall τ-b, overlap), pooled blind labeling
//! export, and label scoring (p@k, nDCG@k). Pure cores with thin IO shells;
//! ranking artifacts are the serialized `InterestingSummary` JSONs.
//! One responsibility per submodule: compare / label_export / label_score.

use std::collections::BTreeMap;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

use anyhow::{Context, Result};
use serde::Serialize;

use crate::summary::interesting::InterestingSummary;

pub fn read_ranking(path: &Path) -> Result<InterestingSummary> {
    let file = File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    serde_json::from_reader(BufReader::new(file))
        .with_context(|| format!("failed to parse ranking JSON {}", path.display()))
}

/// Kendall τ-b over (left_rank, right_rank) pairs. `None` when fewer than
/// two pairs or when a side is entirely tied (denominator 0).
pub fn kendall_tau_b(pairs: &[(usize, usize)]) -> Option<f64> {
    let n = pairs.len();
    if n < 2 {
        return None;
    }
    let (mut concordant, mut discordant, mut ties_left, mut ties_right) = (0i64, 0i64, 0i64, 0i64);
    for i in 0..n {
        for j in (i + 1)..n {
            let dl = pairs[i].0.cmp(&pairs[j].0);
            let dr = pairs[i].1.cmp(&pairs[j].1);
            use std::cmp::Ordering::Equal;
            match (dl, dr) {
                (Equal, Equal) => {}
                (Equal, _) => ties_left += 1,
                (_, Equal) => ties_right += 1,
                (a, b) if a == b => concordant += 1,
                _ => discordant += 1,
            }
        }
    }
    let n0 = (n * (n - 1) / 2) as i64;
    let denom = (((n0 - ties_left) as f64) * ((n0 - ties_right) as f64)).sqrt();
    if denom == 0.0 {
        return None;
    }
    Some((concordant - discordant) as f64 / denom)
}

#[derive(Debug, Clone, Serialize)]
pub struct RankingComparison {
    pub left_len: usize,
    pub right_len: usize,
    pub overlap: usize,
    pub jaccard: f64,
    pub kendall_tau_b: Option<f64>,
    /// Score-block fields whose values differ (field: left vs right).
    pub score_version_mismatches: Vec<String>,
}

pub fn compare_rankings(
    left: &InterestingSummary,
    right: &InterestingSummary,
) -> RankingComparison {
    let left_ranks: BTreeMap<&str, usize> = left
        .rows
        .iter()
        .enumerate()
        .map(|(i, row)| (row.pattern_id.as_str(), i + 1))
        .collect();
    let right_ranks: BTreeMap<&str, usize> = right
        .rows
        .iter()
        .enumerate()
        .map(|(i, row)| (row.pattern_id.as_str(), i + 1))
        .collect();
    let pairs = left_ranks
        .iter()
        .filter_map(|(id, lr)| right_ranks.get(id).map(|rr| (*lr, *rr)))
        .collect::<Vec<_>>();
    let union = left_ranks.len() + right_ranks.len() - pairs.len();
    RankingComparison {
        left_len: left_ranks.len(),
        right_len: right_ranks.len(),
        overlap: pairs.len(),
        jaccard: if union == 0 { 1.0 } else { pairs.len() as f64 / union as f64 },
        kendall_tau_b: kendall_tau_b(&pairs),
        score_version_mismatches: score_block_mismatches(left, right),
    }
}

fn score_block_mismatches(left: &InterestingSummary, right: &InterestingSummary) -> Vec<String> {
    let (l, r) = (
        serde_json::to_value(&left.score_version).expect("block serializes"),
        serde_json::to_value(&right.score_version).expect("block serializes"),
    );
    let (l, r) = (l.as_object().unwrap(), r.as_object().unwrap());
    l.iter()
        .filter(|(key, value)| r.get(*key) != Some(value))
        .map(|(key, value)| format!("{key}: {value} vs {}", r.get(key).unwrap_or(&serde_json::Value::Null)))
        .collect()
}

pub fn run_compare_rankings(left: &Path, right: &Path) -> Result<RankingComparison> {
    Ok(compare_rankings(&read_ranking(left)?, &read_ranking(right)?))
}
```

`lib.rs`: `mod calibration;` and `pub use calibration::{run_compare_rankings, RankingComparison};` (extend in Tasks 4–5). Check the visibility of `summary::interesting` from a sibling module — if `mod summary` doesn't expose `interesting` crate-wide, add `pub(crate)` re-exports in `summary/mod.rs` rather than widening the module.

CLI variant + arm:

```rust
    CompareInterestingRankings {
        #[arg(long)]
        left: PathBuf,
        #[arg(long)]
        right: PathBuf,
    },
```

```rust
        Command::CompareInterestingRankings { left, right } => {
            let comparison = ab_morph_run::run_compare_rankings(&left, &right)?;
            serde_json::to_writer_pretty(std::io::stdout(), &comparison)?;
            println!();
            Ok(())
        }
```

Plus a parse test following the file's idiom.

- [ ] **Step 5: Run the full suite** — expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-run/src/calibration/ crates/ab-morph-run/src/lib.rs crates/ab-morph-run/src/main.rs crates/ab-morph-run/src/summary/interesting.rs crates/ab-morph-run/src/summary/mod.rs
git commit -m "feat(calibration): compare-interesting-rankings (Kendall tau-b, overlap, block diff)"
```

---

### Task 4: `export-interesting-labels` subcommand (blind pooled labeling TSV)

**Files:**
- Create: `crates/ab-morph-run/src/calibration/label_export.rs` (all of this task's code; register in `calibration/mod.rs`)
- Modify: `crates/ab-morph-run/Cargo.toml` (add `ab-plaintext` workspace dep if absent — check first; `pipeline.rs` already uses it, so it is present)
- Modify: `crates/ab-morph-run/src/lib.rs`, `crates/ab-morph-run/src/main.rs`

**Interfaces:**
- Consumes: `read_ranking` (Task 3), `splitmix64` (Task 2, `pub(crate)` in `interesting.rs`), `ab_plaintext::from_aat_value`.
- Produces: `pub fn run_export_labels(opts: &ExportLabelsOptions) -> Result<ExportSummary>` with `pub struct ExportLabelsOptions { pub inputs: Vec<PathBuf>, pub aat_dir: PathBuf, pub output: PathBuf, pub mapping_output: PathBuf, pub seed: u64, pub snippets_per_pattern: usize, pub context_chars: usize, pub force: bool }`; TSV columns `label_id, kind, pattern, examples, source_count, text_count, snippet_1..N, verdict, notes`; `mapping.json` = `{ label_id: { pattern_id, ranks: { <method>: rank } } }` where method = input file stem. Task 5 consumes both files.

**Behavioral contract (test these, in this order of construction):**
1. Pool = union of `rows[].pattern_id` over all inputs, deduped; method name = input file stem; duplicate file stems → hard error (they'd silently merge in the mapping).
2. Blind order: pool sorted by `pattern_id`, then Fisher–Yates with splitmix64(`seed`) — same shuffle primitive as the random baseline. `label_id` = `L001`, `L002`, … in shuffled order.
3. Snippets: for each pattern take up to `snippets_per_pattern` of its `region_examples` (first input that carries the pattern wins — full-corpus inputs share region examples for shared patterns; document this in the module docs). For each example, read `<aat_dir>/<source_id>.json`, `ab_plaintext::from_aat_value`, slice chars `[char_start.saturating_sub(context) .. char_end + context)` **by char indices, not bytes** (`text.chars()` — offsets are char offsets), bracket the disagreement span as `…context【span】context…`. Strip `\t` and newlines from snippets (TSV safety: replace with `␣`). Out-of-range offsets → hard error naming source and region (D9). Missing AAT file → hard error (the corpus dir is wrong).
4. TSV header block: `#` comment lines documenting the verdict vocabulary verbatim — `bug`, `expected-policy`, `expected-dictionary`, `corpus-artifact`, `noise`, `unclear` — then the column header row. No method, rank, score, or pattern_id columns (blindness); `pattern_id` lives only in `mapping.json`.
5. Refuse to overwrite either output without `--force`.
6. Determinism: same inputs + seed ⇒ byte-identical TSV and mapping.

**Tests** (fixture AAT JSON written to a tempdir + two small ranking JSONs built via `serde_json::from_value`): shuffled-but-complete pool; multibyte snippet correctness (use a fixture text with kanji/kana so byte-slicing would split a char — assert exact snippet string); tab/newline scrubbing; mapping ranks per method; duplicate-stem error; out-of-range error; overwrite refusal; seed determinism (export twice, compare bytes).

**Steps:** (same TDD cadence as prior tasks)

- [ ] **Step 0 — verify the no-ortho assumption (D9) in-task:** inspect the `morph-warehouse-run-with-analyzers` recipe in the `justfile` (the invocation that produced the canonical run) and confirm the `analyze-aat` command passes no orthographic-normalization flags (check `analyze-aat`'s clap definition in `main.rs` for what such flags would be named). If an ortho flag WAS active, STOP — snippets cannot be sliced from the raw projection (offsets would index normalized text) and the task needs the offset map; escalate rather than emit wrong snippets.
- [ ] Write failing tests for the pure helpers: `blind_order(ids, seed)`, `snippet(text, start, end, context)` (pure over `&str`), `method_name(path)`.
- [ ] Run: `cargo test -p ab-morph-run --features test-analyzer blind_order snippet method_name` — compile FAIL.
- [ ] Implement pure helpers; tests pass.
- [ ] Write failing integration test `export_labels_end_to_end` (tempdir fixture: 2 ranking JSONs sharing 1 pattern + 1 unique each, 1 AAT file; assert TSV row count 3, snippet content, mapping JSON, determinism, `--force` behavior).
- [ ] Implement `run_export_labels` IO shell + CLI variant `ExportInterestingLabels { input: Vec<PathBuf> (required, repeated), aat_dir, output, mapping_output, seed (default 20260706), snippets_per_pattern (default 3), context_chars (default 20), force }` + arm + parse test + re-exports.
- [ ] Full suite: `cargo test -p ab-morph-run --features test-analyzer` — PASS.
- [ ] Commit: `git commit -m "feat(calibration): export-interesting-labels blind pooled labeling TSV"`

---

### Task 5: `score-interesting-labels` subcommand (p@50, nDCG@50)

**Files:**
- Create: `crates/ab-morph-run/src/calibration/label_score.rs` (all of this task's code; register in `calibration/mod.rs`)
- Modify: `crates/ab-morph-run/src/lib.rs`, `crates/ab-morph-run/src/main.rs`

**Interfaces:**
- Consumes: filled `labels.tsv` + `mapping.json` (Task 4 formats).
- Produces: `pub fn run_score_labels(labels: &Path, mapping: &Path, k: usize) -> Result<LabelScores>`; `pub struct LabelScores { pub k: usize, pub relevance: &'static str, pub gains: &'static str, pub verdict_counts: BTreeMap<String, usize>, pub methods: BTreeMap<String, MethodScores> }`, `pub struct MethodScores { pub labeled: usize, pub precision_at_k: f64, pub ndcg_at_k: f64 }`.

**Metric constants (D5) — one place, documented:**

```rust
/// p@k relevance: a verdict counts as relevant when it indicates the ranker
/// surfaced an analyzer defect or dictionary gap (D5; the report invites
/// dispute — rescoring from the same labels is free).
const RELEVANT_VERDICTS: &[&str] = &["bug", "expected-dictionary"];
/// nDCG gains (D5).
const VERDICT_GAINS: &[(&str, f64)] = &[
    ("bug", 3.0),
    ("expected-dictionary", 2.0),
    ("corpus-artifact", 1.0),
    ("expected-policy", 1.0),
    ("noise", 0.0),
    ("unclear", 0.0),
];
```

**Behavioral contract:**
1. TSV parse: skip `#` comments; require `label_id` and `verdict` columns; verdict must be one of the six (hard error naming label_id + offending value); empty verdict → hard error listing ALL unlabeled label_ids (partial labeling silently biases p@k).
1b. **Set coherence (orphan safety):** the TSV's `label_id` set must equal `mapping.json`'s key set exactly — a missing id (owner deleted a row), an unknown id (typo), or a duplicate id is a hard error naming the offending ids in each direction. This check runs BEFORE verdict validation; without it a deleted row silently drops a pattern from every method's top-k and skews p@k.
2. Per method (from mapping): order its labeled patterns by recorded rank; `precision_at_k` = |relevant among top-k| / k; `dcg_at_k` = Σ gain_i / log2(i+1) for i = 1..k; `idcg` from the pooled union's k best gains (D5); methods whose top-k has fewer than k pooled patterns → hard error (mapping and rankings out of sync).
3. Output JSON to stdout (same shape convention as Task 3).

**Tests:** hand-computed 5-pattern/2-method fixture (p@3 and nDCG@3 verified by hand in the test comments — show the arithmetic); unlabeled-row error lists ids; invalid verdict error; label_id set-mismatch errors (deleted TSV row; unknown/typo'd id; duplicated id); verdict histogram.

**Steps:** same TDD cadence — failing pure tests (`precision_at_k`, `ndcg_at_k` with hand arithmetic) → implement → failing integration test over tempdir TSV+mapping → implement shell + CLI variant `ScoreInterestingLabels { labels, mapping, k (default 50) }` + parse test → full suite → commit `feat(calibration): score-interesting-labels p@k and nDCG@k`.

---

### Task 6: Subset corpora + smoke/triage warehouse runs (operational)

**Files:**
- Create: `reports/morph-warehouse/calibration/make-aat-subset.sh`
- Create: `reports/morph-warehouse/calibration/2026-07-06/runs.md` (run ids, wall times, counts)

No Rust changes. Runs on the main checkout (needs live dictionaries — NOT inside a bare worktree unless the `dictionary` symlink is fixed per Global Constraints).

- [ ] **Step 1: Write the subset builder**

```bash
#!/usr/bin/env bash
# Deterministic sorted-stride AAT subset as a symlink farm (calibration D7).
# Usage: make-aat-subset.sh <aat_dir> <n> <dest_dir>
set -euo pipefail
aat_dir=$1; n=$2; dest=$3
[ -d "$dest" ] && { echo "refusing: $dest exists" >&2; exit 1; }
mkdir -p "$dest"
mapfile -t files < <(ls "$aat_dir"/*.json | sort)
total=${#files[@]}
stride=$(( total / n ))
[ "$stride" -ge 1 ] || { echo "n=$n exceeds corpus size $total" >&2; exit 1; }
for (( i = 0; i < n; i++ )); do
  ln -s "${files[$(( i * stride ))]}" "$dest/"
done
echo "linked $n of $total (stride $stride) into $dest"
```

`chmod +x`, then build both subsets:

```bash
reports/morph-warehouse/calibration/make-aat-subset.sh \
  /db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter 100 \
  /db/ab-validator/aat-corpus/subsets/calib-smoke-100
reports/morph-warehouse/calibration/make-aat-subset.sh \
  /db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter 1000 \
  /db/ab-validator/aat-corpus/subsets/calib-triage-1000
```

Expected: `linked 100 of 17885 (stride 178)` / `linked 1000 of 17885 (stride 17)`.

- [ ] **Step 2: Analyze both subsets** (time each; run in background if near the Bash cap)

```bash
just morph-warehouse-run full /db/ab-validator/aat-corpus/subsets/calib-smoke-100 calib-smoke-100 8
just morph-warehouse-run full /db/ab-validator/aat-corpus/subsets/calib-triage-1000 calib-triage-1000 8
```

(`full` here is the *table* profile — all 12 tables — not corpus size; run ids name the corpus subsets.) Expected: run dirs `/db/ab-validator/morph-warehouse/runs/calib-smoke-100` and `.../calib-triage-1000`, 0 errors, source counts 100 / 1000.

- [ ] **Step 3: Import metadata into both runs** (flips `rarity_basis` to `work`)

```bash
cargo run --release -p ab-morph-run -- import-aozora-metadata \
  --run-dir /db/ab-validator/morph-warehouse/runs/calib-smoke-100 --from ../abc/out/corpus
cargo run --release -p ab-morph-run -- import-aozora-metadata \
  --run-dir /db/ab-validator/morph-warehouse/runs/calib-triage-1000 --from ../abc/out/corpus
```

Expected: sources_mapped ≈ subset size (a strided subset may include the 2 known-unmappable sources; any skip count ≤ 2 with exactly those ids is fine — record actual numbers).

- [ ] **Step 4: Smoke-verify + record**

Run `summarize-warehouse-interesting --run-dir .../calib-smoke-100 --limit 10 --format json` and assert `rarity_basis == "work"`, `rank_scope == "within-kind"`, `score_mode == "rrf"` in the block. Write `runs.md` with: subset recipe + stride, run ids, wall times (analyze + summarize per profile — these are the spec's step-1 benchmark numbers), source counts, import counts.

- [ ] **Step 5: Commit** (script + runs.md only; warehouse dirs are outside the repo)

```bash
git add reports/morph-warehouse/calibration/
git commit -m "feat(calibration): deterministic AAT subset builder + smoke/triage runs record"
```

---

### Task 7: Sweeps on the triage run (operational)

**Files:**
- Create: `reports/morph-warehouse/calibration/2026-07-06/sweeps/` (ranking JSONs + comparison JSONs)
- Create: `reports/morph-warehouse/calibration/2026-07-06/sweep-analysis.md`

All commands target `--run-dir /db/ab-validator/morph-warehouse/runs/calib-triage-1000 --limit 50 --format json --output <artifact> `. Name artifacts exactly as below (the analysis references them).

- [ ] **Step 1: λ-policy sweep (spec step 4 per D1)** — produce `triage-rrf-rank-floor.json` (defaults), `triage-rrf-fixed0.json` (`--lambda-missing-policy fixed:0`), `triage-rrf-fixed0005.json` (`fixed:0.005`), `triage-rrf-fixed001.json` (`fixed:0.010`). Compare each fixed variant against rank-floor with `compare-interesting-rankings`, saving `cmp-lambda-<variant>.json`.

- [ ] **Step 2: Anomaly W_COV sweep (spec step 5)** — `--anomaly-w-cov 2` / default 5 / `10` → `triage-wcov{2,5,10}.json`. For each, tabulate the top-10 anomalies: `has_coverage_mismatch` count, span-length (`char_end - char_start`) min/median/max. The step-5 acceptance question: do coverage mismatches and long spans dominate, or noise?

- [ ] **Step 3: Rank-scope A/B (spec step 3)** — `--rank-scope global` → `triage-rrf-global.json`; `cmp-scope-triage.json` vs rank-floor default. (The p@50 side of step 3 waits on labels; τ/overlap now.)

- [ ] **Step 4: Analysis doc** — `sweep-analysis.md`: τ/overlap table for every comparison, D6 verdicts (stable/unstable per variant), anomaly tables + reading — note the W_COV recommendation is a *human read* of those tables (the spec's step-5 acceptance question is qualitative); record the rationale next to the tables rather than presenting the choice as mechanical — explicit statement of what can lock now (λ policy stays rank-floor if all fixed variants either are stable — meaning the choice barely matters, keep the monotone one — or unstable — meaning fixed λ distorts, keep rank-floor; W_COV recommendation from the tables) and what waits for labels (rank scope, step 8's RRF-vs-frequency verdict).

- [ ] **Step 5: Commit** — `git add reports/morph-warehouse/calibration/2026-07-06/ && git commit -m "feat(calibration): triage-corpus lambda/wcov/rank-scope sweeps + analysis"`

---

### Task 8: Full-corpus method runs + labeling package (operational)

**Files:**
- Create: `reports/morph-warehouse/calibration/2026-07-06/full/` (4 ranking JSONs)
- Create: `reports/morph-warehouse/calibration/2026-07-06/labels/labels.tsv`, `labels/mapping.json`, `labels/README.md`

Full-corpus summarize is ~8–10 min per invocation (DuckDB; `AB_DUCKDB_BIN` must be set) — use `run_in_background`, sequentially (they share the DuckDB temp/memory budget; do not parallelize).

- [ ] **Step 1: Four method runs** against `/db/ab-validator/morph-warehouse/runs/full-2026-07-05_164518-jobs0`, `--limit 50 --format json`:
  - `full-rrf-within.json` — defaults
  - `full-rrf-global.json` — `--rank-scope global`
  - `full-frequency.json` — `--score-mode frequency`
  - `full-random.json` — `--score-mode random --sample-seed 20260706`

- [ ] **Step 2: Continuity check** — `compare-interesting-rankings --left full-rrf-within.json --right scratch/full-novel-interesting-workbasis.json` (item 1's artifact). Expected: overlap 50/50, τ = 1.0, block mismatches only the three new fields. Anything else means the knob plumbing changed default scoring — STOP and fix before proceeding. Also `cmp-scope-full.json` (within vs global on full corpus).

- [ ] **Step 3: Labeling package**

```bash
cargo run --release -p ab-morph-run -- export-interesting-labels \
  --input .../full/full-rrf-within.json --input .../full/full-rrf-global.json \
  --input .../full/full-frequency.json --input .../full/full-random.json \
  --aat-dir /db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter \
  --output .../labels/labels.tsv --mapping-output .../labels/mapping.json
```

Verify: row count = |union| (expect ~120–190), every row has ≥1 non-empty snippet, no method/rank/pattern_id columns in the TSV. `labels/README.md`: verdict vocabulary with one-line definitions, how to fill the TSV (any editor; keep tabs intact), and the exact scoring command: `cargo run --release -p ab-morph-run -- score-interesting-labels --labels labels.tsv --mapping mapping.json`.

- [ ] **Step 4: Commit** — `git commit -m "feat(calibration): full-corpus method rankings + blind labeling package"`

---

### Task 9: Calibration report, spec amendments, handoff

**Files:**
- Create: `reports/morph-warehouse/calibration/2026-07-06/report.md`
- Modify: `docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md` (§Calibration Plan, §Score Versioning)

- [ ] **Step 1: `report.md`** — sections: (1) three-profile benchmark table (analyze + summarize wall times, from Task 6's runs.md + Task 8 timings — this discharges the spec's "benchmarking is part of the calibration plan" note); (2) λ-policy stability verdict (Task 7); (3) W_COV verdict + chosen default; (4) rank-scope τ/overlap with "p@50 verdict pending labels"; (5) locked-now vs pending-labels table for spec step 9 (rrf_k=60 unchanged; λ policy; W_COV; `inheritance_jaccard_threshold` = n/a per D8); (6) labeling instructions pointer + the D5 metric definitions with an explicit "dispute before labeling if you disagree" note; (7) what happens after labels (run score-interesting-labels, step 8's RRF-vs-frequency gate, final lock + spec update).

- [ ] **Step 2: Spec amendments** (each anchored verbatim at implementation time — the amender reads the current section text first):
  1. §Calibration Plan step 4: replace the fixed-constant sweep description with the policy A/B (D1), citing the v1 deviation.
  2. §Calibration Plan step 9: `inheritance_jaccard_threshold` → "n/a — not implemented in v1 (locking deferred to the feature that introduces it)".
  3. §Score Versioning example block: add the three new fields (`rank_scope`, `score_mode`, `sample_seed`) to the JSON example and field list.
  4. §Calibration Plan: append a status line — mechanical steps executed 2026-07-06, artifacts at `reports/morph-warehouse/calibration/2026-07-06/`, labels pending.

- [ ] **Step 3: Full suite once more** — `cargo test -p ab-morph-run --features test-analyzer` — PASS.

- [ ] **Step 4: Commit** — `git commit -m "docs(calibration): report, spec amendments, labeling handoff"`

---

## Self-Review Notes

- Spec coverage: step 1 → Tasks 6+8 (three profiles + benchmarks); step 2 → Tasks 2+8; step 3 → Tasks 1+7+8 (τ now, p@50 after labels); step 4 → Tasks 1+7 (per D1); step 5 → Tasks 1+7; step 6 → Tasks 4+8 (owner fills TSV); step 7 → Tasks 5 (tooling)+9 (pending labels); step 8 → gated on labels, wiring in Task 9's report; step 9 → Task 9 (locked-now vs pending table, D8 for the missing knob); step 10 → post-MVP, untouched.
- The owner-labeling gate is the plan's deliberate end state: Tasks 1–9 complete without owner input; p@50/nDCG@50 and the final lock happen when `labels.tsv` comes back.
- Type consistency: `score_patterns(patterns, rarity_total, rank_scope, lambda_policy)` (Task 1) is what Task 2's `summarize_warehouse_interesting` context assumes; `splitmix64` introduced in Task 2 is consumed by Task 4; `read_ranking` introduced in Task 3 is consumed by Tasks 4–5.
- Several tests in Tasks 1/2 carry `/* fixture */` placeholders by design: the binding content is the assertions; construction goes through the single `calib_stats` helper Task 1 introduces (verified at plan-review time: the test module has NO existing `PatternStats` builders — the unit tests near `interesting.rs:2028`/`2113` drive `fuse`/`lambda_missing` with raw rank arrays — so ONE new minimal helper is created rather than duplicating struct literals per test; B2 lesson).
- Post-plan critical review (2026-07-06) accepted and folded in: conditional-lock framing in the Goal (S8 gate), D10 (profile-terminology decision), the hand-computed within-kind equivalence pin + span-slot global assertion (P1), inline splitmix64 golden constants (P5), the `calib_stats` correction (P6), the `:1175` early-branch note (P7), pooled-IDCG rationale in D5 (P4), `calibration/` module split (design), TSV↔mapping set-coherence invariant in Task 5 (design), Task 4 Step 0 ortho verification (trust boundary), and the Task 7 human-judgment note on W_COV.
