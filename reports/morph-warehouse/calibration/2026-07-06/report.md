# Interestingness Ranking Calibration Campaign — Report (2026-07-06)

Campaign for `docs/superpowers/plans/2026-07-06-calibration-plan.md`, executing
the governing spec's §Calibration Plan (steps 1-9) against
`ab-morph-run summarize-warehouse-interesting`. This is the Task 9 report:
Tasks 1-8 built the knobs (rank scope, λ-missing policy, anomaly weight,
baseline score modes), the comparison/labeling/metrics tooling, ran the
sweeps and method rankings, and produced the owner's blind labeling package.
**The v1 lock is conditional**, not delivered here: it completes only when
the owner's labels return and spec step 8's gate passes. See §7.

All artifacts referenced below live under
`reports/morph-warehouse/calibration/2026-07-06/`.

## 1. Three-profile benchmarks

Spec §Known Limitations notes: "All performance claims are design targets,
not benchmarks. Benchmarking is part of the calibration plan." This
discharges that note with measured wall times across the three corpus sizes
spec step 1 asks for (small/representative/full), plus the summarize side
measured in Task 8.

| Profile | Sources | Analyze wall | Summarize wall (defaults, `--anomalies 10`) | Notes |
|---|---|---|---|---|
| smoke (`calib-smoke-100`) | 100 | analyze phase alone well under 1.5 min (3m17.2s total wall included a from-scratch `cargo build --release`, 1m47s, plus nix dictionary relinks) | 4.34s (10 rows, `--limit 10`) | first run in a fresh worktree |
| triage (`calib-triage-1000`) | 1,000 | 8m02.8s (near-pure analyze: cargo no-op 0.17s, nix relinks only) | 30.08s (50 rows, `--limit 50`, triage sweep baseline) | representative corpus |
| full (`full-2026-07-05_164518-jobs0`) | 17,885 | (pre-existing canonical run; not re-measured this campaign) | 18m06s (defaults, anomalies on, `AB_DUCKDB_MEMORY_LIMIT=28GB`/4 threads) — 7m11s-7m18s each for the three `--anomalies 0` baseline/method runs (`AB_DUCKDB_MEMORY_LIMIT=28GB`/8 threads) | anomaly-bearing runs are the expensive path; see §3 for why |

Counts verified from each run's `runs.parquet`/`sources.parquet`/`errors.parquet`
(Task 6); full-corpus wall times verified from Task 8's `real` timings on
each `summarize-warehouse-interesting` invocation.

## 2. λ-missing policy verdict

Spec step 4 originally asked to sweep a fixed `λ_missing ∈ {0, 0.005, 0.010}`
constant. The implementation ships a **policy**, not a bare constant
(`--lambda-missing-policy rank-floor|fixed:<v>`; recorded in `score_version`
as `lambda_missing_policy`) — v1 deviation 11: a fixed λ breaks the
missing-signal monotonicity guarantee past rank `1/λ - k`, so `rank-floor`
(`λ = 1/(k + N_kind + 1)`, "just below the worst-ranked observed pattern of
the kind") is the shipped default. Task 7's sweep compares `rank-floor`
against `fixed:0`, `fixed:0.005`, `fixed:0.010` on the triage-1000 corpus,
preserving the spec's original intent (top-50 stability under missing-data
handling) via a policy A/B rather than a constant sweep.

**Result:** all three fixed-λ variants are stable vs. `rank-floor` — in
fact exactly identical (overlap 50/50, Kendall τ-b 1.0,
`sweep-analysis.md`'s D6 verdict: **stable**).

**Structural-totality finding (verified in code, not just this sample).**
The exact-agreement result is not sample luck — it is structurally
guaranteed for every v1 signal on every pattern kind the ranker currently
produces. Read `raw_signal_value` in
`crates/ab-morph-run/src/summary/interesting.rs:1022-1031`:

```rust
fn raw_signal_value(signal: Signal, stats: &PatternStats, rarity_total: usize) -> Option<f64> {
    match signal {
        Signal::Coverage => Some((1.0 + stats.coverage_region_count as f64).log2()),
        Signal::Rarity => Some(((rarity_total as f64 + 1.0) / (stats.rarity_count as f64 + 1.0)).log2()),
        Signal::Impact => stats.key.feature_key.as_deref().map(impact_weight),
        Signal::Span => Some(stats.span_p90),
    }
}
```

`Coverage`, `Rarity`, and `Span` are `Some` unconditionally for every
pattern. `Impact` is `None` only if `stats.key.feature_key` is `None` — but
`Impact` is applicable only to `PatternKind::Feature` patterns
(`Signal::applicable`, `interesting.rs:383-385`), and every `Feature`-kind
pattern's key is built by `warehouse_feature_pattern_key`
(`summary_body.rs:3442-3468`), which always sets
`feature_key: Some(feature.feature_key.clone())`. So for every kind that
actually occurs in this corpus (`Segmentation` and `Feature`; see the
`Coverage`-kind corollary in §3), every applicable v1 signal is total:
`λ_missing` has no code path to fire in production. The policy sweep's
identical result on triage-1000 is therefore the expected outcome of the
implementation, not an artifact of a thin sample — confirming rather than
contradicting the brief's premise. The λ-policy knob is real forward
infrastructure for v2 optional signals (surprise, era-novelty), which *can*
be legitimately absent per-pattern; it is inert for v1's four signals.

**Lock: `lambda_missing_policy = rank-floor` retained.** Per the plan's
symmetric decision rule (stable ⇒ the choice barely matters, keep the
monotone default; unstable ⇒ fixed λ would be shown to distort, keep
rank-floor), rank-floor wins under both branches — no p@50 information is
needed for this call, and none is used.

## 3. W_COV verdict

Anomaly score: `anomaly_score(r) = W_COV · has_coverage_mismatch(r) +
log2(1 + char_length(r))`. Task 7 swept `anomaly_w_cov ∈ {2, 5, 10}` on
triage-1000; Task 8 extended the check to the full corpus after the triage
sample turned out to contain zero coverage-mismatch regions.

- **Triage-1000:** all 7,889,226 disagreement regions have
  `has_coverage_mismatch = false`. Top-10 anomalies identical across all
  three `W_COV` values (same 10 regions, same order, same scores).
- **Full corpus:** a direct DuckDB scan over `nway_regions.parquet` found
  all **161,142,784** disagreement regions have `has_coverage_mismatch =
  false` — no exceptions. This makes the coverage term of the anomaly
  score provably zero corpus-wide, for the *entire* ranking at any k, not
  just empirically identical over a top-10 sample. The `w_cov=2`/`w_cov=10`
  ranking artifacts could not be produced (5 OOM/earlyoom-killed attempts
  against this box's ~98GB RAM / `earlyoom -m10` / resident vLLM server
  constraints — see `full/wcov-full-analysis.md`'s incident table); their
  contents are supplied analytically from the scan instead of measured,
  since the scan proves they would be byte-identical to the `w_cov=5`
  artifact modulo the `anomaly_w_cov` field itself.

**Corollary (stated plainly):** the coverage-mismatch channel — and, by
extension, `PatternKind::Coverage` patterns (`coverage_pattern_key`,
`interesting.rs:504-530`, which only ever materializes over
coverage-mismatch regions) — never fire on this AAT corpus at all, under
the current four-analyzer set. This is a property of the corpus/analyzer
combination the campaign ran against, not a code defect; the plumbing
(`ab-morph-diff::model::has_coverage_mismatch`, `ab-warehouse::schema`,
`interesting_sql.rs`'s aggregate) is real and exercised elsewhere. A future
analyzer set or diff definition that produces real coverage mismatches
would exercise this channel; the one-line re-check query is preserved in
`full/wcov-full-analysis.md`.

**Lock: `anomaly_w_cov = 5.0` (default) retained; sweep partially
exercised, knob dormant.** Not contradicted by anything measured on this
corpus, but also not positively stress-tested — the value is provably
irrelevant to every ranking this corpus's disagreement data can produce.
Re-validate if/when a corpus or analyzer set surfaces real
`has_coverage_mismatch = true` regions.

## 4. Rank scope: within-kind vs. global — pending labels

| Corpus | Overlap/50 | Jaccard | Kendall τ-b | D6 verdict |
|---|---|---|---|---|
| triage-1000 | 38 | 0.613 | 0.519 | **unstable** |
| full corpus | 45 | 0.818 | 0.772 | **unstable** (materially closer to the D6 bar, but still under τ ≥ 0.9) |

Both samples show real, meaningful disagreement between within-kind and
global signal-rank pooling — τ/overlap alone cannot say *which* scope is
better, only that they differ. **p@50 verdict pending labels**: spec step
3's acceptance test ("if within-kind does not measurably beat global on
p@50/nDCG@50, drop the within-kind split") is the only test that can
decide this, and it requires the owner's blind verdicts.

For reference, the continuity gate (pre-calibration artifact vs. the first
post-calibration full run, same knobs) passed cleanly: `full/cmp-continuity.json`
— overlap 50/50, Kendall τ-b 1.0, empty `score_version_mismatches` — proving
the new knob plumbing did not change default-path scoring.

## 5. Locked-now vs. pending-labels (spec step 9 constants)

| Knob | Status | Value | Basis |
|---|---|---|---|
| `rrf_k` | **locked** (unchanged) | `60` | never swept this campaign; no evidence to move it |
| `lambda_missing_policy` | **locked** | `rank-floor` | §2 — stable-under-sweep AND structurally a no-op for v1 signals; wins under the plan's decision rule regardless |
| `anomaly_w_cov` | **locked** (dormant) | `5.0` | §3 — knob provably inert on this corpus; not contradicted, not stress-tested |
| `rank_scope` | **pending labels** | `within-kind` (current default, unchanged for now) | §4 — unstable A/B at both corpus sizes; spec step 3's p@50 test required |
| `inheritance_jaccard_threshold` | **n/a** | not implemented in v1 | plan D8 (grep-verified: no occurrence in the crate); locking deferred to the feature that introduces it |
| RRF-vs-frequency gate (step 8) | **pending labels** | — | if RRF does not beat frequency sort at p@50, the mandated outcome is "revisit signal definitions," not a lock — see §7 |
| Overall v1 signal-definition lock | **CONDITIONAL — pending labels + step 8 gate** | — | Tasks 1-9 complete every mechanical step up to the gate; the lock itself waits on the owner |

## 6. Labeling handoff

Package: `reports/morph-warehouse/calibration/2026-07-06/labels/` —
`labels.tsv` (155 rows), `mapping.json` (label_id → pattern_id + per-method
ranks, blind), `README.md` (verdict vocabulary, fill instructions, scoring
command). Full instructions live in that README; do not read `mapping.json`
before labeling — it would break the blinding.

**Pool composition:** union of four full-corpus method top-50s
(`full-rrf-within`, `full-rrf-global`, `full-frequency`, `full-random`),
deduped by `pattern_id`, seed-shuffled (seed `20260706`). Pairwise
intersections: within-kind ∩ global = 45; every other pair (vs. frequency,
vs. random) = 0 — frequency and random sorts are disjoint from everything,
including each other. `|union| = 200 - 45 = 155`, matching the TSV row
count and `mapping.json` key count exactly.

**D5 metric definitions — dispute before labeling if you disagree:**

- **p@50** counts a verdict as relevant iff it is `bug` or
  `expected-dictionary`. The ranker's job is surfacing analyzer defects and
  dictionary gaps; `expected-policy` and `corpus-artifact` are known,
  structural, non-actionable categories, not misses.
- **nDCG@50** gains: `bug` = 3, `expected-dictionary` = 2,
  `corpus-artifact` = 1, `expected-policy` = 1, `noise` = 0, `unclear` = 0.
- **IDCG** is computed from the pooled union's best 50 gains (pooled
  evaluation), not each method's own surfaced set — this is what makes
  methods that surface *different* item sets comparable: every method's DCG
  normalizes against the same ideal ordering, so nDCG penalizes a
  low-value surfaced set instead of only rewarding good ordering within it.

These constants live in exactly one place in code (the `score-interesting-labels`
subcommand) and can be recomputed for free from the same `labels.tsv` if the
owner wants different weights — dispute them now, before labeling, or
after, at the cost of a rerun; either way nothing is lost.

## 7. What happens after labels

1. The owner fills `verdict` (and optionally `notes`) on all 155 rows of
   `labels/labels.tsv`, leaving `label_id` and every other column
   untouched.
2. Run:
   ```
   cargo run --release -p ab-morph-run -- score-interesting-labels \
     --labels reports/morph-warehouse/calibration/2026-07-06/labels/labels.tsv \
     --mapping reports/morph-warehouse/calibration/2026-07-06/labels/mapping.json
   ```
   This reports p@50 and nDCG@50 per method (`rrf-within`, `rrf-global`,
   `frequency`, `random`) over the pooled labels.
3. **Spec step 3 decision (rank scope):** compare `rrf-within` vs.
   `rrf-global` p@50/nDCG@50. If within-kind does not measurably beat
   global, drop the within-kind split and set `rank_scope = global` as the
   new default; otherwise keep `within-kind`.
4. **Spec step 8 gate (RRF vs. frequency) — STOP semantics:** compare
   `rrf-within` (or whichever scope step 3 selects) vs. `frequency` at
   p@50. **If RRF does not beat the frequency sort, the mandated outcome is
   "revisit signal definitions" — not a lock.** This is a hard stop, not a
   soft recommendation: the plan's premise is that RRF's added complexity
   (four signals, rank fusion, missing-data handling) must earn its keep
   over the trivial baseline; failing this gate means the signal set itself
   needs rework before anything is locked, and no v1 default in §5's table
   becomes final under that outcome.
5. **If the gate passes:** lock the v1 signal definitions, `rrf_k = 60`,
   `lambda_missing_policy = rank-floor`, `anomaly_w_cov = 5.0`, and the
   step-3 `rank_scope` decision; update the spec's §Calibration Plan status
   line (already appended below, pending-labels flag flipped) and record
   the final p@50/nDCG@50 numbers next to the lock. `inheritance_jaccard_threshold`
   stays `n/a` regardless of the gate outcome — it is not a v1 knob to lock.
