# Sweep analysis — triage-1000 warehouse (2026-07-06)

Task 7 of the calibration plan (`docs/superpowers/plans/2026-07-06-calibration-plan.md`).
Runs the spec's §Calibration Plan steps 3–5 sweeps (λ-missing policy A/B per
D1, anomaly `W_COV` sweep, rank-scope A/B per D2) against the deterministic
1000-source triage warehouse built in Task 6, and records D6 stability
verdicts. This is **not** the v1 lock: rank scope and the RRF-vs-frequency
gate still wait on the owner's blind labels (spec step 8).

- **Run dir:** `/db/ab-validator/morph-warehouse/runs/calib-triage-1000`
  (1000 sources, `rarity_basis=work`)
- **Engine:** `--engine duckdb` explicitly on every `summarize-warehouse-interesting`
  invocation below (`AB_DUCKDB_BIN=/nix/store/yvjqlzi9lfci2l08fws8wc3bhnrps1pa-duckdb-1.5.2/bin/duckdb`),
  so every ranking artifact is comparable — no `in-memory`/`duckdb` engine
  mismatch confound in any comparison.
- **Command shape (rankings):** `ab-morph-run summarize-warehouse-interesting
  --run-dir <triage> --limit 50 --format json --engine duckdb --output
  <artifact>` plus the one varying knob per row below. Default `--anomalies 10`
  throughout.
- **Command shape (comparisons):** `ab-morph-run compare-interesting-rankings
  --left sweeps/triage-rrf-rank-floor.json --right <variant>` → stdout
  redirected to the `cmp-*.json` artifact.

All artifacts live under `reports/morph-warehouse/calibration/2026-07-06/sweeps/`.

## Ranking artifacts + wall times

| Artifact | Varying knob | Wall time (`real`) |
|---|---|---|
| `triage-rrf-rank-floor.json` | defaults (rank-floor, within-kind, w_cov=5) | 30.08s |
| `triage-rrf-fixed0.json` | `--lambda-missing-policy fixed:0` | 30.11s |
| `triage-rrf-fixed0005.json` | `--lambda-missing-policy fixed:0.005` | 30.16s |
| `triage-rrf-fixed001.json` | `--lambda-missing-policy fixed:0.010` | 30.27s |
| `triage-wcov2.json` | `--anomaly-w-cov 2` | 30.33s |
| `triage-wcov5.json` | `--anomaly-w-cov 5` (identical settings to rank-floor default) | 31.81s |
| `triage-wcov10.json` | `--anomaly-w-cov 10` | 30.55s |
| `triage-rrf-global.json` | `--rank-scope global` | 30.08s |

`triage-wcov5.json` is, by construction, the same invocation as
`triage-rrf-rank-floor.json` (both use the defaults `lambda_missing_policy=rank-floor`,
`rank_scope=within-kind`, `anomaly_w_cov=5.0`) — I ran it as its own artifact
anyway, per the brief, for the W_COV series' completeness/naming symmetry
rather than symlinking. Wall times are flat (~30s) across all eight
invocations, as expected since they differ only in a scoring knob, not in
the scan/collection cost; the 31.8s `wcov5` run is within normal noise for
this box (I/O contention with adjacent runs), not a knob effect.

Every artifact's `score_version` block was spot-checked before comparisons
were run: `rows: 50`, `anomalies: 10` in each file, and the swept field(s)
recorded correctly (e.g. `triage-rrf-fixed001.json` → `lambda_missing_policy:
"fixed:0.01"`, `triage-rrf-global.json` → `rank_scope: "global"`, `triage-wcov10.json`
→ `anomaly_w_cov: 10.0`). No parse failures.

## Comparison artifacts: τ / overlap / score_version_mismatches

| Comparison | overlap/50 | Jaccard | Kendall τ-b | score_version_mismatches | D6 verdict |
|---|---|---|---|---|---|
| `cmp-lambda-fixed0.json` (rank-floor vs fixed:0) | 50 | 1.0 | 1.0 | `lambda_missing_policy: "rank-floor" vs "fixed:0"` | **stable** |
| `cmp-lambda-fixed0005.json` (rank-floor vs fixed:0.005) | 50 | 1.0 | 1.0 | `lambda_missing_policy: "rank-floor" vs "fixed:0.005"` | **stable** |
| `cmp-lambda-fixed001.json` (rank-floor vs fixed:0.010) | 50 | 1.0 | 1.0 | `lambda_missing_policy: "rank-floor" vs "fixed:0.01"` | **stable** |
| `cmp-scope-triage.json` (within-kind vs global) | 38 | 0.613 | 0.519 | `rank_scope: "within-kind" vs "global"` | **unstable** |

D6 thresholds (plan): stable ⇔ Kendall τ-b ≥ 0.9 over the rank intersection
**and** overlap ≥ 45/50. All three λ-policy variants clear both bounds
comfortably (in fact exactly — see investigation note below); the rank-scope
A/B clears neither.

Each comparison's `score_version_mismatches` names **exactly** the one
knob that was swept and nothing else — no red flag from that check.

### Investigation note: why are the three λ-policy comparisons *exactly* τ=1.0, overlap=50/50?

This is the shape the brief's STOP condition warns about ("overlap 50/50 AND
tau 1.0 ... something is wrong"), so I checked it before treating it as a
result rather than a bug. The STOP condition is specifically an **empty**
`score_version_mismatches` list for a non-default variant — that's not what
happened here: each comparison's mismatch list correctly names the swept
`lambda_missing_policy` value, proving the knob took effect and was recorded.

The reason the *ranking itself* doesn't move is structural, not a bug: I
checked every one of the 50 rows in `triage-rrf-rank-floor.json` and found
**zero** signals with `status != "present"` across all 200 signal slots
(50 rows × 4 signals). `λ_missing` (rank-floor vs fixed) only ever enters
the fused score for a signal that's *missing* for a given pattern — if no
pattern in contention for the top 50 has a missing signal, the λ policy is
provably a no-op on this window regardless of its value, and rank-floor vs.
any fixed constant must agree exactly. This triage-1000 sample simply
doesn't surface any patterns with a missing applicable signal near the
top of the ranking. Confirmed not a wiring bug; a genuine (if unexciting)
result of this corpus/window.

## Anomaly tables (top 10, per W_COV value)

`has_coverage_mismatch` count and span-length (`char_end - char_start`)
min/median/max over the top-10 anomalies in each `triage-wcov{2,5,10}.json`:

| W_COV | has_coverage_mismatch count (of 10) | span min | span median | span max |
|---|---|---|---|---|
| 2 | 0 | 66 | 66.0 | 66 |
| 5 | 0 | 66 | 66.0 | 66 |
| 10 | 0 | 66 | 66.0 | 66 |

The top-10 anomaly **set and order** are byte-for-byte identical across all
three W_COV values: same 10 `(source_id, text_id, region_index)` triples, same
score (6.0661) each, only two distinct source texts represented
(`000081_455-99788317b142`, `000124_655-e328c5c8c61a`).

### Investigation note: why is W_COV a no-op on this sample's top 10?

`anomaly_channel_in_memory` (`interesting.rs:1189-1226`) computes
`anomaly_score = round6(coverage_term + log2(1 + char_length))`, where
`coverage_term = w_cov` if `has_coverage_mismatch` else `0.0`. So W_COV only
moves the score for regions that actually have a coverage mismatch. I
queried the triage warehouse directly:

```sql
SELECT has_coverage_mismatch, count(*) AS n
FROM read_parquet('.../calib-triage-1000/nway_regions.parquet')
WHERE NOT is_agreement
GROUP BY 1;
-- has_coverage_mismatch=false → 7,889,226 rows. No true rows at all.
```

Every disagreement region in this 1000-source sample has
`has_coverage_mismatch = false`. The column and its plumbing are real and
exercised elsewhere (`ab-morph-diff::model::has_coverage_mismatch`,
`ab-warehouse::schema`, `interesting_sql.rs`'s `coverage_regions` aggregate)
— this isn't a dead/unpopulated feature, it's a property of this particular
triage subset (or possibly of the broader corpus's analyzer set never
disagreeing on byte/char coverage, only on segmentation/feature values).
Consequently the top-10 anomalies in this sample are driven entirely by the
`log2(1 + char_length)` span term, which is W_COV-invariant.

### W_COV reading (human judgment, per the brief)

This is explicitly a qualitative read, not a mechanical threshold pick. The
spec's step-5 acceptance question — "do coverage mismatches and long spans
dominate the top anomalies, or is it noise?" — **cannot be answered from
this triage sample**, because coverage mismatches never occur in it at all;
there is no coverage-vs-span tradeoff to observe here. What the sample does
show: the top-10 anomalies are long, contiguous multi-character spans
(66 chars, i.e. a real disagreement region, not a stray single-character
tokenization blip), concentrated in just two source texts — that looks like
genuine structural disagreement (e.g. a long quotation or list the analyzers
segment very differently), not noise. That's a mild positive signal for
the anomaly channel generally, but it says nothing about the coverage
component specifically.

**Recommendation: keep the default `W_COV = 5.0` for now.** There is no
evidence in this sample to move it in either direction — every value in
{2, 5, 10} produces an identical top-10, so the "choice barely matters" logic
from D1's fixed-λ decision rule applies here too, but for a different reason
(the term is structurally zero everywhere in this data, not merely converged).
This should be re-checked on a corpus slice known to contain coverage
mismatches (or on the full canonical warehouse, which is far larger and more
likely to contain some) before treating `W_COV=5` as validated rather than
merely "not contradicted."

## What locks now vs. what waits for labels

**Locks now:**
- **λ-missing policy stays `rank-floor`.** All three fixed-λ variants are
  stable (in fact identical) vs. rank-floor on this sample. Per the plan's
  decision rule: when all fixed-λ variants are stable, the choice barely
  matters, so keep the monotone default — rank-floor has the unconditional
  missing-signal monotonicity guarantee (spec deviation 11) that a fixed
  constant lacks past rank `1/λ − k`, and nothing here justifies giving that
  up. No p@50 information is needed for this call since the plan's rule
  is symmetric (stable → keep monotone; unstable → keep rank-floor because
  fixed λ would be shown to distort) — rank-floor wins under both branches.
- **`W_COV = 5.0` (default) stands**, on the "not contradicted, but also not
  exercised" basis above. Recommend re-validating once a coverage-mismatch-
  bearing sample is available (full canonical run, or a targeted subset).

**Waits for labels:**
- **Rank scope (within-kind vs. global).** This A/B is *unstable* by D6
  (overlap 38/50, τ=0.519) — the two scopes meaningfully disagree on which
  patterns are in the top 50 and how they're ordered. τ/overlap alone can't
  decide which is *better*; the spec's step-3 p@50 comparison against the
  owner's blind labels is required before locking this knob.
- **Step 8's RRF-vs-frequency gate** (spec step 8: RRF must beat frequency
  sort at p@50, else the mandated outcome is "revisit signal definitions,"
  not a lock) is untouched by this task and waits on labels + Task 9.
