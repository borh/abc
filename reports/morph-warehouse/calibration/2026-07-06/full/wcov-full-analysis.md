# W_COV full-corpus analysis — partial exercise (2026-07-06)

Task 8 added-scope probe: the triage-1000 W_COV sweep (see
`../sweep-analysis.md`) was unexercised because that subset contains zero
coverage-mismatch regions; this analysis asks whether the FULL canonical
warehouse (`full-2026-07-05_164518-jobs0`, 17,885 sources) exercises the
anomaly channel's coverage term (`anomaly_score = W_COV ·
has_coverage_mismatch + log2(1 + span_chars)`).

**Verdict up front: W_COV = 5.0 retained; sweep partially exercised —
full-corpus anomaly channel measured at the default only.** The w_cov=2 and
w_cov=10 ranking artifacts could not be produced (operational failures
below), but a direct data-layer scan makes the missing artifacts' contents
provable rather than merely presumed.

## Data-layer finding (definitive, measured)

Direct DuckDB scan over the full warehouse's `nway_regions.parquet`:

```sql
SELECT has_coverage_mismatch, is_agreement, count(*) AS n
FROM read_parquet('.../full-2026-07-05_164518-jobs0/nway_regions.parquet/*.parquet')
GROUP BY 1,2;
-- has_coverage_mismatch=false, is_agreement=false → 161,142,784 rows.
-- No has_coverage_mismatch=true rows exist AT ALL.
```

Every one of the 161,142,784 disagreement regions in the full corpus has
`has_coverage_mismatch = false`. The coverage term of the anomaly score is
therefore structurally zero corpus-wide: for EVERY region,
`anomaly_score = log2(1 + span_chars)` exactly, independent of W_COV. This
extends the triage-1000 finding (7,889,226 regions, all false) to the whole
corpus — it was not a sampling artifact of the subset.

## Top-10 anomaly table (w_cov=5 measured; 2/10 analytic)

The w_cov=5.0 column is measured from `full-rrf-within.json` (defaults
run). The w_cov=2/10 columns are **analytic, not measured**: because the
coverage flag is false for every region (scan above), the per-region score
`w_cov·0 + log2(1+span)` is w_cov-invariant, so the top-10 set, order, and
scores are provably identical for any w_cov value:

| W_COV | has_coverage_mismatch (of 10) | span min | span median | span max | basis |
|---|---|---|---|---|---|
| 2 | 0 | 66 | 90.0 | 200 | analytic (identical to w_cov=5 by the scan) |
| 5 | 0 | 66 | 90.0 | 200 | measured (`full-rrf-within.json`) |
| 10 | 0 | 66 | 90.0 | 200 | analytic (identical to w_cov=5 by the scan) |

Set/order across w_cov values: **provably identical** (stronger than the
triage result, which showed identity only empirically over the top 10 —
here the invariance holds for the entire ranking at any k).

Measured w_cov=5 top 10 (from `full-rrf-within.json`), for the record:

| source_id | region | span | score |
|---|---|---|---|
| 000160_2714-28fec5978fd6 | 2393 | 200 | 7.651052 |
| 000160_2714-f5c40416f6ec | 2385 | 200 | 7.651052 |
| 000034_55507-30a9658433b4 | 18024 | 142 | 7.159871 |
| 000081_43733-4063e7c46297 | 555 | 90 | 6.507795 |
| 000081_43733-4063e7c46297 | 590 | 90 | 6.507795 |
| 001095_42986-66260bbf3b03 | 10042 | 90 | 6.507795 |
| 000124_656-fad42d670620 | 263 | 71 | 6.169925 |
| 000081_455-99788317b142 | 122 | 66 | 6.066089 |
| 000081_455-99788317b142 | 3056 | 66 | 6.066089 |
| 000081_455-99788317b142 | 3171 | 66 | 6.066089 |

7 distinct sources; all long contiguous spans (66-200 chars), zero coverage
mismatches. Note the top pair is the same work in two editions
(000160_2714-*), disagreeing on the same ~200-char region — consistent with
genuine structural disagreement (long quotation/list segmentation), not
noise. That reading matches the triage sample's character.

## Reading and lock

The spec's step-5 acceptance question ("do coverage mismatches and long
spans dominate the top anomalies, or is it noise?") decomposes as: long
spans dominate — measurably; coverage mismatches CANNOT dominate — they do
not exist anywhere in the corpus at the current analyzer set. W_COV is
unexercisable corpus-wide at current data: the knob is dormant, its value
provably irrelevant to every ranking the current warehouse can produce.

**Lock: `W_COV = 5.0` (default) retained; knob dormant.** Sweep partially
exercised — full-corpus anomaly channel measured at the default only, with
the 2/10 variants supplied analytically from the data-layer scan. Re-run a
real sweep only if/when an analyzer set or diff definition appears that can
actually produce `has_coverage_mismatch = true` regions (worth a one-line
check on any future warehouse import: the GROUP BY scan above).

## Why the w_cov=2/10 artifacts are absent (operational history)

Producing a wcov artifact requires the anomaly-bearing summarize path,
whose two follow-up COPYs (anomaly channel, anomaly feature exclusion)
carry an unspillable in-DuckDB peak of roughly 24-26 GiB (largely
thread-count-invariant; the feature-exclusion `region_sig` list/struct
aggregation over 161M regions cannot spill), while the box (98GB RAM, zero
swap, `earlyoom -m10`, plus a resident ~12-14GB vLLM server with load
transients) SIGTERMs any process once MemAvailable drops under ~8.75 GiB.
The success window (pool ≥ ~28GB AND RSS below the fluctuating earlyoom
ceiling) is narrow and non-deterministic; the defaults run threaded it once
(28GB/4t, 18m06s), and five wcov attempts failed on either side of it:

| attempt | budget | outcome |
|---|---|---|
| 1 | 28GB / 4 threads | in-DB OOM, anomaly channel (26.0 GiB wall), 18m50s |
| 2 | 36GB / 4 threads | earlyoom SIGTERM (~42.6 GiB RSS), anomaly feature exclusion |
| 3 | 20GB / 4 threads | in-DB OOM, anomaly feature exclusion (18.6 GiB wall), 12m40s |
| 4 | 24GB / 2 threads | in-DB OOM, anomaly feature exclusion (22.3 GiB wall), 20m04s |
| 5 | 30GB / 2 threads | earlyoom SIGTERM at 19:46:06 with duckdb at only 32.5 GiB RSS — external (vLLM) pressure shrank MemAvailable under the watermark; 22m11s |

Given the data-layer scan proves the artifacts would be byte-identical to
`full-rrf-within.json`'s anomalies (modulo the `anomaly_w_cov` field in the
`score_version` block), further retries spend ~20 min each to demonstrate a
foregone conclusion and were stopped per the campaign decision rule.

Operational guidance for reruns on this box: rows-only rankings
(`--anomalies 0`) are cheap and safe (`AB_DUCKDB_MEMORY_LIMIT=28GB`, 8
threads, ~7m15s); anomaly-bearing runs need `AB_DUCKDB_MEMORY_LIMIT=28-30GB`
with 2-4 threads AND a quiet box (MemAvailable ≥ ~55 GiB sustained), and
should be treated as best-effort under memory competition.
