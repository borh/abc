# Phase 3 rotation B — perf gate (PASS)

Stage: `rotation-b` · Gate: `perf` · Verdict: **PASS**
Machine summary: `2026-07-11-phase3-span-perf.summary.json` · Raw runner
output: `2026-07-11-phase3-span-perf.runner.json`

## Candidate identity (C2)

- commit: `a3f91f53fcae9bc18f577ea5b746f3be7f228fcb`
- bin_sha256: `9a94dcc8daf9c38a74611667f316e0e024a999d9f41ec41a3721d493dae277dc`
- version: `ab-aozora 0.3.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git a3f91f53fcae9bc18f577ea5b746f3be7f228fcb)`

Runner JSON `bins.candidate.{id_bin_sha256,id_bin_version}` match the
triple exactly (asserted in the evidence-generation script, not
eyeballed).

## Lane (identical to stage 0 / rotation A)

Frozen `aozora-adapter` baseline, sha256
`de9cfe3ea11b0a8d6e8616bfe52e9bcf3ec158a6049498b027b0a53e5f3391bf`
(`aozora-adapter 0.1.0 aozora unknown`), pinned upstream
`/nix/store/kps3xj67p1lvjvn55k62z2p2bsifjy8b-upstream-parser-aozora-0.1.0`,
corpus `/db/ab-validator/perf-workset-corpus-v1` (6 works), 5 runs per
work, hinoki (32 cores).

## Result

```json
{"baseline_workset_median_s": 0.6421, "candidate_workset_median_s": 0.6159,
 "regression_pct": -4.09, "threshold_pct": 10, "new_timeouts": false, "verdict": "PASS"}
```

**0 new timeouts** (0 on every run of every work, both sides).
Workset median delta **-4.09%** (candidate still faster than the legacy
baseline), within the ≤10% regression threshold.

Per-work medians (baseline / candidate s):

| work_id | baseline | candidate | Δ% |
|---|---:|---:|---:|
| 001529_50685 | 0.6674 | 0.7663 | +14.82% |
| 000311_2012 | 0.6169 | 0.3661 | -40.65% |
| 001562_56146 | 0.8671 | 0.8667 | -0.05% |
| 001562_56145 | 0.7169 | 0.9662 | +34.77% |
| 000363_56656 | 0.3162 | 0.1654 | -47.69% |
| 001562_33224 | 0.4165 | 0.4655 | +11.76% |

## Analysis: the span-composition cost is real but within budget

Rotation A's workset median was **-39.12%**; rotation B lands at
**-4.09%**. The ~35-point swing is the per-span decoded-source
composition introduced by Task 13/14: every emitted span now runs two
`OffsetMap` queries (start + end) plus a `line_starts` binary search for
each of `line_start`/`line_end`, and `line_starts` itself is built per
document. Three of six works individually regressed vs the legacy
baseline (worst +34.77% on 001562_56145), three improved strongly —
markup-dense works pay per-span costs, plain-text-heavy works keep the
fork's decode/parse advantage.

The gate criterion is the workset median (≤10% vs the legacy frozen
baseline) and it passes with margin, but this is the phase's thinnest
perf margin; flagged for Phase 4, which should watch this number before
adding further per-span work.
