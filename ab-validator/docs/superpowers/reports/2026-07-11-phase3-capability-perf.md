# Gate: rotation A perf workset (Phase 3 capability, `ab-aozora` C1 vs frozen adapter)

**Date:** 2026-07-11
**Authority:** `.superpowers/sdd/task-10-brief.md`;
`docs/superpowers/reports/2026-07-11-phase3-stage0-perf.md` (stage 0 lane,
model for this report).
**Verdict:** `PASS` — 0 new timeouts, workset median **-39.12%** (candidate
faster), well within the ≤10% regression threshold.

## Candidate identity

Same C1 binary as the delta and conformance gates (identity asserted before
the run; independently recorded in the runner JSON's `bins.candidate`):

- **commit:** `a81edf066ecc0e9ac12e04c4ccc2551c27009161`
- **`--version` (verbatim):** `ab-aozora 0.2.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git a81edf066ecc0e9ac12e04c4ccc2551c27009161)`
- **binary sha256:** `c3cec69c7dd4261bd9068bab1de06666c54584a77c15783ccc17c8cea355e916`

## Lane setup (identical to stage 0)

Run on hinoki (32 cores). Baseline = frozen `aozora-adapter` wrapping the
pinned upstream parser; candidate = the C1 `ab-aozora` binary. 5 runs per
work, 6 works, corpus `/db/ab-validator/perf-workset-corpus-v1` (reused,
no re-extraction).

```bash
cd ~/Projects/soranoha/.worktrees/parser-fork-phase3/ab-validator
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
cargo build --manifest-path adapters/aozora/Cargo.toml --release
AOZORA_PKG=$(nix build .#upstream-parser-aozora --no-link --print-out-paths)
python3 reports/aat-fidelity/run-perf-workset.py \
  --workset data/perf-workset.json \
  --corpus /db/ab-validator/perf-workset-corpus-v1 \
  --baseline-cmd "env AB_AOZORA_BIN=$AOZORA_PKG/bin/aozora $PWD/adapters/aozora/target/release/aozora-adapter --mode aat" \
  --candidate-cmd "$PWD/target/release/ab-aozora --mode aat" \
  --baseline-id-bin adapters/aozora/target/release/aozora-adapter \
  --candidate-id-bin target/release/ab-aozora \
  --runs 5 --out ~/phase3-capability-perf.json
```

Baseline adapter build sha256
`de9cfe3ea11b0a8d6e8616bfe52e9bcf3ec158a6049498b027b0a53e5f3391bf`
(`aozora-adapter 0.1.0 aozora unknown`) — identical to the stage 0 and
Phase 2 baselines. Pinned upstream resolved to
`/nix/store/kps3xj67p1lvjvn55k62z2p2bsifjy8b-upstream-parser-aozora-0.1.0/bin/aozora`.

## Results

Runner summary (`~/phase3-capability-perf.json`, committed verbatim as
`2026-07-11-phase3-capability-perf.runner.json`):

```json
{
  "baseline_workset_median_s": 0.6422,
  "candidate_workset_median_s": 0.391,
  "regression_pct": -39.12,
  "threshold_pct": 10,
  "new_timeouts": false,
  "verdict": "PASS"
}
```

Per-work medians (baseline / candidate seconds, 5 runs each, 0 timeouts
each side on every work):

| work_id | baseline | candidate | Δ% |
|---|---:|---:|---:|
| 001529_50685 | 0.6677 | 0.6169 | -7.61% |
| 000311_2012 | 0.6167 | 0.3160 | -48.76% |
| 001562_56146 | 0.8679 | 0.5163 | -40.51% |
| 001562_56145 | 0.7165 | 0.4659 | -34.98% |
| 000363_56656 | 0.3159 | 0.1652 | -47.70% |
| 001562_33224 | 0.4168 | 0.2657 | -36.25% |

## Verdict

**`PASS`** — 0 new timeouts; candidate faster on all 6 works; workset
median -39.12% (improvement), consistent with the brief's expectation that
the rotation A classifier branches (per-marker string compares) are
noise-level. Gate summary:
`docs/superpowers/reports/2026-07-11-phase3-capability-perf.summary.json`.
