# Phase 3 stage 0 performance gate: end-to-end AAT-production (perf-workset-v1)

Date: 2026-07-11
Authority: `.superpowers/sdd/task-3-brief.md` (Step 6), mirroring Phase 2's
lane setup verbatim (`docs/superpowers/reports/2026-07-10-phase2-perf.md`).
Raw report: `docs/superpowers/reports/2026-07-11-phase3-stage0-perf.runner.json`.
Gate summary: `docs/superpowers/reports/2026-07-11-phase3-stage0-perf.summary.json`.

## Candidate identity

Same detached worktree and rev-injected build as the parity gate (Step 2),
on hinoki (`~/Projects/soranoha/.worktrees/parser-fork-phase3/ab-validator`,
detached at `c3cb16908b0134ca03856735f10445eeb2a92957`):

```
$ ./target/release/ab-aozora --version
ab-aozora 0.1.0 aat-schema 1 facade 0.1.0 wire-schema 2 (git c3cb16908b0134ca03856735f10445eeb2a92957)
$ sha256sum target/release/ab-aozora
9a41cbfb4d6e61c74261501d26f6af34b50cc68f60cc5f5acd000e867affd6a6  target/release/ab-aozora
```

Same identity triple as the parity gate (Step 4) and the conformance gate
(Step 5's hinoki-built binary) — one candidate identity across all three
Stage 0 gates. The runner's own `bins.candidate.id_bin_sha256` /
`id_bin_version` fields independently confirm this triple (see runner
JSON).

## Lane setup

- Frozen adapter build: `cargo build --manifest-path
  adapters/aozora/Cargo.toml --release` (`RUSTC_WRAPPER=`,
  `SCCACHE_DISABLE=1`), sha256
  `de9cfe3ea11b0a8d6e8616bfe52e9bcf3ec158a6049498b027b0a53e5f3391bf`
  (`adapters/aozora/target/release/aozora-adapter`, `aozora-adapter 0.1.0
  aozora unknown`) — same aozora-\* crate family (pinned `=0.4.1` per ADR
  0032) and same sha256 as Phase 2's Task 9 baseline build (byte-identical
  adapter tree, unchanged by Phase 3 stage 0's sanitize swap).
- Pinned upstream: `nix build .#upstream-parser-aozora --no-link
  --print-out-paths` resolved to
  `/nix/store/kps3xj67p1lvjvn55k62z2p2bsifjy8b-upstream-parser-aozora-0.1.0/bin/aozora`
  — same store path as Phase 2's perf gate.
- Perf-workset corpus: `/db/ab-validator/perf-workset-corpus-v1`, reused
  as-is from Phase 2 (already present on hinoki, 6/6 works, no
  re-extraction needed).

## Baseline argv (exact, as run)

```
env AB_AOZORA_BIN=/nix/store/kps3xj67p1lvjvn55k62z2p2bsifjy8b-upstream-parser-aozora-0.1.0/bin/aozora \
  /home/bor/Projects/soranoha/.worktrees/parser-fork-phase3/ab-validator/adapters/aozora/target/release/aozora-adapter \
  --mode aat
```

Candidate argv:

```
/home/bor/Projects/soranoha/.worktrees/parser-fork-phase3/ab-validator/target/release/ab-aozora --mode aat
```

Both lanes were run in the same ssh session on hinoki
(`run-perf-workset.py`, `--runs 5`, per `data/perf-workset.json`'s
protocol block) — same machine identity for both:

```json
{ "node": "hinoki", "machine": "x86_64", "processor": "", "cpu_count": 32 }
```

## Results (median seconds, 5 measured runs each)

| work_id | baseline median_s | candidate median_s | Δ % |
| --- | ---: | ---: | ---: |
| 001529_50685 | 0.7171 | 0.6661 | -7.11% |
| 000311_2012 | 0.6165 | 0.3157 | -48.79% |
| 001562_56146 | 0.9168 | 0.5658 | -38.29% |
| 001562_56145 | 0.7669 | 0.5656 | -26.25% |
| 000363_56656 | 0.3162 | 0.1648 | -47.88% |
| 001562_33224 | 0.4159 | 0.2651 | -36.26% |

## Summary

```json
{
  "baseline_workset_median_s": 0.6668,
  "candidate_workset_median_s": 0.4406,
  "regression_pct": -33.92,
  "threshold_pct": 10,
  "new_timeouts": false,
  "verdict": "PASS"
}
```

**Verdict: PASS.** Zero new timeouts across both lanes; the candidate is
faster on every one of the 6 works, workset-median -33.92% (well past the
10% threshold in the improving direction, consistent with Phase 2's
end-to-end perf gate which showed -35.29% for the same lane pairing).

## Stage 0 tripwire disposition

The brief's expectation for stage 0 — "0 timeouts; median regression ≤ 10%
(stage 0 should be ~0%)" — is satisfied in the strong sense: the candidate
is *faster*, not merely non-regressed. This matches the Phase 2 perf
result's direction and rough magnitude (both lanes measure the same
end-to-end AAT-production path: baseline is `aozora-adapter --mode aat`
shelling out to the pinned upstream `aozora` binary via `AB_AOZORA_BIN`;
candidate is the native in-process `ab-aozora --mode aat` binary with no
subprocess hop). Task 1's sanitize swap (crates.io sanitize → fork
sanitize implementation, zero intended behavior change) does not alter
this call-surface relationship, so the magnitude tracking Phase 2's
closely (-33.92% vs -35.29%) is expected and provides no signal either way
about the sanitize swap's own cost — it is dominated by the pre-existing
subprocess-vs-native gap this lane measures.
