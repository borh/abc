# Phase 1 performance gate: baseline vs shim (perf-workset-v1)

Date: 2026-07-10
Authority: `docs/superpowers/plans/2026-07-10-consolidated-parser-phase0-1.md`
Task 8. Raw report: `docs/superpowers/reports/2026-07-10-fork-parity-perf.json`.

## Workset

`data/perf-workset.json`, `workset_id: perf-workset-v1`, pinned against
`aozorabunko@0e9ea3e586eb0aa34039fabfc85a407d2f98b165` (matches
`flake.nix`'s `aozorabunko-src` input, confirmed against `flake.lock`).
Corpus store path at pin time:
`/nix/store/sdr1imwrxfldvlwzs2d2fhs11vxncgpx-aozorabunko-corpus`.

The 6 works are the same works selected by
`docs/superpowers/reports/2026-07-08-parser-performance-sample.json`
(the largest works from an earlier 20-work sample, biased toward the
slow end for the aozora2/aozora2html oracle comparison). Each work's
raw text was extracted from its zip archive entry in the corpus, hashed
(sha256), and pinned; `run-perf-workset.py` re-verifies the hash
fail-closed before every gate run and refuses to run on drift.

| work_id | corpus_relpath | archive (relative to corpus) | zip entry | source_sha256 |
| --- | --- | --- | --- | --- |
| 001529_50685 | 001529_50685.txt | cards/001529/files/50685_ruby_67979.zip | ooka_seidan.txt | `9084cc526e4c87705c5f36dd3e09e89987afde1814c4ab8d2ef13ae3889a124e` |
| 000311_2012 | 000311_2012.txt | cards/000311/files/2012_ruby_7800.zip | dohyo.txt | `7621a8bbbe0d146fc8380651913ed211cdd6f40e599ad00227a72ff915cf09c0` |
| 001562_56146 | 001562_56146.txt | cards/001562/files/56146_ruby_66665.zip | shin_suikoden.txt | `f5d9ddda152ffd75c32b9339d54aae335e92db3dcb017d9013e9cef2c14f5597` |
| 001562_56145 | 001562_56145.txt | cards/001562/files/56145_ruby_63231.zip | shinshu_tenmakyo.txt | `f6ef2ad5d5043c3da4ef7b30aba47466400f47d7c4d24640f83da086e8ea0b6e` |
| 000363_56656 | 000363_56656.txt | cards/000363/files/56656_ruby_74439.zip | tsumito_batsu.txt | `388819e28691b858e6503e5502ca1645d2d42f3c74dbdb10b4a3d5bbd6349591` |
| 001562_33224 | 001562_33224.txt | cards/001562/files/33224_ruby_73833.zip | shinran.txt | `15d95168749622b0ac5a832856d4926c4afdb249fd761a634b041d0f4b097bd3` |

The extracted raw texts live under the gitignored `scratch/` directory
(`scratch/perf-workset-corpus/<work_id>.txt`) and are not committed — the
committed artifact is the hash pin, which `run-perf-workset.py` verifies
against whatever `--corpus` directory is supplied at run time. To
reproduce the extraction: unzip each `zip_entry` from `archive` (paths
above, relative to the corpus store path) into a flat file named
`<work_id>.txt`.

Protocol (`data/perf-workset.json` → `protocol`): release build, sccache
disabled, 1 warm-up + 5 measured runs per work, 90s per-work timeout,
10% median-regression block threshold, new timeouts are an unconditional
blocker regardless of the threshold.

## Machine identity

- host: `hinoki`
- CPU: AMD Ryzen 9 9950X 16-Core Processor (`x86_64`)
- cores: 32 (`os.cpu_count()`)
- kernel: `Linux hinoki 6.18.38 #1-NixOS SMP PREEMPT_DYNAMIC`

## Binaries under test

| role | path | `--version` |
| --- | --- | --- |
| baseline | `/nix/store/kps3xj67p1lvjvn55k62z2p2bsifjy8b-upstream-parser-aozora-0.1.0/bin/aozora` (`nix build .#upstream-parser-aozora`) | `aozora 0.4.1` |
| candidate | `target/release/ab-aozora-cli` (`cargo build -p ab-aozora-cli --release`, sccache disabled) | `ab-aozora-cli 0.1.0 (fork of P4suta/aozora @ 1a4f864, ADR 0031)` |

Both invoked as `<bin> inspect nodes -` over stdin per work.

## Results (median ± population stdev, seconds; 5 measured runs each)

| work_id | baseline median_s | baseline stdev_s | candidate median_s | candidate stdev_s | timeouts (base/cand) |
| --- | ---: | ---: | ---: | ---: | :---: |
| 001529_50685 | 0.1157 | 0.0003 | 0.1153 | 0.0002 | 0/0 |
| 000311_2012 | 0.3157 | 0.0001 | 0.3153 | 0.0002 | 0/0 |
| 001562_56146 | 0.3657 | 0.0001 | 0.3654 | 0.0002 | 0/0 |
| 001562_56145 | 0.2652 | 0.0003 | 0.2649 | 0.0002 | 0/0 |
| 000363_56656 | 0.1650 | 0.0002 | 0.1650 | 0.0002 | 0/0 |
| 001562_33224 | 0.1649 | 0.0002 | 0.1648 | 0.0001 | 0/0 |

## Summary

```json
{
  "baseline_workset_median_s": 0.2151,
  "candidate_workset_median_s": 0.215,
  "regression_pct": -0.07,
  "threshold_pct": 10,
  "new_timeouts": false,
  "verdict": "PASS"
}
```

**Verdict: PASS.** Baseline and candidate are the same lifted code
(verbatim rename-only lift, Task 3) invoked through a thin CLI shim
(Task 5/6), so a regression near 0% (here, a very slight, well-within-noise
*improvement*: -0.07%) is exactly the expected result. No new timeouts.
No BLOCK investigation was required.
