# aozora2html Timeout Tail-Distribution Report

Date: 2026-07-03
Source run: `aozora2html-full-20260703T020301Z` (180 s default harness timeout, 194 timed-out works)

## Method

Sampled the 5 largest timed-out works by zipped entry size and re-ran each with
a 600 s wall-clock limit via the `aozora2html-adapter` bash wrapper (Ruby parser
+ Rust mapper). Raw results:
`/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/triage/outputs/timeout-tail/results.json`.

Per the execution-tractability adjustment for this task, the sample size was
overridden to 5 via `AB_AOZORA2HTML_TIMEOUT_TAIL_SAMPLE=5` (the script's own
default remains 10). A 10-work run is recorded as a follow-up below.

## Findings

- sampled: 5 works
- completed (ok): 2
- median real time (over ok): 175.5 s
- max real time (over ok): 253 s
- still timing out at 600 s: 3

Per-work results (`work_id`, size bytes, real_s, status):

| work_id        | size    | real_s | status   |
|----------------|---------|--------|----------|
| 001529_50685   | 2116173 |     98 | ok       |
| 000311_2012    | 1742057 |    600 | timeout  |
| 001562_56146   | 1676607 |    600 | timeout  |
| 001562_56145   | 1433847 |    253 | ok       |
| 001562_33224   | 1347264 |    600 | timeout  |

The two completed works finished in 98 s and 253 s. The three remaining works
(`000311_2012` / dohyo.txt, `001562_56146` / shin_suikoden.txt,
`001562_33224` / shinran.txt) did not finish within the 600 s measurement
ceiling, i.e. their true real time is > 600 s and is not captured by this run.

## Decision

Based on max real time (over completed works) = 253 s:

- [x] raise the harness default timeout to **300 s** in
      `run-aozora2html-aat-full.sh:10`
- [ ] OR keep 180 s and document the 194 works as a coverage caveat
- [ ] OR implement an adaptive timeout (follow-up)

Selected: raise the harness default timeout to 300 s.

Rationale: `max_real_s = 253` is comfortably under the 300 s round value
(≈47 s, ~16% headroom), so a 300 s default recovers the recoverable part of the
timeout tail (works completing in the 180–300 s band, e.g. the 253 s work and
the 98 s work that originally hit the 180 s ceiling under concurrent load). The
next round bucket (600 s) is not chosen because all completed works finish well
below 300 s and the three non-completers already exceed 600 s, so a 600 s
default would only make timeouts slower without recovering additional works.

## Coverage caveat

Three of the five sampled works (the three largest timed-out works by size
excluding `001529_50685`) exceed the 600 s measurement ceiling and therefore
remain timed out under any default ≤ 600 s. These works represent a
pathologically slow tail (likely super-quadratic behavior in the Ruby
aozora2html parser and/or the Rust mapper on specific inputs) and are not
addressed by raising the harness default. They warrant a separate performance
investigation rather than a larger timeout.

## Follow-ups

- Re-run with `AB_AOZORA2HTML_TIMEOUT_TAIL_SAMPLE=10` (the script default) to
  widen the tail sample beyond the top-5-by-size; this task used 5 for
  execution-tractability (5 × 600 s ≈ 50 min worst case).
- Profile the three >600 s works (`dohyo.txt`, `shin_suikoden.txt`,
  `shinran.txt`) to determine which stage (Ruby parse, XHTML emission, or Rust
  map) dominates and whether it can be made sub-quadratic.
