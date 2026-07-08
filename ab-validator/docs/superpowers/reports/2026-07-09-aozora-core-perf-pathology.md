# aozora-core (aozora2) performance pathology — pinpointed

**Date:** 2026-07-09
**Extends:** `2026-07-08-aozora-parser-comparison-study.md` §4.6 (performance), §4.8
(fidelity/robustness split), §5 threat #5; open-work item §3 of
`2026-07-09-parser-comparison-followups-handoff.md`.
**Question (§3).** §4.6 found aozora-core catastrophically slow (16.9 s median, 2/6
timeouts) on a 6-work sample. Is it **slow everywhere** (a uniform constant that would
disqualify it) or **catastrophic only on work-specific inputs** (which §4.8 suspected —
the ruby-heavy works it fails on)? The answer decides whether the pathology is a cheap
fix that re-admits aozora-core to contention.

## Experiment

A controlled contrast designed to separate *size* from *input structure*, run on
aozora-core (`adapters/aozora2`, `aozora2-adapter --mode aat`) alone:

- **Giants** — the **30 works aozora2 fails to complete at full-corpus scale** (the
  robustness deficit from §4.8: 30 missing works holding 13.58% of corpus ruby mass).
- **Controls** — the **30 largest works aozora2 *did* complete**. By construction the
  controls' median size (845 KB) **exceeds** the giants' (537 KB), so if controls run
  fast while giants blow up, size is excluded as the cause.
- 180 s per-work wall limit; GNU `time` for wall/RSS; pinned corpus `aozorabunko@0e9ea3e`.
- Tooling: `reports/aat-fidelity/measure-parser-performance.py` (single `--adapter`),
  analysis `reports/aat-fidelity/analyze-aozora2-giants-perf.py`.
  Output: `/db/ab-validator/parser-performance/aozora2-giants-vs-controls-*/results.json`.

## Result

| cohort | n | completed | timeout (180 s) | errors | median wall | max wall | median ruby/KB |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| **Controls** (larger works) | 30 | **30** | 0 | 0 | **1.25 s** | 16.04 s | 2.44 |
| **Giants** (ruby-heavy) | 30 | 18 | **12** | 0 | **34.68 s** | 173.36 s | **21.42** |

**Verdict: the pathology is WORK-SPECIFIC, not size-driven — and it is ruby-density
driven.**

- **Size is excluded.** The controls are *larger* (median 845 KB vs 537 KB) yet every
  one finished, median 1.25 s, max 16 s. The giants — smaller on average — take 22–173 s
  and time out 12/30. A 148 KB giant (`001310_55609`) takes 35 s while 1.7 MB controls
  finish in ≤16 s: wall time does not track bytes.
- **Ruby density is the discriminator.** Giants carry a **median 21.42 ruby/KB**
  (13,351 ruby/work); controls **2.44 ruby/KB** (2,420/work) — a ~9× gap. This confirms
  §4.8's hypothesis directly: *the robustness gap and the perf pathology are the same
  defect* — ruby-heavy works.
- **Pure time blowup, no crashes (0 errors).** Every failure is a timeout, never an
  OOM or panic. Combined with the ruby-density correlation and superlinear wall growth
  (a 148 KB / very-ruby-dense work costs 35 s), this points at a **superlinear
  (likely O(n²)) algorithm in aozora2's ruby handling**, not a hard limit or a bug.

## Implications

- **§4.6 generalized.** The earlier 6-work "16.9 s median, 2/6 timeouts" was not a
  uniform slowness — it was the sample catching a few ruby-dense works. On ordinary
  (even large) works aozora-core is ~1 s. Its corpus-scale failure is concentrated in a
  ruby-dense tail (~30 works / 0.17% of the corpus, but 13.58% of ruby mass).
- **Fixability: plausible but unproven.** Because the blowup is algorithmic
  (superlinear, ruby-correlated, no crash) rather than a fundamental limitation, it is a
  *candidate* for a cheap fix — but confirming that needs a profiling pass on one giant
  (e.g. `001562_56145`, 173 s) against the `takahashim/aozora2` source to localize the
  quadratic step. That is the concrete next step if aozora-core is ever reconsidered.
- **Verdict unchanged.** Even a fixed aozora-core still trails on corpus coverage
  (0.855, §4.7/§4.8) and conformance, so this does **not** revise the study's
  recommendation of `aozora-pipeline`. It removes the *ambiguity* in §5 threat #5:
  aozora-core is "occasionally catastrophic on ruby-dense inputs," definitively — not
  "slow everywhere."

## Caveats

- One machine; wall time is sensitive to load. The ≥20× gap between cohorts is far
  larger than any plausible noise, so the qualitative verdict is robust; absolute
  seconds are indicative.
- 180 s limit truncates the 12 timeout giants — their true wall is unbounded (≥180 s);
  the median/max wall figures are over completed works only.
- Ruby-density is the dominant correlate, not a perfect threshold (a few giants have
  low ruby/KB yet still time out) — some inputs likely combine ruby with another
  superlinear trigger (deeply nested or adjacent ruby). Profiling would resolve this.
