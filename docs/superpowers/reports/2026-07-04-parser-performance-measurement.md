# Parser Performance Measurement

Date: 2026-07-04

## Scope

This report treats parser performance as comparison evidence, not as an
optimization mandate for `aozora2html`. `aozora2html` remains useful as an
oracle/comparator, but routine re-parsing must have a bounded budget. A parser
that does not finish within that budget records a DNF result.

## Inputs

- Refreshed aozora2html full run:
  `/db/ab-validator/aat-corpus/aozora2html-full-20260704T014828Z-300s`
- Full-run timeout: `300s`
- Full-run jobs: `24`
- Timeout workset:
  `/db/ab-validator/aat-corpus/aozora2html-full-20260704T014828Z-300s/worksets/adapter-timeout-300s.json`
- Largest-five performance workset:
  `/db/ab-validator/aat-corpus/aozora2html-full-20260704T014828Z-300s/worksets/adapter-timeout-300s-largest-5.json`
- Corrected cross-parser and stage-split performance output:
  `/db/ab-validator/aat-corpus/aozora2html-full-20260704T014828Z-300s/triage/outputs/parser-performance-largest5-60s-v5`

## Refreshed Full-Run Result

The 300s full run completed 17,886 check reports and emitted 17,767 AAT files.

Compared with the earlier 180s run:

| property | old 180s run | refreshed 300s run |
|---|---:|---:|
| adapter_protocol_error | 1 | 0 |
| adapter_timeout | 196 | 119 |
| parse_completeness | 105 | 105 |
| visible_text_body_order | 668 | 689 |
| gaiji_resolution | 190 | 199 |
| ruby_completeness | 127 | 131 |

Interpretation: increasing the broad-run timeout to 300s recovered 77 works and
removed the single adapter protocol panic, but it exposed additional property
failures because more works reached validation.

## Budgeted Retry Result

A targeted 900s retry was started for the 119 works that timed out at 300s, then
intentionally stopped after the practical retry budget was exceeded. This is
recorded as budgeted DNF evidence, not as an incomplete success run.

| metric | value |
|---|---:|
| timeout workset | 119 |
| reports written before stop | 90 |
| AAT files written before stop | 65 |
| not completed before stop | 30 |
| completed reports still timing out at 900s | 25 |

Among reports written before stop, failures included:

| property | failures |
|---|---:|
| adapter_timeout | 25 |
| visible_text_body_order | 20 |
| gaiji_resolution | 8 |
| ruby_completeness | 6 |
| parse_completeness | 1 |

Decision: for semi-frequent comparison work, the useful signal is bounded DNF.
The remaining tail should not be re-run with unbounded or very large timeouts by
default.

## Cross-Parser Timing Sample

The largest five 300s-timeout works were measured under a 60s per-parser budget
using the generic performance harness. `full_adapter` is the comparable axis.
For `aozora2html`, this is the timed Ruby XHTML generation plus Rust XHTML-to-AAT
mapper pipeline.

| adapter | attempted | ok | timeout | median wall s | max wall s | max RSS KB |
|---|---:|---:|---:|---:|---:|---:|
| aozora-rs | 5 | 5 | 0 | 0.150 | 0.330 | 314,364 |
| aozora-epub3 | 5 | 5 | 0 | 0.850 | 1.010 | 216,776 |
| aozora2html | 5 | 5 | 0 | 6.110 | 28.270 | 298,064 |
| aozora2 | 5 | 3 | 2 | 16.170 | 33.150 | 112,016 |

Per-work highlights:

| work_id | bytes | aozora-rs | aozora-epub3 | aozora2 | aozora2html |
|---|---:|---:|---:|---:|---:|
| 000311_2012 | 1,742,057 | 0.04s | 0.85s | 0.54s | 28.27s |
| 001562_56146 | 1,676,607 | 0.23s | 1.01s | DNF 60s | 21.32s |
| 001562_56145 | 1,433,847 | 0.33s | 1.01s | DNF 60s | 4.91s |
| 001562_33224 | 1,347,264 | 0.15s | 0.85s | 33.15s | 5.78s |
| 001562_57875 | 1,298,242 | 0.12s | 0.81s | 16.17s | 6.11s |

Interpretation: after fixing the Rust mapper's JIS lookup table reload, the
`aozora2html` pipeline completes all five largest timeout-tail files inside the
60s comparison budget. `aozora-rs` remains fastest on this sample, and the
Java-backed `aozora-epub3` adapter is also comfortably inside the frequent
comparison budget. `aozora2` is mixed: it is fast on the largest file, but it
still DNFs on two of the five largest cases within the 60s budget.

## aozora2html Mapper Root Cause

The first stage split showed Ruby XHTML generation completing quickly while the
Rust XHTML-to-AAT mapper DNFed. Profiling the retained largest work
(`000311_2012`) showed the mapper repeatedly rebuilding `jis2ucs.yml` for every
gaiji lookup. The fix caches the parsed JIS-to-Unicode table with `OnceLock`.

This was a measurement correction, not production optimization work: the old
DNF mixed parser cost with an avoidable adapter bug and therefore contaminated
the comparison.

## aozora2html Stage Split

For the largest work (`000311_2012`, 1,742,057 bytes), a supplemental stage split
was run with the same 60s budget:

| stage | result | wall s | max RSS KB |
|---|---|---:|---:|
| full adapter | ok | 28.27 | 85,300 |
| Ruby XHTML generator | ok | 4.77 | 33,012 |
| Rust XHTML mapper | ok | 23.58 | 85,904 |

Interpretation: for this representative largest timeout work, Ruby XHTML
generation is not the bottleneck. The Rust mapper remains the expensive stage,
but after caching the JIS lookup table it completes within the 60s diagnostic
budget.

## Follow-Ups

- Use `reports/aat-fidelity/measure-parser-performance.py` for future parser
  comparison samples instead of ad hoc `time` commands.
- Keep default comparison budgets explicit. Suggested defaults:
  - frequent local comparison: 60s per parser/work,
  - corpus fidelity run: 300s per work,
  - one-off diagnostic retry: 900s maximum, interrupt and record DNF once the
    budget is no longer useful.
- Do not block VTBO or parse-incomplete policy work on exhausting the
  aozora2html timeout tail.
