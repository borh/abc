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
- Cross-parser performance output:
  `/db/ab-validator/aat-corpus/aozora2html-full-20260704T014828Z-300s/triage/outputs/parser-performance-largest5-60s-v2`
- Stage-split performance output:
  `/db/ab-validator/aat-corpus/aozora2html-full-20260704T014828Z-300s/triage/outputs/parser-performance-stage-top1-60s-v2`

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

| adapter | attempted | ok | timeout | median wall s | max wall s | max RSS KB |
|---|---:|---:|---:|---:|---:|---:|
| aozora-rs | 5 | 5 | 0 | 0.150 | 0.340 | 313,832 |
| aozora2 | 5 | 3 | 2 | 16.320 | 33.630 | 112,148 |
| aozora2html | 5 | 0 | 5 | DNF | DNF | 178,684 |

Per-work highlights:

| work_id | bytes | aozora-rs | aozora2 | aozora2html |
|---|---:|---:|---:|---:|
| 000311_2012 | 1,742,057 | 0.04s | 0.53s | DNF 60s |
| 001562_56146 | 1,676,607 | 0.24s | DNF 60s | DNF 60s |
| 001562_56145 | 1,433,847 | 0.34s | DNF 60s | DNF 60s |
| 001562_33224 | 1,347,264 | 0.15s | 33.63s | DNF 60s |
| 001562_57875 | 1,298,242 | 0.11s | 16.32s | DNF 60s |

Interpretation: `aozora-rs` is the only local parser path that remains
comfortably viable on this large-file tail under a frequent-comparison budget.
`aozora2` is mixed: much faster than `aozora2html` on some files, but it also
DNFs on two of the five largest cases. `aozora2html` DNFs on all five within the
60s comparison budget and previously timed out on the same works at the 300s
full-run budget.

## aozora2html Stage Split

For the largest work (`000311_2012`, 1,742,057 bytes), a supplemental stage split
was run with the same 60s budget:

| stage | result | wall s | max RSS KB |
|---|---|---:|---:|
| full adapter | DNF | 60.00 | 50,616 |
| Ruby XHTML generator | ok | 4.78 | 32,484 |
| Rust XHTML mapper | DNF | 60.00 | 50,412 |

Interpretation: for this representative largest timeout work, the observed
aozora2html bottleneck is not Ruby XHTML generation; it is the Rust XHTML-to-AAT
mapper path. This is guidance for future Rust parser design and oracle-cost
planning, not an instruction to optimize aozora2html as a production parser.

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
