# Annotation join overlap benchmark (2026-07-11)

The binary-search overlap lookup was measured against the previous exhaustive
`filterv` implementation using real parser-IR and UniDic novel token outputs
from retained run
`aozora-repin-1a4f864-stride44-unidic-novel-2026-07-10`.

The benchmark renders annotations from parser-IR, reconstructs spans for these
legacy surface-only token files, and refuses to run on a reconstruction
mismatch. For each work, the exhaustive and optimized full join results were
equal before timing results were accepted.

| work | selection | tokens | annotations | exhaustive | optimized median (5) | speedup |
|---|---|---:|---:|---:|---:|---:|
| `001562_33224` | largest retained token stream | 388,148 | 14,663 | 701,665.60 ms | 89.27 ms | 7,860.45× |
| `000077_1323` | highest density with ≥10,000 tokens and ≥1,000 annotations | 90,512 | 32,377 | 354,394.18 ms | 69.86 ms | 5,073.24× |

Optimized samples were:

- `001562_33224`: 95.86, 89.27, 88.47, 91.67, 88.60 ms.
- `000077_1323`: 73.40, 69.83, 69.86, 69.73, 69.86 ms.

All five optimized samples were recorded after an unmeasured warm-up call.
The benchmark exits nonzero when a supplied workload measures below the 10×
acceptance floor.

## Work selection

The largest stream was selected by byte-sorting token files, then confirmed
by `wc -l`. The density candidate was selected reproducibly from every row in
`stats/per-work.jsonl`: sum `annotation_counts`, count the matching token-file
lines, retain rows with at least 10,000 tokens and 1,000 annotations, compute
`annotations / tokens`, and sort descending. `000077_1323` ranks first at
0.357709475.

The literal unconstrained density maximum, `001475_51115`, has only 92 tokens
and 33 annotations. A review-triggered boundary run measured 3.84×: fixed
validation and timing overhead dominate at that size. It is excluded by the
predeclared benchmark-scale thresholds because it does not represent the
corpus hotspot; the result is recorded here rather than discarded.

The original protocol requested five exhaustive repetitions. It was stopped
after more than 17 minutes without completing the largest work. The bounded
protocol uses the equality-checking exhaustive pass as the single baseline
and five optimized measurements. This is sufficient for the 10× acceptance
floor: observed speedups exceed it by roughly three orders of magnitude.

The benchmark command is
`clojure -M:test -m abc.tools.annotation-join-benchmark`, followed by at least
two `PARSER_IR TOKENS` path pairs. Input paths are operator arguments; no
machine-local database path is active configuration.
