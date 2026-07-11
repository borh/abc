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
| `001562_33224` | largest retained token stream | 388,148 | 14,663 | 690,658.46 ms | 79.08 ms | 8,733.61× |
| `000077_1323` | highest annotation density among nontrivial large works | 90,512 | 32,377 | 359,493.77 ms | 72.05 ms | 4,989.76× |

Optimized samples were:

- `001562_33224`: 124.68, 87.87, 79.08, 54.35, 53.77 ms.
- `000077_1323`: 72.05, 72.04, 71.54, 72.26, 75.03 ms.

The original protocol requested five exhaustive repetitions. It was stopped
after more than 17 minutes without completing the largest work. The bounded
protocol uses the equality-checking exhaustive pass as the single baseline
and five optimized measurements. This is sufficient for the 10× acceptance
floor: observed speedups exceed it by roughly three orders of magnitude.

The benchmark command is
`clojure -M:test -m abc.tools.annotation-join-benchmark`, followed by at least
two `PARSER_IR TOKENS` path pairs. Input paths are operator arguments; no
machine-local database path is active configuration.
