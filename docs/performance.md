# Measuring publication performance

Run source-history replay with the same pinned runtime and parser executables as
the publisher:

```sh
nix run .#soranoha-replay -- \
  --repo /absolute/path/to/aozorabunko \
  --from START_COMMIT --to END_COMMIT \
  --out /absolute/path/to/new-measurement-directory \
  --concurrency 16
```

The range is inclusive and follows the end revision's first-parent history. The
command copies the source repository into its own checkout, starts with an empty
computation cache, and builds every selected work at each revision. It never
checks out or updates a branch in the supplied repository. The output directory
must not exist; results and diagnostics remain there after success or failure.
Use `--limit N` only for an explicitly labelled partial-corpus experiment.

`measurements.jsonl` records repository setup time, checkout time, build time,
per-stage execution counts, and source/artifact delta counts. Every revision is
then built again unchanged. The command fails if that repeat executes any stage,
changes an artifact, or if the existing delta oracle finds unexplained execution
between revisions. Detailed build reports remain under `build/runs/`.

These are **build timings**, using all ordinary build stages, including source
fidelity evidence. They exclude JVM startup, live assessment, publication
transactions and serving export. An empty computation cache does not imply empty
OS page caches or an absent Nix closure. Record those conditions and the machine
alongside results; do not call this an end-to-end publication benchmark.

Historical source revisions alone cannot reconstruct historical website
observations. Publication replay needs the corresponding assessment inputs and
recorded external responses, with an isolated chain and test signing keys. Keep
those measurements separate from live publication freshness checks.

For an optimization comparison, use the same revisions, concurrency and cache
conditions, interleave baseline/candidate runs, and compare per-pair differences.
Artifact equivalence and the delta oracle are correctness checks, not timing
thresholds. A single replay establishes a baseline, not a measured speedup.
