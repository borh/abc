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

Compare two installed publisher builds on an isolated, already-exported chain:

```sh
nix run .#soranoha-compare-serving -- \
  --baseline /nix/store/BASELINE-soranoha-kernel \
  --candidate /nix/store/CANDIDATE-soranoha-kernel \
  --chain-clone /absolute/path/to/isolated-chain \
  --serve-root /absolute/path/to/isolated-serve \
  --release-pub /absolute/path/to/release.pub \
  --governance-pub /absolute/path/to/governance.pub \
  --out /absolute/path/to/new-comparison-directory
```

The command runs fresh JVMs in ABBA/BAAB order and records elapsed, user and system
CPU time, peak RSS in KiB, stdout and stderr. Each activation must reuse the
existing export, and all eight verification results must agree. It requires only
public keys. Use a chain whose origin is also isolated: activation fetches the
configured origin. These timings include JVM startup, full chain verification,
and checking the exported tree; they do not measure release assembly.

On Speely's isolated four-release, 72,749-blob chain, with Charred 1.042 in both
builds, bypassing per-string buffered-writer construction yielded a median paired
elapsed reduction of 12.8% (four pairs; 95% percentile bootstrap interval 11.4–13.0%).
A subsequent per-document streaming candidate yielded 15.1% (12.6–17.6%). Its
baseline/candidate elapsed medians were 61.785/52.410 seconds and peak-RSS medians
were 2,174,440/1,643,324 KiB. All verification results agreed. Both candidates were
rejected against the preselected 20% elapsed-time threshold. Four pairs describe
this experiment's limited spread, not deployment-wide latency percentiles.

Run the complete publication path with recorded observations:

```sh
nix run .#soranoha-publication-replay -- \
  --repo /absolute/path/to/aozorabunko \
  --from START_COMMIT --to END_COMMIT \
  --recording /absolute/path/to/recording.json \
  --assessment-source /absolute/path/to/assessment-source.json \
  --policy /absolute/path/to/publication-policy.edn \
  --evidence-root /absolute/path/to/retained-evidence \
  --out /absolute/path/to/new-publication-measurement-directory \
  --concurrency 16
```

This command covers the full selection and refuses `--limit`. It creates its own
source checkout, computation cache, reviewed-input repository, local publication
origin, signing files from the public conformance vectors, and serving tree. It
copies the supplied assessment source and policy; it reads retained evidence
without modifying it. The supplied policy still controls publication admission.

The recording maps each full source commit to a named observation round. Several
commits can share a round without duplicating the response map:

```json
{
  "revisions": {"FULL_SOURCE_COMMIT": "observations-1"},
  "rounds": {
    "observations-1": {
      "as_of": "2026-09-06",
      "responses": {
        "https://www.aozora.gr.jp/guide/kijyunn.html": {"sha256": "BODY_DIGEST"},
        "https://www.aozora.gr.jp/cards/000001/card100.html": {"status": 404}
      }
    }
  }
}
```

Supply every URL the assessment requests, including the catalog and edition ZIPs.
`sha256` names a retained body at `evidence-root/DIGEST`; digests must be lowercase
SHA-256 hex. Recorded HTTP failures use statuses 100–599 except 200, including redirects. Body hashes are checked,
and a missing round, missing response or changed body fails the experiment.
There is no network fallback. The evaluator and release path consume these
responses through the same acquisition seam as live HTTP; production CLI commands
have no recorded-observation mode.

Each revision regenerates and commits its assessment snapshot, releases through
the ordinary transaction and verifier, activates the serving tree, then repeats
the release and activation. The repeat must execute no build stages, retain the
publication commit and head, and reuse the verified export. `measurements.jsonl`
records phase times, total and per-stage executions, and published work-entry
deltas using the existing manifest oracle. Detailed build reports retain the trace
keys; the source-history build replay above checks unexplained DAG execution over
the full build selection. GNU time writes whole-process elapsed seconds and peak
RSS in KiB to stderr, including startup. This is a single shared JVM per replay;
it does not measure a new JVM for each incremental release.

These are simulations with the supplied observations. Reusing today's retained
assertions over old commits does not reconstruct historical website state. An
edition absent from the supplied assessment, or one whose source changes beyond
its attestation, remains unadmitted until assessment inputs justify it. Label the
observation fixture, source range, admitted population and cache conditions with
any reported timings.

Reference corpus run on Speely, benchmark revision `987f476c`, source commits
`19549096…` → `36bf8ec8…` → `0e9ea3e5…` (2026-04-23 through 2026-04-25):

| Source date | Assessment (s) | Release (s) | Serving (s) | Repeat release + serving (s) | Executed stages |
| --- | ---: | ---: | ---: | ---: | ---: |
| Apr 23 | 53.6 | 735.6 | 32.1 | 109.3 | 103,840 |
| Apr 24 | 31.7 | 104.8 | 39.8 | 126.9 | 0 |
| Apr 25 | 33.5 | 115.9 | 47.3 | 133.2 | 9 |

The full selection grew from 17,601 to 17,602 candidates; the published population
from 17,307 to 17,308 works. Apr 24 retained every published work entry. Apr 25
added one, changed one and retained 17,306. Every unchanged repeat executed zero
stages and preserved the publication commit and verified export. The fixture used
34,618 URL mappings backed by the 17,308 retained reliance records, supplied as
simulated contemporary responses over those historical commits.

The complete command took 1,598.89 seconds, including JVM startup, setup and all
three repeats, with GNU time reporting 5,821,864 KiB peak RSS. Repository cloning
took 12.97 seconds. This was an empty computation cache with available Nix closures
and uncontrolled OS page caches. It is one baseline run, not a speedup comparison.
The harness retains the assessment result until row emission, so the peak includes
that result. Phase measurements and reports remain under
`/data/soranoha-benchmarks/publication-20260906/run` on Speely.
