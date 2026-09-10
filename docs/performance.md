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

Add `--endpoints-only` to build just the two ends of that range. The range is
still resolved and still has to be a real first-parent range, so the answer
remains about upstream history rather than about two unrelated commits; what is
dropped is every build in between. This is what makes a comparison across an
arbitrary distance affordable, and it is the difference between measuring how
the build behaves over history and asking what two revisions differ by. The
second build runs against the first one's cache, so only works whose sources
moved are converted again.

Use it to answer whether the current toolchain would produce different
documents from the same sources, which is the one question
[`corpus-delta`](private-publication.md) cannot: that command compares source
identities, and source identities are the whole answer only under an unchanged
toolchain. Reach for this recipe when the toolchain has moved, and for
`corpus-delta` otherwise, because comparing sources costs seconds and this
costs a cold build.

`measurements.jsonl` records repository setup time, checkout time, build time,
per-stage execution counts, and source/artifact delta counts. Every revision is
then built again unchanged. The command fails if that repeat executes any stage,
changes an artifact, or if the existing delta oracle finds unexplained execution
between revisions. Detailed build reports remain under `build/runs/`.

These are **build timings**, using all ordinary build stages, including independent
source accountability and interpretation-coverage reports. They exclude JVM
startup, live assessment, publication transactions and serving export. An empty
computation cache does not imply empty
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

On an isolated four-release, 72,749-blob chain, with Charred 1.042 in both
builds, bypassing per-string buffered-writer construction yielded a median paired
elapsed reduction of 12.8% (four pairs; 95% percentile bootstrap interval 11.4–13.0%).
A subsequent per-document streaming candidate yielded 15.1% (12.6–17.6%). Its
baseline/candidate elapsed medians were 61.785/52.410 seconds and peak-RSS medians
were 2,174,440/1,643,324 KiB. All verification results agreed. Both candidates were
rejected against the preselected 20% elapsed-time threshold. Four pairs describe
this experiment's limited spread, not deployment-wide latency percentiles.

### What the browse layer's reading view costs an activation

Serving activation parses and renders every published work's TEI into an HTML
reading page, once per activation, on both the fresh-export path and the reuse
path. Because the reuse check compares generated bytes exactly as it compares
chain content, it must produce them.

Measured on this machine with a synthetic work built through the publication
render path, at the corpus's rough mean TEI size of 42 KiB: 3.8 ms of CPU per
work and 30 KiB of HTML out. Extrapolated over 17,308 works that is about 65
seconds of added CPU and about 0.5 GiB of added tree, against a measured
activation baseline of roughly 52 seconds elapsed. Extrapolation from one
synthetic document is an order-of-magnitude estimate, not a measurement of the
corpus: works vary by more than an order of magnitude in size, and the estimate
excludes writing the pages out.

Rendering is per-work and independent, so it parallelizes if activation latency
turns out to matter. It is sequential today because generation must produce
byte-identical output on every activation for the reuse check to hold, and that
property is easier to maintain deterministically in a single thread than across a
worker pool. Measure before changing it.

### What the bulk archives cost an activation

Serving is a static tree with no runtime, so a bulk selection cannot be
assembled when it is requested: every selection is built at export time, on
both the fresh-export path and the reuse path, for the same reason the reading
pages are.

Each work is deflated once per archive it appears in (the whole-corpus
archive, its author's, and its NDC class's) for each of the two bulk-published
types, so six times per activation. Measured on this machine at level 6, the
published TEI of 蜘蛛の糸 deflates at 66 MiB/s and to 14.1% of its size; its
plain text to 35.3%. At the corpus's rough mean TEI size of 42 KiB that is
about 0.6 ms per work per TEI archive, and small inputs are dominated by
per-entry setup rather than by throughput.

Extrapolated over 17,308 works: roughly 40 seconds of added CPU and roughly
0.4 GiB of added tree, against a measured activation baseline of about 52
seconds elapsed and the reading view's own estimated 65 seconds and 0.5 GiB.
Extrapolation from one document is an order-of-magnitude estimate: works vary
by more than an order of magnitude in size, and a work whose author has many
works still appears in exactly three archives, so the multiplier does not grow
with the corpus.

The readable filenames add four symlinks per work (about 69,000 more entries,
roughly doubling the work-facing layer) and no bytes.

The per-work citation records add two small files per work, about 35,000 more
entries and roughly 25 MiB, against a serving tree measured in gigabytes. They
are generated rather than deflated, so they cost no measurable CPU. Both are
pure functions of the release and its DOI, which is what lets the reuse check
compare them byte for byte like everything else the browse layer writes.

That DOI is the one input to an activation that is not chain content. Setting
or changing it for a commit already exported makes the reuse check fail with
`serving-tree-mismatch`, because the citations in the existing tree name a
different release DOI. The failure is intended: the alternative is one tree
serving two answers. Remove that tree and re-export.

Deflating each work once and reusing the compressed member across the three
archives that hold it would cut the CPU by about two thirds, at the cost of
writing the ZIP container by hand rather than through `ZipOutputStream`, which
is also what handles ZIP64 when an archive outgrows the 32-bit fields. That
trade is not worth taking until activation latency is a measured problem.

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

Assessment execution counts appear separately as `assessment-executions`; release
counts cover every actual engine execution during preflight, building and final
assessment. They include extraction performed before the per-work build report,
so build-report counts alone are not total publication work. `release-phases` and
`repeat-phases` contain inclusive timings: capture and Aozora Bunko checks nest inside
assessment, and capture also occurs inside preflight. Do not sum nested phases.

Compare unchanged publication in an existing completed replay fixture:

```sh
nix run .#soranoha-publication-replay -- \
  --baseline /nix/store/BASELINE-soranoha-publication-replay \
  --candidate /nix/store/CANDIDATE-soranoha-publication-replay \
  --repeat-run /absolute/path/to/completed-publication-measurement-directory \
  --recording /absolute/path/to/recording.json \
  --evidence-root /absolute/path/to/retained-evidence \
  --out /absolute/path/to/new-comparison-directory \
  --concurrency 16
```

The comparison runs fresh JVMs in ABBA/BAAB order, measuring release, serving,
whole-process elapsed time and peak RSS. Each child uses the recorded round for
the fixture checkout's current commit and follows the ordinary release preflight.
It must execute zero stages, preserve the origin's published commit and reuse the
verified serving tree. All eight results must agree. `--repeat-run` alone runs one
such check. These commands require the replay's local fetch and push origin,
contained mutable state and public conformance keys; they do not resume a failed
historical replay or accept production signing configuration.

These are simulations with the supplied observations. Reusing today's retained
assertions over old commits does not reconstruct historical website state. An
edition absent from the supplied assessment, or one whose source changes beyond
its attestation, remains unadmitted until assessment inputs justify it. Label the
observation fixture, source range, admitted population and cache conditions with
any reported timings.

## Reference publication baseline (2026-09)

Reference corpus run using source commits `19549096…` → `36bf8ec8…` → `0e9ea3e5…`
(2026-04-23 through 2026-04-25):

| Source date | Assessment (s) | Release (s) | Serving (s) | Repeat release + serving (s) | Executed stages |
| --- | ---: | ---: | ---: | ---: | ---: |
| Apr 23 | 53.6 | 735.6 | 32.1 | 109.3 | 103,840 |
| Apr 24 | 31.7 | 104.8 | 39.8 | 126.9 | 0 |
| Apr 25 | 33.5 | 115.9 | 47.3 | 133.2 | 9 |

The full selection grew from 17,601 to 17,602 candidates; the published population
from 17,307 to 17,308 works. Apr 24 retained every published work entry. Apr 25
added one, changed one and retained 17,306. Every unchanged repeat executed zero
stages and preserved the publication commit and verified export. Both consecutive
report comparisons passed the delta oracle with zero unexplained executions. The fixture used
34,618 URL mappings backed by the 17,308 retained reliance records.

The complete command took 1,598.89 seconds, including JVM startup, setup and all
three repeats, with GNU time reporting 5,821,864 KiB peak RSS. Repository cloning
took 12.97 seconds. This was an empty computation cache with available Nix closures
and uncontrolled OS page caches. It is a baseline run, not a speedup comparison.

Publication source capture reuses the extraction stage, keyed by the
current ZIP digest, extraction version and Clojure toolchain. Source-fact bytes
must match their CAS digest and name that same archive before assessment consumes
them. Missing outputs recompute; corrupt bytes fail closed. Fresh HTTP acquisition,
retained-evidence validation and assessment of current records run on every attempt.
An unrelated source commit reuses the same extraction; changed edition bytes select a new derivation.

A balanced full-corpus comparison using the completed three-release
fixture at `0e9ea3e586eb0aa34039fabfc85a407d2f98b165` (17,308 published works and
51,929 verified blobs) with concurrency 16 and warm computation cache:

| Metric | Baseline median | Candidate median | Median paired reduction | 95% paired bootstrap interval |
| --- | ---: | ---: | ---: | ---: |
| Final assessment | 30.726 s | 20.205 s | 33.7% | 32.4–35.6% |
| Repeat release | 86.154 s | 68.629 s | 20.1% | 19.1–22.3% |
| Serving | 47.994 s | 48.017 s | -0.4% | -3.6–3.3% |
| Whole command, including startup | 140.600 s | 123.135 s | 12.7% | 10.7–13.8% |
| Peak RSS | 4,822,844 KiB | 4,814,938 KiB | 0.08% | -0.19–1.21% |

The assessment improvement clears the preselected 20% relevant-phase threshold;
the largest paired peak-RSS increase was 0.19%, below the 10% guardrail.
Preflight fell from 14.595 to 7.864 seconds; the two source captures within release
fell from 21.233 to 7.764 seconds combined, and Aozora Bunko checking from 9.636 to 6.364
seconds. Release verification remained approximately 24.4 seconds.

All eight runs executed zero stages and preserved manifest
`027f578070ab4e1ba1458583ca65e874d3f0154960c880058dc8c165093f2e2b`, publication commit
`394ffce2eb50fc4c5493f16fb9c772a2c42a0e06`, and the reused verified export.

## Publication cost grows with chain length, and all of it is chain verification

Publishing a release verifies the whole chain. `transact/publish-build!` resolves
the head through `verify/verify-repository-at` before it assembles anything, so
release *n* verifies *n* manifests. Two rounds of measurement narrowed what that
costs: the first found publication paying for the walk twice, the second found
most of one walk going somewhere other than verification.

`soranoha.snh.chain-bench` builds a synthetic chain at the production work
count, so no corpus is needed and the repository path is what is timed. Ten
releases over 17,602 works, three works moving per release so the verifier's
reuse grant applies to the rest, as it does when an upstream commit touches a
median of two works.

The no-op is the quantity the prerequisite is stated against, because a job that
fires on every upstream commit spends most of its runs discovering it has
nothing to do. It is measured by repeating a release with its projection
unchanged, which is what `decide-against-head` no-ops on, so it is the cost of
resolving the head and deciding, with no assembly.

### The walk was being paid for twice

Publication grew at twice the rate of the verification inside it, because
`publish-build!` verified the chain twice: once on the fetched head before
assembling, and once more on the newly written commit before pushing. The no-op
path returns at `decide-against-head` before the second call, which is why the
no-op tracked a single verification while publication tracked two.

The second walk re-verified a prefix the same process had verified moments
earlier under the same pinned keys, when only the new commit was unestablished.
`verify-increment-at` now checks that one commit and carries the earlier proof
forward. The assembly was never a factor: the benchmark's assembler is a pure
function of the head's withdrawn set and a fixed slug list, so it builds the
same 17,602 work entries at every chain length.

### Most of a walk was not verification

Verifying one commit at this work count cost about 3.2 seconds. Timing the parts
of it found where that went, over a 10.7 MB manifest and a 10.1 MB catalog:

| Per commit | Before | After |
| --- | ---: | ---: |
| Manifest canonicalization | 760 ms | 67 ms |
| Manifest boundary decode, in full | 960 ms | 262 ms |
| Catalog boundary decode, in full | 1076 ms | 266 ms |

The two boundary decodes were about two thirds of verifying one commit, and
reading the JSON was 34 ms of that. The rest was producing the canonical form to
compare the stored bytes against, and it was slow for two reasons that have
nothing to do with what the canonical form is. Every object and array was
assembled with `str` and `join`, so the document was copied again at each
nesting level on the way out; and each of its several hundred thousand strings
went through a general JSON writer that pays its setup per call. Filling one
buffer in place, and writing out the escaping that RFC 8785 inherits from RFC
8259, left every output byte unchanged and took canonicalization to a ninth of
what it was. Schema validation, at 142 ms, is now the largest part of a decode.

### The three measurements together

Growth per additional release, by least squares over the same ten-release chain
at each stage:

| Growth per release | Two walks | One walk | One walk, faster canonical form |
| --- | ---: | ---: | ---: |
| No-op invocation | 3.1 s | 3.5 s | 0.93 s |
| Full-chain verification | 3.1 s | 3.2 s | 0.95 s |
| Publication, end to end | 6.3 s | 4.1 s | 0.97 s |

The three now agree, which is the result to read. Publication grows at the rate
of the one verification inside it, so nothing outside verification grows with
chain length in a way this benchmark can see.

That corrects a residual reported here earlier. Publication was said to run about
0.6 seconds per release above the no-op, possibly a real write-path term. It was
an artefact of taking the slope from the endpoints: the genesis release has no
head to verify and sits well below the line, and including it inflates the
publication slope alone. Fitting over chain lengths 2 to 10 puts publication
about 40 milliseconds per release above the no-op, which ten releases cannot
distinguish from noise. What separates them is a constant, not a slope: assembly
and the repository write cost about 9 seconds at every chain length.

### A run of releases carries its own proof and stops re-verifying the chain

Publication verifies the chain because it has to resolve the head before it
assembles anything. A publisher producing one release pays that once. A
backfill producing thousands pays it once per release, and the sum over a
chain is quadratic.

`transact/publish-build!` now takes the proof its caller computed for that
head, in the same process, from the same view and the same pinned keys, and
returns the successor proof under the same key. A caller publishing a run of
releases threads its own proof forward instead of walking the chain again.
This is neither a checkpoint nor a cache: a proof accepted from anywhere
else, persisted, or read back out of the repository is outside the
specification, for the reason
`adr/0002-increment-verification-before-push.md` gives.

Measured over 96 releases of 800 works, the same parameters run twice:

| Publication cost | Intercept | Growth per release | Total over 96 releases |
| --- | ---: | ---: | ---: |
| Re-verifying each time | 1,973 ms | 15.7 ms | 261.9 s |
| Carrying the proof forward | 619 ms | 0.4 ms | 61.8 s |

The carried growth term is not small, it is absent: fitting it returns a
coefficient of determination of 0.008, which is a flat line with noise on it.
The uncarried slope is the loose figure of the two, fitting at 0.24 because
that run shared the machine with other work; the totals and the endpoint
ratios are what it supports. The advantage widens with the chain, from 2.3x
at length 10 to 5.3x at length 96.

The no-op and verification columns do not move, which is the point of
measuring them alongside. They are what a party holding no proof pays: the
scheduled job that fires on an upstream commit, and the third party checking
the corpus.

### What that costs on the real corpus

The synthetic chain uses small blobs, so it fixes the shape and not the
constant. The constant comes from the reference comparison above: a repeat
release over 17,308 published works and 51,929 verified blobs at chain length 3,
with concurrency 16 and a warm cache, took 68.6 seconds. The same point on the
synthetic chain is 24.4 seconds, so the real corpus costs about 2.8 times the
synthetic one at equal chain length.

That single-point calibration is the weakest step here, and it was taken before
the canonicalization work, which will have moved the real side too. Taking it
unchanged, so that the real figures are if anything pessimistic, each additional
release costs roughly 2.6 seconds of no-op and 2.7 seconds of publication, and
three consequences follow:

- The 30-minute bound on an unchanged invocation is crossed near chain length
  680, against 190 before. At 227 releases a year, which is the current upstream
  push rate, that is year three rather than year two.
- Verifying a finished 5,476-release chain costs approximately 4 hours,
  compared to 14 previously. This verification cost determines whether
  independent external parties can routinely audit the corpus.
- Building that chain no longer scales quadratically. Carrying the head proof
  forward makes publication cost constant per release during batch processing.
  Measured at the production work count, publication requires 16.4 seconds per
  release; carrying the synthetic chain's constant through the 2.8x calibration
  above puts the same figure at 45.9 seconds, and the two routes agree on a
  full 5,476-release backfill of roughly three days of compute. Historical
  backfill is constrained by historical assessment snapshot availability rather
  than publication compute. A single-release scheduled job holds no prior proof
  and pays the 2.7-second incremental growth.

The measurement that mattered most is the one that came back better than the
ledger recorded. The publication rearchitecture ledger measured an unchanged
invocation at 1,319 seconds against the 30-minute bound and called the limit
present. The batching work since has brought that to 68.6 seconds at the same
work count. The bound is not currently breached; it is reached by growth, and
the growth is what needs a decision.

### Chain verification is linear to 60 releases

The slopes above come from ten releases. A 60-release chain at the same work
count settles the shape: least squares over lengths 1 to 53 gives 944.6 ms per
release for verification, with a coefficient of determination of 0.9977, and the
best-fit quadratic coefficient is negative, improving residual root-mean-square
only from 692 to 676 ms. Splitting the range in half lowers the slope rather
than raising it, 948.1 ms over lengths 1 to 30 against 934.5 ms over 31 to 53.
Lengths 54 to 60 were measured while builds competed for the same cores and are
excluded.

### Verification splits across cores, and its runs have to be long

A walk was one thread reading one manifest at a time. `verify-repository-at`
now splits the chain into runs verified concurrently and joins them at the
k-1 seams, a seam being the transition the runs deliberately leave open. Every
invariant the walk establishes is local to one commit or to one adjacent pair,
which is what makes a split possible at all; the test
`a-segmented-walk-rejects-exactly-what-one-walk-rejects` puts the whole build
mutation table through both forms and requires the same rejection from each.

The newest commit of each run reads and hashes every artifact of its manifest,
because no younger verified commit grants it reuse. That cost is paid once per
run, so short runs waste it. Measured on one 96-release chain of 800 works,
verified ten times back to back on a machine with 32 cores:

| Runs | Releases per run | Wall time | Speedup |
| ---: | ---: | ---: | ---: |
| 1 | 96 | 5,220 ms | 1.00x |
| 2 | 48 | 2,523 ms | 2.07x |
| 3 | 32 | 1,796 ms | 2.91x |
| 4 | 24 | 1,437 ms | 3.63x |
| 6 | 16 | 1,133 ms | 4.61x |
| 8 | 12 | 960 ms | 5.44x |
| 12 | 8 | 828 ms | 6.30x |
| 16 | 6 | 801 ms | 6.52x |
| 24 | 4 | 754 ms | 6.92x |
| 32 | 3 | 721 ms | 7.24x |

Wall time follows 244 ms plus 50.9 ms per release in a run, fitted over runs of
12 releases or longer. Neither constant is a free parameter: the intercept is
what verifying one commit with no reuse grant costs at this work count, and the
slope is the marginal cost a single walk pays per release, both measured
separately.

The fit holds within 7% while runs are 16 releases or longer and then breaks
down. At runs of 8 the measured wall time is 27% above the model, at runs of 3
it is 82% above, and speedup saturates near 7x rather than approaching the core
count. Part of that is the fixed cost, which the model already carries; the
remainder is unaccounted for and was not profiled. It is enough to fix the
policy: the automatic run count never produces a run shorter than 32 releases,
and it never asks for more runs than the machine has processors. An explicit
`:segments` overrides both, which is what lets a test drive the seams over a
chain of four.

At corpus scale, carrying the 2.8x calibration above onto both constants, one
commit with no grant costs 24.6 seconds and each further release in a run costs
2.65 seconds:

| Chain length | One walk | 8 runs | 16 runs | 32 runs |
| ---: | ---: | ---: | ---: | ---: |
| 227, one year at the current push rate | 10.4 min | 1.6 min | 1.0 min | 0.7 min |
| 1,000 | 44.5 min | 5.9 min | 3.1 min | 1.7 min |
| 5,476, one release per upstream commit | 4.0 h | 31 min | 15 min | 8 min |

Independent auditors can verify the entire corpus in minutes rather than hours,
removing latency barriers to third-party verification. The 5,476-release row
stays inside the validated range: at 32 runs, each run processes 171 releases.

### What an origin holding the chain costs to keep and to hand out

Software Heritage archives an origin by cloning the URL it is given, so what
the origin costs to hold is also what an archive has to ingest. Two of its
limits are hard rather than merely expensive: the loader does not archive an
object over 100 MB, and its pack-size threshold is around 4 GiB.

Neither is reached, because consecutive manifests delta-compress. A manifest
lists every published work, so two consecutive ones differ in a few entries
out of tens of thousands. `chain-bench --repo-stats` repacks the origin and
reports what each manifest then occupies:

| Works | One manifest | Eight, apparent | In the pack | Each, as a share of itself |
| ---: | ---: | ---: | ---: | ---: |
| 300 | 187 KB | 1,495,616 B | 67,314 B | 4.50% |
| 1,200 | 744 KB | 5,952,416 B | 247,187 B | 4.15% |
| 4,800 | 2,972 KB | 23,779,616 B | 964,307 B | 4.06% |

The share falls as manifests grow. Fitted over these three, a manifest costs
its own size to the power 0.962 in the pack. At the current corpus, where a
manifest is about 10.7 MB, that is roughly 0.41 MB each, so 5,476 releases
carry about 2.2 GB of manifests and one year at the current push rate carries
under 0.1 GB.

The per-object limit is not close either. The largest object a release
publishes is its manifest at about 10.7 MB, with the catalog blob behind it at
10.1 MB. Both grow with the work count, and neither is within an order of
magnitude of 100 MB.

Maintenance is not a factor at these sizes: eight releases leave 228 loose
objects, and repacking took 67, 127 and 321 ms at the three work counts.

These are extrapolations from a synthetic chain with small work artifacts,
fitted across a sixteenfold range of manifest sizes but stopping about four
times short of the real one, and they do not include the peak RSS of a
repack. These measurements should be confirmed on the real origin once populated.

### Reproducing it

From `soranoha/`:

```sh
clojure -Sdeps '{:paths ["src" "test" "resources"]}' -M \
  -m soranoha.snh.chain-bench --works 17602 --releases 10 --changed 3 \
  --tmp /data/soranoha-bench
```

Pass `--tmp` a path on real storage: a corpus-scale chain is gigabytes of loose
objects, and the system temp directory is memory-backed on these machines, so
the default competes with the JVM heap being measured. `--changed all` moves
every work instead, which is what a toolchain change does.

Add `--segments 1,2,4,8,16,32` to verify the finished chain once at each run
count and emit a row for each. One walk is `segments 1`, so the rows compare
directly, and all of them are taken over the one chain.

Verification cost also rises with the number of works, measured on shorter
chains:

| Works | Chain length 1 | Chain length 6 | Marginal cost per additional release |
| ---: | ---: | ---: | ---: |
| 200 | 114 ms | 365 ms | 50 ms |
| 1,000 | 575 ms | 1,410 ms | 155 ms |
| 3,000 | 1,094 ms | 4,106 ms | 602 ms |

Every marginal column here is the slope across a run rather than a single step;
one step is noisy enough to come out negative. The reuse grant removes the
re-hashing of unchanged blobs, and what remains is the per-manifest walk over
every work entry, which is the term that does not go away.
