# Bounded Work-Set Handoff: Index/Traversal Layer Design Investigation

> Hammock-driven-design probe. Facilitation mode: **Synthesizer / Challenger**.  
> Status: **provisional / pre-decision**. The architecture note asserts that
> "the external incremental index should be the default design bias"; this report
> treats that assertion as a hypothesis to be tested, not a closed decision.
> Incubation is recommended before a direction is adopted.

## 1. Problem Statement

### Inputs
The index/traversal layer must accept:

- A pinned corpus snapshot coordinate (fixed-output/content-addressed snapshot
  hash, local checkout path, or archive identifier) —
  `docs/adr/0003-nix-materialization.md:42-45`,
  `docs/high-level-architecture-note.md:723-727`.
- Per-work manifests that carry identity inputs: work content hash, parser
  build/config hash, TEI profile hash, tokenizer/dictionary hash, schema hashes,
  and requested output/analysis recipe —
  `docs/high-level-architecture-note.md:127`,
  `docs/high-level-architecture-note.md:615-617`.
- A *bounded work-set* and requested layers/profile produced outside Nix: the
  external incremental index decides which works have changed or are requested,
  not the Nix evaluator —
  `docs/high-level-architecture-note.md:778-783`,
  `docs/adr/0003-nix-materialization.md:20`.

### Outputs
The layer must hand Nix:

- A bounded set of Nix derivations to realize (one work, a small explicit batch,
  or a single CAS-target builder).
- Enough manifest/sidecar paths so that failures remain attributable to
  individual works —
  `docs/adr/0003-nix-materialization.md:32-34`,
  `docs/adr/0005-operational-runtime.md:56-68`.

### Constraints
- The full Cartesian product of
  `~17,800 works × parser variants × TEI profiles × tokenizer/dictionary combos
  × output formats × analysis recipes`
  must **not** be eagerly materialized or evaluated as one Nix attrset —
  `docs/high-level-architecture-note.md:731-746`,
  `docs/high-level-architecture-note.md:774-776`.
- Work-level invalidation is the default granularity; batching is allowed only
  when evaluation overhead dominates and the batch rule is recorded —
  `docs/adr/0003-nix-materialization.md:26-34`.
- Nix evaluation time and peak memory are acceptance criteria, not
  implementation trivia:
  - Smoke-corpus eval must complete in < 30 s and use < 2 GB peak memory on a
    recorded CI runner —
    `docs/adr/0003-nix-materialization.md:52-53`.
- A first cost-envelope report must measure corpus input size, manifest index
  size, parser IR size, TEI size, tokenized output size, parse time, TEI render
  time, validation time, Nix evaluation time, and peak memory —
  `docs/adr/0003-nix-materialization.md:58-61`,
  `docs/high-level-architecture-note.md:758-759`.
- The current Nix surface is CLI-only apps and checks in `flake.nix`; there is
  no derivation matrix yet —
  `flake.nix:27-215`.
- Existing `abc.tools.manifest-index` already indexes manifests and detects
  reproducibility conflicts, so an external index is not starting from a blank
  sheet —
  `src/abc/tools/manifest_index.clj:17-42`.

## 2. Design Alternatives

### A. On-demand derivation generation from a work manifest + requested profile

**Mechanism:** A Nix function
`abcWork :: { corpusSnapshot, workManifest, parserSpec, teiProfile,
tokenizerSpec, outputFormat, analysisRecipe } -> derivation`.
An external selector/index produces a list of requested work IDs; a small
script maps that list to `nix build` requests or passes the list into a Nix
entry point that evaluates only the selected derivations. Nix never sees an
attrset over all works; it sees only the selected set.

**What it optimizes:** Precise invalidation (one changed work → one derivation
rebuilds), clean per-work failure attribution, and Nix's normal hermetic
closure/recipe pinning.

**Coupling introduced:** The Nix expression must understand how to read a
work manifest; evaluation cost still scales with the size of the requested
set, because every selected derivation is materialized in the evaluator. The
external selector and the Nix function must agree on manifest schema and
profile coordinates.

**What it CANNOT express:** Dynamic discovery of "all works matching a query"
inside Nix. The requested set must be computed externally, then handed to Nix.
It also cannot hide evaluator cost if the selected set itself is large.

### B. Batch-by-author/card/release with an external index deciding the subset

**Mechanism:** The external index groups works into stable batches (e.g. by
author, card ID, or release slice). Nix builds one derivation per batch; the
batch derivation iterates over its member works. Batch membership is recorded
in manifest/run metadata.

**What it optimizes:** Cheap Nix evaluation (fewer derivations) and amortized
runtime startup, especially for JVM-heavy TEI validation —
`docs/high-level-architecture-note.md:663-670`. It fits publication builds
where evaluation overhead dominates.

**Coupling introduced:** Batching policy leaks into both the index and the
Nix derivation; invalidation is coarser; the batch derivation must emit
per-work sidecars to keep failures attributable.

**What it CANNOT express:** Fine-grained cache reuse when only one work in a
batch changes — the whole batch rebuilds. It therefore trades rebuild waste
against evaluation cost.

### C. Content-addressed store + manifest index (no Nix attrset matrix)

**Mechanism:** Nix builds only a generic "realize requested targets"
derivation whose inputs are:
1. a content-addressed source snapshot,
2. a `requested-set.json` produced by the external index,
3. a manifest/index file that maps artifact coordinates to content hashes.

The derivation produces exactly those requested artifacts into a CAS-style
output tree. Nix does not contain a `works` attribute set; it contains a
function from `(requested-set, profile)` to outputs.

**What it optimizes:** The smallest possible Nix evaluation surface. Delta
detection, re-indexing, and query planning can live entirely outside Nix.
Manifests remain the source of truth, matching the architecture note's
"manifest files remain the source of truth" rule —
`docs/high-level-architecture-note.md:636-637`.

**Coupling introduced:** A shared CAS layout convention between the external
index and the Nix builder. Nix purity must be maintained by fixed-output or
content-addressed snapshots; local development paths are impure and
non-releaseable per ADR 0003 —
`docs/adr/0003-nix-materialization.md:36-39`,
`docs/high-level-architecture-note.md:723-727`.

**What it CANNOT express:** Automatic transitive invalidation inside Nix. If
the index omits a dependency, Nix will not rebuild it. It also weakens the
"one Nix expression shows the whole build" property; consumers need both the
flake and the index.

### D. DVC-style graph vs Nix derivation graph tradeoff

**Mechanism:** Use a DVC/MLflow-style pipeline DAG *outside* Nix for coarse
stages (ingest → parse → TEI → tokenize → analyze), and use Nix only to
provide pinned tool/runtime environments. Stages are files/directories; the
external DAG tracks dependencies and caches outputs.

**What it optimizes:** Expressive conditional/recovery logic, cheap
experiment workflows, and no Nix evaluator matrix at all. The architecture
note already flags DVC as useful for ML experiment workflows —
`docs/high-level-architecture-note.md:824`.

**Coupling introduced:** Two parallel caching/provenance systems (DVC + Nix).
The flake alone no longer reproduces outputs; a DVC lockfile must be kept in
sync with `flake.lock`. The "operational runtime" becomes an orchestrator,
which is exactly the fate the design question wants to avoid.

**What it CANNOT express:** Nix-hermetic input closure for each stage unless
every DVC stage shells out to a Nix-built tool. It also cannot give consumers
a single `nix build` recipe for reproducibility.

## 3. One-Thing Acceptance Test per Alternative

For each alternative, the single decisive throwaway test is:

- **A:** With a 1,000-work representative subset, does
  `nix eval` / `nix build` of only the selected derivations stay under the
  ADR 0003 limits (30 s, 2 GB) and correctly invalidate *only* the one changed
  work when a single source file changes?
  (`docs/adr/0003-nix-materialization.md:52-53`,
  `docs/adr/0003-nix-materialization.md:26-28`)

- **B:** For a chosen batch size, what fraction of a batch is rebuilt when a
  single work inside it changes? Does eval time/memory scale with the number
  of batches rather than the number of works? Is the waste ratio acceptable
  relative to A and C?

- **C:** Can the external index hand Nix a `requested-set.json` containing
  1,000 works and have Nix produce exactly those outputs without materializing
  an all-works attrset? Does an externally computed delta produce the same
  result faster than asking Nix to re-evaluate a matrix? Does single-work
  invalidation survive, or does any index change rebuild the whole set?

- **D:** Can a DVC `dvc.lock` reproduce the same output hashes as a Nix-only
  build for the smoke corpus? What orchestrator code size and runner state is
  required? Does the added system justify the benefit?

## 4. Unknowns That Must Be Resolved Before Deciding

None of the following are measured in the repository yet; they are the load-bearing
unknowns:

- Real parser wall-clock time and peak RSS on representative subsets. ADR 0002
  calls for these measurements but candidate reports do not exist yet —
  `docs/adr/0002-parser-evaluation.md:54-58`.
- TEI render time and output size per work; tokenized output size per work —
  `docs/high-level-architecture-note.md:758-759`.
- Nix evaluation time and peak memory for a one-derivation-per-work layout
  versus a batch layout on 100/1,000/5,000-work subsets —
  `docs/adr/0003-nix-materialization.md:52-53`,
  `docs/high-level-architecture-note.md:774-776`.
- Whether work manifests are cheap enough to pass into Nix at evaluation time
  or must be kept outside Nix and referenced as fixed-output paths.
- Average Aozora correction/change rate, which determines how much batching
  wastes rebuilds.
- Cost of fixed-output/content-addressed snapshot inputs vs local-path impure
  inputs for development workflows —
  `docs/adr/0003-nix-materialization.md:36-39`.
- JVM validation startup cost and the optimal validation batch size —
  `docs/high-level-architecture-note.md:663-670`.
- Whether the smoke corpus is defined and available; the architecture note
  references it but no `benchmarks/` directory was found in this repository.
  The note still mandates a first benchmark at
  `docs/high-level-architecture-note.md:758-759`.

## 5. Reproducibility Runtime vs Operational/Query Runtime Boundary (Splitting ADR 0005)

ADR 0005 currently bundles API, observability, retention, orchestration,
concurrency, storage packing, and service-interface concerns into one Draft —
`docs/adr/0005-operational-runtime.md:1-70`. The architecture note already
separates these concerns across several sections
(`docs/high-level-architecture-note.md:635-721`,
 `docs/high-level-architecture-note.md:911-935`).

Proposed split into separable decisions:

1. **Operational Orchestration ADR** — single-host CLI/script runner, batch
   grouping rules, expensive runtime amortization (JVM worker, batch validation),
   concurrency rules, and the JSONL run-summary format —
   `docs/adr/0005-operational-runtime.md:14-27`,
   `docs/high-level-architecture-note.md:663-670`.

2. **Storage/Packing ADR** — hot/warm/cold/archive retention tiers, loose files
   vs SQLite vs tar/zip archives for manifests and sidecars, inode/directory
   traversal costs, and the requirement that packed storage still export to
   ordinary files —
   `docs/high-level-architecture-note.md:683-699`,
   `docs/adr/0005-operational-runtime.md:47-55`.

3. **Query Runtime ADR** — file bundle + generated SQLite index vs SPARQL vs
   XTDB vs DataFusion/Arrow, chosen after access patterns are known —
   `docs/high-level-architecture-note.md:558-565`,
   `docs/high-level-architecture-note.md:643-655`.

4. **API/Service Interface ADR** — REST, gRPC, authenticated endpoints,
   streaming APIs, and distributed scheduling explicitly out of scope for v0,
   with a promote/demote trigger —
   `docs/adr/0005-operational-runtime.md:16-18`,
   `docs/high-level-architecture-note.md:653-655`.

5. **Retention and Archive ADR** — indefinite retention of release manifests,
   schemas, canonicalization fixtures, and archive identifiers (SWHIDs), and the
   policy on when archive triggers fire —
   `docs/adr/0003-nix-materialization.md:42-45`,
   `docs/high-level-architecture-note.md:732-735`.

**Boundary rule:** Nix (the *reproducibility runtime*) owns artifact recipes,
dependency pinning, content-addressed snapshots for publication, and bounded
derivation builds —
`docs/high-level-architecture-note.md:927-928`,
`docs/adr/0003-nix-materialization.md:10-24`.

Nix does **not** own: corpus-delta detection, query planning, multi-host
coordination, retention policy, storage-packing format, or API/service design.
Those live in the operational/query runtime outside the flake.

## 6. Prototype Recommendation

**Top two alternatives to disambiguate:** **A** (on-demand per-work
derivations with external selector) and **C** (content-addressed store +
explicit requested-set, no Nix attrset). They both honor the external-index
bias; the difference is whether Nix holds per-work derivations or only a
generic builder driven by an external manifest.

**Smallest throwaway experiment:**

1. Create `prototypes/bounded-workset-index/` as a disposable directory that
   must not be merged into production paths.
2. Build two harnesses side by side, using the smoke corpus or a synthetic
   1,000-work slice of small Aozora-style files:
   - **Harness A:** a Nix function `abcWork` taking one work manifest and
     returning one derivation. A Clojure/Python script converts an external
     `requested-works.json` into a Nix build invocation for exactly those works.
   - **Harness C:** a single Nix function `abcBuildRequestedSet` taking a
     `requested-set.json` and a pinned source snapshot, returning only the
     requested artifacts. The external index writes `requested-set.json`.
3. Measure:
   - `nix eval` wall time and peak evaluator memory for 100/1,000/5,000 works.
   - Cold `nix build` time.
   - Incremental `nix build` time after changing exactly one input work.
   - Number of derivations evaluated.
   - Whether per-work failure manifests/sidecars remain attributable.
4. Falsifiers/kill conditions:
   - If **A** exceeds 30 s or 2 GB at 1,000 works, one-derivation-per-work is
     dead for the full corpus; prefer **B** or **C**.
   - If **C** rebuilds the entire requested set whenever the index changes,
     it has lost fine-grained invalidation; prefer **A** or **B**.
   - If either cannot produce per-work failure sidecars, it violates the
     granularity requirements of ADR 0003 —
     `docs/adr/0003-nix-materialization.md:32-34`.

**Expected artifact:** measurement log + a one-page decision note that records
which alternative is killed or remains viable, and the remaining unknowns with
owners. No production flake changes come from this prototype.

---

## Incubation Note

The decisive unknowns are *measurements*, not opinions. Before a final
decision is made, the project should sleep on the matrix of tradeoffs,
gather the prototype numbers above, and only then promote one path into a
revised ADR 0003/0005. This report is intentionally provisional; the next step
is a throwaway benchmark, not a production implementation.
