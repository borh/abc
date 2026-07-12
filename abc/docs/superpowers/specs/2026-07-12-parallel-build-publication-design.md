# Parallel build-publication with hot-spot caching — design

- Date: 2026-07-12
- Status: approved
- Scope: `soranoha build-publication` full-corpus wall time (Clojure side only;
  Rust adapters untouched)

## Problem

`soranoha build-publication` publishes the whole nix-pinned aozorabunko corpus
(~17,884 work ZIPs, 17,879 admitted) to TEI, plaintext, manifests, and
validation results. Both hot loops are fully sequential `mapv`:

- Source derivation (`abc/src/abc/tools/soranoha_build_publication.clj:415`):
  per work — private zip staging + full member SHA-256 streaming, two
  subprocess spawns (aozora2html adapter, `ab-aat-to-parser-ir convert`),
  parser-identity assertion, metadata/person ingest.
- Publication (`abc/src/abc/tools/soranoha_build_publication.clj:625`):
  per work — plaintext render, TEI render, RelaxNG (Jing) + Schematron
  validation, preservation sidecar, two manifests with per-artifact SHA-256.

Per-work waste on top of the sequential loops:

- The Jing RelaxNG schema is re-parsed for every work
  (`abc/src/abc/tools/tei.clj` `validate!` builds a fresh `ValidationDriver`
  and calls `.loadSchema` per call; the namespace docstring already flags
  this as a v0 shortcut).
- Parser-IR JSON is re-read several times per work.

The build-publication design doc
(`docs/superpowers/specs/2026-07-08-soranoha-build-publication-design.md`)
explicitly deferred concurrency: sequential was for smoke/demo fixtures, and
concurrency was to become an operational setting recorded in
`build-plan.json` before full-corpus admission. This design pays that debt.

A fixed-thread-pool executor already exists for the separate `--batch` CLI
(`abc/src/abc/tools/materialize_publication.clj` `materialize-batch-jobs!`,
`Executors/newFixedThreadPool` + `.invokeAll`) but is not wired into
`build-publication`.

## Goals

1. Parallelize both per-work loops behind one `--concurrency` knob,
   recorded in `build-plan.json`.
2. Remove the per-work Jing schema re-parse.
3. Remove clearly redundant parser-IR re-reads.
4. Outputs stay byte-identical to a sequential run.

Non-goals: the CI streamed source-bundle corpus evidence gate (just landed,
separate lane); Rust adapter changes; cross-stage streaming (the 3-step
workflow contract — derive, build records, publish — stays as is);
GraalVM/uberjar packaging.

## Approach (chosen)

Bounded fixed thread pool with ordered results, no new libraries. Rejected
alternatives: virtual threads + semaphores (marginal gain over a fixed pool
for this workload, more moving parts); core.async streaming pipeline
(conflicts with the 3-step workflow contract that writes `build-plan.json`
from complete stage-1 results).

### 1. Shared parallel helper: `abc.tools.parallel`

One public function, an ordered bounded parallel map:

```clojure
(ordered-pmap concurrency f coll) ; => vector, same order as (mapv f coll)
```

Semantics:

- `concurrency <= 1` → plain `mapv` (identical code path to today).
- Otherwise `Executors/newFixedThreadPool` + `.invokeAll` over `Callable`s,
  results collected in submission order, executor always shut down
  (`finally`).
- Each task wraps `f` with `bound-fn*` so dynamic bindings convey. This is
  load-bearing: `*derive-parser-ir!*` in `soranoha_build_publication.clj` is
  rebound in tests, and raw executor tasks do not convey bindings.
- `ExecutionException` is unwrapped so the original throwable propagates,
  preserving `ex-info` data that callers dispatch on (e.g.
  `source-bundle-admission-error`).

`materialize-batch-jobs!` in `materialize_publication.clj` is refactored to
call this helper (behavior-preserving deduplication).

### 2. Concurrency knob

- New optional `build-publication` CLI flag `--concurrency N`.
  Absent or `0` → `(.availableProcessors (Runtime/getRuntime))`; clamped to
  `>= 1`. Same resolution rule as the existing `batch-concurrency`.
- The resolved value (what actually ran) is recorded in `build-plan.json`
  (operational setting, as the 2026-07-08 design anticipated).
  `build-config.json` is unaffected — concurrency is a CLI flag, not a
  config key, because it is host-specific and must not change content
  hashes.
- Both loops use the same resolved value.

### 3. Loop 1 — source derivation

Replace the `mapv` at `soranoha_build_publication.clj:415` with
`ordered-pmap`. The `continue-on-failure` try/catch stays inside the
per-candidate function unchanged.

Thread-safety inventory:

- Per-work output dirs under `materialized-root` and per-call
  `Files/createTempFile` staging copies: already collision-free.
- Shared write: person records. `write-materialized-work!` →
  `aozora-ingest/run-from-rows!` → `write-person-file!`
  (`abc/src/abc/tools/aozora_ingest.clj:102`) writes
  `<persons-dir>/<person_id>.json` into a persons dir shared across works.
  Two works by the same author race: the check-then-write is non-atomic and
  `json/write-deterministic-json-file!` writes directly to the target, so a
  concurrent reader can see a partial file (and throw "unparseable JSON").
- Fix: make the person-file write atomic — write to a sibling temp file in
  the same directory, then `Files/move` with `ATOMIC_MOVE` (helper pattern
  already present in `soranoha_build_publication.clj`). With atomic
  replacement, the same-content race is harmless (both threads compute the
  same `person_record_hash` and write identical bytes; last move wins) and
  readers never observe partial JSON. The divergent-hash refusal semantics
  are unchanged. Implementation choice: an atomic variant used at the
  person-write site, or making `write-deterministic-json-file!` atomic
  globally — decided at implementation time; global atomicity is acceptable
  since it is semantics-preserving for all current callers.

### 4. Loop 3 — publication

Replace the `mapv` at `soranoha_build_publication.clj:625` with
`ordered-pmap`. Per-work rendering is pure; per-work output dirs are
disjoint; the Schematron XSLT cache is already an atom holding thread-safe
Saxon `XsltExecutable`s with per-call `Load`. Ordered results keep
`publications-report.json` byte-identical.

### 5. Jing RNG schema cache

Rework `abc.tools.tei/validate!`:

- Load the schema once via Jing's schema-reader API into a
  `com.thaiopensource.validate.Schema` (immutable, thread-safe), cached in a
  `defonce` atom keyed by canonical path + mtime — mirroring the existing
  Schematron executable cache (`abc/src/abc/tools/schematron.clj:44`).
- Per call: `(.createValidator schema props)` with the per-call error-handler
  property map, then run a SAX parse of the XML feeding the validator's
  content handler. The per-call error-handler atom keeps result collection
  exactly as today; return shape `{:label .. :violations [..]}` unchanged.
- Update the namespace docstring (it currently documents the v0 re-parse and
  "sequential use only" — both stop being true).

This removes ~17.9k schema parses per full corpus run and makes `validate!`
safe under loop-3 parallelism.

### 6. Redundant parser-IR re-reads

Within `write-materialized-work!`, pass already-parsed values through
instead of re-reading files just written, only where the re-read is clearly
redundant. Exception kept as-is: `assert-parser-identities!` re-reading
parser-IR from disk is treated as an intentional serialization round-trip
gate unless inspection during implementation shows otherwise; if it is
removed, the commit must say why it was safe.

## Semantics and error handling

- Byte-identical outputs: ordered results + unchanged deterministic JSON
  writers mean every report and manifest is byte-identical to a sequential
  run over the same inputs.
- `continue-on-failure` semantics unchanged (catch inside the task).
- Fail-fast mode difference (accepted): sequential aborts at the first
  failing work; parallel lets in-flight tasks finish before rethrowing the
  same error. The end state is a superset of partial work; exit status and
  error identity are identical.
- The helper must not leak threads on failure (shutdown in `finally`).

## Testing

1. Unit tests for `abc.tools.parallel/ordered-pmap`: result ordering under
   concurrency, exception unwrapping (original `ex-info` propagates),
   dynamic-binding conveyance, `concurrency 1` equals `mapv`,
   executor shutdown.
2. Jing cache: `validate!` returns identical violations before/after; two
   concurrent `validate!` calls do not interfere; cache invalidates on
   schema mtime change.
3. Atomic person write: concurrent same-record writes leave a valid file;
   divergent-hash refusal still throws.
4. Existing build-simulation and publication tests pass unchanged — they are
   the output-drift tripwire.
5. `build-plan.json` records the resolved concurrency.

## Benchmark and acceptance

- Fixed subset (a few hundred works from the nix-pinned corpus), same
  methodology as the annotation-join benchmark
  (`docs/handoffs/2026-07-11-annotation-join-overlap-benchmark.md`):
  median-of-5 after warm-up, sequential (`--concurrency 1`) vs default
  (= cores), recorded in a handoff doc.
- Acceptance: byte-identical outputs between the two runs (diff the output
  roots), all existing tests green, and wall-time speedup roughly tracking
  core count on the subset.
- A full-corpus run on hinoki is later validation, not a gate for this
  change.
