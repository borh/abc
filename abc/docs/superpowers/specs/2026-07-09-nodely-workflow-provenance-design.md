# Nodely Workflow Provenance Design

Status: Proposed design
Date: 2026-07-09
Owner: Soranoha architecture track

This spec evaluates using
[Nodely](https://github.com/nubank/nodely) as a Clojure-native dependency
realization layer inside Soranoha's workflow/provenance system.

## Decision

Use Nodely as an optional Clojure planner/evaluator for target-driven workflow
realization, not as the source of artifact identity, cache identity, or Nix
materialization policy.

The best fit is a three-layer design:

1. **Soranoha workflow protocol** owns stable step records, provenance,
   cache-key construction, validity checking, workflow reports, and schema
   compatibility.
2. **Nodely environment adapter** realizes requested workflow targets from
   Clojure data dependency graphs, especially when conditional dependencies
   matter.
3. **Nix materialization backend** owns pinned tools, flake inputs, store
   realization, and reproducible build/check isolation.

Nodely should sit behind `abc.tools.workflow.nodely` or an equivalent
Soranoha-owned facade. Active ABC code should not scatter direct Nodely API
usage across domain namespaces.

## Context

Soranoha already has a small workflow layer:

- `abc.tools.workflow/run-workflow!` validates a step plan, executes steps in
  topological order, writes `workflow-plan.json`, and refreshes
  `workflow-run.json` after each step.
- `workflow-run.schema.json` records bounded operational provenance:
  workflow/step IDs, timing, status, inputs, outputs, messages, and errors.
- ABC manifests, request sets, snapshot indexes, artifact IDs, and content
  hashes remain canonical publication identity.
- Root and component flakes remain the primary reproducibility entry points.

This current layer is intentionally conservative. It makes explicit pipelines
inspectable, but it does not yet model target-driven realization, conditional
dependencies, cache reuse, or branch-specific dependency avoidance.

Nodely is relevant because its README describes:

- declarative data dependency graphs;
- conditional dependencies through branch nodes;
- lazy dependency resolution;
- multiple execution engines behind the same graph declaration;
- a recommended public API namespace, `nodely.api.v0`;
- mature `:sync.lazy`, `:core-async.lazy-scheduling`, and virtual-thread
  engines. The current `nodely.api.v0` engine map exposes the applicative
  virtual-thread engine as `:applicative.virtual-future`.

This design targets Nodely 2.0.3 or newer in the `dev.nu/nodely` 2.x line,
where the public API namespace is `nodely.api.v0` and the engine keywords used
below exist. In `nodely.api.v0`, `eval`, `eval-key`, and `eval-key-channel`
default to `:core-async.lazy-scheduling`. The Soranoha facade must therefore
pass an explicit `::nodely/engine :sync.lazy` option for the first slice; using
the arity that accepts no options is forbidden.

Nodely 2.0.0 stopped providing `core.async`, `promesa`, and `manifold`
transitively. The current ABC lock already contains `core.async` through other
dependencies, so the first-slice rule is narrower: do not introduce or rely on
a direct `core.async`, `promesa`, or `manifold` dependency for Nodely.

## Problem

The current workflow runner is a good serial command runner, but some
Soranoha workflows are better described as "realize this target value" than
"run this fixed list of stages":

- build a publication bundle only if a selected work has the required source
  material and policy support;
- produce a report target from whichever intermediate evidence is needed;
- compute a validation/admission decision where the expensive checks depend
  on earlier cheap checks;
- materialize only selected artifacts for a bounded request set;
- explain why an artifact can or cannot be produced from current manifests,
  source snapshots, policies, and Nix tools.

In these cases, fixed serial step plans hide conditional dependencies in step
function bodies. A reviewer must read code to know which inputs are always
required, which are conditional, and which branch skipped an expensive or
impure operation.

## Non-Goals

- Do not replace Nix flakes, derivations, apps, or checks.
- Do not make Nodely graph keys part of `artifact_id`, `request_set_id`,
  `snapshot_identity_hash`, or content hashes.
- Do not make Nodely a cross-language workflow engine for Python, Bash, or
  Rust orchestration.
- Do not use Nodely for corpus-scale fanout until a small Clojure target proves
  the semantics and cost.
- Do not use Nodely's experimental engines in the first slice.
- Do not rely on global exception policy (`with-try`) for domain failures that
  should be explicit failure-as-value records.

## Prior Art Assessment

### Existing Soranoha Runner

The existing runner is best for explicit operational pipelines:

```text
source-snapshot -> resolve-request-set -> materialize -> validate -> report
```

It has a stable schema surface and works across Clojure, Bash, Python, Rust,
and Nix because the run report is just JSON.

Its limitation is that branch choice is invisible unless every step manually
records enough messages. It cannot answer "which target values were needed for
this result?" without domain-specific conventions.

### Nodely

Nodely is best for target-driven Clojure dependency realization:

```clojure
{:cheap-check ...
 :expensive-check ...
 :decision (>if cheap-check expensive-check skipped)}
```

Its conditional dependencies are the important capability. A false branch need
not realize the dependency that only belongs to the true branch.

Its limitations for Soranoha are equally important:

- Nodely has no Soranoha manifest identity model.
- Nodely does not by itself define workflow-run JSON, provenance sidecars, or
  cache-key policy.
- Nodely cycle checking is available, but its README warns it can be costly at
  runtime and can report false positives for mutually exclusive conditions.
- Some useful engines require explicit dependencies and engine-specific
  blocking discipline.

### Redun

Redun remains useful prior art for provenance and caching:

- task/input/code hashing;
- file-value validity;
- cache hit/miss records;
- queryable call graphs.

Soranoha should import those concepts into its own workflow protocol rather
than adopting Redun as the runtime.

## Design Overview

Nodely is introduced as a target planner beneath the Soranoha workflow
protocol.

```text
operator command
  -> Soranoha workflow plan
  -> optional Nodely target realization step
  -> Clojure/Nix materialization tasks
  -> manifests and sidecars
  -> workflow-run.json plus derived query index
```

The outer workflow remains a Soranoha workflow run. A Nodely evaluation is one
or more Soranoha workflow steps, depending on the target size:

- small target: one step records the target, realized keys, skipped branches,
  inputs, outputs, and cache decisions;
- larger target: a wrapper step records the Nodely target and emits child task
  records for realized leaves, bounded by configured limits.

## Components

### `abc.tools.workflow`

Keeps the current runner contract and grows only generic protocol fields:

- `:task` record: stable step version, code hash, declared config hashes;
- `:cache` record: key, status, reason, validation result;
- `:executor` record: leaf backend such as `:clojure`, `:nix`, or `:shell`;
- `:planner` record: optional planner metadata such as `:nodely`;
- `:environment` record: bounded Nix/JVM/tool metadata.

This namespace should not depend on Nodely.

### `abc.tools.workflow.nodely`

New facade namespace that owns all direct Nodely integration.

Responsibilities:

- convert a Soranoha target graph to a Nodely environment;
- choose an engine from Soranoha config;
- run Nodely evaluation;
- record realized and skipped dependency keys in a bounded node-summary
  sequence;
- translate Nodely failures into Soranoha workflow errors or failure-as-value
  messages;
- expose test helpers that evaluate nodes with supplied values.

This is a deep module candidate: it hides a third-party planner and exposes a
Soranoha-specific protocol.

The facade contract must pin all of these semantics:

- every call to `nodely.api.v0/eval`, `eval-key`, `eval-key-channel`,
  `eval-node`, or `eval-node-channel` supplies an explicit engine option;
- first-slice calls always use `:sync.lazy`;
- a facade evaluation returns one ordered node-summary sequence;
- for `:sync.lazy`, node-summary ordering is realization order from the target
  under Nodely's depth-first lazy traversal;
- if a later engine changes ordering, the report records the engine and the
  ordering semantics so provenance readers do not infer serial order;
- large targets may emit a bounded summary plus a debug-only JSONL detail file,
  but the bound and debug flag are public facade options, not hidden
  heuristics.

### Domain Graph Namespaces

Domain namespaces may define graph values, but should not own engine choice or
workflow report writing.

Candidate namespaces:

- `abc.tools.workflow.graphs.publication`
- `abc.tools.workflow.graphs.analysis`
- `abc.tools.workflow.graphs.validation`

Each graph namespace should export pure graph constructors. They receive
configuration values and return data.

### Nix Bridge

The Nix bridge remains explicit. A Nodely leaf may call a Soranoha helper that
realizes a flake app/check/package, but the leaf must return a structured value:

```clojure
{:store-path "/nix/store/..."
 :flake-output "packages.x86_64-linux..."
 :lock-nodes [...]
 :outputs [...]
 :messages [...]}
```

No Nodely key or branch result should be used as a Nix attribute name unless it
has first passed through a Soranoha-owned naming function.

## Data Model

### Workflow Target

A workflow target is operational:

```clojure
{:workflow/id "soranoha.publication-target.v1"
 :target/key :publication-bundle
 :engine :sync.lazy
 :inputs {:request-set-id "sha256:..."
          :output-root "target/soranoha/..."}}
```

It is not artifact identity.

### Node Provenance

Each realized Nodely node should be summarized as:

```json
{
  "key": "publication-bundle",
  "node_type": "leaf",
  "status": "passed",
  "realized": true,
  "inputs": ["request-set", "snapshot-index"],
  "conditional_inputs_skipped": ["tei-validation-report"],
  "duration_ms": 123,
  "cache": {
    "status": "miss",
    "key": "sha256:..."
  }
}
```

The report should be bounded. For corpus-scale sequence nodes, summarize counts
and write detailed task records only when a debug flag is enabled.

Skipped conditional dependencies should be represented as skipped node-summary
records when their keys are known. A skipped record is evidence that a branch
was not realized; it is not an execution attempt.

### Node Result Contract

Every Soranoha-authored Nodely leaf must return one of these logical result
classes:

- `:passed`: a successful value and optional provenance records;
- `:failed-value`: a domain failure-as-value, such as an admission rejection,
  diagnostic row, or failure manifest reference;
- `:skipped`: an explicit local skip where the graph could not express the
  skip as a branch.

Thrown exceptions are reserved for infrastructure failures, programming
errors, invalid values, failed subprocesses, and other cases that should fail
the workflow step. Leaf authors should not throw to represent ordinary domain
rejection. The facade translates `:failed-value` into a realized node with
`status = "partial"` and bounded evidence.

### Step Cache Key

The Soranoha cache key for a Nodely node is:

```text
soranoha-cache-v1
+ workflow_id
+ target_key
+ node_key
+ node_kind
+ graph_version
+ declared leaf implementation version/hash
+ canonical input value hashes
+ content hashes of path inputs
+ relevant schema/policy/profile hashes
+ relevant flake.lock node revs/narHashes
+ engine family
```

The leaf implementation component is declared by the graph constructor or leaf
registry. It is not derived from raw function object identity. The first
implementation should use explicit `:impl/id` and `:impl/version` or a
Soranoha-controlled source/content hash for leaf implementations, and tests
must verify that the value is stable across clj-nix execution and REPL-loaded
test execution.

The engine family is included because realization behavior, traversal order,
exception wrapping, scheduling, and intermediate side effects are
engine-dependent. Cache entries are partitioned by engine to preserve correct
provenance attribution. Separately, release-bound artifacts whose bytes are
identified by canonical manifests must remain content-hash stable for a given
identity; if changing engines changes such bytes, that is a determinism finding
for the materialization path, not a reason to drop `engine family` from the
cache key.

### Output Validity

Cache validity is an enforced protocol, not a checklist. The workflow layer
must provide a function with this shape:

```clojure
(valid-cached-node-result cached-node-result current-env)
;; => {:status :ok}
;; => {:status :stale, :reason "...", :evidence {...}}
;; => {:status :invalid, :reason "...", :evidence {...}}
```

The function is called on cache read before a node result is reused. On
`:stale` or `:invalid`, the node is re-realized. The old cache entry is not
trusted and the workflow report records a cache validation event; a malformed
entry or mismatched identity-bearing input is bad provenance, not a silent miss.

A cached node result returns `:ok` only when:

- all declared output paths exist, if paths are declared;
- output content hashes are recomputed from current paths and match recorded
  hashes;
- referenced manifests still validate;
- any cited Nix store paths still exist or can be substituted;
- any runtime config value declared as identity-relevant still matches.

Path, env, and store references are mutable places. A cached path claim is
always re-hashed or checked against an authoritative lock record on read.
Paths alone are never sufficient.

## Engine Policy

First implementation slice:

- Use only `:sync.lazy`.
- The facade must pass `::nodely/engine :sync.lazy` explicitly on every call
  into `nodely.api.v0`.
- No direct `nodely.api.v0` evaluation call may appear outside the facade.
- Run `checked-env` in tests and development validation, not hot production
  paths.
- Do not introduce or rely on a direct `core.async`, `promesa`, or `manifold`
  dependency just to evaluate the first slice.
- Do not pass an applicative context option in the first slice.

Second slice:

- Consider `:core-async.lazy-scheduling` only for non-blocking IO nodes.
- Any blocking operation must be marked in Soranoha metadata before using a
  core.async engine.
- Add `org.clojure/core.async` explicitly if this engine is used.

Third slice:

- Consider `:applicative.virtual-future` only after the ABC JVM baseline is
  known to be Java 21+ in every supported Nix devShell/check environment.
- Do not use experimental engines for publication materialization.
  The repository already uses JDK 21 in selected Nix surfaces; the gate here is
  stronger because publication workflow behavior must be supported in every
  devShell/check path that can run the engine.

## Nix Policy

Nix remains the materialization and isolation backend:

- add Nodely through Clojure dependency management and regenerate the
  `deps-lock.json` through the existing clj-nix path;
- keep the root and ABC flakes as the checked entry points;
- expose any new workflow commands as explicit flake apps;
- preserve `just validate-migration` as the broad gate;
- do not let Nodely enumerate the full corpus into Nix attrsets.

If a Nodely target needs Nix realization, it should call a small bridge around
explicit flake outputs or already materialized input paths. The bridge records
flake output name, lock data, command args, and resulting paths/hashes.

## Error Model

Four error classes remain distinct:

1. **Graph construction failure**: invalid graph data, unknown target key, bad
   config. No target evaluation starts.
2. **Graph validation failure**: cycle or missing dependency detected by
   Soranoha/Nodely validation. No target evaluation starts.
3. **Node failure**: a realized node throws, exits non-zero, or returns an
   invalid value. The workflow step fails unless the node contract says failure
   is a value.
4. **Domain failure-as-value**: a node successfully produces failure manifests,
   diagnostic rows, or admission rejection. Workflow status is `partial` unless
   the workflow policy declares the result acceptable.

Do not use Nodely's environment-level `with-try` to turn all exceptions into
domain values. It is acceptable for coarse probes, but release workflows need
explicit node-level failure semantics.

The facade owns the node-result translation rule: returned `:failed-value`
records become partial realized nodes; thrown exceptions become node failures.
This prevents each leaf from inventing its own throw-versus-return policy.

## Trust, State, and Identity

Nodely environments are operational values. They may be hashed for cache keys,
but they do not define citable identity.

Canonical identity remains:

- `artifact_id` from manifest identity;
- `request_set_id` from request-set identity;
- `snapshot_identity_hash` from snapshot identity;
- file/content hashes;
- flake lock revisions and nar hashes where exact replay matters.

Workflow/Nodely reports may cite these IDs and hashes. If a workflow report
disagrees with a manifest, request set, or snapshot index, the canonical
artifact wins and the workflow report is treated as bad provenance.

## Candidate First Target

The first Nodely-backed target should be small, Clojure-only, and inspectable:

```text
publication-admission-target
  request-set
  snapshot-index
  source-snapshot-workset
  publication-plan
  optional-layout-report
  optional-validation-report
  admission-decision
```

Why this target:

- It has real conditional dependencies.
- It can run with `:sync.lazy`.
- It can reuse existing ABC code and tests.
- It does not require corpus-scale sequence fanout.
- It can write a useful workflow-run sidecar without changing artifact bytes.

Avoid starting with full corpus materialization. That would test concurrency,
Nix cost, and storage at the same time as the Nodely integration.

## Migration Plan

### Slice 1: Design Probe

- Add Nodely as a dependency in ABC only.
- Add `abc.tools.workflow.nodely` with a tiny facade.
- Build one pure target graph fixture and tests using supplied values.
- Verify `checked-env` behavior in tests when the graph shape permits it; if
  mutually exclusive conditions trigger Nodely's documented false-positive
  cycle detection, record that as a test fixture and keep runtime validation in
  the Soranoha facade.
- Verify that the facade always passes `::nodely/engine :sync.lazy`.
- Verify that no direct `nodely.api.v0` evaluation calls appear outside the
  facade.
- Do not call Nix.

### Slice 2: Workflow Report Integration

- Extend `workflow-run.schema.json` with optional Nodely node summaries.
- Because the current workflow-run schema is closed with
  `additionalProperties: false`, adding node summaries requires a schema
  versioned extension or a new optional field in the existing schema plus
  fixture updates. Do not emit ad hoc report keys before the schema accepts
  them.
- Execute the first target through `run-workflow!` as one workflow step.
- Record realized/skipped node keys and cache status.
- Keep existing outputs unchanged.

### Slice 3: Nix Bridge

- Add one Nodely leaf that realizes an explicit Nix app/check/package through
  a Soranoha bridge.
- Record flake output, lock nodes, store path, and output hashes.
- Confirm no full-corpus attrset enumeration is introduced.

### Slice 4: Cache and Query

- Add Soranoha-owned node cache keys and validity checks.
- Implement `valid-cached-node-result` and make cache reuse call it on read.
- Generate a small derived query index over workflow runs.
- Answer "why did this target realize these nodes?" from JSON/derived index.

## Acceptance Criteria

The Nodely design is accepted for implementation when:

- a spec and ADR-level decision clearly state that Nodely is operational
  planning only;
- the first probe can evaluate a branch without realizing the skipped
  dependency;
- tests can evaluate the same node with supplied values and no side effects;
- every Nodely evaluation goes through the facade with an explicit engine
  option, and first-slice evaluations use `:sync.lazy`;
- `checked-env` is used in tests/development validation;
- `checked-env` false positives for mutually exclusive branches are either
  absent from the first target or captured as a documented limitation fixture;
- `workflow-run.json` remains schema-valid and bounded;
- cache validity is specified as read-time revalidation with `:ok`, `:stale`,
  and `:invalid` outcomes before any cache reuse implementation is accepted;
- artifact bytes and manifest identities are unchanged;
- root validation gates still pass;
- dependency and lockfile changes are reviewed through the existing Nix/clj-nix
  workflow.

## Review Findings

### Deepening Candidate

**Type:** Deepen.

**Evidence:** The workflow/Nodely integration is one cohesive concern: convert
Soranoha target graphs into evaluated values while preserving provenance.
Putting direct Nodely calls in domain commands would scatter engine choice,
failure translation, and provenance reporting.

**Hazards:** The facade must not hide artifact identity, Nix realization, or
failure-as-value policy. It should expose those as explicit Soranoha records.
It must also make engine choice, realization ordering, error translation, and
report granularity public interface concerns.

**Acceptance check:** Accept the facade only if all of these hold:

- the target concern is only Clojure target realization and provenance
  translation, not Nix policy or artifact identity;
- the interface is honest about engine choice, traversal ordering, errors,
  cache validation, and configuration;
- state, time, and identity are explicit through node summaries, cache
  validity records, and canonical artifact references;
- callers can understand and test graph behavior without knowing Nodely
  internals.

### Protocol Design

**Type:** Protocol Design.

**Evidence:** Nodely node records become part of `workflow-run.json`, which is
a cross-language operational contract.

**Hazards:** Schema evolution, bounded records, cache status semantics, and
error classes must be versioned. Do not add unbounded per-work node dumps to
the default report.

### Trust/State Review

**Type:** Trust/State Review.

**Evidence:** Cache reuse and path validity involve mutable external state:
local output roots, Nix store paths, flake locks, and runtime config.

**Hazards:** A stale path or env override must not silently select a different
identity-bearing input. Every reusable path claim needs a hash or authoritative
lock record.

## Open Questions

- Should the first probe live under `abc.tools.workflow.graphs.publication` or
  a narrower experimental namespace?
- Should node summaries be embedded in `workflow-run.json`, stored as
  `workflow-nodes.jsonl`, or both for larger targets?
- Should Soranoha define its own tiny graph data format and compile to Nodely,
  or should initial graph constructors return Nodely nodes directly behind the
  facade?
- How much of a Nodely graph can be hashed stably when nodes contain function
  objects? The first cache key should hash Soranoha-declared graph versions and
  declared leaf implementation versions/hashes, not raw function values.
- Should Nodely node summaries eventually move to
  `workflow-nodes.schema.json` rather than expanding `workflow-run.schema.json`
  further?

## Recommendation

Proceed with a bounded Nodely probe.

Nodely is a good fit for conditional Clojure dependency realization, especially
where Soranoha wants to ask for a target value and avoid unnecessary work.
It should not replace the existing workflow-run schema, Nix checks, manifests,
or cross-language workflow helpers.

The durable design is:

```text
Soranoha protocol owns provenance and cache semantics.
Nodely realizes Clojure target graphs behind a facade.
Nix realizes pinned tools and bounded materialization units.
Manifests and content hashes remain canonical identity.
```
