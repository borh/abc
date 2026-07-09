# Workflow Target-Graph Evaluator Design

Status: Proposed design — revised 2026-07-09 (core decision reversed after design review)
Date: 2026-07-09
Owner: Soranoha architecture track
Supersedes: earlier "Nodely Workflow Provenance Design" at this path. Nodely was
evaluated as a candidate backend and **deferred**, not adopted. See
[Alternatives Considered](#alternatives-considered).

This spec designs a small, Soranoha-owned **target-graph evaluator** for
target-driven workflow realization, and records why an external dataflow library
([Nodely](https://github.com/nubank/nodely)) was evaluated and deferred rather
than adopted.

## Decision

Build a minimal Soranoha-owned target-graph evaluator, `abc.tools.workflow.target`,
that realizes requested workflow **targets** from a Soranoha-owned graph **data
value** using internal lazy (delay-based) evaluation with conditional (branch)
dependencies. Do **not** add Nodely or any external dataflow/dependency-graph
library now.

Rationale: the only capability an external engine library buys over roughly a
screen of internal lazy evaluation is *multiple execution engines behind one
graph declaration*. Soranoha's engine need is a single synchronous lazy evaluator
for every foreseeable slice. Adopting a library would add a runtime dependency,
an engine axis in the cache key, that library's own documented false-positive
cycle detection (which forces us to own graph validation anyway), and transitive
dependency management — all to buy flexibility we would not use. Keeping the graph
as **data** behind an evaluator protocol means Nodely (or any engine) can be
adopted later, behind the same seam, the day a concrete concurrency/scheduling
need appears.

The design remains three layers:

1. **Soranoha workflow protocol** (`abc.tools.workflow`) owns stable step records,
   provenance, cache-key construction, validity checking, workflow reports, and
   schema compatibility.
2. **Soranoha target-graph evaluator** (`abc.tools.workflow.target`) realizes
   requested targets from a Soranoha graph data value, especially when conditional
   dependencies matter. It owns graph validation (cycle/missing-dependency), lazy
   realization, branch skipping, and node/edge provenance construction.
3. **Nix materialization backend** owns pinned tools, flake inputs, store
   realization, and reproducible build/check isolation.

Domain namespaces produce graph *data*; the evaluator realizes it. No evaluator
internals should scatter across domain namespaces.

## Context

Soranoha already has a small workflow layer:

- `abc.tools.workflow/run-workflow!` validates a step plan, executes steps in
  topological order, writes `workflow-plan.json`, and refreshes
  `workflow-run.json` after each step.
- `workflow-run.schema.json` records bounded operational provenance:
  workflow/step IDs, timing, status (`passed`/`failed`/`partial`/`skipped`),
  inputs, outputs, messages, and errors. The schema is closed
  (`additionalProperties: false`) at the top level and on every nested object.
- ABC manifests, request sets, snapshot indexes, artifact IDs, and content
  hashes remain canonical publication identity.
- Root and component flakes remain the primary reproducibility entry points.

This current layer is intentionally conservative. It makes explicit pipelines
inspectable, but it does not yet model target-driven realization, conditional
dependencies, cache reuse, or branch-specific dependency avoidance.

## Problem

The current workflow runner is a good serial command runner, but some Soranoha
workflows are better described as "realize this target value" than "run this
fixed list of stages":

- build a publication bundle only if a selected work has the required source
  material and policy support;
- produce a report target from whichever intermediate evidence is needed;
- compute a validation/admission decision where the expensive checks depend on
  earlier cheap checks;
- materialize only selected artifacts for a bounded request set;
- explain why an artifact can or cannot be produced from current manifests,
  source snapshots, policies, and Nix tools.

In these cases, fixed serial step plans hide conditional dependencies in step
function bodies. A reviewer must read code to know which inputs are always
required, which are conditional, and which branch skipped an expensive or impure
operation. A target graph makes those conditional dependencies data.

## Non-Goals

- Do not replace Nix flakes, derivations, apps, or checks.
- Do not make graph node keys part of `artifact_id`, `request_set_id`,
  `snapshot_identity_hash`, or content hashes.
- Do not make the evaluator a cross-language workflow engine for Python, Bash, or
  Rust orchestration.
- Do not use the evaluator for corpus-scale fanout until a small Clojure target
  proves the semantics and cost.
- Do not add an external dataflow/dependency-graph dependency for the first
  implementation.
- Do not use a global catch-all exception policy for domain failures that should
  be explicit failure-as-value records.

## The Internal Evaluator

Design the evaluator as **data plus a tiny interpreter**.

A graph is a map of node-key to node. Node kinds:

- `:value` — a supplied input or constant.
- `:leaf` — `{:deps [k ...] :impl <declared-impl>}`; realizes a value from its
  resolved dependencies.
- `:branch` — `{:cond k :then k :else k}`; realizes only the taken side's
  dependency.

Evaluation:

- `realize(target)` resolves dependencies lazily and memoizes each node. A
  `:branch` forces `:cond`, then realizes only the selected dependency. The
  unselected dependency is recorded as a **skipped** node when its key is known.
- Ordering is an internal detail. Provenance is a **DAG value** — a set of node
  summaries plus an edge list — not an execution sequence. Nothing downstream may
  infer execution order from provenance.
- Graph validation (missing dependency, cycle) is Soranoha-owned and
  deterministic. We do not depend on a third party's heuristic cycle check, which
  removes the documented false-positive hazard for mutually exclusive branches.
- Leaf implementations are declared with `:impl/id` plus a Soranoha-controlled
  source/content hash, never raw function-object identity.
- Pure by construction for the first target: leaves are functions of resolved
  dependencies and supplied inputs. Side effects (Nix) are confined to explicit
  bridge leaves. Tests can evaluate any node with supplied values and no side
  effects.

Evaluator protocol (one implementation now, swappable later):

```clojure
(eval-target graph target inputs opts)
;; => {:value       <realized target value>
;;     :nodes       [node-summary ...]   ; keyed by node key, unordered
;;     :edges       [[from-key to-key] ...]}
```

`inputs` supplies `:value` node values by key; `opts` carries evaluation policy.

`opts` carries evaluation policy. A future backend (for example Nodely) would
implement this same protocol; that is the only place a backend choice would live.

## Components

### `abc.tools.workflow`

Keeps the current runner contract and grows only generic protocol fields:

- `:task` record: stable step version, code hash, declared config hashes;
- `:cache` record: key, status, reason, validation result;
- `:executor` record: leaf backend such as `:clojure`, `:nix`, or `:shell`;
- `:planner` record: optional planner metadata such as `:internal-lazy`;
- `:environment` record: bounded Nix/JVM/tool metadata.

This namespace does not depend on the evaluator internals.

### `abc.tools.workflow.target`

New namespace that owns the internal evaluator.

Responsibilities:

- interpret a Soranoha target graph (data) into realized values;
- own graph validation: missing dependency and cycle detection, deterministic;
- realize lazily, skipping the unselected side of each branch;
- record realized and skipped node keys as a bounded node-summary set plus an
  edge list;
- translate leaf failures into Soranoha workflow errors or failure-as-value
  records;
- expose test helpers that evaluate nodes with supplied values.

This is a deep module: it hides realization and exposes a Soranoha-specific
protocol. Its interface is honest about validation, error translation, provenance
shape, and configuration; it holds no artifact identity, Nix policy, or
failure-as-value policy that a caller cannot see.

### Domain Graph Namespaces

Domain namespaces may define graph values but should not own realization or
workflow-report writing.

Candidate namespaces:

- `abc.tools.workflow.graphs.publication`
- `abc.tools.workflow.graphs.analysis`
- `abc.tools.workflow.graphs.validation`

Each exports pure graph constructors: they receive configuration values and
return graph data.

### Nix Bridge

The Nix bridge remains explicit. A leaf may call a Soranoha helper that realizes
a flake app/check/package, but the leaf must return a structured value:

```clojure
{:store-path   "/nix/store/..."
 :flake-output "packages.x86_64-linux..."
 :lock-nodes   [...]
 :outputs      [...]
 :messages     [...]}
```

No node key or branch result may be used as a Nix attribute name unless it has
first passed through a Soranoha-owned naming function.

## Data Model

### Workflow Target

A workflow target is operational, not artifact identity:

```clojure
{:workflow/id "soranoha.publication-target.v1"
 :target/key  :publication-bundle
 :evaluator   :internal-lazy
 :inputs      {:request-set-id "sha256:..."
               :output-root    "target/soranoha/..."}}
```

### Node Provenance

Provenance is a **set of node summaries keyed by node key, plus an edge list**.
It is not an ordered sequence and carries no implied execution order.

```json
{
  "key": "publication-bundle",
  "node_type": "leaf",
  "status": "passed",
  "realized": true,
  "inputs": ["request-set", "snapshot-index"],
  "conditional_inputs_skipped": ["tei-validation-report"],
  "duration_ms": 123,
  "cache": { "status": "miss", "key": "sha256:..." }
}
```

The report is bounded. For corpus-scale sequence nodes, summarize counts and
write detailed records only when a debug flag is enabled. Skipped conditional
dependencies are skipped node-summary records when their keys are known; a skipped
record is evidence that a branch was not realized, not an execution attempt.

### Node Result Contract

Every Soranoha-authored leaf returns one of:

- `:passed` — a successful value and optional provenance records;
- `:failed-value` — a domain failure-as-value (admission rejection, diagnostic
  row, failure manifest reference);
- `:skipped` — an explicit local skip the graph could not express as a branch.

Thrown exceptions are reserved for infrastructure failures, programming errors,
invalid values, failed subprocesses, and other cases that should fail the
workflow step. Leaf authors do not throw to represent ordinary domain rejection.
The evaluator translates `:failed-value` into a realized node with
`status = "partial"` and bounded evidence.

### Status Vocabulary

Node status and workflow status share one enum: `passed`, `failed`, `partial`,
`skipped` (the enum `workflow-run.schema.json` already uses). The node-to-workflow
rollup is an explicit named function, not prose:

```clojure
(roll-up-status node-statuses policy)
;; any node failed  -> workflow failed  (unless policy accepts the failure)
;; else any partial -> workflow partial (unless policy accepts the partial)
;; else if all realized nodes passed -> passed
;; else (nothing realized) -> skipped
```

A `partial` *node* does not automatically make a `partial` *workflow*; workflow
policy decides.

### Cache Model

Two distinct concerns, kept separate rather than one uniform key:

**(a) Pure node-value cache** — only for pure leaves (functions of inputs). Key:

```text
soranoha-cache-v1
+ workflow_id
+ target_key
+ node_key
+ node_kind
+ graph_version
+ declared leaf impl id + source/content hash
+ canonical input value hashes
+ content hashes of path inputs
+ relevant schema/policy/profile hashes
```

There is no engine axis: there is one internal evaluator, and a pure leaf's value
does not depend on evaluation order. If a second backend is ever added and could
change bytes, that is a determinism finding for that backend, resolved when the
backend is added — not a reason to add an engine axis now.

**(b) Materialization validity check** — for impure/Nix-bridge leaves. There is no
separate value cache; the authoritative record is the Nix store plus `flake.lock`
(already content-addressed). Reuse is gated by `valid-cached-node-result` on read.

Leaf implementation identity is a Soranoha-declared `:impl/id` plus a
source/content hash — automatable, so it cannot silently drift when logic changes
— never a raw function object identity, and never a hand-maintained version
integer alone. Tests verify the value is stable across clj-nix execution and
REPL-loaded test execution.

### Output Validity

Cache validity is an enforced protocol, not a checklist:

```clojure
(valid-cached-node-result cached-node-result current-env)
;; => {:status :ok}
;; => {:status :stale,   :reason "...", :evidence {...}}
;; => {:status :invalid, :reason "...", :evidence {...}}
```

Called on cache read before a node result is reused. On `:stale` or `:invalid`,
the node is re-realized, the old entry is not trusted, and the workflow report
records a cache validation event. A malformed entry or mismatched
identity-bearing input is bad provenance, not a silent miss.

A cached node result returns `:ok` only when:

- all declared output paths exist, if paths are declared;
- output content hashes recomputed from current paths match recorded hashes;
- referenced manifests still validate;
- any cited Nix store paths still exist or can be substituted;
- any runtime config value declared identity-relevant still matches.

Path, env, and store references are mutable places. A cached path claim is always
re-hashed or checked against an authoritative lock record on read. Paths alone are
never sufficient.

## Evaluator Policy

- One evaluator: `:internal-lazy`, synchronous, deterministic.
- Graph validation is Soranoha-owned; it runs in tests and development validation,
  and cheaply on every run (graphs are small).
- No external dataflow dependency, and therefore no `deps-lock.json` regeneration
  for the evaluator itself.
- Future backends (for concurrency or virtual-thread scheduling) are added behind
  the same `eval-target` protocol only when a concrete need exists. Adding one
  re-opens the "engine in the cache key" question, scoped to that backend, at that
  time — not before.

## Nix Policy

Nix remains the materialization and isolation backend:

- keep the root and ABC flakes as the checked entry points;
- expose any new workflow commands as explicit flake apps;
- preserve `just validate-migration` as the broad gate;
- do not let the evaluator enumerate the full corpus into Nix attrsets.

If a target needs Nix realization, it calls a small bridge around explicit flake
outputs or already-materialized input paths. The bridge records flake output name,
lock data, command args, and resulting paths/hashes.

## Error Model

Four error classes remain distinct:

1. **Graph construction failure**: invalid graph data, unknown target key, bad
   config. No target evaluation starts.
2. **Graph validation failure**: cycle or missing dependency detected by
   Soranoha's own validation. No target evaluation starts.
3. **Node failure**: a realized node throws, exits non-zero, or returns an invalid
   value. The workflow step fails unless the node contract says failure is a value.
4. **Domain failure-as-value**: a node successfully produces failure manifests,
   diagnostic rows, or admission rejection. Workflow status is `partial` unless the
   workflow policy declares the result acceptable.

The evaluator owns the node-result translation rule: returned `:failed-value`
records become partial realized nodes; thrown exceptions become node failures.
This prevents each leaf from inventing its own throw-versus-return policy.

## Trust, State, and Identity

Evaluator graphs are operational values. They may be hashed for cache keys, but
they do not define citable identity.

Canonical identity remains:

- `artifact_id` from manifest identity;
- `request_set_id` from request-set identity;
- `snapshot_identity_hash` from snapshot identity;
- file/content hashes;
- flake lock revisions and nar hashes where exact replay matters.

Workflow reports may cite these IDs and hashes. If a workflow report disagrees
with a manifest, request set, or snapshot index, the canonical artifact wins and
the workflow report is treated as bad provenance.

**Exit / removal.** The evaluator is isolated behind `eval-target` and adds no
dependency. If target-driven realization proves unnecessary, delete
`abc.tools.workflow.target` and the graph namespaces; there is no identity or
schema rollback, because identity stayed canonical and node summaries are an
additive sidecar. This bounded removal cost is the accepted risk control for
introducing the evaluator at all.

## Candidate First Target

The first target should be small, Clojure-only, and inspectable:

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
- It runs with `:internal-lazy`.
- It can reuse existing ABC code and tests.
- It does not require corpus-scale sequence fanout.
- It can write a useful workflow-run sidecar without changing artifact bytes.

Avoid starting with full corpus materialization; that would test concurrency, Nix
cost, and storage at the same time as the evaluator integration.

## Report Location Decision

Node summaries go to a **separate `workflow-nodes.jsonl` sidecar** with its own
`workflow-nodes.schema.json`, referenced from `workflow-run.json` by `run_id`
plus one small additive, versioned, bounded block (a `node_summary_ref` and
counts). This keeps `workflow-run.json` bounded by construction and avoids adding
unbounded arrays to its closed object shape. The `workflow-run` schema receives
only a versioned optional field, with fixtures updated. No ad hoc report keys are
emitted before the schema accepts them.

## Migration Plan

### Slice 1: Internal evaluator + pure target fixture

- Add `abc.tools.workflow.target`: graph data model, deterministic validation,
  lazy realization, branch skipping, node/edge provenance.
- Build one pure target-graph fixture and tests using supplied values, no side
  effects, no Nix.
- Verify a false branch does not realize the skipped dependency.
- Verify Soranoha-owned validation catches a missing dependency and a cycle,
  including a mutually-exclusive-branch fixture that a naive heuristic would flag.
- No dependency changes; no `deps-lock.json` regeneration.

### Slice 2: Workflow report integration

- Add `workflow-nodes.schema.json`; add a bounded, versioned optional reference
  field to `workflow-run.schema.json`, with fixture updates.
- Execute the first target through `run-workflow!` as one workflow step; write
  `workflow-nodes.jsonl`.
- Record realized/skipped node keys and cache status.
- Keep existing outputs unchanged; `workflow-run.json` stays schema-valid and
  bounded.

### Slice 3: Nix bridge

- Add one leaf that realizes an explicit Nix app/check/package through a Soranoha
  bridge.
- Record flake output, lock nodes, store path, and output hashes.
- Confirm no full-corpus attrset enumeration is introduced.

### Slice 4: Cache and query

- Add the pure node-value cache and `valid-cached-node-result`; make cache reuse
  call it on read. Keep the pure-value cache and materialization validity check
  separate.
- Generate a small derived query index over workflow runs.
- Answer "why did this target realize these nodes?" from JSON/derived index.

## Acceptance Criteria

- a spec and ADR-level decision clearly state the evaluator is operational
  planning only, internal, with no external dependency;
- the first probe can evaluate a branch without realizing the skipped dependency;
- tests can evaluate the same node with supplied values and no side effects;
- Soranoha-owned graph validation catches missing dependencies and cycles
  deterministically, with a documented mutually-exclusive-branch fixture;
- provenance is a node-set plus edge-list; nothing infers execution order from it;
- `workflow-run.json` remains schema-valid and bounded; node detail lives in
  `workflow-nodes.jsonl`;
- cache validity is specified as read-time revalidation with `:ok`, `:stale`, and
  `:invalid` before any cache reuse implementation is accepted, and the pure-value
  cache is kept separate from materialization validity;
- leaf implementation identity is a declared id plus source/content hash, stable
  across clj-nix and REPL execution (tested);
- the node-to-workflow status rollup is an explicit function;
- artifact bytes and manifest identities are unchanged; root validation gates pass;
- any future backend is added behind `eval-target`, re-opening the engine-in-key
  question scoped to that backend.

## Alternatives Considered

### Nodely (evaluated, deferred)

[Nodely](https://github.com/nubank/nodely) is a Clojure-native declarative data
dependency graph library: conditional dependencies through branch nodes, lazy
resolution, and multiple execution engines behind one graph declaration
(`:sync.lazy`, `:core-async.lazy-scheduling`, virtual-thread), with a public API
namespace `nodely.api.v0`.

Its conditional-dependency capability is genuinely the right shape for this
problem, and `core.async` is already a transitive dependency in `deps-lock.json`,
so that specific cost is low. It was nonetheless **deferred**, not adopted,
because:

- the sole advantage over a small internal lazy evaluator — multiple engines — is
  unused for every foreseeable slice (a single synchronous lazy evaluator suffices);
- Nodely's own README warns its cycle check can be costly and can report false
  positives for mutually exclusive conditions, so Soranoha must own graph
  validation anyway;
- adopting it adds a runtime dependency, an `engine family` axis in the cache key
  to partition provenance, version pinning to the 2.x line, and transitive-dependency
  discipline (2.0 stopped providing `core.async`/`promesa`/`manifold` transitively).

Net value is negative *now*. Because the graph is kept as data behind
`eval-target`, adopting Nodely later — the day a concrete concurrency or
virtual-thread scheduling need appears — is a bounded change behind one seam, and
this deferral is cheap to revisit.

### Redun (prior art)

Redun remains useful prior art for provenance and caching: task/input/code
hashing, file-value validity, cache hit/miss records, and queryable call graphs.
Import those *concepts* into the Soranoha workflow protocol rather than adopting
Redun as the runtime.

### Existing Soranoha runner (retained)

The existing `run-workflow!` runner is best for explicit operational pipelines and
works across languages because the run report is just JSON. The evaluator sits
*beneath* it as one or more target-realization steps, not as a replacement.

## Review Findings

### Deepening Candidate

**Type:** Deepen.

**Evidence:** The target-realization/provenance integration is one cohesive
concern: convert Soranoha target graphs into evaluated values while preserving
provenance. Scattering realization or engine choice into domain commands would
braid it across namespaces.

**Resolution of the one-implementation-seam objection:** the seam
(`eval-target`) is justified not by a second backend today but by keeping the
graph as *data*, which is what makes validation, provenance, and testing
Soranoha-owned. There is a single honest implementation now; no premature second
backend is introduced. If and when a second backend (Nodely) is added, the seam
already has present variation.

**Acceptance check (all hold):**

- the concern is Clojure target realization and provenance translation, not Nix
  policy or artifact identity;
- the interface is honest about validation, traversal-independence of provenance,
  errors, cache validation, and configuration;
- state, time, and identity are explicit through node summaries, cache validity
  records, and canonical artifact references;
- callers can understand and test graph behavior from graph data alone.

### Protocol Design

**Type:** Protocol Design.

**Evidence:** Node records become part of a cross-language operational contract
(`workflow-run.json` plus `workflow-nodes.jsonl`).

**Hazards addressed:** schema evolution is versioned and additive; node detail is
a bounded sidecar, not an unbounded array in the closed `workflow-run` schema;
provenance is order-independent so no consumer couples to traversal order.

### Trust/State Review

**Type:** Trust/State Review.

**Evidence:** Cache reuse and path validity involve mutable external state: local
output roots, Nix store paths, flake locks, runtime config.

**Hazards addressed:** the pure-value cache is separated from the materialization
validity check; every reusable path claim is re-hashed or checked against an
authoritative lock record on read; a stale path or env override cannot silently
select a different identity-bearing input.

## Open Questions

Resolved by this revision: report location (separate `workflow-nodes.jsonl`
sidecar); whether Soranoha owns a graph data format (yes — the evaluator
interprets Soranoha graph data); leaf implementation hashing (declared `:impl/id`
plus source/content hash).

Remaining:

- Should the first probe live under `abc.tools.workflow.graphs.publication` or a
  narrower experimental namespace?
- What is the exact shape of the bounded node-summary reference block in
  `workflow-run.json` (field name, counts, ref)?

## Recommendation

Proceed with a bounded internal target-graph evaluator. Keep the graph as data so
Nodely — or another engine — can be adopted later behind `eval-target` if a
concrete concurrency/scheduling need appears.

```text
Soranoha protocol owns provenance and cache semantics.
The internal evaluator realizes Clojure target graphs from graph data.
Nix realizes pinned tools and bounded materialization units.
Manifests and content hashes remain canonical identity.
```
