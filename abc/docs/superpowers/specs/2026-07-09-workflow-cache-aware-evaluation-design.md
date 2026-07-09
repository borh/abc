# Workflow Cache-Aware Evaluation Design

Status: Proposed design — for review before build (2026-07-09)
Date: 2026-07-09
Owner: Soranoha architecture track
Builds on: [Workflow Target-Graph Evaluator Design](2026-07-09-workflow-target-graph-evaluator-design.md)

This spec designs how the standalone cache primitives
(`abc.tools.workflow.cache`) become **cross-run reuse** inside
`abc.tools.workflow.target/eval-target`, and resolves the three design
decisions that reuse forces. It is deliberately paused before implementation:
the impl-identity decision (below) is a trust boundary and deserves a review
gate.

## Decision (proposed)

Make `eval-target` **optionally cache-aware**: when an injectable cache store is
supplied, a **cacheable leaf** may reuse a prior run's value and outputs instead
of re-running its `impl-fn`, provided the cached entry is still valid. Caching is
**opt-in per leaf** and **fails toward correctness** — a leaf that does not
declare an implementation identity is simply never cached (correct-but-slow),
never silently stale (wrong).

Nothing about publication identity changes: manifests, request sets, artifact
IDs, and content hashes remain canonical. The cache is an *operational reuse*
mechanism only. A cache hit must yield exactly the value a fresh evaluation would
have produced — that is the reuse correctness contract, and every design choice
below serves it.

## Context — where caching stands today

- `abc.tools.workflow.cache` provides two **pure primitives with zero production
  callers**: `node-cache-key` (canonical JCS hash of a node's computation
  identity) and `valid-cached-node-result` (re-hashes recorded output paths on
  read → `:ok` / `:stale` / `:invalid`). The evaluator-design spec deliberately
  specified validity *before* building reuse.
- `abc.tools.workflow.target/eval-target` memoizes strictly **within one
  evaluation** (a `memo` atom discarded when the call returns). There is no
  cross-run reuse.
- `leaf` nodes carry `{:kind :leaf :deps [...] :impl {:impl/id kw :impl/fn f}}`.
  The `impl-fn` is an **opaque closure**; there is no implementation hash.
- The `workflow-nodes.schema.json` node record already accepts an optional
  `cache` object with `status ∈ {hit, miss, stale, invalid}`, and
  `report/node-record` already passes a node's `:cache` field through. The
  producing side is simply not built yet.

## Problem — what reuse must buy, and where

Cross-run leaf caching pays off exactly where a leaf is **expensive and
deterministic**: the nix-bridge flake realization and any corpus/content-hashing
leaf. Cheap or non-deterministic leaves must *not* be cached — caching them adds
key-construction cost and staleness risk for no gain. So the design is opt-in,
concentrated on the few costly-but-pure nodes, not a blanket layer.

Three prerequisites are missing before reuse can be wired. Each is a design
decision, addressed below through the Hickey lens (protocol boundary, trust
boundary, value identity).

## Decision 1 — Cache-store protocol (the missing boundary)

**Observation.** There is a key function and a validity function but no
`get`/`put`-by-key seam. Reuse needs one, and per the evaluator spec's own
discipline (the nix `:runner` is injected, never hard-wired) it must be
**injectable** so the kaocha sandbox can substitute an in-memory store.

**Design.** A small protocol, owned by a new reuse layer
(`abc.tools.workflow.cache-eval`), injected via an `eval-target` opt:

```clojure
(defprotocol NodeCacheStore
  (-fetch  [store cache-key]        "Cached entry map, or nil for a miss.")
  (-store! [store cache-key entry]  "Persist entry under cache-key; returns store."))
```

- **Owner:** the reuse layer. The evaluator depends only on the protocol, never
  on a concrete store — same decoupling shape as the runner.
- **Miss semantics:** `-fetch` returns `nil` for absent keys. Miss is a
  *lookup* outcome, distinct from validity (see Decision 4).
- **Test store:** an in-memory `atom`-backed map. **Production store:** a
  content-addressed directory under `AB_DB_ROOT` (per
  [[db-root-is-deployment-config]] — cache blobs are per-deployment bulk
  storage, resolved at the edge, never baked into identity). GC/eviction is
  **out of scope** for the first cut (content-addressed entries are safe to keep
  or prune out-of-band).
- **Absent store:** when no `:cache-store` opt is supplied, `eval-target`
  behaves exactly as today. Caching is purely additive.

Severity of getting this wrong: **blocker** — an implicit store contract is the
classic entangled cache. Keep it a named protocol.

## Decision 2 — Implementation identity (the load-bearing trust boundary)

**Observation.** `node-cache-key` requires an `impl-hash`, but a leaf carries
only `impl-id` (a keyword) and an opaque `impl-fn`. **You cannot reliably
content-hash a Clojure closure.** So the cache cannot *derive* implementation
identity — it must be *declared*.

**Design — fail toward correctness.** A leaf becomes cacheable only by declaring
an explicit `:impl/hash` — a version stamp the author bumps whenever the leaf's
logic changes:

```clojure
(leaf deps impl-id impl-fn {:impl/hash "admit-v3"
                            :cache/inputs  [:request-set :snapshot]   ; dep keys hashed into identity
                            :cache/policy  {"pack-policy" "sha256:…"}}) ; declared policy hashes
```

- **No `:impl/hash` ⇒ not cacheable.** A leaf with no declared implementation
  identity always runs its `impl-fn`. Forgetting the stamp costs a
  recomputation, never a wrong answer. This is the single most important safety
  property in the design: **absence of identity fails to correct-but-slow, never
  to silently-stale.**
- The author is *trusted* to bump `:impl/hash` on a logic change. That trust is
  explicit and localized (one string next to the code it identifies), not hidden.
- Deferred alternative: derive the hash from the leaf's *source file* content
  (via a build-time macro capturing `*file*`+form). More automatic, but couples
  cache identity to file layout and whitespace and still misses cross-file
  callees. Recommend the declared stamp first; revisit source-derivation only if
  forgotten bumps become a real incident.

Severity: **blocker / trust-boundary.** This is the decision that most warrants
the human review gate — hence the pause.

## Decision 3 — Value identity of inter-leaf dependencies

**Observation.** `input_value_hashes` needs canonical hashes of the values a leaf
consumes from its dependencies, but `eval-target` today passes **arbitrary
Clojure values** between leaves with no serializability requirement.

**Design.** A cacheable leaf's declared `:cache/inputs` dep values must be
**JCS-canonicalizable** (JSON-able). The reuse layer projects each to
`format-sha256(sha256-json-jcs value)` and feeds the map to `node-cache-key` as
`input-value-hashes`. Values that are not JSON-able cannot feed a cacheable leaf
— enforced with a clear error at key-construction time, not a silent
mis-hash. Non-cacheable leaves are unaffected and may pass any value.

Severity: **strong suggestion** — the constraint is real but narrow (only the
declared cache inputs of opt-in leaves), and an explicit error keeps it honest.

## Decision 4 — The status-vocabulary bridge (resolves the flagged Item 1 seam)

The memory note "map `:ok → hit`" is **incomplete and slightly misleading**.
Validity (`:ok`/`:stale`/`:invalid`) and store-lookup (`hit`/`miss`) are
**different layers**. The complete, correct bridge:

| situation                          | schema `cache.status` |
|------------------------------------|-----------------------|
| key absent from store              | `miss`                |
| present + validity `:ok`           | `hit`                 |
| present + validity `:stale`        | `stale`               |
| present + validity `:invalid`      | `invalid`             |

`miss` has **no source in the validity vocabulary at all** — it is purely a
lookup outcome. The bridge is defined **once**, in the reuse layer, and tested:

```clojure
(defn lookup->cache-status
  "Bridge (store lookup + validity) to the schema cache.status vocabulary."
  [cached validity]
  (cond
    (nil? cached)                  "miss"
    (= :ok (:status validity))     "hit"
    :else                          (name (:status validity)))) ; "stale" | "invalid"
```

This function is what feeds a node summary's `:cache {:status …}` field, which
`report/node-record` already carries through to the schema. It stays out of
`abc.tools.workflow.cache` (which remains pure validity) — the vocabulary bridge
belongs at the boundary where the two layers actually meet.

## Cache-aware evaluation flow

Within `eval-target`'s `realize`, the `:leaf` branch gains a cacheable path
(only when a `:cache-store` opt is present **and** the leaf declares
`:impl/hash`):

1. Resolve declared `:cache/inputs` deps (already realized via the normal
   recursion) and project them to `input-value-hashes`.
2. Build the identity map and `node-cache-key` from: workflow/target/node keys,
   `graph-version` (an `eval-target` opt), `node-kind`, `impl-id`,
   declared `:impl/hash`, `input-value-hashes`, declared path/policy hashes.
3. `-fetch` the entry. Run `valid-cached-node-result` against the eval env
   (`:base-dir`, identity-relevant `:config`).
4. Compute `status = (lookup->cache-status cached validity)`.
   - `"hit"` → **reuse** the cached value + outputs; **skip** `impl-fn`.
   - `"miss"` / `"stale"` / `"invalid"` → run `impl-fn`, materialize outputs,
     `-store!` a fresh entry.
5. Record `:cache {:status status}` (and optionally `:key`) on the node summary.

The value and status a node contributes are **identical** whether hit or
recomputed — verified by test (evaluate twice with a shared store; assert equal
`:value`, equal `:status`, and second-run summaries report `"hit"`). This is the
reuse correctness contract made executable.

## Invariants preserved

- **Publication identity stays canonical** — the cache never becomes a source of
  artifact identity; manifests/hashes remain authoritative.
- **Provenance stays order-independent** — the cache changes *whether* a leaf's
  `impl-fn` runs, never the node-set/edge-list shape.
- **Status enum unchanged** — node/workflow status stays
  `passed/failed/partial/skipped`; `cache.status` is an orthogonal axis.
- **Additive** — no cache store ⇒ byte-identical behavior to today.

## Trust and failure modes

- **Author forgets to bump `:impl/hash`** → stale reuse. *Mitigated structurally*
  by making identity required-to-cache: the failure mode of forgetting is
  no-cache (Decision 2), and a bump-on-change discipline only affects leaves that
  opted in.
- **Corrupted/edited output on disk** → `valid-cached-node-result` re-hash
  catches it (`:stale`/`:invalid`) → recompute.
- **Identity-relevant config drift** → `:stale` → recompute.
- **Store unavailable** → treat as miss; recompute. Never fail the target for a
  cache-layer error.

## Proposed slice plan (for the follow-on build)

- **Slice A — Reuse boundary.** `NodeCacheStore` protocol + in-memory test store
  + `lookup->cache-status` bridge (Decision 4). Pure/tested; no `eval-target`
  change. *(Closes the Item 1 seam properly, with a caller.)*
- **Slice B — Leaf identity.** Extend `leaf` with the optional 4th options map
  (`:impl/hash`, `:cache/inputs`, `:cache/policy`); add the `value-hash` helper
  (Decision 3) and a `leaf->cache-key` builder over `node-cache-key`. Pure/tested.
- **Slice C — Cache-aware `eval-target`.** Wire the flow above behind the
  `:cache-store`/`:graph-version` opts; add the `:cache` node-summary field;
  the twice-evaluate reuse-correctness test.
- **Slice D — Production store + end-to-end report.** Content-addressed store
  under `AB_DB_ROOT`; feed the `:cache` field through `report/node-record` to the
  schema; confirm `just schema-drift` green (schema already supports the field —
  no schema edit expected, so the four-place registration gotcha should not
  trigger; verify).

## Open questions for review

1. **impl-identity model** — declared `:impl/hash` stamp (recommended,
   fail-safe) vs. source-file-derived hash? *Load-bearing; needs your call.*
2. **Scope of caching** — leaves only (recommended; branches are routing, value
   nodes are inputs), or ever cache a branch/value node?
3. **Store location** — a dedicated `AB_DB_ROOT/workflow-cache/` subtree, or
   fold into an existing bulk-storage layout?
4. **GC/eviction** — confirm out-of-scope for the first cut (content-addressed,
   pruned out-of-band).

## Alternatives considered

- **Wire reuse without an impl-identity decision** (hash `impl-id` only) —
  rejected: `impl-id` is a stable name, not a version; it would go stale on every
  logic edit that keeps the id. Identity must track *logic*, and since logic
  can't be auto-hashed, it must be declared.
- **Blanket caching of all leaves** — rejected: adds key-construction cost and
  staleness surface to cheap/non-deterministic nodes for no benefit. Opt-in
  concentrates caching where the payoff is.
- **Cache the whole target value** (one entry per target) — rejected for the
  first cut: coarse invalidation (any input change busts everything) throws away
  the branch-skip and per-leaf reuse the evaluator already models. Per-leaf reuse
  composes with the existing DAG.
