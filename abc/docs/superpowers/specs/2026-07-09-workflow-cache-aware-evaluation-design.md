# Workflow Cache-Aware Evaluation Design

Status: Proposed design — revised 2026-07-09 after Hickey + simplification review (see [Review outcome](#review-outcome-what-the-lenses-changed))
Date: 2026-07-09
Owner: Soranoha architecture track
Builds on: [Workflow Target-Graph Evaluator Design](2026-07-09-workflow-target-graph-evaluator-design.md)

This spec designs how the standalone cache primitives
(`abc.tools.workflow.cache`) become **cross-run reuse** inside
`abc.tools.workflow.target/eval-target`, and resolves the three design
decisions that reuse forces. It is deliberately paused before implementation:
the impl-identity decision (below) is a trust boundary and deserves a review
gate.

## Review outcome — what the lenses changed

This spec was reviewed with `rich-hickey-review` and `codebase-simplification`.
The lenses **tightened** it rather than overturning it. Changes folded in below:

- **Build is gated on a measured hot leaf (Finding 1).** The cache has zero
  callers today, and the obvious candidate — nix flake realization — is already
  content-addressed by the nix store (a rebuild is a store hit). Before building,
  identify *one* expensive, deterministic, **uncached** leaf and measure it. If
  none clears the bar, this design rests specified-but-unbuilt.
- **Store is an injected value/fn-pair, not a `defprotocol` (Finding 2).** The
  established precedent — the nix-bridge `:runner` — is a plain injected function,
  and a filesystem cache store *runs in the kaocha sandbox* (unlike nix), so no
  protocol is needed for testability. One real backend + a test double is not
  present variation.
- **Identity hashes ALL resolved deps, never a hand-declared subset (Finding
  3).** A declared input subset can silently narrow identity → wrong reuse, the
  one failure the design exists to prevent. Non-serializable dep ⇒ leaf not
  cacheable (same fail-safe as impl-hash).
- **Slice plan collapses to one thin vertical slice (Finding 6).** Cache the one
  measured leaf end-to-end; generalize only on the second beneficiary.

Decisions that **survived** review unchanged: declared `:impl/hash` stamp
(simpler and fail-safe vs. source-derived), and leaves-only scope.

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

## Decision 1 — Cache-store seam: an injected value, not a protocol

**Observation.** There is a key function and a validity function but no
`get`/`put`-by-key seam. Reuse needs one, injectable so tests can substitute a
trivial store. The evaluator spec's precedent is the nix-bridge `:runner` — a
**plain injected function**, deliberately *not* a protocol.

**Design.** Inject a store *value* via an `eval-target` opt — the minimum that
carries a `fetch`/`store!` pair:

```clojure
;; opts: {:cache-store {:fetch  (fn [cache-key] entry-or-nil)   ; nil = miss
;;                      :store! (fn [cache-key entry] ...)}}
```

- **No `defprotocol`.** A filesystem cache store *runs in the kaocha sandbox*
  (the cache tests already do real temp-dir I/O), unlike nix — so there is no
  can't-run-offline pressure forcing polymorphism. One real backend plus a test
  double is not present variation; a protocol here would be an abstraction paying
  no rent, and inconsistent with the runner next door. Promote to a protocol the
  day a second real backend or a dispatch need appears.
- **Miss semantics:** `fetch` returns `nil` for absent keys. Miss is a *lookup*
  outcome, distinct from validity (see Decision 4).
- **Test store:** an in-memory `atom`-backed map behind the same two fns.
  **Production store:** a content-addressed directory under `AB_DB_ROOT` (per
  [[db-root-is-deployment-config]] — cache blobs are per-deployment bulk storage,
  resolved at the edge, never baked into identity). GC/eviction is **out of
  scope** for the first cut.
- **Absent store:** when no `:cache-store` opt is supplied, `eval-target`
  behaves exactly as today. Caching is purely additive.

Severity of getting this wrong: **strong suggestion** — an implicit store
contract would be an entangled cache, but the honest, minimal seam is the fn-pair,
not a named type.

## Decision 2 — Implementation identity (the load-bearing trust boundary)

**Observation.** `node-cache-key` requires an `impl-hash`, but a leaf carries
only `impl-id` (a keyword) and an opaque `impl-fn`. **You cannot reliably
content-hash a Clojure closure.** So the cache cannot *derive* implementation
identity — it must be *declared*.

**Design — fail toward correctness.** A leaf becomes cacheable only by declaring
an explicit `:impl/hash` — a version stamp the author bumps whenever the leaf's
logic changes:

```clojure
(leaf deps impl-id impl-fn {:impl/hash "admit-v3"})  ; the ONLY cache declaration
```

The declaration is a single string. Everything else in identity is *derived*
from the leaf's already-resolved dependencies (Decision 3), not re-declared —
so there is no second list to keep in sync.

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

## Decision 3 — Value identity: hash ALL resolved deps, never a declared subset

**Observation.** `input_value_hashes` needs canonical hashes of the values a leaf
consumes, but `eval-target` today passes **arbitrary Clojure values** between
leaves with no serializability requirement.

**Design.** Identity covers **every** value the leaf's `:deps` resolve to — the
evaluator has already realized them. The reuse layer projects each to
`format-sha256(sha256-json-jcs value)` and feeds the whole map to
`node-cache-key` as `input-value-hashes`. There is **no hand-declared input
subset**: a subset lets identity silently *narrow*, so a leaf would reuse a stale
result when an un-listed dep changed — wrong reuse, the exact failure this design
exists to prevent (this is why the earlier `:cache/inputs` idea was rejected in
review). If any dep value is not JCS-canonicalizable, the leaf is **not
cacheable** — the same fail-safe as a missing `:impl/hash` (correct-but-slow,
never silently-stale) — surfaced as a clear error at key-construction time, not a
silent mis-hash. Non-cacheable leaves are unaffected and may pass any value.

**One source of identity (Finding 4).** The reuse layer populates
`node-cache-key` from a *single* resolved-inputs projection. Keep
`path_content_hashes` as a distinct channel only where the *mechanism* genuinely
differs — hashing a file by its bytes rather than an in-memory value. "Policy" is
not a separate channel: a policy that affects a leaf's output enters as an
ordinary dependency value and is hashed with the rest.

Severity: **blocker** on the subset question (hash all deps); **strong
suggestion** on collapsing the key's parallel channels.

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

1. Project **all** of the leaf's already-resolved dep values to
   `input-value-hashes` (Decision 3). A non-serializable dep aborts the cache
   path — the leaf runs normally.
2. Build the identity map and `node-cache-key` from a single resolved-inputs
   source: workflow/target/node keys, `graph-version` (an `eval-target` opt),
   `node-kind`, `impl-id`, declared `:impl/hash`, `input-value-hashes`, and
   `path-content-hashes` only where a dep is a file hashed by bytes.
3. `fetch` the entry. Run `valid-cached-node-result` against the eval env
   (`:base-dir`, identity-relevant `:config`).
4. Compute `status = (lookup->cache-status cached validity)`.
   - `"hit"` → **reuse** the cached value + outputs; **skip** `impl-fn`.
   - `"miss"` / `"stale"` / `"invalid"` → run `impl-fn`, materialize outputs,
     `store!` a fresh entry.
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

## Build plan — one thin vertical slice, gated on measurement

**Gate (Finding 1):** first identify a real, expensive, deterministic,
**uncached** leaf and measure its cost across two runs. Do not build until one
clears the bar. (The obvious candidate — nix realization — is already nix-store
cached; the true beneficiary is more likely a large content-hashing or
parse-heavy leaf. Measure, don't assume.)

Once a leaf is chosen, build **one vertical slice** that caches exactly that leaf
end-to-end, rather than the general framework ahead of use:

1. `lookup->cache-status` bridge (Decision 4) + an in-memory `{:fetch :store!}`
   test store — *gives the corrected Item 1 bridge its first real caller.*
2. `leaf`'s optional `{:impl/hash …}` map; the `value-hash` helper and a
   `leaf->cache-key` builder that hashes **all** resolved deps (Decision 3).
3. Cache-aware `eval-target` behind the `:cache-store`/`:graph-version` opts, for
   the one chosen leaf; add the `:cache` node-summary field; the
   twice-evaluate reuse-correctness test (equal `:value`, equal `:status`, second
   run reports `"hit"`).
4. The `:cache` field already flows through `report/node-record` to the schema —
   confirm `just schema-drift` green (no schema edit expected; verify).

**Generalize only on the second beneficiary.** A production content-addressed
store under `AB_DB_ROOT` and broader leaf opt-in are follow-ons, not part of the
first slice.

## Open questions for review

1. **Which leaf justifies the build (Finding 1)** — the gating measurement.
   Until one expensive, deterministic, uncached leaf is identified and measured,
   nothing is built.
2. **Store location** — a dedicated `AB_DB_ROOT/workflow-cache/` subtree, or fold
   into an existing bulk-storage layout? (Deferred to the generalize step.)

*Resolved in review:* impl-identity = declared `:impl/hash` stamp (fail-safe,
less braided than source-derived); scope = leaves only; store seam = injected
fn-pair, no protocol; identity = all resolved deps, no subset; GC = out of scope
for the first cut.

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
- **`defprotocol NodeCacheStore`** — rejected in review: the codebase's own
  injection precedent (nix `:runner`) is a bare function, a filesystem store runs
  in the sandbox, and one real backend + a test double is not present variation.
  An injected fn-pair is the honest minimum (Decision 1).
- **Hand-declared `:cache/inputs` subset** — rejected in review: a subset lets
  cache identity silently narrow, producing wrong reuse. Hash all resolved deps
  (Decision 3).
- **Build the general A–D framework up front** — rejected in review: abstraction
  ahead of a single measured beneficiary. One vertical slice, generalize on the
  second use.
