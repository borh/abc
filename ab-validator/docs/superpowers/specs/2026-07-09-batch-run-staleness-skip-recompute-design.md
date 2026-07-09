# Expensive Batch Run — Staleness &amp; Skip-Recompute Design

Status: Proposed design (2026-07-09) — spec-then-build, both batches, one identity model
Date: 2026-07-09
Owner: Soranoha architecture track
Builds on: [Fidelity Run Idempotency](2026-07-09-fidelity-run-idempotency-design.md),
[Fidelity Phase 2 — resolve/compute/lock](2026-07-09-fidelity-phase2-resolve-compute-lock.md)

This spec designs a single **input-content identity model** that lets the two
expensive Soranoha batch runs — **AAT dump generation** and **morph-warehouse
runs** — *skip recomputation when their inputs are unchanged* and *detect
staleness the instant an input changes*. It applies one identity model to both
batches, sequenced generator → warehouse (the generator's output identity is the
warehouse's input identity).

## Problem

Two batches dominate corpus compute cost and are re-run largely by hand today:

- **AAT dump generation** (`reports/aat-fidelity/run-aat-full.sh`): a full-corpus
  parse by one adapter over ~17.8k works → an `aat/` JSON tree + DuckDB + triage.
  ~19 GB (`aat-corpus`) + ~6 GB (`fidelity-corpus`) ≈ **25 GB across 5 adapters**;
  minutes–hours per adapter.
- **morph-warehouse runs** (`ab-morph-run analyze-aat`): an N-way morphological
  tokenizer-comparison over a checked AAT dir → sealed Parquet fact tables.
  **~45 GB** on `/db`; multi-GB peak RSS; the heaviest single batch.

Neither batch *skips* work when its inputs are unchanged, and the failure is
asymmetric across the two:

- The **fidelity/AAT** path already has a **mutation tripwire** — `resolve-run-set.py`
  content-hashes each dump (`aat_hash.hash_aat_dir`) against the manifest-pinned
  `expected.content_hash` and **fails closed** on mismatch. That answers "did a
  pinned input change?" It does **not** answer "is a fresh output already present
  for these inputs, so skip?"
- The **morph-warehouse** path has **no input→output link at all**: a run is keyed
  by an operator-chosen wall-clock `run_id` (`full-2026-07-06_160136-jobs8`);
  `.staging` cleanup is PID-liveness, not content. "Is this run current for these
  inputs?" is unanswerable.

**This spec builds the missing question — skip-recompute — for both.**

## Two staleness questions (keep them distinct)

1. **Mutation tripwire** — "did a *pinned* input change out from under a committed
   plan?" → fail closed. *Exists for AAT; keep it.*
2. **Skip-recompute** — "given the *current* inputs, is a valid output already
   present, so skip the run?" → the subject of this spec.

They are two readings of one fact — an output's identity as a function of its
inputs. Build that identity once; both questions fall out of it.

## Decision

Give each expensive batch output a **content identity derived from its full input
set**, and make each batch's runner:

**resolve identity → look for a valid output with that identity → skip, or compute
→ record the identity on the output.**

Prefer the **nix shape**: address the output by its input identity so that
*presence + re-verification = fresh*, with **no separate stateful ledger to drift
out of sync** (identity is a value of the inputs, not a place we mutate). Keep
outputs **content-addressed in place on `/db`** — the nix-derivation route was
already evaluated and **rejected** (nix store 94% full; cannot hold 25–45 GB
outputs; the `aat/` tree is already deterministic). Reuse the existing
resolve→compute→lock machinery and `aat_hash`; do not invent a parallel system.

## The identity model (load-bearing)

`input_set_hash = format_sha256(sha256(JCS(identity_object)))`, where
`identity_object` is a canonical, string-keyed map composing **everything that
determines the output**:

- **Content hashes of non-nix input trees** — the raw corpus (generator) or the
  AAT dir (warehouse) — via a raw-bytes, walk-order-independent tree hash
  (`aat_hash.hash_aat_dir` and its generalization). ~0.4 ms/MB → ~9 s/25 GB,
  negligible against minutes–hours of compute.
- **Nix pins for nix inputs** — the adapter binary and the dictionaries are
  already nix-packaged, so their **store path / `narHash` *is* a content id**;
  reuse it rather than re-hashing gigabytes of dictionary.
- **Output-affecting knobs** — enumerated explicitly per batch (below).
- **`identity_version`** — a `const` stamping the identity *format* itself, so a
  future change to what identity covers invalidates cleanly.

**Fail toward correctness (the trust boundary).** Omitting an output-determining
input silently *narrows* identity → a stale output is served as fresh — the exact
wrong-reuse failure the workflow-cache review named. So: **include every input
that can affect the output; when unsure, include it** (spurious recompute is slow,
never wrong). **Content, never mtime.** **Non-determining knobs are excluded only
with a written rationale** (parallelism `--jobs`, temp-dir paths, report ids do
not change bytes of the semantic output).

## Per-batch instantiation

### AAT dump generation (closes the F6 gap)

`identity_object` =
- `corpus_content_hash` — tree hash of the Aozora `cards/` corpus consumed;
- `adapter` — nix store path + `adapter_version` (already in `metadata.json`);
- `feature_patterns_hash` — `sha256-file` of `data/feature-patterns.toml`;
- output-affecting flags (e.g. per-work `--timeout` if it can *truncate* output —
  to confirm; `--jobs` excluded as parallelism-only);
- `identity_version`.

**F6 fix:** `metadata.json` today records `repo_head`/`repo_dirty` but **no hash of
the dump's own inputs** and no self-hash. Record both: the `input_set_hash` above
and the dump's **output** content hash (`hash_aat_dir` of the produced `aat/`
tree, the value already pinned as `expected.content_hash`). Skip generation when a
dump with the current `input_set_hash` already exists on `/db` and its output hash
re-verifies.

### morph-warehouse runs (the quick win — inputs already computable)

`identity_object` =
- `aat_content_hash` — **reuse** the AAT dump's existing `content_hash` (already
  pinned / produced above); no new hashing of the AAT dir needed;
- `dictionaries` — the nix store paths of `AB_SUDACHI_DICT`, `AB_VIBRATO_DICT`,
  `AB_VAPORETTO_DICT` (already content-addressed);
- `analyzers` — the selected analyzer set (`AB_MORPH_ANALYZERS`);
- `warehouse_profile` — `full` | `triage` (changes which tables are kept);
- `schema_version` — `sha256-file` of `ab-morph-run/sql/schema.sql` (+ `ab-warehouse`
  schema), so a schema change invalidates;
- `identity_version`.

Everything here **already exists** — this is why morph-warehouse is the
lowest-effort, highest-gap-closing target. Record `input_set_hash` on the run
(a new column on `runs.parquet` + a `run-manifest.json` sidecar) and skip when a
published run with that identity exists and its Parquet outputs re-verify.

## Stateless addressing vs. an index (a real tradeoff)

- **Generator:** content-address the dump directory by `input_set_hash` (or keep a
  thin `by-input/<hash> → <dump-dir>` symlink). Presence + verify = fresh. No
  ledger.
- **morph-warehouse:** runs are already immutable and human-labelled
  (`full-2026-07-06_…`). Rather than rename run dirs to hashes (disruptive, breaks
  existing references), add a **`by-input/<input_set_hash> → runs/<run-id>`**
  index (symlink or a tiny index table). Skip-decision = "does `by-input/<H>`
  resolve to a run whose Parquet outputs verify?" This keeps the human run-id and
  is the minimal additive change. *Tradeoff:* the index is a small piece of
  derived state; it is rebuildable from run manifests, so drift is recoverable,
  not corrupting.

## Fail-closed staleness

- Addressed output present but **re-verification fails** (corrupt/partial) → treat
  as **stale → recompute**. Never serve an output that does not match its recorded
  hash.
- An input that **cannot be hashed** (missing dir, unreadable) → **refuse**, do not
  silently skip identity (a missing input must not read as "no change").
- An explicit `--force` always recomputes and re-records — the escape hatch that
  keeps skip-recompute *additive* and reversible.

## Verification discipline (codebase-simplification)

These batches **cannot run in a unit sandbox** (25–45 GB, minutes–hours). So,
per the simplification loop, **PROTECT before changing structure** and lean on the
fixture/characterization pattern the repo already uses (`test_aat_hash.py`,
`fidelity-lock-idempotency-smoke.sh`, the `monorepo-fidelity-lock-idempotency`
flake check):

- Small **fixtures** (a handful of AAT JSON files, a tiny warehouse) proving:
  identity is **deterministic** (same inputs → same `input_set_hash`), **sensitive**
  (mutate one input byte → different hash → recompute), and **skip-on-match** (second
  run with an unchanged fixture skips).
- The skip logic is **additive and behavior-preserving**: with `--force`, or on any
  identity miss, the batch computes exactly as today. Any bug/semantic fix found
  while wiring identity is **filed separately, not folded in**.

## Slice plan (subagent-driven build)

Sequenced so each slice ends at an independently testable deliverable. The
warehouse slices can land first (inputs already exist) while the generator's F6
identity is built; the identity *library* is shared.

1. **Shared identity library** — a small Python module (`reports/lib/`) computing
   `input_set_hash` from an `identity_object` via existing JCS+sha256 helpers,
   plus a generalized tree-hash (reuse/extend `aat_hash.hash_aat_dir`). Pure;
   fixture-tested for determinism + sensitivity.
2. **morph-warehouse identity + skip** — compute the warehouse `identity_object`;
   write a `run-manifest.json` sidecar + `input_set_hash` column; add the
   `by-input/<hash>` index; `analyze-aat` checks the index and **skips** on a
   verifying match unless `--force`. Fixture warehouse test.
3. **AAT generator identity + skip (F6)** — record `input_set_hash` + output hash
   in `metadata.json`; `run-aat-full.sh` skips regeneration on a verifying match
   unless forced. Fixture/dry-run test.
4. **CI staleness gate** — extend the idempotency smoke/flake check to assert
   skip-on-unchanged and recompute-on-mutated for both batches on fixtures.

## Alternatives considered

- **Nix-derivation caching** — rejected (already, in Phase 2 Move A): nix store 94%
  full, cannot hold 25–45 GB outputs. Content-address in place on `/db` instead.
- **Stateful skip-ledger** (a DB of input→output) — rejected where avoidable in
  favor of content-addressed presence; used only as a thin, rebuildable
  `by-input` index for morph-warehouse where run dirs stay human-labelled.
- **mtime / run-id staleness** — rejected: not content-based; the current
  wall-clock run-id is precisely the gap.
- **One aggregate run-set hash** — complementary, not required: per-output identity
  composes into a set hash if ever wanted, but per-output is what enables per-batch
  skip.

## Open questions (to resolve during build)

1. **Exact knob inclusion** per batch — confirm from the code which flags change
   output bytes (does `--timeout` truncate a work's AAT? does warehouse profile
   selection change retained tables only, or also values?). Default: include when
   unsure.
2. **Generator output addressing** — content-address the dump dir vs. a `by-input`
   symlink beside the existing human dir name.
3. **Measurement** — capture real generation/warehouse wall-times (from
   `benchmarks/baselines/`) to size the payoff and confirm the ~9 s hashing cost is
   negligible in context.
