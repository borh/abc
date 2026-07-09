# Design: Phase 2 — resolve → compute over a closed, content-addressed lock

**Date:** 2026-07-09
**Status:** proposal / ADR extension. Not yet implemented.
**Builds on:** `2026-07-09-fidelity-run-idempotency-design.md` (the parent ADR) and
its **Phase 1**, merged to main as `a4d15879` (feat `3d5d658e`): the run-set manifest
is now authoritative over ambient `AB_*_AAT_DIR` overrides.
**Lens:** `architecture-triage` → `rich-hickey-review` (idempotency contract, time &
identity, fake seam) grounded in the current `/db` descriptors and compute-tool inputs.
**Goal, stated by the request:** *one obvious way to do things, provably correct.*

## Where Phase 1 leaves us

Phase 1 closed the *override* hole: a stale `AB_AOZORA_AAT_DIR` can no longer swap a
dump, because resolution ignores the `<field>_env` fields entirely (F1/F2). That is
necessary but not sufficient for the stated goal, which decomposes into two properties
Phase 1 does **not** yet provide:

- **Provably correct** — the idempotency contract holds *by construction* and is
  *verifiable*, not merely conventional.
- **One obvious way** — a single canonical path to run anything, with one input value,
  no ambient env, no per-report bash glue.

Both fail today, and the evidence names why.

## Findings (severity-tagged, evidence-grounded)

### F6 — Dump identity is not content-addressed · **Blocker (for "provable")**
> **Observation.** A dump's `metadata.json` descriptor carries `generated_at_utc`,
> `repo_head`, `repo_status_short`, an ephemeral worktree `adapter` path, `corpus`
> (a nix store path), `adapter_version`, `jobs`, `timeout` — and **no hash of the
> dump's own bytes**. Worse, a real descriptor's `repo_status_short` reads
> `"M …/normalized-corpus-coverage.py ?? …"`: the dump was built from a **dirty working
> tree**, so `repo_head` does not pin the code that produced it.
> **Risk.** You cannot prove a dump in `/db` is the one the manifest pins — only that a
> path exists. `validate_run_set` checks `flake.lock` rev == descriptor rev, but the
> descriptor's own rev was dirty, so the pin validates against the wrong thing.
> Idempotency is *hoped for*, not verified; a mutated/truncated dump is undetectable.
> **Alternative (preferred: dumps become derivations).** Make each dump a **nix
> derivation** built from the pinned parser derivation + pinned corpus. Then its **store
> path *is* its content hash** — verification is free (nix already did it, on a clean
> tree), the producing build is pinned (absorbs F5), and the dirty-tree hole closes
> outright. The manifest pins the store path; resolve just checks it realises.
> **Alternative (interim: hand-hash loose `/db` dumps).** If derivations aren't ready,
> hash each dump's AAT tree (narHash-style) into its descriptor at generation; the
> manifest pins that hash; resolve recomputes and compares, failing closed on mismatch.
> A crutch, because loose `/db` dirs are mutable *between* hash and read.
> **Tradeoff.** The derivation route costs upfront nix packaging of the dump build (and
> store disk); the interim costs a full re-hash of ~17k-file dumps on every resolve (no
> free store-path cache — these dirs aren't in the store) plus regenerating every existing
> descriptor to carry the hash. This is the price of "provable," and the reason the
> derivation route is preferred: it makes correctness free rather than computed.

### F7 — Two input channels; dump-selection is a fake seam · **Strong suggestion**
> **Observation.** The compute tools take the fidelity summary as an explicit
> `sys.argv[1]` (a value) but read the run-set via `load_run_set()` **with no argument**
> — an ambient default from `AB_AAT_RUN_SET` or a hardcoded path (`normalized-corpus-
> coverage.py:35`, `rebuild-fidelity-summary.py:123`). Half the real input — *which
> dumps* — enters implicitly.
> **Risk.** It *looks* like the summary is the input, while dump-selection is smuggled in
> ambiently; the tool is not a function of its arguments. Two callers with the same argv
> can compute different reports.
> **Alternative.** One input value: every compute tool takes the **locked manifest** as
> its sole positional argument. `{lock}` in, `report` out. Kill `load_run_set()`-with-no-
> arg.
> **Tradeoff.** An explicit lock artifact must exist before compute runs — which *is* the
> reproducibility record.

### F8 — Ambient env at *compute* time (`AB_DB_ROOT`, `AB_AAT_RUN_SET`) · **Strong suggestion**
> **Observation.** After Phase 1, `_resolve_path` still interpolates `${AB_DB_ROOT}` into
> every manifest path, and `AB_AAT_RUN_SET` still selects the manifest — and these are read
> at *compute* time (compute calls `load_run_set()`).
> **Nuance (per maintainer).** `AB_DB_ROOT` is **not** accidental config-as-place to
> eliminate — it is a legitimate *deployment* binding: the general bulk-storage root, which
> differs by machine like a mount point. A manifest expressed relative to it is *portable*
> across deployments; that is a feature. `AB_AAT_RUN_SET` is likewise a legitimate
> "which run" selector. The defect is only that these are resolved at *compute* time, so
> compute is not a closed function of a value.
> **Risk.** Compute's output depends on the ambient environment, not solely on its input;
> the run is not reproducible from the lock alone.
> **Alternative.** Keep both as deployment/run parameters but **confine them to `resolve`**:
> resolve reads `AB_DB_ROOT` once and bakes the deployment-bound absolute paths into the
> lock (recording the resolved root as provenance); `AB_AAT_RUN_SET` demotes to resolve's
> positional "which manifest" argument. Then the **manifest stays deployment-portable, the
> lock is deployment-bound, and compute reads zero env** — the nix shape exactly (portable
> flake, machine-bound lock, closed build).
> **Tradeoff.** Minor — the interpolation moves from compute-time to resolve-time; the lock
> becomes deployment-specific (correct: it is the resolved value for *this* deployment).

### F9 — Time-as-identity and mutable output location · **Strong suggestion (sharpens F4)**
> **Observation.** `fidelity-denominator-recompute` writes its inventory to a
> `pinned-$(date -u +…)`-named dir inside `/db`, then the summary references that path;
> discovery is CWD/`/db`-relative and GC-able.
> **Risk.** The run's inputs are keyed by wall-clock time and mutable filesystem state,
> not by content — the antithesis of reproducibility.
> **Alternative.** The inventory becomes a resolved input pinned by content hash in the
> lock (like a dump), not a timestamp-named side effect discovered later.
> **Tradeoff.** The inventory build must emit a descriptor + hash, same shape as F6.

### F10 — Surface sprawl: 13 `run-*.sh` + a 1169-line justfile · **Follow-up**
> **Observation.** `reports/` and `benchmarks/` hold 13 `run-*.sh` scripts; the justfile
> is 1169 lines with ~22 recipes that braid `nix build` + `cargo build` + `export` +
> compute in one unit (parent-ADR F3).
> **Risk.** Many ways to run "the same" thing; resolution and computation cannot be
> reasoned about separately; duplication drifts.
> **Alternative.** Once resolve/compute exist, each recipe shrinks to `resolve → compute`;
> the per-report `run-*.sh` superseded by the pure entrypoints are deleted.
> **Tradeoff.** Recipes that also do genuinely-different work (benchmarks) stay; only the
> resolve/compute-shaped ones collapse.

## Target design: `resolve → compute`, closed & content-addressed

```
 manifest (run-sets/current.json, extended)            ← the ONLY authored input (a value)
        │  resolve  (the ONE impure boundary)
        │    · nix build parser derivations / corpus  → store paths
        │    · content-hash each dump + inventory     → verify == pinned hash (F6/F9)
        │    · bake AB_DB_ROOT → absolute paths        (F8)
        │    · FAIL CLOSED on any mismatch             (the idempotency gate)
        ▼
 lock  (fidelity.lock — the flake.lock analog)        ← fully-pinned, closed value:
        │  compute (pure, given resolve)                store paths + verified content
        │    · sole argument = the lock (F7)            hashes; no env, no CWD, no /db
        │    · reads only the lock                      discovery
        ▼
 report(s)  (§4.7/§4.8/§4.9, denominators, seed)
        ▲  invoke  (thin just/CLI: `resolve <manifest> && compute <lock>`; no exports)
```

**The lock is a designed protocol, not an incidental dump.** It is the one contract
between resolve and compute, so it must not repeat the disease (implicit contracts). It has
a local JSON Schema next to its emitter (`reports/aat-fidelity/fidelity-lock.schema.json` —
internal to ab-validator, NOT the published `abc/schemas` package), a `lock_format` version
tag, and a single owner (resolve emits, compute consumes read-only). **This schema was the
first concrete deliverable of Phase 2 (landed in Move B).** Fields (sketch): per adapter — resolved absolute
`aat_dir`, its producing derivation store path (= its content identity) or interim
`content_hash`, `adapter_version`, flake `rev`/`narHash`; plus corpus store path and the
inventory identity. The lock is a *closed value*: everything compute needs, nothing it
reads from the world.

- **resolve** is the sole impure stage and the only place that touches nix, `/db`, or
  env. It verifies content identity and **fails closed** — the analog of nix refusing an
  unpinned input.
- **compute** is a pure function of the lock *given resolve's guarantee* — it reads dump
  JSON from the paths the lock names, so its determinism is only as strong as resolve
  having verified those bytes. With dumps as store paths (immutable) that purity is
  **unconditional**; with interim hand-hashed `/db` dirs it is **contingent** on resolve
  running first. Either way there is no second input channel.
- **invoke** is two lines. The 13 `run-*.sh` and the export-braiding recipes collapse.

## The payoff: the proof you cannot write today

Once dumps are content-addressed (F6) and compute reads only a closed lock (F7/F8),
idempotency becomes a **mechanical test** rather than a hope:

1. `resolve(manifest)` twice → **identical lock** (golden-lock characterization).
2. `compute(lock)` → **byte-identical report** (golden-report characterization).
3. CI gate: re-resolve → lock unchanged; if a dump in `/db` was mutated or GC'd,
   resolve **fails closed** instead of computing on wrong data.

None of these three is writable today, because neither stage is a closed function of a
value. That test suite *is* "provably correct."

## Migration (strangler-fig, behavior-preserving — characterize each move)

**Two distinct keystones — don't conflate them.** Move A is the keystone for the
*property* (without content-addressing, "provable" is unreachable). But Move **B** is the
correct *first migration move*, because A's verification needs a `resolve` stage to live
in — and B is what creates it. Doing A first would bolt verification onto the old braided
recipes, then move it. So the property depends on A; the migration starts at B.

- **Move B (first) — resolve/compute skeleton + single lock value (F7/F10).** Introduce
  the `resolve` step emitting the lock; make every compute tool take the lock as its sole
  argument (remove `load_run_set()`-with-no-arg); shrink recipes to `resolve → compute`;
  delete superseded `run-*.sh`. Absorbs the parent ADR's old "Phase 3." *Characterize:*
  report bytes unchanged vs the Phase-1 baseline (behavior-preserving).
- **Move C — close ambient env (F8).** Bake `AB_DB_ROOT` into the lock at resolve; demote
  `AB_AAT_RUN_SET` to a positional arg. *Characterize:* `env -i compute <lock>` (empty
  environment) produces the identical report — the closure proof.
- **Move A (keystone, last) — content-address dumps + inventory (F6/F9, absorbs F5).**
  Preferably by making dumps nix derivations (store path = identity, verification free);
  else interim tree-hashing. Resolve verifies identity and fails closed. Slots into the
  `resolve` stage B built. *Characterize:* golden-lock (re-resolve → identical lock) and
  fail-closed (mutate a `/db` dump → resolve errors, does not compute).

**Decision criterion for Move A (don't do it on zeal).** B + C alone already deliver *one
obvious way* and close the last of F1; they leave correctness *convention-based*. Pull
Move A forward when **either** a dump-mutation/GC incident actually occurs (the pin proved
insufficient), **or** dumps are being packaged as nix derivations for other reasons (F5)
— whichever comes first. Absent either signal, B + C is a legitimate stopping point.

## Consequences & tradeoffs

- **Cost centre is Move A:** tree-hashing large dumps at resolve (cache by store path) and
  a one-time descriptor-regeneration migration. This is the price of "provable"; it is not
  gold-plating — content-addressing is *the* mechanism that converts correct-by-convention
  into correct-by-construction, which is the stated goal.
- **Gain:** the lock is the single reproducibility record; compute is testable in
  isolation; the env collapses from Phase-1's `AB_DB_ROOT` + `AB_AAT_RUN_SET` to nothing
  compute reads; the `run-*.sh`/recipe sprawl shrinks.
- **Non-goal:** benchmarks and genuinely-different recipes are out of scope; only the
  resolve/compute-shaped fidelity path is collapsed.

## Way forward — the overall plan (supersedes the parent ADR's Phase 2–4 split)

The parent ADR sketched Phases 1–4; this doc's evidence lets us commit to one ordered
roadmap and retire the looser numbering (old Phase 3 folds into Move B, old Phase 4 into
Move A). Each step lands behind its own characterization test, strangler-fig, so the
system is shippable and behavior-preserving at every boundary.

| Step | Delivers | State | Gate (characterization) |
|---|---|---|---|
| **Phase 1** — manifest authoritative | env-overrides impossible | ✅ merged `a4d15879` | byte-identical resolution + `test_aat_runs.py` |
| **2 · Move B** — resolve/compute skeleton + single lock | *one obvious way*; glue deleted | ✅ done (branch) | report bytes == Phase-1 baseline (byte-identical on full corpus) |
| **2 · Move C** — close ambient env | zero-env compute (closed value) | ✅ done (branch) | `env -i compute <lock>` runs — proven in `test_fidelity_lock.py` |
| **2 · Move A** — content-address (via derivations) | *provably correct* | keystone; per criterion above | golden-lock + fail-closed |

**Note on Move C:** it turned out *substantially subsumed by Move B* — because the lock
carries deployment-bound absolute paths and compute reads only the lock, compute was
already env-free once B landed. Move C therefore reduced to (a) recording the `db_root`
deployment binding in the lock as provenance and (b) making the `env -i` closure a durable
test. `AB_AAT_RUN_SET` remains a legitimate run selector confined to `resolve` (F8 nuance).

**The finish line, concretely:** when Move A lands, the three tests in *"the proof you
cannot write today"* become real, and the env compute reads collapses from Phase-1's
`AB_DB_ROOT` + `AB_AAT_RUN_SET` to nothing. At that point "one obvious way, provably
correct" is not a slogan but a passing CI gate.

**Recommended order: B → C → A**, with A governed by the decision criterion (not reflexive
zeal). B first because it builds the stage the rest slots into; C next because closing the
value is cheap once resolve exists; A when its trigger fires.
