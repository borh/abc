# Design: idempotent, low-env fidelity runs ("like nix")

**Date:** 2026-07-09
**Status:** proposal / ADR-style decision record. Not yet implemented.
**Lens:** `architecture-triage` → `rich-hickey-review` (braided concerns + implicit
idempotency contract) → `codebase-simplification` (behavior-preserving patches).
**Motivation:** the fidelity-pipeline tooling is hard to reason about — env sprawl,
bash glue, and runs that are not pure functions of their inputs. This session a stale
`AB_AOZORA_AAT_DIR` **silently swapped in the wrong parser dump** and validation didn't
catch it — a real correctness incident, and the symptom that names the disease.

## The core diagnosis (one sentence)

**Configuration is complected with the ambient shell environment, invocation, and
computation; the run's inputs are read from mutable places (env vars, CWD, whatever
sits in `/db`) instead of being a single pinned value — so a run is not reproducible
from what you can see at the call site.**

Nix is the counter-model: inputs are pinned (`flake.lock`), the build is a pure
function of them, and the output is determined by the inputs. We want the same shape.

## Findings (severity-tagged)

### F1 — Config as ambient place, not value · **Blocker**
> **Observation.** ~10 `AB_*` env vars carry run inputs: per-adapter dump dirs
> (`AB_AOZORA_AAT_DIR`, …), corpus/dict (`AB_VIBRATO_DICT_DIR`, `AB_VIBRATO_CACHE_DIR`),
> `AB_DB_ROOT`, `AB_AOZORA_BIN`, build knobs (`RUSTC_WRAPPER`/`SCCACHE_DISABLE`). The
> "config" of a run is whatever the shell happens to hold; it can't be passed,
> inspected, diffed, or pinned as a value.
> **Risk.** Non-reproducibility (same command, different result); the dump-swap
> incident; to predict a run you must inspect the whole environment. Bus-factor.
> **Alternative.** Config-as-value: a single **run manifest** is the sole input
> surface; env is demoted to at most "which manifest" (`AB_AAT_RUN_SET`). The run-set
> descriptor is already ~80% of this manifest.
> **Tradeoff.** Loses ad-hoc `AB_X=… just foo` overrides; overrides become explicit
> edits/selections of a versioned manifest value. That's the point.

### F2 — The run-set pin is advisory, not authoritative · **Blocker**
> **Observation.** `run-sets/current.json` pins dumps + expected flake revs, but
> `aat_runs._optional_adapter_path` lets `<label>_aat_dir_env` **override** the pin with
> no enforcement. Validation is a separate manual step, not a gate on the run. (This
> session I added a co-location check so a swap at least *fails validation* — but
> resolution still honors the override.)
> **Risk.** The pin is silently defeatable; the idempotency contract is unenforced.
> **Alternative.** Make the manifest the sole authority: drop the `_env` override
> fields, and fold validation *into* the run so compute refuses to proceed on an
> unpinned/mismatched manifest (like nix refusing an unpinned input).
> **Tradeoff.** Less per-invocation flexibility; need one clean "use an alternate
> manifest" path (`--manifest` / `AB_AAT_RUN_SET`).

### F3 — Recipes braid resolve + build + env + compute · **Strong suggestion**
> **Observation.** `parser-performance-all-parsers`, `aozora-notation-spec-comparison`
> are long `\`-joined blocks doing `nix build` (resolution), env exports, `cargo build`,
> and the actual computation in one unit. `run-*.sh` similarly.
> **Risk.** Resolution and computation can't be reasoned about or reproduced
> separately; the computation's inputs are entangled with how they were resolved;
> duplication across recipes.
> **Alternative.** Three phases (below): **resolve** (impure: nix/paths → a locked
> manifest value), **compute** (pure: locked manifest → report), **invoke** (thin
> wrapper). Recipes shrink to 2–3 lines.
> **Tradeoff.** An explicit locked-manifest artifact per run — which *is* the
> reproducibility record.

### F4 — Inputs discovered from mutable `/db` + CWD · **Strong suggestion**
> **Observation.** Coverage/denominator scripts read dumps from hardcoded/env `/db`
> paths and CWD-relative dirs (`dictionary/`, `workspace_path`); which dumps exist in
> `/db` is mutable, GC-able, overwritable ambient state.
> **Risk.** A run's result depends on `/db` contents at run time, not on a pinned
> input. Not content-addressed (the corpus/dump-staleness caveats this session).
> **Alternative.** Pin inputs by identity in the manifest: dump dir + its
> `metadata.json` descriptor (source rev + narHash), corpus by nix rev/store path.
> Resolve verifies descriptor rev == expected before compute.
> **Tradeoff.** Every dump needs a descriptor (some missing now); one verify step.

### F5 — Build determinism as tribal env · **Follow-up / Nit**
> **Observation.** Builds need `export RUSTC_WRAPPER= SCCACHE_DISABLE=1` (sccache
> `SUN_LEN` in long paths) — remembered per-invocation, documented as a handoff gotcha.
> **Risk.** Forgotten → build failure; knowledge lives in prose.
> **Alternative.** Bake into the build entrypoint (nix devshell env or `.cargo/config`
> `[build] rustc-wrapper = ""`); better, prefer the **nix-built parser derivations**
> (`upstream-parser-aozora`, …) in the pipeline — those *are* the idempotent build, so
> the pipeline stops doing `cargo build` at all.
> **Tradeoff.** Minor; mostly relocating config to where it's automatic.

## Target design: resolve → compute → emit

```
 manifest (run-sets/current.json, extended)        ← the ONLY input surface (a value)
        │  resolve   (impure, isolated)
        ▼
 locked manifest  (like flake.lock)                ← nix store paths + verified dump
        │  compute   (PURE: reads only the locked    descriptors; fails closed on
        ▼             manifest — no env, no CWD, no    rev/narHash mismatch)
 report(s)  (§4.7/§4.8/§4.9, denominators, seed)     /db discovery)
        ▲  invoke    (thin just/CLI: resolve then compute; no env exports)
```

- **Manifest = config-as-value.** Extend `run-sets/current.json` to name *every* input:
  adapter dumps (already there), corpus (nix rev), matrix, prior summary, output targets.
  Remove the `_env` override fields (F1/F2).
- **Resolve = the only impure stage.** All `nix build` / path resolution / descriptor
  verification lives here and emits a *locked manifest* (fully-pinned store paths + revs).
  This is the flake.lock analog; it fails closed on mismatch (the idempotency gate).
- **Compute = pure.** Each report tool takes the locked-manifest path as its sole
  argument, reads dumps/corpus from it, touches no env/CWD/`/db`-discovery. Same locked
  manifest → same report, always.
- **Invoke = thin.** `just fidelity-report` = resolve + compute. No `export` lines.

**Env sprawl after:** the ~10 `AB_*` vars collapse to `AB_AAT_RUN_SET` (which manifest)
and optionally `AB_DB_ROOT` (output root — or put it in the manifest). Everything else
becomes an explicit manifest value.

## Migration (strangler-fig, behavior-preserving — characterize each step)

- **Phase 0 (done this session, stopgaps):** hardened `validate_run_set` to *catch* the
  dump-swap; `run-coverage-report.sh` neutralizes env. These treat the symptom.
- **Phase 1 — manifest authoritative (kills F2, most of F1):** make `aat_runs`
  resolution ignore the `_aat_dir_env`/`_run_descriptor_env` overrides; keep only
  `AB_AAT_RUN_SET` for manifest selection. Delete the env-neutralization glue that
  becomes unnecessary. *Characterize:* the three coverage scripts already read the
  run-set — re-run and diff outputs to confirm unchanged. Low risk, high payoff.
- **Phase 2 — locked manifest + pure compute (F3/F4):** extend the manifest with
  corpus/matrix; add a `resolve` step emitting a locked manifest; point compute tools at
  it as their sole input.
- **Phase 3 — thin recipes:** shrink `just` recipes to resolve→compute; delete `run-*.sh`
  glue superseded by the pure entrypoints.
- **Phase 4 — build knobs (F5):** relocate `RUSTC_WRAPPER` to devshell/`.cargo`; prefer
  nix parser derivations over in-pipeline `cargo build`.

## Consequence for this session's in-flight work

The `just fidelity-coverage-report` / `fidelity-denominator-recompute` recipes I drafted
are **Phase-0/1 interim glue** — a single entry point that *encodes* the env-neutralization
dance rather than *removing* the need for it. Keep them only if we want an interim; the
target design deletes the env dance (Phase 1), after which those recipes become the thin
2-line wrappers of Phase 3. The `run-coverage-report.sh` out-dir→`scratch/` fix is a
clean improvement independent of all this.

## Recommended first move

**Phase 1** — it directly closes the correctness hazard (F2), removes the biggest chunk
of env sprawl (F1), is behavior-preserving (the run-set already drives the coverage
scripts), and is verifiable by diffing coverage outputs before/after. Phases 2–4 are
larger and can follow once Phase 1 proves the manifest-authoritative model.
