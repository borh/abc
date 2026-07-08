# Handoff: Aozora parser comparison — followups after fidelity/robustness + re-pin

**Status:** handoff for a fresh session. Self-contained; read top to bottom.
**Prereq:** work in a `.worktrees/<name>` worktree (memory `work-in-worktrees`).
**Predecessor:** `2026-07-08-parser-comparison-handoff.md` (the fork→decision handoff).
**Study (read first):** `docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md`.

## Where things stand

The Aozora parser comparison is **decision-ready and merged to `main`**. The
recommendation stands and is now hardened on every axis: **base a new/forked Rust
parser on `aozora-pipeline` (`P4suta/aozora`)** — it leads corpus coverage (0.969),
conformance breadth (22/25 must), and speed, is the only parser top-tier on *both*
fidelity and robustness, and (new this round) is confirmed stable under active
upstream development.

**Landed since the predecessor handoff (all on `main`):**
- **§4.8 fidelity/robustness split** — decomposes corpus coverage. aozora2html is the
  *most faithful* parser per work (0.974) despite last-place coverage (its deficit is
  pure robustness — fails on 302 ruby-heavy works); aozora-rs's #2 coverage was
  flattered by perfect robustness (lowest fidelity). `fidelity-robustness-split.py`.
- **§4.7 signature normalization** — harmonized indentation (folded aozora2's per-line
  `jisage_line` into jisage: 0.38→~1.0), added **gaiji** (exposed epub3 emits zero),
  added `midashi` heading. Reconciliation assertion keeps §4.7/§4.8 signatures in sync.
- **§7 controlled re-pin** — re-pinned `P4suta/aozora` `5df2cfa5`→`1a4f864` (HEAD).
  A controlled experiment (old vs new parser, **same** pinned corpus) proved the
  parser is **measurement-equivalent**: ≤0.05 on any construct (only `tcy`, −0.05).
  The new parser's marker→typed-node restructuring + schemaVersion 1→2 bump are
  representation-only, absorbed by the union signatures.
- **Two infra fixes** (commit `6d29b56`) that unblocked reproducible re-measurement:
  `ab-index` now indexes the symlinked pinned corpus (previously skipped every
  symlinked work file → 0 works; +regression test); the adapter accepts inspect
  `schemaVersion` 2 (verified a pure version bump by diffing `aozora schema <kind>`).
- **§4.9 parity/support audit** — swept the 21 source constructs vs the 9 tracked.
  Every parser drops ≥1 construct; `aozora-pipeline` drops `jizume`/`yokogumi`
  outright; chitsuki dropped by 3/5, burasage by 2. `parity-support-audit.py`.

## Open work (prioritized)

### 1. Full-nix-corpus denominator recompute — *consistency/reproducibility* (medium)
**Why.** §4.7 numerators are now measurable on the pinned nix corpus (after the
`ab-index` fix), but the **denominators** are still source-authority counts from the
original *local* extraction (`build-inputs.json` → `2026-07-08-corpus-adapter-fidelity.summary.json`).
The pinned nix corpus has slightly fewer `gaiji`/`jisage` source occurrences, so a
measurement that pairs nix numerators with local denominators is inconsistent (this is
exactly why a naive nix re-measure showed phantom gaiji −0.11 / jisage −0.14 drops —
see §7 corpus caveat). The controlled experiment proved rankings don't change, but a
fully-reproducible study should have num **and** denom on the same pinned corpus.
**How.** Regenerate the source-authority occurrence counts (`build-inputs.json` `occ`,
via the source-representability path — see `justfile` `source-representability-gate-smoke`
and `corpus-adapter-fidelity-classifier.py` which reads `SP/build-inputs.json`) against
the pinned corpus; rebuild the fidelity summary; then re-run `normalized-corpus-coverage.py`,
`fidelity-robustness-split.py`, and `parity-support-audit.py` with the aozora glob
pointed at `aozora-full-repin-1a4f864`. Expect only the `gaiji`/`jisage` rows to move
(num and denom together); rankings unchanged.
**Effort.** ~1 session; the heavy parts (index, parse) are now ~5–10 min each.

### 2. Close `aozora-pipeline`'s construct gaps — *for the fork backlog* (small-medium)
From §4.9, the recommended parser **drops `jizume` (字詰め) and `yokogumi` (横組)
entirely**, and the source-authority summary has **no `keigakomi` (罫囲み) denominator**
to score it at all (a gap in the instrument, not just the parsers). These are concrete,
named items for the fork's to-do or an upstream contribution. Low corpus mass (~0.2%)
but real. Verify each against official 青空文庫 docs before filing.

### 3. Expand the performance sample — *pinpoint aozora-core's pathology* (medium)
§4.6 perf is 6 works, one machine. §4.8 already showed aozora2's 30 missing works hold
13.6% of ruby and **are the timeout giants** — so the robustness gap and the perf
pathology are the same defect. Confirm on a larger sample: which inputs blow up, and
is it a cheap fix (if so, aozora-core re-enters contention). Recipe:
`just parser-performance-all-parsers` (needs INDEX, CORPUS; SAMPLE/LIMIT_S envs).

### 4. `aozora-rs-core` gaiji blind spot / adapter typed projection — *deferred*
The `retokenized` dump is blind to gaiji (§5 threat #4); and the production aozora-rs
adapter's typed AAT projection under-represents the parser. See memory
`aozora-rs-adapter-repair-deferred`. Only matters if aozora-rs becomes a serious
throughput-first candidate.

### 5. Expand the official-docs seed (§4.5) — *independent instrument*
From 11 clean cases toward edge cases + precise spans, so it reproduces absolute rates
(a full independent instrument, not a seed). `author-official-seed.py`.

### 6. Uniform per-parser methodology write-up — *publication polish*
A single-scale normalization (or a clear methodology section) that puts all candidates
on one axis, per §5 threat #2.

## The bigger decision (after or instead of the above)

**Start the parser design** on the `aozora-pipeline` base (`brainstorming` →
`writing-plans`). The acceptance gate is bigger than conformance — the 2026-07-06
admission criteria (`docs/superpowers/specs/2026-07-06-comprehensive-parser-acceptance-criteria.md`):
parser-IR validity, publication accounting, zero unknown-markup, perf. Or contribute the
3 `diagnostics` must-fixes upstream where possible and fork for the rest.

## Environment / gotchas (learned this session — save time)

- **Builds fail with `sccache: error: path must be shorter than SUN_LEN`** in the long
  session working paths. Fix: `export RUSTC_WRAPPER= SCCACHE_DISABLE=1` before any
  `cargo`/`just` build. (sccache's socket path derives from the long TMPDIR.)
- **`AB_AOZORA_BIN` is NOT exported by the default `nix develop` shell.** Set it
  explicitly to the built parser: `nix build .#upstream-parser-aozora --print-out-paths`
  then `export AB_AOZORA_BIN=<store-path>/bin/aozora`.
- **The pinned `aozorabunko-corpus` symlinks every leaf work file** into the `-source`
  store; `ab-index` handles this now (don't "fix" it back). `resolve-aozorabunko-corpus.sh`
  resolves it (or honor `AB_CORPUS`/`AB_AOZORA_CORPUS`).
- **Re-pinning `P4suta/aozora` again:** if a future HEAD bumps inspect `schemaVersion`
  to 3, re-diff `aozora schema {nodes,diagnostics,gaiji}` old-vs-new before widening the
  adapter gate (`adapters/aozora/src/lib.rs`, the `matches!(…, 1 | 2)` check). v2 was a
  pure version bump; don't assume v3 is.
- **Full aozora re-measure** (~5–10 min): `nix develop --command bash -c 'export
  AB_AOZORA_BIN=… RUSTC_WRAPPER= SCCACHE_DISABLE=1; reports/aat-fidelity/run-aozora-aat-full.sh
  --out-dir <OUT> --jobs 32 --force'`. Confirm `aat/aozora-adapter/*.json` count = 17,886
  and check-reports have 0 `fatal_error` before trusting the dump.

## Artifacts & data locations

- **Study:** `docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md`
  (§4.7 coverage, §4.8 split, §4.9 parity, §7 reproducibility+re-pin, §8 open work).
- **Reports:** `2026-07-08-{fidelity-robustness-split,parity-support-audit}.{md,json}`,
  `2026-07-08-normalized-corpus-coverage.json`, `2026-07-08-corpus-adapter-fidelity.summary.json`
  (the denominators).
- **Tooling** (`reports/aat-fidelity/`): `normalized-corpus-coverage.py` (§4.7),
  `fidelity-robustness-split.py` (§4.8, reconciles with §4.7),
  `parity-support-audit.py` (§4.9), `corpus-adapter-fidelity-classifier.py` (denominators).
- **Full-corpus AAT dumps** (`/db/ab-validator/`): the 5 adapter dumps the scripts read
  (aozora `aat-corpus/aozora-full-20260705…`, etc.). **Scratch this session:**
  `aat-corpus/aozora-full-repin-1a4f864` (new parser; referenced by
  `parity-support-audit.py`'s auto-detect — keep it or repoint the script) and
  `aat-corpus/aozora-full-oldparser-nixcorpus` (control run, deletable).
- **Pins:** `ab-validator/flake.lock` — aozora `P4suta/aozora@1a4f864`, corpus
  `aozorabunko/aozorabunko@0e9ea3e`.

## Immediate next decision

Either **do §1 (full-nix denominators)** to make the study fully reproducible on the
pinned corpus, or **start the parser design** on the aozora-pipeline base. The research
arm is otherwise publication-adjacent; §2–6 are polish that doesn't change the verdict.
