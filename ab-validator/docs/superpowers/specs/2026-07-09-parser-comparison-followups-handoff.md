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

### 1. Full-nix-corpus denominator recompute — ✅ DONE 2026-07-09
**Report:** `docs/superpowers/reports/2026-07-09-full-nix-denominator-recompute.md`
(commit `9d2a62d2`). Ran `ab-source-inventory` on the pinned corpus. **Headline: the
source-authority denominators are corpus-invariant** — 17/21 constructs byte-identical
to the local extraction; only `gaiji.marker` −4 and `gaiji.un_embed` −3 (an 8-work count
diff); `jisage` identical. This *retires* the §7 corpus caveat: the feared phantom
gaiji/jisage drop was a numerator artifact (pre `ab-index` fix), not denominator drift.
Fully-pinned re-run (repin num + pinned denom) leaves all rankings unchanged (aozora
0.957 #1; §4.8 robustness identical + fidelity ordering preserved; §4.9 drops hold).
Resolves §2 gaps: keigakomi scoreable (denom 717); yokogumi/jizume corpus-invariant.
Tooling (run-set-native): `rebuild-fidelity-summary.py`, `run-coverage-report.sh`.
**Also surfaced + documented a reproducibility hole** (stale `AB_*_AAT_DIR` profile env
vars silently override the run-set) — `2026-07-09-fidelity-workflow-integration.md`.

<details><summary>original §1 brief (superseded)</summary>

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
</details>

### 2. Close `aozora-pipeline`'s construct gaps — ✅ DONE 2026-07-09
**Report:** `docs/superpowers/reports/2026-07-09-aozora-pipeline-construct-gap-backlog.md`
(commit `472021dd`). Verified all three (`jizume`/`yokogumi`/`keigakomi`) are **true
drops** in the HEAD parser (`1a4f864`) via a fresh full-corpus vocabulary scan; notation
verified against official `aozora.gr.jp/annotation/etc.html`; real corpus counts measured.
**Key finding:** the parser already *tokenizes* all three as generic
`containerOpen`/`containerClose` `raw` nodes **with the exact source string + span
preserved** — it just never *classifies* them (cf `jisage_block`). So closing the gaps
is a **classification task, not tokenization** → low-risk, upstream-able. Filed 6
prioritized backlog items (3 parser, 3 instrument). *Instrument gaps surfaced:*
keigakomi has no source-authority denominator (first empirical figure: 106 works / 200
starts, exactly matching aozora-rs's 200 nodes); yokogumi denominator (3690) is ~20×
observed starts — resolve with item §1.

### 3. Expand the performance sample — ✅ DONE 2026-07-09
**Report:** `docs/superpowers/reports/2026-07-09-aozora-core-perf-pathology.{md,json,workset.json}`
(commit `c3dfa8fc`; study §4.6/§5 threat #5 updated). Controlled contrast — aozora-core on
the 30 works it fails at corpus scale vs 30 *larger* works it completes (controls' median
845KB > giants' 537KB), 180s limit. **Verdict: work-specific + ruby-density driven, NOT
size-driven.** Controls all complete (median 1.25s, max 16s, 0 timeouts); ruby-dense
giants blow up (median 34.7s, max 173s, 12/30 timeouts, **0 crashes**); giants carry ~9×
the ruby density (21.4 vs 2.4 ruby/KB). Confirms §4.8 (same defect). All-timeouts-no-crashes
⇒ a superlinear ruby-handling algorithm; a cheap fix is *plausible* but unproven (would
need a profiling pass on e.g. `001562_56145`). Verdict unchanged (aozora-core still trails
coverage 0.855). Tooling: `analyze-aozora2-giants-perf.py`.

### 4. `aozora-rs-core` gaiji blind spot / adapter typed projection — ⏸ ASSESSED, STAYS DEFERRED
**Assessment:** `docs/superpowers/reports/2026-07-09-aozora-rs-adapter-repair-assessment.md`
(commit `0ec732ce`). Measured: adapter **builds clean** (README "not building" is STALE);
**fallback-dominant quantified** — ~97% of nodes `source_fallback`, 80% of works
fallback-dominant, so the production adapter reflects the source lexer not aozora-rs-core.
Repair sites confirmed (`aat.rs:241` drops Kunten/Okurigana; block Deco under-mapped;
`lib.rs:227-232` round-trip gate; + retokenized gaiji blind spot). ~1 session, real
regression risk. **Condition (aozora-rs throughput-first) NOT met** → keep deferred.
Cheap safe follow-up: fix the stale README.

### 5. Expand the official-docs seed (§4.5) — ✅ DONE 2026-07-09
**Report:** `docs/superpowers/reports/2026-07-09-official-docs-seed-expansion.md`
(commit `7c1834b5`). `author-official-seed.py` expanded **11 → 30** vectors, all
provenanced to official docs (傍点/傍線 variants, heading levels+forms, gaiji
Unicode/description/kana, accent decomposition, Latin-base ruby). Activation vs the
recommended parser: 14/19 new edge cases pass; 5 diverge (underline sub-family fold,
accent not typed, range-form heading) — new finer-grained findings. Precise spans + full
4-adapter re-score noted as follow-ups.

### 6. Uniform per-parser methodology write-up — ✅ DONE 2026-07-09
**Report:** `docs/superpowers/reports/2026-07-09-measurement-methodology.md` (commit
`26230fe6`). Clarifies the two measurement layers: the verdict rests on the
**corpus-coverage layer (§4.7-4.9), already a single uniform axis** (all 5 parsers, one
method); the "three methods" (§3.2) are confined to the conformance-breadth
corroboration layer. Anchored by the AAT validation oracle, two-denominator agreement,
and §1's corpus-invariant denominators. Study §5 threat #2 updated.

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

**Done 2026-07-09:** §1, §2, §3, §5, §6 complete; §4 assessed and kept deferred
(condition unmet). None changed the verdict. The research arm is now
**publication-complete** on every axis, fully reproducible on the pinned corpus, with
the measurement methodology and independent instrument documented.

**Remaining, in priority order:**
1. **Start the parser design** on the `aozora-pipeline` base (`brainstorming` →
   `writing-plans`), against the 2026-07-06 acceptance criteria — the forward move.
2. Small opt-ins surfaced by the followups (none block the design):
   - fix the stale `adapters/aozora-rs/README.md` "not building" status (§4).
   - neutralize the stale `AB_*_AAT_DIR` profile env overrides + close the run-set
     validation hole (`2026-07-09-fidelity-workflow-integration.md`); note these touch
     infra under active development in sibling worktrees.
   - the §4 adapter repair itself — only if aozora-rs is reconsidered on throughput.
