# Gate: corpus-wide sanitize-swap byte-parity (Phase 3 stage 0, `ab-aozora` vs Phase 2 dump)

**Date:** 2026-07-11
**Authority:** `.superpowers/sdd/task-3-brief.md`, ADR 0032 (hard detach),
`docs/superpowers/reports/2026-07-10-phase2-absorption-parity.md` (model
for this report and for the identity/build recipe).
**Verdict:** `PASS`

## Gate definition

Task 1 (commit `8d5b4c38`) swapped `ab-aozora-aat`'s sanitize from the
crates.io implementation to the fork implementation, with zero intended
behavior change. This gate proves that corpus-wide: byte equality of every
per-work AAT document between the Phase 3 stage 0 candidate dump (built
from `C0 = c3cb16908b0134ca03856735f10445eeb2a92957`, the ledger-recorded
HEAD after Task 2) and the retained Phase 2 dump
(`/db/ab-validator/aat-corpus/ab-aozora-phase2-9cbb7b7b`), with the two
identity pointers `/meta/adapter` and `/meta/adapter_version` substituted
for a fixed placeholder before comparison
(`reports/aat-fidelity/compare-aat-dumps.py --bytes`). Missing/extra files
or any other byte difference diverge. The comparator also reports the
semantic (JSON-equality-after-pointer-removal) result for localization;
both counts are 0 here.

This gate, together with the conformance-echo gate
(`docs/superpowers/reports/2026-07-11-phase3-stage0-conformance-gate.summary.json`)
and the perf gate
(`docs/superpowers/reports/2026-07-11-phase3-stage0-perf.md`), forms the
complete Phase 3 stage 0 gate. All three share one candidate identity.

## Candidate identity

Built on hinoki (`hinoki.hyakutake-barbel.ts.net`), detached worktree at
`~/Projects/soranoha/.worktrees/parser-fork-phase3` (`ab-validator/`
subdirectory), rev-injected build:

```bash
cd ~/Projects/soranoha && git fetch origin
git worktree add --detach ~/Projects/soranoha/.worktrees/parser-fork-phase3 c3cb16908b0134ca03856735f10445eeb2a92957
cd ~/Projects/soranoha/.worktrees/parser-fork-phase3
test -z "$(git status --porcelain)" && git rev-parse HEAD
# -> c3cb16908b0134ca03856735f10445eeb2a92957  (clean detached tree, confirmed)

cd ab-validator
export RUSTC_WRAPPER= SCCACHE_DISABLE=1 AB_AOZORA_GIT_REV=c3cb16908b0134ca03856735f10445eeb2a92957
cargo build --package ab-aozora --release
./target/release/ab-aozora --version | grep -F c3cb16908b0134ca03856735f10445eeb2a92957
sha256sum ./target/release/ab-aozora
```

Recorded identity triple:

- **commit:** `c3cb16908b0134ca03856735f10445eeb2a92957`
- **`--version` (verbatim):** `ab-aozora 0.1.0 aat-schema 1 facade 0.1.0 wire-schema 2 (git c3cb16908b0134ca03856735f10445eeb2a92957)`
- **binary sha256:** `9a41cbfb4d6e61c74261501d26f6af34b50cc68f60cc5f5acd000e867affd6a6`

The candidate binary's own generated `metadata.json` (from the full-corpus
run below) independently records the same triple under
`adapter_bin_override` — proof the override reached the actual subprocess
`ab-check` invoked, not merely that a flag was passed.

## Reference dump

The retained Phase 2 dump `/db/ab-validator/aat-corpus/ab-aozora-phase2-9cbb7b7b`
(built from candidate `9cbb7b7b22a3c1779b99300a3fbe3e8d0ec12b33`, Phase 2
Task 8, verified via the run-set hash there). Neither this dump nor any
other `/db/ab-validator/` content was deleted by this task; only the new
stage 0 dump directory was created.

## Candidate full-corpus run (detached)

```bash
cd ~/Projects/soranoha/.worktrees/parser-fork-phase3/ab-validator
AB_DB_ROOT=/db/ab-validator nohup reports/aat-fidelity/run-aat-full.sh \
  --adapter ab-aozora --adapter-bin ./target/release/ab-aozora \
  --jobs 32 --report-id phase3-stage0 \
  --out-dir /db/ab-validator/aat-corpus/ab-aozora-phase3-stage0-c3cb169 \
  > ~/phase3-stage0-run.log 2>&1 &
```

- Workflow: `status: passed`, started `2026-07-11T05:13:19Z`, ended
  `2026-07-11T05:16:37Z` (~3m18s).
- Out dir: `/db/ab-validator/aat-corpus/ab-aozora-phase3-stage0-c3cb169`
  (new dump directory, this task's only creation under `/db/ab-validator/`;
  this is also Task 10's delta baseline).
- AAT files produced: **17,886** (`aat/ab-aozora/*.json`); `fidelity.duckdb`
  loaded 17,886 batch reports.
- `metadata.json`: `repo_head` = `c3cb16908b0134ca03856735f10445eeb2a92957`,
  `repo_dirty: false`, `adapter_bin_override` triple identical to the
  recorded identity above.

## Comparator output

Note: both dumps store their per-work JSON files one level deeper than the
literal `aat/` path — under `aat/ab-aozora/*.json` — matching the exact
subdirectory the corpus-run harness always produces (`{aat_dir}/{adapter_id}/`).
The comparator (`pathlib.Path.glob("*.json")`, non-recursive) requires the
adapter subdirectory itself, so both arguments below point one level past
each dump's `aat/`.

```bash
cd ~/Projects/soranoha/.worktrees/parser-fork-phase3/ab-validator
python3 reports/aat-fidelity/compare-aat-dumps.py --bytes \
  /db/ab-validator/aat-corpus/ab-aozora-phase2-9cbb7b7b/aat/ab-aozora \
  /db/ab-validator/aat-corpus/ab-aozora-phase3-stage0-c3cb169/aat/ab-aozora
```

```json
{
  "compared": 17886,
  "missing_count": 0,
  "missing_sample": [],
  "semantic": {
    "diverged_count": 0,
    "diverged_sample": []
  },
  "bytes": {
    "diverged_count": 0,
    "diverged_sample": []
  }
}
```

Exit code 0.

## Verdict

**`PASS`** — all 17,886 works compared; 0 missing, 0 semantic divergence,
0 byte divergence (after the two-pointer identity substitution). The Phase
3 stage 0 candidate `ab-aozora` binary at commit
`c3cb16908b0134ca03856735f10445eeb2a92957` reproduces the retained Phase 2
dump's AAT output byte-for-byte over the full corpus, modulo the sanctioned
`/meta/adapter` and `/meta/adapter_version` identity pointers — confirming
Task 1's sanitize swap (crates.io → fork implementation) is behavior-
preserving corpus-wide, not merely on the tripwire's single fixture.

## Conformance echo (companion gate)

Run locally (not on hinoki) against both frozen suites, joined on the
candidate's `--version` line (identity, not binary bytes — the local build
has `(git unknown)` since it wasn't rev-injected, which is acceptable
because the row comparator ignores identity and scores content only):

```bash
just aozora-notation-spec-comparison "" \
  docs/superpowers/reports/2026-07-11-phase3-stage0-conformance.md \
  docs/superpowers/reports/2026-07-11-phase3-stage0-conformance.summary.json
just aozora-notation-spec-comparison "$PWD/reports/parser-conformance/official-docs-seed" \
  docs/superpowers/reports/2026-07-11-phase3-stage0-conformance-seed.md \
  docs/superpowers/reports/2026-07-11-phase3-stage0-conformance-seed.summary.json

python reports/parser-conformance/compare-adapter-rows.py \
  docs/superpowers/reports/2026-07-10-phase2-conformance-echo-p4suta.summary.json \
  docs/superpowers/reports/2026-07-11-phase3-stage0-conformance.summary.json \
  --adapter ab-aozora
# compared=127 differing=0

python reports/parser-conformance/compare-adapter-rows.py \
  docs/superpowers/reports/2026-07-10-phase2-conformance-echo-official-seed.summary.json \
  docs/superpowers/reports/2026-07-11-phase3-stage0-conformance-seed.summary.json \
  --adapter ab-aozora
# compared=30 differing=0
```

Note: the `official-docs-seed-comparison` wrapper recipe (justfile) is
broken under `just` 1.55 when invoked with `NAME=value` CLI syntax — `just`
does not parse `KEY=value` tokens as named recipe parameters, it passes
them as positional strings verbatim (confirmed with `just --dry-run`), and
the wrapper's own body calls `aozora-notation-spec-comparison` internally
using that same broken syntax regardless of how the wrapper itself is
invoked. Phase 2's Task 5 already worked around this by calling
`aozora-notation-spec-comparison` directly with the seed vectors path as a
positional argument; this task follows the same precedent. Pre-existing
issue, not introduced or touched by this task.

Both suites: `differing=0`, exit 0. Per-adapter (`ab-aozora`) totals match
the frozen baselines exactly: 113 pass / 9 warning / 0 fail / 5 skip
(P4suta, 127 vectors) and 22 pass / 8 warning / 0 fail / 0 skip (official
seed, 30 vectors). Gate summary:
`docs/superpowers/reports/2026-07-11-phase3-stage0-conformance-gate.summary.json`.

## Perf (companion gate)

See `docs/superpowers/reports/2026-07-11-phase3-stage0-perf.md` for the
full lane setup and results. Summary: 0 new timeouts, workset median
-33.92% (candidate faster), well within the 10% regression threshold.

## Retention

The candidate dump `/db/ab-validator/aat-corpus/ab-aozora-phase3-stage0-c3cb169`
and the reference dump `/db/ab-validator/aat-corpus/ab-aozora-phase2-9cbb7b7b`
are both retained on hinoki (neither deleted). Scratch logs
`~/phase3-stage0-run.log`, `~/phase3-stage0-parity.json`,
`~/phase3-stage0-build.log`, `~/phase3-stage0-perf.log`, and
`~/phase3-stage0-perf.json` on hinoki are left in place.
