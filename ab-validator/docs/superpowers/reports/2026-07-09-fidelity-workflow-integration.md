# Fidelity-report workflow: integration & run simplification

**Date:** 2026-07-09
**Context:** the parser-comparison fidelity reports (§4.7 coverage, §4.8 split, §4.9
parity, §1 denominators) were migrated onto the new **AAT run-set** paradigm
(`reports/lib/aat_runs.py`, `run-sets/current.json`, `validate-aat-run-set.py`,
per-dump `metadata.json` descriptors) landed by the `aat-run-descriptors` /
`aat-materialization-workflow` merges. This note records how the followup tooling now
integrates with that paradigm, a **reproducibility hole** found while running §1, and
concrete simplifications.

## The paradigm (as it now stands)

- **Run descriptor** — each AAT dump dir carries a `metadata.json` (corpus, repo_head,
  `adapter_version`, index_path, …) written by the `run-*-aat-full.sh` runners.
- **Run-set** (`run-sets/current.json`) — selects, per adapter, an `aat_dir` +
  `run_descriptor` + an `expected` block (flake input, `rev`, `narHash`, `adapter_id`).
  Resolved by `aat_runs.adapter_aat_globs()`, which honors per-field env overrides
  (`<label>_aat_dir_env`, `<label>_run_descriptor_env`).
- **Consumers** — `normalized-corpus-coverage.py`, `fidelity-robustness-split.py`,
  `parity-support-audit.py` all now call `load_run_set()` + `adapter_aat_globs()`, so
  dump selection is centralized rather than hardcoded.
- **Validation** — `validate-aat-run-set.py` (`validate_run_set`) checks descriptor
  `adapter_id`, and `expected.source.rev/narHash` against `flake.lock`; `--require-paths`
  additionally requires the resolved dirs/descriptors to exist.

## How the followup tooling integrates

- **`rebuild-fidelity-summary.py` (§1)** — run-set-native: reads aozora2/aozora-rs
  AAT dirs via `adapter_aat_dirs(load_run_set())`, so its numerators track the same
  dumps the coverage scripts use. Denominators come from an `ab-source-inventory` run
  on the pinned corpus. Output: `2026-07-09-corpus-adapter-fidelity.summary.json`.
- **Denominator source** — the existing `just source-inventory-full` recipe already
  runs `ab-source-inventory`; §1 uses the same tool/matrix, just pointed at the pinned
  nix corpus + full index. No new instrument.
- **`analyze-aozora2-giants-perf.py` (§3)** — standalone perf analysis; not run-set
  coupled (it drives a single adapter binary, not AAT dumps).

## Reproducibility hole found while running §1 (action needed)

The shell profile exports stale `AB_*_AAT_DIR` values that **silently override the
run-set's pinned choices**:

| env var | points at | run-set default | effect |
| --- | --- | --- | --- |
| `AB_AOZORA_AAT_DIR` | `…/aozora-full-20260705…` (old **5df2cfa5**) | `…/aozora-full-repin-1a4f864` (HEAD) | **wrong dump, silently** |
| `AB_AOZORA_RS_AAT_DIR` | `…/scratch/morph-full-corpus/…` (**nonexistent**) | `…/fidelity-corpus/aozora-rs` | empty walk → all-zero numerators |

Evidence (this session): a naive `rebuild-fidelity-summary` run produced **aozora-rs
num = 0** across every construct (walked 0 files) because of the stale
`AB_AOZORA_RS_AAT_DIR`. Unsetting the overrides restored the correct 17,886-file walk.

**Why validation doesn't fully catch it.** `validate-aat-run-set --require-paths`
flags the *nonexistent* aozora-rs path, but does **not** flag the `AB_AOZORA_AAT_DIR`
swap: the old dump exists, and the descriptor is resolved *separately* (its own env is
unset), so it loads the run-set's default repin `metadata.json` — whose `rev` matches
`flake.lock`. Validation passes while the data dir is the wrong parser. The `aat_dir`
and `run_descriptor` can diverge because they are independent fields.

### Recommendations

1. **Neutralize the stale profile overrides** — remove `AB_AOZORA_AAT_DIR` /
   `AB_AOZORA_RS_AAT_DIR` (and siblings) from the profile, or repoint them at the
   run-set defaults. They predate the run-set and now actively defeat it.
2. **Bind `run_descriptor` to the resolved `aat_dir`** — ✅ *implemented 2026-07-09* as a
   coherence check in `validate_run_set`: it now errors if the resolved `aat_dir` is not
   under the resolved `run_descriptor`'s dump root, so a stale `*_AAT_DIR` override that
   swaps the dump is flagged (verified: the previously-silent `AB_AOZORA_AAT_DIR`→old-dump
   swap now fails validation; clean env still passes). A deeper version would *default*
   the descriptor to `<aat_dir>/../../metadata.json` so the two can't be set
   independently at all.
3. **Emit `metadata.json` for every dump** — aozora2 (`…-layout-fix5`) and aozora-rs
   (`fidelity-corpus/aozora-rs`) currently lack descriptors, so `--require-paths`
   validation fails even in a clean env. Backfill them (the `run-*-aat-full.sh` runners
   already know how to write one).
4. **Surface resolved dirs in report provenance** — `parity`/`FRS` emit `aat_run_set_id`;
   have the coverage scripts also record the *resolved* per-adapter dirs so any override
   drift is visible in the committed report, not just the environment.

## Simplifying the coverage run

Today a full fidelity-coverage refresh is 4 manual steps in a specific order, each
sensitive to the env overrides above:

```
just source-inventory-full INDEX=… CORPUS=…                      # denominators (source authority)
python reports/aat-fidelity/rebuild-fidelity-summary.py …        # summary (denominators + a2/rs num)
python reports/aat-fidelity/normalized-corpus-coverage.py SUMMARY # §4.7
python reports/aat-fidelity/parity-support-audit.py       SUMMARY # §4.9
python reports/aat-fidelity/fidelity-robustness-split.py  COV     # §4.8
```

**Proposed `just fidelity-coverage-report` recipe** (single entry point):

1. `env -u AB_*_AAT_DIR …` — run with the run-set as sole authority (or fail if any
   `AB_*_AAT_DIR` is set, forcing the run-set path).
2. `validate-aat-run-set --require-paths` — fail fast on drift before any long walk.
3. run the three coverage scripts against the run-set + current denominator summary,
   writing dated reports.

A committed driver (`reports/aat-fidelity/run-coverage-report.sh`) can encode steps 1–3
so the recipe is a thin wrapper; this keeps the ordering + clean-env contract in one
reviewed place instead of in operator memory. (Left as a proposal here rather than
edited into `justfile`/`aat_runs.py` directly, since the run-set infra is under active
development in sibling worktrees — fold in where it fits.)
