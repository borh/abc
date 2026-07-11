# Gate: corpus-wide absorption byte-parity (Phase 2, `ab-aozora` vs pinned reference)

**Date:** 2026-07-10
**Authority:** `.superpowers/sdd/task-8-brief.md`, ADR 0032 (hard detach),
`docs/superpowers/reports/2026-07-10-fork-parity-corpus.md` (Phase 1 Gate A,
model for this report and for the reference-resolution recipe).
**Verdict:** `PASS`

## Gate definition

Byte equality of every per-work AAT document between the candidate dump
(built from the CANDIDATE_COMMIT `ab-aozora` binary) and the run-set-pinned
reference dump, with the two identity pointers `/meta/adapter` and
`/meta/adapter_version` substituted for a fixed placeholder before
comparison (`reports/aat-fidelity/compare-aat-dumps.py --bytes`; these two
pointers are the only sanctioned difference — they embed the producing
binary's identity). Missing/extra files or any other byte difference
diverge. The comparator also reports the semantic (JSON-equality-after-
pointer-removal) result for localization; both counts are 0 here.

## Candidate identity

Built on hinoki (`hinoki.hyakutake-barbel.ts.net`), detached worktree at
`~/Projects/soranoha/.worktrees/parser-fork-phase2` (`ab-validator/`
subdirectory), rev-injected build:

```bash
cd ~/Projects/soranoha && git fetch origin
git worktree add --detach ~/Projects/soranoha/.worktrees/parser-fork-phase2 9cbb7b7b22a3c1779b99300a3fbe3e8d0ec12b33
cd ~/Projects/soranoha/.worktrees/parser-fork-phase2
test -z "$(git status --porcelain)" && git rev-parse HEAD
# -> 9cbb7b7b22a3c1779b99300a3fbe3e8d0ec12b33  (clean detached tree, confirmed)

cd ab-validator
export RUSTC_WRAPPER= SCCACHE_DISABLE=1 AB_AOZORA_GIT_REV=9cbb7b7b22a3c1779b99300a3fbe3e8d0ec12b33
cargo build --package ab-aozora --release
./target/release/ab-aozora --version | grep -F 9cbb7b7b22a3c1779b99300a3fbe3e8d0ec12b33
sha256sum ./target/release/ab-aozora
```

Recorded identity triple:

- **commit:** `9cbb7b7b22a3c1779b99300a3fbe3e8d0ec12b33`
- **`--version` (verbatim):** `ab-aozora 0.1.0 aat-schema 1 facade 0.1.0 wire-schema 2 (git 9cbb7b7b22a3c1779b99300a3fbe3e8d0ec12b33)`
- **binary sha256:** `0f699aa57d8420f76269ccd0c302d9a7578d02246876ad18443b2847803851e0`

The candidate binary's own generated `metadata.json` (from the full-corpus
run below) independently records the same triple under
`adapter_bin_override` — proof the override reached the actual subprocess
`ab-check` invoked, not merely that a flag was passed.

## Reference dump (resolved fail-closed via the run-set)

Resolved on hinoki through `reports/aat-fidelity/run-sets/current.json`
(`run_set_id: current-aat-fidelity-2026-07-09`) with
`AB_DB_ROOT=/db/ab-validator`, hashing the on-disk tree with the
repository's own recipe (`reports/lib/aat_hash.py::hash_aat_dir`), the same
invocation as Phase 1's Gate A report:

```json
{
  "run_set_id": "current-aat-fidelity-2026-07-09",
  "aat_dir": "/db/ab-validator/aat-corpus/aozora-full-repin-1a4f864/aat/aozora-adapter",
  "expected": "sha256:9860a7b64bce8b0af2f09f587df239a95ae7ffe9afd97961447306b022cecfa0",
  "actual": "sha256:9860a7b64bce8b0af2f09f587df239a95ae7ffe9afd97961447306b022cecfa0",
  "match": true
}
```

Verification held (`match: true`); the reference dump used below is exactly
the run-set-named tree, never an unpinned substitute.

## Candidate full-corpus run (detached)

```bash
cd ~/Projects/soranoha/.worktrees/parser-fork-phase2/ab-validator
AB_DB_ROOT=/db/ab-validator nohup reports/aat-fidelity/run-aat-full.sh \
  --adapter ab-aozora --adapter-bin ./target/release/ab-aozora \
  --jobs 32 --report-id ab-aozora-phase2-parity \
  --out-dir /db/ab-validator/aat-corpus/ab-aozora-phase2-9cbb7b7b \
  > ~/phase2-parity-run.log 2>&1 &
```

- Workflow: `status: passed`, 4/4 steps, started
  `2026-07-10T10:48:22Z`, ended `2026-07-10T10:51:34Z` (~3m12s).
- Out dir: `/db/ab-validator/aat-corpus/ab-aozora-phase2-9cbb7b7b`
  (retained per the brief — not deleted; stays until Phase 4).
- AAT files produced: **17,886** (`aat/ab-aozora/*.json`), matching
  `check-reports` count; `fidelity.duckdb` loaded 17,886 batch reports.
- `metadata.json`: `repo_head` = candidate commit, `repo_dirty: false`,
  `adapter_bin_override` triple identical to the recorded identity above.

## Comparator output

```bash
cd ~/Projects/soranoha/.worktrees/parser-fork-phase2/ab-validator
python3 reports/aat-fidelity/compare-aat-dumps.py --bytes \
  /db/ab-validator/aat-corpus/aozora-full-repin-1a4f864/aat/aozora-adapter \
  /db/ab-validator/aat-corpus/ab-aozora-phase2-9cbb7b7b/aat/ab-aozora
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

**`PASS`** — all 17,886 works in the run-set's full-corpus `aozora` lane
compared; 0 missing, 0 semantic divergence, 0 byte divergence (after the
two-pointer identity substitution). The Phase 2 candidate `ab-aozora`
binary at commit `9cbb7b7b22a3c1779b99300a3fbe3e8d0ec12b33` reproduces the
pinned reference dump's AAT output byte-for-byte over the full corpus,
modulo the sanctioned `/meta/adapter` and `/meta/adapter_version` identity
pointers.

## Retention

The candidate dump `/db/ab-validator/aat-corpus/ab-aozora-phase2-9cbb7b7b`
and the reference dump `/db/ab-validator/aat-corpus/aozora-full-repin-1a4f864`
are both retained on hinoki (neither deleted, per the brief). Scratch logs
`~/phase2-parity-run.log` and `~/phase2-parity-summary.json` on hinoki are
left in place per the task instructions (retained until Task 12 completes).
