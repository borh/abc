# Gate: local conformance echo (Phase 2, `ab-aozora` vs `aozora-adapter`)

**Date:** 2026-07-10
**Authority:** `.superpowers/sdd/task-10-brief.md`, ADR 0032 (hard detach),
Task 7's justfile lanes + `reports/parser-conformance/compare-echo-lanes.py`.
**Verdict:** `PASS`

## Gate definition

The `just aozora-notation-spec-comparison` recipe runs both the frozen
`aozora-adapter` lane (the pre-fork adapter binary, `env AB_AOZORA_BIN=...
aozora-adapter --mode aat`) and the new `ab-aozora` lane (the Phase 2 fork
binary, `ab-aozora --mode aat`) over the same AAT-vector suite in one pass.
Since both lanes are supposed to be byte-parity ports of the same AAT logic,
`compare-echo-lanes.py` diffs their per-vector rows across two suites
(the 127-vector P4suta/aozora notation spec set and the 30-vector
official-docs seed) and asserts zero divergence. Any difference between the
two lanes is a port defect in `ab-aozora`, not an upstream-behavior question
— it would require fixing the fork, cutting a new candidate commit, and
re-running Tasks 8–10.

## Candidate identity

Built from a **detached** worktree at CANDIDATE_COMMIT (not the branch
tip, which has moved past it with evidence commits):

```bash
git worktree add --detach ~/Projects/soranoha/.worktrees/phase2-candidate \
  9cbb7b7b22a3c1779b99300a3fbe3e8d0ec12b33
cd ~/Projects/soranoha/.worktrees/phase2-candidate/ab-validator
test -z "$(git status --porcelain)" && git rev-parse HEAD
# -> 9cbb7b7b22a3c1779b99300a3fbe3e8d0ec12b33  (clean detached tree, confirmed)

export RUSTC_WRAPPER= SCCACHE_DISABLE=1 AB_AOZORA_GIT_REV=9cbb7b7b22a3c1779b99300a3fbe3e8d0ec12b33
cargo build --package ab-aozora --release
./target/release/ab-aozora --version | grep -F 9cbb7b7b22a3c1779b99300a3fbe3e8d0ec12b33
sha256sum ./target/release/ab-aozora
```

Recorded identity triple:

- **commit:** `9cbb7b7b22a3c1779b99300a3fbe3e8d0ec12b33`
- **`--version` (verbatim):** `ab-aozora 0.1.0 aat-schema 1 facade 0.1.0 wire-schema 2 (git 9cbb7b7b22a3c1779b99300a3fbe3e8d0ec12b33)`
  — exact byte-for-byte match to the cross-gate join key used by Tasks 8 and 9.
- **binary sha256 (this task's local build):**
  `4a1504c8b01a6d5f021b907e023426810a4fff610c4fa051096331e6d822aae3`
  — expected to differ from hinoki's build sha256
  (`0f699aa57d8420f76269ccd0c302d9a7578d02246876ad18443b2847803851e0`,
  recorded in Task 8/9's gate summaries) since the join key across gates is
  the commit + `--version` line, not the binary bytes; non-reproducible
  build metadata (paths, timestamps, mtimes) is expected to vary between
  hosts.

## Recipe runs (both suites, candidate tree)

Both runs used the nix-resolved pinned upstream `aozora` binary
(`nix build .#upstream-parser-aozora`) as `AOZORA_BIN`, invoked from the
candidate tree's `ab-validator/` so the candidate's own harness/justfile
code executes.

### Suite 1 — P4suta / aozora-notation-spec (127 vectors, default `VECTORS`)

```bash
just aozora-notation-spec-comparison "" echo-p4suta.md echo-p4suta.summary.json "$upstream"
```

Totals: 127 vectors x 7 adapters = 889 rows — 455 pass / 336 warning / 68 fail / 30 skip.

Per-adapter (pass/warning/fail/skip):

| adapter | pass | warning | fail | skip |
|---|---|---|---|---|
| `aozora` (upstream inspect) | 111 | 13 | 3 | 0 |
| `aozora-adapter` (frozen AAT) | 113 | 9 | 0 | 5 |
| `ab-aozora` (fork AAT) | 113 | 9 | 0 | 5 |
| `aozora2` | 48 | 64 | 10 | 5 |
| `aozora2html` | 49 | 57 | 16 | 5 |
| `aozora-rs` | 20 | 85 | 17 | 5 |
| `aozora-epub3` | 1 | 99 | 22 | 5 |

`aozora-adapter` and `ab-aozora` are **identical** row-for-row — expected,
since this is exactly the byte-parity relationship this gate exists to
verify.

### Suite 2 — official-docs seed (30 vectors)

```bash
just aozora-notation-spec-comparison "$PWD/reports/parser-conformance/official-docs-seed" \
  echo-official-seed.md echo-official-seed.summary.json "$upstream"
```

Totals: 30 vectors x 7 adapters = 210 rows — 120 pass / 80 warning / 10 fail / 0 skip.

Per-adapter (pass/warning/fail/skip):

| adapter | pass | warning | fail | skip |
|---|---|---|---|---|
| `aozora` (upstream inspect) | 14 | 14 | 2 | 0 |
| `aozora-adapter` (frozen AAT) | 22 | 8 | 0 | 0 |
| `ab-aozora` (fork AAT) | 22 | 8 | 0 | 0 |
| `aozora2` | 25 | 5 | 0 | 0 |
| `aozora2html` | 22 | 8 | 0 | 0 |
| `aozora-rs` | 15 | 15 | 0 | 0 |
| `aozora-epub3` | 0 | 22 | 8 | 0 |

`aozora-adapter` and `ab-aozora` are again identical row-for-row.

## Echo assertion

```bash
python3 reports/parser-conformance/compare-echo-lanes.py \
  echo-p4suta.summary.json echo-official-seed.summary.json \
  --lane-a aozora-adapter --lane-b ab-aozora \
  --out echo-lanes.json
```

```json
{
  "lane_a": "aozora-adapter",
  "lane_b": "ab-aozora",
  "vectors_compared": 157,
  "differing_count": 0,
  "differing": []
}
```

Exit code 0. `vectors_compared: 157` = 127 (P4suta) + 30 (official-docs
seed), `differing_count: 0` — the two lanes echo exactly across both
suites. Required conditions (exit 0, `differing_count` 0, `vectors_compared`
> 0) all hold.

## Verdict

**`PASS`** — the `ab-aozora` fork binary at commit
`9cbb7b7b22a3c1779b99300a3fbe3e8d0ec12b33` reproduces the frozen
`aozora-adapter` lane's AAT scoring exactly across all 157 vectors in both
conformance suites. No port defect found; no new candidate needed.

## Note: upstream `inspect` lane scores vs. the frozen 2026-07-08 summary (schemaVersion drift, not a regression)

The `aozora` (upstream `inspect`) lane's scores in this run (111 pass / 13
warning / 3 fail on the P4suta suite) differ from the frozen 2026-07-08
summary (113 pass / 11 warning / 3 fail) and are **not directly comparable**
to a pre-Task-7 run of this harness. Task 7's investigation
(`.superpowers/sdd/task-7-report.md`, section 3) found the root cause: the
`P4suta/aozora` upstream binary pin moved from `5df2cfa5` (frozen-report
era) to `1a4f8646` (current, the Phase 0/1 ADR-0032 hard-detach base)
sometime between the two reports, while the vectors package itself did not
move. Before Task 7's fix, the harness's `schemaVersion` guard (checking
`!= 1`) rejected the current pin's `inspect` wire output (which reports
`schemaVersion: 2`), collapsing every one of the 127 vectors into a
`fail`/`warning` scored as if it never ran (0 pass / 102 warning / 25 fail)
— a silent degenerate-scoring failure mode, not a real regression. After
Task 7's fix (`!= 2`), the harness scores 111/13/3, which is a genuine
**2-vector move from pass to warning** relative to the frozen 113/11/3,
attributable entirely to the upstream binary pin bump (fail count identical
at 3, so no correctness regression): `keigakomi_inline_framed` (upstream
now emits a `directive` node with a `non_canonical_directive` diagnostic
instead of the expected `emphasis` node) and `line_bold_single` (upstream
now emits `lineGothic` instead of the expected `lineBold`). Both are
upstream-binary behavior changes unrelated to the fork's AAT port and
outside this gate's scope — this gate only asserts `aozora-adapter` vs
`ab-aozora` parity, which held exactly.

## Candidate worktree cleanup

```bash
cd ~/Projects/soranoha/.worktrees/parser-fork-phase2/ab-validator
git worktree remove ~/Projects/soranoha/.worktrees/phase2-candidate
```
