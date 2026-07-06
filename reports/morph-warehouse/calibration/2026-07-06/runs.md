# Calibration subset runs — 2026-07-06

Deterministic AAT subsets + smoke/triage warehouse runs (calibration plan Task 6).

## Subset recipe

Built with `reports/morph-warehouse/calibration/make-aat-subset.sh` (sorted-stride
symlink farm, calibration D7): sort all `*.json` in the AAT dir, take every
`stride = total / n`-th file (indices `i * stride`, integer division), symlink
into the dest dir. Refuses to overwrite an existing dest.

Source corpus: `/db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter`
(17885 sources).

| subset | n | stride | dest |
|---|---|---|---|
| calib-smoke-100 | 100 | 17885 / 100 = 178 | /db/ab-validator/aat-corpus/subsets/calib-smoke-100 |
| calib-triage-1000 | 1000 | 17885 / 1000 = 17 | /db/ab-validator/aat-corpus/subsets/calib-triage-1000 |

Script output: `linked 100 of 17885 (stride 178)` / `linked 1000 of 17885 (stride 17)`.
Neither subset includes the 2 known-unmappable sources
(`000025_kantou-20379f2add12`, `000989_352-69e66488e1fe`).

## Warehouse runs

Command: `just morph-warehouse-run full <subset_dir> <run_id> 8`
(profile `full` = all tables; jobs=8; analyzers vibrato,
vibrato:unidic-novel-202512, sudachi-a, sudachi-c).

| run id | run dir | sources | analyzers | errors | wall time |
|---|---|---|---|---|---|
| calib-smoke-100 | /db/ab-validator/morph-warehouse/runs/calib-smoke-100 | 100 | 4 | 0 | 3m17.2s * |
| calib-triage-1000 | /db/ab-validator/morph-warehouse/runs/calib-triage-1000 | 1000 | 4 | 0 | 8m02.8s |

\* First run in a fresh worktree: includes a from-scratch `cargo build --release`
(1m47s) plus nix dictionary relinks; analyze phase alone was well under 1.5 min.
The triage wall time is near-pure analyze (cargo no-op 0.17s, nix relinks only).
Counts verified from each run's `runs.parquet` (`source_count`, `error_count`)
and by `count(*)` over `sources.parquet` / `errors.parquet`.

## Metadata import (flips rarity_basis to work)

Command: `cargo run --release -p ab-morph-run -- import-aozora-metadata --run-dir <run_dir> --from ../abc/out/corpus`

| run id | works imported | sources covered | skipped | wall time |
|---|---|---|---|---|
| calib-smoke-100 | 100 | 100 | 0 | 0.30s |
| calib-triage-1000 | 1000 | 1000 | 0 | 0.28s |

## Smoke-verify (calib-smoke-100)

`summarize-warehouse-interesting --run-dir .../calib-smoke-100 --limit 10 --format json`
— wall time 4.34s, 10 rows, run_id `calib-smoke-100`. Assertions on the
`score_version` block:

| assertion | observed |
|---|---|
| rarity_basis == "work" | "work" |
| rank_scope == "within-kind" | "within-kind" |
| score_mode == "rrf" | "rrf" |
