# Handoff: Interestingness-Ranking Next Phases (2026-07-06)

Three work items selected by the owner, in priority order. Each is independently executable; do them as separate spec→plan→implement cycles (brainstorming → writing-plans → subagent-driven-development), except item 2 whose spec section is already the plan skeleton.

## Shared state (verified 2026-07-06)

- **Canonical warehouse:** `/db/ab-validator/morph-warehouse/runs/full-2026-07-05_164518-jobs0` — 17,885 sources × 4 analyzers (`vibrato` cwj, `vibrato:unidic-novel-202512`, `sudachi-a`, `sudachi-c`), 0 errors, 43 GB. 161,142,784 nway_regions; 23,356,986,673 nway_feature_diffs; 662,984,226 morphemes. Row counts verified exactly against an independent rerun; `granularity_profile = "suw+luw"`, `rarity_basis = "source"` (that's item 1's target), `feature_profile = "core"`.
- **Governing spec:** `docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md` (MVP v1 shipped + Phase 2; §Score Versioning, §Calibration Plan, §MVP v2 sidecars all current). Granularity vocabulary is suw/muw/luw composition (Phase 2 status note).
- **Ranking command:** `ab-morph-run summarize-warehouse-interesting --run-dir <run> --limit 50 --format json` (DuckDB engine auto; ~10 min full corpus; `AB_DUCKDB_BIN` set in env). Latest top-50 output: repo `scratch/full-novel-interesting.json` (pre-dates the canonical swap but the swapped run is row-identical, so it remains valid).
- **Operational facts:** tests ALWAYS `cargo test -p ab-morph-run --features test-analyzer` (bare invocation false-fails 2 bin tests); warehouse recipes default `jobs=8` (measured wall optimum region; `--jobs 0` = memory-aware auto, resolves ~19 on this box and is slower); analyze-run staging lives at `<warehouse>/.staging/shards-<run_id>/` and self-heals; vibrato dictionaries are nix-only via `dictionary/compiled/` symlinks (`just dictionary-build-all` relinks after GC; repo-root `dictionary` is a tracked symlink to `../vibrato-pipe/dictionary` and dangles in worktrees — `ln -sfn /home/bor/Projects/ab-validator/dictionary dictionary`, restore with `git checkout -- dictionary` before committing).

## Item 1: `aozora_works` import → work-based rarity

**Goal:** `ab-morph-run import-aozora-metadata --from <abc export dir>` materializes `aozora_works.parquet` as an imported projection of ABC's `metadata-record.schema.json`; the ranker's rarity denominator then flips from distinct sources to distinct `work_id` (`rarity_basis = "work"`), fixing the ~2× frequency inflation from paired 旧字/新字 editions.

- Spec sections: §Cross-Repo Dependency on `abc` (field mapping table — follow it verbatim: `author_person_id` not free-text names, `orthographic_style` enum verbatim, `metadata_record_schema_hash` provenance), §v2 sidecar `aozora_works.parquet` table def, §Error Behavior row "import out of sync with ABC schema" (hard error naming expected/observed hashes).
- The summarizer side already exists: `rarity_config` / `read_optional_work_map` in `crates/ab-morph-run/src/summary/interesting.rs` probe for `aozora_works.parquet` in the run dir and switch `rarity_basis` — only the producer is missing. Check the fixture writer `write_aozora_works` in interesting.rs tests for the column shape currently consumed (the spec's full table has more columns; the reader needs `work_id`/`source_id` at minimum — reconcile).
- ABC repo: `../abc` (`/home/bor/Projects/abc`); schema at `schemas/metadata-record.schema.json`; boundary doc `abc/docs/v0-design-bundle/ab-validator-boundary.md`. Where ABC exports live on disk is an open question for the owner.
- Sidecar-only rule: new table, no existing column changes. Whether this alone warrants the schema_version 2 bump (spec Decision 3 says bump at FIRST sidecar) needs a decision at plan time — bumping requires the sql.rs reader-max relaxation described in §MVP v2 Principle.

## Item 2: Calibration Plan (locks v1 scoring)

**Goal:** run the spec's §Calibration Plan (steps 1-9) against the canonical warehouse; end state = locked v1 defaults recorded in `score_version`, and a measured verdict on whether RRF beats frequency sort at p@50 (spec: "if it doesn't, the feature isn't earning its complexity").

- Mechanical parts runnable without the owner: three-profile runs (smoke ~100 / triage ~1000 / full), frequency-sort + random baselines, within-kind vs global RRF A/B (needs a `--rank-scope global` mode — small summarizer addition), λ_missing sensitivity (NOTE: v1 shipped a rank-floor λ policy, not the spec's fixed constant — the sweep design must adapt; see the v1 plan's deviation notes), `anomaly_w_cov ∈ {2,5,10}` sweep with top-10 anomaly inspection.
- Owner-in-the-loop part: labeling the top-50 with verdicts (`bug`/`expected-policy`/`expected-dictionary`/`corpus-artifact`/`noise`/`unclear`) to compute p@50 and nDCG@50. Design the label-collection format up front (TSV the owner fills?) so their time is spent once.
- Best ordered AFTER item 1 (work-based rarity changes the rarity signal, which reshuffles rankings — calibrating before it locks against data known to be inflated). If item 1 stalls on ABC export availability, calibrate with `rarity_basis="source"` and record it in the comparability block honestly.

**Status (2026-07-07): DONE** — implemented in the soranoha monorepo
(`ab-validator/`), branch `feat/projection-spans`; spec
`docs/superpowers/specs/2026-07-06-projection-spans-design.md`, plan + full
validation record `docs/superpowers/plans/2026-07-06-projection-spans.md`.
Canonical warehouse is now `full-2026-07-06_160136-jobs8` (schema_version 2,
projection_spans 10.83M rows). The cycle also landed memory fixes P1–P3
(drop AAT DOM post-projection; no ortho-off text clone; flat boundary
vectors): peak RSS 36.0 GB at jobs=8 vs the 65.6 GB jobs=19 baseline.
Carried-forward perf items for a future window: mmap-backed dictionary
loading (~13.1 GB fixed residency — the largest remaining term),
§3.15 intra-worker parallelism (wall-clock lever), batch-size sweeps,
auto-jobs constant re-fit with the new P1–P3 numbers, and a views.sql fix
for part-directory tables (flat `read_parquet` paths miss them). Next in
the pipeline: Phase 4 ruby oracle (`nway_region_oracle_evidence`), which
this bridge unlocks.

## Item 3: Phase 3 — `projection_spans` + ruby oracle groundwork

**Goal:** the minimal structural bridge sidecar: extend `ab-plaintext`'s `visible_text_projection` (crates/ab-plaintext/src/aat.rs) to optionally emit projected-char-offset → AAT-inline-node mappings; write `projection_spans.parquet` during the analysis pass; schema in `ab-warehouse/src/schema.rs`. This unlocks the ruby oracle (spec §Aozora Oracles — ABC measured ruby.direction as the single largest divergence category, ~1.76M occurrences), which is v2's highest-leverage signal.

- Spec sections: §v2 sidecar `projection_spans.parquet` (column table), Phase 3 bullet list, Open Question 2 (resolved lean: generate during analysis pass, not a separate preprocessing step).
- This is the item that actually triggers the SCHEMA_VERSION 1→2 bump + `sql.rs` assertion relaxation if item 1 didn't already do it.
- Full-corpus regeneration required for the sidecar to exist warehouse-wide (~40 min at jobs=8) — schedule the run as part of validation, and note the analysis pass currently writes 12 tables; the writer's two-phase commit and per-shard merge must include the new table (see `WarehouseWriter::create_for_tables`, `merge_warehouse_shard_runs`).
- The ruby-oracle consumer itself (`nway_region_oracle_evidence`) is Phase 4 — keep Phase 3 scoped to the bridge unless the owner says otherwise.

## Deferred/parked (not selected, for the record)

- SUW-only comparison run (`just morph-warehouse-run-suw`, ~40 min) — cheap policy-noise quantification whenever wanted.
- §3.15 intra-worker parallelism — the real wall-clock lever (jobs-scaling showed high jobs hurt); needs its own measurement window. Jobs-vs-wall sweep script exists in session scratchpad if ever needed.
- Minor code blemishes noted in reviews: empty-run `rarity_basis` hardcoded "source" in the early return; auto-jobs log wording on the `MemAvailable=0` edge; `RerunFull` still rejects `--jobs 0`.
