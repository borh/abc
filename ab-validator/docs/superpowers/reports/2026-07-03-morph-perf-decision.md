# Morph-Pipeline Perf Decision — 2026-07-03

Decision-quality measurement for the three measurement-gated findings in
`docs/handoffs/crates-optimization-audit.md`. Baseline:
[`benchmarks/baselines/morph-2026-07-03.md`](../../benchmarks/baselines/morph-2026-07-03.md).

## §3.5 — WAREHOUSE_REGULAR_BATCH_SIZE = 32

- **Site:** `crates/ab-morph-run/src/lib.rs:38` (`WAREHOUSE_REGULAR_BATCH_SIZE = 32`); `take_batch` at `pipeline.rs:990`.
- **Measured:** 562 shards (parts per sharded table). morphemes: 86,480 row groups / 4.18B rows — 154 row groups per shard (NOT sub-row-group). analyses: 2,805 row groups / 265,335 rows — ~5 row groups per shard (~470 rows, sub-row-group). sources: 3,366 row groups / 106,134 rows — ~6 row groups per shard (~189 rows, sub-row-group).
- **Audit claim correction:** The audit said "most regular shards produce *less than one row group* of morphemes each." This is **disproven for morpheme tables** (154 row groups/shard) but **confirmed for small tables** (analyses/sources at 5-6 row groups/shard with <500 rows each).
- **Threshold (corrected from plan):** shard_count > 4 × max(1, total_morpheme_rows / 50_000) was the plan's formula — but it's the wrong metric. The real signal is: "do most shards produce sub-row-group data?" For the small tables, yes; for the morpheme tables, no.
- **Verdict: DEFER.** Raising the batch size from 32 to ~512 would reduce 562 shards to ~35, cutting small-table parts dramatically. But peak RSS is already 50.6 GiB (53,121,052 kB) — the per-analysis `source_text.clone()` (§3.10) is the memory ceiling, and a larger batch holds more analyses in flight simultaneously. Tuning without an RSS-monitored sweep at batch_size= 64/128/256/512 risks trading file-count savings for OOM. The focused tuning plan must sweep batch_size with RSS monitoring and compare before/after using the harness.

## §3.12 — No Parquet compaction on merge

- **Site:** `crates/ab-morph-run/src/pipeline.rs:1054` `merge_warehouse_shard_runs`; unused coalescing at `ab-warehouse/src/writer.rs:490+` (`append_parquet_table_file`, tested at `writer.rs:823`).
- **Measured:** Per-table parts and median part bytes:
  - analyses: 562 parts, median = 1,382 bytes → **>>
  - sources: 562 parts, median = 1,927 bytes → **>>
  - feature_pattern_counts: 562 parts, median = 49,247 bytes → **>>
  - morphemes: 562 parts, median = 10,525,753 bytes → OK
  - morpheme_features: 562 parts, median = 13,650,865 bytes → OK
  - nway_*: 562 parts, median 5.3-17.7 MiB → OK
- **Threshold:** any high-row table has > 64 parts AND median part bytes < 1 MiB (1,048,576).
- **Crossed?** Yes — 3 tables (analyses, sources, feature_pattern_counts) exceed both thresholds.
- **Verdict: TUNE.** Add a post-merge compaction pass for tables where part_count > 64 AND median_part_bytes < 1 MiB. Use the existing tested `append_parquet_table_file`. The 3 small tables total <2 GiB combined — compaction is cheap and safe.

## §3.15 — Analyzers serialize within a worker

- **Site:** `crates/ab-morph-run/src/pipeline.rs:582` (`for analyzer in analyzers` inside `std::thread::scope`).
- **Measured:** Only the primary run (jobs=32) completed. The jobs-scaling sweep (jobs=1) was attempted but takes ~hours (the j=32 run alone took 30:48; a j=1 run would be ~32× slower). User time was 38,692s over 1,848s wall → CPU utilization was 2,241% (22.4 of 32 cores). This suggests the pipeline already uses ~22 cores effectively at jobs=32, leaving only ~10 cores (31%) underutilized.
- **Threshold:** `scaling_speedup_primary_over_single < 0.6 × primary_jobs` on a ≥2-analyzer corpus.
- **Crossed?** Unknown — no single-jobs baseline was captured.
- **Verdict: DEFER.** Two indirect signals suggest §3.15 may be lower priority than initially thought: (1) the pipeline already saturates ~22 of 32 cores at jobs=32 — the per-worker analyzer serialization may not be the main bottleneck; (2) the large-work tail (few works that take much longer than others) would benefit more from work-stealing than from intra-worker analyzer parallelism. A focused §3.15 plan would need at minimum a jobs=1 and jobs=8 run (each ~30min and ~2h respectively) to establish the scaling curve. If the scaling curve is concave (diminishing returns at high job counts), §3.15 is TUNE; if linear, DEFER.

## Next-allowed plans

1. **§3.12 TUNE plan (highest ROI, lowest risk):** Write a separate implementation plan that adds a compaction pass to `merge_warehouse_shard_runs` for small-part tables. Before/after measurement via `benchmarks/run-morph-corpus.sh`. Safe because the 3 affected tables are small (<2 GiB) and the coalescing machinery is already tested.

2. **§3.5 TUNE plan (needs RSS monitoring):** Add RSS-annotated sweep (batch_size= 32/64/128/256/512) to the harness, run each, compare wall_time + peak_rss + part_count. Only commit if peak_rss stays within safe bounds (e.g. <60 GiB).

3. **§3.15 measurement plan (needs dedicated window):** Run jobs=1 and jobs=8 to establish the scaling curve. If concave, write the intra-worker `rayon::scope` plan.

No tuning diff lands in this plan — each TUNE verdict triggers a separate follow-up plan.
