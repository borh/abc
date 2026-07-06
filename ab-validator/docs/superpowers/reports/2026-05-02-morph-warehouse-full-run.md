# Morph warehouse full corpus run

Date: 2026-05-02

## Command

```bash
TMPDIR=/home/bor/Projects/ab-validator/scratch/tmp \
TMP=/home/bor/Projects/ab-validator/scratch/tmp \
TEMP=/home/bor/Projects/ab-validator/scratch/tmp \
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
/run/current-system/sw/bin/time -v taskset -c 0-11 target/release/ab-morph-run analyze-aat \
  --aat-dir scratch/morph-full-corpus/aats/aozora-rs-adapter \
  --analyzer vibrato \
  --analyzer sudachi-a \
  --analyzer sudachi-c \
  --warehouse-dir scratch/morph-warehouse-full-2026-05-02/warehouse \
  --run-id full-2026-05-02-jobs12-dynamic \
  --jobs 12 \
  --progress
```

The run was launched with the user systemd manager and `RuntimeMaxSec=infinity` so it would not inherit the tool-session timeout. Temporary files were forced into `scratch/tmp`; `/tmp` was not used for this run.

## Result

The run completed and published a sealed warehouse run:

```text
scratch/morph-warehouse-full-2026-05-02/warehouse/runs/full-2026-05-02-jobs12-dynamic
```

The log includes:

```text
ab-morph-run: warehouse merging 592 dynamic shard(s) from 17894 input(s) into run_id=full-2026-05-02-jobs12-dynamic
ab-morph-run: warehouse merge complete run_id=full-2026-05-02-jobs12-dynamic
Exit status: 0
```

## Runtime and memory

```text
Wall time: 1:43:10
User time: 55307.04s
System time: 2256.16s
CPU: 929%
Peak RSS: 23,759,440 KiB (~22.7 GiB)
Swaps: 0
```

Startup memory was approximately 3-4 GiB with the dynamic scheduler, versus roughly 45 GiB when static size-balanced scheduling started all largest AAT files at once.

## Warehouse row counts

```text
runs                         1
run_analyzers                3
sources                      17,894
analyses                     53,682
morphemes                    505,038,964
morpheme_features            7,908,564,778
nway_regions                 165,146,539
nway_region_analyzers        495,439,617
nway_feature_diffs           17,829,159,648
errors                       0
```

## Storage

Total published run size: 34 GiB.

Largest tables:

```text
13G   nway_feature_diffs.parquet
9.0G  morpheme_features.parquet
6.9G  morphemes.parquet
3.5G  nway_region_analyzers.parquet
2.2G  nway_regions.parquet
```

The next storage target is reducing materialized feature facts, especially `nway_feature_diffs.parquet` and `morpheme_features.parquet`.

## Notes

The run used:

- dynamic warehouse work scheduling
- bounded large-document concurrency
- semantic chunking for Sudachi and Vibrato
- bounded warehouse morpheme/feature row batching
- partitioned Parquet merge for parallel shard output

The warehouse views loaded successfully in DuckDB. `top_segmentation_patterns` returned 325,397 rows on this run.
