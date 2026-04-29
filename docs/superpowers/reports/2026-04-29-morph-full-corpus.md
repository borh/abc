# Morph full-corpus report

Date: 2026-04-29

## Scope

This run generated AAT with `aozora-rs-adapter` for the existing full index at `scratch/ab-index.json`, then compared `vibrato:unidic-cwj-202512` against `sudachi-c` over every emitted AAT file.

The AAT phase used `ab-check --jobs 16`. The morph phase was manually sharded into 8 symlink directories and executed with 8 parallel `ab-morph-run` release processes. This avoids the current single-process sequential bottleneck in `ab-morph-run`.

## Commands

AAT generation:

```bash
cargo build --release -p ab-check -p ab-morph-run

target/release/ab-check \
  --index scratch/ab-index.json \
  --corpus references/aozorabunko \
  --adapter adapters/aozora-rs/target/release/aozora-rs-adapter \
  --output scratch/morph-full-corpus/reports \
  --aat-output scratch/morph-full-corpus/aats \
  --jobs 16
```

Morph comparison was run in 8 shards using `target/release/ab-morph-run analyze-aat` with:

```bash
--analyzer vibrato
--analyzer sudachi-c
--errors-output <shard>/errors.jsonl
```

## Row counts

| artifact | rows/files |
| --- | ---: |
| AAT JSON files | 17894 |
| ab-check report JSON files | 17894 |
| analysis rows | 35788 |
| comparison rows | 17894 |
| error rows | 0 |

Analyzer rows are balanced: one Vibrato row and one Sudachi row for each AAT file.

## Failure results

No runtime failures were observed.

| failure class | count |
| --- | ---: |
| AAT generation failures | 0 observed |
| morph analyzer failures | 0 |
| comparison failures | 0 |
| coverage mismatch regions | 0 |
| texts with coverage mismatches | 0 |

The previous Sudachi maximum-input failures did not recur after chunking large Sudachi inputs.

## Boundary metrics

`boundary_f1` was non-null for 17892 comparisons and null for 2 comparisons. The null cases both have exactly one morpheme on each side, one one-to-one region, no segmentation regions, and no coverage mismatch, so there are no internal boundaries to score.

Null-boundary cases:

| text_id | from morphemes | to morphemes | one-to-one regions | segmentation regions | coverage mismatch regions |
| --- | ---: | ---: | ---: | ---: | ---: |
| `000293_60442` | 1 | 1 | 1 | 0 | 0 |
| `000293_48490` | 1 | 1 | 1 | 0 | 0 |

Boundary-F1 distribution over non-null comparisons:

| percentile | value |
| --- | ---: |
| min | 0.6884927066450566 |
| p01 | 0.854054054054054 |
| p05 | 0.9183098591549296 |
| p25 | 0.9628013526780844 |
| median | 0.972972972972973 |
| p75 | 0.9815528203200852 |
| p95 | 0.9887733887733887 |
| max | 1.0 |
| average | 0.9671854520946502 |

Segmentation-region totals:

| metric | value |
| --- | ---: |
| total segmentation regions | 7358000 |
| max segmentation regions in one comparison | 31082 |

Worst boundary-F1 rows:

| text_id | boundary F1 | segmentation regions | vibrato morphemes | sudachi morphemes | coverage mismatches |
| --- | ---: | ---: | ---: | ---: | ---: |
| `000081_43733` | 0.6884927066450566 | 114 | 2008 | 1079 | 0 |
| `000136_45052` | 0.7010309278350515 | 15 | 62 | 37 | 0 |
| `000081_53446` | 0.7272727272727272 | 13 | 109 | 69 | 0 |
| `001675_54805` | 0.7272727272727273 | 9 | 36 | 21 | 0 |
| `001172_44928` | 0.7310344827586206 | 58 | 185 | 107 | 0 |
| `000723_57391` | 0.7317073170731707 | 37 | 256 | 156 | 0 |
| `000136_45030` | 0.7317073170731707 | 14 | 78 | 47 | 0 |
| `000136_45050` | 0.7358490566037735 | 16 | 68 | 40 | 0 |
| `000286_49169` | 0.7399786617893614 | 1114 | 8189 | 4935 | 0 |
| `001633_54082` | 0.7440758293838862 | 32 | 266 | 158 | 0 |

These are analyzer segmentation differences, not adapter coverage failures.

## Documented issues

### 1. Whole-corpus JSONL artifacts are too large

The first combined output pass produced roughly 293 GB in top-level JSONL files:

| file | size |
| --- | ---: |
| `scratch/morph-full-corpus/analyses.jsonl` | 145721451221 bytes |
| `scratch/morph-full-corpus/comparisons.jsonl` | 147675403105 bytes |
| `scratch/morph-full-corpus/errors.jsonl` | 0 bytes |

Those combined files were deleted after counts were recorded because they duplicated the per-shard outputs and the filesystem had only about 61 GB free. The retained full-corpus scratch directory is still about 277 GB.

Root cause: `Analysis` and `Comparison` rows embed full `source_text` and full region/morpheme detail. That is useful for single-work inspection but too heavy for whole-corpus summary workflows.

Recommended fix: add compact output modes before repeated full-corpus runs:

- `--summary-output` for one row per comparison with only IDs, analyzer names, stats, and maybe a bounded examples budget;
- `--no-source-text` or a compact row schema for analyses/comparisons;
- optional zstd-compressed JSONL output;
- built-in sharding or `--jobs` to avoid external symlink shards.

### 2. Duplicate logical text IDs are present

There are 236 duplicate logical `text_id` values in the comparison output. This matches earlier pilot behavior: the corpus index can contain multiple records for one logical ID, and `ab-check` emits one AAT file per indexed record.

Sample duplicate IDs:

| text_id | rows |
| --- | ---: |
| `000008_1083` | 2 |
| `000013_542` | 3 |
| `000022_197` | 3 |
| `000023_1698` | 2 |
| `000023_199` | 3 |
| `000038_323` | 3 |
| `000040_380` | 3 |
| `000051_361` | 3 |
| `000074_431` | 3 |

Recommended fix: keep `ab-morph-run` file-oriented, but make aggregation/reporting explicit about whether it groups by AAT file or logical `text_id`.

### 3. `ab-morph-run` needs built-in parallelism for corpus use

The full morph phase was fast enough only after manual sharding into 8 directories. This should be built into the runner.

Recommended fix: add `--jobs N` to `ab-morph-run analyze-aat`, with per-worker analyzer instances and deterministic output merge order.

## Bottom line

The full corpus run found no analyzer failures, comparison failures, or coverage mismatch bugs. The remaining issues are operational/reporting issues: artifact size, duplicate logical IDs in aggregation semantics, and lack of built-in morph-run parallelism.
