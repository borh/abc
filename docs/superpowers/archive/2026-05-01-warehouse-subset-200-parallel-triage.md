# Warehouse 200-source parallel morph triage

Date: 2026-05-01
Run ID: `subset-200-triage-jobs4-2026-05-01`
Run dir: `scratch/morph-warehouse-subset-2026-05-01/warehouse/runs/subset-200-triage-jobs4-2026-05-01`
Input subset: first 200 AAT JSON files from `scratch/morph-full-corpus/aats`, symlinked into `scratch/morph-warehouse-subset-2026-05-01/aats-200`
Analyzers: `vibrato`, `sudachi-a`, `sudachi-c`
Jobs: `4`

## Run result

Command:

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  time -v target/release/ab-morph-run analyze-aat \
  --aat-dir scratch/morph-warehouse-subset-2026-05-01/aats-200 \
  --analyzer vibrato \
  --analyzer sudachi-a \
  --analyzer sudachi-c \
  --warehouse-dir scratch/morph-warehouse-subset-2026-05-01/warehouse \
  --run-id subset-200-triage-jobs4-2026-05-01 \
  --jobs 4 \
  --progress \
  --progress-interval-seconds 30
```

Metrics from `time -v`:

| metric | value |
|---|---:|
| wall time | 5:27.15 |
| user time | 850.32s |
| system time | 39.29s |
| CPU | 271% |
| max RSS | 8,976,868 KB |
| exit status | 0 |

Serial 200-source baseline before warehouse `--jobs`:

| metric | serial | jobs=4 |
|---|---:|---:|
| wall time | 13:05.12 | 5:27.15 |
| max RSS | 4,478,060 KB | 8,976,868 KB |
| CPU | 99% | 271% |

Interpretation: `--jobs 4` gives roughly 2.4x wall-time improvement on this subset. RSS roughly doubles because multiple workers hold analyzer and per-source state concurrently.

Artifact size:

| artifact | size |
|---|---:|
| warehouse run dir | 598M |

Errors summary:

```text
key errors source_count text_count analyzers stages error_codes sample_source_ids sample_messages
```

No error rows were reported.

## Top lexical segmentation patterns

Top rows from `summarize-warehouse-patterns --kind segmentation --filter lexical-only`:

| examples | source_count | pattern |
|---:|---:|---|
| 2247 | 42 | `sudachi-a+sudachi-c:[あつ|た] ; vibrato:unidic-cwj-202512:[あつた]` |
| 955 | 40 | `vibrato:unidic-cwj-202512:[つ|て] ; sudachi-a+sudachi-c:[つて]` |
| 870 | 44 | `vibrato:unidic-cwj-202512:[な|つ] ; sudachi-a+sudachi-c:[なつ]` |
| 737 | 36 | `vibrato:unidic-cwj-202512:[行|つ] ; sudachi-a+sudachi-c:[行つ]` |
| 735 | 130 | `vibrato:unidic-cwj-202512:[いつ|も] ; sudachi-a+sudachi-c:[いつも]` |

## Top lexical `pos1` feature patterns

Top rows from `summarize-warehouse-patterns --kind feature --feature-key pos1 --filter lexical-only --exclude-feature-value 空白`:

| examples | source_count | pattern |
|---:|---:|---|
| 6327 | 176 | `pos1 whole_region 動詞=>vibrato:unidic-cwj-202512 ; 名詞=>sudachi-a+sudachi-c` |
| 4402 | 177 | `pos1 whole_region 助動詞=>vibrato:unidic-cwj-202512 ; 助詞=>sudachi-a+sudachi-c` |
| 4181 | 190 | `pos1 whole_region 助動詞=>sudachi-a+sudachi-c ; 助詞=>vibrato:unidic-cwj-202512` |
| 2518 | 182 | `pos1 surface:で 助動詞=>sudachi-a+sudachi-c ; 助詞=>vibrato:unidic-cwj-202512` |
| 2263 | 155 | `pos1 whole_region 名詞=>sudachi-a+sudachi-c ; 接尾辞=>vibrato:unidic-cwj-202512` |

## Notes

- The merged warehouse output is queryable by the existing warehouse summary commands.
- The same lexical pattern families from the 100-source run remain dominant at 200 sources.
- The current `--jobs` implementation merges shard Parquet outputs into the same single-run layout, so downstream paths and DuckDB views do not change.
