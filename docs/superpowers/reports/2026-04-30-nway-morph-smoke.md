# N-way Morph Diff Smoke Report

Date: 2026-04-30

## Scope

Smoke input used the first three sorted checked AAT files from `scratch/morph-full-corpus/aats`, staged under `scratch/morph-nway-smoke/aats`.

Analyzers:

- `vibrato`
- `sudachi-a`
- `sudachi-c`

Dictionary:

- `AB_SUDACHI_DICT=$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic`

## Commands

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  cargo run -q -p ab-morph-run -- analyze-aat \
    --aat-dir scratch/morph-nway-smoke/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --output-profile compact \
    --analyses-output scratch/morph-nway-smoke/analyses.jsonl.zst \
    --comparisons-output scratch/morph-nway-smoke/comparisons.jsonl.zst \
    --examples-output scratch/morph-nway-smoke/examples.jsonl.zst \
    --nway-output scratch/morph-nway-smoke/nway.jsonl.zst \
    --errors-output scratch/morph-nway-smoke/errors.jsonl.zst \
    --manifest-output scratch/morph-nway-smoke/manifest.json \
    --jobs 2 \
    --progress-interval-seconds 5
```

```bash
cargo run -q -p ab-morph-run -- summarize-nway \
  --nway scratch/morph-nway-smoke/nway.jsonl.zst \
  --sort-by regions-with-segmentation-disagreement \
  --limit 5
```

```bash
cargo run -q -p ab-morph-run -- summarize-nway-patterns \
  --nway scratch/morph-nway-smoke/nway.jsonl.zst \
  --kind segmentation \
  --limit 5
```

```bash
cargo run -q -p ab-morph-run -- summarize-nway-patterns \
  --nway scratch/morph-nway-smoke/nway.jsonl.zst \
  --kind feature \
  --feature-key pos1 \
  --limit 5
```

## Run result

- Runtime: about 462 seconds for three staged AAT files.
- Memory: about 3.28 GB RSS/PSS at the reported peak.
- Error rows: 0.
- N-way rows: 3.

One staged source was large, so this was a heavier smoke than a tiny fixture run.

## Top N-way sources

| source_id | regions | agreement | feature disagreement | segmentation disagreement | coverage mismatch | variable boundaries |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `000005_53194-ebb0cbaf64b3` | 47071 | 6 | 46788 | 1484 | 0 | 1588 |
| `000005_5-7fd23d54bdb6` | 5740 | 0 | 5714 | 242 | 0 | 330 |
| `000005_55215-f66e2e504f07` | 100 | 0 | 99 | 5 | 0 | 6 |

## Top recurring segmentation patterns

| examples | pattern |
| ---: | --- |
| 1 | `sudachi-a+sudachi-c:[惑|ふ] ; vibrato:unidic-cwj-202512:[惑ふ]` |
| 1 | `sudachi-a+sudachi-c:[為|る] ; vibrato:unidic-cwj-202512:[為る]` |
| 1 | `sudachi-c:[\n|\n] ; sudachi-a+vibrato:unidic-cwj-202512:[\n\n]` |
| 1 | `sudachi-c:[\n|\n] ; sudachi-a+vibrato:unidic-cwj-202512:[\n\n]` |
| 1 | `sudachi-c:[\n|\n] ; sudachi-a+vibrato:unidic-cwj-202512:[\n\n]` |

The duplicate newline patterns are distinct source regions with the same rendered partition. The pattern summary counts matching regions, not unique source IDs.

## Top recurring `pos1` feature patterns

| examples | pattern |
| ---: | --- |
| 11 | `pos1 whole_region 名詞=>sudachi-a+vibrato:unidic-cwj-202512 ; 空白=>sudachi-c` |
| 4 | `pos1 whole_region 助詞=>sudachi-a+vibrato:unidic-cwj-202512 ; 空白=>sudachi-c` |
| 3 | `pos1 whole_region 動詞=>sudachi-a+vibrato:unidic-cwj-202512 ; 空白=>sudachi-c` |
| 2 | `pos1 whole_region 接尾辞=>sudachi-a+vibrato:unidic-cwj-202512 ; 空白=>sudachi-c` |
| 1 | `pos1 whole_region 助動詞=>sudachi-a+vibrato:unidic-cwj-202512 ; 空白=>sudachi-c` |

## Notes

- N-way output is compact-only in this implementation.
- Agreement-only feature groups are omitted from example regions.
- Pattern display escapes control characters so TSV output remains line-oriented.
- Pairwise compact comparisons are still emitted alongside N-way output for boundary F1 and pair-specific summaries.
