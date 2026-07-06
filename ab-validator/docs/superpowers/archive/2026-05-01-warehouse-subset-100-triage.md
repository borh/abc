# Warehouse 100-source morph triage

Date: 2026-05-01
Run ID: `subset-100-triage-2026-05-01`
Run dir: `scratch/morph-warehouse-subset-2026-05-01/warehouse/runs/subset-100-triage-2026-05-01`
Input subset: first 100 AAT JSON files from `scratch/morph-full-corpus/aats`, symlinked into `scratch/morph-warehouse-subset-2026-05-01/aats-100`
Analyzers: `vibrato`, `sudachi-a`, `sudachi-c`

## Run result

Command:

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  time -v target/release/ab-morph-run analyze-aat \
  --aat-dir scratch/morph-warehouse-subset-2026-05-01/aats-100 \
  --analyzer vibrato \
  --analyzer sudachi-a \
  --analyzer sudachi-c \
  --warehouse-dir scratch/morph-warehouse-subset-2026-05-01/warehouse \
  --run-id subset-100-triage-2026-05-01 \
  --progress \
  --progress-interval-seconds 30
```

Metrics from `time -v`:

| metric | value |
|---|---:|
| wall time | 5:31.29 |
| user time | 315.26s |
| system time | 13.98s |
| CPU | 99% |
| max RSS | 14,385,704 KB |
| exit status | 0 |

Artifact size:

| artifact | size |
|---|---:|
| warehouse run dir | 226M |

Errors summary:

```text
key errors source_count text_count analyzers stages error_codes sample_source_ids sample_messages
```

No error rows were reported.

## Generated summaries

Files:

- `scratch/morph-warehouse-subset-2026-05-01/subset-100-top-segmentation.tsv`
- `scratch/morph-warehouse-subset-2026-05-01/subset-100-top-pos1.tsv`
- `scratch/morph-warehouse-subset-2026-05-01/subset-100-examples-segmentation-top.tsv`
- `scratch/morph-warehouse-subset-2026-05-01/subset-100-examples-pos1-top.tsv`
- `scratch/morph-warehouse-subset-2026-05-01/subset-100-examples-pos1-de.tsv`

## Top lexical segmentation patterns

Top rows from `summarize-warehouse-patterns --kind segmentation --filter lexical-only`:

| examples | source_count | pattern |
|---:|---:|---|
| 1903 | 27 | `sudachi-a+sudachi-c:[あつ|た] ; vibrato:unidic-cwj-202512:[あつた]` |
| 885 | 28 | `vibrato:unidic-cwj-202512:[つ|て] ; sudachi-a+sudachi-c:[つて]` |
| 718 | 27 | `vibrato:unidic-cwj-202512:[な|つ] ; sudachi-a+sudachi-c:[なつ]` |
| 689 | 11 | `vibrato:unidic-cwj-202512:[云|つ] ; sudachi-a+sudachi-c:[云つ]` |
| 636 | 26 | `vibrato:unidic-cwj-202512:[行|つ] ; sudachi-a+sudachi-c:[行つ]` |
| 417 | 15 | `vibrato:unidic-cwj-202512:[出|來] ; sudachi-a+sudachi-c:[出來]` |
| 407 | 15 | `vibrato:unidic-cwj-202512:[來|る] ; sudachi-a+sudachi-c:[來る]` |
| 372 | 12 | `sudachi-a+sudachi-c:[來|た] ; vibrato:unidic-cwj-202512:[來た]` |
| 365 | 20 | `vibrato:unidic-cwj-202512:[な|かつ] ; sudachi-a+sudachi-c:[なかつ]` |
| 332 | 72 | `vibrato:unidic-cwj-202512:[いつ|も] ; sudachi-a+sudachi-c:[いつも]` |

Interpretation: the largest segmentation families are old orthography / historical kana and multi-character lexicalization differences. This is qualitatively useful and no longer dominated by whitespace noise.

Representative top segmentation examples:

```text
source_id text_id region_index char_span analyzers
000006_46659-a9a3514319de 000006_46659 193 299..302 sudachi-a:[あつ|た] ; sudachi-c:[あつ|た] ; vibrato:unidic-cwj-202512:[あつた]
000006_46659-a9a3514319de 000006_46659 490 749..752 sudachi-a:[あつ|た] ; sudachi-c:[あつ|た] ; vibrato:unidic-cwj-202512:[あつた]
000006_47064-7e668405c72c 000006_47064 700 987..990 sudachi-a:[あつ|た] ; sudachi-c:[あつ|た] ; vibrato:unidic-cwj-202512:[あつた]
```

## Top lexical `pos1` feature patterns

Top rows from `summarize-warehouse-patterns --kind feature --feature-key pos1 --filter lexical-only --exclude-feature-value 空白`:

| examples | source_count | pattern |
|---:|---:|---|
| 3428 | 94 | `pos1 whole_region 動詞=>vibrato:unidic-cwj-202512 ; 名詞=>sudachi-a+sudachi-c` |
| 2555 | 92 | `pos1 whole_region 助動詞=>vibrato:unidic-cwj-202512 ; 助詞=>sudachi-a+sudachi-c` |
| 2335 | 94 | `pos1 whole_region 助動詞=>sudachi-a+sudachi-c ; 助詞=>vibrato:unidic-cwj-202512` |
| 1274 | 88 | `pos1 surface:で 助動詞=>sudachi-a+sudachi-c ; 助詞=>vibrato:unidic-cwj-202512` |
| 1184 | 91 | `pos1 whole_region 動詞=>sudachi-a+sudachi-c ; 名詞=>vibrato:unidic-cwj-202512` |
| 1128 | 88 | `pos1 whole_region 名詞=>sudachi-a+sudachi-c ; 接尾辞=>vibrato:unidic-cwj-202512` |
| 722 | 80 | `pos1 surface:で 助動詞=>vibrato:unidic-cwj-202512 ; 助詞=>sudachi-a+sudachi-c` |
| 716 | 79 | `pos1 whole_region 名詞=>vibrato:unidic-cwj-202512 ; 接尾辞=>sudachi-a+sudachi-c` |
| 707 | 51 | `pos1 whole_region 名詞=>sudachi-a+sudachi-c ; 記号=>vibrato:unidic-cwj-202512` |
| 662 | 13 | `pos1 surface:來 動詞=>vibrato:unidic-cwj-202512 ; 名詞=>sudachi-a+sudachi-c` |

Representative top `pos1` whole-region examples:

```text
source_id text_id region_index char_span analyzers features
000005_5-7fd23d54bdb6 000005_5 206 332..333 sudachi-a:[聞] ; sudachi-c:[聞] ; vibrato:unidic-cwj-202512:[聞] vibrato:pos1=動詞 ; sudachi-a:pos1=名詞 ; sudachi-c:pos1=名詞
000005_5-7fd23d54bdb6 000005_5 625 1022..1024 sudachi-a:[響き] ; sudachi-c:[響き] ; vibrato:unidic-cwj-202512:[響き] vibrato:pos1=動詞 ; sudachi-a:pos1=名詞 ; sudachi-c:pos1=名詞
000005_5-7fd23d54bdb6 000005_5 1261 2018..2019 sudachi-a:[欺] ; sudachi-c:[欺] ; vibrato:unidic-cwj-202512:[欺] vibrato:pos1=動詞 ; sudachi-a:pos1=名詞 ; sudachi-c:pos1=名詞
```

Representative `で` surface-scoped examples:

```text
source_id text_id region_index char_span analyzers features
000005_5-7fd23d54bdb6 000005_5 239 387..388 sudachi-a:[で] ; sudachi-c:[で] ; vibrato:unidic-cwj-202512:[で] sudachi-a:pos1=助動詞 ; sudachi-c:pos1=助動詞 ; vibrato:pos1=助詞
000005_5-7fd23d54bdb6 000005_5 1182 1891..1892 sudachi-a:[で] ; sudachi-c:[で] ; vibrato:unidic-cwj-202512:[で] sudachi-a:pos1=助動詞 ; sudachi-c:pos1=助動詞 ; vibrato:pos1=助詞
000005_53194-ebb0cbaf64b3 000005_53194 146 258..259 sudachi-a:[で] ; sudachi-c:[で] ; vibrato:unidic-cwj-202512:[で] sudachi-a:pos1=助動詞 ; sudachi-c:pos1=助動詞 ; vibrato:pos1=助詞
```

## Notes

- The current warehouse query path is useful enough for corpus triage: summary rows identify high-frequency recurring patterns, and pattern examples recover concrete source IDs, spans, analyzer surfaces, and feature values.
- The 100-source run used serial warehouse mode and still reached 14.4 GB max RSS. Warehouse streaming avoids retaining output rows, so the remaining memory is dominated by analyzer dictionaries and per-source analyzer/runtime state.
- The release binary had to be rebuilt after adding `summarize-warehouse-pattern-examples`; use `cargo build -p ab-morph-run --release` before timing release workflows after CLI changes.
