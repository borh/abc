# Morph lexical triage from whitespace-aware counters

Date: 2026-04-30

## Goal

Treat whitespace differences as noise for normal triage, then identify high-signal lexical segmentation differences and inspect representative examples from the top lexical rows.

## Inputs

Exact whitespace/lexical compact artifact:

```text
scratch/morph-full-corpus-compact-whitespace-counters/
```

Primary files:

```text
comparisons.jsonl.zst
examples.jsonl.zst
errors.jsonl.zst
```

The exact-counter artifact has zero error rows.

## Summary commands

Top lexical segmentation rows:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-whitespace-counters/comparisons.jsonl.zst \
  --group-by source-id \
  --sort-by lexical-segmentation-regions \
  --limit 25 \
  > scratch/morph-lexical-triage/top-lexical-segmentation.tsv
```

Top whitespace segmentation rows:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-whitespace-counters/comparisons.jsonl.zst \
  --group-by source-id \
  --sort-by whitespace-segmentation-regions \
  --limit 25 \
  > scratch/morph-lexical-triage/top-whitespace-segmentation.tsv
```

Derived ratio reports:

```text
scratch/morph-lexical-triage/whitespace-dominant.tsv
scratch/morph-lexical-triage/lexical-dominant-large.tsv
scratch/morph-lexical-triage/high-total-mostly-whitespace.tsv
```

## Top lexical segmentation rows

| source_id | boundary F1 | total seg | whitespace seg | lexical seg | lexical share |
| --- | ---: | ---: | ---: | ---: | ---: |
| `001529_50685-dd3b2fe4e5bf` | 0.9592061346585344 | 30,098 | 270 | 29,828 | 99.1% |
| `000129_2084-1731cdb61106` | 0.9631461038381292 | 23,292 | 2,821 | 20,471 | 87.9% |
| `001111_42789-1e2257edc5d5` | 0.9708015899304404 | 17,439 | 1,370 | 16,069 | 92.1% |
| `JISTABLE-a3b6bf10cde1` | 0.9647802832790404 | 12,388 | 2 | 12,386 | ~100% |
| `000055_365-ae629349b4c7` | 0.9718352878020788 | 16,744 | 4,416 | 12,328 | 73.6% |
| `001099_46996-10fe9133d385` | 0.970551013863204 | 12,048 | 44 | 12,004 | 99.6% |
| `000311_2012-1bae69ee8181` | 0.9839042682781188 | 15,620 | 4,643 | 10,977 | 70.3% |

## Top whitespace segmentation rows

Whitespace-heavy rows are now explicitly separable and should usually be treated as lower priority for lexical analysis:

| source_id | boundary F1 | total seg | whitespace seg | lexical seg | whitespace share |
| --- | ---: | ---: | ---: | ---: | ---: |
| `001562_57875-10bbfac54ee2` | 0.9791521365283272 | 14,190 | 7,013 | 7,177 | 49.4% |
| `000216_45567-ca9949266c2b` | 0.9817477670871604 | 12,110 | 6,146 | 5,964 | 50.8% |
| `001562_56145-c9fe64a731a3` | 0.9788587331837568 | 12,994 | 5,862 | 7,132 | 45.1% |
| `001562_56146-66c41ed10b8f` | 0.9823634088814872 | 14,639 | 5,628 | 9,011 | 38.4% |
| `000290_24376-fa5aba7f1712` | 0.9768922460647906 | 9,367 | 4,767 | 4,600 | 50.9% |

Rows with high whitespace share and at least 1,000 segmentation regions include `000072_54444-04ef514c5b63` at 83.4% whitespace and `000160_43533-436077a86dac` at 76.0% whitespace. These are likely newline/indentation/presentation-policy cases, not good lexical triage targets.

## Targeted examples-only rerun

Selected top lexical rows:

```text
001529_50685-dd3b2fe4e5bf
000129_2084-1731cdb61106
001111_42789-1e2257edc5d5
JISTABLE-a3b6bf10cde1
000055_365-ae629349b4c7
```

Command:

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  target/release/ab-morph-run rerun-full \
    --aat-dir scratch/morph-full-corpus/aats \
    --source-id 001529_50685-dd3b2fe4e5bf \
    --source-id 000129_2084-1731cdb61106 \
    --source-id 001111_42789-1e2257edc5d5 \
    --source-id JISTABLE-a3b6bf10cde1 \
    --source-id 000055_365-ae629349b4c7 \
    --analyzer vibrato \
    --analyzer sudachi-c \
    --output-dir scratch/morph-lexical-targeted-examples \
    --jobs 2 \
    --detail examples-only \
    --max-examples-per-comparison 100 \
    --examples-output scratch/morph-lexical-targeted-examples/examples.jsonl
```

The rerun completed with zero error rows.

## Targeted example counts

| source_id | examples | lexical | whitespace | split | merge | resegment | feature_diff |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `000055_365-ae629349b4c7` | 100 | 66 | 34 | 2 | 97 | 1 | 0 |
| `000129_2084-1731cdb61106` | 100 | 87 | 13 | 25 | 64 | 11 | 0 |
| `001111_42789-1e2257edc5d5` | 100 | 53 | 47 | 16 | 78 | 6 | 0 |
| `001529_50685-dd3b2fe4e5bf` | 100 | 87 | 13 | 12 | 76 | 12 | 0 |
| `JISTABLE-a3b6bf10cde1` | 100 | 98 | 2 | 4 | 96 | 0 | 0 |

Because compact examples prioritize structural region examples before feature-diff examples, these high-segmentation rows filled the entire budget with segmentation evidence. That is appropriate for this triage step.

## Representative lexical examples

### `001529_50685-dd3b2fe4e5bf`

The examples are mostly compounds, names, suffix-like forms, and classical/old orthography:

| kind | excerpt | Vibrato surfaces | Sudachi surfaces |
| --- | --- | --- | --- |
| merge | `はつ` | `は` + `つ` | `はつ` |
| merge | `徳川時代` | `徳川` + `時代` | `徳川時代` |
| merge | `探偵趣味` | `探偵` + `趣味` | `探偵趣味` |
| merge | `青砥藤綱` | `青砥` + `藤綱` | `青砥藤綱` |
| merge | `日本化` | `日本` + `化` | `日本化` |
| merge | `理想的` | `理想` + `的` | `理想的` |

### `000129_2084-1731cdb61106`

This row has many proper names, Sino-Japanese compounds, numbers, and classical forms:

| kind | excerpt | Vibrato surfaces | Sudachi surfaces |
| --- | --- | --- | --- |
| merge | `頼山陽` | `頼` + `山陽` | `頼山陽` |
| resegment | `国泰寺裏門前杉` | `国泰` + `寺裏` + `門前` + `杉` | `国泰寺` + `裏門` + `前杉` |
| split | `春水` | `春水` | `春` + `水` |
| merge | `至つ` | `至` + `つ` | `至つ` |
| split | `二十` | `二十` | `二` + `十` |
| merge | `森田思軒` | `森田` + `思軒` | `森田思軒` |

### `001111_42789-1e2257edc5d5`

This row is strongly influenced by historical kana/orthography and compounds:

| kind | excerpt | Vibrato surfaces | Sudachi surfaces |
| --- | --- | --- | --- |
| resegment | `植ゑつけた` | `植` + `ゑつ` + `けた` | `植ゑ` + `つけ` + `た` |
| split | `名篇` | `名篇` | `名` + `篇` |
| merge | `つて` | `つ` + `て` | `つて` |
| resegment | `云つた` | `云` + `つた` | `云つ` + `た` |
| merge | `妹達` | `妹` + `達` | `妹達` |
| resegment | `與へた` | `與` + `へた` | `與へ` + `た` |

### `000055_365-ae629349b4c7`

This row looks like modern technical/prose vocabulary and names:

| kind | excerpt | Vibrato surfaces | Sudachi surfaces |
| --- | --- | --- | --- |
| split | `本作り` | `本作り` | `本` + `作り` |
| merge | `パソコン創世記` | `パソコン` + `創世` + `記` | `パソコン創世記` |
| merge | `古川享` | `古川` + `享` | `古川享` |
| merge | `パーソナルコンピューター` | `パーソナル` + `コンピューター` | `パーソナルコンピューター` |
| merge | `日本電気` | `日本` + `電気` | `日本電気` |
| merge | `原子爆弾` | `原子` + `爆弾` | `原子爆弾` |

### `JISTABLE-a3b6bf10cde1`

This is mostly not Japanese prose. It is a technical character table or code-like document:

| kind | excerpt | Vibrato surfaces | Sudachi surfaces |
| --- | --- | --- | --- |
| merge | `//////////////////////////////////////////////////////////////////////` | many individual `/` tokens | one long slash token |
| merge | `JIS` | `J` + `I` + `S` | `JIS` |
| split | `0208` | `0208` | `0` + `2` + `0` + `8` |
| merge | `tama` | `t` + `a` + `m` + `a` | `tama` |
| merge | `jis` | `j` + `i` + `s` | `jis` |

This should probably be excluded from literary Japanese lexical triage or put in a separate Latin/code/table bucket.

## Interpretation

The top lexical segmentation rows are no longer whitespace artifacts. They mostly represent real analyzer policy differences:

1. Compound handling: Sudachi often keeps larger compounds together where Vibrato/UniDic splits into smaller morphemes.
2. Proper names: person/place/work names often differ by dictionary coverage and compound policy.
3. Classical orthography: forms like `云つた`, `與へた`, `至つ`, and `つて` are a major source of resegmentation/merge differences.
4. Numeric/technical/Latin text: `JISTABLE` shows that non-prose inputs can dominate lexical metrics but are not useful for Japanese literary analysis.

## Recommended next decisions

1. Make lexical counters the default triage surface. Use whitespace counters as a diagnostic side channel.
2. Add an optional script/report that excludes obvious non-prose technical rows such as `JISTABLE` from literary lexical rankings.
3. Consider a separate `script/category` classifier for examples: Japanese, Latin/code, numeric, mixed, whitespace. That would keep `JISTABLE`-style rows from contaminating Japanese lexical analysis.
4. If we want analyzer-quality metrics rather than analyzer-policy metrics, add configurable ignore rules for expected compound-policy differences. That is a later decision; for now, the current output is useful because it shows the policy differences directly.
