# Morph compact canonical v2 triage

Date: 2026-04-30

## Goal

Refresh the full compact corpus artifact so one canonical output set includes all current triage fields:

- LF-normalized plaintext
- exact whitespace/lexical segmentation counters
- exact whitespace/lexical feature-diff counters
- source-level script category on comparison summaries
- span-level script category on examples

## Artifact

Canonical v2 compact artifact:

```text
scratch/morph-full-corpus-compact-canonical-v2/
```

Files:

```text
analyses.jsonl.zst
comparisons.jsonl.zst
examples.jsonl.zst
errors.jsonl.zst
manifest.json
```

Run completed in about 1,132 seconds over 17,894 AAT inputs. Peak observed progress-line memory was about 16.8 GB RSS/PSS. Error rows: 0.

## Run command

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/morph-full-corpus/aats \
    --analyzer vibrato \
    --analyzer sudachi-c \
    --output-profile compact \
    --analyses-output scratch/morph-full-corpus-compact-canonical-v2/analyses.jsonl.zst \
    --comparisons-output scratch/morph-full-corpus-compact-canonical-v2/comparisons.jsonl.zst \
    --examples-output scratch/morph-full-corpus-compact-canonical-v2/examples.jsonl.zst \
    --errors-output scratch/morph-full-corpus-compact-canonical-v2/errors.jsonl.zst \
    --manifest-output scratch/morph-full-corpus-compact-canonical-v2/manifest.json \
    --jobs 8 \
    --progress-interval-seconds 30
```

## Triage tables

Generated under:

```text
scratch/morph-canonical-v2-triage/
```

Commands:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical-v2/comparisons.jsonl.zst \
  --group-by source-id \
  --script-category japanese \
  --sort-by lexical-segmentation-regions \
  --limit 25 \
  > scratch/morph-canonical-v2-triage/japanese-lexical-segmentation.tsv

target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical-v2/comparisons.jsonl.zst \
  --group-by source-id \
  --script-category mixed \
  --sort-by lexical-segmentation-regions \
  --limit 25 \
  > scratch/morph-canonical-v2-triage/mixed-lexical-segmentation.tsv

target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical-v2/comparisons.jsonl.zst \
  --group-by source-id \
  --sort-by whitespace-segmentation-regions \
  --limit 25 \
  > scratch/morph-canonical-v2-triage/whitespace-segmentation.tsv

target/release/ab-morph-run summarize-examples \
  --examples scratch/morph-full-corpus-compact-canonical-v2/examples.jsonl.zst \
  --group-by source-id \
  --script-category latin-code \
  --filter lexical-only \
  --sort-by examples \
  --limit 25 \
  > scratch/morph-canonical-v2-triage/latin-code-examples.tsv
```

## Japanese lexical ranking

`JISTABLE` is no longer in the Japanese lexical ranking. Top rows:

| source_id | boundary F1 | total seg | whitespace seg | lexical seg | lexical feature diffs |
| --- | ---: | ---: | ---: | ---: | ---: |
| `001529_50685-dd3b2fe4e5bf` | 0.9592061346585344 | 30,098 | 270 | 29,828 | 396,277 |
| `000129_2084-1731cdb61106` | 0.9631461038381292 | 23,292 | 2,821 | 20,471 | 326,083 |
| `001111_42789-1e2257edc5d5` | 0.9708015899304404 | 17,439 | 1,370 | 16,069 | 330,248 |
| `000055_365-ae629349b4c7` | 0.9718352878020788 | 16,744 | 4,416 | 12,328 | 297,251 |
| `001099_46996-10fe9133d385` | 0.970551013863204 | 12,048 | 44 | 12,004 | 217,780 |
| `000311_2012-1bae69ee8181` | 0.9839042682781188 | 15,620 | 4,643 | 10,977 | 493,566 |
| `000008_47386-49ed3c33666b` | 0.9650342386604726 | 11,239 | 1,212 | 10,027 | 173,247 |

## Mixed-source side channel

The mixed ranking catches sources with substantial non-prose or technical material. `JISTABLE` appears here:

| source_id | boundary F1 | total seg | whitespace seg | lexical seg | category |
| --- | ---: | ---: | ---: | ---: | --- |
| `JISTABLE-a3b6bf10cde1` | 0.9647802832790404 | 12,388 | 2 | 12,386 | mixed |
| `000140_50131-14b5df15049b` | 0.8510015954617977 | 5,164 | 242 | 4,922 | mixed |
| `001475_51028-bf9c6433d8a5` | 0.8554641598119859 | 4,214 | 16 | 4,198 | mixed |
| `001065_18361-7d97e68d3f31` | 0.9051092261422372 | 2,789 | 201 | 2,588 | mixed |
| `001529_44909-233abede3f7e` | 0.9009078123002174 | 4,404 | 2,184 | 2,220 | mixed |

This is the right place for `JISTABLE`: it should remain visible but should not drive Japanese literary triage.

## Whitespace side channel

Whitespace-heavy segmentation remains measurable and separate. Top rows by exact whitespace segmentation are still Japanese sources with many newline/indentation/full-width-space segmentation differences:

| source_id | boundary F1 | total seg | whitespace seg | lexical seg |
| --- | ---: | ---: | ---: | ---: |
| `001562_57875-10bbfac54ee2` | 0.9791521365283272 | 14,190 | 7,013 | 7,177 |
| `000216_45567-ca9949266c2b` | 0.9817477670871604 | 12,110 | 6,146 | 5,964 |
| `001562_56145-c9fe64a731a3` | 0.9788587331837568 | 12,994 | 5,862 | 7,132 |
| `001562_56146-66c41ed10b8f` | 0.9823634088814872 | 14,639 | 5,628 | 9,011 |
| `000290_24376-fa5aba7f1712` | 0.9768922460647906 | 9,367 | 4,767 | 4,600 |

## Latin/code examples side channel

Latin/code examples are isolated at span level, regardless of source-level category. Top rows saturate the bounded example budget:

| source_id | examples | category | segmentation examples |
| --- | ---: | --- | ---: |
| `000025_1144-da87bc69753d` | 10 | latin-code | 10 |
| `000025_1144-dc8e42a7bbde` | 10 | latin-code | 10 |
| `000025_kantou-20379f2add12` | 10 | latin-code | 10 |
| `000159_857-469c33b33d4a` | 10 | latin-code | 10 |
| `001475_51028-bf9c6433d8a5` | 10 | latin-code | 10 |

## Recommended default triage workflow

Use the canonical v2 artifact as the default compact corpus output.

1. Main Japanese literary triage:

```bash
summarize-compact --script-category japanese --sort-by lexical-segmentation-regions
```

2. Whitespace noise side channel:

```bash
summarize-compact --sort-by whitespace-segmentation-regions
```

3. Mixed technical/table/code side channel:

```bash
summarize-compact --script-category mixed --sort-by lexical-segmentation-regions
```

4. Concrete Latin/code evidence:

```bash
summarize-examples --script-category latin-code --filter lexical-only --sort-by examples
```

## Next engineering item

The remaining operational issue is peak memory. This run reached about 16.8 GB RSS/PSS. Size-aware job scheduling should be the next implementation target if we want lower peak memory without reducing throughput.
