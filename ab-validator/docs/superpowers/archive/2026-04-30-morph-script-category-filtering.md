# Script/category filtering for morph triage

Date: 2026-04-30

## Goal

Keep non-prose technical rows such as `JISTABLE` from contaminating Japanese lexical morph triage, while still making those rows inspectable when needed.

## Implementation

Added a lightweight script/category classifier in `ab-morph-run`.

Categories:

| category | meaning |
| --- | --- |
| `whitespace` | non-empty only after whitespace classification, no non-whitespace chars |
| `japanese` | Japanese script dominates the text/span |
| `latin-code` | Latin letters or ASCII/code punctuation dominate, with no Japanese script |
| `numeric` | numeric-only non-whitespace text/span |
| `mixed` | mixed Japanese and Latin/code/numeric content |
| `other` | fallback |

New fields:

| row | field |
| --- | --- |
| compact comparison summary | `source_script_category` |
| compact example row | `script_category` |

New filters:

```bash
ab-morph-run summarize-compact --script-category japanese
ab-morph-run summarize-examples --script-category latin-code
```

The comparison filter works at source-text level. The example filter works at source-excerpt level.

## Smoke command

A two-source smoke used one Japanese source and `JISTABLE`:

```bash
scratch/morph-script-category-smoke/aats/001529_50685-dd3b2fe4e5bf.json
scratch/morph-script-category-smoke/aats/JISTABLE-a3b6bf10cde1.json
```

The compact run produced zero error rows.

## Smoke results

All comparison rows:

| source_id | source category | lexical segmentation |
| --- | --- | ---: |
| `001529_50685-dd3b2fe4e5bf` | `japanese` | 29,828 |
| `JISTABLE-a3b6bf10cde1` | `mixed` | 12,386 |

Japanese-only comparison filter:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-script-category-smoke/out/comparisons.jsonl.zst \
  --group-by source-id \
  --script-category japanese \
  --sort-by lexical-segmentation-regions
```

Output contains only `001529_50685-dd3b2fe4e5bf`.

Latin-code example filter:

```bash
target/release/ab-morph-run summarize-examples \
  --examples scratch/morph-script-category-smoke/out/examples.jsonl.zst \
  --group-by source-id \
  --script-category latin-code \
  --filter lexical-only
```

Output contains `JISTABLE-a3b6bf10cde1` with 7 Latin/code lexical segmentation examples.

## Interpretation

`JISTABLE` is `mixed` at source level because the full source contains both Japanese and technical/code/table material. Its concrete examples can still classify as `latin-code`, which is the right granularity for inspecting why it appears in lexical rankings.

Practical triage guidance:

1. Use `summarize-compact --script-category japanese --sort-by lexical-segmentation-regions` for Japanese literary lexical rankings.
2. Use `summarize-compact --script-category mixed` to inspect mixed prose/table/code sources separately.
3. Use `summarize-examples --script-category latin-code` to isolate Latin/code evidence inside mixed sources.
