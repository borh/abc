# Whitespace isolation for compact morph examples

Date: 2026-04-30

## Goal

Make whitespace-related morph differences directly isolatable without changing raw comparison semantics. The compact comparison row remains the comprehensive summary; bounded example rows now classify whether the source excerpt is whitespace-only, and a new streaming summary command can filter those examples.

## Implementation

`ComparisonExampleRow` now includes:

```json
"whitespace_only": true | false
```

Classification rule:

```text
source_excerpt is non-empty and every char satisfies char::is_whitespace
```

The reader also supports older example artifacts that lack `whitespace_only`; for those rows it computes the value from `source_excerpt` while streaming.

New CLI:

```bash
target/release/ab-morph-run summarize-examples \
  --examples scratch/morph-full-corpus-compact-lf-canonical/examples.jsonl.zst \
  --group-by source-id \
  --filter whitespace-only \
  --sort-by whitespace-examples \
  --limit 20
```

Supported filters:

| filter | meaning |
| --- | --- |
| `all` | include all example rows |
| `whitespace-only` | include only examples whose source excerpt is whitespace-only |
| `lexical-only` | include only examples whose source excerpt is not whitespace-only |

Supported sort modes:

| sort | meaning |
| --- | --- |
| `examples` | total examples after filtering |
| `whitespace-examples` | whitespace-only example count |
| `lexical-examples` | non-whitespace example count |
| `segmentation-examples` | split/merge/resegment example count |
| `feature-diff-examples` | feature-diff example count |
| `coverage-examples` | coverage mismatch example count |

## Smoke commands

```bash
cargo build --release -p ab-morph-run

mkdir -p scratch/morph-whitespace-summary

target/release/ab-morph-run summarize-examples \
  --examples scratch/morph-full-corpus-compact-lf-canonical/examples.jsonl.zst \
  --group-by source-id \
  --filter whitespace-only \
  --sort-by whitespace-examples \
  --limit 20 \
  > scratch/morph-whitespace-summary/lf-whitespace-only-by-source.tsv

target/release/ab-morph-run summarize-examples \
  --examples scratch/morph-full-corpus-compact-lf-canonical/examples.jsonl.zst \
  --group-by source-id \
  --filter lexical-only \
  --sort-by lexical-examples \
  --limit 20 \
  > scratch/morph-whitespace-summary/lf-lexical-only-by-source.tsv

target/release/ab-morph-run summarize-examples \
  --examples scratch/morph-full-corpus-compact-lf-canonical/examples.jsonl.zst \
  --group-by text-id \
  --filter whitespace-only \
  --sort-by whitespace-examples \
  --limit 20 \
  > scratch/morph-whitespace-summary/lf-whitespace-only-by-text.tsv
```

## Observed output

Top whitespace-only source rows all saturated the per-comparison example cap:

```text
key                                   examples  whitespace_examples  lexical_examples  segmentation_examples
000081_1942-21ce24163776              10        10                   0                 10
000081_1942-e5eba08b7872              10        10                   0                 10
000081_43801-2df243e32de7             10        10                   0                 10
000081_4600-20a4071e8b8a              10        10                   0                 10
000094_60550-a5bd56fab443             10        10                   0                 10
```

Top lexical-only source rows also saturated the cap, but now exclude whitespace-only spans:

```text
key                                   examples  whitespace_examples  lexical_examples  segmentation_examples
000008_47374-9c929d4f4d67             10        0                    10                10
000013_542-48ba8ab46e27               10        0                    10                10
000013_542-9dc5cea740ac               10        0                    10                10
000013_542-f2b43aeff7df               10        0                    10                10
000014_46407-6a383e983d17             10        0                    10                10
```

Grouping by logical text ID surfaces duplicate-index amplification explicitly:

```text
key         source_ids  examples  whitespace_examples  lexical_examples
000081_469  3 records   23        23                   0
000111_568  3 records   23        23                   0
000038_323  3 records   21        21                   0
000111_565  3 records   21        21                   0
000081_1942 2 records   20        20                   0
```

## Interpretation

Whitespace-related differences are now separable at the bounded-example layer. This does not produce exhaustive whitespace-diff counts because compact examples are intentionally capped per comparison. It is still useful for triage:

1. Use `summarize-compact` to find high-level bad rows by full stats.
2. Use `summarize-examples --filter whitespace-only` to find rows whose bounded evidence is dominated by whitespace.
3. Use `summarize-examples --filter lexical-only` to inspect linguistically useful examples without newline/indentation examples crowding the top of the evidence budget.

## Remaining limitation

Because compact examples are capped at `--max-examples-per-comparison`, a row with `10` whitespace examples means “the bounded evidence saturated with whitespace,” not “there are exactly 10 whitespace diffs.” Exhaustive whitespace-adjusted metrics require adding counters to the streaming comparison accumulator, not just summarizing examples.
