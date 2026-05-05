# Exact whitespace/lexical morph counters

Date: 2026-04-30

## Goal

Move whitespace isolation beyond bounded examples. Compact comparison summaries now carry exact whitespace-vs-lexical counters for segmentation regions and feature-diff regions, while compact examples remain useful as bounded evidence.

## Generated artifacts

Self-describing example artifact generated before exact counters:

```text
scratch/morph-full-corpus-compact-whitespace/
```

Final exact-counter artifact generated after the model change:

```text
scratch/morph-full-corpus-compact-whitespace-counters/
```

The final run covered 17,894 AAT inputs and completed in about 1,141 seconds. It produced zero error rows.

Peak observed progress-line memory was about 16.4 GB RSS/PSS, again consistent with concurrent large-document processing rather than monotonic retention.

## New comparison summary fields

`ComparisonStats` and compact comparison summary rows now include:

| field | meaning |
| --- | --- |
| `whitespace_segmentation_regions` | segmentation regions whose source span is non-empty and all whitespace |
| `lexical_segmentation_regions` | segmentation regions whose source span is not whitespace-only |
| `whitespace_feature_diff_regions` | one-to-one feature-diff regions whose source span is whitespace-only |
| `lexical_feature_diff_regions` | one-to-one feature-diff regions whose source span is not whitespace-only |

The invariant holds on the final artifact:

```text
whitespace_segmentation_regions + lexical_segmentation_regions == segmentation_regions
whitespace_feature_diff_regions + lexical_feature_diff_regions == one_to_one_with_feature_differences
```

## Corpus totals

| metric | value |
| --- | ---: |
| comparison rows | 17,894 |
| segmentation regions | 5,858,351 |
| whitespace segmentation regions | 1,498,417 |
| lexical segmentation regions | 4,359,934 |
| feature-diff regions | 157,965,418 |
| whitespace feature-diff regions | 2,426,857 |
| lexical feature-diff regions | 155,538,561 |
| coverage mismatch regions | 0 |

Interpretation: after LF canonicalization, about 25.6% of segmentation regions are still whitespace-only. Feature diffs are overwhelmingly lexical/non-whitespace by count; whitespace feature diffs are about 1.5% of feature-diff regions.

## Commands

Exact lexical segmentation ranking:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-whitespace-counters/comparisons.jsonl.zst \
  --group-by source-id \
  --sort-by lexical-segmentation-regions \
  --limit 20
```

Exact whitespace segmentation ranking:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-whitespace-counters/comparisons.jsonl.zst \
  --group-by source-id \
  --sort-by whitespace-segmentation-regions \
  --limit 20
```

Bounded whitespace-only evidence:

```bash
target/release/ab-morph-run summarize-examples \
  --examples scratch/morph-full-corpus-compact-whitespace-counters/examples.jsonl.zst \
  --group-by source-id \
  --filter whitespace-only \
  --sort-by whitespace-examples \
  --limit 20
```

Bounded lexical-only evidence:

```bash
target/release/ab-morph-run summarize-examples \
  --examples scratch/morph-full-corpus-compact-whitespace-counters/examples.jsonl.zst \
  --group-by source-id \
  --filter lexical-only \
  --sort-by lexical-examples \
  --limit 20
```

## Top exact lexical segmentation rows

| source_id | total segmentation | whitespace segmentation | lexical segmentation | boundary F1 |
| --- | ---: | ---: | ---: | ---: |
| `001529_50685-dd3b2fe4e5bf` | 30,098 | 270 | 29,828 | 0.9592061346585344 |
| `000129_2084-1731cdb61106` | 23,292 | 2,821 | 20,471 | 0.9631461038381292 |
| `001111_42789-1e2257edc5d5` | 17,439 | 1,370 | 16,069 | 0.9708015899304404 |
| `JISTABLE-a3b6bf10cde1` | 12,388 | 2 | 12,386 | 0.9647802832790404 |
| `000055_365-ae629349b4c7` | 16,744 | 4,416 | 12,328 | 0.9718352878020788 |
| `001099_46996-10fe9133d385` | 12,048 | 44 | 12,004 | 0.970551013863204 |
| `000311_2012-1bae69ee8181` | 15,620 | 4,643 | 10,977 | 0.9839042682781188 |

## Top exact whitespace segmentation rows

| source_id | total segmentation | whitespace segmentation | lexical segmentation | boundary F1 |
| --- | ---: | ---: | ---: | ---: |
| `001562_57875-10bbfac54ee2` | 14,190 | 7,013 | 7,177 | 0.9791521365283272 |
| `000216_45567-ca9949266c2b` | 12,110 | 6,146 | 5,964 | 0.9817477670871604 |
| `001562_56145-c9fe64a731a3` | 12,994 | 5,862 | 7,132 | 0.9788587331837568 |
| `001562_56146-66c41ed10b8f` | 14,639 | 5,628 | 9,011 | 0.9823634088814872 |
| `000290_24376-fa5aba7f1712` | 9,367 | 4,767 | 4,600 | 0.9768922460647906 |
| `000311_2012-1bae69ee8181` | 15,620 | 4,643 | 10,977 | 0.9839042682781188 |

## Bounded examples remain useful but not exhaustive

The example rows now serialize `whitespace_only` directly. Example row:

```json
{"source_id":"000005_5-7fd23d54bdb6","kind":"merge","source_excerpt":"\n\n　","whitespace_only":true}
```

The top example summaries still saturate at 10 examples because the run used the default `--max-examples-per-comparison 10`. These are evidence budgets, not exact counts. Use compact summary counters for exact corpus ranking, and example summaries for concrete inspection.

## Conclusion

Whitespace-related diffs are now isolated at two levels:

1. Exact metrics in `summarize-compact`, suitable for corpus ranking and filtering decisions.
2. Bounded examples in `summarize-examples`, suitable for inspection and rerun selection.

This gives us a clean workflow: sort by `lexical-segmentation-regions` for linguistically meaningful segmentation disagreements, sort by `whitespace-segmentation-regions` for newline/indentation policy issues, and use example filters to inspect concrete spans.
