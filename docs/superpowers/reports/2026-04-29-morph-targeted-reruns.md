# Targeted morph reruns from compact corpus summaries

Date: 2026-04-29

## Goal

Use compact whole-corpus comparison artifacts to identify suspicious or high-value source rows, rerun a small selected set in full mode, and determine whether the full rows reveal corpus/modeling issues or expected analyzer differences.

## Inputs

Compact corpus artifact:

```bash
scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst
```

AAT input directory used for full reruns:

```bash
scratch/morph-full-corpus/aats
```

Analyzers:

```bash
--analyzer vibrato --analyzer sudachi-c
```

Sudachi dictionary:

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic"
```

## Compact summary commands

```bash
mkdir -p scratch/morph-targeted-summary

target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst \
  --group-by source-id \
  --sort-by boundary-f1 \
  --limit 10 \
  > scratch/morph-targeted-summary/worst-boundary.tsv

target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst \
  --group-by source-id \
  --sort-by segmentation-regions \
  --limit 10 \
  > scratch/morph-targeted-summary/worst-segmentation.tsv

target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst \
  --group-by source-id \
  --sort-by feature-differences \
  --limit 10 \
  > scratch/morph-targeted-summary/worst-features.tsv
```

## Selected source IDs

The targeted rerun used two null-boundary rows, one low-boundary-F1 row, one worst-segmentation row, and one worst-feature-difference row:

| source_id | selection reason |
| --- | --- |
| `000293_48490-bc2210d727db` | boundary F1 is `null`, 1 feature-diff region |
| `000293_60442-9301b1e3a1e3` | boundary F1 is `null`, 1 feature-diff region |
| `000081_43733-4063e7c46297` | worst non-null boundary F1 in summary: `0.6884927066450566` |
| `001529_50685-dd3b2fe4e5bf` | highest segmentation-region count: `31082` |
| `000311_2012-1bae69ee8181` | highest feature-difference count: `494071` |

## Full rerun command

```bash
rm -rf scratch/morph-targeted-full
mkdir -p scratch/morph-targeted-full

for id in \
  000293_48490-bc2210d727db \
  000293_60442-9301b1e3a1e3 \
  000081_43733-4063e7c46297 \
  001529_50685-dd3b2fe4e5bf \
  000311_2012-1bae69ee8181
 do
  AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  target/release/ab-morph-run rerun-full \
    --aat-dir scratch/morph-full-corpus/aats \
    --source-id "$id" \
    --analyzer vibrato \
    --analyzer sudachi-c \
    --output-dir "scratch/morph-targeted-full/$id" \
    --jobs 1 \
    --examples-output "scratch/morph-targeted-full/$id/examples.jsonl" \
    --max-examples-per-comparison 100
 done
```

All five reruns completed without error rows.

## Findings

### `000293_48490-bc2210d727db`

Output sizes:

| artifact | size |
| --- | ---: |
| analyses.jsonl | 798 bytes |
| comparisons.jsonl | 1029 bytes |
| examples.jsonl | 599 bytes |
| errors.jsonl | 0 bytes |

Comparison stats:

| metric | value |
| --- | ---: |
| from_morphemes | 1 |
| to_morphemes | 1 |
| one_to_one_regions | 1 |
| one_to_one_with_feature_differences | 1 |
| segmentation_regions | 0 |
| coverage_mismatch_regions | 0 |
| boundary_f1 | `null` |

Interpretation:

The source excerpt is only carriage returns: `"\r\r\r\r\r"`. The null boundary score is expected because a one-token analysis has no internal boundaries. The only difference is a feature/schema difference over whitespace/control text, not a meaningful segmentation issue.

### `000293_60442-9301b1e3a1e3`

Output sizes:

| artifact | size |
| --- | ---: |
| analyses.jsonl | 742 bytes |
| comparisons.jsonl | 997 bytes |
| examples.jsonl | 551 bytes |
| errors.jsonl | 0 bytes |

Comparison stats:

| metric | value |
| --- | ---: |
| from_morphemes | 1 |
| to_morphemes | 1 |
| one_to_one_regions | 1 |
| one_to_one_with_feature_differences | 1 |
| segmentation_regions | 0 |
| coverage_mismatch_regions | 0 |
| boundary_f1 | `null` |

Interpretation:

The source excerpt is only `"\r"`. This is the same class as the previous row: a control/whitespace-only projected text. It is useful as a corpus-cleanliness signal, but not a morph-diff algorithm problem.

### `000081_43733-4063e7c46297`

Output sizes:

| artifact | size |
| --- | ---: |
| analyses.jsonl | 1,389,048 bytes |
| comparisons.jsonl | 903,035 bytes |
| examples.jsonl | 42,579 bytes |
| errors.jsonl | 0 bytes |

Comparison stats:

| metric | value |
| --- | ---: |
| from_morphemes | 2008 |
| to_morphemes | 1079 |
| one_to_one_regions | 949 |
| one_to_one_with_feature_differences | 948 |
| segmentation_regions | 114 |
| split_regions | 13 |
| merge_regions | 98 |
| resegment_regions | 3 |
| coverage_mismatch_regions | 0 |
| boundary_precision | 0.9851576994434137 |
| boundary_recall | 0.5291479820627802 |
| boundary_f1 | 0.6884927066450566 |

Interpretation:

The low boundary F1 is mostly recall loss from Sudachi merging spans that Vibrato splits. The first examples are newline/CRLF merges, such as Vibrato `"\r"`, `"\n"` versus Sudachi `"\r\n"`. Other examples include real analyzer behavior differences such as `強て` split as `強` + `て`, and `求むる` split by Vibrato but kept by Sudachi.

This is not a coverage or alignment failure. It is a mixture of newline policy differences and expected analyzer segmentation differences around orthography/classical forms.

### `001529_50685-dd3b2fe4e5bf`

Output sizes:

| artifact | size |
| --- | ---: |
| analyses.jsonl | 393,786,508 bytes |
| comparisons.jsonl | 397,853,148 bytes |
| examples.jsonl | 34,846 bytes |
| errors.jsonl | 0 bytes |

Comparison stats:

| metric | value |
| --- | ---: |
| from_morphemes | 452596 |
| to_morphemes | 442631 |
| one_to_one_regions | 396844 |
| one_to_one_with_feature_differences | 396836 |
| segmentation_regions | 31082 |
| split_regions | 9564 |
| merge_regions | 17322 |
| resegment_regions | 4196 |
| coverage_mismatch_regions | 0 |
| boundary_f1 | 0.9560166438604819 |

Interpretation:

The source is very large. Full analysis plus comparison rows are roughly 792 MB for this one source. The bounded examples are dominated by CRLF and blank-line merges near the start, with occasional content segmentation differences such as `は` + `つ` versus `はつ`.

The high absolute segmentation count is mostly a scale effect: boundary F1 remains high. The full output is too large for routine inspection; compact summaries and bounded examples are the right default.

### `000311_2012-1bae69ee8181`

Output sizes:

| artifact | size |
| --- | ---: |
| analyses.jsonl | 463,106,759 bytes |
| comparisons.jsonl | 475,506,600 bytes |
| examples.jsonl | 34,892 bytes |
| errors.jsonl | 0 bytes |

Comparison stats:

| metric | value |
| --- | ---: |
| from_morphemes | 540691 |
| to_morphemes | 516449 |
| one_to_one_regions | 494199 |
| one_to_one_with_feature_differences | 494071 |
| segmentation_regions | 20709 |
| split_regions | 1098 |
| merge_regions | 19244 |
| resegment_regions | 367 |
| coverage_mismatch_regions | 0 |
| boundary_f1 | 0.9741528542158167 |

Interpretation:

This is another very large source. Full analysis plus comparison rows are roughly 939 MB. Examples are dominated by CRLF, blank-line, and whitespace/full-width-space combinations. The large feature-difference count is expected because the two analyzers expose different feature schemas and dictionary fields for nearly every one-to-one morpheme.

## Cross-cutting conclusions

1. No targeted full rerun produced coverage mismatches or runner errors.
2. `boundary_f1 = null` currently surfaces whitespace/control-only texts; these should be filtered or separately categorized in summary workflows rather than treated as suspicious morph failures.
3. CRLF/newline handling is a major source of structural examples. This is analyzer behavior, but it can obscure more linguistically interesting segmentation differences.
4. Full reruns over large sources are too large for routine diagnosis. The compact artifact remains the comprehensive corpus artifact; full mode should be reserved for selected small/medium sources or filtered slices.
5. Feature-difference counts are dominated by analyzer schema differences, especially Sudachi dictionary/normalized/reading fields versus Vibrato UniDic-style fields. Feature summaries should eventually support key-level filtering before being used as a quality signal.

## Recommended follow-up

1. Add summary-level filtering for null-boundary rows, especially rows whose projected text is only whitespace/control characters.
2. Add an example filter for structural examples involving only whitespace/newline spans, so bounded examples can show linguistically useful differences earlier.
3. Add targeted full-output filters before doing broader full reruns: source slices around compact examples, segmentation-only comparisons, and feature-key allow/deny lists.
4. Add feature-difference aggregation by key pair. Raw feature-difference totals are too coarse to distinguish expected schema mismatch from suspicious analyzer disagreement.
