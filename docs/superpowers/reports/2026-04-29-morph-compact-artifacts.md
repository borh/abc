# Morph compact artifacts report

Date: 2026-04-29

## Goal

Make complete morpheme comparison runs practical without storing full `Analysis` and `Comparison` payloads for every corpus item by separating artifacts by purpose.

## Implemented artifact shape

- `--output-profile full` preserves the prior JSONL shape and resume behavior.
- `--output-profile compact` writes compact summary rows for analyses and comparisons.
- Compact runs can additionally write bounded examples with `--examples-output` and `--max-examples-per-comparison`.
- `.zst` output paths are compressed automatically. Appending/resume writes additional zstd frames.
- `--jobs` works with compact and compressed outputs; shard outputs are compressed and then concatenated as zstd frames.
- Compact resume keys on `source_id` derived from the AAT file stem so duplicate logical `text_id` values are not accidentally skipped.
- Error rows include both `source_id` and logical `text_id` when available.
- `--manifest-output` records the input path, input file count, analyzer arguments, job count, and artifact paths.

## Smoke validation

Command shape:

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  cargo run -p ab-morph-run -- analyze-aat \
    --aat-dir scratch/morph-real-sample/aats/aozora-rs-adapter \
    --analyzer vibrato \
    --analyzer sudachi-c \
    --output-profile compact \
    --analyses-output scratch/morph-compact-smoke/analyses.jsonl.zst \
    --comparisons-output scratch/morph-compact-smoke/comparisons.jsonl.zst \
    --examples-output scratch/morph-compact-smoke/examples.jsonl.zst \
    --errors-output scratch/morph-compact-smoke/errors.jsonl.zst \
    --manifest-output scratch/morph-compact-smoke/manifest.json \
    --jobs 2
```

Observed output for 10 AAT inputs:

| artifact | compressed bytes | rows |
| --- | ---: | ---: |
| analyses.jsonl.zst | 730 | 20 |
| comparisons.jsonl.zst | 1,419 | 10 |
| examples.jsonl.zst | 4,865 | 100 |
| errors.jsonl.zst | 18 | 0 |
| manifest.json | 526 | n/a |

Total smoke artifact size: 7,558 bytes.

All 10 comparisons saturated the default per-comparison example budget: `10 comparisons * 10 examples = 100 example rows`. Raise `--max-examples-per-comparison` or rerun a targeted source with `--output-profile full` when exhaustive evidence is needed.

## Full-corpus compact validation

Command shape:

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/morph-full-corpus/aats \
    --analyzer vibrato \
    --analyzer sudachi-c \
    --output-profile compact \
    --analyses-output scratch/morph-full-corpus-compact-canonical/analyses.jsonl.zst \
    --comparisons-output scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst \
    --examples-output scratch/morph-full-corpus-compact-canonical/examples.jsonl.zst \
    --errors-output scratch/morph-full-corpus-compact-canonical/errors.jsonl.zst \
    --manifest-output scratch/morph-full-corpus-compact-canonical/manifest.json \
    --jobs 8
```

Observed output for the same 17,894 AAT inputs as `2026-04-29-morph-full-corpus.md`:

| artifact | compressed bytes | rows | decompressed bytes |
| --- | ---: | ---: | ---: |
| analyses.jsonl.zst | 712,266 | 35,788 | 5,726,629 |
| comparisons.jsonl.zst | 1,500,844 | 17,894 | 9,736,230 |
| examples.jsonl.zst | 5,386,657 | 178,908 | 65,810,562 |
| errors.jsonl.zst | 72 | 0 | 0 |
| manifest.json | 576 | n/a | n/a |

Total full-corpus compact artifact size: 7,600,415 bytes. Excluding the manifest, compact compressed JSONL artifacts are 7,599,839 bytes.

The previous full-detail combined JSONL artifacts were 293,396,854,326 bytes for analyses plus comparisons. Compact summaries plus bounded examples are about 38,606x smaller than those full-detail JSONL files. Compact summaries without examples are 2,213,182 bytes, about 132,568x smaller.

Rows match the full-detail baseline: 35,788 analysis rows, 17,894 comparison rows, and 0 error rows. The full-corpus example count is 178,908, which is 32 rows below the theoretical `17,894 comparisons * 10` budget cap; almost every comparison saturated the default example budget.

The canonical full-corpus run completed in 1,037 seconds with `--jobs 8 --progress`. The progress summary reported `inputs=17894`, `rss_kb=1553272`, and `pss_kb=1551273` at process end. A prior shared-analyzer compact run completed in 1,045 seconds, so the recursive discovery and compact source-text trimming changes did not produce an observed wall-time regression.

## Notes

- Compact examples use `ab-morph-diff` character spans as the source of truth and derive byte offsets only for serialized convenience.
- Feature-diff examples include changed feature payloads; segmentation and coverage examples include surfaces resolved from region data or source analyses.
- Region and coverage examples are emitted before feature-diff examples under the shared per-comparison budget, so tight budgets prioritize structural differences.
- Full-detail artifacts remain available when needed, but should not be the default artifact for comprehensive corpus runs.

## Regenerated full-corpus run after streaming compact comparisons

After regenerating `scratch/morph-full-corpus/aats` from `scratch/ab-index.json`, the compact full-corpus run was repeated with the streaming compact comparison path.

| artifact | compressed bytes | rows | decompressed bytes |
| --- | ---: | ---: | ---: |
| analyses.jsonl.zst | 712,256 | 35,788 | 5,726,628 |
| comparisons.jsonl.zst | 1,500,853 | 17,894 | 9,736,232 |
| examples.jsonl.zst | 5,386,285 | 178,908 | 65,806,323 |
| errors.jsonl.zst | 72 | 0 | 0 |
| manifest.json | 576 | n/a | n/a |

Total compact artifact size including manifest: 7,600,042 bytes. The run completed in 1,015 seconds with 17,894 inputs and 0 error rows.

Memory sampling during the run found a low final RSS/PSS (`rss_kb=764664`, `pss_kb=762664`) but a short-lived peak around 17.16 GB RSS/PSS. This confirms compact storage is solved, while peak runtime memory still needs size-aware scheduling for unusually large works.

## Workflow tooling

The standard corpus workflow is documented in `docs/morph-corpus-workflow.md`. It covers checked AAT generation, compact corpus comparison, compact summary ranking, targeted full-detail reruns by `source_id`, and the `source_id` vs `text_id` grouping policy.
