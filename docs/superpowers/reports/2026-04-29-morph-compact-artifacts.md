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

## Notes

- Compact examples use `ab-morph-diff` character spans as the source of truth and derive byte offsets only for serialized convenience.
- Feature-diff examples include changed feature payloads; segmentation and coverage examples include surfaces resolved from region data or source analyses.
- Region and coverage examples are emitted before feature-diff examples under the shared per-comparison budget, so tight budgets prioritize structural differences.
- Full-detail artifacts remain available when needed, but should not be the default artifact for comprehensive corpus runs.
- This smoke uses a 10-file sample, not the full corpus baseline from `2026-04-29-morph-full-corpus.md`; an apples-to-apples corpus-scale compact/full size ratio still requires a compact run over the same input set.
