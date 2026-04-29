# Morph Corpus Workflow

This is the standard workflow for complete morpheme comparison runs over checked AAT JSON.

## 1. Build tools

```bash
cargo build --release -p ab-check -p ab-morph-run
cargo build --release --manifest-path adapters/aozora-rs/Cargo.toml
```

## 2. Generate checked AAT

```bash
target/release/ab-check \
  --index scratch/ab-index.json \
  --corpus references/aozorabunko \
  --adapter adapters/aozora-rs/target/release/aozora-rs-adapter \
  --output scratch/morph-full-corpus/reports \
  --aat-output scratch/morph-full-corpus/aats \
  --jobs 16
```

## 3. Run compact morph comparison

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
    --jobs 8 \
    --progress-interval-seconds 30
```

Compact output is the default artifact for comprehensive runs. Full-detail output is available for targeted debugging, but it is too large as a default corpus artifact.

## 4. Summarize worst cases

Summary commands are dictionary-free. They read compact JSONL artifacts and do not load Vibrato or Sudachi.

Lowest boundary F1 by source record. Rows with undefined boundary F1 (`null`) sort before numeric scores because they need explicit inspection:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst \
  --group-by source-id \
  --sort-by boundary-f1 \
  --limit 20
```

Most segmentation regions by logical text id:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst \
  --group-by text-id \
  --sort-by segmentation-regions \
  --limit 20 \
  --json
```

The tabular output includes `source_ids` and `text_ids`. Use `--json` when membership lists are too long for comfortable terminal reading.

## 5. Rerun full detail for selected sources

Use full detail when the selected source is small enough that full analyses and full comparisons are worth the storage cost:

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  target/release/ab-morph-run rerun-full \
    --aat-dir scratch/morph-full-corpus/aats \
    --source-id 001529_50685-dd3b2fe4e5bf \
    --analyzer vibrato \
    --analyzer sudachi-c \
    --output-dir scratch/morph-targeted-full/001529_50685-dd3b2fe4e5bf \
    --jobs 2 \
    --examples-output scratch/morph-targeted-full/001529_50685-dd3b2fe4e5bf/examples.jsonl \
    --max-examples-per-comparison 100
```

The rerun writes full `analyses.jsonl`, `comparisons.jsonl`, `errors.jsonl`, and `manifest.json` into the output directory. If `--examples-output` is provided, it also writes bounded example rows for quick inspection.

For very large or noisy sources, start with examples-only detail. This keeps the selected rerun streaming and compact: `analyses.jsonl` contains compact analysis summaries, `examples.jsonl` contains bounded comparison examples, and no `comparisons.jsonl` is written.

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  target/release/ab-morph-run rerun-full \
    --aat-dir scratch/morph-full-corpus/aats \
    --source-id 001529_50685-dd3b2fe4e5bf \
    --analyzer vibrato \
    --analyzer sudachi-c \
    --output-dir scratch/morph-targeted-examples/001529_50685-dd3b2fe4e5bf \
    --jobs 2 \
    --detail examples-only \
    --max-examples-per-comparison 100
```

## Identity policy

`source_id` is the stable row/run identity. It is derived from the AAT file stem and distinguishes duplicate records in the corpus index.

`text_id` is the logical work id. It is useful for grouped reporting, but multiple `source_id` values can share one `text_id`.

Reporting tools must make grouping explicit. Use `--group-by source-id` when investigating a concrete AAT file or rerunning full details. Use `--group-by text-id` when asking logical-work questions.
