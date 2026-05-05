# Morph Corpus Workflow

This is the standard workflow for complete morpheme comparison runs over checked AAT JSON.

## 1. Build tools

```bash
cargo build --release -p ab-check -p ab-morph-run
cargo build --release --manifest-path adapters/aozora-rs/Cargo.toml
```

## 2. Generate checked AAT

`ab-index` and `ab-check` only treat sources under `cards/{card}/files/` as Aozora literary-work corpus inputs. Reference/support files outside that tree, such as `tools/JISTABLE.zip::JISTABLE.TXT`, may be useful for JIS/gaiji handling but are not Aozora markup works and should not be parsed as corpus text.

```bash
target/release/ab-check \
  --index scratch/ab-index.json \
  --corpus references/aozorabunko \
  --adapter adapters/aozora-rs/target/release/aozora-rs-adapter \
  --output scratch/morph-full-corpus/reports \
  --aat-output scratch/morph-full-corpus/aats \
  --jobs 16
```

## 3. Legacy compact JSONL morph comparison

Prefer warehouse mode for new comprehensive corpus analysis. The compact JSONL output path remains useful for compatibility and targeted debugging, but use explicit output filenames with `analyze-aat` (no implicit JSONL `--output-dir` defaults), and the old `summarize-*` JSONL commands are now removed.

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

Full-detail JSONL output is available for targeted debugging, but it is too large as a default corpus artifact.

Keep JSONL outputs only for compatibility/debug with old artifacts and targeted exports (status: compat). New comprehensive corpus analysis should use warehouse mode plus `summarize-warehouse-triage`.

## Warehouse mode: canonical comprehensive artifact

Warehouse mode writes sealed Parquet fact tables. It is the preferred format for complete corpus analysis. It does not write JSONL outputs.

Use `/db` for warehouse outputs and DuckDB temporary spill on machines where the project filesystem is space-constrained. The triage profile is the default choice for first-pass corpus analysis: it keeps segmentation, N-way, pairwise, morpheme, and materialized core POS pattern facts, but omits the raw `morpheme_features.parquet` and `nway_feature_diffs.parquet` tables.

```bash
TMPDIR=/db/ab-validator/tmp \
TMP=/db/ab-validator/tmp \
TEMP=/db/ab-validator/tmp \
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/morph-full-corpus/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --warehouse-dir /db/ab-validator/morph-warehouse \
    --run-id triage-2026-05-03 \
    --warehouse-profile triage \
    --jobs 12
```

The default `--warehouse-profile full` preserves all raw feature fact tables for later drill-down, at significantly higher storage cost. Use full only when raw `morpheme_features` or raw `nway_feature_diffs` queries are required.

Warehouse mode does not support `--resume`. Interrupted runs leave staging directories under `.staging/`; the next run with the same `--run-id` removes stale staging before starting. Published runs under `runs/<run-id>/` are immutable.

Query with DuckDB:

```bash
duckdb -c ".read /db/ab-validator/morph-warehouse/runs/triage-2026-05-03/views.sql" \
       -c "SELECT * FROM top_segmentation_patterns LIMIT 50;"
```

Query recurring warehouse patterns through `ab-morph-run` without JSONL intermediates. The standard first-pass triage bundle writes errors, top lexical segmentation patterns, and top lexical `pos1` feature patterns to a directory:

```bash
target/release/ab-morph-run summarize-warehouse-triage \
  --run-dir /db/ab-validator/morph-warehouse/runs/triage-2026-05-03 \
  --output-dir /db/ab-validator/morph-warehouse/triage/triage-2026-05-03 \
  --limit 50
```

Use the individual summary commands for custom grouping, filtering, or drill-down:

```bash
target/release/ab-morph-run summarize-warehouse-nway \
  --run-dir /db/ab-validator/morph-warehouse/runs/triage-2026-05-03 \
  --group-by source-id \
  --sort-by regions-with-segmentation-disagreement \
  --limit 50

target/release/ab-morph-run summarize-warehouse-pairwise \
  --run-dir /db/ab-validator/morph-warehouse/runs/triage-2026-05-03 \
  --sort-by segmentation-regions \
  --filter lexical-only \
  --limit 50

target/release/ab-morph-run summarize-warehouse-patterns \
  --run-dir /db/ab-validator/morph-warehouse/runs/triage-2026-05-03 \
  --kind segmentation \
  --filter lexical-only \
  --limit 50

target/release/ab-morph-run summarize-warehouse-patterns \
  --run-dir /db/ab-validator/morph-warehouse/runs/triage-2026-05-03 \
  --kind feature \
  --feature-key pos1 \
  --feature-profile core \
  --filter lexical-only \
  --limit 50

# Feature pattern examples need --warehouse-profile full because they read raw nway_feature_diffs.
target/release/ab-morph-run summarize-warehouse-pattern-examples \
  --run-dir /db/ab-validator/morph-warehouse/runs/full-2026-05-03 \
  --kind feature \
  --feature-key pos1 \
  --filter lexical-only \
  --pattern 'pos1 whole_region 助動詞=>vibrato:unidic-cwj-202512 ; 助詞=>sudachi-a+sudachi-c' \
  --limit 20

target/release/ab-morph-run summarize-warehouse-regions \
  --run-dir /db/ab-validator/morph-warehouse/runs/triage-2026-05-03 \
  --kind segmentation \
  --filter lexical-only \
  --limit 20 \
  --json

target/release/ab-morph-run summarize-warehouse-errors \
  --run-dir /db/ab-validator/morph-warehouse/runs/triage-2026-05-03 \
  --group-by error-code \
  --limit 50
```

The existing JSONL compatibility mode remains for targeted debugging, with explicit output paths required when using `--output-dir`; it is not the canonical comprehensive store.

## 4. Summarize worst cases

Summary commands are dictionary-free and read only warehouse facts. Use these first to rank corpus-level disagreement patterns.

Variable-boundary source ranking:

```bash
target/release/ab-morph-run summarize-warehouse-pairwise \
  --run-dir /db/ab-validator/morph-warehouse/runs/triage-2026-05-03 \
  --sort-by variable-boundary-count \
  --limit 20
```

Source-level segmentation disagreement prioritization:

```bash
target/release/ab-morph-run summarize-warehouse-nway \
  --run-dir /db/ab-validator/morph-warehouse/runs/triage-2026-05-03 \
  --group-by source-id \
  --sort-by regions-with-segmentation-disagreement \
  --limit 20
```

Source-level ranking by logical work ID:

```bash
target/release/ab-morph-run summarize-warehouse-nway \
  --run-dir /db/ab-validator/morph-warehouse/runs/triage-2026-05-03 \
  --group-by text-id \
  --sort-by regions-with-segmentation-disagreement \
  --limit 20
```

## 5. Region-level evidence views

Use `summarize-warehouse-regions` to inspect concrete bounded regions for triage.

```bash
target/release/ab-morph-run summarize-warehouse-regions \
  --run-dir /db/ab-validator/morph-warehouse/runs/triage-2026-05-03 \
  --kind segmentation \
  --filter lexical-only \
  --limit 20 \
  --json
```

Whitespace-only regions:

```bash
target/release/ab-morph-run summarize-warehouse-regions \
  --run-dir /db/ab-validator/morph-warehouse/runs/triage-2026-05-03 \
  --kind segmentation \
  --filter whitespace-only \
  --limit 20 \
  --json
```

## 6. Pattern-level recurrence

Use `summarize-warehouse-patterns` for recurring segmentation and feature transitions.

```bash
target/release/ab-morph-run summarize-warehouse-patterns \
  --run-dir /db/ab-validator/morph-warehouse/runs/triage-2026-05-03 \
  --kind segmentation \
  --filter lexical-only \
  --limit 25
```

```bash
target/release/ab-morph-run summarize-warehouse-patterns \
  --run-dir /db/ab-validator/morph-warehouse/runs/triage-2026-05-03 \
  --kind feature \
  --feature-key pos1 \
  --feature-profile core \
  --filter lexical-only \
  --limit 25
```

Inspect concrete spans for a matched pattern with `summarize-warehouse-pattern-examples`:

```bash
target/release/ab-morph-run summarize-warehouse-pattern-examples \
  --run-dir /db/ab-validator/morph-warehouse/runs/full-2026-05-03 \
  --kind feature \
  --feature-key pos1 \
  --filter lexical-only \
  --pattern 'pos1 whole_region 助動詞=>vibrato:unidic-cwj-202512 ; 助詞=>sudachi-a+sudachi-c' \
  --limit 20
```

## 7. Run true N-way tokenizer comparison

Use N-way in warehouse mode when comparing three or more analyzers over the same AAT inputs. Pairwise is still useful for source-level ranking.

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/morph-full-corpus/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --warehouse-dir /db/ab-validator/morph-warehouse \
    --run-id triage-nway-2026-05-03 \
    --warehouse-profile full \
    --jobs 12
```

```bash
target/release/ab-morph-run summarize-warehouse-nway \
  --run-dir /db/ab-validator/morph-warehouse/runs/triage-nway-2026-05-03 \
  --group-by source-id \
  --sort-by regions-with-feature-disagreement \
  --limit 20
```

```bash
target/release/ab-morph-run summarize-warehouse-patterns \
  --run-dir /db/ab-validator/morph-warehouse/runs/triage-nway-2026-05-03 \
  --kind feature \
  --feature-key pos1 \
  --filter lexical-only \
  --limit 25
```

## 8. Rerun full detail for selected sources

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
