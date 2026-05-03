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

Prefer warehouse mode for new comprehensive corpus analysis. Compact JSONL remains useful for compatibility and targeted debugging while the old summary commands are being retired.

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

Warehouse replacements for legacy JSONL summary commands:

| Legacy JSONL command | Warehouse replacement |
|---|---|
| `summarize-compact` | `summarize-warehouse-nway` and `summarize-warehouse-pairwise` |
| `summarize-examples` | `summarize-warehouse-regions` |
| `summarize-differences` | `summarize-warehouse-patterns` plus `summarize-warehouse-pattern-examples` |
| `summarize-nway` | `summarize-warehouse-nway` |
| `summarize-nway-patterns` | `summarize-warehouse-patterns` |

Keep JSONL commands only for compatibility with old artifacts and targeted debugging exports. New comprehensive corpus analysis should use warehouse mode plus `summarize-warehouse-triage`.

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

The existing JSONL `--output-dir` mode remains for compatibility and targeted debugging, but it is not the canonical comprehensive store.

## 4. Summarize worst cases

Summary commands are dictionary-free. They read compact JSONL artifacts and do not load Vibrato or Sudachi.

Lowest boundary F1 by source record. Rows with undefined boundary F1 (`null`) sort before numeric scores because they need explicit inspection:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical-v2/comparisons.jsonl.zst \
  --group-by source-id \
  --sort-by boundary-f1 \
  --limit 20
```

Most segmentation regions by logical text id:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical-v2/comparisons.jsonl.zst \
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

## 6. Isolate whitespace-related example evidence

Compact example rows include `whitespace_only`, computed from the source excerpt. This keeps raw comparison metrics intact while making newline/indentation evidence separable during triage.

Whitespace-only examples by source record:

```bash
target/release/ab-morph-run summarize-examples \
  --examples scratch/morph-full-corpus-compact-canonical-v2/examples.jsonl.zst \
  --group-by source-id \
  --filter whitespace-only \
  --sort-by whitespace-examples \
  --limit 20
```

Lexical examples by source record, excluding whitespace-only spans:

```bash
target/release/ab-morph-run summarize-examples \
  --examples scratch/morph-full-corpus-compact-canonical-v2/examples.jsonl.zst \
  --group-by source-id \
  --filter lexical-only \
  --sort-by lexical-examples \
  --limit 20
```

The example summary is bounded-evidence triage, not an exhaustive count of all whitespace or lexical diffs. Increase `--max-examples-per-comparison` or run targeted detail when the cap saturates.

## 7. Use exact whitespace/lexical compact counters

Compact comparison summaries include exact whitespace-vs-lexical counters. These are exhaustive per comparison and are better for ranking than bounded examples.

Lexical segmentation disagreements:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical-v2/comparisons.jsonl.zst \
  --group-by source-id \
  --sort-by lexical-segmentation-regions \
  --limit 20
```

Whitespace-only segmentation disagreements:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical-v2/comparisons.jsonl.zst \
  --group-by source-id \
  --sort-by whitespace-segmentation-regions \
  --limit 20
```

Lexical feature-diff ranking:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical-v2/comparisons.jsonl.zst \
  --group-by source-id \
  --sort-by lexical-feature-differences \
  --limit 20
```

Use `summarize-examples` after this to inspect bounded concrete evidence for the selected source IDs.

## 8. Filter morph triage by script/category

Compact comparison summaries include a source-level `source_script_category`; compact example rows include a span-level `script_category`.

Japanese-source lexical segmentation ranking:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical-v2/comparisons.jsonl.zst \
  --group-by source-id \
  --script-category japanese \
  --sort-by lexical-segmentation-regions \
  --limit 20
```

Mixed-source ranking, useful for technical tables or sources containing both Japanese and code/Latin text:

```bash
target/release/ab-morph-run summarize-compact \
  --comparisons scratch/morph-full-corpus-compact-canonical-v2/comparisons.jsonl.zst \
  --group-by source-id \
  --script-category mixed \
  --sort-by lexical-segmentation-regions \
  --limit 20
```

Latin/code example evidence inside any source category:

```bash
target/release/ab-morph-run summarize-examples \
  --examples scratch/morph-full-corpus-compact-canonical-v2/examples.jsonl.zst \
  --group-by source-id \
  --script-category latin-code \
  --filter lexical-only \
  --sort-by examples \
  --limit 20
```

## 9. Summarize recurring tokenizer differences

Use `summarize-differences` to aggregate concrete compact example rows into recurring segmentation patterns and feature-value transitions. This is a bounded-evidence report: it summarizes the examples captured during the compact run, not every diff in every source unless the run used a sufficiently high `--max-examples-per-comparison`.

Top Japanese lexical segmentation patterns:

```bash
target/release/ab-morph-run summarize-differences \
  --examples scratch/morph-full-corpus-compact-canonical-v2/examples.jsonl.zst \
  --kind segmentation \
  --filter lexical-only \
  --script-category japanese \
  --limit 25
```

Top Japanese lexical feature transitions:

```bash
target/release/ab-morph-run summarize-differences \
  --examples scratch/morph-full-corpus-compact-canonical-v2/examples.jsonl.zst \
  --kind feature \
  --filter lexical-only \
  --script-category japanese \
  --limit 25
```

Top Japanese POS transitions for a specific feature key:

```bash
target/release/ab-morph-run summarize-differences \
  --examples scratch/morph-full-corpus-compact-canonical-v2/examples.jsonl.zst \
  --kind feature \
  --feature-key pos1 \
  --filter lexical-only \
  --script-category japanese \
  --limit 25
```

The tabular output shows source/text counts plus short ID samples. Use `--json` when full source/text membership is needed.

## 10. Run true N-way tokenizer comparison

Use N-way output when comparing three or more analyzers over the same AAT inputs. Pairwise compact comparisons remain useful for boundary F1; N-way rows expose regions where any analyzer differs and aggregate recurring multi-analyzer patterns.

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/morph-full-corpus/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --output-profile compact \
    --output-dir scratch/morph-full-corpus-nway \
    --nway \
    --nway-pattern-counts \
    --jobs 8 \
    --progress-interval-seconds 30
```

`--output-dir` writes standard artifact names:

- `analyses.jsonl.zst`
- `comparisons.jsonl.zst`
- `examples.jsonl.zst`
- `errors.jsonl.zst`
- `manifest.json`
- `nway.jsonl.zst` when `--nway` is set
- `nway-pattern-counts.jsonl.zst` when `--nway-pattern-counts` is set

The older `--*-output` flags remain available as advanced overrides. N-way output is compact-only in this phase. Keep exact pattern counts in `nway-pattern-counts.jsonl.zst`; `nway.jsonl.zst` stays small and contains source-level summary rows plus bounded examples. Resume requires the analysis, N-way, and pattern-count outputs to agree on completed `source_id` values; incomplete sources are rerun.

Worst N-way sources by segmentation disagreement:

```bash
target/release/ab-morph-run summarize-nway \
  --nway scratch/morph-full-corpus-nway/nway.jsonl.zst \
  --sort-by regions-with-segmentation-disagreement \
  --limit 20
```

Recurring N-way segmentation partitions:

```bash
target/release/ab-morph-run summarize-nway-patterns \
  --pattern-counts scratch/morph-full-corpus-nway/nway-pattern-counts.jsonl.zst \
  --kind segmentation \
  --limit 25
```

Recurring N-way POS disagreements:

```bash
target/release/ab-morph-run summarize-nway-patterns \
  --pattern-counts scratch/morph-full-corpus-nway/nway-pattern-counts.jsonl.zst \
  --kind feature \
  --feature-key pos1 \
  --exclude-feature-value 空白 \
  --limit 25
```

Pattern-count reports count exact matching regions. If one source row contains the same pattern in five regions, it contributes five examples to that pattern. If `--pattern-counts` is unavailable, `summarize-nway-patterns --nway ...` still works as a bounded-example fallback.
