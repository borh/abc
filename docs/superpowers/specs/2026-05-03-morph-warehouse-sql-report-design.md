# Morph Warehouse SQL Report Design

## Goal

Create a reproducible, SQL-backed reporting layer for the Aozora Bunko morph warehouse. The first version targets the 2026-05-03 triage run and produces durable TSV/Markdown artifacts under `/db`, while keeping all query definitions in the repository.

## Scope

This design covers a literate report scaffold, reusable DuckDB SQL queries, and a local build script. It does not build the interactive browser site yet; the generated report outputs should make the later site straightforward by identifying which drill-downs are useful and which queries need materialized marts.

## Inputs

The report reads an existing warehouse run directory, for example:

```text
/db/ab-validator/morph-warehouse/runs/triage-2026-05-03-jobs12
```

The triage profile includes `runs`, `sources`, `analyses`, `morphemes`, `nway_regions`, `nway_region_analyzers`, and `feature_pattern_counts`. It intentionally omits raw `morpheme_features` and `nway_feature_diffs`, so v1 report queries must use only the triage-safe tables.

## Outputs

The report build writes to an explicit output directory, normally under `/db`:

```text
/db/ab-validator/morph-warehouse/reports/triage-2026-05-03-jobs12/
  index.md
  outputs/*.tsv
  queries/*.sql
```

`index.md` is a tangled-style document: each section states the question, links the exact SQL file, and links the generated result table. Query files are copied beside the outputs so a report snapshot remains reproducible even if repo queries later change.

## Query Set

V1 should include these triage-safe sections:

1. Run summary and table sizes.
2. Source-level disagreement density.
3. Analyzer-pair segmentation disagreement ranking.
4. Top segmentation patterns.
5. Core POS feature pattern ranking for `pos1`.
6. Historical-kana surface probes for forms such as `つて`, `あつ`, `けふ`, `ゐ`, and `ゑ`.
7. Dialogue/punctuation-adjacent disagreement probes around Japanese quote marks.
8. Largest disagreement regions for drill-down.

The query files should be plain DuckDB SQL templates using `__RUN_DIR__` and `__LIMIT__` placeholders. The build script substitutes those placeholders and runs each query with `duckdb`.

## Architecture

Repository files:

- `reports/morph-warehouse/README.md`: operator-facing usage and query inventory.
- `reports/morph-warehouse/build-report.sh`: validates arguments, substitutes SQL templates, writes TSV outputs and `index.md`.
- `reports/morph-warehouse/queries/*.sql`: one SQL template per report section.
- `tests/morph-warehouse-report-smoke.sh`: fast shell smoke test with a tiny synthetic warehouse.

Data files stay outside the repository. The build script creates any DuckDB temp/spill directory under the chosen output directory.

## Interactive Site Path

The interactive site should be a second phase. It should reuse the same SQL query inventory and generated marts rather than query the full 13G warehouse directly from the browser. Good first screens are overview, top segmentation patterns, POS patterns, source ranking, term probe, and pattern drill-down.

## Validation

The shell smoke test creates a tiny warehouse fixture under `/db/ab-validator/tmp`, runs the report builder, and checks that:

- `index.md` is created.
- expected TSV outputs exist.
- substituted query snapshots contain no unresolved placeholders.
- at least one generated TSV contains expected fixture content.

Full validation runs the report builder against the real triage warehouse and verifies that outputs are written under `/db`.
