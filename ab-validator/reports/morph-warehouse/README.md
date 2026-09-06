# Morph Warehouse SQL Report

This directory contains DuckDB SQL templates for reproducible Aozora morph warehouse reports.

Build a report from a warehouse run:

```bash
reports/morph-warehouse/build-report.sh \
  /path/to/warehouse-run \
  /path/to/report \
  50
```

The runner writes all generated data to the output directory. Choose an output filesystem with space for DuckDB temporary files and TSV outputs.

The current query set is triage-safe. It reads only:

- `runs.parquet`
- `sources.parquet`
- `analyses.parquet`
- `morphemes.parquet`
- `nway_regions.parquet`
- `nway_region_analyzers.parquet`
- `feature_pattern_counts.parquet`
- `errors.parquet`

## Interactive marimo notebook

The colocated marimo notebook is `reports/morph-warehouse/warehouse_explorer.py`.
It uses inline `uv`/PEP 723 dependencies, so it can be launched without adding a Python project environment.
Use the wrapper on this Nix-based workstation; it adds the GCC runtime library path needed by the DuckDB Python wheel and then runs the current pinned marimo tool version through `uvx`:

```bash
reports/morph-warehouse/open-warehouse-explorer.sh
```

Set `AB_MORPH_MARIMO_HOST` and `AB_MORPH_MARIMO_PORT` for the listen address.
Select the input run and temporary spill directory explicitly:

```bash
AB_MORPH_WAREHOUSE_RUN_DIR=/path/to/warehouse-run \
AB_MORPH_DUCKDB_TEMP_DIR=/path/to/temporary-storage \
reports/morph-warehouse/open-warehouse-explorer.sh
```

The notebook uses contextual marimo UI controls for report view, row limit, feature key, term probes, source/text substring filters, and comma- or whitespace-separated exact `source_id` / `text_id` lists. It is triage-safe: it only reads the same warehouse tables as the static report, plus the selected source's AAT JSON when the `AAT structure` view is active. AAT JSON is rendered with marimo's native JSON tree viewer.

## Hydrated example bundles

`ab-morph-run hydrate-interesting` turns an interestingness ranking JSON into
a self-contained `examples.md` + `examples.json` bundle: projected-text
snippets with the disagreement region marked 【…】, per-analyzer
segmentation/POS tables, aozora markup reconstructed from the AAT, and work
metadata (title, author, year — author names resolved from an ABC catalog
export's `persons/`). Run it where the warehouse run dir and AAT corpus live
then copy the bundle next to the existing reports:

```bash
just morph-warehouse-hydrate-interesting \
  /path/to/ranking.json \
  /path/to/warehouse-run \
  /path/to/examples \
  /path/to/catalog
```

Every layer degrades independently (missing catalog → author ids; changed
AAT → `projection-mismatch` instead of a wrong quote); the bundle records
per-example `errors[]` and the CLI prints a full/partial/failed tally.
