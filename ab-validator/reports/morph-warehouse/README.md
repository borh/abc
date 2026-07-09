# Morph Warehouse SQL Report

This directory contains DuckDB SQL templates for reproducible Aozora morph warehouse reports.

Build a report from a warehouse run:

```bash
reports/morph-warehouse/build-report.sh \
  /db/ab-validator/morph-warehouse/runs/triage-2026-05-03-jobs12 \
  /db/ab-validator/morph-warehouse/reports/triage-2026-05-03-jobs12 \
  50
```

The runner writes all generated data to the output directory. Use `/db` for real corpus reports so DuckDB temporary files and TSV outputs do not consume project filesystem space.

The current query set is triage-safe. It reads only:

- `runs.parquet`
- `sources.parquet`
- `analyses.parquet`
- `morphemes.parquet`
- `nway_regions.parquet`
- `nway_region_analyzers.parquet`
- `feature_pattern_counts.parquet`
- `errors.parquet`

Raw feature drill-down queries should be added separately after a full-profile warehouse run exists.

## Interactive marimo notebook

The colocated marimo notebook is `reports/morph-warehouse/warehouse_explorer.py`.
It uses inline `uv`/PEP 723 dependencies, so it can be launched without adding a Python project environment.
Use the wrapper on this Nix-based workstation; it adds the GCC runtime library path needed by the DuckDB Python wheel and then runs the current pinned marimo tool version through `uvx`:

```bash
reports/morph-warehouse/open-warehouse-explorer.sh
```

By default the wrapper binds to:

```text
farspark.hyakutake-barbel.ts.net:27188
```

Override the host or port with `AB_MORPH_MARIMO_HOST` and `AB_MORPH_MARIMO_PORT`.

By default it opens the current triage warehouse:

```text
/db/ab-validator/morph-warehouse/runs/triage-2026-05-03-jobs12
```

Override the run directory or DuckDB temporary spill directory with:

```bash
AB_MORPH_WAREHOUSE_RUN_DIR=/db/ab-validator/morph-warehouse/runs/triage-2026-05-03-jobs12 \
AB_MORPH_DUCKDB_TEMP_DIR=/db/ab-validator/tmp/marimo-morph-warehouse \
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
(hinoki for full-corpus runs — use the Tailscale FQDN
`hinoki.hyakutake-barbel.ts.net`; the bare `hinoki` ssh alias resolves to a
different host), then copy the bundle next to the existing reports:

```bash
just morph-warehouse-hydrate-interesting \
  /db/ab-validator/morph-warehouse/reports/dict-cmp-m2-full-2026-07-09/interesting-dictcmp-m2-full.json \
  /db/ab-validator/morph-warehouse/runs/dict-cmp-m2-full-2026-07-09 \
  /db/ab-validator/morph-warehouse/reports/dict-cmp-m2-full-2026-07-09/examples \
  /db/ab-validator/abc-corpus/aozora-catalog-0e9ea3e5
```

Every layer degrades independently (missing catalog → author ids; changed
AAT → `projection-mismatch` instead of a wrong quote); the bundle records
per-example `errors[]` and the CLI prints a full/partial/failed tally.
