# /// script
# requires-python = ">=3.13"
# dependencies = [
#     "marimo==0.23.4",
#     "polars>=1.0",
# ]
# ///

import marimo

__generated_with = "0.23.4"
app = marimo.App(width="wide")


@app.cell
def _():
    import json
    import os
    from pathlib import Path

    import marimo as mo
    import polars as pl

    return Path, json, mo, os, pl


@app.cell
def _(Path, os):
    default_report = Path("reports/aat-fidelity/fixtures/report.json")
    report_path = Path(os.environ.get("AB_AAT_FIDELITY_REPORT", default_report)).expanduser()
    return report_path,


@app.cell
def _(mo, report_path):
    mo.md(f"# AAT Fidelity Explorer\n\nReport: `{report_path}`")
    return


@app.cell
def _(json, pl, report_path):
    raw_report = json.loads(report_path.read_text())
    rows = raw_report.get("rows", raw_report if isinstance(raw_report, list) else [])
    table = pl.DataFrame(rows) if rows else pl.DataFrame()
    return raw_report, rows, table


@app.cell
def _(mo, rows):
    adapters = sorted({row.get("adapter", "") for row in rows if row.get("adapter")})
    schema_statuses = sorted({row.get("schema_status", "") for row in rows if row.get("schema_status")})
    upstream_statuses = sorted({row.get("upstream_status", "") for row in rows if row.get("upstream_status")})
    oracle_statuses = sorted({row.get("oracle_status", "") for row in rows if row.get("oracle_status")})

    adapter = mo.ui.dropdown(options=[""] + adapters, value="", label="Adapter")
    schema_status = mo.ui.dropdown(options=[""] + schema_statuses, value="", label="Schema")
    upstream_status = mo.ui.dropdown(options=[""] + upstream_statuses, value="", label="Upstream")
    oracle_status = mo.ui.dropdown(options=[""] + oracle_statuses, value="", label="Oracle")
    case_filter = mo.ui.text(value="", label="Case contains")

    mo.hstack([adapter, schema_status, upstream_status, oracle_status, case_filter], widths="equal")
    return adapter, case_filter, oracle_status, schema_status, upstream_status


@app.cell
def _(adapter, case_filter, oracle_status, rows, schema_status, upstream_status):
    filtered_rows = rows
    if adapter.value:
        filtered_rows = [row for row in filtered_rows if row.get("adapter") == adapter.value]
    if schema_status.value:
        filtered_rows = [
            row for row in filtered_rows if row.get("schema_status") == schema_status.value
        ]
    if upstream_status.value:
        filtered_rows = [
            row for row in filtered_rows if row.get("upstream_status") == upstream_status.value
        ]
    if oracle_status.value:
        filtered_rows = [
            row for row in filtered_rows if row.get("oracle_status") == oracle_status.value
        ]
    if case_filter.value.strip():
        needle = case_filter.value.strip()
        filtered_rows = [row for row in filtered_rows if needle in row.get("case_id", "")]
    return filtered_rows,


@app.cell
def _(filtered_rows, mo, pl):
    filtered_table = pl.DataFrame(filtered_rows) if filtered_rows else pl.DataFrame()
    mo.ui.table(filtered_table, selection="single")
    return filtered_table


@app.cell
def _(filtered_rows, mo):
    selected = filtered_rows[0] if filtered_rows else {}
    mo.json(selected)
    return


if __name__ == "__main__":
    app.run()
