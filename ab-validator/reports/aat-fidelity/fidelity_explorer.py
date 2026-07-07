# /// script
# requires-python = ">=3.13"
# dependencies = [
#     "duckdb>=1.1",
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

    import duckdb
    import marimo as mo
    import polars as pl

    return Path, duckdb, json, mo, os, pl


@app.cell
def _(Path, os):
    ab_db_root = Path(os.environ.get("AB_DB_ROOT", "scratch/state"))
    default_db = ab_db_root / "aat-fidelity/cross-adapter/fidelity.duckdb"
    default_report = ab_db_root / "aat-fidelity/cross-adapter/report.json"
    fixture_report = Path("reports/aat-fidelity/fixtures/report.json")
    db_path = Path(os.environ.get("AB_AAT_FIDELITY_DB", default_db)).expanduser()
    report_path = Path(os.environ.get("AB_AAT_FIDELITY_REPORT", default_report)).expanduser()
    if not report_path.exists():
        report_path = fixture_report
    return (
        db_path,
        report_path,
    )


@app.cell
def _(db_path, mo, report_path):
    mo.md(f"# AAT Fidelity Explorer\n\nDuckDB: `{db_path}`\n\nReport fallback: `{report_path}`")
    return


@app.cell
def _(db_path, duckdb, json, pl, report_path):
    xhtml_observations = []
    xhtml_table = pl.DataFrame()
    if db_path.exists():
        conn = duckdb.connect(str(db_path), read_only=True)
        table = conn.sql(
            """
            SELECT
              r.report_id,
              r.row_index,
              r.adapter,
              r.case_id,
              r.category,
              r.schema_status,
              r.upstream_status,
              r.oracle_status,
              r.oracle_review_status,
              r.oracle_evidence_strength,
              r.failure_count,
              r.failures_json::VARCHAR AS failures_json,
              r.source_utf8,
              r.notes,
              COALESCE(
                list(s.syntax_row_id ORDER BY s.syntax_row_id)
                  FILTER (WHERE s.syntax_row_id IS NOT NULL),
                []
              ) AS syntax_row_ids
            FROM fidelity_rows r
            LEFT JOIN fidelity_syntax_rows s
              ON r.report_id = s.report_id
             AND r.row_index = s.row_index
             AND r.adapter = s.adapter
             AND r.case_id = s.case_id
            GROUP BY
              r.report_id,
              r.row_index,
              r.adapter,
              r.case_id,
              r.category,
              r.schema_status,
              r.upstream_status,
              r.oracle_status,
              r.oracle_review_status,
              r.oracle_evidence_strength,
              r.failure_count,
              r.failures_json,
              r.source_utf8,
              r.notes
            ORDER BY r.adapter, r.case_id
            """
        ).pl()
        rows = table.to_dicts()
        table_names = {
            row["table_name"]
            for row in conn.sql("SHOW TABLES").pl().to_dicts()
            if row.get("table_name")
        }
        if "fidelity_xhtml_observations" in table_names:
            xhtml_columns = {
                row["name"]
                for row in conn.sql("PRAGMA table_info('fidelity_xhtml_observations')")
                .pl()
                .to_dicts()
            }
            optional_xhtml_columns = []
            for column in (
                "card_url",
                "source_url",
                "upstream_url",
                "feature_tags",
                "manifest_status",
                "rendered_body_proxy_eligible",
                "proxy_basis",
                "first_diff_index",
                "upstream_diff_context",
                "local_diff_context",
            ):
                if column in xhtml_columns:
                    optional_xhtml_columns.append(column)
                else:
                    if column == "first_diff_index":
                        optional_xhtml_columns.append("-1 AS first_diff_index")
                    elif column == "rendered_body_proxy_eligible":
                        optional_xhtml_columns.append("false AS rendered_body_proxy_eligible")
                    elif column == "proxy_basis":
                        optional_xhtml_columns.append("'not_eligible' AS proxy_basis")
                    else:
                        optional_xhtml_columns.append(f"'' AS {column}")
            xhtml_table = conn.sql(
                f"""
                SELECT
                  report_id,
                  case_id,
                  raw_equal,
                  main_text_equal,
                  comparison_status,
                  upstream_xhtml_path,
                  local_xhtml_path,
                  upstream_sha256,
                  local_sha256,
                  upstream_main_text_hash,
                  local_main_text_hash,
                  {", ".join(optional_xhtml_columns)},
                  upstream_main_text,
                  local_main_text
                FROM fidelity_xhtml_observations
                ORDER BY report_id, case_id
                """
            ).pl()
            xhtml_observations = xhtml_table.to_dicts()
        raw_report = {"source": "duckdb", "db_path": str(db_path), "rows": rows}
        conn.close()
        source_label = f"DuckDB `{db_path}`"
    else:
        raw_report = json.loads(report_path.read_text())
        rows = raw_report.get("rows", raw_report if isinstance(raw_report, list) else [])
        table = pl.DataFrame(rows) if rows else pl.DataFrame()
        source_label = f"JSON `{report_path}`"
    return raw_report, rows, source_label, table, xhtml_observations, xhtml_table


@app.cell
def _(mo, pl, rows):
    axis_summary_rows = []
    for adapter_name in sorted({row.get("adapter", "") for row in rows if row.get("adapter")}):
        adapter_rows = [row for row in rows if row.get("adapter") == adapter_name]
        axis_summary_rows.append(
            {
                "adapter": adapter_name,
                "cases": len(adapter_rows),
                "schema_pass": sum(1 for row in adapter_rows if row.get("schema_status") == "pass"),
                "upstream_faithful": sum(
                    1 for row in adapter_rows if row.get("upstream_status") == "faithful"
                ),
                "oracle_pass": sum(1 for row in adapter_rows if row.get("oracle_status") == "pass"),
                "oracle_fail": sum(1 for row in adapter_rows if row.get("oracle_status") == "fail"),
            }
        )
    axis_summary = pl.DataFrame(axis_summary_rows) if axis_summary_rows else pl.DataFrame()

    mo.vstack(
        [
            mo.md("## Adapter Axis Summary"),
            mo.ui.table(axis_summary) if axis_summary_rows else mo.md("No fidelity rows loaded."),
        ]
    )
    return (axis_summary,)


@app.cell
def _(mo, rows, source_label):
    adapters = sorted({row.get("adapter", "") for row in rows if row.get("adapter")})
    categories = sorted({row.get("category", "") for row in rows if row.get("category")})
    schema_statuses = sorted(
        {row.get("schema_status", "") for row in rows if row.get("schema_status")}
    )
    upstream_statuses = sorted(
        {row.get("upstream_status", "") for row in rows if row.get("upstream_status")}
    )
    oracle_statuses = sorted(
        {row.get("oracle_status", "") for row in rows if row.get("oracle_status")}
    )
    review_statuses = sorted(
        {row.get("oracle_review_status", "") for row in rows if row.get("oracle_review_status")}
    )
    evidence_strengths = sorted(
        {
            row.get("oracle_evidence_strength", "")
            for row in rows
            if row.get("oracle_evidence_strength")
        }
    )
    syntax_rows = sorted(
        {syntax_row for row in rows for syntax_row in row.get("syntax_row_ids", []) if syntax_row}
    )

    adapter = mo.ui.dropdown(options=[""] + adapters, value="", label="Adapter")
    category = mo.ui.dropdown(options=[""] + categories, value="", label="Category")
    syntax_row = mo.ui.dropdown(options=[""] + syntax_rows, value="", label="Syntax row")
    schema_status = mo.ui.dropdown(options=[""] + schema_statuses, value="", label="Schema")
    upstream_status = mo.ui.dropdown(options=[""] + upstream_statuses, value="", label="Upstream")
    oracle_status = mo.ui.dropdown(options=[""] + oracle_statuses, value="", label="Oracle")
    review_status = mo.ui.dropdown(options=[""] + review_statuses, value="", label="Review")
    evidence_strength = mo.ui.dropdown(
        options=[""] + evidence_strengths, value="", label="Evidence"
    )
    case_filter = mo.ui.text(value="", label="Case contains")
    failure_filter = mo.ui.text(value="", label="Failure contains")

    mo.vstack(
        [
            mo.md(f"Loaded from {source_label}. Rows: **{len(rows)}**"),
            mo.hstack(
                [adapter, category, syntax_row, schema_status, upstream_status, oracle_status],
                widths="equal",
            ),
            mo.hstack(
                [review_status, evidence_strength, case_filter, failure_filter],
                widths="equal",
            ),
        ]
    )
    return (
        adapter,
        case_filter,
        category,
        evidence_strength,
        failure_filter,
        oracle_status,
        review_status,
        schema_status,
        syntax_row,
        upstream_status,
    )


@app.cell
def _(
    adapter,
    case_filter,
    category,
    evidence_strength,
    failure_filter,
    oracle_status,
    review_status,
    rows,
    schema_status,
    syntax_row,
    upstream_status,
):
    filtered_rows = rows
    if adapter.value:
        filtered_rows = [row for row in filtered_rows if row.get("adapter") == adapter.value]
    if category.value:
        filtered_rows = [row for row in filtered_rows if row.get("category") == category.value]
    if syntax_row.value:
        filtered_rows = [
            row for row in filtered_rows if syntax_row.value in row.get("syntax_row_ids", [])
        ]
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
    if review_status.value:
        filtered_rows = [
            row for row in filtered_rows if row.get("oracle_review_status") == review_status.value
        ]
    if evidence_strength.value:
        filtered_rows = [
            row
            for row in filtered_rows
            if row.get("oracle_evidence_strength") == evidence_strength.value
        ]
    if case_filter.value.strip():
        needle = case_filter.value.strip()
        filtered_rows = [row for row in filtered_rows if needle in row.get("case_id", "")]
    if failure_filter.value.strip():
        needle = failure_filter.value.strip()
        filtered_rows = [row for row in filtered_rows if needle in row.get("failures_json", "")]
    return (filtered_rows,)


@app.cell
def _(filtered_rows, mo, pl):
    filtered_table = pl.DataFrame(filtered_rows) if filtered_rows else pl.DataFrame()
    mo.ui.table(filtered_table)
    return (filtered_table,)


@app.cell
def _(filtered_rows, mo):
    row_options = [
        f"{index}: {row.get('adapter', '')} {row.get('case_id', '')} {row.get('oracle_status', '')}"
        for index, row in enumerate(filtered_rows)
    ]
    selected_row = mo.ui.dropdown(
        options=row_options,
        value=row_options[0] if row_options else None,
        label="Row detail",
    )
    selected_row
    return (selected_row,)


@app.cell
def _(filtered_rows, json, mo, selected_row):
    selected = {}
    if selected_row.value:
        index = int(selected_row.value.split(":", 1)[0])
        selected = dict(filtered_rows[index])
        if isinstance(selected.get("failures_json"), str):
            selected["failures"] = json.loads(selected["failures_json"])
    mo.json(selected)
    return


@app.cell
def _(mo, xhtml_observations):
    xhtml_report_ids = sorted(
        {row.get("report_id", "") for row in xhtml_observations if row.get("report_id")}
    )
    default_xhtml_report = (
        "upstream-xhtml-full"
        if "upstream-xhtml-full" in xhtml_report_ids
        else (xhtml_report_ids[0] if xhtml_report_ids else "")
    )
    xhtml_report_id = mo.ui.dropdown(
        options=xhtml_report_ids,
        value=default_xhtml_report,
        label="XHTML report",
    )
    mo.vstack(
        [
            mo.md("## XHTML Report"),
            xhtml_report_id if xhtml_report_ids else mo.md("No XHTML observation reports loaded."),
        ]
    )
    return (xhtml_report_id,)


@app.cell
def _(pl, xhtml_observations, xhtml_report_id):
    active_xhtml_observations = [
        row
        for row in xhtml_observations
        if not xhtml_report_id.value or row.get("report_id") == xhtml_report_id.value
    ]
    active_xhtml_table = (
        pl.DataFrame(active_xhtml_observations) if active_xhtml_observations else pl.DataFrame()
    )
    return active_xhtml_observations, active_xhtml_table


@app.cell
def _(active_xhtml_observations, active_xhtml_table, mo, pl, xhtml_report_id):
    raw_equal_count = sum(1 for row in active_xhtml_observations if row.get("raw_equal"))
    main_text_equal_count = sum(
        1 for row in active_xhtml_observations if row.get("main_text_equal")
    )
    main_text_mismatch_count = len(active_xhtml_observations) - main_text_equal_count
    proxy_eligible_count = sum(
        1 for row in active_xhtml_observations if row.get("rendered_body_proxy_eligible")
    )
    local_missing_main_text_count = sum(
        1
        for row in active_xhtml_observations
        if row.get("comparison_status") == "local_missing_main_text"
    )
    upstream_missing_main_text_count = sum(
        1
        for row in active_xhtml_observations
        if row.get("comparison_status") == "upstream_missing_main_text"
    )
    status_rows = [
        {"comparison_status": row.get("comparison_status") or "unknown"}
        for row in active_xhtml_observations
    ]
    xhtml_status_summary = (
        pl.DataFrame(status_rows)
        .group_by("comparison_status")
        .agg(pl.len().alias("observations"))
        .sort("comparison_status")
        if status_rows
        else pl.DataFrame()
    )
    feature_rows = []
    for row in active_xhtml_observations:
        tags = [tag.strip() for tag in (row.get("feature_tags") or "").split(";") if tag.strip()]
        for tag in tags or ["unclassified"]:
            feature_rows.append(
                {
                    "Feature Tag": tag,
                    "comparison_status": row.get("comparison_status"),
                    "main_text_equal": row.get("main_text_equal"),
                }
            )
    if feature_rows:
        xhtml_feature_summary = (
            pl.DataFrame(feature_rows)
            .group_by(["Feature Tag", "comparison_status"])
            .agg(
                [
                    pl.len().alias("observations"),
                    pl.col("main_text_equal").sum().alias("main_text_equal"),
                ]
            )
            .sort(["Feature Tag", "comparison_status"])
        )
    else:
        xhtml_feature_summary = pl.DataFrame()
    proxy_rows = [
        {"proxy_basis": row.get("proxy_basis") or "not_eligible"}
        for row in active_xhtml_observations
    ]
    xhtml_proxy_summary = (
        pl.DataFrame(proxy_rows)
        .group_by("proxy_basis")
        .agg(pl.len().alias("observations"))
        .sort("proxy_basis")
        if proxy_rows
        else pl.DataFrame()
    )
    mo.vstack(
        [
            mo.md(
                "## XHTML Source Layer\n\n"
                f"Report: `{xhtml_report_id.value}`  \n"
                f"Observations: **{len(active_xhtml_observations)}**  \n"
                f"Raw equal: **{raw_equal_count}**  \n"
                f"`main_text` equal: **{main_text_equal_count}**  \n"
                f"Rendered-body proxy eligible: **{proxy_eligible_count}**  \n"
                f"`main_text` mismatch: **{main_text_mismatch_count}**  \n"
                f"Upstream missing `main_text`: **{upstream_missing_main_text_count}**  \n"
                f"Local missing `main_text`: **{local_missing_main_text_count}**  \n\n"
                "`local_adapter_error` rows are adapter-abort JSON payloads, not "
                "malformed XHTML source files."
            ),
            mo.md("### Status Summary"),
            mo.ui.table(xhtml_status_summary) if status_rows else mo.md("No status rows loaded."),
            mo.md("### Proxy Basis Summary"),
            mo.ui.table(xhtml_proxy_summary)
            if proxy_rows
            else mo.md("No proxy observations loaded."),
            mo.md("### Feature Tag Summary"),
            mo.ui.table(xhtml_feature_summary)
            if feature_rows
            else mo.md("No feature tags loaded."),
            mo.md("### Observations"),
            mo.ui.table(active_xhtml_table)
            if active_xhtml_observations
            else mo.md("No upstream XHTML observations loaded."),
        ]
    )
    return xhtml_feature_summary, xhtml_proxy_summary, xhtml_status_summary


@app.cell
def _(active_xhtml_observations, mo):
    mo.json(active_xhtml_observations[0] if active_xhtml_observations else {})
    return


@app.cell
def _():
    def xhtml_length_delta_bucket(row):
        upstream_len = len(row.get("upstream_main_text") or "")
        local_len = len(row.get("local_main_text") or "")
        delta = local_len - upstream_len
        if delta == 0:
            return "0"
        if abs(delta) <= 10:
            return "1-10"
        if abs(delta) <= 100:
            return "11-100"
        if abs(delta) <= 1000:
            return "101-1000"
        return "1001+"

    def xhtml_first_diff_family(row):
        context = (row.get("upstream_diff_context") or "") + (row.get("local_diff_context") or "")
        if "［＃" in context:
            return "aozora_note_marker"
        if "※" in context:
            return "gaiji_marker"
        if any(marker in context for marker in ["改ページ", "改行"]):
            return "break_marker"
        if any(marker in context for marker in ["字下げ", "地付き", "字詰め", "ぶら下げ"]):
            return "layout_marker"
        if any(marker in context for marker in ["キャプション", ".png", ".jpg", ".jpeg", ".gif"]):
            return "media_marker"
        return "text_content"

    return xhtml_first_diff_family, xhtml_length_delta_bucket


@app.cell
def _(
    active_xhtml_observations,
    mo,
    xhtml_first_diff_family,
    xhtml_length_delta_bucket,
):
    candidate_rows = [
        row for row in active_xhtml_observations if not row.get("rendered_body_proxy_eligible")
    ]
    status_options = sorted(
        {row.get("comparison_status", "") for row in candidate_rows if row.get("comparison_status")}
    )
    feature_options = sorted(
        {
            tag.strip()
            for row in candidate_rows
            for tag in (row.get("feature_tags") or "").split(";")
            if tag.strip()
        }
    )
    bucket_options = sorted({xhtml_length_delta_bucket(row) for row in candidate_rows})
    family_options = sorted({xhtml_first_diff_family(row) for row in candidate_rows})

    xhtml_status_filter = mo.ui.dropdown(
        options=[""] + status_options,
        value="",
        label="Status",
    )
    xhtml_feature_filter = mo.ui.dropdown(
        options=[""] + feature_options,
        value="",
        label="Feature",
    )
    xhtml_length_bucket_filter = mo.ui.dropdown(
        options=[""] + bucket_options,
        value="",
        label="Length bucket",
    )
    xhtml_diff_family_filter = mo.ui.dropdown(
        options=[""] + family_options,
        value="",
        label="Diff family",
    )
    mo.vstack(
        [
            mo.md("### Mismatch Filters"),
            mo.hstack(
                [
                    xhtml_status_filter,
                    xhtml_feature_filter,
                    xhtml_length_bucket_filter,
                    xhtml_diff_family_filter,
                ]
            ),
        ]
    )
    return (
        xhtml_diff_family_filter,
        xhtml_feature_filter,
        xhtml_length_bucket_filter,
        xhtml_status_filter,
    )


@app.cell
def _(
    active_xhtml_observations,
    mo,
    pl,
    xhtml_diff_family_filter,
    xhtml_feature_filter,
    xhtml_first_diff_family,
    xhtml_length_bucket_filter,
    xhtml_length_delta_bucket,
    xhtml_status_filter,
):
    xhtml_mismatch_rows = [
        row
        for row in active_xhtml_observations
        if not row.get("rendered_body_proxy_eligible")
        and (
            not xhtml_status_filter.value
            or row.get("comparison_status") == xhtml_status_filter.value
        )
        and (
            not xhtml_feature_filter.value
            or xhtml_feature_filter.value
            in [tag.strip() for tag in (row.get("feature_tags") or "").split(";")]
        )
        and (
            not xhtml_length_bucket_filter.value
            or xhtml_length_delta_bucket(row) == xhtml_length_bucket_filter.value
        )
        and (
            not xhtml_diff_family_filter.value
            or xhtml_first_diff_family(row) == xhtml_diff_family_filter.value
        )
    ]
    shape_rows = []
    for row in xhtml_mismatch_rows:
        upstream_len = len(row.get("upstream_main_text") or "")
        local_len = len(row.get("local_main_text") or "")
        delta = local_len - upstream_len
        shape_rows.append(
            {
                "status": row.get("comparison_status"),
                "length_direction": "local_longer"
                if delta > 0
                else ("upstream_longer" if delta < 0 else "same_length"),
                "length_delta_bucket": xhtml_length_delta_bucket(row),
                "first_diff_family": xhtml_first_diff_family(row),
            }
        )
    mismatch_shape_summary = (
        pl.DataFrame(shape_rows)
        .group_by(["status", "length_direction", "length_delta_bucket", "first_diff_family"])
        .agg(pl.len().alias("observations"))
        .sort(["status", "length_direction", "length_delta_bucket", "first_diff_family"])
        if shape_rows
        else pl.DataFrame()
    )
    mismatch_table = (
        pl.DataFrame(
            [
                {
                    "case_id": row.get("case_id"),
                    "status": row.get("comparison_status"),
                    "proxy_basis": row.get("proxy_basis"),
                    "first_diff_index": row.get("first_diff_index"),
                    "length_delta_bucket": xhtml_length_delta_bucket(row),
                    "first_diff_family": xhtml_first_diff_family(row),
                    "feature_tags": row.get("feature_tags"),
                    "card_url": row.get("card_url"),
                    "upstream_len": len(row.get("upstream_main_text") or ""),
                    "local_len": len(row.get("local_main_text") or ""),
                }
                for row in xhtml_mismatch_rows
            ]
        )
        if xhtml_mismatch_rows
        else pl.DataFrame()
    )
    options = [
        f"{index}: {row.get('case_id', '')} {row.get('comparison_status', '')}"
        for index, row in enumerate(xhtml_mismatch_rows)
    ]
    selected_xhtml_row = mo.ui.dropdown(
        options=options,
        value=options[0] if options else None,
        label="XHTML mismatch",
    )
    mo.vstack(
        [
            mo.md("## XHTML Mismatch Drilldown"),
            mo.md("### Mismatch Shape Summary"),
            mo.ui.table(mismatch_shape_summary)
            if shape_rows
            else mo.md("No mismatch shape rows loaded."),
            mo.md("### Rows"),
            mo.ui.table(mismatch_table)
            if xhtml_mismatch_rows
            else mo.md("No XHTML mismatches loaded."),
            selected_xhtml_row,
        ]
    )
    return mismatch_shape_summary, selected_xhtml_row, xhtml_mismatch_rows


@app.cell
def _(mo, selected_xhtml_row, xhtml_mismatch_rows):
    selected = {}
    if selected_xhtml_row.value:
        index = int(selected_xhtml_row.value.split(":", 1)[0])
        row = dict(xhtml_mismatch_rows[index])
        selected = {
            "case_id": row.get("case_id"),
            "comparison_status": row.get("comparison_status"),
            "rendered_body_proxy_eligible": row.get("rendered_body_proxy_eligible"),
            "proxy_basis": row.get("proxy_basis"),
            "feature_tags": row.get("feature_tags"),
            "card_url": row.get("card_url"),
            "first_diff_index": row.get("first_diff_index"),
            "upstream_diff_context": row.get("upstream_diff_context"),
            "local_diff_context": row.get("local_diff_context"),
            "upstream_main_text": row.get("upstream_main_text"),
            "local_main_text": row.get("local_main_text"),
            "upstream_xhtml_path": row.get("upstream_xhtml_path"),
            "local_xhtml_path": row.get("local_xhtml_path"),
        }
    mo.json(selected)
    return


if __name__ == "__main__":
    app.run()
