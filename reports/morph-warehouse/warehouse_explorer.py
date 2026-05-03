# /// script
# requires-python = ">=3.13"
# dependencies = [
#     "duckdb>=1.1",
#     "jedi<0.20",
#     "marimo[sql]==0.23.4",
#     "polars[pyarrow]>=1.0",
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

    return Path, duckdb, json, mo, os


@app.cell
def _(mo):
    mo.md("""
    # Morph Warehouse Explorer

    Interactive DuckDB exploration for the Aozora Bunko morph warehouse triage run.

    Launch from the repository root with:

    ```bash
    reports/morph-warehouse/open-warehouse-explorer.sh
    ```

    The default run directory is `/db/ab-validator/morph-warehouse/runs/triage-2026-05-03-jobs12`.
    Override it with `AB_MORPH_WAREHOUSE_RUN_DIR=/path/to/run`.
    """)
    return


@app.cell
def _(Path, duckdb, mo, os):
    default_run_dir = "/db/ab-validator/morph-warehouse/runs/triage-2026-05-03-jobs12"
    run_dir = Path(
        os.environ.get("AB_MORPH_WAREHOUSE_RUN_DIR", default_run_dir)
    ).expanduser()
    temp_dir = Path(
        os.environ.get(
            "AB_MORPH_DUCKDB_TEMP_DIR",
            "/db/ab-validator/tmp/marimo-morph-warehouse",
        )
    ).expanduser()
    temp_dir.mkdir(parents=True, exist_ok=True)

    conn = duckdb.connect()
    conn.execute(
        f"SET temp_directory='{str(temp_dir).replace(chr(39), chr(39) + chr(39))}'"
    )
    conn.execute("SET preserve_insertion_order=false")
    conn.execute("SET threads=4")
    conn.execute("SET memory_limit='8GB'")

    def _sql_path(path):
        return str(path).replace("'", "''")

    def _read_table(table, file_name=None):
        file_name = file_name or table
        path = run_dir / f"{file_name}.parquet"
        if path.is_dir():
            return f"read_parquet('{_sql_path(path)}/*.parquet')"
        return f"read_parquet('{_sql_path(path)}')"

    required = [
        run_dir / "runs.parquet",
        run_dir / "sources.parquet",
        run_dir / "analyses.parquet",
        run_dir / "morphemes.parquet",
        run_dir / "nway_regions.parquet",
        run_dir / "nway_region_analyzers.parquet",
        run_dir / "feature_pattern_counts.parquet",
        run_dir / "errors.parquet",
    ]
    missing = [str(path) for path in required if not path.exists()]

    if not missing:
        conn.execute(
            f"CREATE OR REPLACE VIEW warehouse_runs AS SELECT * FROM {_read_table('runs')}"
        )
        conn.execute(
            f"CREATE OR REPLACE VIEW warehouse_sources AS SELECT * FROM {_read_table('sources')}"
        )
        conn.execute(
            f"CREATE OR REPLACE VIEW warehouse_analyses AS SELECT * FROM {_read_table('analyses')}"
        )
        conn.execute(
            f"CREATE OR REPLACE VIEW warehouse_morphemes AS SELECT * FROM {_read_table('morphemes')}"
        )
        conn.execute(
            f"CREATE OR REPLACE VIEW warehouse_nway_regions AS SELECT * FROM {_read_table('nway_regions')}"
        )
        conn.execute(
            f"CREATE OR REPLACE VIEW warehouse_nway_region_analyzers AS SELECT * FROM {_read_table('nway_region_analyzers')}"
        )
        conn.execute(
            f"CREATE OR REPLACE VIEW warehouse_feature_pattern_counts AS SELECT * FROM {_read_table('feature_pattern_counts')}"
        )
        conn.execute(
            f"CREATE OR REPLACE VIEW warehouse_errors AS SELECT * FROM {_read_table('errors')}"
        )

    status = (
        mo.md(f"Connected to `{run_dir}` with DuckDB temp under `{temp_dir}`.")
        if not missing
        else mo.md(
            "Missing warehouse files:\n\n"
            + "\n".join(f"- `{path}`" for path in missing)
        )
    )
    status
    return conn, missing, run_dir


@app.cell
def _(mo):
    view = mo.ui.dropdown(
        options=[
            "Source/Text lookup",
            "Top segmentation patterns",
            "Top POS patterns",
            "Source disagreement density",
            "Pairwise segmentation",
            "Term probe",
            "Dialogue punctuation",
            "Largest disagreement regions",
            "AAT structure",
        ],
        value="Source/Text lookup",
        label="Report view",
    )
    feature_key = mo.ui.dropdown(
        options=["pos1", "pos2", "pos3", "pos4"],
        value="pos1",
        label="Feature",
    )
    limit = mo.ui.slider(start=10, stop=200, step=10, value=50, label="Limit")
    term = mo.ui.text(value="あつ", label="Term/probe")
    source_filter = mo.ui.text(value="", label="Source id contains")
    text_filter = mo.ui.text(value="", label="Text id contains")
    source_ids = mo.ui.text(value="", label="Source IDs")
    text_ids = mo.ui.text(value="", label="Text IDs")

    def controls_for_view(view_name):
        rows = [mo.hstack([view, limit], widths="equal")]
        if view_name == "Top POS patterns":
            rows.append(feature_key)
        if view_name == "Term probe":
            rows.append(term)
        if view_name != "Top POS patterns":
            rows.append(
                mo.hstack(
                    [source_filter, text_filter, source_ids, text_ids],
                    widths="equal",
                )
            )
        return mo.vstack(rows)

    return (
        controls_for_view,
        feature_key,
        limit,
        source_filter,
        source_ids,
        term,
        text_filter,
        text_ids,
        view,
    )


@app.cell
def _(controls_for_view, view):
    controls_for_view(view.value)
    return


@app.cell
def _():
    def sql_literal(value):
        return "'" + str(value).replace("'", "''") + "'"

    def parse_ids(value):
        return [part for part in value.replace(",", " ").split() if part]

    def limit_value(limit_widget):
        return max(1, min(int(limit_widget.value), 500))

    def source_text_filter_clause(
        source_filter_widget,
        text_filter_widget,
        source_ids_widget,
        text_ids_widget,
        table_alias,
    ):
        clauses = []
        value = source_filter_widget.value.strip()
        if value:
            clauses.append(
                f"{table_alias}.source_id LIKE {sql_literal('%' + value + '%')}"
            )

        value = text_filter_widget.value.strip()
        if value:
            clauses.append(f"{table_alias}.text_id LIKE {sql_literal('%' + value + '%')}")

        ids = parse_ids(source_ids_widget.value)
        if ids:
            clauses.append(
                f"{table_alias}.source_id IN ({', '.join(sql_literal(id_) for id_ in ids)})"
            )

        ids = parse_ids(text_ids_widget.value)
        if ids:
            clauses.append(
                f"{table_alias}.text_id IN ({', '.join(sql_literal(id_) for id_ in ids)})"
            )

        if not clauses:
            return ""
        return "AND " + " AND ".join(clauses)

    return limit_value, parse_ids, source_text_filter_clause, sql_literal


@app.cell
def _(
    Path,
    conn,
    feature_key,
    json,
    limit,
    limit_value,
    missing,
    mo,
    parse_ids,
    run_dir,
    source_filter,
    source_ids,
    source_text_filter_clause,
    sql_literal,
    term,
    text_filter,
    text_ids,
    view,
    warehouse_feature_pattern_counts,
):
    if missing:
        result = mo.md("Fix the missing warehouse files before running queries.")
    elif view.value == "Source/Text lookup":
        result = mo.sql(
            f"""
            WITH analysis_counts AS (
              SELECT
                source_id,
                text_id,
                count(*) AS analyzer_count,
                string_agg(analyzer_id, ', ' ORDER BY analyzer_id) AS analyzers,
                sum(morpheme_count) AS total_morphemes
              FROM warehouse_analyses
              GROUP BY source_id, text_id
            ),
            region_counts AS (
              SELECT
                source_id,
                text_id,
                count(*) AS regions,
                count(*) FILTER (WHERE has_segmentation_disagreement) AS segmentation_regions,
                count(*) FILTER (WHERE has_feature_disagreement) AS feature_regions,
                count(*) FILTER (WHERE has_coverage_mismatch) AS coverage_mismatch_regions
              FROM warehouse_nway_regions
              WHERE NOT is_nonempty_whitespace
              GROUP BY source_id, text_id
            )
            SELECT
              s.source_id,
              s.text_id,
              s.aat_path,
              s.source_bytes,
              s.source_chars,
              coalesce(ac.analyzer_count, 0) AS analyzer_count,
              coalesce(ac.analyzers, '') AS analyzers,
              coalesce(ac.total_morphemes, 0) AS total_morphemes,
              coalesce(rc.regions, 0) AS regions,
              coalesce(rc.segmentation_regions, 0) AS segmentation_regions,
              coalesce(rc.feature_regions, 0) AS feature_regions,
              coalesce(rc.coverage_mismatch_regions, 0) AS coverage_mismatch_regions
            FROM warehouse_sources AS s
            LEFT JOIN analysis_counts AS ac USING (source_id, text_id)
            LEFT JOIN region_counts AS rc USING (source_id, text_id)
            WHERE 1 = 1
              {source_text_filter_clause(source_filter, text_filter, source_ids, text_ids, "s")}
            ORDER BY s.source_id, s.text_id
            LIMIT {limit_value(limit)}
            """,
            engine=conn,
        )
    elif view.value == "Top segmentation patterns":
        result = mo.sql(
            f"""
            WITH region_patterns AS (
              SELECT
                r.source_id,
                r.text_id,
                r.region_index,
                string_agg(
                  a.analyzer_id || ':[' || array_to_string(a.surfaces, '|') || ']',
                  ' ; '
                  ORDER BY a.analyzer_id
                ) AS pattern
              FROM warehouse_nway_regions AS r
              JOIN warehouse_nway_region_analyzers AS a
                USING (run_id, source_id, text_id, region_index)
              WHERE r.has_segmentation_disagreement
                AND NOT r.is_nonempty_whitespace
                {source_text_filter_clause(source_filter, text_filter, source_ids, text_ids, "r")}
              GROUP BY r.source_id, r.text_id, r.region_index
            )
            SELECT
              count(*) AS examples,
              count(DISTINCT source_id) AS source_count,
              count(DISTINCT text_id) AS text_count,
              string_agg(DISTINCT source_id, ',' ORDER BY source_id)[:240] AS sample_source_ids,
              pattern
            FROM region_patterns
            GROUP BY pattern
            ORDER BY examples DESC, source_count DESC, pattern
            LIMIT {limit_value(limit)}
            """,
            engine=conn,
        )
    elif view.value == "Top POS patterns":
        result = mo.sql(
            f"""
            SELECT
              examples,
              source_count,
              text_count,
              sample_source_ids,
              sample_text_ids,
              pattern
            FROM warehouse_feature_pattern_counts
            WHERE kind = 'feature'
              AND feature_profile = 'core'
              AND feature_key = {sql_literal(feature_key.value)}
              AND NOT is_nonempty_whitespace
            ORDER BY examples DESC, source_count DESC, pattern
            LIMIT {limit_value(limit)}
            """,
            engine=conn,
        )
    elif view.value == "Source disagreement density":
        result = mo.sql(
            f"""
            WITH region_counts AS (
              SELECT
                source_id,
                text_id,
                count(*) AS regions,
                count(*) FILTER (WHERE has_segmentation_disagreement) AS segmentation_regions,
                count(*) FILTER (WHERE has_feature_disagreement) AS feature_regions,
                count(*) FILTER (WHERE has_coverage_mismatch) AS coverage_mismatch_regions
              FROM warehouse_nway_regions AS r
              WHERE NOT is_nonempty_whitespace
                {source_text_filter_clause(source_filter, text_filter, source_ids, text_ids, "r")}
              GROUP BY source_id, text_id
            )
            SELECT
              s.source_id,
              s.text_id,
              s.source_chars,
              rc.regions,
              rc.segmentation_regions,
              rc.feature_regions,
              rc.coverage_mismatch_regions,
              round(rc.segmentation_regions * 1000.0 / greatest(s.source_chars, 1), 3)
                AS segmentation_regions_per_1k_chars,
              round(rc.feature_regions * 1000.0 / greatest(s.source_chars, 1), 3)
                AS feature_regions_per_1k_chars
            FROM region_counts AS rc
            JOIN warehouse_sources AS s USING (source_id, text_id)
            ORDER BY segmentation_regions_per_1k_chars DESC, segmentation_regions DESC, s.source_id
            LIMIT {limit_value(limit)}
            """,
            engine=conn,
        )
    elif view.value == "Pairwise segmentation":
        result = mo.sql(
            f"""
            WITH pairs AS (
              SELECT
                a.analyzer_id AS analyzer_a,
                b.analyzer_id AS analyzer_b,
                r.source_id,
                r.text_id,
                r.region_index,
                a.surfaces AS surfaces_a,
                b.surfaces AS surfaces_b
              FROM warehouse_nway_regions AS r
              JOIN warehouse_nway_region_analyzers AS a
                USING (run_id, source_id, text_id, region_index)
              JOIN warehouse_nway_region_analyzers AS b
                USING (run_id, source_id, text_id, region_index)
              WHERE r.has_segmentation_disagreement
                AND NOT r.is_nonempty_whitespace
                AND a.analyzer_id < b.analyzer_id
                AND a.surfaces <> b.surfaces
                {source_text_filter_clause(source_filter, text_filter, source_ids, text_ids, "r")}
            )
            SELECT
              analyzer_a,
              analyzer_b,
              count(*) AS regions,
              count(DISTINCT source_id) AS source_count,
              count(DISTINCT text_id) AS text_count,
              string_agg(DISTINCT source_id, ',' ORDER BY source_id)[:240] AS sample_source_ids
            FROM pairs
            GROUP BY analyzer_a, analyzer_b
            ORDER BY regions DESC, analyzer_a, analyzer_b
            """,
            engine=conn,
        )
    elif view.value == "Term probe":
        probe = term.value.strip() or "あつ"
        result = mo.sql(
            f"""
            WITH analyzer_surfaces AS (
              SELECT
                r.source_id,
                r.text_id,
                r.region_index,
                a.analyzer_id,
                array_to_string(a.surfaces, '') AS surface_text,
                r.has_segmentation_disagreement,
                r.has_feature_disagreement
              FROM warehouse_nway_regions AS r
              JOIN warehouse_nway_region_analyzers AS a
                USING (run_id, source_id, text_id, region_index)
              WHERE NOT r.is_nonempty_whitespace
                AND contains(array_to_string(a.surfaces, ''), {sql_literal(probe)})
                {source_text_filter_clause(source_filter, text_filter, source_ids, text_ids, "r")}
            )
            SELECT
              analyzer_id,
              count(*) AS analyzer_region_hits,
              count(DISTINCT source_id) AS source_count,
              count(DISTINCT text_id) AS text_count,
              count(*) FILTER (WHERE has_segmentation_disagreement) AS segmentation_region_hits,
              count(*) FILTER (WHERE has_feature_disagreement) AS feature_region_hits,
              string_agg(DISTINCT source_id, ',' ORDER BY source_id)[:240] AS sample_source_ids,
              string_agg(DISTINCT surface_text, ' / ' ORDER BY surface_text)[:300] AS sample_surfaces
            FROM analyzer_surfaces
            GROUP BY analyzer_id
            ORDER BY analyzer_region_hits DESC, analyzer_id
            LIMIT {limit_value(limit)}
            """,
            engine=conn,
        )
    elif view.value == "Dialogue punctuation":
        result = mo.sql(
            f"""
            WITH region_patterns AS (
              SELECT
                r.source_id,
                r.text_id,
                r.region_index,
                max(
                  CASE
                    WHEN regexp_matches(array_to_string(a.surfaces, ''), '[「」『』]')
                    THEN 1
                    ELSE 0
                  END
                ) AS has_quote,
                string_agg(
                  a.analyzer_id || ':[' || array_to_string(a.surfaces, '|') || ']',
                  ' ; '
                  ORDER BY a.analyzer_id
                ) AS pattern
              FROM warehouse_nway_regions AS r
              JOIN warehouse_nway_region_analyzers AS a
                USING (run_id, source_id, text_id, region_index)
              WHERE r.has_segmentation_disagreement
                AND NOT r.is_nonempty_whitespace
                {source_text_filter_clause(source_filter, text_filter, source_ids, text_ids, "r")}
              GROUP BY r.source_id, r.text_id, r.region_index
            )
            SELECT
              count(*) AS examples,
              count(DISTINCT source_id) AS source_count,
              count(DISTINCT text_id) AS text_count,
              string_agg(DISTINCT source_id, ',' ORDER BY source_id)[:240] AS sample_source_ids,
              pattern
            FROM region_patterns
            WHERE has_quote = 1
            GROUP BY pattern
            ORDER BY examples DESC, source_count DESC, pattern
            LIMIT {limit_value(limit)}
            """,
            engine=conn,
        )
    elif view.value == "Largest disagreement regions":
        result = mo.sql(
            f"""
            WITH candidate_regions AS (
              SELECT
                r.source_id,
                r.text_id,
                r.region_index,
                r.char_start,
                r.char_end,
                r.byte_start,
                r.byte_end,
                r.has_segmentation_disagreement,
                r.has_feature_disagreement,
                r.has_coverage_mismatch
              FROM warehouse_nway_regions AS r
              WHERE NOT r.is_nonempty_whitespace
                AND NOT r.is_agreement
                {source_text_filter_clause(source_filter, text_filter, source_ids, text_ids, "r")}
              ORDER BY r.char_end - r.char_start DESC, r.source_id, r.region_index
              LIMIT {limit_value(limit)}
            )
            SELECT
              r.source_id,
              r.text_id,
              r.region_index,
              r.char_start,
              r.char_end,
              r.char_end - r.char_start AS char_len,
              r.has_segmentation_disagreement,
              r.has_feature_disagreement,
              r.has_coverage_mismatch,
              string_agg(
                a.analyzer_id || ':[' || array_to_string(a.surfaces, '|') || ']',
                ' ; '
                ORDER BY a.analyzer_id
              ) AS pattern
            FROM candidate_regions AS r
            JOIN warehouse_nway_region_analyzers AS a
              USING (source_id, text_id, region_index)
            GROUP BY
              r.source_id,
              r.text_id,
              r.region_index,
              r.char_start,
              r.char_end,
              r.has_segmentation_disagreement,
              r.has_feature_disagreement,
              r.has_coverage_mismatch
            ORDER BY char_len DESC, source_id, region_index
            """,
            engine=conn,
        )
    else:
        source_where = source_text_filter_clause(
            source_filter, text_filter, source_ids, text_ids, "s"
        )
        selected_source_ids = parse_ids(source_ids.value)
        selected_text_ids = parse_ids(text_ids.value)
        if not source_where and selected_source_ids:
            source_where = (
                "AND s.source_id = " + sql_literal(selected_source_ids[0])
            )
        elif not source_where and selected_text_ids:
            source_where = "AND s.text_id = " + sql_literal(selected_text_ids[0])

        row = conn.execute(
            f"""
            SELECT s.source_id, s.text_id, s.aat_path, s.source_bytes, s.source_chars
            FROM warehouse_sources AS s
            WHERE 1 = 1
              {source_where}
            ORDER BY s.source_id, s.text_id
            LIMIT 1
            """
        ).fetchone()

        def resolve_aat_path(aat_path):
            path = Path(aat_path).expanduser()
            if path.is_absolute():
                return path
            for candidate in [Path.cwd() / path, run_dir / path]:
                if candidate.exists():
                    return candidate
            return Path.cwd() / path

        if row is None:
            result = mo.md("No source matched the current source/text controls.")
        else:
            source_id, text_id, aat_path, source_bytes, source_chars = row
            resolved_path = resolve_aat_path(aat_path)
            if not resolved_path.exists():
                result = mo.md(
                    f"""
                    No AAT JSON file was found for `{source_id}`.

                    Warehouse path: `{aat_path}`

                    Resolved path checked: `{resolved_path}`
                    """
                )
            else:
                with resolved_path.open("r", encoding="utf-8") as file:
                    aat = json.load(file)

                meta = aat.get("meta", {}) if isinstance(aat, dict) else {}
                blocks = aat.get("blocks", []) if isinstance(aat, dict) else []
                block_rows = [
                    "| index | kind | content items |",
                    "| ---: | --- | ---: |",
                ]
                for index, block in enumerate(blocks[:20]):
                    if isinstance(block, dict):
                        content = block.get("content", [])
                        content_len = len(content) if isinstance(content, list) else 0
                        block_rows.append(
                            f"| {index} | `{block.get('kind', '')}` | {content_len} |"
                        )
                    else:
                        block_rows.append(f"| {index} | `{type(block).__name__}` | 0 |")
                if len(blocks) > 20:
                    block_rows.append(f"| ... | {len(blocks) - 20} more blocks |  |")

                result = mo.vstack(
                    [
                        mo.md(
                            f"""
                            ## AAT structure

                            Source: `{source_id}`

                            Text: `{text_id}`

                            AAT path: `{resolved_path}`

                            Source bytes/chars: `{source_bytes}` / `{source_chars}`

                            Adapter: `{meta.get("adapter", "")}`

                            Adapter version: `{meta.get("adapter_version", "")}`

                            Parse complete: `{meta.get("parse_complete", "")}`

                            Top-level blocks: `{len(blocks)}`

                            {"\n".join(block_rows)}
                            """
                        ),
                        mo.json(aat, label="AAT JSON"),
                    ]
                )

    result
    return


@app.cell
def _(mo):
    mo.md("""
    ## Notes

    This notebook is intentionally triage-safe. It does not query
    `morpheme_features.parquet` or `nway_feature_diffs.parquet`, because the
    current optimized warehouse profile omits those raw feature tables.

    For lemma, reading, normalization, and conjugation drill-downs, create a
    full-profile warehouse or targeted full rerun and add those views here.
    """)
    return


if __name__ == "__main__":
    app.run()
