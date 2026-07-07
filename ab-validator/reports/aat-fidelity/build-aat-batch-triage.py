#!/usr/bin/env python
"""Load ab-check batch outputs into DuckDB and write a SQL-backed triage report."""

from __future__ import annotations

import argparse
import csv
import json
from collections.abc import Iterable
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

import duckdb


@dataclass(frozen=True)
class ReportFile:
    path: Path
    relative_path: Path
    payload: dict[str, Any]


def iter_json_reports(root: Path) -> Iterable[ReportFile]:
    for path in sorted(root.rglob("*.json")):
        payload = json.loads(path.read_text())
        if isinstance(payload, dict) and "results" in payload:
            yield ReportFile(path=path, relative_path=path.relative_to(root), payload=payload)


def read_json_if_present(path: Path) -> dict[str, Any] | None:
    if not path.exists():
        return None
    payload = json.loads(path.read_text())
    return payload if isinstance(payload, dict) else None


def json_text(value: Any) -> str:
    return json.dumps(value, ensure_ascii=False, sort_keys=True)


def walk_source_derived_nodes(value: Any, path: str = "$") -> Iterable[dict[str, Any]]:
    if isinstance(value, dict):
        if value.get("x-provenance") == "source-derived":
            yield {
                "node_path": path,
                "node_kind": str(value.get("kind", "")),
                "syntax_id": str(value.get("x-aozora-syntax-id", "")),
                "node_json": json_text(node_preview(value)),
            }
        for key, child in value.items():
            yield from walk_source_derived_nodes(child, f"{path}.{key}")
    elif isinstance(value, list):
        for index, child in enumerate(value):
            yield from walk_source_derived_nodes(child, f"{path}[{index}]")


def semantic_summary_rows(aat: dict[str, Any]) -> Iterable[dict[str, Any]]:
    syntax = (
        aat.get("meta", {})
        .get("semantic_summary", {})
        .get("syntax", {})
    )
    if not isinstance(syntax, dict):
        return
    for syntax_id, observations in syntax.items():
        if not isinstance(observations, list):
            continue
        for observation in observations:
            if not isinstance(observation, dict):
                continue
            if observation.get("provenance") != "source-derived":
                continue
            yield {
                "syntax_id": str(syntax_id),
                "observation_kind": str(observation.get("kind", "")),
                "provenance": str(observation.get("provenance", "")),
                "value_json": json_text(observation.get("value")),
            }


def node_preview(node: dict[str, Any]) -> dict[str, Any]:
    """Keep source-derived drill-down rows small; full context stays in the AAT file."""
    preview: dict[str, Any] = {}
    for key, value in node.items():
        if key in {"content", "children", "base_content"}:
            continue
        if isinstance(value, (dict, list)):
            continue
        preview[key] = value
    return preview


def create_tables(conn: duckdb.DuckDBPyConnection) -> None:
    conn.execute(
        """
        CREATE TABLE IF NOT EXISTS aat_batch_reports (
          report_id TEXT NOT NULL,
          adapter TEXT NOT NULL,
          work_id TEXT NOT NULL,
          adapter_version TEXT NOT NULL,
          report_path TEXT NOT NULL,
          aat_path TEXT,
          schema_valid BOOLEAN,
          parse_complete BOOLEAN,
          result_count UBIGINT NOT NULL,
          failing_result_count UBIGINT NOT NULL,
          source_derived_node_count UBIGINT NOT NULL,
          loaded_at_utc TIMESTAMP NOT NULL,
          PRIMARY KEY (report_id, adapter, work_id, report_path)
        )
        """
    )
    conn.execute(
        """
        CREATE TABLE IF NOT EXISTS aat_batch_property_results (
          report_id TEXT NOT NULL,
          adapter TEXT NOT NULL,
          work_id TEXT NOT NULL,
          report_path TEXT NOT NULL,
          property TEXT NOT NULL,
          pass BOOLEAN NOT NULL,
          message TEXT,
          line UBIGINT,
          path TEXT,
          confidence TEXT,
          PRIMARY KEY (report_id, adapter, work_id, report_path, property)
        )
        """
    )
    conn.execute(
        """
        CREATE TABLE IF NOT EXISTS aat_batch_source_derived_nodes (
          report_id TEXT NOT NULL,
          adapter TEXT NOT NULL,
          work_id TEXT NOT NULL,
          report_path TEXT NOT NULL,
          node_index UBIGINT NOT NULL,
          node_path TEXT NOT NULL,
          node_kind TEXT NOT NULL,
          syntax_id TEXT NOT NULL,
          node_json JSON NOT NULL,
          PRIMARY KEY (report_id, adapter, work_id, report_path, node_index)
        )
        """
    )
    conn.execute(
        """
        CREATE TABLE IF NOT EXISTS aat_batch_semantic_summary (
          report_id TEXT NOT NULL,
          adapter TEXT NOT NULL,
          work_id TEXT NOT NULL,
          report_path TEXT NOT NULL,
          syntax_id TEXT NOT NULL,
          observation_index UBIGINT NOT NULL,
          observation_kind TEXT NOT NULL,
          provenance TEXT NOT NULL,
          value_json JSON NOT NULL,
          PRIMARY KEY (report_id, adapter, work_id, report_path, syntax_id, observation_index)
        )
        """
    )
    conn.execute(
        """
        CREATE OR REPLACE VIEW aat_batch_report_summary AS
        SELECT
          report_id,
          count(*)::UBIGINT AS total_reports,
          sum(CASE WHEN coalesce(schema_valid, false) THEN 0 ELSE 1 END)::UBIGINT
            AS schema_invalid_or_missing_reports,
          sum(CASE WHEN failing_result_count > 0 THEN 1 ELSE 0 END)::UBIGINT
            AS reports_with_failures,
          sum(failing_result_count)::UBIGINT AS total_failures,
          sum(CASE WHEN coalesce(parse_complete, false) THEN 0 ELSE 1 END)::UBIGINT
            AS parse_incomplete_or_missing_reports,
          sum(CASE WHEN source_derived_node_count > 0 THEN 1 ELSE 0 END)::UBIGINT
            AS reports_with_source_derived_nodes,
          sum(source_derived_node_count)::UBIGINT AS total_source_derived_nodes
        FROM aat_batch_reports
        GROUP BY report_id
        """
    )


def delete_report_id(conn: duckdb.DuckDBPyConnection, report_id: str) -> None:
    for table in [
        "aat_batch_semantic_summary",
        "aat_batch_source_derived_nodes",
        "aat_batch_property_results",
        "aat_batch_reports",
    ]:
        conn.execute(f"DELETE FROM {table} WHERE report_id = ?", [report_id])


def load_batch(
    *,
    conn: duckdb.DuckDBPyConnection,
    report_id: str,
    reports_dir: Path,
    aat_dir: Path | None,
) -> int:
    loaded_at = datetime.now(UTC).replace(tzinfo=None)
    report_rows: list[list[Any]] = []
    property_rows: list[list[Any]] = []
    node_rows: list[list[Any]] = []
    semantic_rows: list[list[Any]] = []

    for report in iter_json_reports(reports_dir):
        payload = report.payload
        adapter = str(payload.get("adapter", ""))
        work_id = str(payload.get("work_id", ""))
        adapter_version = str(payload.get("adapter_version", ""))
        results = payload.get("results", {})
        if not isinstance(results, dict):
            results = {}

        aat_path = aat_dir / report.relative_path if aat_dir is not None else None
        aat = read_json_if_present(aat_path) if aat_path is not None else None
        parse_complete = None
        source_nodes: list[dict[str, Any]] = []
        if aat is not None:
            parse_complete_raw = aat.get("meta", {}).get("parse_complete")
            parse_complete = parse_complete_raw if isinstance(parse_complete_raw, bool) else None
            source_nodes = list(walk_source_derived_nodes(aat))

        schema_result = results.get("schema_valid")
        schema_valid = (
            schema_result.get("pass")
            if isinstance(schema_result, dict) and isinstance(schema_result.get("pass"), bool)
            else None
        )
        failing_result_count = 0
        for property_name, result in sorted(results.items()):
            if not isinstance(result, dict):
                continue
            passed = bool(result.get("pass"))
            if not passed:
                failing_result_count += 1
            property_rows.append(
                [
                    report_id,
                    adapter,
                    work_id,
                    str(report.path),
                    str(property_name),
                    passed,
                    result.get("message"),
                    result.get("line"),
                    result.get("path"),
                    result.get("confidence"),
                ]
            )

        report_rows.append(
            [
                report_id,
                adapter,
                work_id,
                adapter_version,
                str(report.path),
                str(aat_path) if aat is not None and aat_path is not None else None,
                schema_valid,
                parse_complete,
                len(results),
                failing_result_count,
                len(source_nodes),
                loaded_at,
            ]
        )

        for node_index, node in enumerate(source_nodes):
            node_rows.append(
                [
                    report_id,
                    adapter,
                    work_id,
                    str(report.path),
                    node_index,
                    node["node_path"],
                    node["node_kind"],
                    node["syntax_id"],
                    node["node_json"],
                ]
            )

        if aat is not None:
            for observation_index, observation in enumerate(semantic_summary_rows(aat)):
                semantic_rows.append(
                    [
                        report_id,
                        adapter,
                        work_id,
                        str(report.path),
                        observation["syntax_id"],
                        observation_index,
                        observation["observation_kind"],
                        observation["provenance"],
                        observation["value_json"],
                    ]
                )

    conn.execute("BEGIN")
    try:
        delete_report_id(conn, report_id)
        if report_rows:
            conn.executemany(
                """
                INSERT INTO aat_batch_reports
                  (report_id, adapter, work_id, adapter_version, report_path, aat_path,
                   schema_valid, parse_complete, result_count, failing_result_count,
                   source_derived_node_count, loaded_at_utc)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                report_rows,
            )
        if property_rows:
            conn.executemany(
                """
                INSERT INTO aat_batch_property_results
                  (report_id, adapter, work_id, report_path, property, pass,
                   message, line, path, confidence)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                property_rows,
            )
        if node_rows:
            conn.executemany(
                """
                INSERT INTO aat_batch_source_derived_nodes
                  (report_id, adapter, work_id, report_path, node_index,
                   node_path, node_kind, syntax_id, node_json)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?::JSON)
                """,
                node_rows,
            )
        if semantic_rows:
            conn.executemany(
                """
                INSERT INTO aat_batch_semantic_summary
                  (report_id, adapter, work_id, report_path, syntax_id,
                   observation_index, observation_kind, provenance, value_json)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?::JSON)
                """,
                semantic_rows,
            )
        conn.execute("COMMIT")
    except Exception:
        conn.execute("ROLLBACK")
        raise
    return len(report_rows)


QUERIES: dict[str, str] = {
    "summary": """
        SELECT *
        FROM aat_batch_report_summary
        WHERE report_id = ?
    """,
    "property_failures": """
        SELECT property, count(*)::UBIGINT AS failures,
               count(DISTINCT work_id)::UBIGINT AS affected_works,
               min(message) AS example_message
        FROM aat_batch_property_results
        WHERE report_id = ? AND NOT pass
        GROUP BY property
        ORDER BY failures DESC, property
        LIMIT 50
    """,
    "failure_examples": """
        SELECT property, adapter, work_id, report_path, path, line, message
        FROM aat_batch_property_results
        WHERE report_id = ? AND NOT pass
        ORDER BY property, work_id, report_path
        LIMIT 200
    """,
    "adapter_errors": """
        SELECT property, count(*)::UBIGINT AS failures,
               min(message) AS example_message
        FROM aat_batch_property_results
        WHERE report_id = ?
          AND NOT pass
          AND property IN ('fatal_error', 'adapter_timeout', 'adapter_protocol_error')
        GROUP BY property
        ORDER BY failures DESC, property
    """,
    "source_derived_nodes": """
        SELECT node_kind, coalesce(nullif(syntax_id, ''), '(missing)') AS syntax_id,
               count(*)::UBIGINT AS nodes,
               count(DISTINCT work_id)::UBIGINT AS affected_works
        FROM aat_batch_source_derived_nodes
        WHERE report_id = ?
        GROUP BY node_kind, coalesce(nullif(syntax_id, ''), '(missing)')
        ORDER BY nodes DESC, affected_works DESC, node_kind
        LIMIT 100
    """,
    "source_derived_syntax": """
        SELECT syntax_id, observation_kind, count(*)::UBIGINT AS observations,
               count(DISTINCT work_id)::UBIGINT AS affected_works
        FROM aat_batch_semantic_summary
        WHERE report_id = ? AND provenance = 'source-derived'
        GROUP BY syntax_id, observation_kind
        ORDER BY observations DESC, affected_works DESC, syntax_id
        LIMIT 100
    """,
    "source_derived_examples": """
        SELECT adapter, work_id, report_path, node_path, node_kind,
               coalesce(nullif(syntax_id, ''), '(missing)') AS syntax_id,
               node_json
        FROM aat_batch_source_derived_nodes
        WHERE report_id = ?
        ORDER BY syntax_id, work_id, report_path, node_index
        LIMIT 200
    """,
    "parse_incomplete_examples": """
        SELECT adapter, work_id, report_path, aat_path, failing_result_count
        FROM aat_batch_reports
        WHERE report_id = ? AND NOT coalesce(parse_complete, false)
        ORDER BY failing_result_count DESC, work_id
        LIMIT 100
    """,
}


def write_csv(path: Path, columns: list[str], rows: list[tuple[Any, ...]]) -> None:
    with path.open("w", newline="") as handle:
        writer = csv.writer(handle)
        writer.writerow(columns)
        writer.writerows(rows)


def markdown_csv_value(value: Any) -> str:
    if value is None:
        return ""
    text = str(value).replace("\r", "\\r").replace("\n", "\\n")
    if len(text) > 240:
        return text[:237] + "..."
    return text


def run_report_queries(
    conn: duckdb.DuckDBPyConnection,
    *,
    report_id: str,
    out_dir: Path,
) -> None:
    queries_dir = out_dir / "queries"
    outputs_dir = out_dir / "outputs"
    queries_dir.mkdir(parents=True, exist_ok=True)
    outputs_dir.mkdir(parents=True, exist_ok=True)

    output_summaries: list[tuple[str, list[str], list[tuple[Any, ...]]]] = []
    for name, sql in QUERIES.items():
        normalized_sql = "\n".join(line.rstrip() for line in sql.strip().splitlines()) + "\n"
        (queries_dir / f"{name}.sql").write_text(normalized_sql)
        cursor = conn.execute(sql, [report_id])
        rows = cursor.fetchall()
        columns = [column[0] for column in cursor.description]
        write_csv(outputs_dir / f"{name}.csv", columns, rows)
        output_summaries.append((name, columns, rows[:10]))

    lines = [
        "# AAT Batch Triage",
        "",
        f"- report_id: `{report_id}`",
        f"- generated_at_utc: `{datetime.now(UTC).isoformat(timespec='seconds')}`",
        f"- sql_dir: `{queries_dir}`",
        f"- output_dir: `{outputs_dir}`",
        "",
    ]
    for name, columns, rows in output_summaries:
        lines.extend([f"## {name}", "", f"- sql: `{queries_dir / f'{name}.sql'}`"])
        lines.append(f"- csv: `{outputs_dir / f'{name}.csv'}`")
        if rows:
            lines.extend(["", "```csv", ",".join(columns)])
            lines.extend(",".join(markdown_csv_value(value) for value in row) for row in rows)
            lines.append("```")
        else:
            lines.extend(["", "_No rows._"])
        lines.append("")
    (out_dir / "index.md").write_text("\n".join(lines))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--reports-dir", type=Path, required=True)
    parser.add_argument("--aat-dir", type=Path)
    parser.add_argument("--db", type=Path, required=True)
    parser.add_argument("--report-id", required=True)
    parser.add_argument("--out-dir", type=Path, required=True)
    args = parser.parse_args()

    args.db.parent.mkdir(parents=True, exist_ok=True)
    args.out_dir.mkdir(parents=True, exist_ok=True)
    conn = duckdb.connect(str(args.db))
    try:
        create_tables(conn)
        loaded = load_batch(
            conn=conn,
            report_id=args.report_id,
            reports_dir=args.reports_dir,
            aat_dir=args.aat_dir,
        )
        run_report_queries(conn, report_id=args.report_id, out_dir=args.out_dir)
    finally:
        conn.close()
    print(f"loaded {loaded} AAT batch reports into {args.db} as {args.report_id}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
