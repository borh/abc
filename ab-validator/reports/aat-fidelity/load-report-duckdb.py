#!/usr/bin/env python3
"""Load an AAT fidelity report JSON into DuckDB tables."""

from __future__ import annotations

import argparse
import json
import tomllib
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

import duckdb


def load_report_rows(path: Path) -> list[dict[str, Any]]:
    raw = json.loads(path.read_text())
    if isinstance(raw, dict):
        rows = raw.get("rows", [])
    elif isinstance(raw, list):
        rows = raw
    else:
        rows = []
    if not isinstance(rows, list):
        raise ValueError(f"report rows must be a list in {path}")
    return [row for row in rows if isinstance(row, dict)]


def load_oracle_cases(path: Path) -> dict[str, dict[str, Any]]:
    raw = tomllib.loads(path.read_text())
    cases = raw.get("case", [])
    return {case["id"]: case for case in cases if isinstance(case, dict) and "id" in case}


def create_tables(conn: duckdb.DuckDBPyConnection) -> None:
    conn.execute(
        """
        CREATE TABLE IF NOT EXISTS fidelity_reports (
          report_id TEXT PRIMARY KEY,
          report_json_path TEXT NOT NULL,
          oracle_path TEXT NOT NULL,
          loaded_at_utc TIMESTAMP NOT NULL,
          row_count UBIGINT NOT NULL
        )
        """
    )
    conn.execute(
        """
        CREATE TABLE IF NOT EXISTS fidelity_rows (
          report_id TEXT NOT NULL,
          row_index UBIGINT NOT NULL,
          adapter TEXT NOT NULL,
          case_id TEXT NOT NULL,
          category TEXT NOT NULL,
          schema_status TEXT NOT NULL,
          upstream_status TEXT NOT NULL,
          oracle_status TEXT NOT NULL,
          oracle_review_status TEXT NOT NULL,
          oracle_evidence_strength TEXT NOT NULL,
          failure_count UBIGINT NOT NULL,
          failures_json JSON NOT NULL,
          source_utf8 TEXT NOT NULL,
          notes TEXT NOT NULL,
          PRIMARY KEY (report_id, row_index)
        )
        """
    )
    conn.execute(
        """
        CREATE TABLE IF NOT EXISTS fidelity_failures (
          report_id TEXT NOT NULL,
          row_index UBIGINT NOT NULL,
          failure_index UBIGINT NOT NULL,
          adapter TEXT NOT NULL,
          case_id TEXT NOT NULL,
          failure TEXT NOT NULL,
          PRIMARY KEY (report_id, row_index, failure_index)
        )
        """
    )
    conn.execute(
        """
        CREATE TABLE IF NOT EXISTS fidelity_syntax_rows (
          report_id TEXT NOT NULL,
          row_index UBIGINT NOT NULL,
          adapter TEXT NOT NULL,
          case_id TEXT NOT NULL,
          syntax_row_id TEXT NOT NULL,
          PRIMARY KEY (report_id, row_index, syntax_row_id)
        )
        """
    )


def load_into_duckdb(
    *,
    db_path: Path,
    report_path: Path,
    oracle_path: Path,
    report_id: str,
) -> None:
    rows = load_report_rows(report_path)
    cases = load_oracle_cases(oracle_path)

    db_path.parent.mkdir(parents=True, exist_ok=True)
    conn = duckdb.connect(str(db_path))
    create_tables(conn)

    conn.execute("BEGIN")
    try:
        for table in [
            "fidelity_reports",
            "fidelity_rows",
            "fidelity_failures",
            "fidelity_syntax_rows",
        ]:
            conn.execute(f"DELETE FROM {table} WHERE report_id = ?", [report_id])

        conn.execute(
            """
            INSERT INTO fidelity_reports
              (report_id, report_json_path, oracle_path, loaded_at_utc, row_count)
            VALUES (?, ?, ?, ?, ?)
            """,
            [
                report_id,
                str(report_path),
                str(oracle_path),
                datetime.now(UTC).replace(tzinfo=None),
                len(rows),
            ],
        )

        row_values = []
        failure_values = []
        syntax_values = []
        for row_index, row in enumerate(rows):
            case_id = str(row.get("case_id", ""))
            adapter = str(row.get("adapter", ""))
            case = cases.get(case_id, {})
            failures = row.get("failures", [])
            if not isinstance(failures, list):
                failures = [str(failures)]
            syntax_row_ids = case.get("syntax_row_ids", [])
            if not isinstance(syntax_row_ids, list):
                syntax_row_ids = []

            row_values.append(
                [
                    report_id,
                    row_index,
                    adapter,
                    case_id,
                    str(case.get("category", "")),
                    str(row.get("schema_status", "")),
                    str(row.get("upstream_status", "")),
                    str(row.get("oracle_status", "")),
                    str(row.get("oracle_review_status", "")),
                    str(row.get("oracle_evidence_strength", "")),
                    len(failures),
                    json.dumps(failures, ensure_ascii=False),
                    str(case.get("source_utf8", "")),
                    str(case.get("notes", "")),
                ]
            )

            for failure_index, failure in enumerate(failures):
                failure_values.append(
                    [report_id, row_index, failure_index, adapter, case_id, str(failure)]
                )
            for syntax_row_id in syntax_row_ids:
                syntax_values.append(
                    [report_id, row_index, adapter, case_id, str(syntax_row_id)]
                )

        conn.executemany(
            """
            INSERT INTO fidelity_rows
              (report_id, row_index, adapter, case_id, category, schema_status,
               upstream_status, oracle_status, oracle_review_status,
               oracle_evidence_strength, failure_count, failures_json,
               source_utf8, notes)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?::JSON, ?, ?)
            """,
            row_values,
        )
        if failure_values:
            conn.executemany(
                """
                INSERT INTO fidelity_failures
                  (report_id, row_index, failure_index, adapter, case_id, failure)
                VALUES (?, ?, ?, ?, ?, ?)
                """,
                failure_values,
            )
        if syntax_values:
            conn.executemany(
                """
                INSERT INTO fidelity_syntax_rows
                  (report_id, row_index, adapter, case_id, syntax_row_id)
                VALUES (?, ?, ?, ?, ?)
                """,
                syntax_values,
            )
        conn.execute("COMMIT")
    except Exception:
        conn.execute("ROLLBACK")
        raise
    finally:
        conn.close()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--report", type=Path, required=True)
    parser.add_argument("--oracle", type=Path, default=Path("data/aat-oracle-cases.toml"))
    parser.add_argument("--db", type=Path, required=True)
    parser.add_argument("--report-id", default="cross-adapter")
    args = parser.parse_args()

    load_into_duckdb(
        db_path=args.db,
        report_path=args.report,
        oracle_path=args.oracle,
        report_id=args.report_id,
    )
    print(f"loaded {args.report} into {args.db} as {args.report_id}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
