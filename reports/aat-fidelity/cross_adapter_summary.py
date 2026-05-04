#!/usr/bin/env python3
"""Render a compact cross-adapter AAT oracle summary."""

from __future__ import annotations

import argparse
import json
import tomllib
from collections import Counter, defaultdict
from pathlib import Path
from typing import Any

try:
    import duckdb
except ImportError:  # pragma: no cover - handled by optional CLI branch.
    duckdb = None


def load_cases(path: Path) -> dict[str, dict[str, Any]]:
    data = tomllib.loads(path.read_text())
    return {case["id"]: case for case in data.get("case", [])}


def status_counts(rows: list[dict[str, Any]], key: str) -> Counter[str]:
    return Counter(str(row.get(key, "")) for row in rows)


def failure_bucket(row: dict[str, Any]) -> str:
    if row.get("schema_status") != "pass":
        return "schema"
    if row.get("upstream_status") == "faithful" and row.get("oracle_status") == "fail":
        if row.get("adapter") == "aozora2html":
            return "faithful-rendered-output-vs-oracle"
        return "faithful-upstream-vs-oracle"
    if row.get("upstream_status") == "no_observation" and row.get("oracle_status") == "fail":
        return "needs-upstream-observation"
    if row.get("oracle_status") == "fail":
        return "oracle-mismatch"
    return "passing"


def case_family(case_id: str) -> str:
    return case_id.split(".", 1)[0]


def md_table(headers: list[str], rows: list[list[Any]]) -> list[str]:
    out = [
        "| " + " | ".join(headers) + " |",
        "| " + " | ".join("---" for _ in headers) + " |",
    ]
    for row in rows:
        out.append("| " + " | ".join(str(cell) for cell in row) + " |")
    return out


def xhtml_evidence_rows(db_path: Path, report_id: str) -> list[list[Any]]:
    if duckdb is None:
        raise RuntimeError("duckdb is required when --xhtml-db is used")
    conn = duckdb.connect(str(db_path), read_only=True)
    try:
        rows = conn.execute(
            """
            SELECT
              'total observations' AS metric,
              count(*)::UBIGINT AS value
            FROM fidelity_xhtml_observations
            WHERE report_id = ?
            UNION ALL
            SELECT
              'rendered-body proxy eligible' AS metric,
              sum(rendered_body_proxy_eligible)::UBIGINT AS value
            FROM fidelity_xhtml_observations
            WHERE report_id = ?
            UNION ALL
            SELECT
              comparison_status AS metric,
              count(*)::UBIGINT AS value
            FROM fidelity_xhtml_observations
            WHERE report_id = ?
            GROUP BY comparison_status
            ORDER BY metric
            """,
            [report_id, report_id, report_id],
        ).fetchall()
    finally:
        conn.close()
    return [[metric, value] for metric, value in rows]


def render(
    report_path: Path,
    oracle_path: Path,
    *,
    xhtml_db: Path | None = None,
    xhtml_report_id: str = "upstream-xhtml-full",
) -> str:
    report = json.loads(report_path.read_text())
    rows = report.get("rows", [])
    cases = load_cases(oracle_path)

    by_adapter: dict[str, list[dict[str, Any]]] = defaultdict(list)
    for row in rows:
        by_adapter[row["adapter"]].append(row)

    lines: list[str] = [
        "# Cross-Adapter AAT Oracle Summary",
        "",
        f"Report JSON: `{report_path}`",
        f"Oracle cases: `{oracle_path}`",
        "",
        "## Adapter Totals",
        "",
    ]

    totals = []
    for adapter in sorted(by_adapter):
        adapter_rows = by_adapter[adapter]
        totals.append(
            [
                adapter,
                len(adapter_rows),
                status_counts(adapter_rows, "schema_status")["pass"],
                status_counts(adapter_rows, "upstream_status")["faithful"],
                status_counts(adapter_rows, "oracle_status")["pass"],
                status_counts(adapter_rows, "oracle_status")["fail"],
            ]
        )
    lines.extend(
        md_table(
            ["adapter", "cases", "schema pass", "upstream faithful", "oracle pass", "oracle fail"],
            totals,
        )
    )

    lines.extend(["", "## Failure Buckets", ""])
    bucket_rows = []
    for adapter in sorted(by_adapter):
        counts = Counter(failure_bucket(row) for row in by_adapter[adapter])
        for bucket, count in sorted(counts.items()):
            if bucket != "passing":
                bucket_rows.append([adapter, bucket, count])
    lines.extend(md_table(["adapter", "bucket", "count"], bucket_rows))

    lines.extend(["", "## Failure Families", ""])
    family_rows = []
    for adapter in sorted(by_adapter):
        counts = Counter(
            case_family(row["case_id"])
            for row in by_adapter[adapter]
            if row.get("oracle_status") == "fail"
        )
        for family, count in sorted(counts.items()):
            family_rows.append([adapter, family, count])
    lines.extend(md_table(["adapter", "case family", "failures"], family_rows))

    if xhtml_db is not None:
        lines.extend(["", "## XHTML Source Evidence", ""])
        lines.append(f"XHTML DuckDB: `{xhtml_db}`")
        lines.append(f"XHTML report id: `{xhtml_report_id}`")
        lines.append("")
        lines.extend(md_table(["metric", "value"], xhtml_evidence_rows(xhtml_db, xhtml_report_id)))
        lines.extend(
            [
                "",
                "- `aozora2html` source-level oracle failures should be interpreted beside rendered-XHTML evidence: `raw_equal` and `main_text_equal` rows support rendered-body proxy claims, while `main_text_mismatch`, adapter errors, and missing-main-text rows require separate triage.",
            ]
        )

    lines.extend(["", "## Syntax Row Coverage", ""])
    syntax_rows: dict[str, dict[str, Counter[str]]] = defaultdict(lambda: defaultdict(Counter))
    for row in rows:
        case = cases.get(row["case_id"], {})
        for syntax_row_id in case.get("syntax_row_ids", []):
            syntax_rows[syntax_row_id][row["adapter"]][row["oracle_status"]] += 1

    coverage_rows = []
    for syntax_row_id in sorted(syntax_rows):
        for adapter in sorted(syntax_rows[syntax_row_id]):
            counts = syntax_rows[syntax_row_id][adapter]
            coverage_rows.append(
                [
                    syntax_row_id,
                    adapter,
                    counts["pass"],
                    counts["fail"],
                ]
            )
    lines.extend(md_table(["syntax row", "adapter", "pass", "fail"], coverage_rows))

    lines.extend(["", "## Triage Notes", ""])
    for adapter in sorted(by_adapter):
        adapter_rows = by_adapter[adapter]
        schema_fail = status_counts(adapter_rows, "schema_status")["fail"]
        upstream_nonfaithful = sum(
            1 for row in adapter_rows if row.get("upstream_status") != "faithful"
        )
        oracle_fail = status_counts(adapter_rows, "oracle_status")["fail"]
        if schema_fail == 0 and upstream_nonfaithful == 0 and oracle_fail == 0:
            lines.append(
                f"- `{adapter}` passes schema, upstream-observation, and oracle axes for all reviewed cases."
            )
        elif schema_fail == 0 and upstream_nonfaithful == 0 and oracle_fail:
            lines.append(
                f"- `{adapter}` is fully observed but has {oracle_fail} oracle divergence(s); failures are now classified as faithful-output vs oracle-correctness work."
            )
        else:
            lines.append(
                f"- `{adapter}` has {schema_fail} schema failure(s), {upstream_nonfaithful} upstream-observation gap(s), and {oracle_fail} oracle failure(s)."
            )
    lines.append(
        "- Next implementation work should target one remaining adapter/family at a time, using these observations as the pre-fix upstream contract."
    )
    return "\n".join(lines) + "\n"


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--report", type=Path, required=True)
    parser.add_argument("--oracle", type=Path, default=Path("data/aat-oracle-cases.toml"))
    parser.add_argument("--xhtml-db", type=Path)
    parser.add_argument("--xhtml-report-id", default="upstream-xhtml-full")
    args = parser.parse_args()
    print(
        render(
            args.report,
            args.oracle,
            xhtml_db=args.xhtml_db,
            xhtml_report_id=args.xhtml_report_id,
        ),
        end="",
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
