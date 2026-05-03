#!/usr/bin/env python3
"""Render a compact cross-adapter AAT oracle summary."""

from __future__ import annotations

import argparse
import json
import tomllib
from collections import Counter, defaultdict
from pathlib import Path
from typing import Any


def load_cases(path: Path) -> dict[str, dict[str, Any]]:
    data = tomllib.loads(path.read_text())
    return {case["id"]: case for case in data.get("case", [])}


def status_counts(rows: list[dict[str, Any]], key: str) -> Counter[str]:
    return Counter(str(row.get(key, "")) for row in rows)


def failure_bucket(row: dict[str, Any]) -> str:
    if row.get("schema_status") != "pass":
        return "schema"
    if row.get("upstream_status") == "faithful" and row.get("oracle_status") == "fail":
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


def render(report_path: Path, oracle_path: Path) -> str:
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

    lines.extend(
        [
            "",
            "## Triage Notes",
            "",
            "- `aozora2` is the current oracle baseline for the reviewed AAT cases.",
            "- `aozora-rs` has one known faithful-upstream oracle failure: `gaiji.jis.2-13-47` remains unresolved upstream.",
            "- `aozora-rs` needs more upstream observations before the remaining oracle failures can be cleanly split between faithful upstream behavior and adapter projection gaps.",
            "- `aozora2html` is indirect: it is faithful to rendered XHTML. Many source-level assertions require reconstructing Aozora markers from XHTML notes or accepting that the rendered form discarded the source distinction.",
        ]
    )
    return "\n".join(lines) + "\n"


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--report", type=Path, required=True)
    parser.add_argument("--oracle", type=Path, default=Path("data/aat-oracle-cases.toml"))
    args = parser.parse_args()
    print(render(args.report, args.oracle), end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
