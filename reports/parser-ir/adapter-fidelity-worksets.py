#!/usr/bin/env python3
"""Generate adapter fidelity worksets from the TEI-EAJ matrix."""

from __future__ import annotations

import argparse
import hashlib
import json
import pathlib
from typing import Any

SCHEMA_VERSION = "adapter-fidelity-worksets-v1"
REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
INCLUDED_BUCKETS = (
    "adapter_over_segmented",
    "adapter_collapsed",
    "adapter_under_segmented",
    "converter_paragraph_mismatch",
    "adapter_raw_only",
)
EXCLUDED_BUCKETS = ("aligned", "source_note_back_routing", "page_break_projection")


def load_json(path: pathlib.Path) -> dict[str, Any]:
    with path.open(encoding="utf-8") as handle:
        value = json.load(handle)
    if not isinstance(value, dict):
        raise SystemExit(f"{path} must contain a JSON object")
    return value


def sha256_file(path: pathlib.Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return f"sha256:{digest.hexdigest()}"


def display_path(path: pathlib.Path) -> str:
    try:
        return str(path.resolve().relative_to(REPO_ROOT))
    except ValueError:
        return str(path)


def as_int(value: Any) -> int:
    if value is None:
        return 0
    if isinstance(value, bool):
        return int(value)
    if isinstance(value, int):
        return value
    if isinstance(value, float):
        return int(value)
    raise SystemExit(f"expected numeric value, got {value!r}")


def paragraph_origin(row: dict[str, Any]) -> str:
    classification = row.get("classification", {})
    if not isinstance(classification, dict):
        return ""
    return str(classification.get("paragraph_origin_bucket") or "")


def adapter(row: dict[str, Any]) -> str | None:
    selected = row.get("selected_aat")
    if isinstance(selected, dict) and selected.get("adapter"):
        return str(selected["adapter"])
    value = row.get("adapter")
    return str(value) if value is not None else None


def body_text_relation(row: dict[str, Any]) -> str | None:
    text = row.get("text")
    if not isinstance(text, dict):
        return None
    value = text.get("body_base_text_relation") or text.get("body_text_match_bucket")
    return str(value) if value is not None else None


def workset_row(row: dict[str, Any]) -> dict[str, Any]:
    parser_ir = row.get("parser_ir") if isinstance(row.get("parser_ir"), dict) else {}
    return {
        "work_id": row.get("work_id"),
        "adapter": adapter(row),
        "tei_eaj_file": row.get("tei_eaj_file"),
        "paragraph_origin_bucket": paragraph_origin(row),
        "body_text_relation": body_text_relation(row),
        "parser_ir_nodes": as_int(parser_ir.get("nodes")),
        "paragraph_count": as_int(parser_ir.get("paragraph_count")),
        "source_note_count": as_int(parser_ir.get("source_note_count")),
    }


def build_summary(args: argparse.Namespace) -> dict[str, Any]:
    matrix = load_json(args.matrix_summary)
    rows = matrix.get("rows")
    if not isinstance(rows, list):
        rows = []
    worksets = {bucket: {"rows": [], "count": 0} for bucket in INCLUDED_BUCKETS}
    excluded_counts = {bucket: 0 for bucket in EXCLUDED_BUCKETS}
    unknown_bucket_counts: dict[str, int] = {}
    for raw_row in rows:
        if not isinstance(raw_row, dict):
            continue
        bucket = paragraph_origin(raw_row)
        if bucket in worksets:
            worksets[bucket]["rows"].append(workset_row(raw_row))
            worksets[bucket]["count"] += 1
        elif bucket in excluded_counts:
            excluded_counts[bucket] += 1
        elif bucket:
            unknown_bucket_counts[bucket] = unknown_bucket_counts.get(bucket, 0) + 1
    return {
        "schema_version": SCHEMA_VERSION,
        "matrix_summary": {"path": display_path(args.matrix_summary), "hash": sha256_file(args.matrix_summary)},
        "included_buckets": list(INCLUDED_BUCKETS),
        "excluded_buckets": list(EXCLUDED_BUCKETS),
        "worksets": worksets,
        "excluded_counts": excluded_counts,
        "unknown_bucket_counts": unknown_bucket_counts,
    }


def render_markdown(summary: dict[str, Any]) -> str:
    lines = [
        "# Adapter Fidelity Worksets",
        "",
        "| bucket | rows |",
        "| --- | ---: |",
    ]
    for bucket, workset in summary["worksets"].items():
        lines.append(f"| `{bucket}` | {workset['count']} |")
    lines.extend(["", "Excluded buckets: " + ", ".join(f"`{bucket}`" for bucket in summary["excluded_buckets"]), ""])
    return "\n".join(lines)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--matrix-summary", type=pathlib.Path, required=True)
    parser.add_argument("--summary-json", type=pathlib.Path, required=True)
    parser.add_argument("--report-md", type=pathlib.Path, required=True)
    args = parser.parse_args()
    summary = build_summary(args)
    args.summary_json.parent.mkdir(parents=True, exist_ok=True)
    args.report_md.parent.mkdir(parents=True, exist_ok=True)
    args.summary_json.write_text(json.dumps(summary, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    args.report_md.write_text(render_markdown(summary), encoding="utf-8")


if __name__ == "__main__":
    main()
