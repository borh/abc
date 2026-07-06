#!/usr/bin/env python3
"""Classify TEI-EAJ text-difference rows by policy cause."""

from __future__ import annotations

import argparse
import hashlib
import json
import pathlib
from collections import Counter
from typing import Any

SCHEMA_VERSION = "parser-ir-text-policy-delta-v1"
REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
CAUSES = (
    "ruby_or_parenthetical_policy",
    "front_back_source_region_policy",
    "body_visible_layout_policy",
    "adapter_text_loss",
    "tei_eaj_editorial_or_enrichment",
    "unknown_text_delta",
)
ADAPTER_BUCKETS = {
    "adapter_over_segmented",
    "adapter_collapsed",
    "adapter_under_segmented",
    "converter_paragraph_mismatch",
    "adapter_raw_only",
}
LAYOUT_PROFILES = {"lineated_text", "verse"}
FRONT_BACK_PROFILES = {"front_back_matter"}
EDITORIAL_PROFILES = {"lv4_enrichment"}
SOURCE_MARKUP_BACKED_CAUSES = {
    "ruby_or_parenthetical_policy",
    "front_back_source_region_policy",
    "body_visible_layout_policy",
    "adapter_text_loss",
    "unknown_text_delta",
}


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


def row_id(row: dict[str, Any], index: int) -> str:
    work_id = row.get("work_id", "unknown-work")
    adapter = row.get("adapter") or row.get("selected_aat", {}).get("adapter") or "unknown-adapter"
    tei_file = pathlib.Path(str(row.get("tei_eaj_file", ""))).name
    parts = [str(work_id), str(adapter)]
    if tei_file:
        parts.append(tei_file)
    parts.append(str(index))
    return "::".join(parts)


def relation_at(row: dict[str, Any], key: str) -> str | None:
    surface = row.get("text", {}).get("surface_relations", {})
    if not isinstance(surface, dict):
        return None
    value = surface.get(key)
    if not isinstance(value, dict):
        return None
    relation = value.get("relation")
    return str(relation) if relation is not None else None


def profiles(row: dict[str, Any]) -> set[str]:
    tei_eaj = row.get("tei_eaj", {})
    values = tei_eaj.get("structure_profiles")
    if isinstance(values, list):
        return {str(value) for value in values}
    value = tei_eaj.get("structure_profile")
    return {str(value)} if value else set()


def body_text_relation(row: dict[str, Any]) -> str:
    text = row.get("text", {})
    return str(text.get("body_base_text_relation") or text.get("body_text_match_bucket") or "")


def paragraph_origin(row: dict[str, Any]) -> str:
    classification = row.get("classification", {})
    return str(classification.get("paragraph_origin_bucket") or "")


def classify_different(row: dict[str, Any]) -> str:
    row_profiles = profiles(row)
    classification = row.get("classification", {})
    origin = paragraph_origin(row)
    if row_profiles & EDITORIAL_PROFILES:
        return "tei_eaj_editorial_or_enrichment"
    if relation_at(row, "ruby_expanded_parenless") == "equal" or relation_at(row, "base_drop_parentheticals") == "equal":
        return "ruby_or_parenthetical_policy"
    if classification.get("source_note_body_excluded") or origin == "source_note_back_routing" or row_profiles & FRONT_BACK_PROFILES:
        return "front_back_source_region_policy"
    if origin == "page_break_projection" or row_profiles & LAYOUT_PROFILES:
        return "body_visible_layout_policy"
    if origin in ADAPTER_BUCKETS:
        return "adapter_text_loss"
    return "unknown_text_delta"


def compact_row(row: dict[str, Any], index: int, cause: str | None = None) -> dict[str, Any]:
    result = {
        "row_id": row_id(row, index),
        "work_id": row.get("work_id"),
        "adapter": row.get("adapter") or row.get("selected_aat", {}).get("adapter"),
        "tei_eaj_file": row.get("tei_eaj_file"),
        "body_text_relation": body_text_relation(row),
        "paragraph_origin_bucket": paragraph_origin(row),
        "structure_profiles": sorted(profiles(row)),
    }
    if cause is not None:
        result["cause"] = cause
    return result


def build_summary(args: argparse.Namespace) -> dict[str, Any]:
    matrix = load_json(args.matrix_summary)
    rows = matrix.get("rows")
    if not isinstance(rows, list):
        rows = []
    counts = Counter({cause: 0 for cause in CAUSES})
    examples: dict[str, list[dict[str, Any]]] = {cause: [] for cause in CAUSES}
    source_blockers: list[dict[str, Any]] = []
    calibration_only: list[dict[str, Any]] = []
    already_equal = 0
    total_different = 0

    for index, raw_row in enumerate(rows):
        if not isinstance(raw_row, dict):
            continue
        relation = body_text_relation(raw_row)
        if relation == "different":
            cause = classify_different(raw_row)
            counts[cause] += 1
            total_different += 1
            compact = compact_row(raw_row, index, cause)
            if len(examples[cause]) < 10:
                examples[cause].append(compact)
            if cause in SOURCE_MARKUP_BACKED_CAUSES:
                source_blockers.append(compact)
        elif relation == "equal":
            already_equal += 1
        elif relation:
            calibration_only.append(compact_row(raw_row, index))

    bucket_different = matrix.get("body_text_relation_buckets", {}).get("different")
    return {
        "schema_version": SCHEMA_VERSION,
        "matrix_summary": {"path": display_path(args.matrix_summary), "hash": sha256_file(args.matrix_summary)},
        "total_different_rows": total_different,
        "matrix_bucket_different_rows": bucket_different,
        "counts_by_cause": dict(counts),
        "source_markup_backed_blockers": source_blockers,
        "calibration_only_rows": calibration_only,
        "already_equal_rows": already_equal,
        "examples_by_cause": examples,
    }


def render_markdown(summary: dict[str, Any]) -> str:
    lines = [
        "# Text Policy Delta",
        "",
        f"Total different rows: {summary['total_different_rows']}",
        "",
        "| cause | rows |",
        "| --- | ---: |",
    ]
    for cause, count in summary["counts_by_cause"].items():
        lines.append(f"| `{cause}` | {count} |")
    lines.extend(["", f"Calibration-only rows: {len(summary['calibration_only_rows'])}", ""])
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
