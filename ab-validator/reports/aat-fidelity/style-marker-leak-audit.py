#!/usr/bin/env python3
"""Audit Aozora marker text surviving inside AAT style nodes."""

from __future__ import annotations

import argparse
import json
import re
import tempfile
from collections import Counter, defaultdict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

MARKER_RE = re.compile(r"［＃[^］]*］")
SCHEMA = "soranoha.aat-style-marker-leak-audit.v1"
EXCERPT_LIMIT = 160


def marker_occurrences(text: str) -> tuple[list[str], int]:
    matches = list(MARKER_RE.finditer(text))
    complete_starts = {match.start() for match in matches}
    opener_starts: list[int] = []
    offset = 0
    while True:
        found = text.find("［＃", offset)
        if found < 0:
            break
        opener_starts.append(found)
        offset = found + 2
    return [match.group(0) for match in matches], sum(
        start not in complete_starts for start in opener_starts
    )


def marker_signature(markers: list[str]) -> str:
    if len(markers) == 1:
        return markers[0]
    if len(markers) == 2:
        return f"{markers[0]}…{markers[1]}"
    return " | ".join(markers)


def excerpt(text: str) -> str:
    normalized = text.replace("\r", "\\r").replace("\n", "\\n")
    if len(normalized) <= EXCERPT_LIMIT:
        return normalized
    return normalized[: EXCERPT_LIMIT - 1] + "…"


def table_cell(text: str) -> str:
    return text.replace("|", "\\|")


@dataclass
class FileAudit:
    file_stem: str
    style_nodes_inspected: int = 0
    affected_style_paths: set[str] = field(default_factory=set)
    affected_text_nodes: int = 0
    marker_occurrences: int = 0
    unmatched_marker_open: int = 0
    by_style_type: Counter[str] = field(default_factory=Counter)
    by_signature: Counter[str] = field(default_factory=Counter)
    candidates: list[dict[str, Any]] = field(default_factory=list)
    unmatched_candidates: list[dict[str, Any]] = field(default_factory=list)
    malformed: list[str] = field(default_factory=list)


def pointer(path: tuple[str | int, ...]) -> str:
    if not path:
        return ""
    return "/" + "/".join(str(part).replace("~", "~0").replace("/", "~1") for part in path)


def audit_value(
    value: Any,
    path: tuple[str | int, ...],
    owner: tuple[str, str] | None,
    audit: FileAudit,
) -> None:
    if isinstance(value, list):
        for index, item in enumerate(value):
            audit_value(item, path + (index,), owner, audit)
        return
    if not isinstance(value, dict):
        return

    kind = value.get("kind")
    if kind == "style":
        audit.style_nodes_inspected += 1
        style_type = value.get("style_type")
        if not isinstance(style_type, str) or not style_type:
            style_type = "<missing>"
        owner = (pointer(path), style_type)

    if kind == "text" and owner is not None:
        if "value" not in value or not isinstance(value["value"], str):
            audit.malformed.append(f"{pointer(path)}: style text value must be a string")
            return
        text_value = value["value"]
        markers, partial = marker_occurrences(text_value)
        style_path, style_type = owner
        if markers:
            signature = marker_signature(markers)
            audit.affected_style_paths.add(style_path)
            audit.affected_text_nodes += 1
            audit.marker_occurrences += len(markers)
            audit.by_style_type[style_type] += len(markers)
            audit.by_signature[signature] += 1
            audit.candidates.append(
                {
                    "file_stem": audit.file_stem,
                    "style_type": style_type,
                    "json_path": pointer(path),
                    "marker_forms": markers,
                    "signature": signature,
                    "excerpt": excerpt(text_value),
                }
            )
        if partial:
            audit.unmatched_marker_open += partial
            audit.unmatched_candidates.append(
                {
                    "file_stem": audit.file_stem,
                    "style_type": style_type,
                    "json_path": pointer(path),
                    "count": partial,
                    "excerpt": excerpt(text_value),
                }
            )
        return

    for key in sorted(value):
        audit_value(value[key], path + (key,), owner, audit)


def select_examples(candidates: list[dict[str, Any]], limit: int) -> list[dict[str, Any]]:
    grouped: dict[str, list[dict[str, Any]]] = defaultdict(list)
    for candidate in candidates:
        grouped[candidate["style_type"]].append(candidate)
    for rows in grouped.values():
        rows.sort(key=lambda row: (row["file_stem"], row["json_path"], row.get("signature", "")))

    selected: list[dict[str, Any]] = []
    per_file: Counter[str] = Counter()
    style_types = sorted(grouped)
    while len(selected) < limit:
        progressed = False
        for style_type in style_types:
            rows = grouped[style_type]
            while rows and per_file[rows[0]["file_stem"]] >= 2:
                rows.pop(0)
            if not rows:
                continue
            row = rows.pop(0)
            selected.append(row)
            per_file[row["file_stem"]] += 1
            progressed = True
            if len(selected) == limit:
                break
        if not progressed:
            break
    return selected


def audit_directory(input_dir: Path, input_label: str, example_limit: int) -> dict[str, Any]:
    totals: Counter[str] = Counter()
    by_style_type: Counter[str] = Counter()
    by_signature: Counter[str] = Counter()
    candidates: list[dict[str, Any]] = []
    unmatched_candidates: list[dict[str, Any]] = []
    malformed_inputs: list[dict[str, Any]] = []

    for path in sorted(input_dir.glob("*.json"), key=lambda item: item.name):
        totals["files_scanned"] += 1
        audit = FileAudit(path.stem)
        try:
            document = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, UnicodeError, json.JSONDecodeError) as error:
            audit.malformed.append(f"JSON read failed: {type(error).__name__}")
            document = None

        if document is not None:
            blocks = document.get("blocks") if isinstance(document, dict) else None
            if not isinstance(blocks, list):
                audit.malformed.append("/blocks: expected an array")
            else:
                audit_value(blocks, ("blocks",), None, audit)

        totals["style_nodes_inspected"] += audit.style_nodes_inspected
        totals["affected_style_nodes"] += len(audit.affected_style_paths)
        totals["affected_text_nodes"] += audit.affected_text_nodes
        totals["marker_occurrences"] += audit.marker_occurrences
        totals["unmatched_marker_open"] += audit.unmatched_marker_open
        by_style_type.update(audit.by_style_type)
        by_signature.update(audit.by_signature)
        candidates.extend(audit.candidates)
        unmatched_candidates.extend(audit.unmatched_candidates)
        if audit.affected_style_paths:
            totals["affected_files"] += 1
        if audit.malformed:
            totals["malformed_files"] += 1
            malformed_inputs.append(
                {"file_stem": audit.file_stem, "errors": sorted(set(audit.malformed))}
            )

    for key in [
        "files_scanned",
        "malformed_files",
        "affected_files",
        "style_nodes_inspected",
        "affected_style_nodes",
        "affected_text_nodes",
        "marker_occurrences",
        "unmatched_marker_open",
    ]:
        totals.setdefault(key, 0)

    return {
        "schema": SCHEMA,
        "input_label": input_label,
        "totals": dict(sorted(totals.items())),
        "by_style_type": dict(sorted(by_style_type.items())),
        "by_signature": dict(sorted(by_signature.items())),
        "examples": select_examples(candidates, example_limit),
        "unmatched_examples": select_examples(unmatched_candidates, example_limit),
        "malformed_inputs": sorted(malformed_inputs, key=lambda row: row["file_stem"]),
        "limitations": [
            "Complete markers are shortest non-overlapping matches within one text value.",
            "Markers split across text nodes are not reconstructed or counted.",
        ],
    }


def markdown(summary: dict[str, Any]) -> str:
    totals = summary["totals"]
    lines = [
        "# AAT style marker leak audit",
        "",
        f"- input: `{summary['input_label']}`",
        f"- files scanned: {totals['files_scanned']:,}",
        f"- affected files: {totals['affected_files']:,}",
        f"- affected style nodes: {totals['affected_style_nodes']:,}",
        f"- affected text nodes: {totals['affected_text_nodes']:,}",
        f"- complete marker occurrences: {totals['marker_occurrences']:,}",
        f"- unmatched marker opens: {totals['unmatched_marker_open']:,}",
        f"- malformed files: {totals['malformed_files']:,}",
        "",
        "## Counts by style type",
        "",
        "| style type | marker occurrences |",
        "|---|---:|",
    ]
    lines.extend(f"| `{key}` | {value:,} |" for key, value in summary["by_style_type"].items())
    lines.extend(["", "## Counts by signature", "", "| signature | text nodes |", "|---|---:|"])
    lines.extend(
        f"| `{table_cell(key)}` | {value:,} |" for key, value in summary["by_signature"].items()
    )
    lines.extend(["", "## Examples", ""])
    for row in summary["examples"]:
        lines.append(
            f"- `{row['file_stem']}` `{row['style_type']}` `{row['json_path']}`: "
            f"`{row['signature']}` — {row['excerpt']}"
        )
    if not summary["examples"]:
        lines.append("- none")
    lines.extend(["", "## Limitations", ""])
    lines.extend(f"- {item}" for item in summary["limitations"])
    lines.append("")
    return "\n".join(lines)


def atomic_write(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile("w", encoding="utf-8", dir=path.parent, delete=False) as out:
        out.write(content)
        temporary = Path(out.name)
    temporary.replace(path)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("aat_dir", type=Path)
    parser.add_argument("--summary-json", type=Path, required=True)
    parser.add_argument("--report-md", type=Path, required=True)
    parser.add_argument("--example-limit", type=int, default=30)
    parser.add_argument("--input-label", required=True)
    args = parser.parse_args()
    if args.example_limit < 0:
        parser.error("--example-limit must be nonnegative")
    return args


def main() -> int:
    args = parse_args()
    summary = audit_directory(args.aat_dir, args.input_label, args.example_limit)
    atomic_write(
        args.summary_json,
        json.dumps(summary, ensure_ascii=False, indent=2, sort_keys=True) + "\n",
    )
    atomic_write(args.report_md, markdown(summary))
    return 1 if summary["totals"]["malformed_files"] else 0


if __name__ == "__main__":
    raise SystemExit(main())
