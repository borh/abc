#!/usr/bin/env python
"""Classify local aozora2html adapter-abort payloads in XHTML observations."""

from __future__ import annotations

import argparse
import csv
import json
import re
from collections import Counter, defaultdict
from pathlib import Path
from typing import Any

import duckdb


def first_warning_message(path: Path) -> str:
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as exc:
        return f"payload_read_failed: {exc}"
    if not isinstance(payload, dict):
        return "payload_not_object"
    meta = payload.get("meta")
    if not isinstance(meta, dict):
        return "payload_missing_meta"
    warnings = meta.get("warnings")
    if not isinstance(warnings, list) or not warnings:
        return "payload_missing_warning"
    first = warnings[0]
    if not isinstance(first, dict):
        return "payload_warning_not_object"
    return str(first.get("message", ""))


def classify_message(message: str) -> str:
    if "字下げを閉じようとしましたが" in message:
        return "jisage_close_without_open"
    if "undefined method" in message and "close_tag" in message:
        return "ruby_close_tag_no_method"
    if "同じ箇所に2つのルビ" in message:
        return "double_ruby_forbidden"
    if "改行コードを" in message:
        return "crlf_required"
    if "author twice" in message:
        return "duplicate_author"
    if "字詰め中に本文が終了" in message:
        return "jizume_unclosed_at_eof"
    if "字下げ中に本文が終了" in message:
        return "jisage_unclosed_at_eof"
    if "burasage" in message and "本文が終了" in message:
        return "burasage_unclosed_at_eof"
    if "NoMethodError" in message:
        return "ruby_no_method_error"
    if "parser aborted" in message:
        return "other_parser_abort"
    return "unclassified_adapter_error"


def normalized_snippet(message: str, limit: int = 240) -> str:
    return re.sub(r"\s+", " ", message).strip()[:limit]


def load_rows(db_path: Path, report_id: str) -> list[dict[str, Any]]:
    conn = duckdb.connect(str(db_path), read_only=True)
    try:
        rows = conn.execute(
            """
            SELECT case_id, local_xhtml_path, source_url, card_url, feature_tags
            FROM fidelity_xhtml_observations
            WHERE report_id = ?
              AND comparison_status = 'local_adapter_error'
            ORDER BY case_id
            """,
            [report_id],
        ).fetchall()
    finally:
        conn.close()
    return [
        {
            "case_id": case_id,
            "local_xhtml_path": local_xhtml_path,
            "source_url": source_url,
            "card_url": card_url,
            "feature_tags": feature_tags,
        }
        for case_id, local_xhtml_path, source_url, card_url, feature_tags in rows
    ]


def classify_rows(rows: list[dict[str, Any]]) -> list[dict[str, str]]:
    classified = []
    for row in rows:
        path = Path(str(row["local_xhtml_path"]))
        message = first_warning_message(path)
        error_class = classify_message(message)
        classified.append(
            {
                "case_id": str(row["case_id"]),
                "error_class": error_class,
                "message_snippet": normalized_snippet(message),
                "local_xhtml_path": str(row["local_xhtml_path"]),
                "source_url": str(row.get("source_url", "")),
                "card_url": str(row.get("card_url", "")),
                "feature_tags": str(row.get("feature_tags", "")),
            }
        )
    return classified


def write_csvs(out_dir: Path, classified: list[dict[str, str]]) -> None:
    out_dir.mkdir(parents=True, exist_ok=True)
    counts = Counter(row["error_class"] for row in classified)

    with (out_dir / "adapter-error-classes.csv").open("w", encoding="utf-8", newline="") as f:
        writer = csv.writer(f, lineterminator="\n")
        writer.writerow(["error_class", "row_count"])
        for error_class, row_count in sorted(counts.items(), key=lambda item: (-item[1], item[0])):
            writer.writerow([error_class, row_count])

    examples_by_class: dict[str, list[dict[str, str]]] = defaultdict(list)
    for row in classified:
        examples_by_class[row["error_class"]].append(row)

    with (out_dir / "adapter-error-examples.csv").open("w", encoding="utf-8", newline="") as f:
        fieldnames = [
            "case_id",
            "error_class",
            "message_snippet",
            "local_xhtml_path",
            "source_url",
            "card_url",
            "feature_tags",
        ]
        writer = csv.DictWriter(f, fieldnames=fieldnames, lineterminator="\n")
        writer.writeheader()
        for error_class in sorted(examples_by_class):
            for row in examples_by_class[error_class][:5]:
                writer.writerow(row)


def write_markdown(out_dir: Path, db_path: Path, report_id: str, classified: list[dict[str, str]]) -> None:
    counts = Counter(row["error_class"] for row in classified)
    lines = [
        "# XHTML Adapter Error Classification",
        "",
        f"- DuckDB: `{db_path}`",
        f"- Report id: `{report_id}`",
        f"- Local adapter errors: `{len(classified)}`",
        "",
        "## Classes",
        "",
        "```csv",
        "error_class,row_count",
    ]
    for error_class, row_count in sorted(counts.items(), key=lambda item: (-item[1], item[0])):
        lines.append(f"{error_class},{row_count}")
    lines.extend(
        [
            "```",
            "",
            "## Outputs",
            "",
            "| Output | Purpose |",
            "| --- | --- |",
            "| [`adapter-error-classes.csv`](adapter-error-classes.csv) | Counts by normalized adapter-abort class. |",
            "| [`adapter-error-examples.csv`](adapter-error-examples.csv) | Up to five example rows per class with message snippets and source links. |",
            "",
        ]
    )
    (out_dir / "index.md").write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--db", type=Path, required=True)
    parser.add_argument("--report-id", default="upstream-xhtml-full")
    parser.add_argument("--out-dir", type=Path, required=True)
    args = parser.parse_args()

    classified = classify_rows(load_rows(args.db, args.report_id))
    write_csvs(args.out_dir, classified)
    write_markdown(args.out_dir, args.db, args.report_id, classified)
    print(f"xhtml_adapter_error_report={args.out_dir / 'index.md'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
