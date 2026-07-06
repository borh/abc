#!/usr/bin/env python3
"""Classify aozora2html parse_incomplete AATs by failure mode.

Deterministic regex classification of meta.warnings[0].message for AAT files
whose meta.parse_complete == false. Classes: ruby_structural, invalid_xhtml,
ruby_internal_error, other.
"""
from __future__ import annotations

import argparse
import glob
import json
import re
import sys
from collections import defaultdict
from pathlib import Path

CLASSES = ("ruby_structural", "invalid_xhtml", "ruby_internal_error", "other")
RUBY_STRUCTURAL_RE = re.compile(r"エラー\([^)]*行目\):")


def classify(message: str) -> str:
    if "invalid XHTML:" in message:
        return "invalid_xhtml"
    if "NoMethodError" in message or "private method" in message:
        return "ruby_internal_error"
    if RUBY_STRUCTURAL_RE.search(message):
        return "ruby_structural"
    return "other"


def markdown_cell(message: str, limit: int = 240) -> str:
    clean = re.sub(r"\s+", " ", message.strip())
    if len(clean) > limit:
        clean = clean[: limit - 3].rstrip() + "..."
    return clean.replace("|", "\\|")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("aat_dir")
    parser.add_argument("--report-md", required=True, type=Path)
    args = parser.parse_args()

    buckets: dict[str, list[str]] = defaultdict(list)
    examples: dict[str, str] = {}
    for path in sorted(glob.glob(f"{args.aat_dir}/*.json")):
        with open(path, encoding="utf-8") as f:
            data = json.load(f)
        if data.get("meta", {}).get("parse_complete", True):
            continue
        warnings = data.get("meta", {}).get("warnings") or []
        msg = warnings[0].get("message", "") if warnings else ""
        cls = classify(msg)
        wid = data.get("work_id", "")
        buckets[cls].append(wid)
        examples.setdefault(cls, msg.strip())

    lines = [
        "# aozora2html parse_incomplete Classification Report",
        "",
        "Date: 2026-07-03",
        f"Source AAT dir: `{'/'.join(args.aat_dir.split('/')[-3:])}`",
        "",
        "## Summary",
        "",
        "| Class | Reports | Example message |",
        "|---|---:|---|",
    ]
    for cls in CLASSES:
        wids = buckets.get(cls, [])
        ex = markdown_cell(examples.get(cls, "") or "")
        lines.append(f"| {cls} | {len(wids)} | {ex} |")
    lines.append(f"| **Total** | **{sum(len(v) for v in buckets.values())}** | |")
    lines.append("")
    lines.append("## Remediation scope (deferred)")
    lines.append("")
    lines.append(
        "- `ruby_structural`: upstream aozora2html parser rejects edge-case markup; "
        "investigate wrapper pre-normalization (e.g., CRLF) as a follow-up, not here."
    )
    lines.append(
        "- `invalid_xhtml`: mapper strictness; a `roxmltree` -> `html5ever` migration "
        "is a separate plan, not this one."
    )
    lines.append(
        "- `ruby_internal_error`: upstream aozora2html 3.0.1 bugs; file upstream reports only."
    )
    lines.append("- `other`: inspect the example message before routing.")
    args.report_md.parent.mkdir(parents=True, exist_ok=True)
    args.report_md.write_text("\n".join(lines) + "\n", encoding="utf-8")

    print(json.dumps({cls: len(buckets.get(cls, [])) for cls in CLASSES}, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
