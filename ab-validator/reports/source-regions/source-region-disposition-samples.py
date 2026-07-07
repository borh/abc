#!/usr/bin/env python
"""Report source-region publication disposition samples."""

from __future__ import annotations

import argparse
import pathlib
import sys
from typing import Any

_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(_REPO_ROOT))

from reports.lib.evidence import as_int, input_record, read_json_object
from reports.lib.io import write_json

SCHEMA_VERSION = "source-region-disposition-samples-v1"
VERDICT = "SOURCE_REGION_DISPOSITION_SAMPLES_READY"
LETTER_CLASS = "letter_address_origin"
SOURCE_COUNTERS = {
    "notation_legend": "source_apparatus_occurrences",
    "notation_placeholder": "front_matter_occurrences",
    "body_end_boundary": "body_end_boundary_occurrences",
    "terminal_provenance": "terminal_provenance_occurrences",
    "colophon_metadata": "colophon_metadata_occurrences",
    LETTER_CLASS: "letter_address_origin_occurrences",
    "malformed_source": "malformed_source_occurrences",
}


def policy_dispositions(policy: dict[str, Any]) -> list[dict[str, Any]]:
    dispositions = policy.get("dispositions")
    if not isinstance(dispositions, list):
        raise SystemExit("policy missing dispositions array")
    rows: list[dict[str, Any]] = []
    for row in dispositions:
        if not isinstance(row, dict):
            raise SystemExit("policy disposition rows must be objects")
        rows.append(row)
    return rows


def status_for_policy_row(row: dict[str, Any]) -> str:
    if row.get("target_class") == "unsupported_gap":
        return "policy_needed"
    return "admitted"


def class_row(row: dict[str, Any], counters: dict[str, Any], present: bool) -> dict[str, Any]:
    source_class = str(row["source_class"])
    counter_name = SOURCE_COUNTERS.get(source_class)
    measured = None if counter_name is None else as_int(counters.get(counter_name))
    return {
        "source_class": source_class,
        "status": status_for_policy_row(row) if present else "policy_needed",
        "policy_class_present": present,
        "target_class": row.get("target_class"),
        "tei_target": row.get("tei_target"),
        "custom_sidecar": row.get("custom_sidecar"),
        "plaintext_projection": row.get("plaintext_projection"),
        "measurement_status": row.get("measurement_status"),
        "measured_counter": counter_name,
        "measured_occurrences": measured,
    }


def build_summary(args: argparse.Namespace) -> dict[str, Any]:
    source = read_json_object(args.source_summary)
    policy = read_json_object(args.policy)
    counters = source.get("source_region_coverage")
    if not isinstance(counters, dict):
        raise SystemExit("source summary missing source_region_coverage")
    dispositions = policy_dispositions(policy)
    classes = [class_row(row, counters, present=True) for row in dispositions]
    if not any(row.get("source_class") == LETTER_CLASS for row in dispositions):
        classes.append(
            class_row(
                {
                    "source_class": LETTER_CLASS,
                    "target_class": None,
                    "tei_target": None,
                    "custom_sidecar": True,
                    "plaintext_projection": "omit",
                    "measurement_status": "measured",
                },
                counters,
                present=False,
            )
        )
    classes.sort(key=lambda row: row["source_class"])
    return {
        "schema_version": SCHEMA_VERSION,
        "verdict": VERDICT,
        "policy": {
            "policy_id": policy.get("policy_id"),
            "policy_version": policy.get("policy_version"),
            **input_record(args.policy),
        },
        "source_summary": {
            **input_record(args.source_summary),
            "works_scanned": source.get("works_scanned"),
            "gate_status": source.get("gate_status"),
        },
        "source_report_md": input_record(args.source_report_md),
        "classes": classes,
    }


def render_markdown(summary: dict[str, Any]) -> str:
    lines = [
        "# Source-Region Disposition Samples",
        "",
        f"Verdict: `{summary['verdict']}`",
        "",
        "| source_class | status | target_class | tei_target | plaintext | measured |",
        "| --- | --- | --- | --- | --- | --- |",
    ]
    for row in summary["classes"]:
        lines.append(
            "| {source_class} | {status} | {target_class} | {tei_target} | {plaintext_projection} | {measured_occurrences} |".format(
                **{key: "" if value is None else value for key, value in row.items()}
            )
        )
    lines.append("")
    return "\n".join(lines)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source-summary", type=pathlib.Path, required=True)
    parser.add_argument("--source-report-md", type=pathlib.Path, required=True)
    parser.add_argument("--policy", type=pathlib.Path, required=True)
    parser.add_argument("--summary-json", type=pathlib.Path, required=True)
    parser.add_argument("--report-md", type=pathlib.Path, required=True)
    args = parser.parse_args()
    summary = build_summary(args)
    args.summary_json.parent.mkdir(parents=True, exist_ok=True)
    args.report_md.parent.mkdir(parents=True, exist_ok=True)
    write_json(args.summary_json, summary)
    args.report_md.write_text(render_markdown(summary), encoding="utf-8")


if __name__ == "__main__":
    main()
