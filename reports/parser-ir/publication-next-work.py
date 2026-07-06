#!/usr/bin/env python3
"""Synthesize the next-work ledger for Aozora publication completion."""

from __future__ import annotations

import argparse
import hashlib
import json
import pathlib
from typing import Any

SCHEMA_VERSION = "aozora-publication-next-work-v1"
VERDICT_OPEN = "AOZORA_PUBLICATION_NEXT_WORK_OPEN"
REQUIRED_PARSERS = ("aozora2html", "aozora-epub3", "aozora-rs", "aozora2", "aozora")
SOURCE_COUNTER_SOURCE = "source_region_coverage"
REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
ADAPTER_DISTORTION_INCLUDED_BUCKETS = (
    "adapter_over_segmented",
    "adapter_collapsed",
    "adapter_under_segmented",
    "converter_paragraph_mismatch",
    "adapter_raw_only",
)
ADAPTER_DISTORTION_EXCLUDED_BUCKETS = (
    "aligned",
    "source_note_back_routing",
    "page_break_projection",
)


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


def document_hash(value: object) -> str:
    encoded = json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":")).encode("utf-8")
    return f"sha256:{hashlib.sha256(encoded).hexdigest()}"


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


def input_record(path: pathlib.Path) -> dict[str, str]:
    return {"path": display_path(path), "hash": sha256_file(path)}


def coverage_input_record(path: pathlib.Path, coverage: dict[str, Any]) -> dict[str, str]:
    stable_coverage = dict(coverage)
    stable_coverage.pop("next_work_dashboard", None)
    return {
        "path": display_path(path),
        "hash": document_hash(stable_coverage),
        "hash_basis": "canonical_json_without_next_work_dashboard",
    }


def completed_gate(gate_id: str, status: str, evidence: dict[str, Any]) -> dict[str, Any]:
    return {"id": gate_id, "status": status, "evidence": evidence}


def source_authority_gate(source: dict[str, Any]) -> dict[str, Any]:
    counters = source.get(SOURCE_COUNTER_SOURCE)
    if not isinstance(counters, dict):
        raise SystemExit(f"source summary missing {SOURCE_COUNTER_SOURCE}")
    unsupported = as_int(counters.get("unsupported_body_markup_occurrences"))
    unknown_region = as_int(counters.get("unknown_region_occurrences"))
    unknown_unreviewed = as_int(counters.get("unknown_unreviewed_occurrences"))
    status = (
        "complete"
        if source.get("gate_status") == "SOURCE_AUTHORITY_GATE_PASS"
        and unsupported == 0
        and unknown_region == 0
        and unknown_unreviewed == 0
        else "open"
    )
    return completed_gate(
        "source_authority",
        status,
        {
            "gate_status": source.get("gate_status"),
            "works_scanned": as_int(source.get("works_scanned")),
            "counter_source": SOURCE_COUNTER_SOURCE,
            "unsupported_body_markup_occurrences": unsupported,
            "unknown_region_occurrences": unknown_region,
            "unknown_unreviewed_occurrences": unknown_unreviewed,
        },
    )


def bundle_rows_failed(bundle: dict[str, Any]) -> int:
    if "rows_failed" in bundle:
        return as_int(bundle.get("rows_failed"))
    if "failures" in bundle:
        failures = bundle.get("failures")
        if isinstance(failures, list):
            return len(failures)
    return 0


def publication_bundle_gate(coverage: dict[str, Any]) -> dict[str, Any]:
    bundle = coverage.get("publication_bundle_contract")
    if not isinstance(bundle, dict):
        bundle = {}
    rows_failed = bundle_rows_failed(bundle)
    rows_validated = as_int(bundle.get("rows_validated"))
    status = "complete" if rows_failed == 0 and rows_validated > 0 else "open"
    return completed_gate(
        "publication_bundle_contract",
        status,
        {
            "verdict": bundle.get("verdict"),
            "rows_validated": rows_validated,
            "rows_failed": rows_failed,
            "path": bundle.get("path"),
        },
    )


def ir_publication_gate(coverage: dict[str, Any]) -> dict[str, Any]:
    status = "complete" if coverage.get("verdict") == "IR_PUBLICATION_COVERAGE_COMPLETE" else "open"
    closure = coverage.get("closure_gaps") if isinstance(coverage.get("closure_gaps"), dict) else {}
    return completed_gate(
        "ir_publication_coverage",
        status,
        {
            "verdict": coverage.get("verdict"),
            "classified_but_not_admitted": closure.get("classified_but_not_admitted", {}).get("count"),
            "true_unsupported_gaps": closure.get("true_unsupported_gaps", {}).get("count"),
        },
    )


def parser_lanes(conversion: dict[str, Any]) -> list[dict[str, Any]]:
    candidates = conversion.get("compatibility_candidates", [])
    lanes: dict[str, dict[str, Any]] = {}
    if isinstance(candidates, list):
        for candidate in candidates:
            if not isinstance(candidate, dict):
                continue
            adapter = candidate.get("aat_adapter") or candidate.get("adapter")
            scope = candidate.get("evidence_scope") if isinstance(candidate.get("evidence_scope"), dict) else {}
            if not adapter and isinstance(scope, dict):
                adapter = scope.get("adapter")
            if not adapter:
                continue
            lanes[str(adapter)] = {
                "adapter": str(adapter),
                "files_scanned": as_int(scope.get("files_scanned")),
                "files_succeeded": as_int(scope.get("files_succeeded")),
                "files_failed": as_int(scope.get("files_failed")),
                "compatibility": candidate.get("compatibility"),
            }
    return [lanes.get(adapter, {"adapter": adapter, "files_scanned": 0, "files_failed": None}) for adapter in REQUIRED_PARSERS]


def five_parser_gate(conversion: dict[str, Any], lanes: list[dict[str, Any]]) -> dict[str, Any]:
    totals = conversion.get("totals") if isinstance(conversion.get("totals"), dict) else {}
    present = sorted(lane["adapter"] for lane in lanes if lane.get("files_scanned", 0) > 0)
    failures = as_int(totals.get("files_failed"))
    status = "complete" if sorted(REQUIRED_PARSERS) == present and failures == 0 else "open"
    return completed_gate(
        "five_parser_conversion",
        status,
        {
            "required_parsers": list(REQUIRED_PARSERS),
            "present_parsers": present,
            "files_attempted": as_int(totals.get("files_attempted")),
            "files_failed": failures,
        },
    )


def source_reference_gate(reference: dict[str, Any]) -> dict[str, Any]:
    totals = reference.get("totals") if isinstance(reference.get("totals"), dict) else reference
    observed_without = as_int(totals.get("observed_without_syntax_row"))
    p4suta_unmapped = as_int(totals.get("p4suta_feature_unmapped"))
    status = (
        "complete"
        if reference.get("verdict") == "SOURCE_REFERENCE_RECONCILIATION_COMPLETE"
        and observed_without == 0
        and p4suta_unmapped == 0
        else "open"
    )
    return completed_gate(
        "source_reference_reconciliation",
        status,
        {
            "verdict": reference.get("verdict"),
            "observed_without_syntax_row": observed_without,
            "p4suta_feature_unmapped": p4suta_unmapped,
        },
    )


def next_work_items(matrix: dict[str, Any]) -> list[dict[str, Any]]:
    text_buckets = matrix.get("body_text_relation_buckets") if isinstance(matrix.get("body_text_relation_buckets"), dict) else {}
    paragraph_buckets = matrix.get("paragraph_origin_buckets") if isinstance(matrix.get("paragraph_origin_buckets"), dict) else {}
    adapter_distortion_rows = sum(as_int(paragraph_buckets.get(bucket)) for bucket in ADAPTER_DISTORTION_INCLUDED_BUCKETS)
    return [
        {
            "id": "source_region_disposition_samples",
            "owner": "ab-validator+abc",
            "status": "open",
            "evidence": {
                "reason": "front/back apparatus classes need disposition samples and policy confirmation",
            },
        },
        {
            "id": "text_policy_calibration",
            "owner": "ab-validator+abc",
            "status": "open",
            "evidence": {
                "different_rows": as_int(text_buckets.get("different")),
                "generated_contains_tei_eaj_rows": as_int(text_buckets.get("generated_contains_tei_eaj")),
                "tei_eaj_contains_generated_rows": as_int(text_buckets.get("tei_eaj_contains_generated")),
            },
        },
        {
            "id": "adapter_fidelity_worksets",
            "owner": "ab-validator",
            "status": "open",
            "evidence": {
                "adapter_distortion_rows": adapter_distortion_rows,
                "included_buckets": list(ADAPTER_DISTORTION_INCLUDED_BUCKETS),
                "excluded_buckets": list(ADAPTER_DISTORTION_EXCLUDED_BUCKETS),
            },
        },
        {
            "id": "tei_p5_mapping_dossiers",
            "owner": "ab-validator+abc",
            "status": "open",
            "evidence": {
                "reason": "feature-family dossiers must pin TEI P5 target, ABC extension, sidecar, diagnostic, or body-visible-text-only disposition",
            },
        },
        {
            "id": "parser_acceptance_criteria",
            "owner": "ab-validator",
            "status": "open",
            "evidence": {
                "reason": "future parser work needs measured acceptance criteria before replacing parser comparison evidence",
            },
        },
    ]


def render_markdown(summary: dict[str, Any]) -> str:
    lines = [
        "# Aozora Publication Next Work",
        "",
        f"Verdict: `{summary['verdict']}`",
        "",
        "## Completed Gates",
        "",
    ]
    for gate in summary["completed_gates"]:
        lines.append(f"- `{gate['id']}`: `{gate['status']}`")
    lines.extend(["", "## Next Work Items", ""])
    for item in summary["next_work_items"]:
        lines.append(f"- `{item['id']}` ({item['owner']}): `{item['status']}`")
    lines.extend(["", "## Parser Lanes", ""])
    for lane in summary["parser_lanes"]:
        lines.append(f"- `{lane['adapter']}`: files_scanned={lane.get('files_scanned')}")
    lines.extend(["", "## Calibration Only", ""])
    for item in summary["calibration_only_items"]:
        lines.append(f"- `{item['id']}`")
    lines.append("")
    return "\n".join(lines)


def build_summary(args: argparse.Namespace) -> dict[str, Any]:
    source = load_json(args.source_summary)
    coverage = load_json(args.coverage_summary)
    matrix = load_json(args.matrix_summary)
    conversion = load_json(args.conversion_summary)
    reference = load_json(args.source_reference_summary)
    performance_text = args.performance_report.read_text(encoding="utf-8")
    lanes = parser_lanes(conversion)
    completed_gates = [
        source_authority_gate(source),
        ir_publication_gate(coverage),
        publication_bundle_gate(coverage),
        five_parser_gate(conversion, lanes),
        source_reference_gate(reference),
    ]
    return {
        "schema_version": SCHEMA_VERSION,
        "verdict": VERDICT_OPEN,
        "completed_gates": completed_gates,
        "next_work_items": next_work_items(matrix),
        "calibration_only_items": [
            {
                "id": "tei_eaj_editorial_enrichment",
                "status": "calibration_only",
                "reason": "TEI-EAJ editorial enrichment is evidence for calibration, not source-authority admission.",
            }
        ],
        "parser_lanes": lanes,
        "performance": {"dnf_recorded": "DNF" in performance_text},
        "evidence_inputs": {
            "source_summary": input_record(args.source_summary),
            "coverage_summary": coverage_input_record(args.coverage_summary, coverage),
            "matrix_summary": input_record(args.matrix_summary),
            "conversion_summary": input_record(args.conversion_summary),
            "source_reference_summary": input_record(args.source_reference_summary),
            "performance_report": input_record(args.performance_report),
        },
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source-summary", type=pathlib.Path, required=True)
    parser.add_argument("--coverage-summary", type=pathlib.Path, required=True)
    parser.add_argument("--matrix-summary", type=pathlib.Path, required=True)
    parser.add_argument("--conversion-summary", type=pathlib.Path, required=True)
    parser.add_argument("--source-reference-summary", type=pathlib.Path, required=True)
    parser.add_argument("--performance-report", type=pathlib.Path, required=True)
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
