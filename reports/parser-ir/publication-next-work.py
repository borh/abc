#!/usr/bin/env python3
"""Synthesize the next-work ledger for Aozora publication completion."""

from __future__ import annotations

import argparse
import hashlib
import json
import pathlib
import re
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


def status_counts(values: list[str]) -> dict[str, int]:
    counts: dict[str, int] = {}
    for value in values:
        counts[value] = counts.get(value, 0) + 1
    return dict(sorted(counts.items()))


def source_disposition_evidence(summary: dict[str, Any]) -> dict[str, Any]:
    classes = summary.get("classes")
    if not isinstance(classes, list):
        classes = []
    statuses: list[str] = []
    admitted: list[str] = []
    policy_needed: list[str] = []
    evidence_needed: list[str] = []
    measured_occurrences_by_class: dict[str, int] = {}
    for entry in classes:
        if not isinstance(entry, dict):
            continue
        source_class = str(entry.get("source_class", ""))
        status = str(entry.get("status", "unknown"))
        statuses.append(status)
        if status == "admitted":
            admitted.append(source_class)
        if status == "policy_needed":
            policy_needed.append(source_class)
        if entry.get("measurement_status") == "evidence_needed":
            evidence_needed.append(source_class)
        if "measured_occurrences" in entry:
            measured_occurrences_by_class[source_class] = as_int(entry.get("measured_occurrences"))
    return {
        "verdict": summary.get("verdict"),
        "classes_total": len(classes),
        "admitted_classes": sorted(admitted),
        "admitted_count": len(admitted),
        "policy_needed_classes": sorted(policy_needed),
        "policy_needed_count": len(policy_needed),
        "evidence_needed_classes": sorted(evidence_needed),
        "status_counts": status_counts(statuses),
        "measured_occurrences_by_class": dict(sorted(measured_occurrences_by_class.items())),
    }


def list_length(value: Any) -> int:
    if isinstance(value, list):
        return len(value)
    return 0


def text_policy_evidence(summary: dict[str, Any]) -> dict[str, Any]:
    counts = summary.get("counts_by_cause")
    if not isinstance(counts, dict):
        counts = {}
    return {
        "schema_version": summary.get("schema_version"),
        "different_rows": as_int(summary.get("total_different_rows")),
        "counts_by_cause": {str(key): as_int(value) for key, value in sorted(counts.items())},
        "source_markup_backed_blockers": list_length(summary.get("source_markup_backed_blockers")),
        "calibration_only_rows": list_length(summary.get("calibration_only_rows")),
    }


def workset_counts(value: Any) -> dict[str, int]:
    if not isinstance(value, dict):
        return {}
    counts: dict[str, int] = {}
    for key, entry in value.items():
        if isinstance(entry, dict):
            counts[str(key)] = as_int(entry.get("count"))
        else:
            counts[str(key)] = as_int(entry)
    return dict(sorted(counts.items()))


def adapter_worksets_evidence(summary: dict[str, Any], fallback_distortion_rows: int) -> dict[str, Any]:
    worksets = workset_counts(summary.get("worksets"))
    excluded_counts = workset_counts(summary.get("excluded_counts"))
    total = sum(worksets.values()) if worksets else fallback_distortion_rows
    return {
        "schema_version": summary.get("schema_version"),
        "adapter_distortion_rows": total,
        "worksets": worksets,
        "workset_files": summary.get("workset_files", {}),
        "excluded_counts": excluded_counts,
        "included_buckets": list(ADAPTER_DISTORTION_INCLUDED_BUCKETS),
        "excluded_buckets": list(ADAPTER_DISTORTION_EXCLUDED_BUCKETS),
    }


def parse_status(text: str) -> str:
    match = re.search(r"^Status:\s*(.+?)\s*$", text, flags=re.MULTILINE)
    return match.group(1) if match else "unknown"


def dossier_evidence(dossier_dir: pathlib.Path) -> dict[str, Any]:
    dossiers: list[dict[str, str]] = []
    for path in sorted(dossier_dir.glob("*.md")):
        if path.name == "README.md":
            continue
        text = path.read_text(encoding="utf-8")
        dossiers.append({"name": path.stem, "path": display_path(path), "status": parse_status(text)})
    return {
        "dossier_count": len(dossiers),
        "status_counts": status_counts([dossier["status"] for dossier in dossiers]),
        "dossiers": dossiers,
    }


def parser_acceptance_evidence(path: pathlib.Path) -> dict[str, Any]:
    text = path.read_text(encoding="utf-8")
    required_inputs = 0
    in_required = False
    for line in text.splitlines():
        if line.startswith("## "):
            in_required = line.strip() == "## Required Evidence Inputs"
            continue
        if in_required and line.startswith("- "):
            required_inputs += 1
    title_match = re.search(r"^#\s+(.+?)\s*$", text, flags=re.MULTILINE)
    return {
        "path": display_path(path),
        "hash": sha256_file(path),
        "title": title_match.group(1) if title_match else path.name,
        "spec_status": parse_status(text),
        "required_evidence_inputs": required_inputs,
    }


def next_work_items(
    matrix: dict[str, Any],
    *,
    source_disposition: dict[str, Any],
    text_policy: dict[str, Any],
    adapter_worksets: dict[str, Any],
    dossiers: dict[str, Any],
    parser_acceptance: dict[str, Any],
) -> list[dict[str, Any]]:
    text_buckets = matrix.get("body_text_relation_buckets") if isinstance(matrix.get("body_text_relation_buckets"), dict) else {}
    paragraph_buckets = matrix.get("paragraph_origin_buckets") if isinstance(matrix.get("paragraph_origin_buckets"), dict) else {}
    adapter_distortion_rows = sum(as_int(paragraph_buckets.get(bucket)) for bucket in ADAPTER_DISTORTION_INCLUDED_BUCKETS)
    text_evidence = {
        "different_rows": as_int(text_buckets.get("different")),
        "generated_contains_tei_eaj_rows": as_int(text_buckets.get("generated_contains_tei_eaj")),
        "tei_eaj_contains_generated_rows": as_int(text_buckets.get("tei_eaj_contains_generated")),
    }
    text_evidence.update(text_policy)
    adapter_evidence = {
        "adapter_distortion_rows": adapter_distortion_rows,
        "included_buckets": list(ADAPTER_DISTORTION_INCLUDED_BUCKETS),
        "excluded_buckets": list(ADAPTER_DISTORTION_EXCLUDED_BUCKETS),
    }
    adapter_evidence.update(adapter_worksets_evidence(adapter_worksets, adapter_distortion_rows))
    return [
        {
            "id": "source_region_disposition_samples",
            "owner": "ab-validator+abc",
            "status": "open",
            "evidence": source_disposition,
        },
        {
            "id": "text_policy_calibration",
            "owner": "ab-validator+abc",
            "status": "open",
            "evidence": text_evidence,
        },
        {
            "id": "adapter_fidelity_worksets",
            "owner": "ab-validator",
            "status": "open",
            "evidence": adapter_evidence,
        },
        {
            "id": "tei_p5_mapping_dossiers",
            "owner": "ab-validator+abc",
            "status": "open",
            "evidence": dossiers,
        },
        {
            "id": "parser_acceptance_criteria",
            "owner": "ab-validator",
            "status": "open",
            "evidence": parser_acceptance,
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
        for line in render_item_evidence(item):
            lines.append(f"  - {line}")
    lines.extend(["", "## Parser Lanes", ""])
    for lane in summary["parser_lanes"]:
        lines.append(f"- `{lane['adapter']}`: files_scanned={lane.get('files_scanned')}")
    lines.extend(["", "## Calibration Only", ""])
    for item in summary["calibration_only_items"]:
        lines.append(f"- `{item['id']}`")
    lines.append("")
    return "\n".join(lines)


def render_item_evidence(item: dict[str, Any]) -> list[str]:
    evidence = item.get("evidence")
    if not isinstance(evidence, dict):
        return []
    item_id = item.get("id")
    if item_id == "source_region_disposition_samples":
        return [
            f"classes_total={evidence.get('classes_total')}, admitted={evidence.get('admitted_count')}, policy_needed={evidence.get('policy_needed_count')}",
            f"policy_needed_classes={json.dumps(evidence.get('policy_needed_classes', []), ensure_ascii=False)}",
            f"evidence_needed_classes={json.dumps(evidence.get('evidence_needed_classes', []), ensure_ascii=False)}",
        ]
    if item_id == "text_policy_calibration":
        return [
            f"different_rows={evidence.get('different_rows')}, source_markup_backed_blockers={evidence.get('source_markup_backed_blockers')}, calibration_only_rows={evidence.get('calibration_only_rows')}",
            f"counts_by_cause={json.dumps(evidence.get('counts_by_cause', {}), ensure_ascii=False, sort_keys=True)}",
        ]
    if item_id == "adapter_fidelity_worksets":
        workset_file_paths = {
            bucket: record.get("all", {}).get("path")
            for bucket, record in evidence.get("workset_files", {}).items()
            if isinstance(record, dict)
        }
        return [
            f"adapter_distortion_rows={evidence.get('adapter_distortion_rows')}",
            f"worksets={json.dumps(evidence.get('worksets', {}), ensure_ascii=False, sort_keys=True)}",
            f"workset_files={json.dumps(workset_file_paths, ensure_ascii=False, sort_keys=True)}",
            f"excluded_counts={json.dumps(evidence.get('excluded_counts', {}), ensure_ascii=False, sort_keys=True)}",
        ]
    if item_id == "tei_p5_mapping_dossiers":
        return [
            f"dossier_count={evidence.get('dossier_count')}",
            f"status_counts={json.dumps(evidence.get('status_counts', {}), ensure_ascii=False, sort_keys=True)}",
        ]
    if item_id == "parser_acceptance_criteria":
        return [
            f"spec={evidence.get('path')}",
            f"spec_status={evidence.get('spec_status')}, required_evidence_inputs={evidence.get('required_evidence_inputs')}",
        ]
    return [f"evidence={json.dumps(evidence, ensure_ascii=False, sort_keys=True)}"]


def build_summary(args: argparse.Namespace) -> dict[str, Any]:
    source = load_json(args.source_summary)
    coverage = load_json(args.coverage_summary)
    matrix = load_json(args.matrix_summary)
    conversion = load_json(args.conversion_summary)
    reference = load_json(args.source_reference_summary)
    source_disposition = load_json(args.source_disposition_summary)
    text_policy = load_json(args.text_policy_summary)
    adapter_worksets = load_json(args.adapter_worksets_summary)
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
        "next_work_items": next_work_items(
            matrix,
            source_disposition=source_disposition_evidence(source_disposition),
            text_policy=text_policy_evidence(text_policy),
            adapter_worksets=adapter_worksets,
            dossiers=dossier_evidence(args.dossier_dir),
            parser_acceptance=parser_acceptance_evidence(args.parser_acceptance_spec),
        ),
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
            "source_disposition_summary": input_record(args.source_disposition_summary),
            "text_policy_summary": input_record(args.text_policy_summary),
            "adapter_worksets_summary": input_record(args.adapter_worksets_summary),
            "dossier_dir": {"path": display_path(args.dossier_dir)},
            "parser_acceptance_spec": input_record(args.parser_acceptance_spec),
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
    parser.add_argument("--source-disposition-summary", type=pathlib.Path, required=True)
    parser.add_argument("--text-policy-summary", type=pathlib.Path, required=True)
    parser.add_argument("--adapter-worksets-summary", type=pathlib.Path, required=True)
    parser.add_argument("--dossier-dir", type=pathlib.Path, required=True)
    parser.add_argument("--parser-acceptance-spec", type=pathlib.Path, required=True)
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
