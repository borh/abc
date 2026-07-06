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
VERDICT_COMPLETE = "AOZORA_PUBLICATION_NEXT_WORK_COMPLETE"
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
REQUIRED_DOSSIER_SECTIONS = (
    "Source Inventory",
    "Parser-IR Representation",
    "TEI P5 Target",
    "ABC Extension Or Sidecar",
    "Plaintext Projection",
    "Current Evidence",
    "Open Decisions",
)
TEI_P5_REFERENCE_RE = re.compile(r"`?(\.\./abc/references/TEI/P5/[^\s`)]+)`?")


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
    evidence = {
        "schema_version": summary.get("schema_version"),
        "different_rows": as_int(summary.get("total_different_rows")),
        "counts_by_cause": {str(key): as_int(value) for key, value in sorted(counts.items())},
        "source_markup_backed_blockers": list_length(summary.get("source_markup_backed_blockers")),
        "calibration_only_rows": list_length(summary.get("calibration_only_rows")),
    }
    workset_files = summary.get("workset_files")
    if isinstance(workset_files, dict):
        evidence["workset_files"] = workset_files
    source_workset_files = summary.get("source_markup_backed_workset_files")
    if isinstance(source_workset_files, dict):
        evidence["source_markup_backed_workset_files"] = source_workset_files
    manual_workset_files = summary.get("manual_classification_workset_files")
    if isinstance(manual_workset_files, dict):
        evidence["manual_classification_workset_files"] = manual_workset_files
    return evidence


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


def missing_dossier_sections(text: str) -> list[str]:
    present = set(re.findall(r"^##\s+(.+?)\s*$", text, flags=re.MULTILINE))
    return [section for section in REQUIRED_DOSSIER_SECTIONS if section not in present]


def tei_p5_references(text: str) -> list[str]:
    return sorted(set(TEI_P5_REFERENCE_RE.findall(text)))


def default_tei_p5_root() -> pathlib.Path:
    candidates = [
        (REPO_ROOT.parent / "abc" / "references" / "TEI" / "P5").resolve(),
    ]
    if REPO_ROOT.parent.name == ".worktrees":
        candidates.append((REPO_ROOT.parents[2] / "abc" / "references" / "TEI" / "P5").resolve())
    for candidate in candidates:
        if candidate.exists():
            return candidate
    return candidates[0]


def resolve_tei_reference(reference: str, tei_p5_root: pathlib.Path) -> pathlib.Path:
    prefix = "../abc/references/TEI/P5/"
    if reference.startswith(prefix):
        return (tei_p5_root / reference.removeprefix(prefix)).resolve()
    if not reference.startswith("../abc/"):
        return (REPO_ROOT / reference).resolve()
    suffix = reference.removeprefix("../abc/")
    candidates = [
        (REPO_ROOT / reference).resolve(),
        (REPO_ROOT.parent / "abc" / suffix).resolve(),
    ]
    if REPO_ROOT.parent.name == ".worktrees":
        candidates.append((REPO_ROOT.parents[2] / "abc" / suffix).resolve())
    for candidate in candidates:
        if candidate.exists():
            return candidate
    return candidates[0]


def tei_reference_audit(references: list[str], tei_p5_root: pathlib.Path) -> dict[str, Any]:
    directories: list[str] = []
    missing: list[str] = []
    unverified: list[str] = []
    file_count = 0
    root_exists = tei_p5_root.exists()
    for reference in references:
        path = resolve_tei_reference(reference, tei_p5_root)
        if path.is_dir() or path.suffix == "":
            directories.append(reference)
            continue
        file_count += 1
        if not root_exists:
            unverified.append(reference)
        elif not path.is_file():
            missing.append(reference)
    return {
        "root": display_path(tei_p5_root),
        "root_exists": root_exists,
        "file_count": file_count,
        "directory_references": sorted(directories),
        "missing_references": sorted(missing),
        "unverified_references": sorted(unverified),
    }


def dossier_evidence(dossier_dir: pathlib.Path, tei_p5_root: pathlib.Path) -> dict[str, Any]:
    dossiers: list[dict[str, Any]] = []
    all_references: list[str] = []
    for path in sorted(dossier_dir.glob("*.md")):
        if path.name == "README.md":
            continue
        text = path.read_text(encoding="utf-8")
        missing_sections = missing_dossier_sections(text)
        references = tei_p5_references(text)
        all_references.extend(references)
        dossiers.append(
            {
                "name": path.stem,
                "path": display_path(path),
                "status": parse_status(text),
                "missing_sections": missing_sections,
                "tei_p5_references": references,
            }
        )
    incomplete = [dossier["name"] for dossier in dossiers if dossier["missing_sections"]]
    reference_audit = tei_reference_audit(all_references, tei_p5_root)
    return {
        "dossier_count": len(dossiers),
        "status_counts": status_counts([dossier["status"] for dossier in dossiers]),
        "complete_section_count": len(dossiers) - len(incomplete),
        "incomplete_section_dossiers": incomplete,
        "tei_p5_reference_count": len(all_references),
        "tei_p5_reference_root": reference_audit["root"],
        "tei_p5_reference_root_exists": reference_audit["root_exists"],
        "tei_p5_reference_file_count": reference_audit["file_count"],
        "tei_p5_reference_directory_count": len(reference_audit["directory_references"]),
        "directory_tei_p5_references": reference_audit["directory_references"],
        "missing_tei_p5_references": reference_audit["missing_references"],
        "unverified_tei_p5_references": reference_audit["unverified_references"],
        "dossiers": dossiers,
    }


def required_evidence_lines(text: str) -> list[str]:
    values: list[str] = []
    in_required = False
    for line in text.splitlines():
        if line.startswith("## "):
            in_required = line.strip() == "## Required Evidence Inputs"
            continue
        if in_required and line.startswith("- "):
            value = line[2:].strip()
            code_match = re.fullmatch(r"`(.+?)`", value)
            if code_match:
                value = code_match.group(1)
            values.append(value)
    return values


def evidence_path(value: str) -> pathlib.Path | None:
    if "/" not in value and not value.startswith("."):
        return None
    path = pathlib.Path(value)
    if path.is_absolute():
        return path
    return (REPO_ROOT / path).resolve()


def parser_acceptance_evidence(path: pathlib.Path) -> dict[str, Any]:
    text = path.read_text(encoding="utf-8")
    required_inputs = required_evidence_lines(text)
    path_inputs: list[dict[str, Any]] = []
    missing_paths: list[str] = []
    for value in required_inputs:
        path_value = evidence_path(value)
        if path_value is None:
            continue
        exists = path_value.exists()
        display = display_path(path_value)
        path_inputs.append({"path": display, "exists": exists})
        if not exists:
            missing_paths.append(display)
    title_match = re.search(r"^#\s+(.+?)\s*$", text, flags=re.MULTILINE)
    return {
        "path": display_path(path),
        "hash": sha256_file(path),
        "title": title_match.group(1) if title_match else path.name,
        "spec_status": parse_status(text),
        "required_evidence_inputs": len(required_inputs),
        "required_evidence_paths_total": len(path_inputs),
        "required_evidence_paths_existing": sum(1 for item in path_inputs if item["exists"]),
        "missing_required_evidence_paths": missing_paths,
        "required_evidence_paths": path_inputs,
    }


def source_disposition_status(evidence: dict[str, Any]) -> str:
    status_counts_value = evidence.get("status_counts")
    if not isinstance(status_counts_value, dict):
        status_counts_value = {}
    return (
        "complete"
        if as_int(evidence.get("classes_total")) > 0
        and as_int(evidence.get("policy_needed_count")) == 0
        and not evidence.get("evidence_needed_classes")
        and set(status_counts_value) <= {"admitted"}
        else "open"
    )


def text_policy_status(evidence: dict[str, Any]) -> str:
    counts = evidence.get("counts_by_cause")
    workset_files = evidence.get("workset_files")
    manual_workset_files = evidence.get("manual_classification_workset_files")
    if evidence.get("schema_version") != "parser-ir-text-policy-delta-v1":
        return "open"
    if not isinstance(counts, dict) or not isinstance(workset_files, dict):
        return "open"
    if as_int(evidence.get("different_rows")) != sum(as_int(value) for value in counts.values()):
        return "open"
    if as_int(counts.get("unknown_text_delta")) != 0:
        return "open"
    for cause, count in counts.items():
        if as_int(count) == 0:
            continue
        if not has_workset_file_record(workset_files.get(cause)):
            return "open"
    if isinstance(manual_workset_files, dict):
        for record in manual_workset_files.values():
            if isinstance(record, dict) and record.get("requires_manual_classification") and as_int(record.get("count")) > 0:
                return "open"
    return "complete"


def has_workset_file_record(value: Any) -> bool:
    if not isinstance(value, dict):
        return False
    return bool(value.get("path")) and bool(value.get("hash")) and "count" in value


def adapter_worksets_status(evidence: dict[str, Any]) -> str:
    worksets = evidence.get("worksets")
    workset_files = evidence.get("workset_files")
    if evidence.get("schema_version") != "adapter-fidelity-worksets-v1":
        return "open"
    if not isinstance(worksets, dict) or not isinstance(workset_files, dict):
        return "open"
    for bucket in ADAPTER_DISTORTION_INCLUDED_BUCKETS:
        if bucket not in worksets:
            return "open"
        record = workset_files.get(bucket)
        if not isinstance(record, dict) or not has_workset_file_record(record.get("all")):
            return "open"
    return "complete"


def dossier_status(evidence: dict[str, Any]) -> str:
    return (
        "complete"
        if as_int(evidence.get("dossier_count")) > 0
        and as_int(evidence.get("complete_section_count")) == as_int(evidence.get("dossier_count"))
        and not evidence.get("incomplete_section_dossiers")
        and as_int(evidence.get("tei_p5_reference_directory_count")) == 0
        and not evidence.get("missing_tei_p5_references")
        and not evidence.get("unverified_tei_p5_references")
        else "open"
    )


def parser_acceptance_status(evidence: dict[str, Any]) -> str:
    return (
        "complete"
        if evidence.get("spec_status") in {"Accepted", "Admitted", "Complete"}
        and as_int(evidence.get("required_evidence_paths_total")) == as_int(evidence.get("required_evidence_paths_existing"))
        and not evidence.get("missing_required_evidence_paths")
        else "open"
    )


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
            "status": source_disposition_status(source_disposition),
            "evidence": source_disposition,
        },
        {
            "id": "text_policy_calibration",
            "owner": "ab-validator+abc",
            "status": text_policy_status(text_evidence),
            "evidence": text_evidence,
        },
        {
            "id": "adapter_fidelity_worksets",
            "owner": "ab-validator",
            "status": adapter_worksets_status(adapter_evidence),
            "evidence": adapter_evidence,
        },
        {
            "id": "tei_p5_mapping_dossiers",
            "owner": "ab-validator+abc",
            "status": dossier_status(dossiers),
            "evidence": dossiers,
        },
        {
            "id": "parser_acceptance_criteria",
            "owner": "ab-validator",
            "status": parser_acceptance_status(parser_acceptance),
            "evidence": parser_acceptance,
        },
    ]


def next_work_verdict(items: list[dict[str, Any]]) -> str:
    return VERDICT_COMPLETE if items and all(item.get("status") == "complete" for item in items) else VERDICT_OPEN


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
        workset_file_paths = {
            cause: record.get("path")
            for cause, record in evidence.get("source_markup_backed_workset_files", {}).items()
            if isinstance(record, dict) and record.get("path")
        }
        manual_workset_file_paths = {
            cause: record.get("path")
            for cause, record in evidence.get("manual_classification_workset_files", {}).items()
            if isinstance(record, dict) and record.get("path")
        }
        return [
            f"different_rows={evidence.get('different_rows')}, source_markup_backed_blockers={evidence.get('source_markup_backed_blockers')}, calibration_only_rows={evidence.get('calibration_only_rows')}",
            f"counts_by_cause={json.dumps(evidence.get('counts_by_cause', {}), ensure_ascii=False, sort_keys=True)}",
            f"source_markup_backed_worksets={json.dumps(workset_file_paths, ensure_ascii=False, sort_keys=True)}",
            f"manual_classification_worksets={json.dumps(manual_workset_file_paths, ensure_ascii=False, sort_keys=True)}",
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
            f"complete_section_count={evidence.get('complete_section_count')}, incomplete_section_dossiers={json.dumps(evidence.get('incomplete_section_dossiers', []), ensure_ascii=False)}",
            f"tei_p5_reference_count={evidence.get('tei_p5_reference_count')}, file_count={evidence.get('tei_p5_reference_file_count')}, directory_count={evidence.get('tei_p5_reference_directory_count')}",
            f"tei_p5_reference_root={evidence.get('tei_p5_reference_root')}, root_exists={evidence.get('tei_p5_reference_root_exists')}, unverified={len(evidence.get('unverified_tei_p5_references', []))}",
        ]
    if item_id == "parser_acceptance_criteria":
        return [
            f"spec={evidence.get('path')}",
            f"spec_status={evidence.get('spec_status')}, required_evidence_inputs={evidence.get('required_evidence_inputs')}",
            f"required_evidence_paths_existing={evidence.get('required_evidence_paths_existing')}/{evidence.get('required_evidence_paths_total')}",
            f"missing_required_evidence_paths={json.dumps(evidence.get('missing_required_evidence_paths', []), ensure_ascii=False)}",
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
    tei_p5_root = args.tei_p5_root.resolve() if args.tei_p5_root else default_tei_p5_root()
    lanes = parser_lanes(conversion)
    completed_gates = [
        source_authority_gate(source),
        ir_publication_gate(coverage),
        publication_bundle_gate(coverage),
        five_parser_gate(conversion, lanes),
        source_reference_gate(reference),
    ]
    items = next_work_items(
        matrix,
        source_disposition=source_disposition_evidence(source_disposition),
        text_policy=text_policy_evidence(text_policy),
        adapter_worksets=adapter_worksets,
        dossiers=dossier_evidence(args.dossier_dir, tei_p5_root),
        parser_acceptance=parser_acceptance_evidence(args.parser_acceptance_spec),
    )
    return {
        "schema_version": SCHEMA_VERSION,
        "verdict": next_work_verdict(items),
        "completed_gates": completed_gates,
        "next_work_items": items,
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
            "tei_p5_root": {"path": display_path(tei_p5_root), "exists": tei_p5_root.exists()},
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
    parser.add_argument("--tei-p5-root", type=pathlib.Path)
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
