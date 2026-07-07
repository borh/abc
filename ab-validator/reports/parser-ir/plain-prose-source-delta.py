#!/usr/bin/env python
"""Classify plain-prose Level 3 source deltas from measured matrix evidence."""

from __future__ import annotations

import argparse
import pathlib
import sys
from collections import Counter, defaultdict
from typing import Any

_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from reports.lib.io import read_json, write_json

SCHEMA_VERSION = "plain-prose-source-delta-probe-v1"
REQUIRED_PARSERS = ("aozora2html", "aozora-epub3", "aozora-rs", "aozora2", "aozora")
PASSING_PARAGRAPH_ORIGINS = {"aligned", "source_note_back_routing"}
ADAPTER_PARAGRAPH_FAILURES = {
    "adapter_over_segmented",
    "adapter_under_segmented",
    "adapter_collapsed",
    "adapter_raw_only",
}
PARSER_IR_FAILURES = {"converter_paragraph_mismatch"}
ABC_RENDERER_FAILURES = {"renderer_paragraph_mismatch"}
RUBY_TEXT_BUCKETS = {
    "ruby_expanded_generated_contains_tei_eaj",
    "ruby_expanded_parenless_generated_contains_tei_eaj",
    "ruby_expanded_tei_eaj_contains_generated",
    "ruby_expanded_parenless_tei_eaj_contains_generated",
}
EXACT_RUBY_EQUIVALENCE_TEXT_BUCKETS = {
    "ruby_expanded_equal",
    "ruby_expanded_parenless_equal",
}
SOURCE_NOTE_TEXT_BUCKETS = {"base_drop_parentheticals_equal"}
PASSING_TEXT_BUCKETS = {"base_equal"}
OWNER_ORDER = {
    "adapter": 0,
    "evidence": 1,
    "policy": 2,
    "parser_ir": 3,
    "abc_renderer": 4,
}
SOURCE_AUTHORITY_GATE_FIELDS = (
    "gate_status",
    "works_scanned",
    "works_failed",
    "markers_total",
    "unknown_markers_total",
    "unallowlisted_unknown_markers_total",
    "allowlisted_unknown_markers_total",
    "strict_errors",
    "decode_failures",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--admission-summary", required=True, type=pathlib.Path)
    parser.add_argument("--matrix-summary", required=True, type=pathlib.Path)
    parser.add_argument("--structural-summary", required=True, type=pathlib.Path)
    parser.add_argument("--source-summary", required=True, type=pathlib.Path)
    parser.add_argument("--mapping", required=True, type=pathlib.Path)
    parser.add_argument("--summary-json", required=True, type=pathlib.Path)
    parser.add_argument("--report-md", required=True, type=pathlib.Path)
    parser.add_argument("--allow-missing-parser-evidence", action="store_true")
    return parser.parse_args()


def write_text(path: pathlib.Path, value: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(value, encoding="utf-8")


def require_mapping(admission: dict[str, Any], mapping: dict[str, Any]) -> dict[str, Any]:
    candidate = admission.get("mapping") if isinstance(admission.get("mapping"), dict) else {}
    fields = (
        "mapping_id",
        "mapping_version",
        "mapping_hash",
        "mapping_schema_hash",
        "target_parser_ir_schema_hash",
        "generated_mapping_rules",
    )
    if all(candidate.get(field) for field in fields):
        return {field: candidate[field] for field in fields}

    rules = mapping.get("transform_rule_descriptions")
    fallback = {
        "mapping_id": mapping.get("mapping_id"),
        "mapping_version": mapping.get("mapping_version"),
        "mapping_hash": candidate.get("mapping_hash"),
        "mapping_schema_hash": mapping.get("mapping_schema_hash"),
        "target_parser_ir_schema_hash": mapping.get("target_parser_ir_schema_hash"),
        "generated_mapping_rules": len(rules) if isinstance(rules, list) else None,
    }
    missing = [field for field in fields if not fallback.get(field)]
    if missing:
        raise SystemExit(f"mapping missing required fields: {', '.join(missing)}")
    return fallback


def row_profile(row: dict[str, Any]) -> str:
    return row.get("tei_eaj", {}).get("structure_profile") or "unknown"


def row_adapter(row: dict[str, Any]) -> str:
    return row.get("selected_aat", {}).get("adapter") or "unknown"


def row_key(row: dict[str, Any]) -> tuple[str, str]:
    return (str(row.get("work_id") or "unknown"), str(row.get("tei_eaj_file") or "unknown"))


def work_file_key(row: dict[str, Any]) -> tuple[str, str]:
    return (str(row.get("work_id") or "unknown"), str(row.get("tei_eaj_file") or "unknown"))


def generated_tag_count(row: dict[str, Any], tag: str) -> int:
    generated = row.get("generated_tei")
    if not isinstance(generated, dict):
        return 0
    total = 0
    for field in ("body_tag_counts", "document_tag_counts"):
        counts = generated.get(field)
        if isinstance(counts, dict):
            value = counts.get(tag)
            if isinstance(value, int):
                total += value
    return total


def generated_has_ruby_structure(row: dict[str, Any]) -> bool:
    return generated_tag_count(row, "ruby") > 0


def sorted_owners(owners: set[str]) -> list[str]:
    return sorted(owners, key=lambda owner: (OWNER_ORDER.get(owner, 99), owner))


def structural_rows_by_file(structural: dict[str, Any]) -> dict[str, dict[str, Any]]:
    rows = structural.get("rows", [])
    if not isinstance(rows, list):
        return {}
    by_file: dict[str, dict[str, Any]] = {}
    for row in rows:
        if not isinstance(row, dict):
            continue
        tei = row.get("tei")
        if not isinstance(tei, dict):
            continue
        tei_eaj_file = tei.get("tei_eaj_file")
        if tei_eaj_file:
            by_file[str(tei_eaj_file)] = row
    return by_file


def structural_adapters(row: dict[str, Any] | None) -> set[str]:
    if not isinstance(row, dict):
        return set()
    inputs = row.get("aat_inputs", [])
    if not isinstance(inputs, list):
        return set()
    adapters: set[str] = set()
    for entry in inputs:
        if not isinstance(entry, dict):
            continue
        aat = entry.get("aat")
        adapter = None
        if isinstance(aat, dict):
            adapter = aat.get("adapter")
        if not adapter:
            adapter = entry.get("adapter")
        if adapter:
            adapters.add(str(adapter))
    return adapters


def load_source_authority_gate(
    admission: dict[str, Any], source: dict[str, Any]
) -> tuple[dict[str, Any], bool, str | None]:
    source_gate = (
        source.get("source_authority_gate")
        if isinstance(source.get("source_authority_gate"), dict)
        else source
    )
    if not isinstance(source_gate, dict):
        source_gate = {}
    if not source_gate:
        admission_gate = admission.get("source_authority_gate")
        if isinstance(admission_gate, dict):
            source_gate = admission_gate

    concise_gate = {
        field: source_gate.get(field)
        for field in SOURCE_AUTHORITY_GATE_FIELDS
        if field in source_gate
    }
    gate_status = concise_gate.get("gate_status")
    unknown_total = concise_gate.get("unallowlisted_unknown_markers_total")
    gate_failed = gate_status != "SOURCE_AUTHORITY_GATE_PASS" or unknown_total not in (0, "0")
    reason = "source authority gate did not pass" if gate_failed else None
    return concise_gate, gate_failed, reason


def classify_text(row: dict[str, Any]) -> tuple[list[str], set[str], list[str]]:
    bucket = row.get("text", {}).get("body_text_match_bucket") or "unknown"
    source_note_excluded = bool(row.get("classification", {}).get("source_note_body_excluded"))
    classifications: list[str] = []
    owners: set[str] = set()
    reasons: list[str] = []

    if bucket in PASSING_TEXT_BUCKETS or (
        bucket in EXACT_RUBY_EQUIVALENCE_TEXT_BUCKETS
        and generated_has_ruby_structure(row)
    ):
        return classifications, owners, reasons
    if source_note_excluded or bucket in SOURCE_NOTE_TEXT_BUCKETS:
        classifications.append("source_note_metadata_excluded")
        owners.add("policy")
        reasons.append("source-note or parenthetical source attribution belongs outside body plaintext")
    elif bucket in RUBY_TEXT_BUCKETS or bucket in EXACT_RUBY_EQUIVALENCE_TEXT_BUCKETS:
        classifications.append("ruby_metadata_not_plaintext")
        owners.add("policy")
        reasons.append("ruby-expanded surfaces require generated ruby structure for admission")
    else:
        classifications.append("source_text_policy_required")
        owners.add("policy")
        reasons.append(f"text policy required for {bucket}")
    return classifications, owners, reasons


def classify_row(row: dict[str, Any], missing_for_row: list[str]) -> dict[str, Any]:
    origin = row.get("classification", {}).get("paragraph_origin_bucket") or "unknown"
    text_bucket = row.get("text", {}).get("body_text_match_bucket") or "unknown"
    classifications: list[str] = []
    owners: set[str] = set()
    reasons: list[str] = []

    if missing_for_row:
        classifications.append("missing_parser_evidence")
        owners.add("evidence")
        reasons.append("missing required parser evidence: " + ", ".join(missing_for_row))

    if origin in PASSING_PARAGRAPH_ORIGINS:
        pass
    elif origin in ADAPTER_PARAGRAPH_FAILURES:
        classifications.append("adapter_paragraph_bug")
        owners.add("adapter")
        reasons.append(f"paragraph origin is {origin}")
    elif origin in PARSER_IR_FAILURES:
        classifications.append("parser_ir_projection_bug")
        owners.add("parser_ir")
        reasons.append(f"parser-IR projection mismatch: {origin}")
    elif origin in ABC_RENDERER_FAILURES:
        classifications.append("abc_renderer_bug")
        owners.add("abc_renderer")
        reasons.append(f"ABC renderer mismatch: {origin}")
    else:
        classifications.append("tei_eaj_editorial_segmentation")
        owners.add("policy")
        reasons.append(f"paragraph segmentation requires profile policy: {origin}")

    text_classes, text_owners, text_reasons = classify_text(row)
    classifications.extend(text_classes)
    owners.update(text_owners)
    reasons.extend(text_reasons)

    return {
        "work_id": row.get("work_id"),
        "title": row.get("title"),
        "tei_eaj_file": row.get("tei_eaj_file"),
        "level": row.get("level"),
        "state": row.get("state"),
        "adapter": row_adapter(row),
        "paragraph_origin_bucket": origin,
        "body_text_match_bucket": text_bucket,
        "classifications": classifications,
        "blocking_owners": sorted_owners(owners),
        "evidence": {
            "missing_required_parsers_for_row": missing_for_row,
            "materialization_status": row.get("materialization", {}).get("status"),
        },
        "reasons": reasons,
    }


def classify_skipped_row(
    skipped: dict[str, Any], structural_row: dict[str, Any] | None
) -> dict[str, Any]:
    tei = structural_row.get("tei") if isinstance(structural_row, dict) else {}
    classification = structural_row.get("classification") if isinstance(structural_row, dict) else {}
    notes = classification.get("notes") if isinstance(classification, dict) else None
    skip_reason = skipped.get("reason") or "unknown"
    reasons = [f"skipped matrix entry: {skip_reason}"]
    if isinstance(notes, list):
        reasons.extend(str(note) for note in notes if note)

    return {
        "work_id": tei.get("work_id"),
        "title": tei.get("title"),
        "tei_eaj_file": skipped.get("tei_eaj_file"),
        "level": tei.get("level"),
        "state": tei.get("state"),
        "adapter": "missing",
        "paragraph_origin_bucket": "not_evaluated",
        "body_text_match_bucket": "not_evaluated",
        "classifications": ["evidence_gap"],
        "blocking_owners": ["evidence"],
        "evidence": {
            "skip_reason": skip_reason,
            "classification_notes": notes if isinstance(notes, list) else [],
        },
        "reasons": reasons,
    }


def apply_source_authority_gate(
    rows: list[dict[str, Any]], gate_failed: bool, reason: str | None
) -> list[dict[str, Any]]:
    if not gate_failed or not reason:
        return rows

    updated: list[dict[str, Any]] = []
    for row in rows:
        classifications = list(row.get("classifications", []))
        if "evidence_gap" not in classifications:
            classifications.append("evidence_gap")

        owners = set(row.get("blocking_owners", []))
        owners.add("evidence")

        reasons = list(row.get("reasons", []))
        if reason not in reasons:
            reasons.append(reason)

        evidence = dict(row.get("evidence", {}))
        evidence["source_authority_gate_reason"] = reason

        updated.append(
            {
                **row,
                "classifications": classifications,
                "blocking_owners": sorted_owners(owners),
                "evidence": evidence,
                "reasons": reasons,
            }
        )
    return updated


def parser_evidence_coverage(
    rows: list[dict[str, Any]], structural_by_file: dict[str, dict[str, Any]]
) -> tuple[dict[str, Any], dict[tuple[str, str], list[str]]]:
    adapters_by_row: dict[tuple[str, str], set[str]] = defaultdict(set)
    for row in rows:
        key = row_key(row)
        tei_eaj_file = str(row.get("tei_eaj_file") or "")
        adapters_by_row[key].update(structural_adapters(structural_by_file.get(tei_eaj_file)))
        adapters_by_row[key].add(row_adapter(row))

    observed_rows_by_parser: Counter[str] = Counter()
    for adapters in adapters_by_row.values():
        for adapter in adapters:
            observed_rows_by_parser[adapter] += 1
    observed_global = set(observed_rows_by_parser)
    missing_global = [parser for parser in REQUIRED_PARSERS if parser not in observed_global]
    missing_by_row = {
        key: [parser for parser in REQUIRED_PARSERS if parser not in adapters]
        for key, adapters in adapters_by_row.items()
    }
    work_files_missing = sum(1 for missing in missing_by_row.values() if missing)
    row_entries_missing = sum(1 for row in rows if missing_by_row.get(row_key(row)))
    verdict = "FIVE_PARSER_EVIDENCE_COMPLETE"
    if missing_global or row_entries_missing:
        verdict = "FIVE_PARSER_EVIDENCE_INCOMPLETE"
    return (
        {
            "verdict": verdict,
            "observed_rows_by_parser": dict(sorted(observed_rows_by_parser.items())),
            "missing_parsers": missing_global,
            "row_entries_missing_required_parser_evidence": row_entries_missing,
            "work_files_missing_required_parser_evidence": work_files_missing,
            "rows_missing_required_parser_evidence": row_entries_missing,
        },
        missing_by_row,
    )


def build_summary(
    admission: dict[str, Any],
    matrix: dict[str, Any],
    structural: dict[str, Any],
    source: dict[str, Any],
    mapping: dict[str, Any],
    allow_missing_parser_evidence: bool,
) -> dict[str, Any]:
    structural_by_file = structural_rows_by_file(structural)
    source_authority_gate, source_gate_failed, source_gate_reason = load_source_authority_gate(
        admission, source
    )
    plain_rows = [row for row in matrix.get("rows", []) if row_profile(row) == "plain_prose"]
    coverage, missing_by_row = parser_evidence_coverage(plain_rows, structural_by_file)
    if coverage["verdict"] != "FIVE_PARSER_EVIDENCE_COMPLETE" and not allow_missing_parser_evidence:
        raise SystemExit("missing parser evidence; rerun with --allow-missing-parser-evidence for exploratory report")
    classified_rows = [classify_row(row, missing_by_row.get(row_key(row), [])) for row in plain_rows]
    skipped_rows = [
        classify_skipped_row(skipped, structural_by_file.get(str(skipped.get("tei_eaj_file") or "")))
        for skipped in matrix.get("skipped", [])
        if isinstance(skipped, dict)
    ]
    classified = apply_source_authority_gate(
        classified_rows + skipped_rows,
        source_gate_failed,
        source_gate_reason,
    )
    classification_counts = Counter(
        classification for row in classified for classification in row["classifications"]
    )
    owner_counts = Counter(owner for row in classified for owner in row["blocking_owners"])
    works = {work_file_key(row) for row in classified}
    return {
        "schema_version": SCHEMA_VERSION,
        "required_parsers": list(REQUIRED_PARSERS),
        "parser_evidence_coverage": coverage,
        "source_authority_gate": source_authority_gate,
        "mapping": require_mapping(admission, mapping),
        "plain_prose_scope": {
            "rows_total": len(classified),
            "works_total": len(works),
        },
        "classification_counts": dict(sorted(classification_counts.items())),
        "blocking_owners": sorted_owners(set(owner_counts)),
        "blocking_owner_counts": dict(sorted(owner_counts.items())),
        "rows": classified,
    }


def render_markdown(summary: dict[str, Any]) -> str:
    lines = [
        "# Plain Prose Source Delta Probe",
        "",
        "This report diagnoses plain-prose Level 3 blockers. It is not a Level 3 admission decision.",
        "",
        "## Verdict",
        "",
        f"- parser_evidence_coverage: `{summary['parser_evidence_coverage']['verdict']}`",
        f"- blocking_owners: `{', '.join(summary['blocking_owners']) or 'none'}`",
        f"- mapping_id: `{summary['mapping']['mapping_id']}`",
        f"- mapping_version: `{summary['mapping']['mapping_version']}`",
        "",
        "## Required Parsers",
        "",
    ]
    for parser in summary["required_parsers"]:
        count = summary["parser_evidence_coverage"]["observed_rows_by_parser"].get(parser, 0)
        lines.append(f"- `{parser}`: {count} rows")
    lines.extend(["", "## Classification Counts", "", "| classification | rows |", "|---|---:|"])
    for name, count in summary["classification_counts"].items():
        lines.append(f"| {name} | {count} |")
    lines.extend(["", "## Rows", "", "| work_id | adapter | classifications | owners | file |", "|---|---|---|---|---|"])
    for row in summary["rows"]:
        lines.append(
            "| {work_id} | {adapter} | {classes} | {owners} | `{file}` |".format(
                work_id=row.get("work_id"),
                adapter=row.get("adapter"),
                classes=", ".join(row["classifications"]) or "none",
                owners=", ".join(row["blocking_owners"]) or "none",
                file=row.get("tei_eaj_file"),
            )
        )
    lines.append("")
    return "\n".join(lines)


def main() -> int:
    args = parse_args()
    admission = read_json(args.admission_summary)
    matrix = read_json(args.matrix_summary)
    structural = read_json(args.structural_summary)
    source = read_json(args.source_summary)
    mapping = read_json(args.mapping)
    summary = build_summary(
        admission,
        matrix,
        structural,
        source,
        mapping,
        args.allow_missing_parser_evidence,
    )
    write_json(args.summary_json, summary)
    write_text(args.report_md, render_markdown(summary))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
