#!/usr/bin/env python3
"""Classify profile-aware Level 3 TEI admission from measured reports."""

from __future__ import annotations

import argparse
import hashlib
import json
import pathlib
from collections import Counter
from typing import Any

SCHEMA_VERSION = "profile-aware-level3-tei-admission-v1"
PASSING_PARAGRAPH_ORIGINS = {"aligned", "source_note_back_routing"}
ADAPTER_PARAGRAPH_FAILURES = {
    "adapter_over_segmented",
    "adapter_under_segmented",
    "adapter_collapsed",
    "adapter_raw_only",
}
POLICY_PARAGRAPH_FAILURES = {"page_break_projection", "unknown"}
PARSER_IR_FAILURES = {"converter_paragraph_mismatch"}
ABC_RENDERER_FAILURES = {"renderer_paragraph_mismatch"}
PASSING_TEXT_BUCKETS = {"base_equal"}
EXACT_RUBY_EQUIVALENCE_TEXT_BUCKETS = {
    "ruby_expanded_equal",
    "ruby_expanded_parenless_equal",
}
POLICY_TEXT_BUCKETS = {
    "base_drop_parentheticals_equal",
    "ruby_expanded_parenless_generated_contains_tei_eaj",
    "ruby_expanded_generated_contains_tei_eaj",
    "base_drop_parentheticals_generated_contains_tei_eaj",
    "base_generated_contains_tei_eaj",
    "ruby_expanded_parenless_tei_eaj_contains_generated",
    "ruby_expanded_tei_eaj_contains_generated",
    "base_drop_parentheticals_tei_eaj_contains_generated",
    "base_tei_eaj_contains_generated",
    "different",
}
PLAINTEXT_POLICY = {
    "plaintext_surface": "body_base_text",
    "ruby_expanded_surfaces": "admit_exact_structural_equivalence_only",
    "metadata_policy": "exclude_typed_metadata_from_plaintext",
}
LANE_POLICY_REQUIRED = {"drama", "verse", "notes", "front_back_matter"}
LANE_OUT_OF_SCOPE = {"lv4_enrichment"}
REQUIRED_MAPPING_FIELDS = (
    "mapping_id",
    "mapping_version",
    "mapping_schema_hash",
    "target_parser_ir_schema_id",
    "target_parser_ir_schema_hash",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--matrix-summary", required=True, type=pathlib.Path)
    parser.add_argument("--source-summary", required=True, type=pathlib.Path)
    parser.add_argument("--summary-json", required=True, type=pathlib.Path)
    parser.add_argument("--report-md", required=True, type=pathlib.Path)
    return parser.parse_args()


def load_json(path: pathlib.Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))


def canonical_json(value: object) -> str:
    return json.dumps(
        value,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
    ).replace("/", "\\/")


def document_hash(value: object) -> str:
    return "sha256:" + hashlib.sha256(canonical_json(value).encode("utf-8")).hexdigest()


def resolve_path(raw: str, base_dir: pathlib.Path) -> pathlib.Path:
    path = pathlib.Path(raw)
    if path.is_absolute():
        return path
    return (base_dir / path).resolve()


def resolve_mapping_path(
    raw: str,
    base_dir: pathlib.Path,
    repo_root: pathlib.Path,
) -> pathlib.Path:
    path = pathlib.Path(raw)
    if not path.is_absolute():
        return resolve_path(raw, base_dir)

    try:
        relative = path.relative_to(repo_root)
    except ValueError:
        parts = path.parts
        if "data" in parts:
            suffix = pathlib.Path(*parts[parts.index("data") :])
            candidate = repo_root / suffix
            if candidate.exists():
                return candidate.resolve()
        raise SystemExit(
            "absolute mapping path is outside the current checkout and cannot be remapped "
            f"into it: {path}"
        )

    return (repo_root / relative).resolve()


def write_json(path: pathlib.Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def write_text(path: pathlib.Path, value: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(value, encoding="utf-8")


def source_authority_gate(source: dict[str, Any]) -> dict[str, Any]:
    return {
        "gate_status": source.get("gate_status"),
        "works_scanned": source.get("works_scanned"),
        "unallowlisted_unknown_markers_total": source.get(
            "unallowlisted_unknown_markers_total"
        ),
    }


def source_authority_passed(source: dict[str, Any]) -> bool:
    return (
        source.get("gate_status") == "SOURCE_AUTHORITY_GATE_PASS"
        and source.get("unallowlisted_unknown_markers_total") == 0
    )


def require_non_empty_mapping_string(mapping: dict[str, Any], field: str) -> str:
    value = mapping.get(field)
    if not isinstance(value, str) or not value.strip():
        raise SystemExit(f"mapping missing or empty required field: {field}")
    return value


def require_transform_rule_descriptions(mapping: dict[str, Any]) -> list[Any]:
    value = mapping.get("transform_rule_descriptions")
    if not isinstance(value, list) or not value:
        raise SystemExit(
            "mapping.transform_rule_descriptions must be a non-empty list"
        )
    return value


def build_mapping_block(mapping: dict[str, Any], mapping_hash: str) -> dict[str, Any]:
    transform_rule_descriptions = require_transform_rule_descriptions(mapping)
    return {
        "mapping_id": require_non_empty_mapping_string(mapping, "mapping_id"),
        "mapping_version": require_non_empty_mapping_string(mapping, "mapping_version"),
        "mapping_hash": mapping_hash,
        "mapping_schema_hash": require_non_empty_mapping_string(
            mapping, "mapping_schema_hash"
        ),
        "target_parser_ir_schema_id": require_non_empty_mapping_string(
            mapping, "target_parser_ir_schema_id"
        ),
        "target_parser_ir_schema_hash": require_non_empty_mapping_string(
            mapping, "target_parser_ir_schema_hash"
        ),
        "generated_mapping_rules": len(transform_rule_descriptions),
    }


def parser_ir_infrastructure_verdict(matrix: dict[str, Any]) -> str:
    totals = matrix.get("totals", {})
    if totals.get("rows_attempted", 0) and totals.get("materialization_failed", 0) == 0:
        return "LEVEL3_IR_INFRASTRUCTURE_READY"
    return "LEVEL3_IR_INFRASTRUCTURE_BLOCKED"


def row_adapter(row: dict[str, Any]) -> str:
    return row.get("selected_aat", {}).get("adapter") or "unknown"


def row_profile(row: dict[str, Any]) -> str:
    return row.get("tei_eaj", {}).get("structure_profile") or "unknown"


def row_paragraph_origin(row: dict[str, Any]) -> str:
    return row.get("classification", {}).get("paragraph_origin_bucket") or "unknown"


def row_text_bucket(row: dict[str, Any]) -> str:
    return row.get("text", {}).get("body_text_match_bucket") or "unknown"


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


def text_bucket_passes(row: dict[str, Any], text_bucket: str) -> bool:
    if text_bucket in PASSING_TEXT_BUCKETS:
        return True
    return (
        text_bucket in EXACT_RUBY_EQUIVALENCE_TEXT_BUCKETS
        and generated_has_ruby_structure(row)
    )


def row_materialization_ok(row: dict[str, Any]) -> bool:
    return row.get("materialization", {}).get("status") == "passed"


def classify_plain_prose_row(row: dict[str, Any], source_passed: bool) -> dict[str, Any]:
    owners: set[str] = set()
    reasons: list[str] = []
    origin = row_paragraph_origin(row)
    text_bucket = row_text_bucket(row)

    if not source_passed:
        owners.add("evidence")
        reasons.append("source authority gate did not pass")

    if not row_materialization_ok(row):
        owners.add("abc_renderer")
        reasons.append("materialization did not pass")

    if origin in PASSING_PARAGRAPH_ORIGINS:
        pass
    elif origin in ADAPTER_PARAGRAPH_FAILURES:
        owners.add("adapter")
        reasons.append(f"paragraph origin is {origin}")
    elif origin in POLICY_PARAGRAPH_FAILURES:
        owners.add("policy")
        reasons.append(f"paragraph origin requires policy: {origin}")
    elif origin in PARSER_IR_FAILURES:
        owners.add("parser_ir")
        reasons.append(f"parser-IR paragraph mismatch: {origin}")
    elif origin in ABC_RENDERER_FAILURES:
        owners.add("abc_renderer")
        reasons.append(f"ABC renderer paragraph mismatch: {origin}")
    else:
        owners.add("evidence")
        reasons.append(f"unknown paragraph origin: {origin}")

    if text_bucket_passes(row, text_bucket):
        pass
    elif text_bucket in POLICY_TEXT_BUCKETS or text_bucket in EXACT_RUBY_EQUIVALENCE_TEXT_BUCKETS:
        owners.add("policy")
        reasons.append(f"text policy required for {text_bucket}")
    else:
        owners.add("evidence")
        reasons.append(f"unknown text bucket: {text_bucket}")

    passed = not owners
    return {
        "work_id": row.get("work_id"),
        "title": row.get("title"),
        "tei_eaj_file": row.get("tei_eaj_file"),
        "adapter": row_adapter(row),
        "paragraph_origin_bucket": origin,
        "body_text_match_bucket": text_bucket,
        "passed": passed,
        "owners": sorted(owners),
        "reasons": reasons,
    }


def verdict_from_failures(
    rows_total: int,
    rows_failed: int,
    failures_by_owner: Counter[str],
) -> str:
    if rows_total == 0:
        return "LEVEL3_PLAIN_PROSE_NOT_EVALUATED"
    if rows_failed == 0:
        return "LEVEL3_PLAIN_PROSE_ADMITTED"
    blocking_owners = sorted(failures_by_owner, key=owner_sort_key)
    if blocking_owners == ["adapter"]:
        return "LEVEL3_PLAIN_PROSE_BLOCKED_ADAPTER_FIDELITY"
    if blocking_owners == ["policy"]:
        return "LEVEL3_PLAIN_PROSE_BLOCKED_TEXT_POLICY"
    if blocking_owners == ["evidence"]:
        return "LEVEL3_PLAIN_PROSE_BLOCKED_EVIDENCE"
    labels = "_AND_".join(OWNER_VERDICT_LABELS[owner] for owner in blocking_owners)
    return f"LEVEL3_PLAIN_PROSE_BLOCKED_{labels}"


OWNER_ORDER = {
    "adapter": 0,
    "policy": 1,
    "evidence": 2,
    "parser_ir": 3,
    "abc_renderer": 4,
}
OWNER_VERDICT_LABELS = {
    "adapter": "ADAPTER_FIDELITY",
    "policy": "TEXT_POLICY",
    "evidence": "EVIDENCE",
    "parser_ir": "PARSER_IR",
    "abc_renderer": "ABC_RENDERER",
}


def owner_sort_key(owner: str) -> tuple[int, str]:
    return (OWNER_ORDER.get(owner, 99), owner)


def build_plain_prose_admission(
    rows: list[dict[str, Any]],
    source_passed: bool,
) -> dict[str, Any]:
    classified = classify_plain_prose_rows(rows, source_passed)
    failures_by_owner: Counter[str] = Counter()
    failures_by_adapter: Counter[str] = Counter()
    text_policy_buckets: Counter[str] = Counter()
    paragraph_origin_buckets: Counter[str] = Counter()

    for item in classified:
        paragraph_origin_buckets[item["paragraph_origin_bucket"]] += 1
        text_policy_buckets[item["body_text_match_bucket"]] += 1
        if not item["passed"]:
            failures_by_adapter[item["adapter"]] += 1
            for owner in item["owners"]:
                failures_by_owner[owner] += 1

    rows_failed = sum(1 for item in classified if not item["passed"])
    rows_passed = len(classified) - rows_failed
    blocking_owners = sorted(failures_by_owner, key=owner_sort_key)
    return {
        "scope": "tei-eaj-generated-matrix plain_prose rows",
        "plaintext_policy": PLAINTEXT_POLICY,
        "rows_total": len(classified),
        "rows_passed": rows_passed,
        "rows_failed": rows_failed,
        "verdict": verdict_from_failures(len(classified), rows_failed, failures_by_owner),
        "blocking_owners": blocking_owners,
        "failures_by_owner": dict(sorted(failures_by_owner.items())),
        "failures_by_adapter": dict(sorted(failures_by_adapter.items())),
        "text_policy_buckets": dict(sorted(text_policy_buckets.items())),
        "paragraph_origin_buckets": dict(sorted(paragraph_origin_buckets.items())),
        "failed_rows": [item for item in classified if not item["passed"]],
    }


def classify_plain_prose_rows(
    rows: list[dict[str, Any]],
    source_passed: bool,
) -> list[dict[str, Any]]:
    return [
        classify_plain_prose_row(row, source_passed)
        for row in rows
        if row_profile(row) == "plain_prose"
    ]


def work_file_key(row: dict[str, Any]) -> tuple[str, str]:
    return (
        str(row.get("work_id") or "unknown"),
        str(row.get("tei_eaj_file") or "unknown"),
    )


def work_file_verdict(work_files_total: int, work_files_failed: int) -> str:
    if work_files_total == 0:
        return "LEVEL3_PLAIN_PROSE_WORKSET_NOT_EVALUATED"
    if work_files_failed == 0:
        return "LEVEL3_PLAIN_PROSE_WORKSET_ADMITTED"
    return "LEVEL3_PLAIN_PROSE_WORKSET_BLOCKED"


def build_plain_prose_workset_admission(
    rows: list[dict[str, Any]],
    source_passed: bool,
) -> dict[str, Any]:
    classified = classify_plain_prose_rows(rows, source_passed)
    by_work_file: dict[tuple[str, str], list[dict[str, Any]]] = {}
    for item in classified:
        by_work_file.setdefault(work_file_key(item), []).append(item)

    passing_candidates_by_adapter: Counter[str] = Counter()
    failures_by_owner: Counter[str] = Counter()
    failed_work_files: list[dict[str, Any]] = []

    for (work_id, tei_eaj_file), candidates in sorted(by_work_file.items()):
        passing_candidates = [item for item in candidates if item["passed"]]
        if passing_candidates:
            for item in passing_candidates:
                passing_candidates_by_adapter[item["adapter"]] += 1
            continue

        owners = sorted(
            {owner for item in candidates for owner in item["owners"]},
            key=owner_sort_key,
        )
        for owner in owners:
            failures_by_owner[owner] += 1
        failed_work_files.append(
            {
                "work_id": work_id,
                "title": candidates[0].get("title"),
                "tei_eaj_file": tei_eaj_file,
                "owners": owners,
                "candidates": candidates,
            }
        )

    work_files_total = len(by_work_file)
    work_files_failed = len(failed_work_files)
    return {
        "scope": "tei-eaj-generated-matrix plain_prose work files",
        "admission_unit": "tei_eaj_work_file_with_any_passing_parser_candidate",
        "work_files_total": work_files_total,
        "work_files_passed": work_files_total - work_files_failed,
        "work_files_failed": work_files_failed,
        "verdict": work_file_verdict(work_files_total, work_files_failed),
        "blocking_owners": sorted(failures_by_owner, key=owner_sort_key),
        "failures_by_owner": dict(sorted(failures_by_owner.items())),
        "passing_candidates_by_adapter": dict(
            sorted(passing_candidates_by_adapter.items())
        ),
        "failed_work_files": failed_work_files,
    }


def lane_verdict(profile: str) -> str:
    if profile in LANE_OUT_OF_SCOPE:
        return "LANE_OUT_OF_SCOPE_FOR_LEVEL3"
    if profile in LANE_POLICY_REQUIRED:
        return "LANE_POLICY_REQUIRED"
    if profile == "plain_prose":
        return "LANE_READY_FOR_GATE_IMPLEMENTATION"
    return "LANE_BLOCKED_EVIDENCE"


def build_profile_lanes(rows: list[dict[str, Any]]) -> dict[str, Any]:
    counts: Counter[str] = Counter(row_profile(row) for row in rows)
    lanes: dict[str, Any] = {}
    for profile in sorted(set(counts) | LANE_POLICY_REQUIRED | LANE_OUT_OF_SCOPE):
        lanes[profile] = {
            "rows": counts.get(profile, 0),
            "verdict": lane_verdict(profile),
        }
    return lanes


def profile_label(profile: str) -> str:
    if profile == "plain_prose":
        return "Plain prose"
    if profile == "lv4_enrichment":
        return "Level 4 enrichment"
    if profile == "front_back_matter":
        return "Front/back matter"
    if profile == "source_note_back_routing":
        return "Source note back routing"
    return profile.replace("_", " ").title()


def build_evidence_gaps(matrix: dict[str, Any]) -> dict[str, Any]:
    skipped = matrix.get("skipped", [])
    reasons: Counter[str] = Counter(item.get("reason", "unknown") for item in skipped)
    return {
        "rows": len(skipped),
        "reasons": dict(sorted(reasons.items())),
        "examples": skipped[:10],
    }


def build_summary(
    matrix: dict[str, Any],
    source: dict[str, Any],
    mapping: dict[str, Any],
    mapping_hash: str,
) -> dict[str, Any]:
    rows = matrix.get("rows", [])
    source_passed = source_authority_passed(source)
    plain_prose = build_plain_prose_admission(rows, source_passed)
    plain_prose_workset = build_plain_prose_workset_admission(rows, source_passed)
    return {
        "schema_version": SCHEMA_VERSION,
        "source_authority_gate": source_authority_gate(source),
        "parser_ir_infrastructure_verdict": parser_ir_infrastructure_verdict(matrix),
        "plain_prose_admission": plain_prose,
        "plain_prose_workset_admission": plain_prose_workset,
        "profile_lanes": build_profile_lanes(rows),
        "evidence_gaps": build_evidence_gaps(matrix),
        "mapping": build_mapping_block(mapping, mapping_hash),
        "inputs": matrix.get("inputs", {}),
    }


def render_markdown(summary: dict[str, Any]) -> str:
    plain = summary["plain_prose_admission"]
    workset = summary["plain_prose_workset_admission"]
    mapping = summary["mapping"]
    lines = [
        "# Profile-Aware Level 3 TEI Admission",
        "",
        "This report classifies measured parser-IR generated TEI evidence into profile-aware Level 3 admission buckets. TEI-EAJ remains comparison evidence; source inventory remains source authority.",
        "",
        "## Verdict",
        "",
        f"- source_authority_gate: `{summary['source_authority_gate'].get('gate_status')}`",
        f"- parser_ir_infrastructure_verdict: `{summary['parser_ir_infrastructure_verdict']}`",
        f"- plain_prose_verdict: `{plain['verdict']}`",
        f"- plain_prose_workset_verdict: `{workset['verdict']}`",
        f"- blocking_owners: `{', '.join(plain['blocking_owners']) or 'none'}`",
        f"- mapping_hash: `{summary['mapping']['mapping_hash']}`",
        "",
        "## Mapping",
        "",
        f"- mapping_id: `{mapping['mapping_id']}`",
        f"- mapping_version: `{mapping['mapping_version']}`",
        f"- mapping_hash: `{mapping['mapping_hash']}`",
        f"- mapping_schema_hash: `{mapping['mapping_schema_hash']}`",
        f"- target_parser_ir_schema_id: `{mapping['target_parser_ir_schema_id']}`",
        f"- target_parser_ir_schema_hash: `{mapping['target_parser_ir_schema_hash']}`",
        f"- generated_mapping_rules: `{mapping['generated_mapping_rules']}`",
        "",
        "## Plain Prose Admission",
        "",
        f"- plaintext_surface: `{plain['plaintext_policy']['plaintext_surface']}`",
        f"- ruby_expanded_surfaces: `{plain['plaintext_policy']['ruby_expanded_surfaces']}`",
        f"- metadata_policy: `{plain['plaintext_policy']['metadata_policy']}`",
        "",
        "| metric | value |",
        "|---|---:|",
        f"| rows_total | {plain['rows_total']} |",
        f"| rows_passed | {plain['rows_passed']} |",
        f"| rows_failed | {plain['rows_failed']} |",
        "",
        "## Plain Prose Workset Admission",
        "",
        f"- admission_unit: `{workset['admission_unit']}`",
        f"- verdict: `{workset['verdict']}`",
        f"- blocking_owners: `{', '.join(workset['blocking_owners']) or 'none'}`",
        "",
        "| metric | value |",
        "|---|---:|",
        f"| work_files_total | {workset['work_files_total']} |",
        f"| work_files_passed | {workset['work_files_passed']} |",
        f"| work_files_failed | {workset['work_files_failed']} |",
        "",
        "### Passing Candidates By Adapter",
        "",
        "| adapter | work files |",
        "|---|---:|",
    ]
    for adapter, count in workset["passing_candidates_by_adapter"].items():
        lines.append(f"| {adapter} | {count} |")

    lines.extend(
        [
            "",
            "### Workset Failures By Owner",
            "",
            "| owner | work files |",
            "|---|---:|",
        ]
    )
    for owner, count in workset["failures_by_owner"].items():
        lines.append(f"| {owner} | {count} |")

    lines.extend(
        [
            "",
            "### Failures By Owner",
            "",
            "| owner | rows |",
            "|---|---:|",
        ]
    )
    for owner, count in plain["failures_by_owner"].items():
        lines.append(f"| {owner} | {count} |")

    lines.extend(["", "### Paragraph Origin Buckets", "", "| bucket | rows |", "|---|---:|"])
    for bucket, count in plain["paragraph_origin_buckets"].items():
        lines.append(f"| {bucket} | {count} |")

    lines.extend(["", "### Text Policy Buckets", "", "| bucket | rows |", "|---|---:|"])
    for bucket, count in plain["text_policy_buckets"].items():
        lines.append(f"| {bucket} | {count} |")

    lines.extend(
        ["", "## Profile Lanes", "", "| profile | label | rows | verdict |", "|---|---|---:|---|"]
    )
    for profile, lane in summary["profile_lanes"].items():
        lines.append(
            f"| {profile} | {profile_label(profile)} | {lane['rows']} | `{lane['verdict']}` |"
        )

    lines.extend(["", "## Evidence Gaps", "", f"- rows: {summary['evidence_gaps']['rows']}"])
    for reason, count in summary["evidence_gaps"]["reasons"].items():
        lines.append(f"- {reason}: {count}")

    if plain["failed_rows"]:
        lines.extend(
            [
                "",
                "## Plain Prose Failed Rows",
                "",
                "| work_id | adapter | paragraph origin | text bucket | owners | file |",
                "|---|---|---|---|---|---|",
            ]
        )
        for row in plain["failed_rows"]:
            lines.append(
                "| {work_id} | {adapter} | {origin} | {text} | {owners} | `{file}` |".format(
                    work_id=row.get("work_id"),
                    adapter=row["adapter"],
                    origin=row["paragraph_origin_bucket"],
                    text=row["body_text_match_bucket"],
                    owners=", ".join(row["owners"]),
                    file=row.get("tei_eaj_file"),
                )
            )

    lines.append("")
    return "\n".join(lines)


def main() -> int:
    args = parse_args()
    repo_root = pathlib.Path(__file__).resolve().parents[2]
    matrix = load_json(args.matrix_summary)
    source = load_json(args.source_summary)
    mapping_input = matrix.get("inputs", {}).get("mapping")
    if not mapping_input:
        raise SystemExit("matrix summary missing inputs.mapping")
    mapping_path = resolve_mapping_path(mapping_input, args.matrix_summary.parent, repo_root)
    mapping = load_json(mapping_path)
    summary = build_summary(matrix, source, mapping, document_hash(mapping))
    write_json(args.summary_json, summary)
    write_text(args.report_md, render_markdown(summary))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
