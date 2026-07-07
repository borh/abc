#!/usr/bin/env python
"""Validate a materialized parser-IR publication bundle across artifacts.

ABC owns full schema/profile validation. This checker records the ab-validator
join evidence needed by the publication coverage gate: the generated files are
present, manifest sidecars point at the generated sidecars, TEI provenance
attributes resolve to preservation records, preservation pointers resolve back
to TEI/parser-IR source pointers, source-region evidence is green, and plaintext
contains body-visible text only.
"""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys
import xml.etree.ElementTree as ET
from typing import Any

_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from reports.lib.hashing import file_sha256
from reports.lib.io import read_json, write_json
from reports.lib.paths import repo_root

SCHEMA_VERSION = "publication-bundle-validation-evidence-v1"
BATCH_SCHEMA_VERSION = "publication-bundle-batch-validation-evidence-v1"
PASSED_VERDICT = "PUBLICATION_BUNDLE_VALIDATION_PASSED"
FAILED_VERDICT = "PUBLICATION_BUNDLE_VALIDATION_FAILED"
BATCH_PASSED_VERDICT = "PUBLICATION_BUNDLE_BATCH_VALIDATION_PASSED"
BATCH_FAILED_VERDICT = "PUBLICATION_BUNDLE_BATCH_VALIDATION_FAILED"
REPO_ROOT = repo_root()
ABC_NS = "{https://w3id.org/abc/ns/tei}"
XML_ID = "{http://www.w3.org/XML/1998/namespace}id"
SOURCE_REGION_REQUIRED_COUNTERS = {
    "body_typed_occurrences",
    "body_raw_preserved_occurrences",
    "source_apparatus_occurrences",
    "front_matter_occurrences",
    "back_matter_occurrences",
    "body_end_boundary_occurrences",
    "terminal_provenance_occurrences",
    "colophon_metadata_occurrences",
    "letter_address_origin_occurrences",
    "malformed_source_occurrences",
    "unsupported_body_markup_occurrences",
    "unknown_region_occurrences",
    "unknown_unreviewed_occurrences",
}
REQUIRED_CHECKS = (
    "parser_ir_schema_valid",
    "source_region_coverage_valid",
    "tei_profile_valid",
    "preservation_schema_valid",
    "tei_manifest_valid",
    "plaintext_manifest_valid",
    "tei_manifest_references_preservation",
    "tei_manifest_references_validation_result",
    "source_region_sidecar_role_available",
    "tei_abc_projection_resolves_to_sidecar",
    "preservation_tei_pointers_resolve",
    "preservation_source_pointers_resolve",
    "plaintext_body_only",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--parser-ir", type=pathlib.Path)
    parser.add_argument("--source-region-summary", required=True, type=pathlib.Path)
    parser.add_argument("--publication-dir", type=pathlib.Path)
    parser.add_argument("--batch-root", type=pathlib.Path)
    parser.add_argument("--batch-scope", default="representative")
    parser.add_argument("--abc-commit")
    parser.add_argument("--command", default="clojure -M:abc/materialize-publication ...")
    parser.add_argument("--summary-json", required=True, type=pathlib.Path)
    parser.add_argument("--report-md", required=True, type=pathlib.Path)
    args = parser.parse_args()
    if args.batch_root is not None:
        if args.parser_ir is not None or args.publication_dir is not None:
            parser.error("--batch-root cannot be combined with --parser-ir or --publication-dir")
    elif args.parser_ir is None or args.publication_dir is None:
        parser.error("single-bundle validation requires --parser-ir and --publication-dir")
    return args


def write_text(path: pathlib.Path, value: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(value, encoding="utf-8")


def file_hash(path: pathlib.Path) -> str | None:
    try:
        return file_sha256(path)
    except OSError:
        return None


def display_path(path: pathlib.Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT))
    except ValueError:
        return str(path)


def artifact(path: pathlib.Path) -> dict[str, Any]:
    return {"path": display_path(path), "hash": file_hash(path)}


def add_failure(
    failures: list[dict[str, Any]],
    check: str,
    message: str,
    details: dict[str, Any] | None = None,
) -> bool:
    failure: dict[str, Any] = {"check": check, "message": message}
    if details is not None:
        failure["details"] = details
    failures.append(failure)
    return False


def sidecar_by_role(manifest: dict[str, Any], role: str) -> dict[str, Any] | None:
    for row in manifest.get("sidecars", []):
        if isinstance(row, dict) and row.get("role") == role:
            return row
    return None


def content_hash_matches(manifest: dict[str, Any], path: pathlib.Path) -> bool:
    return manifest.get("content", {}).get("content_hash") == file_hash(path)


def sidecar_hash_matches(
    manifest: dict[str, Any],
    role: str,
    path_hint: str,
    path: pathlib.Path,
) -> bool:
    sidecar = sidecar_by_role(manifest, role)
    return bool(
        sidecar and sidecar.get("path_hint") == path_hint and sidecar.get("hash") == file_hash(path)
    )


def source_region_valid(source_region: dict[str, Any]) -> bool:
    region = source_region.get("source_region_coverage", {})
    required_counters_present = all(
        isinstance(region.get(counter), int) for counter in SOURCE_REGION_REQUIRED_COUNTERS
    )
    return bool(
        source_region.get("schema_version") == "aozora-source-region-coverage-v1"
        and source_region.get("gate_status") == "SOURCE_AUTHORITY_GATE_PASS"
        and required_counters_present
        and (source_region.get("unallowlisted_unknown_markers_total") or 0) == 0
        and (region.get("unsupported_body_markup_occurrences") or 0) == 0
        and (region.get("unknown_region_occurrences") or 0) == 0
        and (region.get("unknown_unreviewed_occurrences") or 0) == 0
    )


def collect_values_by_key(value: Any, key: str) -> set[str]:
    found: set[str] = set()
    if isinstance(value, dict):
        for item_key, item_value in value.items():
            if item_key == key and isinstance(item_value, str):
                found.add(item_value)
            found.update(collect_values_by_key(item_value, key))
    elif isinstance(value, list):
        for item in value:
            found.update(collect_values_by_key(item, key))
    return found


def tei_id_set(root: ET.Element) -> set[str]:
    ids: set[str] = set()
    for element in root.iter():
        for key in (XML_ID, "id"):
            value = element.attrib.get(key)
            if value:
                ids.add(value)
    return ids


def preservation_record_refs(root: ET.Element) -> list[str]:
    refs: list[str] = []
    for element in root.iter():
        value = element.attrib.get(f"{ABC_NS}preservation-record")
        if value:
            refs.extend(token for token in value.split() if token)
    return refs


def tei_pointer_resolves(pointer: Any, ids: set[str]) -> bool:
    if pointer is None:
        return True
    if not isinstance(pointer, str) or not pointer:
        return False
    if pointer.startswith("#"):
        return pointer[1:] in ids
    return False


def normalize_plaintext(value: str) -> str:
    return re.sub(r"\s+", "", value)


def node_visible_body_text(node: dict[str, Any]) -> str:
    node_type = node.get("type")
    if node_type == "text":
        return node.get("text") if isinstance(node.get("text"), str) else ""
    if node_type == "ruby":
        base = node.get("ruby", {}).get("base")
        return base if isinstance(base, str) else ""
    if node_type == "gaiji":
        gaiji = node.get("gaiji", {})
        for key in ("unicode", "image_or_glyph_fallback"):
            value = gaiji.get(key)
            if isinstance(value, str) and value:
                return value
        raw_marker = gaiji.get("raw_marker")
        if isinstance(raw_marker, str) and raw_marker.startswith("※"):
            return raw_marker
        return ""
    if node_type in {"emphasis", "heading"}:
        children = node.get("inline_children")
        if isinstance(children, list):
            return "".join(
                node_visible_body_text(child) for child in children if isinstance(child, dict)
            )
        text = node.get("text")
        return text if isinstance(text, str) else ""
    if node_type == "layout-span":
        children = node.get("inline_children")
        if isinstance(children, list):
            child_text = "".join(
                node_visible_body_text(child) for child in children if isinstance(child, dict)
            )
            if child_text:
                return child_text
        text = node.get("text")
        return text if isinstance(text, str) else ""
    if node_type in {"indentation", "quote", "caption", "editor-note"}:
        text = node.get("text")
        return text if isinstance(text, str) else ""
    if node_type in {"line-break", "page-break"}:
        return "\n"
    return ""


def parser_ir_body_plaintext(parser_ir: dict[str, Any]) -> str:
    nodes = parser_ir.get("nodes", [])
    if not isinstance(nodes, list):
        return ""
    return "".join(
        node_visible_body_text(node)
        for node in nodes
        if isinstance(node, dict) and node.get("type") != "source-note"
    )


def plaintext_body_only(parser_ir: dict[str, Any], plaintext: str) -> bool:
    return normalize_plaintext(plaintext) == normalize_plaintext(
        parser_ir_body_plaintext(parser_ir)
    )


def preview_around(value: str, offset: int, radius: int = 80) -> str:
    start = max(0, offset - radius)
    end = min(len(value), offset + radius)
    return value[start:end]


def plaintext_mismatch_details(parser_ir: dict[str, Any], plaintext: str) -> dict[str, Any] | None:
    expected = normalize_plaintext(parser_ir_body_plaintext(parser_ir))
    actual = normalize_plaintext(plaintext)
    if expected == actual:
        return None
    first_diff: int | None = None
    for offset, (expected_char, actual_char) in enumerate(zip(expected, actual)):
        if expected_char != actual_char:
            first_diff = offset
            break
    if first_diff is None:
        first_diff = min(len(expected), len(actual))
    return {
        "kind": "plaintext_mismatch",
        "expected_length": len(expected),
        "actual_length": len(actual),
        "first_diff_offset": first_diff,
        "expected_preview": preview_around(expected, first_diff),
        "actual_preview": preview_around(actual, first_diff),
    }


def validate_bundle(args: argparse.Namespace) -> dict[str, Any]:
    publication_dir = args.publication_dir
    paths = {
        "parser_ir": args.parser_ir,
        "tei": publication_dir / "tei.xml",
        "plaintext": publication_dir / "plain.txt",
        "preservation": publication_dir / "preservation.json",
        "source_region_coverage": args.source_region_summary,
        "tei_manifest": publication_dir / "tei.manifest.json",
        "plaintext_manifest": publication_dir / "plaintext.manifest.json",
        "tei_validation_result": publication_dir / "tei-validation-result.json",
    }
    validated_bundle = {
        key: artifact(path) for key, path in paths.items() if key != "tei_validation_result"
    }
    failures: list[dict[str, Any]] = []

    try:
        parser_ir = read_json(args.parser_ir)
        source_region = read_json(args.source_region_summary)
        preservation = read_json(paths["preservation"])
        tei_manifest = read_json(paths["tei_manifest"])
        plaintext_manifest = read_json(paths["plaintext_manifest"])
        tei_validation = read_json(paths["tei_validation_result"])
        plaintext = paths["plaintext"].read_text(encoding="utf-8")
        tei_root = ET.parse(paths["tei"]).getroot()
    except (json.JSONDecodeError, OSError, ET.ParseError) as error:
        return {
            "schema_version": SCHEMA_VERSION,
            "verdict": FAILED_VERDICT,
            "validator": "ab-validator publication-bundle-validate",
            "command": args.command,
            "abc_commit": args.abc_commit,
            "validated_bundle": validated_bundle,
            "checks": {},
            "failures": [{"check": "bundle_readable", "message": str(error)}],
        }

    record_ids = {
        record.get("record_id")
        for record in preservation.get("records", [])
        if isinstance(record, dict) and isinstance(record.get("record_id"), str)
    }
    tei_ids = tei_id_set(tei_root)
    source_pointers = collect_values_by_key(parser_ir, "source_pointer")

    checks: dict[str, bool] = {}
    checks["parser_ir_schema_valid"] = bool(
        parser_ir.get("schema_id") == "https://w3id.org/abc/schemas/parser-ir.schema.json"
        and isinstance(parser_ir.get("schema_hash"), str)
        and parser_ir.get("schema_hash", "").startswith("sha256:")
    )
    checks["source_region_coverage_valid"] = source_region_valid(source_region)
    checks["tei_profile_valid"] = bool(tei_validation.get("status") == "passed")
    checks["preservation_schema_valid"] = bool(
        preservation.get("schema_id")
        == "https://w3id.org/abc/schemas/parser-ir-publication-preservation.schema.json"
        and preservation.get("schema_version") == "0.2.0"
        and preservation.get("coverage", {}).get("record_count")
        == len(preservation.get("records", []))
    )
    checks["tei_manifest_valid"] = bool(
        tei_manifest.get("artifact_kind") == "tei"
        and tei_manifest.get("validation_status") == "passed"
        and content_hash_matches(tei_manifest, paths["tei"])
    )
    checks["plaintext_manifest_valid"] = bool(
        plaintext_manifest.get("artifact_kind") == "plaintext"
        and plaintext_manifest.get("validation_status") == "passed"
        and content_hash_matches(plaintext_manifest, paths["plaintext"])
    )
    checks["tei_manifest_references_preservation"] = sidecar_hash_matches(
        tei_manifest,
        "preservation",
        "preservation.json",
        paths["preservation"],
    )
    checks["tei_manifest_references_validation_result"] = sidecar_hash_matches(
        tei_manifest,
        "validation-result",
        "tei-validation-result.json",
        paths["tei_validation_result"],
    )
    checks["source_region_sidecar_role_available"] = checks["source_region_coverage_valid"]
    checks["tei_abc_projection_resolves_to_sidecar"] = all(
        ref in record_ids for ref in preservation_record_refs(tei_root)
    )
    checks["preservation_tei_pointers_resolve"] = all(
        tei_pointer_resolves(record.get("tei_pointer"), tei_ids)
        for record in preservation.get("records", [])
        if isinstance(record, dict)
    )
    checks["preservation_source_pointers_resolve"] = all(
        record.get("source_pointer") in source_pointers
        for record in preservation.get("records", [])
        if isinstance(record, dict) and record.get("source_pointer") is not None
    )
    plaintext_details = plaintext_mismatch_details(parser_ir, plaintext)
    checks["plaintext_body_only"] = plaintext_details is None

    messages = {
        "parser_ir_schema_valid": "Parser-IR JSON does not carry the expected ABC parser-IR schema identity.",
        "source_region_coverage_valid": "Source-region coverage is not a passing source-authority gate.",
        "tei_profile_valid": "TEI validation result is not passed.",
        "preservation_schema_valid": "Preservation sidecar does not match the expected schema identity or record count.",
        "tei_manifest_valid": "TEI manifest is not passed or its content hash does not match tei.xml.",
        "plaintext_manifest_valid": "Plaintext manifest is not passed or its content hash does not match plain.txt.",
        "tei_manifest_references_preservation": "TEI manifest does not reference preservation.json with the correct hash.",
        "tei_manifest_references_validation_result": "TEI manifest does not reference tei-validation-result.json with the correct hash.",
        "source_region_sidecar_role_available": "Source-region coverage evidence is not available as a valid bundle input.",
        "tei_abc_projection_resolves_to_sidecar": "A TEI abc:preservation-record value does not resolve to a preservation record_id.",
        "preservation_tei_pointers_resolve": "A preservation tei_pointer does not resolve to a TEI xml:id.",
        "preservation_source_pointers_resolve": "A preservation source_pointer does not resolve to a parser-IR source_pointer.",
        "plaintext_body_only": "Plaintext differs from the parser-IR body-only projection.",
    }
    for check, passed in checks.items():
        if not passed:
            details = plaintext_details if check == "plaintext_body_only" else None
            add_failure(failures, check, messages[check], details)

    return {
        "schema_version": SCHEMA_VERSION,
        "verdict": PASSED_VERDICT if not failures else FAILED_VERDICT,
        "validator": "ab-validator publication-bundle-validate",
        "command": args.command,
        "abc_commit": args.abc_commit,
        "validated_bundle": validated_bundle,
        "checks": checks,
        "failures": failures,
    }


def discover_batch_rows(batch_root: pathlib.Path) -> list[tuple[str, pathlib.Path, pathlib.Path]]:
    candidates: list[tuple[str, pathlib.Path, pathlib.Path]] = []
    for parser_ir in sorted(batch_root.rglob("parser-ir.json")):
        row_dir = parser_ir.parent
        publication_dir = row_dir / "publication"
        if not publication_dir.is_dir():
            continue
        try:
            row_id = str(row_dir.relative_to(batch_root))
        except ValueError:
            row_id = str(row_dir)
        if row_id.startswith("rows/"):
            row_id = row_id.removeprefix("rows/")
        candidates.append((row_id, parser_ir, publication_dir))
    return candidates


def validate_batch(args: argparse.Namespace) -> dict[str, Any]:
    row_inputs = discover_batch_rows(args.batch_root)
    rows: list[dict[str, Any]] = []
    failures: list[dict[str, Any]] = []

    for row_id, parser_ir, publication_dir in row_inputs:
        row_args = argparse.Namespace(
            parser_ir=parser_ir,
            source_region_summary=args.source_region_summary,
            publication_dir=publication_dir,
            abc_commit=args.abc_commit,
            command=args.command,
        )
        row_summary = validate_bundle(row_args)
        row_failures = row_summary.get("failures", [])
        row = {
            "row_id": row_id,
            "row_dir": display_path(parser_ir.parent),
            "verdict": row_summary.get("verdict"),
            "validated_bundle": row_summary.get("validated_bundle", {}),
            "checks": row_summary.get("checks", {}),
            "failures": row_failures,
        }
        rows.append(row)
        for failure in row_failures:
            if isinstance(failure, dict):
                failures.append(
                    {
                        "row_id": row_id,
                        "check": str(failure.get("check")),
                        "message": str(failure.get("message")),
                        **(
                            {"details": failure.get("details")}
                            if isinstance(failure.get("details"), dict)
                            else {}
                        ),
                    }
                )

    if not rows:
        failures.append(
            {
                "row_id": "",
                "check": "rows_present",
                "message": "No row directories containing parser-ir.json and publication/ were found.",
            }
        )

    checks = {
        check: bool(rows) and all(row.get("checks", {}).get(check) is True for row in rows)
        for check in REQUIRED_CHECKS
    }
    rows_failed = sum(1 for row in rows if row.get("verdict") != PASSED_VERDICT)
    scope = {
        "kind": args.batch_scope,
        "batch_root": display_path(args.batch_root),
        "rows_discovered": len(row_inputs),
        "rows_validated": len(rows),
        "rows_passed": len(rows) - rows_failed,
        "rows_failed": rows_failed,
    }

    return {
        "schema_version": BATCH_SCHEMA_VERSION,
        "verdict": BATCH_PASSED_VERDICT if not failures else BATCH_FAILED_VERDICT,
        "validator": "ab-validator publication-bundle-validate batch",
        "command": args.command,
        "abc_commit": args.abc_commit,
        "scope": scope,
        "checks": checks,
        "rows": rows,
        "failures": failures,
    }


def render_markdown(summary: dict[str, Any]) -> str:
    if summary.get("schema_version") == BATCH_SCHEMA_VERSION:
        scope = summary.get("scope", {})
        lines = [
            "# Publication Bundle Batch Validation",
            "",
            f"Verdict: `{summary['verdict']}`",
            "",
            "## Scope",
            "",
            f"Kind: `{scope.get('kind')}`",
            "",
            f"Rows validated: `{scope.get('rows_validated')}`",
            "",
            f"Rows failed: `{scope.get('rows_failed')}`",
            "",
            "## Checks",
            "",
            "| Check | Passed |",
            "|---|---:|",
        ]
        for check, passed in summary.get("checks", {}).items():
            lines.append(f"| `{check}` | `{str(passed).lower()}` |")
        if summary.get("failures"):
            lines.extend(["", "## Failures", ""])
            for failure in summary["failures"][:50]:
                lines.append(f"- `{failure['row_id']}` `{failure['check']}`: {failure['message']}")
                details = failure.get("details")
                if isinstance(details, dict) and details.get("kind") == "plaintext_mismatch":
                    lines.append(
                        "  - first_diff_offset: "
                        f"`{details.get('first_diff_offset')}`; "
                        "expected_length: "
                        f"`{details.get('expected_length')}`; "
                        "actual_length: "
                        f"`{details.get('actual_length')}`"
                    )
                    lines.append(f"  - expected_preview: `{details.get('expected_preview', '')}`")
                    lines.append(f"  - actual_preview: `{details.get('actual_preview', '')}`")
        lines.append("")
        return "\n".join(lines)

    lines = [
        "# Publication Bundle Validation",
        "",
        f"Verdict: `{summary['verdict']}`",
        "",
        "## Checks",
        "",
        "| Check | Passed |",
        "|---|---:|",
    ]
    for check, passed in summary.get("checks", {}).items():
        lines.append(f"| `{check}` | `{str(passed).lower()}` |")
    if summary.get("failures"):
        lines.extend(["", "## Failures", ""])
        for failure in summary["failures"]:
            lines.append(f"- `{failure['check']}`: {failure['message']}")
    lines.append("")
    return "\n".join(lines)


def main() -> None:
    args = parse_args()
    summary = validate_batch(args) if args.batch_root is not None else validate_bundle(args)
    write_json(args.summary_json, summary)
    write_text(args.report_md, render_markdown(summary))


if __name__ == "__main__":
    main()
