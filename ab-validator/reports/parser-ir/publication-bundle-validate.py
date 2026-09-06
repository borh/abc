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

from jsonschema import Draft202012Validator, FormatChecker

_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from reports.lib.evidence import artifact_record, optional_file_sha256
from reports.lib.io import read_json, write_json
from reports.lib.paths import display_path
from reports.lib.source_region import coverage_passes_source_authority

SCHEMA_VERSION = "publication-bundle-validation-evidence-v2"
BATCH_SCHEMA_VERSION = "publication-bundle-batch-validation-evidence-v2"
PASSED_VERDICT = "PUBLICATION_BUNDLE_VALIDATION_PASSED"
FAILED_VERDICT = "PUBLICATION_BUNDLE_VALIDATION_FAILED"
BATCH_PASSED_VERDICT = "PUBLICATION_BUNDLE_BATCH_VALIDATION_PASSED"
BATCH_FAILED_VERDICT = "PUBLICATION_BUNDLE_BATCH_VALIDATION_FAILED"
ABC_NS = "{https://w3id.org/abc/ns/tei}"
XML_ID = "{http://www.w3.org/XML/1998/namespace}id"
STRUCTURE_CHECK_CANDIDATES = (
    "tei_profile_valid",
    "preservation_schema_valid",
    "tei_manifest_valid",
    "plaintext_manifest_valid",
    "tei_manifest_references_preservation",
    "tei_manifest_references_validation_result",
    "tei_abc_projection_resolves_to_sidecar",
    "preservation_tei_pointers_resolve",
    "preservation_source_pointers_resolve",
    "plaintext_body_only",
)
SUPPORTING_PRECONDITIONS = (
    "parser_ir_schema_valid",
    "source_region_coverage_valid",
    "source_region_sidecar_role_available",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--parser-ir", type=pathlib.Path)
    parser.add_argument("--source-region-summary", type=pathlib.Path)
    parser.add_argument("--parser-ir-schema", required=True, type=pathlib.Path)
    parser.add_argument("--preservation-schema", required=True, type=pathlib.Path)
    parser.add_argument("--validator-identity", required=True, type=pathlib.Path)
    parser.add_argument("--publication-dir", type=pathlib.Path)
    parser.add_argument("--batch-root", type=pathlib.Path)
    parser.add_argument("--batch-scope", default="representative")
    parser.add_argument("--abc-commit")
    parser.add_argument("--command", default="not recorded")
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
    return manifest.get("content", {}).get("content_hash") == optional_file_sha256(path)


def sidecar_hash_matches(
    manifest: dict[str, Any],
    role: str,
    path_hint: str,
    path: pathlib.Path,
) -> bool:
    sidecar = sidecar_by_role(manifest, role)
    return bool(
        sidecar
        and sidecar.get("path_hint") == path_hint
        and sidecar.get("hash") == optional_file_sha256(path)
    )


def source_region_valid(source_region: dict[str, Any]) -> bool:
    return coverage_passes_source_authority(source_region)


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


def validate_schema(instance: object, contract: dict[str, Any]) -> list[str]:
    validator = Draft202012Validator(contract, format_checker=FormatChecker())
    return [
        f"{'.'.join(str(part) for part in error.absolute_path) or '$'}: {error.message}"
        for error in sorted(validator.iter_errors(instance), key=lambda item: list(item.path))
    ]


def _join_node_errors(node: object, path: str) -> list[str]:
    if not isinstance(node, dict):
        return [f"{path} must be an object"]
    node_type = node.get("type")
    known = {
        "text",
        "ruby",
        "gaiji",
        "emphasis",
        "heading",
        "layout-span",
        "indentation",
        "quote",
        "caption",
        "editor-note",
        "line-break",
        "page-break",
        "source-note",
    }
    errors = []
    if node_type not in known:
        errors.append(f"{path}.type is not recognized")
        return errors
    pointer = node.get("source_pointer")
    if pointer is not None and (not isinstance(pointer, str) or not pointer):
        errors.append(f"{path}.source_pointer must be a non-empty string")
    if node_type in {"text", "indentation", "quote", "caption", "editor-note", "source-note"}:
        if not isinstance(node.get("text"), str):
            errors.append(f"{path}.text must be a string")
    elif node_type == "ruby":
        ruby = node.get("ruby")
        if not isinstance(ruby, dict) or not isinstance(ruby.get("base"), str):
            errors.append(f"{path}.ruby.base must be a string")
    elif node_type == "gaiji":
        if not isinstance(node.get("gaiji"), dict):
            errors.append(f"{path}.gaiji must be an object")
    elif node_type in {"emphasis", "heading", "layout-span"}:
        children = node.get("inline_children")
        if children is not None:
            if not isinstance(children, list):
                errors.append(f"{path}.inline_children must be an array")
            else:
                for offset, child in enumerate(children):
                    errors.extend(_join_node_errors(child, f"{path}.inline_children[{offset}]"))
        elif not isinstance(node.get("text"), str):
            errors.append(f"{path} requires string text or inline_children")
    return errors


def publication_join_input_errors(parser_ir: object) -> list[str]:
    if not isinstance(parser_ir, dict):
        return ["parser-IR must be an object"]
    nodes = parser_ir.get("nodes")
    if not isinstance(nodes, list):
        return ["parser-IR nodes must be an array"]
    errors = []
    for offset, node in enumerate(nodes):
        errors.extend(_join_node_errors(node, f"nodes[{offset}]"))
    return errors


def publication_counts(
    parser_ir: dict[str, Any], preservation: dict[str, Any], tei_root: ET.Element
) -> dict[str, Any]:
    records = preservation.get("records", [])
    records = records if isinstance(records, list) else []
    constructs: dict[str, int] = {}
    for record in records:
        if isinstance(record, dict) and isinstance(record.get("construct"), str):
            construct = record["construct"]
            constructs[construct] = constructs.get(construct, 0) + 1
    tei_pointers = [record.get("tei_pointer") for record in records if isinstance(record, dict)]
    source_pointers = [
        record.get("source_pointer") for record in records if isinstance(record, dict)
    ]
    return {
        "preservation_records": len(records),
        "tei_preservation_references": len(preservation_record_refs(tei_root)),
        "non_null_tei_pointers": sum(pointer is not None for pointer in tei_pointers),
        "non_null_source_pointers": sum(pointer is not None for pointer in source_pointers),
        "by_construct": dict(sorted(constructs.items())),
    }


def validate_values(
    *,
    parser_ir: object,
    preservation: object,
    tei_root: ET.Element,
    plaintext: str,
    tei_manifest: dict[str, Any],
    plaintext_manifest: dict[str, Any],
    tei_validation: dict[str, Any],
    source_region: dict[str, Any] | None,
    parser_ir_schema: dict[str, Any],
    preservation_schema: dict[str, Any],
    paths: dict[str, pathlib.Path] | None = None,
) -> dict[str, Any]:
    parser_ir_errors = validate_schema(parser_ir, parser_ir_schema)
    preservation_errors = validate_schema(preservation, preservation_schema)
    supporting = {
        "parser_ir_schema_valid": not parser_ir_errors,
        "source_region_coverage_valid": bool(
            source_region is not None and source_region_valid(source_region)
        ),
        "source_region_sidecar_role_available": bool(
            source_region is not None and source_region_valid(source_region)
        ),
    }
    join_errors = publication_join_input_errors(parser_ir)
    join_input = {
        "status": "valid" if not join_errors else "invalid",
        "errors": join_errors,
    }
    if join_errors or not isinstance(parser_ir, dict) or not isinstance(preservation, dict):
        return {
            "structure_check_candidates": None,
            "supporting_preconditions": supporting,
            "join_input": join_input,
            "counts": None,
            "schema_errors": {
                "parser_ir": parser_ir_errors,
                "preservation": preservation_errors,
            },
        }

    record_ids = {
        record.get("record_id")
        for record in preservation.get("records", [])
        if isinstance(record, dict) and isinstance(record.get("record_id"), str)
    }
    tei_ids = tei_id_set(tei_root)
    source_pointers = collect_values_by_key(parser_ir, "source_pointer")
    checks = {
        "tei_profile_valid": tei_validation.get("status") == "passed",
        "preservation_schema_valid": not preservation_errors,
        "tei_manifest_valid": bool(
            paths
            and tei_manifest.get("artifact_kind") == "tei"
            and tei_manifest.get("validation_status") == "passed"
            and content_hash_matches(tei_manifest, paths["tei"])
        ),
        "plaintext_manifest_valid": bool(
            paths
            and plaintext_manifest.get("artifact_kind") == "plaintext"
            and plaintext_manifest.get("validation_status") == "passed"
            and content_hash_matches(plaintext_manifest, paths["plaintext"])
        ),
        "tei_manifest_references_preservation": bool(
            paths
            and sidecar_hash_matches(
                tei_manifest, "preservation", "preservation.json", paths["preservation"]
            )
        ),
        "tei_manifest_references_validation_result": bool(
            paths
            and sidecar_hash_matches(
                tei_manifest,
                "validation-result",
                "tei-validation-result.json",
                paths["tei_validation_result"],
            )
        ),
        "tei_abc_projection_resolves_to_sidecar": all(
            ref in record_ids for ref in preservation_record_refs(tei_root)
        ),
        "preservation_tei_pointers_resolve": all(
            tei_pointer_resolves(record.get("tei_pointer"), tei_ids)
            for record in preservation.get("records", [])
            if isinstance(record, dict)
        ),
        "preservation_source_pointers_resolve": all(
            record.get("source_pointer") in source_pointers
            for record in preservation.get("records", [])
            if isinstance(record, dict) and record.get("source_pointer") is not None
        ),
        "plaintext_body_only": plaintext_body_only(parser_ir, plaintext),
    }
    return {
        "structure_check_candidates": checks,
        "supporting_preconditions": supporting,
        "join_input": join_input,
        "counts": publication_counts(parser_ir, preservation, tei_root),
        "schema_errors": {
            "parser_ir": parser_ir_errors,
            "preservation": preservation_errors,
        },
    }


def validate_bundle(args: argparse.Namespace) -> dict[str, Any]:
    publication_dir = args.publication_dir
    paths = {
        "parser_ir": args.parser_ir,
        "tei": publication_dir / "tei.xml",
        "plaintext": publication_dir / "plain.txt",
        "preservation": publication_dir / "preservation.json",
        "tei_manifest": publication_dir / "tei.manifest.json",
        "plaintext_manifest": publication_dir / "plaintext.manifest.json",
        "tei_validation_result": publication_dir / "tei-validation-result.json",
    }
    if args.source_region_summary is not None:
        paths["source_region_coverage"] = args.source_region_summary
    validated_bundle = {
        key: artifact_record(path) for key, path in paths.items() if key != "tei_validation_result"
    }
    failures: list[dict[str, Any]] = []

    try:
        parser_ir = read_json(args.parser_ir)
        source_region = (
            read_json(args.source_region_summary)
            if args.source_region_summary is not None
            else None
        )
        parser_ir_schema = read_json(args.parser_ir_schema)
        preservation_schema = read_json(args.preservation_schema)
        validator_identity = read_json(args.validator_identity)
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
            "structure_check_candidates": None,
            "supporting_preconditions": {},
            "join_input": {"status": "invalid", "errors": ["bundle unreadable"]},
            "counts": None,
            "failures": [{"check": "bundle_readable", "message": str(error)}],
        }

    detailed = validate_values(
        parser_ir=parser_ir,
        preservation=preservation,
        tei_root=tei_root,
        plaintext=plaintext,
        tei_manifest=tei_manifest,
        plaintext_manifest=plaintext_manifest,
        tei_validation=tei_validation,
        source_region=source_region,
        parser_ir_schema=parser_ir_schema,
        preservation_schema=preservation_schema,
        paths=paths,
    )
    checks = detailed["structure_check_candidates"] or {}
    plaintext_details = plaintext_mismatch_details(parser_ir, plaintext)

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
    for check, passed in {
        **(detailed["supporting_preconditions"] or {}),
        **checks,
    }.items():
        if not passed:
            details = plaintext_details if check == "plaintext_body_only" else None
            add_failure(failures, check, messages[check], details)

    return {
        "schema_version": SCHEMA_VERSION,
        "verdict": (
            PASSED_VERDICT
            if detailed["join_input"]["status"] == "valid" and not failures
            else FAILED_VERDICT
        ),
        "validator": "ab-validator publication-bundle-validate",
        "command": args.command,
        "abc_commit": args.abc_commit,
        "validated_bundle": validated_bundle,
        "validator_identity": {
            "validator_id": validator_identity.get("validator_id"),
            "validator_semantics_hash": validator_identity.get("validator_semantics_hash"),
        },
        "parser_ir_schema_hash": optional_file_sha256(args.parser_ir_schema),
        "preservation_schema_hash": optional_file_sha256(args.preservation_schema),
        **detailed,
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
            parser_ir_schema=args.parser_ir_schema,
            preservation_schema=args.preservation_schema,
            validator_identity=args.validator_identity,
        )
        row_summary = validate_bundle(row_args)
        row_failures = row_summary.get("failures", [])
        row = {
            "row_id": row_id,
            "row_dir": display_path(parser_ir.parent),
            "verdict": row_summary.get("verdict"),
            "validated_bundle": row_summary.get("validated_bundle", {}),
            "structure_check_candidates": row_summary.get("structure_check_candidates"),
            "supporting_preconditions": row_summary.get("supporting_preconditions", {}),
            "join_input": row_summary.get("join_input", {}),
            "counts": row_summary.get("counts"),
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

    structure_checks = {
        check: bool(rows)
        and all((row.get("structure_check_candidates") or {}).get(check) is True for row in rows)
        for check in STRUCTURE_CHECK_CANDIDATES
    }
    supporting_checks = {
        check: bool(rows)
        and all(row.get("supporting_preconditions", {}).get(check) is True for row in rows)
        for check in SUPPORTING_PRECONDITIONS
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
        "structure_check_candidates": structure_checks,
        "supporting_preconditions": supporting_checks,
        "join_input": {
            "status": (
                "valid"
                if bool(rows)
                and all(row.get("join_input", {}).get("status") == "valid" for row in rows)
                else "invalid"
            )
        },
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
            "## Structure check candidates",
            "",
            "| Check | Passed |",
            "|---|---:|",
        ]
        for check, passed in summary.get("structure_check_candidates", {}).items():
            lines.append(f"| `{check}` | `{str(passed).lower()}` |")
        lines.extend(
            ["", "## Join input", "", f"Status: `{summary.get('join_input', {}).get('status')}`"]
        )
        lines.extend(["", "## Supporting preconditions", "", "| Check | Passed |", "|---|---:|"])
        for check, passed in summary.get("supporting_preconditions", {}).items():
            lines.append(f"| `{check}` | `{str(passed).lower()}` |")
        lines.extend(["", "## Counts", "", f"Rows: `{len(summary.get('rows', []))}`"])
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
        "## Structure check candidates",
        "",
        "| Check | Passed |",
        "|---|---:|",
    ]
    for check, passed in (summary.get("structure_check_candidates") or {}).items():
        lines.append(f"| `{check}` | `{str(passed).lower()}` |")
    lines.extend(
        ["", "## Join input", "", f"Status: `{summary.get('join_input', {}).get('status')}`"]
    )
    lines.extend(["", "## Supporting preconditions", "", "| Check | Passed |", "|---|---:|"])
    for check, passed in summary.get("supporting_preconditions", {}).items():
        lines.append(f"| `{check}` | `{str(passed).lower()}` |")
    lines.extend(
        [
            "",
            "## Counts",
            "",
            "```json",
            json.dumps(summary.get("counts"), ensure_ascii=False, sort_keys=True),
            "```",
        ]
    )
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
