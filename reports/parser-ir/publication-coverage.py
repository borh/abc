#!/usr/bin/env python3
"""Report full Parser-IR publication coverage across TEI/custom/plaintext targets."""

from __future__ import annotations

import argparse
import hashlib
import json
import pathlib
from collections import Counter
from typing import Any

SCHEMA_VERSION = "ir-publication-coverage-v1"
REQUIRED_PARSERS = ("aozora2html", "aozora-epub3", "aozora-rs", "aozora2", "aozora")
REQUIRED_CUSTOM_CONTRACT_ID = "https://w3id.org/abc/schemas/ir-publication-preservation-v1.json"
PLAINTEXT_POLICY = {
    "plaintext_surface": "visible_body_text",
    "metadata_policy": "exclude_ruby_readings_layout_source_notes_custom_records_warnings_and_provenance",
}
NODE_COVERAGE_CLASSES = {
    "text": "tei_exact",
    "ruby": "tei_exact",
    "gaiji": "tei_exact",
    "editor-note": "tei_policy_projection",
    "emphasis": "tei_policy_projection",
    "heading": "tei_exact",
    "indentation": "tei_policy_projection",
    "page-break": "tei_exact",
    "line-break": "tei_exact",
    "image": "tei_exact",
    "caption": "tei_policy_projection",
    "quote": "tei_policy_projection",
    "source-note": "tei_policy_projection",
    "warigaki": "tei_plus_abc_extension",
    "raw-source": "tei_policy_projection",
}
CONSTRUCT_COVERAGE_CLASSES = {
    "ruby": "tei_exact",
    "raw": "tei_policy_projection",
    "warigaki": "tei_plus_abc_extension",
    "accent": "tei_plus_abc_extension",
    "caption_block": "tei_policy_projection",
    "quote_block": "tei_policy_projection",
    "keigakomi_block": "tei_policy_projection",
    "yokogumi_block": "tei_policy_projection",
    "gaiji.resolved": "tei_exact",
}
CONSTRUCT_CLASS_PRIORITY = {
    "unsupported_gap": 0,
    "tei_plus_abc_extension": 1,
    "custom_sidecar": 2,
    "tei_policy_projection": 3,
    "plaintext_only": 4,
    "tei_exact": 5,
}
CLASS_ORDER = (
    "tei_exact",
    "tei_policy_projection",
    "tei_plus_abc_extension",
    "custom_sidecar",
    "plaintext_only",
    "unsupported_gap",
)


def extract_node_types(schema: dict[str, Any]) -> list[str]:
    defs = schema.get("$defs", {})
    node = defs.get("node", {})
    node_refs = node.get("oneOf", [])
    result: list[str] = []
    for entry in node_refs:
        ref = entry.get("$ref")
        if not isinstance(ref, str) or not ref.startswith("#/$defs/"):
            continue
        def_name = ref.rsplit("/", 1)[-1]
        node_def = defs.get(def_name, {})
        const = (
            node_def.get("properties", {})
            .get("type", {})
            .get("const")
        )
        if const is None:
            for all_of in node_def.get("allOf", []):
                const = (
                    all_of.get("properties", {})
                    .get("type", {})
                    .get("const")
                )
                if const is not None:
                    break
        if isinstance(const, str):
            result.append(const)
    return sorted(set(result))


def count_by_class(items: dict[str, dict[str, Any]]) -> dict[str, int]:
    counts: Counter[str] = Counter(item["class"] for item in items.values())
    return {name: counts.get(name, 0) for name in CLASS_ORDER if counts.get(name, 0)}


def node_target(node_type: str, coverage_class: str) -> str:
    targets = {
        "text": "TEI text node and plaintext text",
        "ruby": "TEI ruby type=furigana; plaintext base text only",
        "gaiji": "TEI g plus charDecl",
        "editor-note": "TEI note; omitted from plaintext",
        "emphasis": "TEI hi with rend/rendition",
        "heading": "TEI head",
        "indentation": "TEI p@rend or seg/div policy",
        "page-break": "TEI pb",
        "line-break": "TEI lb",
        "image": "TEI figure/graphic",
        "caption": "TEI figure/head or typed div/seg policy",
        "quote": "TEI quote or cit policy",
        "source-note": "TEI front/body/back note routing",
        "warigaki": "TEI inline note plus ABC split-line preservation",
        "raw-source": "TEI raw-source segment or ABC recovery record",
    }
    return targets.get(node_type, f"unsupported {coverage_class}")


def node_coverage_from_schema(schema: dict[str, Any]) -> dict[str, Any]:
    by_node_type: dict[str, dict[str, Any]] = {}
    unsupported: list[dict[str, Any]] = []
    for node_type in extract_node_types(schema):
        coverage_class = NODE_COVERAGE_CLASSES.get(node_type, "unsupported_gap")
        entry = {
            "class": coverage_class,
            "target": node_target(node_type, coverage_class),
        }
        by_node_type[node_type] = entry
        if coverage_class == "unsupported_gap":
            unsupported.append({"node_type": node_type, "owner": "abc_custom_schema"})
    return {
        "by_node_type": by_node_type,
        "counts_by_class": count_by_class(by_node_type),
        "unsupported": unsupported,
    }


def construct_from_pointer(pointer: str | None) -> str:
    if not pointer:
        return "unknown"
    for token in (
        "caption_block",
        "quote_block",
        "keigakomi_block",
        "yokogumi_block",
        "warigaki",
        "gaiji.resolved",
        "accent",
        "raw",
    ):
        if token in pointer:
            return token
    if ".ruby" in pointer or "ruby." in pointer:
        return "ruby"
    return pointer.rsplit(".", 1)[-1].replace("[]", "")


def _more_conservative_class(current_class: str | None, candidate_class: str) -> str:
    if current_class is None:
        return candidate_class

    # Lower rank is more conservative / higher priority.
    current_rank = CONSTRUCT_CLASS_PRIORITY.get(current_class, len(CONSTRUCT_CLASS_PRIORITY) + 1)
    candidate_rank = CONSTRUCT_CLASS_PRIORITY.get(candidate_class, len(CONSTRUCT_CLASS_PRIORITY) + 1)
    return current_class if current_rank <= candidate_rank else candidate_class


def source_construct_coverage(mapping: dict[str, Any]) -> dict[str, Any]:
    by_construct: dict[str, dict[str, Any]] = {}
    unsupported: list[dict[str, Any]] = []
    for rule in mapping.get("transform_rule_descriptions", []):
        aat_pointer = rule.get("aat_pointer")
        construct = construct_from_pointer(aat_pointer)
        category = str(rule.get("category") or "unknown")
        coverage_class = CONSTRUCT_COVERAGE_CLASSES.get(construct)
        if category == "UNSUPPORTED":
            coverage_class = "unsupported_gap"
        if coverage_class is None:
            coverage_class = "unsupported_gap"

        if construct not in by_construct:
            by_construct[construct] = {
                "class": coverage_class,
                "rules": 0,
                "categories": {},
                "examples": [],
            }

        entry = by_construct[construct]
        entry["class"] = _more_conservative_class(entry["class"], coverage_class)
        entry["rules"] += 1
        entry["categories"][category] = entry["categories"].get(category, 0) + 1
        if len(entry["examples"]) < 3:
            entry["examples"].append(
                {
                    "rule_id": rule.get("rule_id"),
                    "aat_pointer": aat_pointer,
                    "parser_ir_pointer": rule.get("parser_ir_pointer"),
                    "category": category,
                }
            )
        if coverage_class == "unsupported_gap":
            unsupported.append(
                {
                    "rule_id": rule.get("rule_id"),
                    "aat_pointer": aat_pointer,
                    "parser_ir_pointer": rule.get("parser_ir_pointer"),
                    "category": category,
                    "owner": "parser_ir_schema",
                }
            )
    return {
        "by_construct": dict(sorted(by_construct.items())),
        "counts_by_class": count_by_class(by_construct),
        "unsupported": unsupported,
    }


def source_authority_gate(source: dict[str, Any]) -> dict[str, Any]:
    return {
        "gate_status": source.get("gate_status"),
        "works_scanned": source.get("works_scanned"),
        "unallowlisted_unknown_markers_total": source.get("unallowlisted_unknown_markers_total"),
    }


def source_authority_passed(source: dict[str, Any]) -> bool:
    return (
        source.get("gate_status") == "SOURCE_AUTHORITY_GATE_PASS"
        and source.get("unallowlisted_unknown_markers_total") == 0
    )


def parser_evidence_coverage(matrix: dict[str, Any], source_delta: dict[str, Any]) -> dict[str, Any]:
    observed = Counter()
    for row in matrix.get("rows", []):
        adapter = row.get("selected_aat", {}).get("adapter")
        if adapter:
            observed[adapter] += 1
    delta_coverage = source_delta.get("parser_evidence_coverage", {})
    missing = sorted(parser for parser in REQUIRED_PARSERS if observed.get(parser, 0) == 0)
    verdict = "FIVE_PARSER_EVIDENCE_COMPLETE" if not missing else "FIVE_PARSER_EVIDENCE_INCOMPLETE"
    return {
        "verdict": verdict,
        "required_parsers": list(REQUIRED_PARSERS),
        "observed_rows_by_parser": {parser: observed.get(parser, 0) for parser in REQUIRED_PARSERS},
        "missing_parsers": missing,
        "plain_prose_delta_verdict": delta_coverage.get("verdict"),
    }


def custom_contract_block(custom_contract_schema: pathlib.Path | None) -> dict[str, Any]:
    if custom_contract_schema is None:
        return {
            "verdict": "CUSTOM_CONTRACT_MISSING",
            "schema_id": None,
            "schema_version": None,
            "message": "ABC custom preservation contract has not been supplied to this report.",
        }

    try:
        contract = load_json(custom_contract_schema)
    except json.JSONDecodeError:
        return {
            "verdict": "CUSTOM_CONTRACT_INVALID",
            "schema_id": None,
            "schema_version": None,
            "path": str(custom_contract_schema),
            "message": "ABC custom preservation contract is not valid JSON.",
        }

    contract_id = contract.get("$id") or contract.get("schema_id")
    if contract_id != REQUIRED_CUSTOM_CONTRACT_ID:
        return {
            "verdict": "CUSTOM_CONTRACT_INVALID",
            "schema_id": contract_id,
            "schema_version": contract.get("schema_version"),
            "path": str(custom_contract_schema),
            "message": "ABC custom preservation contract does not identify the required contract schema.",
        }
    return {
        "verdict": "CUSTOM_CONTRACT_PRESENT",
        "schema_id": contract_id,
        "schema_version": contract.get("schema_version"),
        "path": str(custom_contract_schema),
    }


def mapping_block(mapping: dict[str, Any]) -> dict[str, Any]:
    return {
        "mapping_id": mapping.get("mapping_id"),
        "mapping_version": mapping.get("mapping_version"),
        "mapping_hash": document_hash(mapping),
        "mapping_schema_hash": mapping.get("mapping_schema_hash"),
        "target_parser_ir_schema_id": mapping.get("target_parser_ir_schema_id"),
        "target_parser_ir_schema_hash": mapping.get("target_parser_ir_schema_hash"),
        "generated_mapping_rules": len(mapping.get("transform_rule_descriptions", [])),
    }


def unsupported_gaps(node_coverage: dict[str, Any], construct_coverage: dict[str, Any]) -> dict[str, Any]:
    items = []
    items.extend(node_coverage.get("unsupported", []))
    items.extend(construct_coverage.get("unsupported", []))
    return {"count": len(items), "items": items}


def publication_verdict(
    source_passed: bool,
    evidence: dict[str, Any],
    custom_contract: dict[str, Any],
    gaps: dict[str, Any],
) -> str:
    if not source_passed:
        return "IR_PUBLICATION_COVERAGE_BLOCKED_SOURCE_AUTHORITY"
    if evidence.get("verdict") != "FIVE_PARSER_EVIDENCE_COMPLETE":
        return "IR_PUBLICATION_COVERAGE_BLOCKED_INCOMPLETE_PARSER_EVIDENCE"
    if gaps.get("count", 0) > 0:
        return "IR_PUBLICATION_COVERAGE_BLOCKED_UNSUPPORTED_GAPS"
    if custom_contract.get("verdict") != "CUSTOM_CONTRACT_PRESENT":
        return "IR_PUBLICATION_COVERAGE_BLOCKED_CUSTOM_CONTRACT_MISSING"
    return "IR_PUBLICATION_COVERAGE_COMPLETE"


def build_summary(
    parser_schema: dict[str, Any],
    mapping: dict[str, Any],
    source: dict[str, Any],
    matrix: dict[str, Any],
    source_delta: dict[str, Any],
    custom_contract_schema: pathlib.Path | None,
) -> dict[str, Any]:
    node_coverage = node_coverage_from_schema(parser_schema)
    construct_coverage = source_construct_coverage(mapping)
    evidence = parser_evidence_coverage(matrix, source_delta)
    custom_contract = custom_contract_block(custom_contract_schema)
    gaps = unsupported_gaps(node_coverage, construct_coverage)
    source_passed = source_authority_passed(source)
    return {
        "schema_version": SCHEMA_VERSION,
        "verdict": publication_verdict(source_passed, evidence, custom_contract, gaps),
        "source_authority_gate": source_authority_gate(source),
        "parser_evidence_coverage": evidence,
        "parser_ir_schema": {
            "schema_id": parser_schema.get("$id"),
            "schema_hash": mapping.get("target_parser_ir_schema_hash") or parser_schema.get("properties", {}).get("schema_hash", {}).get("const"),
        },
        "tei_profile": {
            "role": "primary_publication_xml",
            "calibration": "tei_eaj_calibration_only",
        },
        "custom_contract": custom_contract,
        "mapping": mapping_block(mapping),
        "node_coverage": node_coverage,
        "source_construct_coverage": construct_coverage,
        "unsupported_gaps": gaps,
        "tei_eaj_calibration": {
            "matrix_schema_version": matrix.get("schema_version"),
            "rows_attempted": matrix.get("totals", {}).get("rows_attempted"),
            "materialization_failed": matrix.get("totals", {}).get("materialization_failed"),
            "role": "calibration_not_source_authority",
        },
        "plaintext_policy": PLAINTEXT_POLICY,
    }


def render_markdown(summary: dict[str, Any]) -> str:
    node_counts = summary["node_coverage"]["counts_by_class"]
    construct_counts = summary["source_construct_coverage"]["counts_by_class"]
    gaps = summary["unsupported_gaps"]
    lines = [
        "# IR Publication Coverage",
        "",
        f"Verdict: `{summary['verdict']}`",
        "",
        "## Node Coverage",
        "",
        "| Class | Node types |",
        "|---|---:|",
    ]
    for name, count in node_counts.items():
        lines.append(f"| `{name}` | {count} |")
    lines.extend(["", "## Source Construct Coverage", "", "| Class | Constructs |", "|---|---:|"])
    for name, count in construct_counts.items():
        lines.append(f"| `{name}` | {count} |")
    lines.extend(["", "## Unsupported gaps", "", f"Count: {gaps['count']}", ""])
    for item in gaps["items"][:20]:
        label = item.get("aat_pointer") or item.get("node_type")
        lines.append(f"- `{label}` owner `{item.get('owner')}`")
    lines.extend([
        "",
        "## Plaintext Policy",
        "",
        f"`{summary['plaintext_policy']['metadata_policy']}`",
        "",
        "## TEI-EAJ Calibration",
        "",
        "TEI-EAJ rows are calibration evidence, not source authority.",
        "",
    ])
    return "\n".join(lines)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--parser-ir-schema", required=True, type=pathlib.Path)
    parser.add_argument("--mapping", required=True, type=pathlib.Path)
    parser.add_argument("--source-summary", required=True, type=pathlib.Path)
    parser.add_argument("--matrix-summary", required=True, type=pathlib.Path)
    parser.add_argument("--source-delta-summary", required=True, type=pathlib.Path)
    parser.add_argument("--custom-contract-schema", type=pathlib.Path)
    parser.add_argument("--summary-json", required=True, type=pathlib.Path)
    parser.add_argument("--report-md", required=True, type=pathlib.Path)
    return parser.parse_args()


def load_json(path: pathlib.Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))


def write_json(path: pathlib.Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def write_text(path: pathlib.Path, value: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(value, encoding="utf-8")


def canonical_json(value: object) -> str:
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":")).replace("/", "\\/")


def document_hash(value: object) -> str:
    return "sha256:" + hashlib.sha256(canonical_json(value).encode("utf-8")).hexdigest()


def main() -> None:
    args = parse_args()
    summary = build_summary(
        parser_schema=load_json(args.parser_ir_schema),
        mapping=load_json(args.mapping),
        source=load_json(args.source_summary),
        matrix=load_json(args.matrix_summary),
        source_delta=load_json(args.source_delta_summary),
        custom_contract_schema=args.custom_contract_schema,
    )
    write_json(args.summary_json, summary)
    write_text(args.report_md, render_markdown(summary))


if __name__ == "__main__":
    main()
