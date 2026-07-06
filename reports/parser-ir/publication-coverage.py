#!/usr/bin/env python3
"""Report full Parser-IR publication coverage across TEI/custom/plaintext targets."""

from __future__ import annotations

import argparse
import hashlib
import json
import pathlib
import re
from collections import Counter
from typing import Any

SCHEMA_VERSION = "ir-publication-coverage-v1"
REQUIRED_PARSERS = ("aozora2html", "aozora-epub3", "aozora-rs", "aozora2", "aozora")
REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
ABC_PRESERVATION_SCHEMA_ID = "https://w3id.org/abc/schemas/parser-ir-publication-preservation.schema.json"
ABC_PRESERVATION_SCHEMA_VERSION = "0.2.0"
TRUSTED_ABC_PRESERVATION_SCHEMA_PATH = (
    REPO_ROOT / "data/abc-schemas/schemas/parser-ir-publication-preservation.schema.json"
).resolve()
TEI_PROFILE_RECORD_CLASS = "tei_profile_projection"
TEI_PROFILE_FAMILIES = {
    "accent",
    "figure_metadata",
    "heading_jisage_structure",
    "style_rendition",
}
TEI_PROFILE_CONSTRUCTS = set(TEI_PROFILE_FAMILIES)
TEI_PROFILE_LANES = {"tei_policy_projection", "tei_plus_abc_extension"}
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
    "layout-span": "tei_policy_projection",
    "warigaki": "tei_plus_abc_extension",
    "raw-source": "tei_policy_projection",
}
CONSTRUCT_COVERAGE_CLASSES = {
    "caption": "tei_policy_projection",
    "image": "tei_exact",
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
FIELD_COVERAGE_SPECS = {
    "gaiji.raw_marker": {
        "class": "tei_policy_projection",
        "target": "TEI g/charDecl raw-marker preservation policy",
        "kind": "node_field",
        "schema_locators": [("gaijiNode", ("gaiji", "raw_marker"))],
    },
    "gaiji.reference": {
        "class": "tei_exact",
        "target": "TEI g/charDecl reference linkage",
        "kind": "node_field",
        "schema_locators": [("gaijiNode", ("gaiji", "reference"))],
    },
    "gaiji.unicode": {
        "class": "tei_exact",
        "target": "TEI g/charDecl Unicode value",
        "kind": "node_field",
        "schema_locators": [("gaijiNode", ("gaiji", "unicode"))],
    },
    "gaiji.resolved": {
        "class": "tei_plus_abc_extension",
        "target": "TEI visible glyph plus ABC resolution-status preservation",
        "kind": "node_field",
        "schema_locators": [("gaijiNode", ("gaiji", "resolved"))],
    },
    "ruby.base": {
        "class": "tei_exact",
        "target": "TEI ruby base text",
        "kind": "node_field",
        "schema_locators": [("rubyNode", ("ruby", "base"))],
    },
    "ruby.reading": {
        "class": "tei_exact",
        "target": "TEI ruby reading text",
        "kind": "node_field",
        "schema_locators": [("rubyNode", ("ruby", "reading"))],
    },
    "ruby.direction": {
        "class": "tei_policy_projection",
        "target": "TEI ruby placement policy for left/right readings",
        "kind": "node_field",
        "schema_locators": [("rubyNode", ("ruby", "direction"))],
    },
    "heading.level": {
        "class": "tei_policy_projection",
        "target": "TEI section/head level policy",
        "kind": "node_field",
        "schema_locators": [("headingNode", ("level",))],
    },
    "emphasis.inline_children": {
        "class": "tei_policy_projection",
        "target": "TEI hi content with nested inline children policy",
        "kind": "node_field",
        "schema_locators": [("emphasisNode", ("inline_children",))],
    },
    "paragraph.layout": {
        "class": "tei_policy_projection",
        "target": "TEI p@rend paragraph layout policy",
        "kind": "paragraph_field",
        "schema_locators": [("paragraph", ("layout",))],
    },
    "paragraph.node_range": {
        "class": "custom_sidecar",
        "target": "ABC sidecar node-range traceability",
        "kind": "paragraph_field",
        "schema_locators": [("paragraph", ("node_range",))],
    },
    "paragraph.role": {
        "class": "tei_policy_projection",
        "target": "TEI paragraph routing/body-vs-note role policy",
        "kind": "paragraph_field",
        "schema_locators": [("paragraph", ("role",))],
    },
    "source-note.placement": {
        "class": "tei_policy_projection",
        "target": "TEI front/body/back source-note routing",
        "kind": "node_field",
        "schema_locators": [("sourceNoteNode", ("placement",))],
    },
    "source-note.classification": {
        "class": "custom_sidecar",
        "target": "ABC sidecar source-note provenance classification",
        "kind": "node_field",
        "schema_locators": [("sourceNoteNode", ("classification",))],
    },
    "caption.target": {
        "class": "tei_policy_projection",
        "target": "TEI caption-to-figure association policy",
        "kind": "node_field",
        "schema_locators": [("captionNode", ("target",))],
    },
    "quote.marker_type": {
        "class": "tei_policy_projection",
        "target": "TEI quote/cit marker interpretation policy",
        "kind": "node_field",
        "schema_locators": [("quoteNode", ("marker_type",))],
    },
    "mapping.identity": {
        "class": "custom_sidecar",
        "target": "ABC sidecar mapping id/version/hash linkage",
        "kind": "contract_field",
    },
    "divergence.records": {
        "class": "custom_sidecar",
        "target": "ABC sidecar divergence record set",
        "kind": "contract_field",
    },
    "source.pointer": {
        "class": "custom_sidecar",
        "target": "ABC sidecar source pointer linkage",
        "kind": "contract_field",
        "schema_locators": [
            ("paragraph", ("source_pointer",)),
            ("sourceNoteNode", ("source_pointer",)),
        ],
    },
}
DIAGNOSTIC_COVERAGE_SPECS = {
    "warnings[]": {
        "class": "custom_sidecar",
        "target": "ABC sidecar warning record collection",
        "kind": "diagnostic_collection",
    },
    "warnings[].code": {
        "class": "custom_sidecar",
        "target": "ABC sidecar warning code preservation",
        "kind": "diagnostic_field",
    },
    "warnings[].severity": {
        "class": "custom_sidecar",
        "target": "ABC sidecar warning severity preservation",
        "kind": "diagnostic_field",
    },
    "warnings[].message": {
        "class": "custom_sidecar",
        "target": "ABC sidecar warning message preservation",
        "kind": "diagnostic_field",
    },
    "warnings[].span": {
        "class": "custom_sidecar",
        "target": "ABC sidecar warning span preservation",
        "kind": "diagnostic_field",
    },
    "errors[]": {
        "class": "custom_sidecar",
        "target": "ABC sidecar error record collection",
        "kind": "diagnostic_collection",
    },
    "errors[].code": {
        "class": "custom_sidecar",
        "target": "ABC sidecar error code preservation",
        "kind": "diagnostic_field",
    },
    "errors[].severity": {
        "class": "custom_sidecar",
        "target": "ABC sidecar error severity preservation",
        "kind": "diagnostic_field",
    },
    "errors[].message": {
        "class": "custom_sidecar",
        "target": "ABC sidecar error message preservation",
        "kind": "diagnostic_field",
    },
    "errors[].span": {
        "class": "custom_sidecar",
        "target": "ABC sidecar error span preservation",
        "kind": "diagnostic_field",
    },
}
KNOWN_COMPOUND_CONSTRUCTS = ("gaiji.resolved",)
KNOWN_CONSTRUCT_SEGMENTS = (
    "caption_block",
    "caption",
    "image",
    "figure",
    "quote_block",
    "keigakomi_block",
    "yokogumi_block",
    "warigaki",
    "accent",
    "raw",
    "ruby",
)
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
DIRECT_POINTER_PATTERN = re.compile(r"^[A-Za-z0-9_-]+(?:\.[A-Za-z0-9_-]+)*$")
OBSERVED_OCCURRENCES_PATTERN = re.compile(r"\bObserved\s+(\d+)\s+occurrences\b", re.IGNORECASE)
CUSTOM_SCHEMA_OWNER = "custom_schema"
CLOSURE_FAMILIES = {
    "span_coordinates": {
        "closure_lane": "custom_sidecar",
        "owner": CUSTOM_SCHEMA_OWNER,
        "admission_gate": "ABC custom contract preserves span coordinates or explicit IR pointer span records.",
    },
    "style_rendition": {
        "closure_lane": "tei_policy_projection",
        "owner": "policy",
        "admission_gate": "ABC TEI profile declares style/rendition vocabulary and sidecar preserves exact source marker where needed.",
    },
    "accent": {
        "closure_lane": "tei_plus_abc_extension",
        "owner": "policy",
        "admission_gate": "ABC TEI profile declares accent rendition vocabulary and custom contract preserves original accent code.",
    },
    "source_identity": {
        "closure_lane": "custom_sidecar",
        "owner": CUSTOM_SCHEMA_OWNER,
        "admission_gate": "ABC custom contract and manifest linkage preserve source identity and normalization fields.",
    },
    "provenance_metrics": {
        "closure_lane": "custom_sidecar",
        "owner": CUSTOM_SCHEMA_OWNER,
        "admission_gate": "ABC custom contract includes producer metrics or superseding validation/admission verdicts.",
    },
    "figure_metadata": {
        "closure_lane": "tei_policy_projection",
        "owner": "policy",
        "admission_gate": "ABC TEI profile maps figure dimensions/classes and custom contract preserves exact source class when TEI rend is not exact.",
    },
    "gaiji_unresolved_reason": {
        "closure_lane": "custom_sidecar",
        "owner": CUSTOM_SCHEMA_OWNER,
        "admission_gate": "ABC custom contract preserves gaiji resolution diagnostics.",
    },
    "heading_jisage_structure": {
        "closure_lane": "tei_policy_projection",
        "owner": "policy",
        "admission_gate": "ABC TEI profile and renderer policy admit parser-IR heading, indentation, and paragraph layout projection for heading and jisage structures.",
    },
    "heading_inline_content": {
        "closure_lane": "parser_ir_schema_delta",
        "owner": "parser_ir_schema",
        "admission_gate": "Parser-IR schema represents inline children inside headings so gaiji/ruby/style facts in head text can be preserved exactly.",
    },
    "font_tcy": {
        "closure_lane": "parser_ir_schema_delta",
        "owner": "parser_ir_schema",
        "admission_gate": "Parser-IR schema represents font_size and tcy, ABC profile declares rendition values, and custom contract preserves marker identity.",
    },
    "keigakomi_yokogumi": {
        "closure_lane": "parser_ir_schema_delta",
        "owner": "parser_ir_schema",
        "admission_gate": "Parser-IR schema represents inline/block layout containers and ABC profile names keigakomi/yokogumi TEI vocabulary.",
    },
}


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


def count_values(items: list[dict[str, Any]], key: str) -> dict[str, int]:
    counts: Counter[str] = Counter()
    for item in items:
        value = item.get(key)
        if isinstance(value, str) and value:
            counts[value] += 1
    return dict(sorted(counts.items()))


def merged_properties(schema_node: dict[str, Any]) -> dict[str, Any]:
    properties: dict[str, Any] = {}
    for all_of in schema_node.get("allOf", []):
        if isinstance(all_of, dict):
            properties.update(all_of.get("properties", {}))
    properties.update(schema_node.get("properties", {}))
    return properties


def schema_locator_exists(schema: dict[str, Any], def_name: str, segments: tuple[str, ...]) -> bool:
    current = schema.get("$defs", {}).get(def_name)
    if not isinstance(current, dict):
        return False

    properties = merged_properties(current)
    for segment in segments:
        current = properties.get(segment)
        if not isinstance(current, dict):
            return False
        properties = current.get("properties", {})
    return True


def format_schema_locator(def_name: str, segments: tuple[str, ...]) -> str:
    return "#/$defs/" + "/properties/".join((def_name, *segments))


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
            unsupported.append({"node_type": node_type, "owner": CUSTOM_SCHEMA_OWNER})
    return {
        "by_node_type": by_node_type,
        "counts_by_class": count_by_class(by_node_type),
        "unsupported": unsupported,
    }


def field_coverage_from_schema(schema: dict[str, Any]) -> dict[str, Any]:
    by_field: dict[str, dict[str, Any]] = {}
    unsupported: list[dict[str, Any]] = []
    for field_name, spec in FIELD_COVERAGE_SPECS.items():
        locators = spec.get("schema_locators", [])
        entry = {
            "class": spec["class"],
            "target": spec["target"],
            "kind": spec["kind"],
        }
        if locators:
            entry["defined_in_schema"] = any(
                schema_locator_exists(schema, def_name, segments)
                for def_name, segments in locators
            )
            entry["schema_paths"] = [
                format_schema_locator(def_name, segments)
                for def_name, segments in locators
            ]
        by_field[field_name] = entry
        if spec["class"] == "unsupported_gap":
            unsupported.append({"field": field_name, "owner": CUSTOM_SCHEMA_OWNER})
    return {
        "by_field": by_field,
        "counts_by_class": count_by_class(by_field),
        "unsupported": unsupported,
    }


def diagnostic_field_key(pointer: Any) -> str | None:
    if not isinstance(pointer, str):
        return None
    candidate = pointer.strip()
    if candidate in DIAGNOSTIC_COVERAGE_SPECS:
        return candidate
    for prefix in ("warnings[].span", "errors[].span"):
        if candidate == prefix or candidate.startswith(prefix + "."):
            return prefix
    return None


def field_key_for_pointer(pointer: Any) -> str | None:
    if not isinstance(pointer, str):
        return None
    candidate = pointer.strip()
    if not DIRECT_POINTER_PATTERN.fullmatch(candidate):
        return None
    if candidate in FIELD_COVERAGE_SPECS:
        return candidate
    return None


def diagnostic_coverage(mapping: dict[str, Any]) -> dict[str, Any]:
    by_field: dict[str, dict[str, Any]] = {}
    unsupported: list[dict[str, Any]] = []
    for field_name, spec in DIAGNOSTIC_COVERAGE_SPECS.items():
        by_field[field_name] = {
            "class": spec["class"],
            "target": spec["target"],
            "kind": spec["kind"],
            "mapped_rules": 0,
            "examples": [],
        }
        if spec["class"] == "unsupported_gap":
            unsupported.append({"field": field_name, "owner": CUSTOM_SCHEMA_OWNER})

    for rule in mapping.get("transform_rule_descriptions", []):
        field_name = diagnostic_field_key(rule.get("parser_ir_pointer"))
        if field_name is None:
            continue
        entry = by_field[field_name]
        entry["mapped_rules"] += 1
        if len(entry["examples"]) < 3:
            entry["examples"].append(
                {
                    "rule_id": rule.get("rule_id"),
                    "aat_pointer": rule.get("aat_pointer"),
                    "parser_ir_pointer": rule.get("parser_ir_pointer"),
                    "category": rule.get("category"),
                }
            )

    return {
        "by_field": by_field,
        "counts_by_class": count_by_class(by_field),
        "unsupported": unsupported,
    }


def construct_from_pointer(pointer: str | None) -> str:
    if not pointer:
        return "unknown"
    segments = [segment[:-2] if segment.endswith("[]") else segment for segment in pointer.split(".")]
    for token in KNOWN_COMPOUND_CONSTRUCTS:
        parts = token.split(".")
        for start in range(len(segments) - len(parts) + 1):
            if segments[start : start + len(parts)] == parts:
                return token
    for token in KNOWN_CONSTRUCT_SEGMENTS:
        if token in segments:
            return token
    return segments[-1] if segments else "unknown"


def normalize_direct_pointer(pointer: Any) -> str | None:
    if not isinstance(pointer, str):
        return None
    candidate = pointer.strip()
    if not DIRECT_POINTER_PATTERN.fullmatch(candidate):
        return None
    return candidate


def construct_from_rule(rule: dict[str, Any]) -> str:
    parser_pointer = normalize_direct_pointer(rule.get("parser_ir_pointer"))
    if parser_pointer is not None:
        construct = construct_from_pointer(parser_pointer)
        if construct != "unknown":
            return construct
    return construct_from_pointer(rule.get("aat_pointer"))


def _more_conservative_class(current_class: str | None, candidate_class: str) -> str:
    if current_class is None:
        return candidate_class

    # Lower rank is more conservative / higher priority.
    current_rank = CONSTRUCT_CLASS_PRIORITY.get(current_class, len(CONSTRUCT_CLASS_PRIORITY) + 1)
    candidate_rank = CONSTRUCT_CLASS_PRIORITY.get(candidate_class, len(CONSTRUCT_CLASS_PRIORITY) + 1)
    return current_class if current_rank <= candidate_rank else candidate_class


def unsupported_owner(rule: dict[str, Any], construct: str) -> str:
    del construct
    category = str(rule.get("category") or "").upper()
    owners = {
        "UNSUPPORTED": "parser_ir_schema",
        "LOSS": CUSTOM_SCHEMA_OWNER,
        "STRUCTURAL": "aat_to_parser_ir_converter",
        "AMBIGUITY": "policy",
    }
    return owners.get(category, "policy")


def unsupported_gap_prevalence(rule: dict[str, Any]) -> tuple[int | None, str]:
    description = rule.get("description")
    if isinstance(description, str):
        match = OBSERVED_OCCURRENCES_PATTERN.search(description)
        if match is not None:
            return int(match.group(1)), "rule_description"
    return None, "unavailable"


def source_construct_coverage(mapping: dict[str, Any]) -> dict[str, Any]:
    by_construct: dict[str, dict[str, Any]] = {}
    unsupported: list[dict[str, Any]] = []
    for rule in mapping.get("transform_rule_descriptions", []):
        if diagnostic_field_key(rule.get("parser_ir_pointer")) is not None:
            continue
        if field_key_for_pointer(rule.get("parser_ir_pointer")) is not None:
            continue
        aat_pointer = rule.get("aat_pointer")
        construct = construct_from_rule(rule)
        category = str(rule.get("category") or "unknown")
        coverage_class = CONSTRUCT_COVERAGE_CLASSES.get(construct)
        if category == "UNSUPPORTED" and coverage_class is None:
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
            observed_occurrences, prevalence_source = unsupported_gap_prevalence(rule)
            unsupported.append(
                {
                    "rule_id": rule.get("rule_id"),
                    "aat_pointer": aat_pointer,
                    "parser_ir_pointer": rule.get("parser_ir_pointer"),
                    "category": category,
                    "owner": unsupported_owner(rule, construct),
                    "observed_occurrences": observed_occurrences,
                    "prevalence_source": prevalence_source,
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
    qualified_rows = 0
    for row in matrix.get("rows", []):
        adapter = row.get("selected_aat", {}).get("adapter")
        parser_ir = row.get("parser_ir", {})
        materialization = row.get("materialization")
        if not adapter:
            continue
        if not isinstance(parser_ir.get("schema_hash"), str) or not parser_ir.get("schema_hash"):
            continue
        node_count = parser_ir.get("nodes")
        if isinstance(node_count, bool) or not isinstance(node_count, (int, float)):
            continue
        if isinstance(materialization, dict) and materialization.get("status") != "passed":
            continue
        observed[adapter] += 1
        qualified_rows += 1
    delta_coverage = source_delta.get("parser_evidence_coverage", {})
    delta_missing = {
        parser
        for parser in delta_coverage.get("missing_parsers", [])
        if isinstance(parser, str) and parser in REQUIRED_PARSERS
    }
    missing = sorted(
        {
            *(parser for parser in REQUIRED_PARSERS if observed.get(parser, 0) == 0),
            *delta_missing,
        }
    )
    delta_verdict = delta_coverage.get("verdict")
    verdict = (
        "FIVE_PARSER_EVIDENCE_COMPLETE"
        if not missing and delta_verdict != "FIVE_PARSER_EVIDENCE_INCOMPLETE"
        else "FIVE_PARSER_EVIDENCE_INCOMPLETE"
    )
    return {
        "verdict": verdict,
        "required_parsers": list(REQUIRED_PARSERS),
        "observed_rows_by_parser": {parser: observed.get(parser, 0) for parser in REQUIRED_PARSERS},
        "missing_parsers": missing,
        "qualified_rows": qualified_rows,
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
            "path": display_path(custom_contract_schema),
            "message": "ABC custom preservation contract is not valid JSON.",
        }
    except OSError:
        return {
            "verdict": "CUSTOM_CONTRACT_MISSING",
            "schema_id": None,
            "schema_version": None,
            "path": display_path(custom_contract_schema),
            "message": "ABC custom preservation contract path was supplied but could not be read.",
        }

    contract_id = None
    contract_version = None
    if isinstance(contract, dict):
        contract_id = contract.get("schema_id") or contract.get("$id")
        contract_version = (
            contract.get("schema_version")
            or contract.get("properties", {}).get("schema_version", {}).get("const")
        )
    record_classes = sorted(schema_enum_values(contract, ("$defs", "record", "properties", "class", "enum")))
    coverage_classes = sorted(
        schema_enum_values(contract, ("properties", "coverage", "properties", "classes", "items", "enum"))
    )
    constructs = sorted(schema_enum_values(contract, ("$defs", "record", "properties", "construct", "enum")))
    contract_hash = document_hash(contract)
    trusted_schema_hash = trusted_custom_contract_hash()
    trusted_schema_path_match = custom_contract_schema.resolve() == TRUSTED_ABC_PRESERVATION_SCHEMA_PATH
    if (
        contract_id == ABC_PRESERVATION_SCHEMA_ID
        and contract_version == ABC_PRESERVATION_SCHEMA_VERSION
        and trusted_schema_path_match
        and contract_hash == trusted_schema_hash
    ):
        return {
            "verdict": "CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION",
            "schema_id": contract_id,
            "schema_version": contract_version,
            "path": display_path(custom_contract_schema),
            "contract_hash": contract_hash,
            "trusted_schema_path": display_path(TRUSTED_ABC_PRESERVATION_SCHEMA_PATH),
            "trusted_schema_hash": trusted_schema_hash,
            "trusted_schema_path_match": trusted_schema_path_match,
            "record_classes": record_classes,
            "coverage_classes": coverage_classes,
            "constructs": constructs,
            "message": "ABC-owned parser-IR publication preservation schema is available and admits custom-sidecar publication facts.",
        }
    return {
        "verdict": "CUSTOM_CONTRACT_CANDIDATE_PROVIDED",
        "schema_id": contract_id,
        "schema_version": contract_version,
        "path": display_path(custom_contract_schema),
        "contract_hash": contract_hash,
        "trusted_schema_path": display_path(TRUSTED_ABC_PRESERVATION_SCHEMA_PATH),
        "trusted_schema_hash": trusted_schema_hash,
        "trusted_schema_path_match": trusted_schema_path_match,
        "record_classes": record_classes,
        "coverage_classes": coverage_classes,
        "constructs": constructs,
        "message": "Readable contract candidate recorded as evidence only; ABC-owned integration must confirm the publication contract before admission can unblock.",
    }


def schema_enum_values(schema: Any, path: tuple[str, ...]) -> set[str]:
    cursor = schema
    for key in path:
        if not isinstance(cursor, dict):
            return set()
        cursor = cursor.get(key)
    if not isinstance(cursor, list):
        return set()
    return {value for value in cursor if isinstance(value, str)}


def display_path(path: pathlib.Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT))
    except ValueError:
        return str(path)


def trusted_custom_contract_hash() -> str | None:
    try:
        return document_hash(load_json(TRUSTED_ABC_PRESERVATION_SCHEMA_PATH))
    except (json.JSONDecodeError, OSError):
        return None


def tei_profile_contract_block(custom_contract: dict[str, Any]) -> dict[str, Any]:
    if custom_contract.get("verdict") != "CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION":
        return {
            "verdict": "TEI_PROFILE_CONTRACT_MISSING",
            "schema_id": custom_contract.get("schema_id"),
            "schema_version": custom_contract.get("schema_version"),
            "missing_record_classes": [TEI_PROFILE_RECORD_CLASS],
            "missing_coverage_classes": [TEI_PROFILE_RECORD_CLASS],
            "missing_constructs": sorted(TEI_PROFILE_CONSTRUCTS),
            "message": "ABC TEI profile projection evidence is unavailable until the ABC preservation schema is confirmed.",
        }

    record_classes = set(custom_contract.get("record_classes") or [])
    coverage_classes = set(custom_contract.get("coverage_classes") or [])
    constructs = set(custom_contract.get("constructs") or [])
    missing_record_classes = sorted({TEI_PROFILE_RECORD_CLASS} - record_classes)
    missing_coverage_classes = sorted({TEI_PROFILE_RECORD_CLASS} - coverage_classes)
    missing_constructs = sorted(TEI_PROFILE_CONSTRUCTS - constructs)
    if missing_record_classes or missing_coverage_classes or missing_constructs:
        return {
            "verdict": "TEI_PROFILE_CONTRACT_INCOMPLETE",
            "schema_id": custom_contract.get("schema_id"),
            "schema_version": custom_contract.get("schema_version"),
            "missing_record_classes": missing_record_classes,
            "missing_coverage_classes": missing_coverage_classes,
            "missing_constructs": missing_constructs,
            "message": "ABC preservation schema is confirmed but does not admit all TEI profile projection evidence required by the coverage gate.",
        }
    return {
        "verdict": "TEI_PROFILE_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION",
        "schema_id": custom_contract.get("schema_id"),
        "schema_version": custom_contract.get("schema_version"),
        "record_class": TEI_PROFILE_RECORD_CLASS,
        "constructs": sorted(TEI_PROFILE_CONSTRUCTS),
        "message": "ABC preservation schema admits TEI profile projection records for the measured namespace-extension families.",
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


def parser_ir_schema_hash(parser_schema: dict[str, Any], mapping: dict[str, Any]) -> Any:
    return (
        mapping.get("target_parser_ir_schema_hash")
        or parser_schema.get("properties", {}).get("schema_hash", {}).get("const")
    )


def summary_scope(
    parser_schema: dict[str, Any],
    mapping: dict[str, Any],
    source: dict[str, Any],
    matrix: dict[str, Any],
) -> dict[str, Any]:
    return {
        "kind": "ir_publication_coverage",
        "source_authority_works_scanned": source.get("works_scanned"),
        "required_parsers": list(REQUIRED_PARSERS),
        "matrix_rows_attempted": matrix.get("totals", {}).get("rows_attempted"),
        "mapping_id": mapping.get("mapping_id"),
        "mapping_version": mapping.get("mapping_version"),
        "parser_ir_schema_hash": parser_ir_schema_hash(parser_schema, mapping),
        "custom_contract_required": True,
    }


def unsupported_gaps(
    node_coverage: dict[str, Any],
    field_coverage: dict[str, Any],
    diagnostic_coverage: dict[str, Any],
    construct_coverage: dict[str, Any],
) -> dict[str, Any]:
    items: list[dict[str, Any]] = []
    for raw_item in node_coverage.get("unsupported", []):
        items.append(
            {
                **raw_item,
                "observed_occurrences": None,
                "prevalence_source": "unavailable",
            }
        )
    for raw_item in field_coverage.get("unsupported", []):
        items.append(
            {
                **raw_item,
                "observed_occurrences": None,
                "prevalence_source": "unavailable",
            }
        )
    for raw_item in diagnostic_coverage.get("unsupported", []):
        items.append(
            {
                **raw_item,
                "observed_occurrences": None,
                "prevalence_source": "unavailable",
            }
        )
    items.extend(construct_coverage.get("unsupported", []))
    return {
        "count": len(items),
        "counts_by_owner": count_values(items, "owner"),
        "items": items,
    }


def gap_pointer(item: dict[str, Any]) -> str:
    return str(
        item.get("aat_pointer")
        or item.get("parser_ir_pointer")
        or item.get("node_type")
        or item.get("field")
        or ""
    )


def closure_family_for_pointer(pointer: str) -> str | None:
    if pointer.endswith(".span") or ".span." in pointer:
        return "span_coordinates"
    if pointer.endswith(".style"):
        return "style_rendition"
    if ".accent" in pointer or pointer.endswith(".accent"):
        return "accent"
    if pointer in {
        "meta.source_encoding",
        "meta.source_hash",
        "source.normalization",
        "source.source_path",
        "schema_id/schema_hash",
    }:
        return "source_identity"
    if pointer in {"meta.metrics", "meta.parse_complete", "meta.semantic_summary"}:
        return "provenance_metrics"
    if ".figure." in pointer or pointer.endswith(".figure"):
        return "figure_metadata"
    if pointer.endswith(".gaiji.unresolved_reason"):
        return "gaiji_unresolved_reason"
    if ".heading.content[].gaiji" in pointer:
        return "heading_inline_content"
    if pointer.endswith(".font_size") or pointer.endswith(".tcy"):
        return "font_tcy"
    if pointer.endswith(".keigakomi") or pointer.endswith(".yokogumi"):
        return "keigakomi_yokogumi"
    if pointer.endswith(".heading") or pointer.endswith(".jisage_block") or ".heading." in pointer:
        return "heading_jisage_structure"
    return None


def classify_closure_gap(item: dict[str, Any]) -> dict[str, Any] | None:
    family = closure_family_for_pointer(gap_pointer(item))
    if family is None:
        return None
    spec = CLOSURE_FAMILIES[family]
    return {
        **item,
        "closure_family": family,
        "closure_lane": spec["closure_lane"],
        "closure_owner": spec["owner"],
        "admission_gate": spec["admission_gate"],
    }


def closure_gaps(
    gaps: dict[str, Any],
    custom_contract: dict[str, Any],
    tei_profile_contract: dict[str, Any],
) -> dict[str, Any]:
    classified: list[dict[str, Any]] = []
    admitted_by_custom_contract: list[dict[str, Any]] = []
    admitted_by_tei_profile: list[dict[str, Any]] = []
    true_unsupported: list[dict[str, Any]] = []
    custom_contract_confirmed = custom_contract.get("verdict") == "CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"
    tei_profile_confirmed = (
        tei_profile_contract.get("verdict") == "TEI_PROFILE_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"
    )
    for item in gaps.get("items", []):
        closure_item = classify_closure_gap(item)
        if closure_item is None:
            true_unsupported.append({**item, "closure_family": None, "closure_lane": "unsupported_gap"})
        elif custom_contract_confirmed and closure_item.get("closure_lane") == "custom_sidecar":
            admitted_by_custom_contract.append(
                {**closure_item, "admitted_by": custom_contract.get("schema_id")}
            )
        elif (
            tei_profile_confirmed
            and closure_item.get("closure_family") in TEI_PROFILE_FAMILIES
            and closure_item.get("closure_lane") in TEI_PROFILE_LANES
        ):
            admitted_by_tei_profile.append(
                {**closure_item, "admitted_by": tei_profile_contract.get("schema_id")}
            )
        else:
            classified.append(closure_item)
    return {
        "admitted_by_custom_contract": {
            "count": len(admitted_by_custom_contract),
            "counts_by_family": count_values(admitted_by_custom_contract, "closure_family"),
            "items": admitted_by_custom_contract,
        },
        "admitted_by_tei_profile": {
            "count": len(admitted_by_tei_profile),
            "counts_by_family": count_values(admitted_by_tei_profile, "closure_family"),
            "items": admitted_by_tei_profile,
        },
        "classified_but_not_admitted": {
            "count": len(classified),
            "counts_by_family": count_values(classified, "closure_family"),
            "counts_by_lane": count_values(classified, "closure_lane"),
            "counts_by_owner": count_values(classified, "closure_owner"),
            "items": classified,
        },
        "true_unsupported_gaps": {
            "count": len(true_unsupported),
            "counts_by_owner": count_values(true_unsupported, "owner"),
            "items": true_unsupported,
        },
    }


def publication_verdict(
    source_passed: bool,
    evidence: dict[str, Any],
    custom_contract: dict[str, Any],
    gaps: dict[str, Any],
    closures: dict[str, Any],
) -> str:
    if not source_passed:
        return "IR_PUBLICATION_COVERAGE_BLOCKED_SOURCE_AUTHORITY"
    if evidence.get("verdict") != "FIVE_PARSER_EVIDENCE_COMPLETE":
        return "IR_PUBLICATION_COVERAGE_BLOCKED_INCOMPLETE_PARSER_EVIDENCE"
    if closures["true_unsupported_gaps"].get("count", 0) > 0:
        return "IR_PUBLICATION_COVERAGE_BLOCKED_UNSUPPORTED_GAPS"
    if closures["classified_but_not_admitted"].get("count", 0) > 0:
        return "IR_PUBLICATION_COVERAGE_BLOCKED_CLASSIFIED_GAPS"
    if custom_contract.get("verdict") != "CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION":
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
    field_coverage = field_coverage_from_schema(parser_schema)
    diagnostics = diagnostic_coverage(mapping)
    construct_coverage = source_construct_coverage(mapping)
    evidence = parser_evidence_coverage(matrix, source_delta)
    custom_contract = custom_contract_block(custom_contract_schema)
    tei_profile_contract = tei_profile_contract_block(custom_contract)
    gaps = unsupported_gaps(node_coverage, field_coverage, diagnostics, construct_coverage)
    closures = closure_gaps(gaps, custom_contract, tei_profile_contract)
    source_passed = source_authority_passed(source)
    return {
        "schema_version": SCHEMA_VERSION,
        "scope": summary_scope(parser_schema, mapping, source, matrix),
        "verdict": publication_verdict(source_passed, evidence, custom_contract, gaps, closures),
        "source_authority_gate": source_authority_gate(source),
        "parser_evidence_coverage": evidence,
        "parser_ir_schema": {
            "schema_id": parser_schema.get("$id"),
            "schema_hash": parser_ir_schema_hash(parser_schema, mapping),
        },
        "tei_profile": {
            "role": "primary_publication_xml",
            "calibration": "tei_eaj_calibration_only",
        },
        "custom_contract": custom_contract,
        "tei_profile_contract": tei_profile_contract,
        "mapping": mapping_block(mapping),
        "node_coverage": node_coverage,
        "field_coverage": field_coverage,
        "diagnostic_coverage": diagnostics,
        "source_construct_coverage": construct_coverage,
        "unsupported_gaps": gaps,
        "closure_gaps": closures,
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
    field_counts = summary["field_coverage"]["counts_by_class"]
    diagnostic_counts = summary["diagnostic_coverage"]["counts_by_class"]
    construct_counts = summary["source_construct_coverage"]["counts_by_class"]
    gaps = summary["unsupported_gaps"]
    closures = summary["closure_gaps"]
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
    lines.extend(["", "## Field Coverage", "", "| Class | Field facts |", "|---|---:|"])
    for name, count in field_counts.items():
        lines.append(f"| `{name}` | {count} |")
    lines.extend(["", "| Field | Class | Target |", "|---|---|---|"])
    for field_name, entry in summary["field_coverage"]["by_field"].items():
        lines.append(f"| `{field_name}` | `{entry['class']}` | {entry['target']} |")
    lines.extend(["", "## Diagnostic Coverage", "", "| Class | Diagnostic facts |", "|---|---:|"])
    for name, count in diagnostic_counts.items():
        lines.append(f"| `{name}` | {count} |")
    lines.extend(["", "| Diagnostic field | Class | Target |", "|---|---|---|"])
    for field_name, entry in summary["diagnostic_coverage"]["by_field"].items():
        lines.append(f"| `{field_name}` | `{entry['class']}` | {entry['target']} |")
    lines.extend(["", "## Source Construct Coverage", "", "| Class | Constructs |", "|---|---:|"])
    for name, count in construct_counts.items():
        lines.append(f"| `{name}` | {count} |")
    lines.extend(["", "## Unsupported gaps", "", f"Count: {gaps['count']}", ""])
    if gaps.get("counts_by_owner"):
        lines.extend(["| Owner | Count |", "|---|---:|"])
        for owner, count in gaps["counts_by_owner"].items():
            lines.append(f"| `{owner}` | {count} |")
        lines.append("")
    for item in gaps["items"][:20]:
        label = item.get("aat_pointer") or item.get("node_type")
        lines.append(f"- `{label}` owner `{item.get('owner')}`")
    lines.extend([
        "",
        "## Closure Gaps",
        "",
        f"Admitted by custom contract: {closures['admitted_by_custom_contract']['count']}",
        "",
        f"Admitted by TEI profile: {closures['admitted_by_tei_profile']['count']}",
        "",
        f"Classified but not admitted: {closures['classified_but_not_admitted']['count']}",
        "",
    ])
    if closures["admitted_by_custom_contract"].get("counts_by_family"):
        lines.extend(["| Admitted family | Count |", "|---|---:|"])
        for family, count in closures["admitted_by_custom_contract"]["counts_by_family"].items():
            lines.append(f"| `{family}` | {count} |")
        lines.append("")
    if closures["admitted_by_tei_profile"].get("counts_by_family"):
        lines.extend(["| TEI profile admitted family | Count |", "|---|---:|"])
        for family, count in closures["admitted_by_tei_profile"]["counts_by_family"].items():
            lines.append(f"| `{family}` | {count} |")
        lines.append("")
    if closures["classified_but_not_admitted"].get("counts_by_family"):
        lines.extend(["| Family | Count |", "|---|---:|"])
        for family, count in closures["classified_but_not_admitted"]["counts_by_family"].items():
            lines.append(f"| `{family}` | {count} |")
        lines.append("")
    lines.extend([
        f"True unsupported gaps: {closures['true_unsupported_gaps']['count']}",
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
