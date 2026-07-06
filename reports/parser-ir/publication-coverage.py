#!/usr/bin/env python3
"""Report full Parser-IR publication coverage across TEI/custom/plaintext targets."""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys
from collections import Counter
from typing import Any

_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from reports.lib.hashing import sha256_hex
from reports.lib.io import read_json as load_json
from reports.lib.io import write_json
from reports.lib.paths import policy_dir, repo_root, schemas_dir

SCHEMA_VERSION = "ir-publication-coverage-v1"
REQUIRED_PARSERS = ("aozora2html", "aozora-epub3", "aozora-rs", "aozora2", "aozora")
REPO_ROOT = repo_root()
ABC_PRESERVATION_SCHEMA_ID = "https://w3id.org/abc/schemas/parser-ir-publication-preservation.schema.json"
ABC_PRESERVATION_SCHEMA_VERSION = "0.2.0"
TRUSTED_ABC_PRESERVATION_SCHEMA_PATH = (
    schemas_dir() / "parser-ir-publication-preservation.schema.json"
).resolve()
ABC_SOURCE_REGION_SCHEMA_ID = "https://w3id.org/abc/schemas/source-region-coverage.schema.json"
ABC_SOURCE_REGION_SCHEMA_VERSION = "aozora-source-region-coverage-v1"
ABC_SOURCE_REGION_POLICY_ID = "https://w3id.org/abc/policies/source-region-publication-v0"
ABC_SOURCE_REGION_POLICY_VERSION = "0.2.0"
TRUSTED_ABC_SOURCE_REGION_SCHEMA_PATH = (
    schemas_dir() / "source-region-coverage.schema.json"
).resolve()
TRUSTED_ABC_SOURCE_REGION_POLICY_PATH = (
    policy_dir() / "source-region-publication-policy-v0.json"
).resolve()
TRUSTED_ABC_MANIFEST_SCHEMA_PATH = (
    schemas_dir() / "manifest.schema.json"
).resolve()
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
SOURCE_REGION_REQUIRED_LEGACY_COUNTERS = {
    "typed_occurrences",
    "raw_preserved_occurrences",
    "out_of_body_occurrences",
    "malformed_noise_occurrences",
    "unsupported_occurrences",
    "needs_research_occurrences",
}
SOURCE_REGION_REQUIRED_CLASSES = {
    "notation_legend",
    "notation_placeholder",
    "body_end_boundary",
    "terminal_provenance",
    "colophon_metadata",
    "letter_address_origin",
    "malformed_source",
}
SOURCE_REGION_MEASURED_CLASS_COUNTERS = {
    "body_end_boundary": "body_end_boundary_occurrences",
    "terminal_provenance": "terminal_provenance_occurrences",
    "colophon_metadata": "colophon_metadata_occurrences",
    "letter_address_origin": "letter_address_origin_occurrences",
}
SOURCE_REGION_ALLOWED_TARGET_CLASSES = {
    "tei_policy_projection",
    "tei_plus_abc_extension",
    "custom_sidecar",
    "diagnostic",
    "unsupported_gap",
}
PUBLICATION_BUNDLE_EVIDENCE_SCHEMA_VERSION = "publication-bundle-validation-evidence-v1"
PUBLICATION_BUNDLE_BATCH_EVIDENCE_SCHEMA_VERSION = "publication-bundle-batch-validation-evidence-v1"
PUBLICATION_BUNDLE_PASSED_VERDICT = "PUBLICATION_BUNDLE_VALIDATION_PASSED"
PUBLICATION_BUNDLE_BATCH_PASSED_VERDICT = "PUBLICATION_BUNDLE_BATCH_VALIDATION_PASSED"
PUBLICATION_BUNDLE_REQUIRED_ARTIFACTS = {
    "parser_ir",
    "tei",
    "plaintext",
    "preservation",
    "source_region_coverage",
    "tei_manifest",
    "plaintext_manifest",
}
PUBLICATION_BUNDLE_REQUIRED_CHECKS = {
    "parser_ir_schema_valid",
    "tei_profile_valid",
    "preservation_schema_valid",
    "source_region_coverage_valid",
    "tei_manifest_valid",
    "plaintext_manifest_valid",
    "tei_manifest_references_preservation",
    "tei_manifest_references_validation_result",
    "source_region_sidecar_role_available",
    "tei_abc_projection_resolves_to_sidecar",
    "preservation_tei_pointers_resolve",
    "preservation_source_pointers_resolve",
    "plaintext_body_only",
}
NEXT_WORK_SCHEMA_VERSION = "aozora-publication-next-work-v1"
SHA256_PATTERN = re.compile(r"^sha256:[0-9a-f]{64}$")
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


def source_region_contract_block(
    source_region_schema: pathlib.Path | None,
    source_region_policy: pathlib.Path | None,
    manifest_schema: pathlib.Path | None,
) -> dict[str, Any]:
    if source_region_schema is None or source_region_policy is None or manifest_schema is None:
        return {
            "verdict": "SOURCE_REGION_CONTRACT_MISSING",
            "schema_id": None,
            "schema_version": None,
            "policy_id": None,
            "policy_version": None,
            "message": "ABC source-region schema, publication policy, and manifest schema snapshots are required.",
        }

    try:
        schema_doc = load_json(source_region_schema)
        policy_doc = load_json(source_region_policy)
        manifest_doc = load_json(manifest_schema)
    except json.JSONDecodeError:
        return {
            "verdict": "SOURCE_REGION_CONTRACT_INVALID",
            "schema_id": None,
            "schema_version": None,
            "policy_id": None,
            "policy_version": None,
            "path": display_path(source_region_schema),
            "policy_path": display_path(source_region_policy),
            "manifest_schema_path": display_path(manifest_schema),
            "message": "ABC source-region contract snapshot is not valid JSON.",
        }
    except OSError:
        return {
            "verdict": "SOURCE_REGION_CONTRACT_MISSING",
            "schema_id": None,
            "schema_version": None,
            "policy_id": None,
            "policy_version": None,
            "path": display_path(source_region_schema),
            "policy_path": display_path(source_region_policy),
            "manifest_schema_path": display_path(manifest_schema),
            "message": "ABC source-region contract snapshot path was supplied but could not be read.",
        }

    schema_id = schema_doc.get("$id") if isinstance(schema_doc, dict) else None
    schema_version = None
    if isinstance(schema_doc, dict):
        schema_version = schema_doc.get("properties", {}).get("schema_version", {}).get("const")
    policy_id = policy_doc.get("policy_id") if isinstance(policy_doc, dict) else None
    policy_version = policy_doc.get("policy_version") if isinstance(policy_doc, dict) else None
    source_region_counters = set()
    representability_counters = set()
    if isinstance(schema_doc, dict):
        source_region_counters = set(
            schema_doc.get("properties", {})
            .get("source_region_coverage", {})
            .get("required", [])
        )
        representability_counters = set(
            schema_doc.get("properties", {})
            .get("representability", {})
            .get("required", [])
        )
    dispositions = policy_doc.get("dispositions", []) if isinstance(policy_doc, dict) else []
    disposition_by_class = {
        row.get("source_class"): row
        for row in dispositions
        if isinstance(row, dict) and isinstance(row.get("source_class"), str)
    }
    policy_classes = set(disposition_by_class)
    missing_policy_classes = sorted(SOURCE_REGION_REQUIRED_CLASSES - policy_classes)
    duplicate_policy_classes = sorted(
        source_class
        for source_class, count in Counter(
            row.get("source_class") for row in dispositions if isinstance(row, dict)
        ).items()
        if isinstance(source_class, str) and count > 1
    )
    invalid_target_classes = sorted(
        source_class
        for source_class, row in disposition_by_class.items()
        if row.get("target_class") not in SOURCE_REGION_ALLOWED_TARGET_CLASSES
    )
    non_omitted_plaintext_classes = sorted(
        source_class
        for source_class, row in disposition_by_class.items()
        if row.get("plaintext_projection") != "omit"
    )
    missing_measured_class_counters = sorted(
        source_class
        for source_class, counter in SOURCE_REGION_MEASURED_CLASS_COUNTERS.items()
        if counter not in source_region_counters
    )
    unmeasured_policy_classes = sorted(
        source_class
        for source_class in SOURCE_REGION_MEASURED_CLASS_COUNTERS
        if disposition_by_class.get(source_class, {}).get("measurement_status")
        != "measured"
    )
    manifest_sidecar_roles = schema_enum_values(
        manifest_doc,
        ("$defs", "sidecar", "properties", "role", "enum"),
    )
    manifest_sidecar_role_present = "source-region-coverage" in manifest_sidecar_roles
    schema_hash = document_hash(schema_doc)
    policy_hash = document_hash(policy_doc)
    manifest_schema_hash = document_hash(manifest_doc)
    trusted_schema_hash = trusted_json_hash(TRUSTED_ABC_SOURCE_REGION_SCHEMA_PATH)
    trusted_policy_hash = trusted_json_hash(TRUSTED_ABC_SOURCE_REGION_POLICY_PATH)
    trusted_manifest_schema_hash = trusted_json_hash(TRUSTED_ABC_MANIFEST_SCHEMA_PATH)
    schema_path_match = source_region_schema.resolve() == TRUSTED_ABC_SOURCE_REGION_SCHEMA_PATH
    policy_path_match = source_region_policy.resolve() == TRUSTED_ABC_SOURCE_REGION_POLICY_PATH
    manifest_schema_path_match = manifest_schema.resolve() == TRUSTED_ABC_MANIFEST_SCHEMA_PATH
    missing_source_region_counters = sorted(
        SOURCE_REGION_REQUIRED_COUNTERS - source_region_counters
    )
    missing_legacy_counters = sorted(
        SOURCE_REGION_REQUIRED_LEGACY_COUNTERS - representability_counters
    )
    confirmed = (
        schema_id == ABC_SOURCE_REGION_SCHEMA_ID
        and schema_version == ABC_SOURCE_REGION_SCHEMA_VERSION
        and policy_id == ABC_SOURCE_REGION_POLICY_ID
        and policy_version == ABC_SOURCE_REGION_POLICY_VERSION
        and not missing_source_region_counters
        and not missing_legacy_counters
        and not missing_policy_classes
        and not duplicate_policy_classes
        and not invalid_target_classes
        and not non_omitted_plaintext_classes
        and not missing_measured_class_counters
        and not unmeasured_policy_classes
        and manifest_sidecar_role_present
        and schema_path_match
        and policy_path_match
        and manifest_schema_path_match
        and schema_hash == trusted_schema_hash
        and policy_hash == trusted_policy_hash
        and manifest_schema_hash == trusted_manifest_schema_hash
    )
    return {
        "verdict": (
            "SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"
            if confirmed
            else "SOURCE_REGION_CONTRACT_CANDIDATE_PROVIDED"
        ),
        "schema_id": schema_id,
        "schema_version": schema_version,
        "policy_id": policy_id,
        "policy_version": policy_version,
        "path": display_path(source_region_schema),
        "policy_path": display_path(source_region_policy),
        "manifest_schema_path": display_path(manifest_schema),
        "schema_hash": schema_hash,
        "policy_hash": policy_hash,
        "manifest_schema_hash": manifest_schema_hash,
        "trusted_schema_path": display_path(TRUSTED_ABC_SOURCE_REGION_SCHEMA_PATH),
        "trusted_policy_path": display_path(TRUSTED_ABC_SOURCE_REGION_POLICY_PATH),
        "trusted_manifest_schema_path": display_path(TRUSTED_ABC_MANIFEST_SCHEMA_PATH),
        "trusted_schema_hash": trusted_schema_hash,
        "trusted_policy_hash": trusted_policy_hash,
        "trusted_manifest_schema_hash": trusted_manifest_schema_hash,
        "trusted_schema_path_match": schema_path_match,
        "trusted_policy_path_match": policy_path_match,
        "trusted_manifest_schema_path_match": manifest_schema_path_match,
        "source_region_counters": sorted(source_region_counters),
        "legacy_representability_counters": sorted(representability_counters),
        "policy_classes": sorted(policy_classes),
        "target_classes": sorted(
            {
                row.get("target_class")
                for row in disposition_by_class.values()
                if isinstance(row.get("target_class"), str)
            }
        ),
        "manifest_sidecar_role_present": manifest_sidecar_role_present,
        "missing_source_region_counters": missing_source_region_counters,
        "missing_legacy_counters": missing_legacy_counters,
        "missing_policy_classes": missing_policy_classes,
        "duplicate_policy_classes": duplicate_policy_classes,
        "invalid_target_classes": invalid_target_classes,
        "non_omitted_plaintext_classes": non_omitted_plaintext_classes,
        "measured_class_counters": SOURCE_REGION_MEASURED_CLASS_COUNTERS,
        "missing_measured_class_counters": missing_measured_class_counters,
        "unmeasured_policy_classes": unmeasured_policy_classes,
        "missing_measurement_split_classes": unmeasured_policy_classes,
        "message": (
            "ABC source-region schema, publication policy, and manifest sidecar role are confirmed."
            if confirmed
            else "Readable source-region contract candidate recorded as evidence only; synced ABC integration must confirm the source-region contract before admission can unblock."
        ),
    }


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


def trusted_json_hash(path: pathlib.Path) -> str | None:
    try:
        return document_hash(load_json(path))
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


def publication_bundle_contract_block(bundle_summary_path: pathlib.Path | None) -> dict[str, Any]:
    if bundle_summary_path is None:
        return {
            "verdict": "PUBLICATION_BUNDLE_CONTRACT_MISSING",
            "schema_version": None,
            "path": None,
            "bundle_hash": None,
            "missing_artifacts": sorted(PUBLICATION_BUNDLE_REQUIRED_ARTIFACTS),
            "missing_checks": sorted(PUBLICATION_BUNDLE_REQUIRED_CHECKS),
            "failed_checks": [],
            "message": "ABC publication bundle validation evidence has not been supplied to this report.",
        }

    try:
        evidence = load_json(bundle_summary_path)
    except json.JSONDecodeError:
        return {
            "verdict": "PUBLICATION_BUNDLE_CONTRACT_INVALID",
            "schema_version": None,
            "path": display_path(bundle_summary_path),
            "bundle_hash": None,
            "missing_artifacts": sorted(PUBLICATION_BUNDLE_REQUIRED_ARTIFACTS),
            "missing_checks": sorted(PUBLICATION_BUNDLE_REQUIRED_CHECKS),
            "failed_checks": [],
            "message": "ABC publication bundle validation evidence is not valid JSON.",
        }
    except OSError:
        return {
            "verdict": "PUBLICATION_BUNDLE_CONTRACT_MISSING",
            "schema_version": None,
            "path": display_path(bundle_summary_path),
            "bundle_hash": None,
            "missing_artifacts": sorted(PUBLICATION_BUNDLE_REQUIRED_ARTIFACTS),
            "missing_checks": sorted(PUBLICATION_BUNDLE_REQUIRED_CHECKS),
            "failed_checks": [],
            "message": "ABC publication bundle validation evidence path was supplied but could not be read.",
        }

    schema_version = evidence.get("schema_version") if isinstance(evidence, dict) else None
    if schema_version == PUBLICATION_BUNDLE_BATCH_EVIDENCE_SCHEMA_VERSION:
        checks = evidence.get("checks", {}) if isinstance(evidence, dict) else {}
        rows = evidence.get("rows", []) if isinstance(evidence, dict) else []
        scope = evidence.get("scope", {}) if isinstance(evidence, dict) else {}
        rows = rows if isinstance(rows, list) else []
        present_artifact_failures: list[str] = []
        invalid_artifact_hashes: list[str] = []
        sample_validated_bundles: list[dict[str, Any]] = []
        for row in rows:
            if not isinstance(row, dict):
                continue
            row_id = str(row.get("row_id") or "")
            validated_bundle = row.get("validated_bundle", {})
            if not isinstance(validated_bundle, dict):
                validated_bundle = {}
            if len(sample_validated_bundles) < 5:
                sample_validated_bundles.append(
                    {
                        "row_id": row_id,
                        "row_dir": row.get("row_dir"),
                        "validated_bundle": {
                            key: validated_bundle.get(key)
                            for key in sorted(PUBLICATION_BUNDLE_REQUIRED_ARTIFACTS)
                            if key in validated_bundle
                        },
                    }
                )
            row_present = {
                name
                for name, artifact in validated_bundle.items()
                if isinstance(name, str)
                and isinstance(artifact, dict)
                and isinstance(artifact.get("path"), str)
                and artifact.get("path")
                and isinstance(artifact.get("hash"), str)
                and SHA256_PATTERN.fullmatch(artifact.get("hash"))
            }
            present_artifact_failures.extend(
                f"{row_id}:{name}"
                for name in sorted(PUBLICATION_BUNDLE_REQUIRED_ARTIFACTS - row_present)
            )
            invalid_artifact_hashes.extend(
                f"{row_id}:{name}"
                for name, artifact in validated_bundle.items()
                if name in PUBLICATION_BUNDLE_REQUIRED_ARTIFACTS
                and isinstance(artifact, dict)
                and isinstance(artifact.get("hash"), str)
                and not SHA256_PATTERN.fullmatch(artifact.get("hash"))
            )
        missing_checks = sorted(
            check
            for check in PUBLICATION_BUNDLE_REQUIRED_CHECKS
            if check not in checks
        )
        failed_checks = sorted(
            check
            for check in PUBLICATION_BUNDLE_REQUIRED_CHECKS
            if checks.get(check) is not True
        )
        rows_validated = scope.get("rows_validated")
        rows_failed = scope.get("rows_failed")
        confirmed = (
            evidence.get("verdict") == PUBLICATION_BUNDLE_BATCH_PASSED_VERDICT
            and isinstance(rows_validated, int)
            and rows_validated > 0
            and rows_validated == len(rows)
            and rows_failed == 0
            and not present_artifact_failures
            and not invalid_artifact_hashes
            and not missing_checks
            and not failed_checks
        )
        return {
            "verdict": (
                "PUBLICATION_BUNDLE_CONTRACT_CONFIRMED_BY_ABC_VALIDATION"
                if confirmed
                else "PUBLICATION_BUNDLE_CONTRACT_INCOMPLETE"
            ),
            "schema_version": schema_version,
            "validation_scope": "batch",
            "path": display_path(bundle_summary_path),
            "bundle_hash": document_hash(evidence),
            "abc_commit": evidence.get("abc_commit") if isinstance(evidence, dict) else None,
            "validator": evidence.get("validator") if isinstance(evidence, dict) else None,
            "command": evidence.get("command") if isinstance(evidence, dict) else None,
            "rows_discovered": scope.get("rows_discovered") if isinstance(scope, dict) else None,
            "rows_validated": rows_validated,
            "rows_failed": rows_failed,
            "sample_validated_bundles": sample_validated_bundles,
            "checks": {key: checks.get(key) for key in sorted(PUBLICATION_BUNDLE_REQUIRED_CHECKS)},
            "missing_artifacts": sorted(present_artifact_failures),
            "invalid_artifact_hashes": sorted(invalid_artifact_hashes),
            "missing_checks": missing_checks,
            "failed_checks": failed_checks,
            "message": (
                "ABC publication bundle batch validation evidence confirms parser-IR, TEI, preservation, source-region, manifest, and plaintext cross-artifact checks."
                if confirmed
                else "ABC publication bundle batch validation evidence is missing required artifacts, rows, or cross-artifact checks."
            ),
        }

    validated_bundle = evidence.get("validated_bundle", {}) if isinstance(evidence, dict) else {}
    checks = evidence.get("checks", {}) if isinstance(evidence, dict) else {}
    present_artifacts = {
        name
        for name, artifact in validated_bundle.items()
        if isinstance(name, str)
        and isinstance(artifact, dict)
        and isinstance(artifact.get("path"), str)
        and artifact.get("path")
        and isinstance(artifact.get("hash"), str)
        and SHA256_PATTERN.fullmatch(artifact.get("hash"))
    }
    missing_artifacts = sorted(PUBLICATION_BUNDLE_REQUIRED_ARTIFACTS - present_artifacts)
    invalid_artifact_hashes = sorted(
        name
        for name, artifact in validated_bundle.items()
        if name in PUBLICATION_BUNDLE_REQUIRED_ARTIFACTS
        and isinstance(artifact, dict)
        and isinstance(artifact.get("hash"), str)
        and not SHA256_PATTERN.fullmatch(artifact.get("hash"))
    )
    missing_checks = sorted(
        check
        for check in PUBLICATION_BUNDLE_REQUIRED_CHECKS
        if check not in checks
    )
    failed_checks = sorted(
        check
        for check in PUBLICATION_BUNDLE_REQUIRED_CHECKS
        if checks.get(check) is not True
    )
    confirmed = (
        isinstance(evidence, dict)
        and evidence.get("schema_version") == PUBLICATION_BUNDLE_EVIDENCE_SCHEMA_VERSION
        and evidence.get("verdict") == PUBLICATION_BUNDLE_PASSED_VERDICT
        and not missing_artifacts
        and not invalid_artifact_hashes
        and not missing_checks
        and not failed_checks
    )
    return {
        "verdict": (
            "PUBLICATION_BUNDLE_CONTRACT_CONFIRMED_BY_ABC_VALIDATION"
            if confirmed
            else "PUBLICATION_BUNDLE_CONTRACT_INCOMPLETE"
        ),
        "schema_version": evidence.get("schema_version") if isinstance(evidence, dict) else None,
        "validation_scope": "single",
        "path": display_path(bundle_summary_path),
        "bundle_hash": document_hash(evidence),
        "abc_commit": evidence.get("abc_commit") if isinstance(evidence, dict) else None,
        "validator": evidence.get("validator") if isinstance(evidence, dict) else None,
        "command": evidence.get("command") if isinstance(evidence, dict) else None,
        "validated_bundle": {
            key: validated_bundle.get(key)
            for key in sorted(PUBLICATION_BUNDLE_REQUIRED_ARTIFACTS)
            if key in validated_bundle
        },
        "checks": {key: checks.get(key) for key in sorted(PUBLICATION_BUNDLE_REQUIRED_CHECKS)},
        "missing_artifacts": missing_artifacts,
        "invalid_artifact_hashes": invalid_artifact_hashes,
        "missing_checks": missing_checks,
        "failed_checks": failed_checks,
        "message": (
            "ABC publication bundle validation evidence confirms parser-IR, TEI, preservation, source-region, manifest, and plaintext cross-artifact checks."
            if confirmed
            else "ABC publication bundle validation evidence is missing required artifacts or cross-artifact checks."
        ),
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


def next_work_dashboard_block(next_work_summary_path: pathlib.Path | None) -> dict[str, Any] | None:
    if next_work_summary_path is None:
        return None
    try:
        evidence = load_json(next_work_summary_path)
    except (json.JSONDecodeError, OSError):
        return {
            "path": display_path(next_work_summary_path),
            "hash": None,
            "verdict": "NEXT_WORK_SUMMARY_UNREADABLE",
            "next_work_items_count": None,
            "item_ids": [],
        }
    next_work_items = evidence.get("next_work_items", []) if isinstance(evidence, dict) else []
    if not isinstance(next_work_items, list):
        next_work_items = []
    schema_version = evidence.get("schema_version") if isinstance(evidence, dict) else None
    schema_valid = schema_version == NEXT_WORK_SCHEMA_VERSION
    item_ids = [
        item.get("id")
        for item in next_work_items
        if isinstance(item, dict) and isinstance(item.get("id"), str)
    ]
    return {
        "path": display_path(next_work_summary_path),
        "hash": document_hash(evidence),
        "schema_version": schema_version,
        "schema_valid": schema_valid,
        "verdict": (
            evidence.get("verdict")
            if isinstance(evidence, dict) and schema_valid
            else "NEXT_WORK_SUMMARY_SCHEMA_MISMATCH"
        ),
        "next_work_items_count": len(next_work_items),
        "item_ids": item_ids,
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
        "source_region_contract_required": True,
        "custom_contract_required": True,
        "publication_bundle_validation_required": True,
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
        "description": (
            "Raw unsupported-derived mapping rows before closure folding. "
            "Use unsupported_derived_closure_coverage or closure_gaps to decide whether any row remains a true unsupported gap."
        ),
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


def unsupported_derived_closure_coverage(closures: dict[str, Any]) -> dict[str, Any]:
    status_items = {
        "admitted_by_custom_contract": closures["admitted_by_custom_contract"].get("items", []),
        "admitted_by_tei_profile": closures["admitted_by_tei_profile"].get("items", []),
        "classified_but_not_admitted": closures["classified_but_not_admitted"].get("items", []),
        "true_unsupported_gap": closures["true_unsupported_gaps"].get("items", []),
    }
    return {
        "description": (
            "Closure-adjusted view of raw unsupported-derived mapping rows. "
            "A row is a true unsupported gap only if it has no TEI/profile/custom closure family."
        ),
        "total": sum(len(items) for items in status_items.values()),
        "counts_by_status": {
            status: len(items) for status, items in status_items.items()
        },
        "counts_by_family_by_status": {
            status: count_values(items, "closure_family")
            for status, items in status_items.items()
        },
    }


def publication_verdict(
    source_passed: bool,
    evidence: dict[str, Any],
    source_region_contract: dict[str, Any],
    custom_contract: dict[str, Any],
    publication_bundle_contract: dict[str, Any],
    gaps: dict[str, Any],
    closures: dict[str, Any],
) -> str:
    if not source_passed:
        return "IR_PUBLICATION_COVERAGE_BLOCKED_SOURCE_AUTHORITY"
    if (
        source_region_contract.get("verdict")
        != "SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"
    ):
        return "IR_PUBLICATION_COVERAGE_BLOCKED_SOURCE_REGION_CONTRACT_MISSING"
    if evidence.get("verdict") != "FIVE_PARSER_EVIDENCE_COMPLETE":
        return "IR_PUBLICATION_COVERAGE_BLOCKED_INCOMPLETE_PARSER_EVIDENCE"
    if closures["true_unsupported_gaps"].get("count", 0) > 0:
        return "IR_PUBLICATION_COVERAGE_BLOCKED_UNSUPPORTED_GAPS"
    if closures["classified_but_not_admitted"].get("count", 0) > 0:
        return "IR_PUBLICATION_COVERAGE_BLOCKED_CLASSIFIED_GAPS"
    if custom_contract.get("verdict") != "CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION":
        return "IR_PUBLICATION_COVERAGE_BLOCKED_CUSTOM_CONTRACT_MISSING"
    if (
        publication_bundle_contract.get("verdict")
        != "PUBLICATION_BUNDLE_CONTRACT_CONFIRMED_BY_ABC_VALIDATION"
    ):
        return "IR_PUBLICATION_COVERAGE_BLOCKED_BUNDLE_VALIDATION_MISSING"
    return "IR_PUBLICATION_COVERAGE_COMPLETE"


def build_summary(
    parser_schema: dict[str, Any],
    mapping: dict[str, Any],
    source: dict[str, Any],
    matrix: dict[str, Any],
    source_delta: dict[str, Any],
    custom_contract_schema: pathlib.Path | None,
    source_region_schema: pathlib.Path | None,
    source_region_policy: pathlib.Path | None,
    manifest_schema: pathlib.Path | None,
    bundle_validation_summary: pathlib.Path | None,
    next_work_summary: pathlib.Path | None,
) -> dict[str, Any]:
    node_coverage = node_coverage_from_schema(parser_schema)
    field_coverage = field_coverage_from_schema(parser_schema)
    diagnostics = diagnostic_coverage(mapping)
    construct_coverage = source_construct_coverage(mapping)
    evidence = parser_evidence_coverage(matrix, source_delta)
    source_region_contract = source_region_contract_block(
        source_region_schema,
        source_region_policy,
        manifest_schema,
    )
    custom_contract = custom_contract_block(custom_contract_schema)
    tei_profile_contract = tei_profile_contract_block(custom_contract)
    publication_bundle_contract = publication_bundle_contract_block(
        bundle_validation_summary,
    )
    next_work_dashboard = next_work_dashboard_block(next_work_summary)
    gaps = unsupported_gaps(node_coverage, field_coverage, diagnostics, construct_coverage)
    closures = closure_gaps(gaps, custom_contract, tei_profile_contract)
    source_passed = source_authority_passed(source)
    summary = {
        "schema_version": SCHEMA_VERSION,
        "scope": summary_scope(parser_schema, mapping, source, matrix),
        "verdict": publication_verdict(
            source_passed,
            evidence,
            source_region_contract,
            custom_contract,
            publication_bundle_contract,
            gaps,
            closures,
        ),
        "source_authority_gate": source_authority_gate(source),
        "source_region_contract": source_region_contract,
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
        "publication_bundle_contract": publication_bundle_contract,
        "mapping": mapping_block(mapping),
        "node_coverage": node_coverage,
        "field_coverage": field_coverage,
        "diagnostic_coverage": diagnostics,
        "source_construct_coverage": construct_coverage,
        "unsupported_gaps": gaps,
        "closure_gaps": closures,
        "unsupported_derived_closure_coverage": unsupported_derived_closure_coverage(closures),
        "tei_eaj_calibration": {
            "matrix_schema_version": matrix.get("schema_version"),
            "rows_attempted": matrix.get("totals", {}).get("rows_attempted"),
            "materialization_failed": matrix.get("totals", {}).get("materialization_failed"),
            "role": "calibration_not_source_authority",
        },
        "plaintext_policy": PLAINTEXT_POLICY,
    }
    if next_work_dashboard is not None:
        summary["next_work_dashboard"] = next_work_dashboard
    return summary


def render_markdown(summary: dict[str, Any]) -> str:
    node_counts = summary["node_coverage"]["counts_by_class"]
    field_counts = summary["field_coverage"]["counts_by_class"]
    diagnostic_counts = summary["diagnostic_coverage"]["counts_by_class"]
    construct_counts = summary["source_construct_coverage"]["counts_by_class"]
    adjusted_closure = summary["unsupported_derived_closure_coverage"]
    gaps = summary["unsupported_gaps"]
    closures = summary["closure_gaps"]
    source_region_contract = summary["source_region_contract"]
    publication_bundle_contract = summary["publication_bundle_contract"]
    next_work_dashboard = summary.get("next_work_dashboard")
    lines = [
        "# IR Publication Coverage",
        "",
        f"Verdict: `{summary['verdict']}`",
        "",
        "## Source Region Contract",
        "",
        f"Verdict: `{source_region_contract['verdict']}`",
        "",
        f"Schema: `{source_region_contract.get('schema_id')}` `{source_region_contract.get('schema_version')}`",
        "",
        f"Policy: `{source_region_contract.get('policy_id')}` `{source_region_contract.get('policy_version')}`",
        "",
        f"Manifest sidecar role present: `{str(source_region_contract.get('manifest_sidecar_role_present')).lower()}`",
        "",
        "## Publication Bundle Contract",
        "",
        f"Verdict: `{publication_bundle_contract['verdict']}`",
        "",
        f"Evidence: `{publication_bundle_contract.get('path')}`",
        "",
        f"Bundle hash: `{publication_bundle_contract.get('bundle_hash')}`",
        "",
        f"ABC commit: `{publication_bundle_contract.get('abc_commit')}`",
        "",
        f"Validation scope: `{publication_bundle_contract.get('validation_scope')}`",
        "",
        f"Rows validated: `{publication_bundle_contract.get('rows_validated')}`",
        "",
        f"Rows failed: `{publication_bundle_contract.get('rows_failed')}`",
        "",
        "## Node Coverage",
        "",
        "| Class | Node types |",
        "|---|---:|",
    ]
    if isinstance(next_work_dashboard, dict):
        lines.extend(
            [
                "## Next Work Dashboard",
                "",
                f"Verdict: `{next_work_dashboard.get('verdict')}`",
                "",
                f"Evidence: `{next_work_dashboard.get('path')}`",
                "",
                f"Next work items: `{next_work_dashboard.get('next_work_items_count')}`",
                "",
            ]
        )
        for item_id in next_work_dashboard.get("item_ids", []):
            lines.append(f"- `{item_id}`")
        lines.append("")
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
    lines.extend([
        "",
        "## Unsupported-Derived Closure Coverage",
        "",
        adjusted_closure["description"],
        "",
        f"Total raw unsupported-derived rows: {adjusted_closure['total']}",
        "",
        "| Status | Rows |",
        "|---|---:|",
    ])
    for status, count in adjusted_closure["counts_by_status"].items():
        lines.append(f"| `{status}` | {count} |")
    lines.extend([
        "",
        "## Raw Unsupported-Derived Mapping Rows",
        "",
        gaps["description"],
        "",
        f"Count: {gaps['count']}",
        "",
    ])
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
    parser.add_argument(
        "--source-region-schema",
        type=pathlib.Path,
        default=TRUSTED_ABC_SOURCE_REGION_SCHEMA_PATH,
    )
    parser.add_argument(
        "--source-region-policy",
        type=pathlib.Path,
        default=TRUSTED_ABC_SOURCE_REGION_POLICY_PATH,
    )
    parser.add_argument(
        "--manifest-schema",
        type=pathlib.Path,
        default=TRUSTED_ABC_MANIFEST_SCHEMA_PATH,
    )
    parser.add_argument("--bundle-validation-summary", type=pathlib.Path)
    parser.add_argument("--next-work-summary", type=pathlib.Path)
    parser.add_argument("--summary-json", required=True, type=pathlib.Path)
    parser.add_argument("--report-md", required=True, type=pathlib.Path)
    return parser.parse_args()


def write_text(path: pathlib.Path, value: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(value, encoding="utf-8")


def canonical_json(value: object) -> str:
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":")).replace("/", "\\/")


def document_hash(value: object) -> str:
    return sha256_hex(canonical_json(value))


def main() -> None:
    args = parse_args()
    summary = build_summary(
        parser_schema=load_json(args.parser_ir_schema),
        mapping=load_json(args.mapping),
        source=load_json(args.source_summary),
        matrix=load_json(args.matrix_summary),
        source_delta=load_json(args.source_delta_summary),
        custom_contract_schema=args.custom_contract_schema,
        source_region_schema=args.source_region_schema,
        source_region_policy=args.source_region_policy,
        manifest_schema=args.manifest_schema,
        bundle_validation_summary=args.bundle_validation_summary,
        next_work_summary=args.next_work_summary,
    )
    write_json(args.summary_json, summary)
    write_text(args.report_md, render_markdown(summary))


if __name__ == "__main__":
    main()
