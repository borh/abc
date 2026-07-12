#!/usr/bin/env python
"""Generate a disposable AAT->parser-IR mapping document from measured rules."""

import argparse
import json
import re
from collections import Counter
from pathlib import Path

import c14n


PROBE_DIR = Path(__file__).resolve().parent
REPO_ROOT = PROBE_DIR.parents[1]
CATEGORY_PREFIX = {
    "AMBIGUITY": "A",
    "INVENTION": "I",
    "LOSS": "L",
    "STRUCTURAL": "S",
    "UNSUPPORTED": "U",
}
CATEGORY_ORDER = tuple(CATEGORY_PREFIX)
ACTION_BY_CATEGORY = {
    "AMBIGUITY": "project",
    "INVENTION": "invent",
    "LOSS": "drop",
    "STRUCTURAL": "flatten",
    "UNSUPPORTED": "drop",
}
LOSS_TAXONOMY = {
    "LOSS": {
        "description": "AAT carries information that parser-IR has no field for.",
        "default_action": "drop-sidecar",
        "records_sidecar": True,
    },
    "INVENTION": {
        "description": "parser-IR requires a value that AAT does not supply.",
        "default_action": "invent",
        "records_sidecar": False,
    },
    "AMBIGUITY": {
        "description": "Same concept exists in both schemas but with different semantics or range.",
        "default_action": "drop-sidecar",
        "records_sidecar": True,
    },
    "UNSUPPORTED": {
        "description": "AAT node kind has no parser-IR representation.",
        "default_action": "drop-sidecar",
        "records_sidecar": True,
    },
    "STRUCTURAL": {
        "description": "Tree-shape differences force boundary/span loss.",
        "default_action": "drop-sidecar",
        "records_sidecar": True,
    },
}
SYNTHETIC_EVIDENCE_DESCRIPTIONS = [
    {
        "evidence_id": "SYN-005",
        "parser_ir_pointer": "source.work_content_hash",
        "source": "converter_policy",
        "action": "declare",
        "description": "Carries authoritative bundle identity supplied at conversion, with primary-text identity fallback only for historical callers.",
    }
]


def aat_pointer_bucket(pointer):
    bucket = re.sub(r"\[[0-9]+\]", "[]", pointer)
    return re.sub(r"^([A-Za-z0-9_.]+)=.*$", r"\1", bucket)


def parser_ir_pointer(value):
    return None if value == "(none)" else value


def source_pointer(value):
    return None if value in ("(none)", "(top-level)") else value


def summarize_ledger(ledger_entries):
    counts = Counter()
    first_path = {}
    first_note = {}
    for entry in ledger_entries:
        key = (
            entry["category"],
            aat_pointer_bucket(entry["aat"]),
            entry["parser_ir"],
        )
        counts[key] += 1
        first_path.setdefault(key, entry["aat"])
        first_note.setdefault(key, entry["note"])
    return counts, first_path, first_note


def build_mapping_document_from_counts(
    rule_counts,
    first_path_by_rule,
    first_note_by_rule,
    repo_root=REPO_ROOT,
    mapping_version="0.4.0",
):
    rules = []
    for category in CATEGORY_ORDER:
        category_keys = sorted(key for key in rule_counts if key[0] == category)
        for index, key in enumerate(category_keys, start=1):
            _, aat, target = key
            count = rule_counts[key]
            occurrence_word = "occurrence" if count == 1 else "occurrences"
            rules.append(
                {
                    "rule_id": f"{CATEGORY_PREFIX[category]}-{index:02d}",
                    "category": category,
                    "aat_pointer": source_pointer(aat),
                    "parser_ir_pointer": parser_ir_pointer(target),
                    "action": ACTION_BY_CATEGORY[category],
                    "description": (
                        f"Observed {count} {occurrence_word}; "
                        f"first_path={first_path_by_rule[key]}. {first_note_by_rule[key]}"
                    ),
                }
            )

    return {
        "mapping_id": "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe",
        "mapping_version": mapping_version,
        "mapping_schema_hash": c14n.schema_hash(
            repo_root / "schemas" / "aat-parser-ir-mapping.schema.json"
        ),
        "source_aat_version": 1,
        "target_parser_ir_schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
        "target_parser_ir_schema_hash": c14n.schema_hash(
            repo_root / "schemas" / "parser-ir.schema.json"
        ),
        "transform_rule_descriptions": rules,
        "synthetic_evidence_descriptions": SYNTHETIC_EVIDENCE_DESCRIPTIONS,
        "loss_taxonomy": LOSS_TAXONOMY,
    }


def build_mapping_document(ledger_entries, repo_root=REPO_ROOT, mapping_version="0.4.0"):
    counts, first_path, first_note = summarize_ledger(ledger_entries)
    return build_mapping_document_from_counts(
        counts,
        first_path,
        first_note,
        repo_root,
        mapping_version=mapping_version,
    )


def write_mapping_document(doc, path):
    with open(path, "w", encoding="utf-8") as f:
        json.dump(doc, f, ensure_ascii=False, indent=2)
        f.write("\n")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("ledger_json", help="JSON file containing an array of probe ledger entries")
    parser.add_argument("--out", default=str(PROBE_DIR / "mapping.generated.json"))
    parser.add_argument("--mapping-version", default="0.4.0")
    args = parser.parse_args()

    with open(args.ledger_json, encoding="utf-8") as f:
        doc = build_mapping_document(json.load(f), mapping_version=args.mapping_version)
    write_mapping_document(doc, args.out)


if __name__ == "__main__":
    main()
