#!/usr/bin/env python3
"""Generate an ab-validator-owned AAT->parser-IR mapping from measured AATs."""

from __future__ import annotations

import argparse
import json
import sys
from collections import Counter
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPT_DIR))

import mapper
import mapping_doc
import validate_contract

CATEGORIES = ("LOSS", "AMBIGUITY", "INVENTION", "UNSUPPORTED", "STRUCTURAL")


def walk_inline_kinds(block: dict) -> list[str]:
    kinds: list[str] = []

    def visit_inline(node: dict) -> None:
        kind = node.get("kind")
        if kind is not None:
            kinds.append(kind)
        for key in ("content", "base_content", "reading_content", "upper", "lower"):
            for child in node.get(key, []) or []:
                if isinstance(child, dict):
                    visit_inline(child)

    def visit_block(node: dict) -> None:
        for child in node.get("content", []) or []:
            if isinstance(child, dict):
                visit_inline(child)
        for child in node.get("children", []) or []:
            if isinstance(child, dict):
                visit_block(child)

    visit_block(block)
    return kinds


def map_aat_document(aat: dict) -> tuple[list[dict], list[dict], Counter, Counter, bool]:
    ledger_list: list[dict] = []
    nodes: list[dict] = []
    block_kinds: Counter = Counter()
    inline_kinds: Counter = Counter()
    has_warigaki = False
    offset = 0

    for index, block in enumerate(aat.get("blocks", []) or []):
        block_kinds[block.get("kind")] += 1
        document_inline_kinds = walk_inline_kinds(block)
        inline_kinds.update(document_inline_kinds)
        has_warigaki = has_warigaki or "warigaki" in document_inline_kinds
        offset = mapper.map_block(block, nodes, ledger_list, offset, f"blocks[{index}]")

    mapper.map_meta_source(aat, ledger_list)
    ledger_list.append(
        mapper.ledger(
            "INVENTION",
            "(top-level)",
            "schema_id/schema_hash",
            "parser-IR requires schema_id+schema_hash; AAT supplies only version=1",
        )
    )
    mapper.map_warnings(aat, ledger_list)
    ledger_list.append(
        mapper.ledger(
            "INVENTION",
            "(none)",
            "errors[]",
            "parser-IR requires errors[]; AAT has no errors concept -> defaulted empty",
        )
    )
    return ledger_list, nodes, block_kinds, inline_kinds, has_warigaki


def validate_mapping(document: dict, abc_root: Path) -> None:
    try:
        import jsonschema
    except ImportError as exc:
        raise SystemExit("jsonschema is required for mapping validation") from exc

    schema_path = abc_root / "schemas/aat-parser-ir-mapping.schema.json"
    schema = json.loads(schema_path.read_text())
    jsonschema.Draft202012Validator(schema).validate(document)


def write_report(summary: dict, report_path: Path) -> None:
    category_counts = summary["category_counts"]
    lines = [
        "# AAT-to-Parser-IR Mapping Generation",
        "",
        "Date: 2026-07-03",
        "",
        "## Summary",
        "",
        "| Metric | Value |",
        "|---|---:|",
        f"| files scanned | {summary['files_scanned']} |",
        f"| files failed to parse | {summary['files_failed_to_parse']} |",
        f"| files with UNSUPPORTED | {summary['files_with_unsupported']} |",
        f"| files with warigaki | {summary['files_with_warigaki']} |",
        f"| total parser-IR nodes emitted | {summary['total_parser_ir_nodes_emitted']} |",
        f"| total ledger entries | {summary['total_ledger_entries']} |",
        f"| generated mapping rules | {summary['generated_mapping_rules']} |",
        "",
        "## Category Counts",
        "",
        "| Category | Count |",
        "|---|---:|",
    ]
    lines.extend(
        f"| {category} | {category_counts.get(category, 0)} |"
        for category in CATEGORIES
    )
    lines.extend(
        [
            "",
            "## Schema Hashes",
            "",
            f"- mapping schema hash: `{summary['mapping_schema_hash']}`",
            f"- target parser-IR schema hash: `{summary['target_parser_ir_schema_hash']}`",
            "",
            "## Policy Checks",
            "",
            "- `ruby.direction` projects directly into parser-IR.",
            "- `style` maps to parser-IR `emphasis`.",
            "- `windows-31j-lossy` maps to parser-IR `source.encoding = Shift_JIS` with an `AMBIGUITY` entry.",
            "- Generated mapping is derived from folded measured rule buckets, not the historical 27-rule synthesized table.",
            "",
        ]
    )
    lines.extend(["", "## Inputs", ""])
    lines.extend(f"- `{path}`" for path in summary["aat_dirs"])
    lines.append("")
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text("\n".join(lines))


def main() -> int:
    parser = argparse.ArgumentParser()
    repo_root = SCRIPT_DIR.parents[2]
    parser.add_argument("--aat-dir", type=Path, action="append", required=True)
    parser.add_argument("--abc-root", type=Path, default=Path("../abc"))
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--summary-json", type=Path, required=True)
    parser.add_argument("--report-md", type=Path)
    parser.add_argument("--assert-zero-unsupported", action="store_true")
    parser.add_argument("--mapping-version", default="0.1.1")
    parser.add_argument("--aat-schema", type=Path, default=repo_root / "data/aat-schema.json")
    args = parser.parse_args()

    files = []
    for aat_dir in args.aat_dir:
        files.extend(sorted(aat_dir.glob("*.json")))
    if not files:
        dirs = ", ".join(str(path) for path in args.aat_dir)
        raise SystemExit(f"no AAT JSON files found under: {dirs}")

    category_counts: Counter = Counter()
    mapping_rule_counter: Counter = Counter()
    first_path_by_rule: dict[tuple[str, str, str], str] = {}
    first_note_by_rule: dict[tuple[str, str, str], str] = {}
    block_kind_counts: Counter = Counter()
    inline_kind_counts: Counter = Counter()
    files_failed_to_parse = 0
    files_with_unsupported = 0
    files_with_warigaki = 0
    total_parser_ir_nodes = 0

    for path in files:
        try:
            aat = json.loads(path.read_text())
        except Exception:
            files_failed_to_parse += 1
            continue

        ledger_list, nodes, block_kinds, inline_kinds, has_warigaki = map_aat_document(
            aat
        )
        block_kind_counts.update(block_kinds)
        inline_kind_counts.update(inline_kinds)
        total_parser_ir_nodes += len(nodes)
        if has_warigaki:
            files_with_warigaki += 1

        had_unsupported = False
        for entry in ledger_list:
            category_counts[entry["category"]] += 1
            aat_bucket = mapper.aat_pointer_bucket(entry["aat"])
            key = (entry["category"], aat_bucket, entry["parser_ir"])
            mapping_rule_counter[key] += 1
            first_path_by_rule.setdefault(key, entry["aat"])
            first_note_by_rule.setdefault(key, entry["note"])
            if entry["category"] == "UNSUPPORTED":
                had_unsupported = True
        if had_unsupported:
            files_with_unsupported += 1

    mapping_document = mapping_doc.build_mapping_document_from_counts(
        mapping_rule_counter,
        first_path_by_rule,
        first_note_by_rule,
        repo_root=args.abc_root.resolve(),
        mapping_version=args.mapping_version,
    )
    validate_mapping(mapping_document, args.abc_root.resolve())
    aat_schema_path = args.aat_schema
    if not aat_schema_path.is_absolute():
        aat_schema_path = repo_root / aat_schema_path
    validate_contract.validate_mapping_contract(
        mapping_document,
        json.loads(aat_schema_path.read_text(encoding="utf-8")),
    )

    args.out.parent.mkdir(parents=True, exist_ok=True)
    mapping_doc.write_mapping_document(mapping_document, args.out)

    summary = {
        "files_scanned": len(files),
        "files_failed_to_parse": files_failed_to_parse,
        "files_with_unsupported": files_with_unsupported,
        "files_with_warigaki": files_with_warigaki,
        "total_blocks_scanned": sum(block_kind_counts.values()),
        "total_inline_nodes_scanned": sum(inline_kind_counts.values()),
        "total_parser_ir_nodes_emitted": total_parser_ir_nodes,
        "total_ledger_entries": sum(category_counts.values()),
        "category_counts": {
            category: category_counts.get(category, 0) for category in CATEGORIES
        },
        "block_kind_counts": dict(block_kind_counts),
        "inline_kind_counts": dict(inline_kind_counts),
        "generated_mapping_rules": len(
            mapping_document["transform_rule_descriptions"]
        ),
        "mapping_version": mapping_document["mapping_version"],
        "mapping_schema_hash": mapping_document["mapping_schema_hash"],
        "target_parser_ir_schema_hash": mapping_document[
            "target_parser_ir_schema_hash"
        ],
        "aat_dirs": [str(path) for path in args.aat_dir],
        "mapping_path": str(args.out),
    }

    args.summary_json.parent.mkdir(parents=True, exist_ok=True)
    args.summary_json.write_text(json.dumps(summary, ensure_ascii=False, indent=2) + "\n")
    if args.report_md:
        write_report(summary, args.report_md)

    print(json.dumps(summary, ensure_ascii=False, indent=2))
    if args.assert_zero_unsupported and files_with_unsupported != 0:
        raise SystemExit(f"expected zero UNSUPPORTED files, got {files_with_unsupported}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
