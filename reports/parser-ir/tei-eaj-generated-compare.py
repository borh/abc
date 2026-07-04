#!/usr/bin/env python3
"""Compare generated parser-IR TEI against TEI-EAJ rows.

This is an operator audit. It intentionally invokes both ab-validator's
converter and ABC's publication materializer, then measures generated TEI
structure against the pinned TEI-EAJ XML. Non-zero deltas are reported as
evidence, not treated as process failures.
"""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import shutil
import subprocess
import sys
import xml.etree.ElementTree as ET
from typing import Any

TEI = "{http://www.tei-c.org/ns/1.0}"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workset", required=True, type=pathlib.Path)
    parser.add_argument("--structural-summary", required=True, type=pathlib.Path)
    parser.add_argument("--mapping", required=True, type=pathlib.Path)
    parser.add_argument("--converter-bin", required=True, type=pathlib.Path)
    parser.add_argument("--abc-root", required=True, type=pathlib.Path)
    parser.add_argument("--abc-schema-root", required=True, type=pathlib.Path)
    parser.add_argument("--out-dir", required=True, type=pathlib.Path)
    parser.add_argument("--max-rows", default=0, type=int)
    parser.add_argument("--tei-eaj-file", action="append", default=[])
    parser.add_argument(
        "--adapter-preference",
        default="aozora2html,aozora-epub3,aozora-rs,aozora2",
        help="Comma-separated adapter preference for candidate AAT selection.",
    )
    parser.add_argument(
        "--metadata-record",
        default="examples/v0/example-work/metadata-record.json",
        type=pathlib.Path,
    )
    parser.add_argument(
        "--persons-dir",
        default="examples/v0/example-persons",
        type=pathlib.Path,
    )
    parser.add_argument(
        "--source-manifest",
        default="examples/v0/example-work/source.manifest.json",
        type=pathlib.Path,
    )
    parser.add_argument("--generated-at", default="2026-07-04T00:00:00Z")
    return parser.parse_args()


def load_json(path: pathlib.Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))


def local_name(tag: str) -> str:
    if "}" in tag:
        return tag.rsplit("}", 1)[1]
    return tag


def first_descendant(root: ET.Element, local: str) -> ET.Element | None:
    wanted = f"{TEI}{local}"
    for node in root.iter():
        if node.tag == wanted or local_name(node.tag) == local:
            return node
    return None


def descendants(parent: ET.Element | None, local: str) -> list[ET.Element]:
    if parent is None:
        return []
    wanted = f"{TEI}{local}"
    return [
        node
        for node in parent.iter()
        if node.tag == wanted or local_name(node.tag) == local
    ]


def text_of(node: ET.Element | None) -> str:
    if node is None:
        return ""
    return "".join(node.itertext())


def base_text_of(
    node: ET.Element | None,
    skipped: set[str] | None = None,
) -> str:
    if node is None:
        return ""
    skipped = skipped or {"note", "rp", "rt"}
    parts: list[str] = []

    def walk(element: ET.Element) -> None:
        if local_name(element.tag) in skipped:
            if element.tail:
                parts.append(element.tail)
            return
        if element.text:
            parts.append(element.text)
        for child in list(element):
            walk(child)
        if element.tail:
            parts.append(element.tail)

    walk(node)
    return "".join(parts)


def normalize_body_text(text: str) -> str:
    return re.sub(r"\s+", "", text)


def strip_japanese_parentheses(text: str) -> str:
    return text.replace("（", "").replace("）", "")


def drop_japanese_parenthetical_groups(text: str) -> str:
    return re.sub(r"（[^）]*）", "", text)


def compare_text_surface(generated_text: str, tei_eaj_text: str) -> dict[str, Any]:
    generated = normalize_body_text(generated_text)
    tei_eaj = normalize_body_text(tei_eaj_text)
    if generated == tei_eaj:
        relation = "equal"
    elif tei_eaj and tei_eaj in generated:
        relation = "generated_contains_tei_eaj"
    elif generated and generated in tei_eaj:
        relation = "tei_eaj_contains_generated"
    else:
        relation = "different"

    first_diff = None
    if relation != "equal":
        offset = 0
        for left, right in zip(generated, tei_eaj):
            if left != right:
                break
            offset += 1
        first_diff = {
            "offset": offset,
            "generated_preview": generated[offset : offset + 48],
            "tei_eaj_preview": tei_eaj[offset : offset + 48],
        }

    return {
        "body_base_text_relation": relation,
        "generated_body_base_text_length": len(generated),
        "tei_eaj_body_base_text_length": len(tei_eaj),
        "body_base_text_length_delta": len(generated) - len(tei_eaj),
        "body_base_text_first_diff": first_diff,
    }


def surface_relation(generated_text: str, tei_eaj_text: str) -> dict[str, Any]:
    comparison = compare_text_surface(generated_text, tei_eaj_text)
    return {
        "relation": comparison["body_base_text_relation"],
        "generated_length": comparison["generated_body_base_text_length"],
        "tei_eaj_length": comparison["tei_eaj_body_base_text_length"],
        "length_delta": comparison["body_base_text_length_delta"],
    }


def best_body_text_match_bucket(surfaces: dict[str, dict[str, Any]]) -> str:
    for surface in (
        "base",
        "ruby_expanded",
        "ruby_expanded_parenless",
        "base_drop_parentheticals",
    ):
        if surfaces[surface]["relation"] == "equal":
            return f"{surface}_equal"
    for surface in (
        "ruby_expanded_parenless",
        "ruby_expanded",
        "base_drop_parentheticals",
        "base",
    ):
        relation = surfaces[surface]["relation"]
        if relation != "different":
            return f"{surface}_{relation}"
    return "different"


def body_text_comparison(
    generated_base_text: str,
    tei_eaj_base_text: str,
    generated_ruby_expanded_text: str,
    tei_eaj_ruby_expanded_text: str,
) -> dict[str, Any]:
    base = compare_text_surface(generated_base_text, tei_eaj_base_text)
    surfaces = {
        "base": surface_relation(generated_base_text, tei_eaj_base_text),
        "ruby_expanded": surface_relation(
            generated_ruby_expanded_text, tei_eaj_ruby_expanded_text
        ),
        "ruby_expanded_parenless": surface_relation(
            strip_japanese_parentheses(generated_ruby_expanded_text),
            strip_japanese_parentheses(tei_eaj_ruby_expanded_text),
        ),
        "base_drop_parentheticals": surface_relation(
            drop_japanese_parenthetical_groups(generated_base_text),
            drop_japanese_parenthetical_groups(tei_eaj_base_text),
        ),
    }
    base["surface_relations"] = surfaces
    base["body_text_match_bucket"] = best_body_text_match_bucket(surfaces)
    return base


def tei_counts(path: pathlib.Path) -> dict[str, Any]:
    root = ET.parse(path).getroot()
    body = first_descendant(root, "body")
    back = first_descendant(root, "back")
    body_notes = descendants(body, "note")
    back_notes = descendants(back, "note")
    back_source_notes = [
        note for note in back_notes if note.attrib.get("type") == "source-attribution"
    ]
    return {
        "path": str(path),
        "body_p_count": len(descendants(body, "p")),
        "body_note_count": len(body_notes),
        "back_note_count": len(back_notes),
        "back_source_note_count": len(back_source_notes),
        "body_text": text_of(body),
        "body_base_text": base_text_of(body),
        "body_ruby_expanded_text": base_text_of(body, {"note", "rp"}),
        "back_text": text_of(back),
    }


def sanitize(value: str) -> str:
    cleaned = re.sub(r"[^A-Za-z0-9_.-]+", "_", value)
    return cleaned.strip("._") or "row"


def row_key(row: dict[str, Any]) -> str:
    return str(row.get("tei", {}).get("tei_eaj_file", ""))


def adapter_rank(adapter: str | None, preference: dict[str, int]) -> int:
    if adapter is None:
        return len(preference) + 100
    return preference.get(adapter, len(preference) + 100)


def select_candidate(
    row: dict[str, Any],
    preference: dict[str, int],
) -> dict[str, Any] | None:
    candidates = []
    for candidate in row.get("aat_inputs", []):
        if not candidate.get("conversion", {}).get("success"):
            continue
        if not candidate.get("parser_ir", {}).get("paragraphs_represented"):
            continue
        path = pathlib.Path(candidate.get("path", ""))
        if not path.is_file():
            continue
        aat = candidate.get("aat", {})
        parser_ir = candidate.get("parser_ir", {})
        needs_source_note = bool(aat.get("final_source_attribution_candidate"))
        source_note_ok = (not needs_source_note) or bool(
            parser_ir.get("source_attribution_represented")
        )
        candidates.append(
            (
                0 if source_note_ok else 1,
                0 if candidate.get("verdict", {}).get("residual_free") else 1,
                adapter_rank(aat.get("adapter"), preference),
                candidate.get("label", ""),
                candidate,
            )
        )
    if not candidates:
        return None
    candidates.sort(key=lambda item: item[:4])
    return candidates[0][-1]


def paragraph_delta_bucket(generated: int | None, tei_eaj: int | None) -> str:
    if generated is None or tei_eaj is None:
        return "unknown"
    delta = generated - tei_eaj
    if delta == 0:
        return "exact"
    if generated == 1 and tei_eaj > 1:
        return "collapsed"
    if delta > 0:
        return "over_split"
    return "under_split"


def run_checked(command: list[str], *, cwd: pathlib.Path | None = None) -> None:
    subprocess.run(command, cwd=cwd, check=True)


def materialize_row(
    args: argparse.Namespace,
    row: dict[str, Any],
    candidate: dict[str, Any],
    tei_eaj_path: pathlib.Path,
    row_dir: pathlib.Path,
) -> dict[str, Any]:
    parser_ir_path = row_dir / "parser-ir.json"
    divergence_path = row_dir / "divergence.json"
    publication_dir = row_dir / "publication"
    publication_dir.mkdir(parents=True, exist_ok=True)

    run_checked(
        [
            str(args.converter_bin),
            "convert",
            "--aat",
            str(candidate["path"]),
            "--mapping",
            str(args.mapping),
            "--parser-ir-out",
            str(parser_ir_path),
            "--divergence-out",
            str(divergence_path),
            "--abc-root",
            str(args.abc_schema_root),
        ]
    )

    run_checked(
        [
            "clojure",
            "-M:abc/materialize-publication",
            str(parser_ir_path),
            str(args.metadata_record),
            str(args.persons_dir),
            str(publication_dir),
            "--source-manifest",
            str(args.source_manifest),
            "--generated-at",
            args.generated_at,
        ],
        cwd=args.abc_root,
    )

    parser_ir = load_json(parser_ir_path)
    validation = load_json(publication_dir / "tei-validation-result.json")
    generated = tei_counts(publication_dir / "tei.xml")
    tei_eaj = tei_counts(tei_eaj_path)
    source_note_texts = [
        node.get("text", "")
        for node in parser_ir.get("nodes", [])
        if node.get("type") == "source-note"
    ]
    non_empty_source_notes = [text for text in source_note_texts if text]
    source_note_in_body = any(
        text in generated["body_text"] for text in non_empty_source_notes
    )
    source_note_in_back = any(
        text in generated["back_text"] for text in non_empty_source_notes
    )
    text = body_text_comparison(
        generated["body_base_text"],
        tei_eaj["body_base_text"],
        generated["body_ruby_expanded_text"],
        tei_eaj["body_ruby_expanded_text"],
    )

    generated_body_p = generated["body_p_count"]
    tei_eaj_body_p = tei_eaj["body_p_count"]
    return {
        "work_id": row.get("tei", {}).get("work_id"),
        "title": row.get("tei", {}).get("title"),
        "tei_eaj_file": row.get("tei", {}).get("tei_eaj_file"),
        "selected_aat": {
            "label": candidate.get("label"),
            "path": candidate.get("path"),
            "adapter": candidate.get("aat", {}).get("adapter"),
            "adapter_version": candidate.get("aat", {}).get("adapter_version"),
        },
        "materialization": {
            "status": validation.get("status"),
            "findings_count": len(validation.get("findings", [])),
        },
        "parser_ir": {
            "schema_hash": parser_ir.get("schema_hash"),
            "nodes": len(parser_ir.get("nodes", [])),
            "paragraph_count": len(parser_ir.get("paragraphs", [])),
            "source_note_count": len(source_note_texts),
        },
        "generated_tei": {
            "body_p_count": generated_body_p,
            "body_note_count": generated["body_note_count"],
            "back_note_count": generated["back_note_count"],
            "back_source_note_count": generated["back_source_note_count"],
        },
        "tei_eaj": {
            "body_p_count": tei_eaj_body_p,
            "body_note_count": tei_eaj["body_note_count"],
            "back_note_count": tei_eaj["back_note_count"],
            "back_source_note_count": tei_eaj["back_source_note_count"],
            "workset_p_count": row.get("tei", {}).get("tei_eaj_p_count"),
            "workset_note_count": row.get("tei", {}).get("tei_eaj_note_count"),
        },
        "deltas": {
            "body_p_count": generated_body_p - tei_eaj_body_p,
            "body_note_count": generated["body_note_count"] - tei_eaj["body_note_count"],
            "back_source_note_count": generated["back_source_note_count"]
            - tei_eaj["back_source_note_count"],
        },
        "text": text,
        "classification": {
            "paragraph_delta_bucket": paragraph_delta_bucket(
                generated_body_p, tei_eaj_body_p
            ),
            "source_note_body_excluded": bool(non_empty_source_notes)
            and not source_note_in_body,
            "source_note_back_present": bool(non_empty_source_notes)
            and source_note_in_back,
        },
    }


def render_markdown(summary: dict[str, Any]) -> str:
    lines = [
        "# Generated Parser-IR TEI vs TEI-EAJ Workset Audit",
        "",
        "This report materializes ABC TEI from parser-IR, then compares the generated TEI structure against pinned TEI-EAJ XML. It measures structural deltas; it does not treat TEI-EAJ Level 4 enrichment as parser-IR admission scope.",
        "",
        "## Totals",
        "",
        "| metric | value |",
        "|---|---:|",
    ]
    for key, value in summary["totals"].items():
        lines.append(f"| {key} | {value} |")

    lines.extend(
        [
            "",
            "## Paragraph Delta Buckets",
            "",
            "| bucket | rows |",
            "|---|---:|",
        ]
    )
    for bucket, count in sorted(summary["paragraph_delta_buckets"].items()):
        lines.append(f"| {bucket} | {count} |")

    lines.extend(
        [
            "",
            "## Body Text Relation Buckets",
            "",
            "| relation | rows |",
            "|---|---:|",
        ]
    )
    for relation, count in sorted(summary["body_text_relation_buckets"].items()):
        lines.append(f"| {relation} | {count} |")

    lines.extend(
        [
            "",
            "## Body Text Match Buckets",
            "",
            "| bucket | rows |",
            "|---|---:|",
        ]
    )
    for bucket, count in sorted(summary["body_text_match_buckets"].items()):
        lines.append(f"| {bucket} | {count} |")

    lines.extend(
        [
            "",
            "## Rows",
            "",
            "| work_id | TEI-EAJ file | adapter | generated body p | TEI-EAJ body p | delta | bucket | base text | best text match | source note |",
            "|---|---|---|---:|---:|---:|---|---|---|---|",
        ]
    )
    for row in summary["rows"]:
        source_note = "-"
        if row["parser_ir"]["source_note_count"]:
            source_note = (
                "back"
                if row["classification"]["source_note_back_present"]
                else "not-back"
            )
        lines.append(
            "| {work_id} | `{tei_eaj_file}` | {adapter} | {generated} | {tei_eaj} | {delta} | {bucket} | {body_text} | {body_text_match} | {source_note} |".format(
                work_id=row.get("work_id"),
                tei_eaj_file=row.get("tei_eaj_file"),
                adapter=row["selected_aat"].get("adapter"),
                generated=row["generated_tei"]["body_p_count"],
                tei_eaj=row["tei_eaj"]["body_p_count"],
                delta=row["deltas"]["body_p_count"],
                bucket=row["classification"]["paragraph_delta_bucket"],
                body_text=row.get("text", {}).get("body_base_text_relation"),
                body_text_match=row.get("text", {}).get("body_text_match_bucket"),
                source_note=source_note,
            )
        )
    lines.append("")
    return "\n".join(lines)


def main() -> int:
    args = parse_args()
    args.out_dir.mkdir(parents=True, exist_ok=True)
    if not args.converter_bin.is_file():
        raise SystemExit(f"missing converter binary: {args.converter_bin}")
    if not args.abc_root.is_dir():
        raise SystemExit(f"missing ABC repo: {args.abc_root}")
    if shutil.which("clojure") is None:
        raise SystemExit("missing clojure executable")

    workset = load_json(args.workset)
    structural = load_json(args.structural_summary)
    tei_root = pathlib.Path(workset["tei_eaj_source"]["root"])
    allowed_files = set(args.tei_eaj_file)
    preference = {
        adapter: index
        for index, adapter in enumerate(
            item.strip()
            for item in args.adapter_preference.split(",")
            if item.strip()
        )
    }
    workset_files = {row["tei_eaj_file"]: row for row in workset.get("files", [])}
    rows = []
    skipped = []
    attempted = 0

    for row in structural.get("rows", []):
        tei_file = row_key(row)
        if allowed_files and tei_file not in allowed_files:
            continue
        workset_row = workset_files.get(tei_file)
        if workset_row is None:
            skipped.append({"tei_eaj_file": tei_file, "reason": "not_in_workset"})
            continue
        tei_eaj_path = tei_root / tei_file
        if not tei_eaj_path.is_file():
            skipped.append({"tei_eaj_file": tei_file, "reason": "tei_eaj_missing"})
            continue
        candidate = select_candidate(row, preference)
        if candidate is None:
            skipped.append(
                {"tei_eaj_file": tei_file, "reason": "no_materializable_aat"}
            )
            continue
        if args.max_rows and attempted >= args.max_rows:
            continue

        attempted += 1
        work_id = str(row.get("tei", {}).get("work_id") or "unknown")
        row_dir = args.out_dir / "rows" / sanitize(f"{attempted:04d}_{work_id}_{tei_file}")
        row_dir.mkdir(parents=True, exist_ok=True)
        try:
            rows.append(materialize_row(args, row, candidate, tei_eaj_path, row_dir))
        except subprocess.CalledProcessError as error:
            rows.append(
                {
                    "work_id": row.get("tei", {}).get("work_id"),
                    "tei_eaj_file": tei_file,
                    "selected_aat": {
                        "label": candidate.get("label"),
                        "path": candidate.get("path"),
                        "adapter": candidate.get("aat", {}).get("adapter"),
                        "adapter_version": candidate.get("aat", {}).get("adapter_version"),
                    },
                    "materialization": {
                        "status": "failed",
                        "returncode": error.returncode,
                    },
                    "classification": {
                        "paragraph_delta_bucket": "materialization_failed"
                    },
                }
            )

    buckets: dict[str, int] = {}
    body_text_buckets: dict[str, int] = {}
    body_text_match_buckets: dict[str, int] = {}
    materialized = 0
    for row in rows:
        bucket = row.get("classification", {}).get("paragraph_delta_bucket", "unknown")
        buckets[bucket] = buckets.get(bucket, 0) + 1
        relation = row.get("text", {}).get("body_base_text_relation", "unknown")
        body_text_buckets[relation] = body_text_buckets.get(relation, 0) + 1
        match_bucket = row.get("text", {}).get("body_text_match_bucket", "unknown")
        body_text_match_buckets[match_bucket] = (
            body_text_match_buckets.get(match_bucket, 0) + 1
        )
        if row.get("materialization", {}).get("status") == "passed":
            materialized += 1

    summary = {
        "schema_version": "tei-eaj-generated-comparison-v1",
        "inputs": {
            "workset": str(args.workset),
            "structural_summary": str(args.structural_summary),
            "mapping": str(args.mapping),
            "abc_root": str(args.abc_root),
            "adapter_preference": list(preference.keys()),
            "max_rows": args.max_rows,
            "tei_eaj_file_filter": args.tei_eaj_file,
        },
        "totals": {
            "rows_attempted": len(rows),
            "materialization_succeeded": materialized,
            "materialization_failed": len(rows) - materialized,
            "rows_skipped": len(skipped),
        },
        "paragraph_delta_buckets": buckets,
        "body_text_relation_buckets": body_text_buckets,
        "body_text_match_buckets": body_text_match_buckets,
        "rows": rows,
        "skipped": skipped,
    }

    (args.out_dir / "summary.json").write_text(
        json.dumps(summary, ensure_ascii=False, indent=2) + "\n",
        encoding="utf-8",
    )
    (args.out_dir / "report.md").write_text(
        render_markdown(summary),
        encoding="utf-8",
    )
    print(
        "generated TEI-EAJ comparison: "
        f"{materialized}/{len(rows)} materialized, {len(skipped)} skipped"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
