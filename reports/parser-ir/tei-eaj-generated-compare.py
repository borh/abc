#!/usr/bin/env python3
"""Compare generated parser-IR TEI against TEI-EAJ rows.

This is an operator audit. It intentionally invokes both ab-validator's
converter and ABC's publication materializer, then measures generated TEI
structure against the pinned TEI-EAJ XML. Non-zero deltas are reported as
evidence, not treated as process failures.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import json
import pathlib
import re
import shutil
import subprocess
import sys
import xml.etree.ElementTree as ET
from typing import Any

TEI = "{http://www.tei-c.org/ns/1.0}"

TEI_STRUCTURE_TAGS = (
    "front",
    "body",
    "back",
    "div",
    "p",
    "sp",
    "speaker",
    "stage",
    "castList",
    "castItem",
    "role",
    "roleName",
    "said",
    "l",
    "lg",
    "note",
    "pb",
    "lb",
    "ruby",
    "rb",
    "rt",
    "persName",
    "placeName",
    "rs",
)


REQUIRED_PARSERS = (
    "aozora2html",
    "aozora-epub3",
    "aozora-rs",
    "aozora2",
    "aozora",
)


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
        default=",".join(REQUIRED_PARSERS),
        help="Comma-separated adapter preference for candidate AAT selection.",
    )
    parser.add_argument(
        "--candidate-mode",
        choices=["selected", "all"],
        default="selected",
        help="Materialize only the preferred AAT candidate, or all materializable candidates per TEI-EAJ row.",
    )
    parser.add_argument(
        "--jobs",
        default=1,
        type=int,
        help="Number of TEI materialization jobs to run concurrently.",
    )
    parser.add_argument(
        "--materialization-mode",
        choices=["batch", "per-row"],
        default="batch",
        help="Use one ABC batch materializer process, or the legacy process-per-row path.",
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


def write_json(path: pathlib.Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(value, ensure_ascii=False, sort_keys=True, indent=2) + "\n",
        encoding="utf-8",
    )


def logical_report_path(path: pathlib.Path) -> str:
    text = str(path)
    marker = "/.worktrees/"
    if marker not in text:
        return text
    repo_root, rest = text.split(marker, 1)
    parts = rest.split("/", 1)
    if len(parts) == 1:
        return repo_root
    return f"{repo_root}/{parts[1]}"


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


def tag_counts(parent: ET.Element | None) -> dict[str, int]:
    if parent is None:
        return {}
    counts: dict[str, int] = {}
    for node in parent.iter():
        name = local_name(node.tag)
        counts[name] = counts.get(name, 0) + 1
    return counts


def selected_tag_counts(counts: dict[str, int]) -> dict[str, int]:
    return {tag: counts[tag] for tag in TEI_STRUCTURE_TAGS if counts.get(tag)}


def tei_structure_profiles(
    body_counts: dict[str, int],
    document_counts: dict[str, int],
    *,
    back_source_note_count: int,
) -> list[str]:
    profiles: list[str] = []
    if any(
        body_counts.get(tag, 0)
        for tag in ("sp", "speaker", "stage", "castList", "castItem", "role")
    ):
        profiles.append("drama")
    if any(body_counts.get(tag, 0) for tag in ("l", "lg")):
        profiles.append("verse")
    elif line_break_dominant(body_counts):
        profiles.append("lineated_text")
    if any(
        body_counts.get(tag, 0)
        for tag in ("said", "persName", "placeName", "roleName", "rs")
    ):
        profiles.append("lv4_enrichment")
    if body_counts.get("note", 0) or back_source_note_count:
        profiles.append("notes")
    if document_counts.get("front", 0) or document_counts.get("back", 0):
        profiles.append("front_back_matter")
    return profiles or ["plain_prose"]


def line_break_dominant(body_counts: dict[str, int]) -> bool:
    lb_count = body_counts.get("lb", 0)
    paragraph_count = body_counts.get("p", 0)
    return lb_count >= 2 and lb_count >= max(2, paragraph_count * 2)


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


def is_missing_body_gap_paragraph(element: ET.Element) -> bool:
    children = list(element)
    return (
        (element.text or "").strip() == ""
        and len(children) == 1
        and local_name(children[0].tag) == "gap"
        and children[0].attrib.get("reason") == "missing"
        and (children[0].tail or "").strip() == ""
    )


def tei_counts(path: pathlib.Path) -> dict[str, Any]:
    root = ET.parse(path).getroot()
    body = first_descendant(root, "body")
    back = first_descendant(root, "back")
    document_tag_counts = selected_tag_counts(tag_counts(root))
    body_tag_counts = selected_tag_counts(tag_counts(body))
    body_paragraphs = descendants(body, "p")
    missing_body_gap_paragraphs = [
        paragraph
        for paragraph in body_paragraphs
        if is_missing_body_gap_paragraph(paragraph)
    ]
    body_notes = descendants(body, "note")
    back_notes = descendants(back, "note")
    back_source_notes = [
        note for note in back_notes if note.attrib.get("type") == "source-attribution"
    ]
    structure_profiles = tei_structure_profiles(
        body_tag_counts,
        document_tag_counts,
        back_source_note_count=len(back_source_notes),
    )
    return {
        "path": str(path),
        "body_p_count": len(body_paragraphs) - len(missing_body_gap_paragraphs),
        "body_p_count_raw": len(body_paragraphs),
        "body_missing_gap_p_count": len(missing_body_gap_paragraphs),
        "body_note_count": len(body_notes),
        "back_note_count": len(back_notes),
        "back_source_note_count": len(back_source_notes),
        "body_tag_counts": body_tag_counts,
        "document_tag_counts": document_tag_counts,
        "structure_profile": structure_profiles[0],
        "structure_profiles": structure_profiles,
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


def materializable_candidates(
    row: dict[str, Any],
    preference: dict[str, int],
) -> list[dict[str, Any]]:
    candidates = []
    for candidate in row.get("aat_inputs", []):
        if not candidate.get("conversion", {}).get("success"):
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
    candidates.sort(key=lambda item: item[:4])
    return [item[-1] for item in candidates]


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


def nullable_delta(left: int | None, right: int | None) -> int | None:
    if left is None or right is None:
        return None
    return left - right


def paragraph_origin_bucket(
    *,
    aat_paragraph_blocks: int | None,
    aat_raw_only_parser_residue_blocks: int | None,
    parser_ir_paragraphs: int | None,
    generated_body_p: int | None,
    tei_eaj_body_p: int | None,
    paragraph_rendering: dict[str, Any] | None = None,
) -> str:
    if generated_body_p is None or tei_eaj_body_p is None:
        return "unknown"
    if generated_body_p == tei_eaj_body_p:
        return "aligned"
    if parser_ir_paragraphs is not None and generated_body_p != parser_ir_paragraphs:
        missing_generated = parser_ir_paragraphs - generated_body_p
        rendering = paragraph_rendering or {}
        if (
            missing_generated > 0
            and rendering.get("source_note_back_ranges") == missing_generated
        ):
            return "source_note_back_routing"
        if (
            missing_generated > 0
            and rendering.get("empty_body_ranges") == missing_generated
        ):
            return "empty_body_paragraph_range"
        return "renderer_paragraph_mismatch"
    if (
        aat_paragraph_blocks is not None
        and parser_ir_paragraphs is not None
        and aat_paragraph_blocks != parser_ir_paragraphs
    ):
        if (
            parser_ir_paragraphs == 0
            and aat_raw_only_parser_residue_blocks is not None
            and aat_raw_only_parser_residue_blocks == aat_paragraph_blocks
        ):
            return "adapter_raw_only"
        rendering = paragraph_rendering or {}
        if (
            aat_paragraph_blocks > parser_ir_paragraphs
            and aat_paragraph_blocks - parser_ir_paragraphs
            == rendering.get("page_break_nodes")
        ):
            return "page_break_projection"
        return "converter_paragraph_mismatch"
    if aat_paragraph_blocks is None:
        return "unknown"
    if aat_paragraph_blocks == 1 and tei_eaj_body_p > 1:
        return "adapter_collapsed"
    if aat_paragraph_blocks < tei_eaj_body_p:
        return "adapter_under_segmented"
    if aat_paragraph_blocks > tei_eaj_body_p:
        return "adapter_over_segmented"
    return "unclassified"


def node_renders_body_p_content(node: dict[str, Any]) -> bool:
    node_type = node.get("type")
    if node_type in {"text", "gaiji", "editor-note", "emphasis"}:
        return True
    if node_type == "ruby":
        ruby = node.get("ruby", {})
        return bool(ruby.get("base") and ruby.get("reading"))
    if node_type in {"indentation", "quote"}:
        return bool(node.get("text"))
    if node_type == "source-note":
        return node.get("placement") == "body" and bool(node.get("text"))
    return False


def paragraph_rendering_summary(parser_ir: dict[str, Any]) -> dict[str, Any]:
    nodes = parser_ir.get("nodes", [])
    paragraphs = parser_ir.get("paragraphs", [])
    summary = {
        "total_ranges": len(paragraphs),
        "page_break_nodes": sum(1 for node in nodes if node.get("type") == "page-break"),
        "body_ranges": 0,
        "source_note_ranges": 0,
        "empty_body_ranges": 0,
        "structural_only_body_ranges": 0,
        "body_ranges_with_inline_content": 0,
        "source_note_back_ranges": 0,
        "source_note_front_ranges": 0,
        "source_note_body_ranges": 0,
    }
    for paragraph in paragraphs:
        role = paragraph.get("role")
        node_range = paragraph.get("node_range", {})
        start = node_range.get("start")
        end = node_range.get("end")
        if not isinstance(start, int) or not isinstance(end, int):
            continue
        node_slice = nodes[start:end]
        if role == "source-note":
            summary["source_note_ranges"] += 1
            placements = {
                node.get("placement")
                for node in node_slice
                if node.get("type") == "source-note"
            }
            if "back" in placements:
                summary["source_note_back_ranges"] += 1
            if "front" in placements:
                summary["source_note_front_ranges"] += 1
            if "body" in placements:
                summary["source_note_body_ranges"] += 1
            continue
        if role == "body":
            summary["body_ranges"] += 1
            if start == end:
                summary["empty_body_ranges"] += 1
            elif any(node_renders_body_p_content(node) for node in node_slice):
                summary["body_ranges_with_inline_content"] += 1
            else:
                summary["structural_only_body_ranges"] += 1
    return summary


def is_parser_raw_node(node: Any) -> bool:
    return (
        isinstance(node, dict)
        and node.get("kind") == "raw"
        and node.get("x-provenance") == "parser-derived"
    )


def collect_raw_stats(value: Any, stats: dict[str, int]) -> None:
    if isinstance(value, dict):
        if value.get("kind") == "raw":
            stats["raw_nodes_total"] += 1
            source = value.get("source")
            if isinstance(source, str) and source:
                stats["raw_nodes_with_source"] += 1
            else:
                stats["raw_nodes_empty_source"] += 1
            provenance = value.get("x-provenance")
            if provenance == "parser-derived":
                stats["parser_derived_raw_nodes"] += 1
            elif provenance == "source-derived":
                stats["source_derived_raw_nodes"] += 1
        for child in value.values():
            collect_raw_stats(child, stats)
    elif isinstance(value, list):
        for child in value:
            collect_raw_stats(child, stats)


def raw_stats(aat_doc: dict[str, Any]) -> dict[str, int]:
    stats = {
        "raw_nodes_total": 0,
        "raw_nodes_empty_source": 0,
        "raw_nodes_with_source": 0,
        "parser_derived_raw_nodes": 0,
        "source_derived_raw_nodes": 0,
        "raw_only_parser_residue_blocks": 0,
        "raw_only_empty_parser_residue_blocks": 0,
    }
    collect_raw_stats(aat_doc, stats)
    for block in aat_doc.get("blocks", []):
        if not isinstance(block, dict) or block.get("kind") != "paragraph":
            continue
        content = block.get("content")
        if (
            isinstance(content, list)
            and content
            and all(is_parser_raw_node(node) for node in content)
        ):
            stats["raw_only_parser_residue_blocks"] += 1
            if all(node.get("source", "") == "" for node in content):
                stats["raw_only_empty_parser_residue_blocks"] += 1
    return stats


def aat_summary(candidate: dict[str, Any], aat_doc: dict[str, Any]) -> dict[str, Any]:
    aat = candidate.get("aat", {})
    return {
        "block_count": aat.get("block_count"),
        "block_kinds": aat.get("block_kinds", {}),
        "paragraph_blocks": aat.get("paragraph_blocks"),
        "final_block_kind": aat.get("final_block_kind"),
        "final_source_attribution_candidate": aat.get(
            "final_source_attribution_candidate"
        ),
        "hints_count": len(aat.get("hints", [])),
    } | raw_stats(aat_doc)


def run_checked(command: list[str], *, cwd: pathlib.Path | None = None) -> None:
    subprocess.run(command, cwd=cwd, check=True)


def convert_task_outputs(
    args: argparse.Namespace,
    task: dict[str, Any],
) -> dict[str, pathlib.Path]:
    parser_ir_path = task["row_dir"] / "parser-ir.json"
    divergence_path = task["row_dir"] / "divergence.json"
    publication_dir = task["row_dir"] / "publication"
    publication_dir.mkdir(parents=True, exist_ok=True)
    run_checked(
        [
            str(args.converter_bin),
            "convert",
            "--aat",
            str(task["candidate"]["path"]),
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
    return {
        "parser_ir_path": parser_ir_path,
        "divergence_path": divergence_path,
        "publication_dir": publication_dir,
    }


def materialize_single_publication(
    args: argparse.Namespace,
    parser_ir_path: pathlib.Path,
    publication_dir: pathlib.Path,
) -> None:
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


def materialize_batch(
    args: argparse.Namespace,
    tasks: list[dict[str, Any]],
) -> None:
    batch_path = args.out_dir / "materialization-batch.json"
    summary_path = args.out_dir / "materialization-summary.json"
    write_json(
        batch_path,
        {
            "jobs": [
                {
                    "id": str(task["task_index"]),
                    "parser_ir_path": str(task["parser_ir_path"]),
                    "metadata_record_path": str(args.metadata_record),
                    "persons_dir": str(args.persons_dir),
                    "source_manifest_path": str(args.source_manifest),
                    "output_dir": str(task["publication_dir"]),
                    "generated_at": args.generated_at,
                }
                for task in tasks
            ]
        },
    )
    run_checked(
        [
            "clojure",
            "-M:abc/materialize-publications-batch",
            str(batch_path),
            "--summary",
            str(summary_path),
            "--jobs",
            str(max(1, args.jobs)),
        ],
        cwd=args.abc_root,
    )


def build_materialized_row(
    row: dict[str, Any],
    candidate: dict[str, Any],
    tei_eaj_path: pathlib.Path,
    parser_ir_path: pathlib.Path,
    publication_dir: pathlib.Path,
) -> dict[str, Any]:
    aat_doc = load_json(pathlib.Path(candidate["path"]))
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
    aat = aat_summary(candidate, aat_doc)
    parser_ir_paragraph_count = len(parser_ir.get("paragraphs", []))
    paragraph_rendering = paragraph_rendering_summary(parser_ir)
    paragraph_origin = paragraph_origin_bucket(
        aat_paragraph_blocks=aat["paragraph_blocks"],
        aat_raw_only_parser_residue_blocks=aat["raw_only_parser_residue_blocks"],
        parser_ir_paragraphs=parser_ir_paragraph_count,
        generated_body_p=generated_body_p,
        tei_eaj_body_p=tei_eaj_body_p,
        paragraph_rendering=paragraph_rendering,
    )

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
        "aat": aat,
        "materialization": {
            "status": validation.get("status"),
            "findings_count": len(validation.get("findings", [])),
        },
        "parser_ir": {
            "schema_hash": parser_ir.get("schema_hash"),
            "nodes": len(parser_ir.get("nodes", [])),
            "paragraph_count": parser_ir_paragraph_count,
            "source_note_count": len(source_note_texts),
        },
        "paragraph_rendering": paragraph_rendering,
        "generated_tei": {
            "body_p_count": generated_body_p,
            "body_p_count_raw": generated["body_p_count_raw"],
            "body_missing_gap_p_count": generated["body_missing_gap_p_count"],
            "body_note_count": generated["body_note_count"],
            "back_note_count": generated["back_note_count"],
            "back_source_note_count": generated["back_source_note_count"],
            "body_tag_counts": generated["body_tag_counts"],
            "document_tag_counts": generated["document_tag_counts"],
        },
        "tei_eaj": {
            "body_p_count": tei_eaj_body_p,
            "body_p_count_raw": tei_eaj["body_p_count_raw"],
            "body_missing_gap_p_count": tei_eaj["body_missing_gap_p_count"],
            "body_note_count": tei_eaj["body_note_count"],
            "back_note_count": tei_eaj["back_note_count"],
            "back_source_note_count": tei_eaj["back_source_note_count"],
            "body_tag_counts": tei_eaj["body_tag_counts"],
            "document_tag_counts": tei_eaj["document_tag_counts"],
            "structure_profile": tei_eaj["structure_profile"],
            "structure_profiles": tei_eaj["structure_profiles"],
            "workset_p_count": row.get("tei", {}).get("tei_eaj_p_count"),
            "workset_note_count": row.get("tei", {}).get("tei_eaj_note_count"),
        },
        "deltas": {
            "body_p_count": generated_body_p - tei_eaj_body_p,
            "generated_vs_parser_ir_body_p_count": nullable_delta(
                generated_body_p, parser_ir_paragraph_count
            ),
            "parser_ir_paragraph_count_vs_aat_paragraph_blocks": nullable_delta(
                parser_ir_paragraph_count, aat["paragraph_blocks"]
            ),
            "aat_paragraph_blocks_vs_tei_eaj_body_p_count": nullable_delta(
                aat["paragraph_blocks"], tei_eaj_body_p
            ),
            "body_note_count": generated["body_note_count"] - tei_eaj["body_note_count"],
            "back_source_note_count": generated["back_source_note_count"]
            - tei_eaj["back_source_note_count"],
        },
        "text": text,
        "classification": {
            "paragraph_delta_bucket": paragraph_delta_bucket(
                generated_body_p, tei_eaj_body_p
            ),
            "paragraph_origin_bucket": paragraph_origin,
            "source_note_body_excluded": bool(non_empty_source_notes)
            and not source_note_in_body,
            "source_note_back_present": bool(non_empty_source_notes)
            and source_note_in_back,
        },
    }


def failed_task_row(
    row: dict[str, Any],
    candidate: dict[str, Any],
    tei_file: str,
    error: subprocess.CalledProcessError,
) -> dict[str, Any]:
    return {
        "work_id": row.get("tei", {}).get("work_id"),
        "tei_eaj_file": tei_file,
        "selected_aat": {
            "label": candidate.get("label"),
            "path": candidate.get("path"),
            "adapter": candidate.get("aat", {}).get("adapter"),
            "adapter_version": candidate.get("aat", {}).get("adapter_version"),
        },
        "aat": aat_summary(candidate),
        "materialization": {
            "status": "failed",
            "returncode": error.returncode,
        },
        "classification": {"paragraph_delta_bucket": "materialization_failed"},
    }


def materialize_row(
    args: argparse.Namespace,
    row: dict[str, Any],
    candidate: dict[str, Any],
    tei_eaj_path: pathlib.Path,
    row_dir: pathlib.Path,
) -> dict[str, Any]:
    task = {"candidate": candidate, "row_dir": row_dir}
    outputs = convert_task_outputs(args, task)
    materialize_single_publication(
        args,
        outputs["parser_ir_path"],
        outputs["publication_dir"],
    )
    return build_materialized_row(
        row,
        candidate,
        tei_eaj_path,
        outputs["parser_ir_path"],
        outputs["publication_dir"],
    )


def materialize_task(args: argparse.Namespace, task: dict[str, Any]) -> dict[str, Any]:
    try:
        result = materialize_row(
            args,
            task["row"],
            task["candidate"],
            task["tei_eaj_path"],
            task["row_dir"],
        )
    except subprocess.CalledProcessError as error:
        result = failed_task_row(
            task["row"],
            task["candidate"],
            task["tei_file"],
            error,
        )
    result["_task_index"] = task["task_index"]
    return result


def convert_task(args: argparse.Namespace, task: dict[str, Any]) -> dict[str, Any]:
    try:
        outputs = convert_task_outputs(args, task)
        result = {**task, **outputs, "conversion_failed": False}
    except subprocess.CalledProcessError as error:
        row = failed_task_row(
            task["row"],
            task["candidate"],
            task["tei_file"],
            error,
        )
        row["_task_index"] = task["task_index"]
        result = {**task, "conversion_failed": True, "failed_row": row}
    return result


def run_batch_tasks(
    args: argparse.Namespace,
    tasks: list[dict[str, Any]],
    jobs: int,
) -> list[dict[str, Any]]:
    if jobs == 1:
        converted = [convert_task(args, task) for task in tasks]
    else:
        with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as executor:
            futures = [executor.submit(convert_task, args, task) for task in tasks]
            converted = [
                future.result() for future in concurrent.futures.as_completed(futures)
            ]

    rows = [task["failed_row"] for task in converted if task.get("conversion_failed")]
    materializable = [task for task in converted if not task.get("conversion_failed")]
    if materializable:
        materialize_batch(args, materializable)
        for task in materializable:
            row = build_materialized_row(
                task["row"],
                task["candidate"],
                task["tei_eaj_path"],
                task["parser_ir_path"],
                task["publication_dir"],
            )
            row["_task_index"] = task["task_index"]
            rows.append(row)
    return rows


def increment_bucket(
    buckets: dict[str, int],
    bucket: str,
) -> None:
    buckets[bucket] = buckets.get(bucket, 0) + 1


def increment_nested_bucket(
    buckets: dict[str, dict[str, int]],
    key: str,
    bucket: str,
) -> None:
    nested = buckets.setdefault(key, {})
    nested[bucket] = nested.get(bucket, 0) + 1


def increment_double_nested_bucket(
    buckets: dict[str, dict[str, dict[str, int]]],
    outer_key: str,
    inner_key: str,
    bucket: str,
) -> None:
    outer = buckets.setdefault(outer_key, {})
    inner = outer.setdefault(inner_key, {})
    inner[bucket] = inner.get(bucket, 0) + 1


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
            "## Paragraph Origin Buckets",
            "",
            "| bucket | rows |",
            "|---|---:|",
        ]
    )
    for bucket, count in sorted(summary["paragraph_origin_buckets"].items()):
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
            "## TEI-EAJ Structure Profile Buckets",
            "",
            "| profile | rows |",
            "|---|---:|",
        ]
    )
    for profile, count in sorted(summary["tei_eaj_structure_profile_buckets"].items()):
        lines.append(f"| {profile} | {count} |")

    lines.extend(
        [
            "",
            "## Paragraph Origin by TEI-EAJ Profile",
            "",
            "| profile | origin | rows |",
            "|---|---|---:|",
        ]
    )
    for profile, buckets in sorted(
        summary["paragraph_origin_by_tei_eaj_profile"].items()
    ):
        for bucket, count in sorted(buckets.items()):
            lines.append(f"| {profile} | {bucket} | {count} |")

    lines.extend(
        [
            "",
            "## Adapter Paragraph Delta Buckets",
            "",
            "| adapter | bucket | rows |",
            "|---|---|---:|",
        ]
    )
    for adapter, buckets in sorted(summary["adapter_paragraph_delta_buckets"].items()):
        for bucket, count in sorted(buckets.items()):
            lines.append(f"| {adapter} | {bucket} | {count} |")

    lines.extend(
        [
            "",
            "## Adapter Paragraph Origin Buckets",
            "",
            "| adapter | bucket | rows |",
            "|---|---|---:|",
        ]
    )
    for adapter, buckets in sorted(summary["adapter_paragraph_origin_buckets"].items()):
        for bucket, count in sorted(buckets.items()):
            lines.append(f"| {adapter} | {bucket} | {count} |")

    lines.extend(
        [
            "",
            "## Adapter Body Text Match Buckets",
            "",
            "| adapter | bucket | rows |",
            "|---|---|---:|",
        ]
    )
    for adapter, buckets in sorted(summary["adapter_body_text_match_buckets"].items()):
        for bucket, count in sorted(buckets.items()):
            lines.append(f"| {adapter} | {bucket} | {count} |")

    lines.extend(
        [
            "",
            "## Rows",
            "",
            "| work_id | TEI-EAJ file | adapter | AAT p | parser-IR p | generated body p | TEI-EAJ body p | delta | bucket | origin | base text | best text match | source note |",
            "|---|---|---|---:|---:|---:|---:|---:|---|---|---|---|---|",
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
            "| {work_id} | `{tei_eaj_file}` | {adapter} | {aat_p} | {parser_ir_p} | {generated} | {tei_eaj} | {delta} | {bucket} | {origin} | {body_text} | {body_text_match} | {source_note} |".format(
                work_id=row.get("work_id"),
                tei_eaj_file=row.get("tei_eaj_file"),
                adapter=row["selected_aat"].get("adapter"),
                aat_p=row.get("aat", {}).get("paragraph_blocks"),
                parser_ir_p=row["parser_ir"]["paragraph_count"],
                generated=row["generated_tei"]["body_p_count"],
                tei_eaj=row["tei_eaj"]["body_p_count"],
                delta=row["deltas"]["body_p_count"],
                bucket=row["classification"]["paragraph_delta_bucket"],
                origin=row["classification"].get("paragraph_origin_bucket"),
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
    tasks = []
    tei_eaj_rows_attempted = 0
    jobs = max(1, args.jobs)

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
        candidates = materializable_candidates(row, preference)
        if not candidates:
            skipped.append(
                {"tei_eaj_file": tei_file, "reason": "no_materializable_aat"}
            )
            continue
        if args.max_rows and tei_eaj_rows_attempted >= args.max_rows:
            continue

        tei_eaj_rows_attempted += 1
        if args.candidate_mode == "selected":
            candidates = candidates[:1]

        work_id = str(row.get("tei", {}).get("work_id") or "unknown")
        for candidate_index, candidate in enumerate(candidates, start=1):
            adapter = candidate.get("aat", {}).get("adapter") or "unknown"
            row_dir = (
                args.out_dir
                / "rows"
                / sanitize(
                    f"{tei_eaj_rows_attempted:04d}_{candidate_index:02d}_{adapter}_{work_id}_{tei_file}"
                )
            )
            row_dir.mkdir(parents=True, exist_ok=True)
            tasks.append(
                {
                    "task_index": len(tasks),
                    "row": row,
                    "candidate": candidate,
                    "tei_file": tei_file,
                    "tei_eaj_path": tei_eaj_path,
                    "row_dir": row_dir,
                }
            )

    if args.materialization_mode == "batch":
        rows = run_batch_tasks(args, tasks, jobs)
    elif jobs == 1:
        rows = [materialize_task(args, task) for task in tasks]
    else:
        with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as executor:
            futures = [executor.submit(materialize_task, args, task) for task in tasks]
            rows = [
                future.result() for future in concurrent.futures.as_completed(futures)
            ]
    rows.sort(key=lambda row: row["_task_index"])
    for row in rows:
        row.pop("_task_index", None)

    buckets: dict[str, int] = {}
    paragraph_origin_buckets: dict[str, int] = {}
    body_text_buckets: dict[str, int] = {}
    body_text_match_buckets: dict[str, int] = {}
    adapter_paragraph_delta_buckets: dict[str, dict[str, int]] = {}
    adapter_paragraph_origin_buckets: dict[str, dict[str, int]] = {}
    adapter_body_text_match_buckets: dict[str, dict[str, int]] = {}
    tei_eaj_structure_profile_buckets: dict[str, int] = {}
    paragraph_origin_by_tei_eaj_profile: dict[str, dict[str, int]] = {}
    adapter_paragraph_origin_by_tei_eaj_profile: dict[
        str, dict[str, dict[str, int]]
    ] = {}
    materialized = 0
    for row in rows:
        adapter = row.get("selected_aat", {}).get("adapter") or "unknown"
        bucket = row.get("classification", {}).get("paragraph_delta_bucket", "unknown")
        increment_bucket(buckets, bucket)
        increment_nested_bucket(adapter_paragraph_delta_buckets, adapter, bucket)
        origin_bucket = row.get("classification", {}).get(
            "paragraph_origin_bucket", "unknown"
        )
        increment_bucket(paragraph_origin_buckets, origin_bucket)
        increment_nested_bucket(adapter_paragraph_origin_buckets, adapter, origin_bucket)
        structure_profile = row.get("tei_eaj", {}).get(
            "structure_profile", "unknown"
        )
        increment_bucket(tei_eaj_structure_profile_buckets, structure_profile)
        increment_nested_bucket(
            paragraph_origin_by_tei_eaj_profile,
            structure_profile,
            origin_bucket,
        )
        increment_double_nested_bucket(
            adapter_paragraph_origin_by_tei_eaj_profile,
            adapter,
            structure_profile,
            origin_bucket,
        )
        relation = row.get("text", {}).get("body_base_text_relation", "unknown")
        increment_bucket(body_text_buckets, relation)
        match_bucket = row.get("text", {}).get("body_text_match_bucket", "unknown")
        increment_bucket(body_text_match_buckets, match_bucket)
        increment_nested_bucket(adapter_body_text_match_buckets, adapter, match_bucket)
        if row.get("materialization", {}).get("status") == "passed":
            materialized += 1

    summary = {
        "schema_version": "tei-eaj-generated-comparison-v1",
        "inputs": {
            "workset": logical_report_path(args.workset),
            "structural_summary": logical_report_path(args.structural_summary),
            "mapping": logical_report_path(args.mapping),
            "abc_root": logical_report_path(args.abc_root),
            "adapter_preference": list(preference.keys()),
            "candidate_mode": args.candidate_mode,
            "materialization_mode": args.materialization_mode,
            "jobs": jobs,
            "max_rows": args.max_rows,
            "tei_eaj_file_filter": args.tei_eaj_file,
        },
        "totals": {
            "rows_attempted": len(rows),
            "tei_eaj_rows_attempted": tei_eaj_rows_attempted,
            "materialization_succeeded": materialized,
            "materialization_failed": len(rows) - materialized,
            "rows_skipped": len(skipped),
        },
        "paragraph_delta_buckets": buckets,
        "paragraph_origin_buckets": paragraph_origin_buckets,
        "body_text_relation_buckets": body_text_buckets,
        "body_text_match_buckets": body_text_match_buckets,
        "tei_eaj_structure_profile_buckets": tei_eaj_structure_profile_buckets,
        "paragraph_origin_by_tei_eaj_profile": paragraph_origin_by_tei_eaj_profile,
        "adapter_paragraph_origin_by_tei_eaj_profile": (
            adapter_paragraph_origin_by_tei_eaj_profile
        ),
        "adapter_paragraph_delta_buckets": adapter_paragraph_delta_buckets,
        "adapter_paragraph_origin_buckets": adapter_paragraph_origin_buckets,
        "adapter_body_text_match_buckets": adapter_body_text_match_buckets,
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
