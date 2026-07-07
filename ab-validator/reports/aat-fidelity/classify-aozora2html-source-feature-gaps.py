#!/usr/bin/env python
"""Classify aozora2html source-feature residuals into actionable candidate worksets."""

from __future__ import annotations

import argparse
import io
import json
import re
import tomllib
from collections import Counter
from pathlib import Path
from typing import Any
from zipfile import ZipFile

FAMILIES = ("warigaki", "kunten")
RESIDUAL_BUCKET = "source_feature_without_aat_observation"
REQUIRED_SUMMARY_KEYS = (
    "run_dir",
    "residual_summary",
    "families",
    "marker_class_counts",
    "context_hint_counts",
    "works",
    "worksets",
    "verdict_inputs",
)
BODY_HINT = "body_candidate"
NON_BODY_HINTS = ("notation_example", "base_text_note", "publication_or_editor_note")
KUNTEN_FEATURES = ("kaeriten", "okurigana")
CONTEXT_HINT_PATTERNS = {
    "notation_example": ("（例）", "：返り点"),
    "base_text_note": ("題は底本では", "題名の次行", "底本では"),
    "publication_or_editor_note": ("初出", "ファイル末", "入力"),
}
WARIAKI_MARKERS = {
    "warigaki.koko_start_end": ("［＃ここから割り注］", "［＃ここで割り注終わり］"),
    "warigaki.compact_start_end": ("［＃割り注］", "［＃割り注終わり］"),
    "warigaki.legacy_warigaki": ("［＃割書］", "［＃割書終わり］"),
}
NAMED_KAERITEN_RE = re.compile(r"［＃返り点[^］]*］")
NAMED_OKURIGANA_RE = re.compile(r"［＃訓点送り仮名「[^」]+」］")


def read_json(path: Path) -> Any:
    return json.loads(path.read_text())


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n")


def load_feature_patterns(path: Path) -> dict[str, re.Pattern[str]]:
    payload = tomllib.loads(path.read_text())
    features = payload.get("features")
    if not isinstance(features, dict):
        raise SystemExit("feature-patterns.toml lacks [features]")
    compiled: dict[str, re.Pattern[str]] = {}
    for key, value in features.items():
        if not isinstance(value, dict) or not isinstance(value.get("pattern"), str):
            continue
        compiled[str(key)] = re.compile(str(value["pattern"]))
    return compiled


def load_index(run_dir: Path) -> tuple[Path, dict[str, dict[str, Any]]]:
    index = read_json(run_dir / "index.json")
    if not isinstance(index, dict):
        raise SystemExit("index.json is not an object")
    corpus_root = index.get("corpus_root")
    works = index.get("works")
    if not isinstance(corpus_root, str) or not corpus_root:
        raise SystemExit("index.json lacks corpus_root")
    if not isinstance(works, list):
        raise SystemExit("index.json lacks works array")
    work_index: dict[str, dict[str, Any]] = {}
    for work in works:
        if isinstance(work, dict) and isinstance(work.get("id"), str):
            work_index[str(work["id"])] = work
    return Path(corpus_root), work_index


def decode_source_bytes(raw: bytes) -> tuple[str, str]:
    for encoding, label in (
        ("utf-8-sig", "utf-8-bom"),
        ("utf-8", "utf-8"),
        ("cp932", "windows-31j"),
    ):
        try:
            return raw.decode(encoding), label
        except UnicodeDecodeError:
            continue
    return raw.decode("cp932", errors="replace"), "windows-31j-lossy"


def read_source_text(corpus_root: Path, txt_path: str) -> tuple[str, str]:
    if "::" in txt_path:
        archive_rel, member = txt_path.split("::", 1)
        raw = (corpus_root / archive_rel).read_bytes()
        with ZipFile(io.BytesIO(raw)) as zf:
            source_raw = zf.read(member)
        return decode_source_bytes(source_raw)
    return decode_source_bytes((corpus_root / txt_path).read_bytes())


def context_hints_for_line(line: str) -> list[str]:
    hints = [
        hint
        for hint, needles in CONTEXT_HINT_PATTERNS.items()
        if any(needle in line for needle in needles)
    ]
    if not hints:
        hints.append(BODY_HINT)
    return hints


def short_markers_for_regex(pattern: re.Pattern[str], line: str) -> list[str]:
    return sorted({match.group(0) for match in pattern.finditer(line)})


def classify_warigaki_line(line: str) -> dict[str, list[str]]:
    classes: dict[str, list[str]] = {}
    for marker_class, needles in WARIAKI_MARKERS.items():
        markers = [needle for needle in needles if needle in line]
        if markers:
            classes[marker_class] = sorted(set(markers))
    if any(name.startswith("warigaki.") for name in classes) and "［＃改行］" in line:
        classes["warigaki.with_source_line_break"] = ["［＃改行］"]
    return classes


def classify_kunten_line(
    line: str,
    *,
    kaeriten_pattern: re.Pattern[str],
    okurigana_pattern: re.Pattern[str],
) -> dict[str, list[str]]:
    classes: dict[str, list[str]] = {}
    named_kaeriten = short_markers_for_regex(NAMED_KAERITEN_RE, line)
    if named_kaeriten:
        classes["kunten.kaeriten.named"] = named_kaeriten
    compact_kaeriten = [
        marker
        for marker in short_markers_for_regex(kaeriten_pattern, line)
        if not marker.startswith("［＃返り点")
    ]
    if compact_kaeriten:
        classes["kunten.kaeriten.compact"] = compact_kaeriten

    named_okurigana = short_markers_for_regex(NAMED_OKURIGANA_RE, line)
    if named_okurigana:
        classes["kunten.okurigana.named"] = named_okurigana
    parenthesized = [
        marker
        for marker in short_markers_for_regex(okurigana_pattern, line)
        if marker.startswith("［＃（") and marker.endswith("）］")
    ]
    if parenthesized:
        classes["kunten.okurigana.parenthesized"] = parenthesized
    return classes


def classify_line(
    *,
    family: str,
    line: str,
    kaeriten_pattern: re.Pattern[str],
    okurigana_pattern: re.Pattern[str],
) -> tuple[list[str], list[str]]:
    classes = (
        classify_warigaki_line(line)
        if family == "warigaki"
        else classify_kunten_line(
            line,
            kaeriten_pattern=kaeriten_pattern,
            okurigana_pattern=okurigana_pattern,
        )
    )
    if not classes:
        return ["unknown"], []
    marker_classes = sorted(classes.keys())
    markers: list[str] = []
    for key in marker_classes:
        markers.extend(classes[key])
    return marker_classes, sorted(set(markers))


def feature_line_numbers(family: str, work: dict[str, Any]) -> list[tuple[int, str]]:
    feature_lines = work.get("feature_lines")
    if not isinstance(feature_lines, dict):
        return []
    ordered: list[tuple[int, str]] = []
    features = ("warigaki",) if family == "warigaki" else KUNTEN_FEATURES
    seen: set[tuple[int, str]] = set()
    for feature in features:
        values = feature_lines.get(feature)
        if not isinstance(values, list):
            continue
        for raw_line_number in values:
            if not isinstance(raw_line_number, int):
                continue
            item = (raw_line_number, feature)
            if item in seen:
                continue
            seen.add(item)
            ordered.append(item)
    return sorted(ordered)


def validate_workset_array(path: Path) -> None:
    payload = read_json(path)
    if not isinstance(payload, list) or any(not isinstance(item, str) for item in payload):
        raise SystemExit(f"{path} is not a flat JSON array of strings")
    if payload != sorted(set(payload)):
        raise SystemExit(f"{path} is not a sorted unique JSON array of strings")


def format_counter(counter: Counter[str]) -> str:
    if not counter:
        return ""
    return ", ".join(f"{key}:{counter[key]}" for key in sorted(counter))


def markdown_table(rows: list[list[str]]) -> list[str]:
    if not rows:
        return ["_No rows._", ""]
    out = ["| " + " | ".join(rows[0]) + " |", "|" + "|".join(["---"] * len(rows[0])) + "|"]
    for row in rows[1:]:
        out.append("| " + " | ".join(row) + " |")
    out.append("")
    return out


def make_argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument("--run-dir", required=True, type=Path)
    parser.add_argument("--residual-summary", required=True, type=Path)
    parser.add_argument("--out-md", required=True, type=Path)
    parser.add_argument("--summary-json", required=True, type=Path)
    parser.add_argument("--worksets-dir", required=True, type=Path)
    return parser


def main() -> int:
    args = make_argument_parser().parse_args()
    run_dir = args.run_dir.resolve()
    residual_summary_path = args.residual_summary.resolve()
    feature_patterns = load_feature_patterns(
        Path(__file__).resolve().parents[2] / "data" / "feature-patterns.toml"
    )
    kaeriten_pattern = feature_patterns["kaeriten"]
    okurigana_pattern = feature_patterns["okurigana"]

    corpus_root, work_index = load_index(run_dir)
    residual_summary = read_json(residual_summary_path)
    if not isinstance(residual_summary, dict):
        raise SystemExit("residual summary is not an object")

    residual_worksets = residual_summary.get("worksets")
    if not isinstance(residual_worksets, dict):
        raise SystemExit("residual summary lacks worksets")

    family_work_ids: dict[str, list[str]] = {}
    for family in FAMILIES:
        key = f"{family}.{RESIDUAL_BUCKET}"
        workset_path = residual_worksets.get(key)
        if not isinstance(workset_path, str):
            raise SystemExit(f"residual summary lacks {key} workset")
        work_ids = read_json(Path(workset_path))
        if not isinstance(work_ids, list) or any(not isinstance(item, str) for item in work_ids):
            raise SystemExit(f"{workset_path} is not a flat JSON array of strings")
        family_work_ids[family] = sorted(set(work_ids))

    works_summary: dict[str, dict[str, Any]] = {}
    marker_class_counts: Counter[str] = Counter()
    context_hint_counts: Counter[str] = Counter()
    family_marker_counts: dict[str, Counter[str]] = {family: Counter() for family in FAMILIES}
    family_context_counts: dict[str, Counter[str]] = {family: Counter() for family in FAMILIES}
    family_body_candidates: dict[str, set[str]] = {family: set() for family in FAMILIES}

    warigaki_adapter_obligation: set[str] = set()
    kunten_adapter_obligation: set[str] = set()
    source_index_only: set[str] = set()
    unknown_union: set[str] = set()

    for family in FAMILIES:
        for work_id in family_work_ids[family]:
            work = work_index.get(work_id)
            if not isinstance(work, dict):
                raise SystemExit(f"index.json missing work {work_id}")
            if work_id in works_summary:
                work_summary = works_summary[work_id]
            else:
                work_summary = {
                    "family_membership": [],
                    "txt_path": str(work.get("txt_path", "")),
                    "html_path": str(work.get("html_path", "")),
                    "marker_classes": [],
                    "context_hints": [],
                    "source_encoding": "",
                    "line_records": [],
                    "errors": [],
                }
                works_summary[work_id] = work_summary
            work_summary["family_membership"].append(family)

            try:
                source_text, encoding_label = read_source_text(
                    corpus_root, str(work.get("txt_path", ""))
                )
                work_summary["source_encoding"] = encoding_label
                lines = source_text.splitlines()
            except Exception as exc:
                work_summary["errors"].append(f"source_read_error:{exc}")
                lines = []

            line_records: list[dict[str, Any]] = []
            work_marker_classes: set[str] = set(work_summary["marker_classes"])
            work_context_hints: set[str] = set(work_summary["context_hints"])
            has_unreadable_line = False
            family_has_body = False

            for line_number, feature in feature_line_numbers(family, work):
                if line_number < 1 or line_number > len(lines):
                    marker_classes = ["unknown"]
                    context_hints = []
                    markers: list[str] = []
                    has_unreadable_line = True
                    work_summary["errors"].append(f"unreadable_source_line:{line_number}")
                else:
                    source_line = lines[line_number - 1]
                    marker_classes, markers = classify_line(
                        family=family,
                        line=source_line,
                        kaeriten_pattern=kaeriten_pattern,
                        okurigana_pattern=okurigana_pattern,
                    )
                    context_hints = context_hints_for_line(source_line)
                line_records.append(
                    {
                        "line_number": line_number,
                        "feature": feature,
                        "marker_classes": marker_classes,
                        "markers": markers,
                        "context_hints": context_hints,
                    }
                )
                for marker_class in marker_classes:
                    work_marker_classes.add(marker_class)
                    marker_class_counts[marker_class] += 1
                    family_marker_counts[family][marker_class] += 1
                for context_hint in context_hints:
                    work_context_hints.add(context_hint)
                    context_hint_counts[context_hint] += 1
                    family_context_counts[family][context_hint] += 1
                    if context_hint == BODY_HINT:
                        family_has_body = True
            if family_has_body:
                family_body_candidates[family].add(work_id)
            if has_unreadable_line:
                work_marker_classes.add("unknown")

            work_summary["line_records"].extend(line_records)
            work_summary["marker_classes"] = sorted(work_marker_classes)
            work_summary["context_hints"] = sorted(work_context_hints)

    for work_id, work_summary in works_summary.items():
        work_marker_classes = set(work_summary["marker_classes"])
        work_context_hints = set(work_summary["context_hints"])
        has_unknown = "unknown" in work_marker_classes
        has_body = BODY_HINT in work_context_hints

        if has_unknown:
            unknown_union.add(work_id)
            continue
        if has_body:
            if work_id in family_body_candidates["warigaki"]:
                warigaki_adapter_obligation.add(work_id)
            if work_id in family_body_candidates["kunten"]:
                kunten_adapter_obligation.add(work_id)
            continue
        source_index_only.add(work_id)

    worksets = {
        "warigaki.adapter_obligation_candidates": str(
            args.worksets_dir / "warigaki-adapter-obligation-candidates.json"
        ),
        "kunten.adapter_obligation_candidates": str(
            args.worksets_dir / "kunten-adapter-obligation-candidates.json"
        ),
        "adapter_obligation_union": str(
            args.worksets_dir / "source-feature-gap-adapter-obligation-union.json"
        ),
        "unknown_union": str(args.worksets_dir / "source-feature-gap-unknown-union.json"),
        "source_index_only_candidates": str(
            args.worksets_dir / "source-feature-gap-source-index-only-candidates.json"
        ),
    }

    write_json(
        Path(worksets["warigaki.adapter_obligation_candidates"]),
        sorted(warigaki_adapter_obligation),
    )
    write_json(
        Path(worksets["kunten.adapter_obligation_candidates"]), sorted(kunten_adapter_obligation)
    )
    write_json(
        Path(worksets["adapter_obligation_union"]),
        sorted(warigaki_adapter_obligation | kunten_adapter_obligation),
    )
    write_json(Path(worksets["unknown_union"]), sorted(unknown_union))
    write_json(Path(worksets["source_index_only_candidates"]), sorted(source_index_only))

    for path in worksets.values():
        validate_workset_array(Path(path))

    families = {
        family: {
            "source_feature_without_aat_observation": len(family_work_ids[family]),
            "marker_class_counts": dict(sorted(family_marker_counts[family].items())),
            "context_hint_counts": dict(sorted(family_context_counts[family].items())),
            "adapter_obligation_candidates": len(
                warigaki_adapter_obligation if family == "warigaki" else kunten_adapter_obligation
            ),
        }
        for family in FAMILIES
    }

    verdict_inputs = {
        "source_feature_without_aat_observation": {
            family: len(family_work_ids[family]) for family in FAMILIES
        },
        "adapter_obligation_candidates": {
            "warigaki": len(warigaki_adapter_obligation),
            "kunten": len(kunten_adapter_obligation),
            "union": len(warigaki_adapter_obligation | kunten_adapter_obligation),
        },
        "source_index_only_candidates": len(source_index_only),
        "unknown": len(unknown_union),
    }

    summary = {
        "run_dir": str(run_dir),
        "residual_summary": str(residual_summary_path),
        "families": families,
        "marker_class_counts": dict(sorted(marker_class_counts.items())),
        "context_hint_counts": dict(sorted(context_hint_counts.items())),
        "works": dict(sorted(works_summary.items())),
        "worksets": worksets,
        "verdict_inputs": verdict_inputs,
    }
    missing_keys = [key for key in REQUIRED_SUMMARY_KEYS if key not in summary]
    if missing_keys:
        raise SystemExit(f"summary missing keys: {', '.join(missing_keys)}")

    write_json(args.summary_json, summary)

    md_lines = [
        "# Aozora2html Source-Feature Gap Classification",
        "",
        f"- run_dir: `{run_dir}`",
        f"- residual_summary: `{residual_summary_path}`",
        "",
        "## Marker Class Counts",
        "",
    ]
    md_lines.extend(
        markdown_table(
            [["Marker class", "Count"]]
            + [
                [key, str(summary["marker_class_counts"][key])]
                for key in sorted(summary["marker_class_counts"])
            ]
        )
    )
    md_lines.extend(["## Context Hint Counts", ""])
    md_lines.extend(
        markdown_table(
            [["Context hint", "Count"]]
            + [
                [key, str(summary["context_hint_counts"][key])]
                for key in sorted(summary["context_hint_counts"])
            ]
        )
    )
    md_lines.extend(["## Candidate Worksets", ""])
    md_lines.extend(
        markdown_table(
            [["Workset", "Works", "Path"]]
            + [
                [
                    key,
                    str(len(read_json(Path(path)))),
                    f"`{path}`",
                ]
                for key, path in worksets.items()
            ]
        )
    )
    md_lines.extend(["## Works", ""])
    md_lines.extend(
        markdown_table(
            [["Work ID", "Families", "Marker classes", "Context hints", "Line records", "Errors"]]
            + [
                [
                    work_id,
                    ", ".join(works_summary[work_id]["family_membership"]),
                    ", ".join(works_summary[work_id]["marker_classes"]),
                    ", ".join(works_summary[work_id]["context_hints"]),
                    str(len(works_summary[work_id]["line_records"])),
                    ", ".join(works_summary[work_id]["errors"]),
                ]
                for work_id in sorted(works_summary)
            ]
        )
    )
    md_lines.extend(["## Family Summary", ""])
    family_rows = [
        ["Family", "Residual works", "Marker classes", "Context hints", "Adapter candidates"]
    ]
    for family in FAMILIES:
        family_summary = families[family]
        family_rows.append(
            [
                family,
                str(family_summary["source_feature_without_aat_observation"]),
                format_counter(Counter(family_summary["marker_class_counts"])),
                format_counter(Counter(family_summary["context_hint_counts"])),
                str(family_summary["adapter_obligation_candidates"]),
            ]
        )
    md_lines.extend(markdown_table(family_rows))
    args.out_md.parent.mkdir(parents=True, exist_ok=True)
    args.out_md.write_text("\n".join(md_lines))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
