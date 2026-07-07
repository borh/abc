#!/usr/bin/env python
from __future__ import annotations

import argparse
import pathlib
import sys
import tomllib
from typing import Any

_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(_REPO_ROOT))

from reports.lib.hashing import file_sha256 as sha256_file
from reports.lib.io import read_json, write_json


SCHEMA_VERSION = "aozora-source-reference-reconciliation-v1"
COMPLETE_VERDICT = "SOURCE_REFERENCE_RECONCILIATION_COMPLETE"
REVIEW_VERDICT = "SOURCE_REFERENCE_RECONCILIATION_REVIEW_REQUIRED"

OFFICIAL_REFERENCE_FRAGMENT = "aozorabunko/rules/"

P4SUTA_FEATURE_TO_SOURCE_ROWS: dict[str, list[str]] = {
    "accent": ["accent.dotted_letter"],
    "annotation": [
        "annotation.bouki",
        "annotation.chuuki",
        "annotation.layout_note",
        "glyph.variant_note",
        "source.note_label",
        "source.page_reference",
        "source.reviewed_residual_command",
    ],
    "bouten": ["decoration.boten"],
    "break": ["break.line_explicit", "break.page_line"],
    "container": [
        "indentation.basic",
        "indentation.burasage",
        "indentation.chitsuki",
        "indentation.jisage_block",
        "indentation.jisage_oneline",
        "indentation.jizume",
        "layout.center_page",
        "structure.quote_block",
    ],
    "emphasis": [
        "emphasis.basic",
        "decoration.bold_italic",
        "decoration.bousen",
        "decoration.font_size",
        "decoration.typeface",
    ],
    "font_size": ["decoration.font_size"],
    "fraction": ["fraction.basic"],
    "gaiji": [
        "gaiji.marker",
        "gaiji.jis_code",
        "gaiji.un_embed",
        "gaiji.unicode_codepoint",
        "gaiji_ruby.inline_base",
    ],
    "heading": ["heading.basic", "heading.dogyo", "heading.mado"],
    "horizontal": ["layout.yokogumi"],
    "kaeriten": ["kunten.kaeriten"],
    "keigakomi": ["decoration.keigakomi"],
    "kunten": ["kunten.kaeriten", "kunten.okurigana"],
    "layout": [
        "indentation.basic",
        "indentation.burasage",
        "indentation.chitsuki",
        "indentation.jisage_block",
        "indentation.jisage_oneline",
        "indentation.jizume",
        "layout.center_page",
    ],
    "ruby": ["ruby.basic", "ruby.placement_directional", "gaiji_ruby.inline_base"],
    "sashie": ["figure.image_caption", "figure.image_inline"],
    "structural-marker": ["reference.frontref", "source.note_label"],
    "tables_columns": ["layout.multicolumn", "structure.table"],
    "tate_chu_yoko": ["layout.tcy"],
    "tcy": ["layout.tcy"],
    "warichu": ["warichu.basic"],
}

P4SUTA_COMPARISON_ONLY_FEATURES = {
    "angle_quote",
    "composite",
    "plain",
    "recovery",
}


def load_json(path: pathlib.Path) -> dict[str, Any]:
    value = read_json(path)
    if not isinstance(value, dict):
        raise SystemExit(f"{path}: expected JSON object")
    return value


def write_text(path: pathlib.Path, value: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(value, encoding="utf-8")


def load_syntax_rows(path: pathlib.Path) -> list[dict[str, Any]]:
    data = tomllib.loads(path.read_text(encoding="utf-8"))
    rows = data.get("syntax", [])
    if not isinstance(rows, list):
        raise SystemExit(f"{path}: expected [[syntax]] rows")
    return [row for row in rows if isinstance(row, dict)]


def is_official_row(row: dict[str, Any]) -> bool:
    sources = row.get("reference_sources", [])
    return any(OFFICIAL_REFERENCE_FRAGMENT in str(source) for source in sources)


def source_inventory_row(row: dict[str, Any]) -> str | None:
    representability = row.get("representability", {})
    if not isinstance(representability, dict):
        return None
    value = representability.get("source_inventory_row")
    return str(value) if value is not None else None


def representability_status(row: dict[str, Any]) -> str | None:
    representability = row.get("representability", {})
    if not isinstance(representability, dict):
        return None
    value = representability.get("status")
    return str(value) if value is not None else None


def source_entry(source_rows: dict[str, Any], row_id: str | None) -> dict[str, Any] | None:
    if row_id is None:
        return None
    value = source_rows.get(row_id)
    return value if isinstance(value, dict) else None


def occurrence_count(source_rows: dict[str, Any], row_id: str | None) -> int:
    entry = source_entry(source_rows, row_id)
    if not entry:
        return 0
    value = entry.get("occurrences", 0)
    return int(value) if isinstance(value, int) else 0


def works_count(source_rows: dict[str, Any], row_id: str | None) -> int:
    entry = source_entry(source_rows, row_id)
    if not entry:
        return 0
    value = entry.get("works_with_marker", 0)
    return int(value) if isinstance(value, int) else 0


def sample_works(source_rows: dict[str, Any], row_id: str | None) -> list[str]:
    entry = source_entry(source_rows, row_id)
    if not entry:
        return []
    value = entry.get("sample_works", [])
    if not isinstance(value, list):
        return []
    return [str(item) for item in value[:5]]


def syntax_summary(row: dict[str, Any], source_rows: dict[str, Any]) -> dict[str, Any]:
    inventory_row = source_inventory_row(row)
    return {
        "syntax_id": str(row.get("id")),
        "source_inventory_row": inventory_row,
        "representability_status": representability_status(row),
        "reference_sources": [str(source) for source in row.get("reference_sources", [])],
        "source_patterns": [str(pattern) for pattern in row.get("source_patterns", [])],
        "works_with_marker": works_count(source_rows, inventory_row),
        "occurrences": occurrence_count(source_rows, inventory_row),
        "sample_works": sample_works(source_rows, inventory_row),
    }


def manual_reference_files(manual_root: pathlib.Path | None, syntax_rows: list[dict[str, Any]]) -> list[dict[str, Any]]:
    referenced: set[str] = set()
    for row in syntax_rows:
        for source in row.get("reference_sources", []):
            source_text = str(source)
            if OFFICIAL_REFERENCE_FRAGMENT in source_text:
                referenced.add(source_text.rsplit("/", 1)[-1])

    files = []
    for filename in sorted(referenced):
        entry: dict[str, Any] = {"filename": filename}
        if manual_root is not None:
            path = manual_root / filename
            entry["path"] = str(path)
            entry["exists"] = path.exists()
            if path.exists() and path.is_file():
                entry["hash"] = sha256_file(path)
        files.append(entry)
    return files


def p4suta_features(notation_summary: dict[str, Any]) -> dict[str, dict[str, Any]]:
    features: dict[str, dict[str, Any]] = {}
    for row in notation_summary.get("rows", []):
        if not isinstance(row, dict):
            continue
        feature = str(row.get("feature"))
        if feature == "None":
            continue
        entry = features.setdefault(
            feature,
            {
                "feature": feature,
                "vectors": set(),
                "levels": set(),
                "adapters": set(),
                "rows": 0,
                "fail": 0,
                "warning": 0,
                "pass": 0,
            },
        )
        entry["rows"] += 1
        entry["vectors"].add(str(row.get("vector")))
        entry["levels"].add(str(row.get("level")))
        entry["adapters"].add(str(row.get("adapter")))
        status = str(row.get("status"))
        if status in {"pass", "warning", "fail"}:
            entry[status] += 1

    normalized = {}
    for feature, entry in features.items():
        normalized[feature] = {
            **entry,
            "vectors": sorted(entry["vectors"]),
            "levels": sorted(entry["levels"]),
            "adapters": sorted(entry["adapters"]),
        }
    return normalized


def build_summary(
    syntax_coverage: pathlib.Path,
    source_summary_path: pathlib.Path,
    notation_summary_path: pathlib.Path,
    manual_root: pathlib.Path | None,
) -> dict[str, Any]:
    syntax_rows = load_syntax_rows(syntax_coverage)
    source_summary = load_json(source_summary_path)
    notation_summary = load_json(notation_summary_path)
    source_rows = source_summary.get("rows", {})
    if not isinstance(source_rows, dict):
        raise SystemExit(f"{source_summary_path}: expected top-level rows object")

    official_rows = [row for row in syntax_rows if is_official_row(row)]
    syntax_by_id = {str(row.get("id")): row for row in syntax_rows}
    claimed_inventory_rows = {
        row_id
        for row in syntax_rows
        if (row_id := source_inventory_row(row)) is not None
    }

    documented_observed = []
    documented_unobserved = []
    for row in official_rows:
        summary = syntax_summary(row, source_rows)
        if summary["occurrences"] > 0:
            documented_observed.append(summary)
        else:
            documented_unobserved.append(summary)

    observed_without_syntax_row = []
    for row_id, value in sorted(source_rows.items()):
        if not isinstance(value, dict):
            continue
        occurrences = int(value.get("occurrences", 0)) if isinstance(value.get("occurrences", 0), int) else 0
        if occurrences > 0 and row_id not in claimed_inventory_rows:
            observed_without_syntax_row.append(
                {
                    "source_inventory_row": row_id,
                    "works_with_marker": works_count(source_rows, row_id),
                    "occurrences": occurrences,
                    "sample_works": sample_works(source_rows, row_id),
                }
            )

    feature_entries = p4suta_features(notation_summary)
    p4suta_feature_mapped = []
    p4suta_feature_unmapped = []
    p4suta_feature_unobserved = []
    p4suta_feature_comparison_only = []
    for feature, feature_entry in sorted(feature_entries.items()):
        if feature in P4SUTA_COMPARISON_ONLY_FEATURES:
            p4suta_feature_comparison_only.append(feature_entry)
            continue

        configured_rows = P4SUTA_FEATURE_TO_SOURCE_ROWS.get(feature, [])
        existing_rows = [row_id for row_id in configured_rows if row_id in claimed_inventory_rows or row_id in syntax_by_id]
        if not existing_rows:
            p4suta_feature_unmapped.append(feature_entry)
            continue

        source_inventory_rows = sorted(
            {
                source_inventory_row(syntax_by_id[row_id])
                for row_id in existing_rows
                if row_id in syntax_by_id and source_inventory_row(syntax_by_id[row_id]) is not None
            }
        )
        occurrences = sum(occurrence_count(source_rows, row_id) for row_id in source_inventory_rows)
        mapped = {
            **feature_entry,
            "syntax_rows": existing_rows,
            "source_inventory_rows": source_inventory_rows,
            "occurrences": occurrences,
        }
        p4suta_feature_mapped.append(mapped)
        if occurrences == 0:
            p4suta_feature_unobserved.append(mapped)

    verdict = (
        COMPLETE_VERDICT
        if not observed_without_syntax_row and not p4suta_feature_unmapped
        else REVIEW_VERDICT
    )

    return {
        "schema_version": SCHEMA_VERSION,
        "verdict": verdict,
        "inputs": {
            "syntax_coverage": str(syntax_coverage),
            "source_summary": str(source_summary_path),
            "notation_summary": str(notation_summary_path),
            "manual_root": str(manual_root) if manual_root is not None else None,
        },
        "totals": {
            "syntax_rows": len(syntax_rows),
            "official_syntax_rows": len(official_rows),
            "source_inventory_rows": len(source_rows),
            "documented_observed": len(documented_observed),
            "documented_unobserved": len(documented_unobserved),
            "observed_without_syntax_row": len(observed_without_syntax_row),
            "p4suta_features": len(feature_entries),
            "p4suta_feature_mapped": len(p4suta_feature_mapped),
            "p4suta_feature_unmapped": len(p4suta_feature_unmapped),
            "p4suta_feature_unobserved": len(p4suta_feature_unobserved),
            "p4suta_feature_comparison_only": len(p4suta_feature_comparison_only),
        },
        "manual_reference_files": manual_reference_files(manual_root, official_rows),
        "documented_observed": documented_observed,
        "documented_unobserved": documented_unobserved,
        "observed_without_syntax_row": observed_without_syntax_row,
        "p4suta_feature_mapped": p4suta_feature_mapped,
        "p4suta_feature_unmapped": p4suta_feature_unmapped,
        "p4suta_feature_unobserved": p4suta_feature_unobserved,
        "p4suta_feature_comparison_only": p4suta_feature_comparison_only,
    }


def table_rows(rows: list[dict[str, Any]], columns: list[tuple[str, str]], limit: int = 30) -> list[str]:
    out = [
        "| " + " | ".join(label for label, _key in columns) + " |",
        "| " + " | ".join("---" for _label, _key in columns) + " |",
    ]
    for row in rows[:limit]:
        values = []
        for _label, key in columns:
            value = row.get(key, "")
            if isinstance(value, list):
                value = ", ".join(str(item) for item in value)
            values.append(str(value).replace("\n", " "))
        out.append("| " + " | ".join(values) + " |")
    if len(rows) > limit:
        out.append(f"| ... | {len(rows) - limit} more rows omitted | |")
    return out


def render_markdown(summary: dict[str, Any]) -> str:
    totals = summary["totals"]
    lines = [
        "# Source Reference Reconciliation",
        "",
        f"Verdict: `{summary['verdict']}`",
        "",
        "## Totals",
        "",
        "| Metric | Count |",
        "| --- | ---: |",
    ]
    for key, value in totals.items():
        lines.append(f"| `{key}` | `{value}` |")

    lines.extend(
        [
            "",
            "## Review Items",
            "",
            "### Observed Source Rows Without Syntax Rows",
            "",
        ]
    )
    lines.extend(
        table_rows(
            summary["observed_without_syntax_row"],
            [
                ("source_inventory_row", "source_inventory_row"),
                ("works", "works_with_marker"),
                ("occurrences", "occurrences"),
                ("sample_works", "sample_works"),
            ],
        )
    )

    lines.extend(["", "### P4suta Features Without Local Mapping", ""])
    lines.extend(
        table_rows(
            summary["p4suta_feature_unmapped"],
            [
                ("feature", "feature"),
                ("vectors", "vectors"),
                ("levels", "levels"),
                ("rows", "rows"),
            ],
        )
    )

    lines.extend(["", "### Official Rows Documented But Unobserved", ""])
    lines.extend(
        table_rows(
            summary["documented_unobserved"],
            [
                ("syntax_id", "syntax_id"),
                ("source_inventory_row", "source_inventory_row"),
                ("status", "representability_status"),
                ("references", "reference_sources"),
            ],
        )
    )

    lines.extend(["", "## Mapped P4suta Features", ""])
    lines.extend(
        table_rows(
            summary["p4suta_feature_mapped"],
            [
                ("feature", "feature"),
                ("syntax_rows", "syntax_rows"),
                ("source_inventory_rows", "source_inventory_rows"),
                ("occurrences", "occurrences"),
            ],
        )
    )

    lines.append("")
    return "\n".join(lines)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--syntax-coverage", type=pathlib.Path, required=True)
    parser.add_argument("--source-summary", type=pathlib.Path, required=True)
    parser.add_argument("--notation-summary", type=pathlib.Path, required=True)
    parser.add_argument("--manual-root", type=pathlib.Path)
    parser.add_argument("--summary-json", type=pathlib.Path, required=True)
    parser.add_argument("--report-md", type=pathlib.Path, required=True)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    summary = build_summary(
        syntax_coverage=args.syntax_coverage,
        source_summary_path=args.source_summary,
        notation_summary_path=args.notation_summary,
        manual_root=args.manual_root,
    )
    write_json(args.summary_json, summary)
    write_text(args.report_md, render_markdown(summary))


if __name__ == "__main__":
    main()
