#!/usr/bin/env python3
import argparse
import collections
import pathlib
import re
import sys
import xml.etree.ElementTree as ET


SKIP_BASE_TEXT_LOCALS = {"rt", "rp"}
REPORT_FEATURES = [
    "p",
    "note",
    "div",
    "head",
    "ruby",
    "rp",
    "persName",
    "rs",
    "placeName",
    "roleName",
    "said",
    "listPerson",
    "person",
    "revisionDesc",
    "editorialDecl",
    "sourceDesc",
    "front",
    "body",
    "back",
]
CORPUS_TABLE_FEATURES = [
    "p",
    "ruby",
    "note",
    "persName",
    "placeName",
    "roleName",
    "said",
    "listPerson",
]


def canonical_work_id(value):
    text = str(value).strip()
    if not re.fullmatch(r"\d+", text):
        return None
    normalized = text.lstrip("0")
    return normalized if normalized else "0"


def local_name(tag):
    if tag.startswith("{"):
        return tag.split("}", 1)[1]
    return tag


def normalize_ws(text):
    return " ".join(text.split())


def remove_ws(text):
    return "".join(text.split())


def element_text(element, skip_locals=frozenset()):
    parts = []

    def visit(node):
        if local_name(node.tag) in skip_locals:
            return
        if node.text:
            parts.append(node.text)
        for child in list(node):
            visit(child)
            if child.tail:
                parts.append(child.tail)

    if element is not None:
        visit(element)
    return "".join(parts)


def first_element(root, wanted_local):
    for element in root.iter():
        if local_name(element.tag) == wanted_local:
            return element
    return None


def namespace_name(tag):
    if tag.startswith("{"):
        return tag[1:].split("}", 1)[0]
    return ""


def analyze_file(path):
    path = pathlib.Path(path)
    root = ET.parse(path).getroot()
    counts = collections.Counter(local_name(element.tag) for element in root.iter())
    namespaces = collections.Counter(namespace_name(element.tag) for element in root.iter())
    title_element = first_element(root, "title")
    work_id = None
    for element in root.iter():
        if local_name(element.tag) == "idno" and element.attrib.get("type") == "aozora-work-id":
            work_id = canonical_work_id(element_text(element))
            break
    body = first_element(root, "body")
    full_body_text = element_text(body)
    base_body_text = element_text(body, SKIP_BASE_TEXT_LOCALS)
    return {
        "path": str(path),
        "title": normalize_ws(element_text(title_element)) if title_element is not None else None,
        "aozora_work_id": work_id,
        "counts": dict(sorted(counts.items())),
        "namespace_counts": dict(sorted(namespaces.items())),
        "body_text_no_ws": remove_ws(full_body_text),
        "body_base_text_no_ws": remove_ws(base_body_text),
        "body_base_text_length": len(remove_ws(base_body_text)),
        "body_full_text_length": len(remove_ws(full_body_text)),
    }


def relpath(path, root):
    return pathlib.Path(path).relative_to(root).as_posix()


def state_from_relpath(relative_path):
    parts = pathlib.PurePosixPath(relative_path).parts
    if len(parts) >= 2 and parts[0] == "data":
        return parts[1]
    return parts[0] if parts else None


def level_from_relpath(relative_path):
    match = re.search(r"tei_lib_lv([2-5])", relative_path)
    if not match:
        return None
    return f"Level {match.group(1)}"


def tei_eaj_work_id(relative_path):
    stem = pathlib.PurePosixPath(relative_path).stem
    match = re.fullmatch(r"(\d+)_tei", stem)
    if match:
        return canonical_work_id(match.group(1))
    match = re.fullmatch(r"(\d+)_header.*", stem)
    if match:
        return canonical_work_id(match.group(1))
    match = re.fullmatch(r"(\d+)-\d+_tei", stem)
    if match:
        return canonical_work_id(match.group(1))
    match = re.fullmatch(r"\d+_(\d+)", stem)
    if match:
        return canonical_work_id(match.group(1))
    match = re.fullmatch(r"(\d{3,})", stem)
    if match:
        return canonical_work_id(match.group(1))
    return None


def discover_tei_eaj_files(source_root):
    source_root = pathlib.Path(source_root)
    data_root = source_root / "data"
    records = []
    for path in sorted(data_root.rglob("*.xml")):
        relative_path = relpath(path, source_root)
        features = analyze_file(path)
        records.append(
            {
                "path": str(path),
                "relpath": relative_path,
                "state": state_from_relpath(relative_path),
                "level": level_from_relpath(relative_path),
                "work_id": tei_eaj_work_id(relative_path),
                "title": features["title"],
                "features": features,
            }
        )
    return records


def is_melos_record(record):
    basename = pathlib.PurePosixPath(record["relpath"]).name
    title = record.get("title") or ""
    return basename.startswith("1567") or "走れメロス" in title


def melos_records(records):
    return [record for record in records if is_melos_record(record)]


def first_text_difference(left, right, context=36):
    if left == right:
        return None
    limit = min(len(left), len(right))
    index = next((i for i in range(limit) if left[i] != right[i]), limit)
    start = max(0, index - context)
    end = index + context
    return {
        "index": index,
        "abc": left[start:end],
        "tei_eaj": right[start:end],
    }


def compare_features(abc_features, record):
    other = record["features"]
    abc_text = abc_features["body_base_text_no_ws"]
    other_text = other["body_base_text_no_ws"]
    return {
        "relpath": record["relpath"],
        "work_id": record.get("work_id"),
        "state": record["state"],
        "level": record["level"],
        "title": record["title"],
        "base_text_equal": abc_text == other_text,
        "abc_body_base_text_length": len(abc_text),
        "tei_eaj_body_base_text_length": len(other_text),
        "first_difference": first_text_difference(abc_text, other_text),
        "feature_counts": {
            feature: {
                "abc": abc_features["counts"].get(feature, 0),
                "tei_eaj": other["counts"].get(feature, 0),
            }
            for feature in REPORT_FEATURES
        },
    }


def analyze_abc_counterpart(path):
    features = analyze_file(path)
    work_id = features.get("aozora_work_id")
    if work_id is None:
        return None
    return {
        "work_id": work_id,
        "path": pathlib.Path(path).as_posix(),
        "features": features,
    }


def abc_counterpart_from_spec(spec):
    if "=" in str(spec):
        work_id, path = str(spec).split("=", 1)
        return {
            "work_id": canonical_work_id(work_id),
            "path": pathlib.Path(path).as_posix(),
            "features": analyze_file(path),
        }
    return analyze_abc_counterpart(spec)


def discover_abc_counterparts(abc_tei_specs, abc_tei_dirs):
    counterparts = {}
    for spec in abc_tei_specs:
        counterpart = abc_counterpart_from_spec(spec)
        if counterpart and counterpart["work_id"]:
            counterparts[counterpart["work_id"]] = counterpart

    for directory in abc_tei_dirs:
        directory = pathlib.Path(directory)
        if not directory.exists():
            continue
        for path in sorted(directory.rglob("*.xml")):
            try:
                counterpart = analyze_abc_counterpart(path)
            except ET.ParseError:
                continue
            if counterpart and counterpart["work_id"] not in counterparts:
                counterparts[counterpart["work_id"]] = counterpart
    return counterparts


def all_work_comparison_row(counterparts, record):
    work_id = record.get("work_id")
    if work_id is None:
        return {
            "work_id": None,
            "relpath": record["relpath"],
            "state": record["state"],
            "level": record["level"],
            "title": record["title"],
            "abc_path": None,
            "base_text_equal": None,
            "abc_body_base_text_length": None,
            "tei_eaj_body_base_text_length": record["features"]["body_base_text_length"],
            "feature_counts": None,
            "first_difference": None,
            "missing_reason": "no_tei_eaj_work_id",
        }
    counterpart = counterparts.get(work_id)
    if counterpart is None:
        return {
            "work_id": work_id,
            "relpath": record["relpath"],
            "state": record["state"],
            "level": record["level"],
            "title": record["title"],
            "abc_path": None,
            "base_text_equal": None,
            "abc_body_base_text_length": None,
            "tei_eaj_body_base_text_length": record["features"]["body_base_text_length"],
            "feature_counts": None,
            "first_difference": None,
            "missing_reason": "missing_abc_counterpart",
        }

    comparison = compare_features(counterpart["features"], record)
    comparison["abc_path"] = counterpart["path"]
    comparison["missing_reason"] = None
    return comparison


def group_counts(records):
    grouped = collections.Counter((record["state"], record["level"]) for record in records)
    return [
        {"state": state, "level": level, "count": count}
        for (state, level), count in sorted(grouped.items(), key=lambda item: (item[0][0] or "", item[0][1] or ""))
    ]


def feature_prevalence(records):
    rows = []
    for feature in REPORT_FEATURES:
        counts = [record["features"]["counts"].get(feature, 0) for record in records]
        rows.append(
            {
                "feature": feature,
                "files_with_feature": sum(1 for count in counts if count > 0),
                "max_count": max(counts) if counts else 0,
            }
        )
    return rows


def corpus_rows(records):
    rows = []
    for record in records:
        counts = record["features"]["counts"]
        row = {
            "relpath": record["relpath"],
            "state": record["state"],
            "level": record["level"],
            "title": record["title"],
        }
        row.update({feature: counts.get(feature, 0) for feature in CORPUS_TABLE_FEATURES})
        rows.append(row)
    return rows


def build_all_work_report(abc_tei_specs, abc_tei_dirs, source_root, source_rev=None):
    records = discover_tei_eaj_files(source_root)
    counterparts = discover_abc_counterparts(abc_tei_specs, abc_tei_dirs)
    rows = [all_work_comparison_row(counterparts, record) for record in records]
    compared_rows = [row for row in rows if row["abc_path"]]
    missing_counterpart_rows = [row for row in rows if row["missing_reason"] == "missing_abc_counterpart"]
    no_work_id_rows = [row for row in rows if row["missing_reason"] == "no_tei_eaj_work_id"]
    return {
        "abc_tei_specs": [str(spec) for spec in abc_tei_specs],
        "abc_tei_dirs": [str(directory) for directory in abc_tei_dirs],
        "abc_counterparts": {
            work_id: counterpart["path"]
            for work_id, counterpart in sorted(counterparts.items(), key=lambda item: item[0])
        },
        "abc_counterpart_count": len(counterparts),
        "tei_eaj_root": pathlib.Path(source_root).as_posix(),
        "tei_eaj_source_rev": source_rev,
        "tei_eaj_file_count": len(records),
        "tei_eaj_work_id_count": len({record["work_id"] for record in records if record.get("work_id")}),
        "tei_eaj_groups": group_counts(records),
        "feature_prevalence": feature_prevalence(records),
        "corpus_rows": corpus_rows(records),
        "all_work_rows": rows,
        "compared_file_count": len(compared_rows),
        "missing_counterpart_count": len(missing_counterpart_rows),
        "no_work_id_count": len(no_work_id_rows),
        "uncompared_file_count": len(missing_counterpart_rows) + len(no_work_id_rows),
        "base_text_equal_count": sum(1 for row in compared_rows if row["base_text_equal"]),
        "base_text_mismatch_count": sum(1 for row in compared_rows if row["base_text_equal"] is False),
    }


def build_report(abc_tei, source_root, source_rev=None):
    abc_features = analyze_file(abc_tei)
    records = discover_tei_eaj_files(source_root)
    melos = melos_records(records)
    return {
        "abc_path": pathlib.Path(abc_tei).as_posix(),
        "tei_eaj_root": pathlib.Path(source_root).as_posix(),
        "tei_eaj_source_rev": source_rev,
        "abc_features": abc_features,
        "tei_eaj_file_count": len(records),
        "tei_eaj_groups": group_counts(records),
        "feature_prevalence": feature_prevalence(records),
        "corpus_rows": corpus_rows(records),
        "melos_file_count": len(melos),
        "complete_melos_file_count": sum(1 for record in melos if record["state"] == "complete"),
        "draft_melos_file_count": sum(1 for record in melos if record["state"] == "draft"),
        "melos_comparisons": [compare_features(abc_features, record) for record in melos],
    }


def markdown_table(headers, rows):
    lines = [
        "| " + " | ".join(headers) + " |",
        "| " + " | ".join("---" for _ in headers) + " |",
    ]
    for row in rows:
        lines.append("| " + " | ".join(str(cell) if cell is not None else "" for cell in row) + " |")
    return "\n".join(lines)


def compact_path(path):
    return f"`{path}`" if path else ""


def render_markdown(report):
    lines = [
        "# TEI-EAJ Aozora Melos Comparison Report",
        "",
        "Status: Generated comparison probe",
        "Date: 2026-07-04",
        "",
        "## Inputs",
        "",
        f"- ABC TEI: `{report['abc_path']}`",
        f"- TEI-EAJ source root: `{report['tei_eaj_root']}`",
    ]
    if report.get("tei_eaj_source_rev"):
        lines.append(f"- TEI-EAJ source revision: `{report['tei_eaj_source_rev']}`")
    lines.extend(
        [
            f"- TEI-EAJ XML files scanned: {report['tei_eaj_file_count']}",
            f"- TEI-EAJ Melos files found: {report['melos_file_count']}",
            f"- Complete Melos files: {report['complete_melos_file_count']}",
            f"- Draft Melos files: {report['draft_melos_file_count']}",
            "",
        ]
    )
    if report["draft_melos_file_count"] == 0:
        lines.extend(
            [
                "No draft Melos TEI files were found in the pinned TEI-EAJ checkout.",
                "Draft coverage below is therefore corpus-wide structural context, not",
                "a literal draft Melos text comparison.",
                "",
            ]
        )

    lines.extend(
        [
            "## Level/State Coverage",
            "",
            markdown_table(
                ["State", "Level", "Files"],
                [[row["state"], row["level"], row["count"]] for row in report["tei_eaj_groups"]],
            ),
            "",
            "## Melos File Comparisons",
            "",
        ]
    )

    melos_rows = []
    for comparison in report["melos_comparisons"]:
        counts = comparison["feature_counts"]
        melos_rows.append(
            [
                f"`{comparison['relpath']}`",
                comparison["state"],
                comparison["level"],
                "yes" if comparison["base_text_equal"] else "no",
                comparison["abc_body_base_text_length"],
                comparison["tei_eaj_body_base_text_length"],
                counts["p"]["abc"],
                counts["p"]["tei_eaj"],
                counts["note"]["abc"],
                counts["note"]["tei_eaj"],
                counts["persName"]["tei_eaj"],
                counts["said"]["tei_eaj"],
            ]
        )
    lines.extend(
        [
            markdown_table(
                [
                    "TEI-EAJ file",
                    "State",
                    "Level",
                    "Base text equal",
                    "ABC chars",
                    "TEI-EAJ chars",
                    "ABC p",
                    "TEI-EAJ p",
                    "ABC note",
                    "TEI-EAJ note",
                    "TEI-EAJ persName",
                    "TEI-EAJ said",
                ],
                melos_rows,
            ),
            "",
            "### First Text Differences",
            "",
        ]
    )
    for comparison in report["melos_comparisons"]:
        diff = comparison["first_difference"]
        lines.append(f"- `{comparison['relpath']}`:")
        if diff is None:
            lines.append("  base text matches after whitespace and ruby-reading removal.")
        else:
            lines.append(f"  first difference at normalized base-text offset {diff['index']}.")
            lines.append(f"  ABC window: `{diff['abc']}`")
            lines.append(f"  TEI-EAJ window: `{diff['tei_eaj']}`")
    lines.append("")

    lines.extend(
        [
            "## Feature Prevalence Across All TEI-EAJ Files",
            "",
            markdown_table(
                ["Feature", "Files With Feature", "Max Count In One File", "ABC Count"],
                [
                    [
                        row["feature"],
                        row["files_with_feature"],
                        row["max_count"],
                        report["abc_features"]["counts"].get(row["feature"], 0),
                    ]
                    for row in report["feature_prevalence"]
                ],
            ),
            "",
            "## All TEI-EAJ XML Files",
            "",
            markdown_table(
                ["Path", "State", "Level", "Title", *CORPUS_TABLE_FEATURES],
                [
                    [
                        f"`{row['relpath']}`",
                        row["state"],
                        row["level"],
                        row["title"],
                        *[row[feature] for feature in CORPUS_TABLE_FEATURES],
                    ]
                    for row in report["corpus_rows"]
                ],
            ),
            "",
            "## Interpretation",
            "",
            "- The literal Melos comparison covers every Melos TEI file in the pinned TEI-EAJ checkout.",
            "- Finished and draft TEI-EAJ files are both included in the corpus-wide structural profile.",
            "- Paragraph and source-note/source-attribution structure remain the immediate Level 3 gap for ABC.",
            "- TEI-EAJ Level 4 entity, role, place, and speech markup should remain an enrichment comparison track unless ABC declares an editorial enrichment layer.",
        ]
    )
    return "\n".join(lines) + "\n"


def render_all_work_markdown(report):
    lines = [
        "# TEI-EAJ Aozora All-Work Comparison Report",
        "",
        "Status: Generated comparison probe",
        "Date: 2026-07-04",
        "",
        "## Inputs",
        "",
        f"- TEI-EAJ source root: `{report['tei_eaj_root']}`",
    ]
    if report.get("tei_eaj_source_rev"):
        lines.append(f"- TEI-EAJ source revision: `{report['tei_eaj_source_rev']}`")
    lines.extend(
        [
            f"- ABC TEI specs: {', '.join(compact_path(spec) for spec in report['abc_tei_specs']) or '(none)'}",
            f"- ABC TEI directories: {', '.join(compact_path(directory) for directory in report['abc_tei_dirs']) or '(none)'}",
            f"- ABC counterpart works discovered: {report['abc_counterpart_count']}",
            "",
            "## Summary",
            "",
            f"- TEI-EAJ XML files scanned: {report['tei_eaj_file_count']}",
            f"- TEI-EAJ candidate work IDs: {report['tei_eaj_work_id_count']}",
            f"- Compared TEI-EAJ files: {report['compared_file_count']}",
            f"- Base-text matches: {report['base_text_equal_count']}",
            f"- Base-text mismatches: {report['base_text_mismatch_count']}",
            f"- Missing ABC counterparts: {report['missing_counterpart_count']}",
            f"- TEI-EAJ files without candidate work IDs: {report['no_work_id_count']}",
            f"- Uncompared TEI-EAJ files: {report['uncompared_file_count']}",
            "",
            "Missing counterparts mean no ABC-generated TEI with the same normalized",
            "Aozora work ID was found in the supplied counterpart paths. Files",
            "without candidate work IDs need separate source identification before",
            "ABC can compare them automatically.",
            "",
            "## Level/State Coverage",
            "",
            markdown_table(
                ["State", "Level", "Files"],
                [[row["state"], row["level"], row["count"]] for row in report["tei_eaj_groups"]],
            ),
            "",
            "## ABC Counterparts",
            "",
        ]
    )
    counterpart_rows = [[work_id, compact_path(path)] for work_id, path in report["abc_counterparts"].items()]
    if counterpart_rows:
        lines.append(markdown_table(["Work ID", "ABC TEI"], counterpart_rows))
    else:
        lines.append("No ABC counterparts were discovered.")
    lines.extend(
        [
            "",
            "## All TEI-EAJ File Comparisons",
            "",
        ]
    )

    comparison_rows = []
    for row in report["all_work_rows"]:
        counts = row["feature_counts"] or {}
        p_counts = counts.get("p", {})
        note_counts = counts.get("note", {})
        comparison_rows.append(
            [
                row["work_id"],
                compact_path(row["relpath"]),
                row["state"],
                row["level"],
                row["title"],
                compact_path(row["abc_path"]),
                "" if row["base_text_equal"] is None else ("yes" if row["base_text_equal"] else "no"),
                row["abc_body_base_text_length"],
                row["tei_eaj_body_base_text_length"],
                p_counts.get("abc"),
                p_counts.get("tei_eaj"),
                note_counts.get("abc"),
                note_counts.get("tei_eaj"),
                row["missing_reason"],
            ]
        )
    lines.extend(
        [
            markdown_table(
                [
                    "Work ID",
                    "TEI-EAJ file",
                    "State",
                    "Level",
                    "Title",
                    "ABC TEI",
                    "Base text equal",
                    "ABC chars",
                    "TEI-EAJ chars",
                    "ABC p",
                    "TEI-EAJ p",
                    "ABC note",
                    "TEI-EAJ note",
                    "Missing reason",
                ],
                comparison_rows,
            ),
            "",
            "## Feature Prevalence Across All TEI-EAJ Files",
            "",
            markdown_table(
                ["Feature", "Files With Feature", "Max Count In One File"],
                [
                    [
                        row["feature"],
                        row["files_with_feature"],
                        row["max_count"],
                    ]
                    for row in report["feature_prevalence"]
                ],
            ),
            "",
            "## Interpretation",
            "",
            "- This report enumerates every XML file in the pinned TEI-EAJ `aozora_tei` checkout, including complete and draft files.",
            "- Rows with an ABC counterpart compare normalized body base text after whitespace and ruby-reading removal.",
            "- Rows without an ABC counterpart define the immediate materialization backlog, not a parser text mismatch.",
            "- Level 3 paragraph and source-note gaps should be resolved before ABC claims TEI-EAJ Level 3 publication TEI for generated prose.",
        ]
    )
    return "\n".join(lines) + "\n"


def main(argv=None):
    parser = argparse.ArgumentParser(description="Compare ABC TEI with pinned TEI-EAJ/aozora_tei files.")
    parser.add_argument("--report", choices=["melos", "all-work"], default="melos", help="Report to generate")
    parser.add_argument("--abc", help="ABC-generated Melos TEI path, or one ABC TEI path for all-work indexing")
    parser.add_argument("--abc-tei", action="append", default=[], help="ABC TEI counterpart path or WORK_ID=PATH mapping")
    parser.add_argument("--abc-tei-dir", action="append", default=[], help="Directory to scan recursively for ABC TEI counterparts")
    parser.add_argument("--tei-eaj-root", required=True, help="Pinned TEI-EAJ/aozora_tei checkout root")
    parser.add_argument("--source-rev", default=None, help="Pinned TEI-EAJ source revision")
    parser.add_argument("--output", default=None, help="Markdown report output path; stdout when omitted")
    args = parser.parse_args(argv)

    if args.report == "melos":
        if not args.abc:
            parser.error("--abc is required for --report melos")
        report = build_report(args.abc, args.tei_eaj_root, args.source_rev)
        markdown = render_markdown(report)
    else:
        abc_tei_specs = list(args.abc_tei)
        if args.abc:
            abc_tei_specs.append(args.abc)
        report = build_all_work_report(abc_tei_specs, args.abc_tei_dir, args.tei_eaj_root, args.source_rev)
        markdown = render_all_work_markdown(report)
    if args.output:
        output = pathlib.Path(args.output)
        output.parent.mkdir(parents=True, exist_ok=True)
        output.write_text(markdown, encoding="utf-8")
    else:
        sys.stdout.write(markdown)


if __name__ == "__main__":
    main()
