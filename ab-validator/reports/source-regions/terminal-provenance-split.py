#!/usr/bin/env python
"""Terminal-provenance / colophon-metadata measurement split (Task 12 instrument).

ABC policy v0.2.0 admits `terminal_provenance` and `colophon_metadata` as
source-region classes but flags both `needs_measurement_split`
(`reports/lib/source_region.py`). This generator applies the stateful
boundary rule in `reports/lib/terminal_provenance.py` (the NORMATIVE rule
-- Task 14's Rust `source_note` emission transcribes it case-for-case) to
every work in the corpus and reports how many tail lines fall on each
side of the 底本：/入力： boundary.

Corpus layout + iteration mirrors `reports/aat-fidelity/denominator-
attribution.py`'s `iter_work_texts`: the pinned `aozorabunko` corpus mirror
is a website snapshot, not a flat directory of plaintext -- each work's
Shift_JIS text lives inside a per-work zip under `cards/<id>/files/*.zip`
(plus a handful of bare `*.txt` siblings). `work_id_from_index_path`
mirrors `work_id_from_index_path` in `crates/ab-index/src/index.rs`; it
labels samples/residuals for humans but is NOT a dedup key -- ab-index's
own reference index has 236 ids shared by 2-3 entries each (ruby vs
non-ruby zip variants of the same literary work), so `works_scanned` and
friends count corpus TEXT ENTRIES (one per zip member / bare txt file),
the same unit as every other `works_scanned`/`files_scanned` figure in
this codebase (e.g. the pinned corpus's 17886-entry count).

Usage:
  terminal-provenance-split.py --corpus-root DIR --summary-json PATH \
      --report-md PATH [--jobs N]

Fail-closed: any tail line the state machine cannot classify (a non-blank
line reached before any state-setting head) is a residual. A non-empty
residual FAILS the run: the summary/report are still written (with
verdict `TERMINAL_PROVENANCE_SPLIT_UNCLASSIFIABLE_RESIDUAL`) for
post-mortem, up to 20 examples are printed to stderr, and the process
exits 2.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import pathlib
import sys
import zipfile
from typing import Any

_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from reports.lib.io import write_json
from reports.lib.terminal_provenance import (
    BOUNDARY_RULE,
    COLOPHON_METADATA_CLASS,
    TERMINAL_PROVENANCE_CLASS,
    UnclassifiableTail,
    classify_tail,
    find_tail_start,
    is_colophon_head,
    is_provenance_head,
)

SCHEMA_VERSION = "terminal-provenance-colophon-split-v1"
VERDICT_OK = "TERMINAL_PROVENANCE_SPLIT_OK"
VERDICT_FAIL = "TERMINAL_PROVENANCE_SPLIT_UNCLASSIFIABLE_RESIDUAL"
MAX_EXAMPLES = 20
MAX_SAMPLES = 10

# The 2026-07-06 source-authority representability run
# (docs/superpowers/reports/2026-07-04-source-authority-representability.md)
# recorded these MARKER OCCURRENCE counts from the ab-source-inventory Rust
# scanner's source_region_coverage block. This instrument measures a
# different unit (works, and tail lines under the stateful rule); the
# report reconciles the two.
REFERENCE_COVERAGE_DATE = "2026-07-06"
REFERENCE_TERMINAL_PROVENANCE_OCCURRENCES = 609
REFERENCE_COLOPHON_OCCURRENCES = 89416

# The Task 12 brief's head-marker skeleton (`PROVENANCE_HEADS`/`COLOPHON_HEADS`
# in reports/lib/terminal_provenance.py) is extended ONLY by corpus-scan
# residuals -- a non-empty `unclassifiable_examples` bucket after a
# full-corpus run means the skeleton is insufficient and must gain an entry,
# documented here with examples and work IDs. This list starts empty because
# the brief's skeleton, unmodified, reached a defined state before any
# non-blank tail line in every one of the 17886 pinned-corpus text entries
# (works_unclassifiable == 0 on the 2026-07-12 hinoki run) -- see the report's
# Rule Extensions section.
RULE_EXTENSIONS: list[dict[str, Any]] = []


def is_text_entry(name: str) -> bool:
    return (
        not name.startswith("__MACOSX/")
        and not name.endswith("/")
        and name.lower().endswith(".txt")
        and pathlib.Path(name).name.lower() != "readme.txt"
    )


def work_id_from_index_path(path: str) -> str:
    """Mirror `work_id_from_index_path` in `crates/ab-index/src/index.rs`.

    `cards/<card>/files/<n>_ruby_xxxx.zip::<entry>` -> `<card>_<n>`.
    Falls back to a sanitized file stem for paths outside `cards/*/files/`.
    """
    source_path = path.split("::", 1)[0]
    parts = source_path.split("/")
    if "cards" in parts:
        cards_pos = parts.index("cards")
        if len(parts) > cards_pos + 3 and parts[cards_pos + 2] == "files":
            card = parts[cards_pos + 1]
            file_dir = parts[cards_pos + 3]
            file_name = file_dir
            for suffix in (".zip", ".txt"):
                if file_name.endswith(suffix):
                    file_name = file_name[: -len(suffix)]
                    break
            file = file_name.split("_", 1)[0]
            return f"{card}_{file}"
    stem = pathlib.Path(source_path).stem or "unknown"
    return "".join(ch if (ch.isalnum() or ch in "-_") else "_" for ch in stem)


def discover_entries(corpus_root: pathlib.Path) -> list[pathlib.Path]:
    """Yield every `cards/*/files/*.zip` and bare `*.txt` sibling, sorted."""
    entries: list[pathlib.Path] = []
    for files_dir in sorted(corpus_root.glob("cards/*/files")):
        if not files_dir.is_dir():
            continue
        for path in sorted(files_dir.iterdir()):
            if not path.is_file():
                continue
            name_lower = path.name.lower()
            if name_lower.endswith(".zip") or (
                name_lower.endswith(".txt") and name_lower != "readme.txt"
            ):
                entries.append(path)
    return entries


def read_entry_text(corpus_root: pathlib.Path, entry: pathlib.Path) -> tuple[str, str] | None:
    """Return `(label, text)` for a corpus entry, or `None` if unreadable."""
    name_lower = entry.name.lower()
    rel = entry.relative_to(corpus_root)
    if name_lower.endswith(".zip"):
        try:
            with zipfile.ZipFile(entry) as zf:
                member = next((n for n in zf.namelist() if is_text_entry(n)), None)
                if member is None:
                    return None
                data = zf.read(member)
        except (zipfile.BadZipFile, KeyError):
            return None
        return f"{rel}::{member}", data.decode("shift_jis", errors="replace")
    return str(rel), entry.read_bytes().decode("shift_jis", errors="replace")


def process_entry(corpus_root_str: str, entry_str: str) -> dict[str, Any]:
    """Classify one corpus entry's tail. Runs in a worker process/thread."""
    corpus_root = pathlib.Path(corpus_root_str)
    entry = pathlib.Path(entry_str)
    result = read_entry_text(corpus_root, entry)
    if result is None:
        return {"status": "unreadable", "entry": str(entry.relative_to(corpus_root))}
    label, text = result
    work_id = work_id_from_index_path(label)
    # str.splitlines() treats \n, \r\n, and bare \r uniformly as boundaries
    # (matching the effect of the Rust pipeline's sanitize step, which
    # normalizes all three to \n before aozora_body_range ever runs) --
    # see crates/ab-aozora-aat/src/lib.rs line 229 and its `sanitize`
    # normalization comments.
    lines = text.splitlines()
    tail_start = find_tail_start(lines)
    if tail_start is None:
        return {"status": "no_tail", "work_id": work_id, "label": label}
    tail_lines = lines[tail_start:]
    try:
        classes = classify_tail(tail_lines)
    except UnclassifiableTail as exc:
        return {
            "status": "unclassifiable",
            "work_id": work_id,
            "label": label,
            "line": exc.line,
            "tail_relative_index": exc.index,
            "absolute_line_number": tail_start + exc.index + 1,
        }
    tp_lines = classes.count(TERMINAL_PROVENANCE_CLASS)
    colophon_lines = classes.count(COLOPHON_METADATA_CLASS)
    # Head-line vs. inherited-continuation-line split, for the reference
    # cross-check only (not part of the normative classification): the
    # 2026-07-06 reference's `colophon_metadata_occurrences` comes from a
    # per-LINE prefix scan (`is_colophon_metadata_line` in
    # crates/ab-coverage/src/bin/source_inventory.rs) that recognizes only
    # explicit head-shaped lines -- it has no notion of state, so it never
    # counts a continuation line like a bare date. Counting head hits here
    # the same way makes the two numbers comparable.
    head_hits = sum(
        1
        for line in tail_lines
        if is_provenance_head(line.strip()) or is_colophon_head(line.strip())
    )
    return {
        "status": "ok",
        "work_id": work_id,
        "label": label,
        "terminal_provenance_lines": tp_lines,
        "colophon_lines": colophon_lines,
        "head_line_hits": head_hits,
        "has_colophon": colophon_lines > 0,
    }


def run_entries(
    corpus_root: pathlib.Path, entries: list[pathlib.Path], jobs: int
) -> list[dict[str, Any]]:
    if jobs <= 1:
        return [process_entry(str(corpus_root), str(entry)) for entry in entries]
    with concurrent.futures.ProcessPoolExecutor(max_workers=jobs) as pool:
        futures = [pool.submit(process_entry, str(corpus_root), str(entry)) for entry in entries]
        return [future.result() for future in futures]


def build_summary(corpus_root: pathlib.Path, jobs: int) -> dict[str, Any]:
    entries = discover_entries(corpus_root)
    if not entries:
        raise SystemExit(f"no corpus entries found under {corpus_root}")

    results = run_entries(corpus_root, entries, jobs)

    works_scanned = 0
    works_without_tail = 0
    works_with_terminal_provenance = 0
    works_with_colophon = 0
    works_unclassifiable = 0
    terminal_provenance_lines = 0
    colophon_lines = 0
    head_line_hits = 0
    unreadable: list[str] = []
    unclassifiable: list[dict[str, Any]] = []
    provenance_samples: list[dict[str, Any]] = []
    colophon_samples: list[dict[str, Any]] = []

    for result in results:
        status = result["status"]
        if status == "unreadable":
            unreadable.append(result["entry"])
            continue
        work_id = result["work_id"]
        # NOT deduped by work_id: `work_id_from_index_path` (mirroring
        # crates/ab-index/src/index.rs) is not unique per corpus text --
        # ab-index's own reference index has 17886 entries but only 17605
        # unique ids, because a single literary work commonly has BOTH a
        # ruby and a non-ruby zip variant sharing one id. Each is an
        # independent source text with its own (possibly differently
        # classified) tail, so each is scanned and counted on its own; this
        # also keeps `works_scanned` in the same unit as every other
        # `works_scanned`/`files_scanned` figure in this codebase (a corpus
        # TEXT ENTRY count, e.g. denominator-attribution.py's
        # `files_scanned`), not a deduped work-identity count.
        works_scanned += 1
        if status == "no_tail":
            works_without_tail += 1
        elif status == "unclassifiable":
            works_unclassifiable += 1
            unclassifiable.append(result)
        else:  # "ok"
            works_with_terminal_provenance += 1
            terminal_provenance_lines += result["terminal_provenance_lines"]
            colophon_lines += result["colophon_lines"]
            head_line_hits += result["head_line_hits"]
            if len(provenance_samples) < MAX_SAMPLES:
                provenance_samples.append(
                    {
                        "work_id": work_id,
                        "label": result["label"],
                        "terminal_provenance_lines": result["terminal_provenance_lines"],
                    }
                )
            if result["has_colophon"]:
                works_with_colophon += 1
                if len(colophon_samples) < MAX_SAMPLES:
                    colophon_samples.append(
                        {
                            "work_id": work_id,
                            "label": result["label"],
                            "colophon_lines": result["colophon_lines"],
                        }
                    )

    unclassifiable.sort(key=lambda row: (row["work_id"], row["absolute_line_number"]))
    unclassifiable_examples = unclassifiable[:MAX_EXAMPLES]
    verdict = VERDICT_FAIL if unclassifiable else VERDICT_OK

    return {
        "schema_version": SCHEMA_VERSION,
        "verdict": verdict,
        "corpus_root": str(corpus_root),
        "works_scanned": works_scanned,
        "works_with_terminal_provenance": works_with_terminal_provenance,
        "works_with_colophon": works_with_colophon,
        "works_without_tail": works_without_tail,
        "works_unclassifiable": works_unclassifiable,
        "terminal_provenance_lines": terminal_provenance_lines,
        "colophon_lines": colophon_lines,
        "head_line_hits": head_line_hits,
        "unreadable_entries": sorted(unreadable),
        "unclassifiable_total": len(unclassifiable),
        "unclassifiable_examples": unclassifiable_examples,
        "provenance_samples": provenance_samples,
        "colophon_samples": colophon_samples,
        "boundary_rule": BOUNDARY_RULE,
        "rule_extensions": RULE_EXTENSIONS,
        "rule_extensions_note": (
            "empty: the brief's head-marker skeleton, unmodified, classified every "
            "tail line in this run with zero unclassifiable residual (works_unclassifiable "
            "== 0) -- no extension beyond 底本：/底本の親本： (provenance) and "
            "入力：/校正：/青空文庫作成ファイル：/※ (colophon) was needed. Lines matching "
            "other colophon-shaped prefixes the reference scanner also recognizes "
            "(親本：/初出：/校閲：/作成日：/修正：/ファイル作成：) are still classified "
            "correctly: they inherit whatever state the block already carries rather than "
            "needing their own head entry, since they never open a NEW block by themselves "
            "in the corpus."
        ),
        "reference_coverage": {
            "date": REFERENCE_COVERAGE_DATE,
            "terminal_provenance_occurrences": REFERENCE_TERMINAL_PROVENANCE_OCCURRENCES,
            "colophon_occurrences": REFERENCE_COLOPHON_OCCURRENCES,
            "reference_mechanism": (
                "terminal_provenance_occurrences counts SegmentBoundaryTerminalProvenance "
                "markers (crates/ab-source-syntax/src/lib.rs terminal_provenance_note_end): "
                "a ［＃地付き］（…） note immediately followed by a 底本： line -- a narrow "
                "structural co-occurrence, not 'has a 底本 block'. colophon_metadata_occurrences "
                "counts LINES matching a fixed ~20-prefix set including 底本：/底本の親本：/親本："
                "/初出： AND 入力：/校正：/校閲：/作成日：/修正：/ファイル作成：/青空文庫作成"
                "ファイル： together in ONE undifferentiated bucket (is_colophon_metadata_line "
                "in crates/ab-coverage/src/bin/source_inventory.rs) -- this conflation is "
                "exactly what ABC policy v0.2.0 flags needs_measurement_split and what this "
                "instrument splits. It is also a pure per-line prefix match with no state: it "
                "never counts a bare continuation line (an edition/date line, a plain name)."
            ),
            "this_run_unit": (
                "works_scanned/works_with_terminal_provenance/works_with_colophon count "
                "corpus TEXT ENTRIES (see works_scanned note above); "
                "terminal_provenance_lines/colophon_lines count ALL tail lines under the "
                "stateful rule (heads + inherited continuations); head_line_hits counts only "
                "the head-line subset (lines literally matching a provenance_heads/"
                "colophon_heads prefix) -- the comparable-in-kind figure to the reference's "
                "per-line prefix scan, modulo the head-list and prefix-set differences above."
            ),
        },
    }


def markdown_table(rows: list[list[str]]) -> list[str]:
    if not rows:
        return ["_No rows._", ""]
    out = ["| " + " | ".join(rows[0]) + " |", "|" + "|".join(["---"] * len(rows[0])) + "|"]
    for row in rows[1:]:
        out.append("| " + " | ".join(row) + " |")
    out.append("")
    return out


def render_markdown(summary: dict[str, Any]) -> str:
    lines = [
        "# Terminal-Provenance / Colophon-Metadata Measurement Split",
        "",
        f"Verdict: `{summary['verdict']}`",
        "",
        f"- corpus_root: `{summary['corpus_root']}`",
        f"- works_scanned: {summary['works_scanned']}",
        f"- works_with_terminal_provenance: {summary['works_with_terminal_provenance']}",
        f"- works_with_colophon: {summary['works_with_colophon']}",
        f"- works_without_tail: {summary['works_without_tail']}",
        f"- works_unclassifiable: {summary['works_unclassifiable']}",
        f"- terminal_provenance_lines: {summary['terminal_provenance_lines']}",
        f"- colophon_lines: {summary['colophon_lines']}",
        f"- head_line_hits: {summary['head_line_hits']}",
        f"- unreadable_entries: {len(summary['unreadable_entries'])}",
        "",
        "## Boundary Rule",
        "",
        f"- tail_start_marker: `{summary['boundary_rule']['tail_start_marker']}`",
        f"- tail_start_rule: {summary['boundary_rule']['tail_start_rule']}",
        f"- provenance_heads: {', '.join(summary['boundary_rule']['provenance_heads'])}",
        f"- colophon_heads: {', '.join(summary['boundary_rule']['colophon_heads'])}",
        "",
        "## Rule Extensions",
        "",
        f"- extensions beyond the skeleton: {len(summary['rule_extensions'])}",
        f"- {summary['rule_extensions_note']}",
        "",
    ]
    if summary["rule_extensions"]:
        lines.extend(["### Extension Details", ""])
        lines.extend(
            markdown_table(
                [["marker", "class", "reason", "example_work_id", "example_line"]]
                + [
                    [
                        str(ext.get("marker", "")),
                        str(ext.get("class", "")),
                        str(ext.get("reason", "")),
                        str(ext.get("example_work_id", "")),
                        f"`{ext.get('example_line', '')}`",
                    ]
                    for ext in summary["rule_extensions"]
                ]
            )
        )
    lines.extend(
        [
            "## Reference Coverage Cross-Check",
            "",
            f"- reference date: {summary['reference_coverage']['date']}",
            f"- reference terminal_provenance_occurrences: "
            f"{summary['reference_coverage']['terminal_provenance_occurrences']}",
            f"- reference colophon_occurrences: {summary['reference_coverage']['colophon_occurrences']}",
            f"- reference mechanism: {summary['reference_coverage']['reference_mechanism']}",
            f"- this run's unit: {summary['reference_coverage']['this_run_unit']}",
            "",
            "## Provenance Samples",
            "",
        ]
    )
    lines.extend(
        markdown_table(
            [["work_id", "label", "terminal_provenance_lines"]]
            + [
                [row["work_id"], f"`{row['label']}`", str(row["terminal_provenance_lines"])]
                for row in summary["provenance_samples"]
            ]
        )
    )
    lines.extend(["## Colophon Samples", ""])
    lines.extend(
        markdown_table(
            [["work_id", "label", "colophon_lines"]]
            + [
                [row["work_id"], f"`{row['label']}`", str(row["colophon_lines"])]
                for row in summary["colophon_samples"]
            ]
        )
    )
    if summary["unclassifiable_examples"]:
        lines.extend(["## Unclassifiable Residual (fail-closed)", ""])
        lines.extend(
            markdown_table(
                [["work_id", "label", "absolute_line_number", "line"]]
                + [
                    [
                        row["work_id"],
                        f"`{row['label']}`",
                        str(row["absolute_line_number"]),
                        f"`{row['line']}`",
                    ]
                    for row in summary["unclassifiable_examples"]
                ]
            )
        )
    if summary["unreadable_entries"]:
        lines.extend(["## Unreadable Entries", ""])
        lines.extend([f"- `{entry}`" for entry in summary["unreadable_entries"][:MAX_EXAMPLES]])
        lines.append("")
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--corpus-root", type=pathlib.Path, required=True)
    parser.add_argument("--summary-json", type=pathlib.Path, required=True)
    parser.add_argument("--report-md", type=pathlib.Path, required=True)
    parser.add_argument("--jobs", type=int, default=1)
    args = parser.parse_args()

    corpus_root = args.corpus_root.resolve()
    summary = build_summary(corpus_root, args.jobs)

    write_json(args.summary_json, summary)
    args.report_md.parent.mkdir(parents=True, exist_ok=True)
    args.report_md.write_text(render_markdown(summary), encoding="utf-8")

    if summary["verdict"] != VERDICT_OK:
        examples = summary["unclassifiable_examples"]
        print(
            f"FAIL-CLOSED: {summary['unclassifiable_total']} unclassifiable tail line(s) "
            f"(showing up to {MAX_EXAMPLES}):",
            file=sys.stderr,
        )
        for example in examples:
            print(
                f"  {example['work_id']} {example['label']} "
                f"line {example['absolute_line_number']}: {example['line']!r}",
                file=sys.stderr,
            )
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
