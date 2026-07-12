#!/usr/bin/env python3
"""Keigakomi 44-marker denominator residual: attribute-or-errata (Task 11).

Background
----------
The frozen keigakomi denominator is 717 (`decoration.keigakomi` in
`docs/superpowers/reports/2026-07-09-corpus-adapter-fidelity.summary.json`,
produced by the Rust source-authority scanner
`crates/ab-coverage/src/bin/source_inventory.rs`). A prior audit
(`docs/superpowers/reports/2026-07-11-keigakomi-yokogumi-denominator-
attribution.md`) applied the construct's own 13 `source_patterns` from
`data/aozora-syntax-coverage.toml` (`decoration.keigakomi`, line 2471) as
a plain `re.finditer` alternation directly against the corpus text and
got 673 -- a 44-marker shortfall against 717, not explained by UTF-8
decoding artifacts (checked and ruled out in that report).

This script reproduces BOTH figures under two counting modes over the
EXACT SAME production work universe (`reports/lib/corpus_reader.py`,
`work` class only -- the 17,886-entry universe the frozen fidelity
summary's `works_scanned` counts), then diffs every occurrence the two
modes disagree on:

  matrix mode   -- `re.finditer` over the union alternation of the 13
                   `decoration.keigakomi.source_patterns`, applied to
                   the RAW WORK TEXT directly. This is what produced 673.
                   `re.finditer` commits to the FIRST alternative that
                   matches at a given start position (textual order of
                   the alternation) and consumes non-overlapping spans.

  rust mode     -- a faithful Python port of the Rust scanner's counting
                   unit. `source_inventory.rs::scan_work` calls
                   `ab_source_syntax::source_markers(text)` to tokenize
                   the document into discrete MARKERS FIRST (Ruby,
                   Gaiji, Command, EditorialNote, SegmentBoundary,
                   BracketNote/AccentNotation, and their Malformed
                   variants -- see `crates/ab-source-syntax/src/lib.rs`,
                   `scan_next_marker`/`scan_markers`), THEN tests each
                   of the 13 keigakomi patterns against each marker's
                   RAW text via `Regex::is_match` (substring search, not
                   full-match) -- see `crates/ab-coverage/src/
                   source_inventory.rs::matching_rows`. A row gets AT
                   MOST ONE credit per marker (`!rows.contains(row_id)`
                   dedup), plus ONE additional credit per pair of
                   TEXTUALLY ADJACENT markers (`marker.span.end ==
                   next_marker.span.start`, i.e. zero characters
                   between them) whose COMBINED raw text matches a
                   pattern that NEITHER marker matches alone
                   (`append_composite_matching_rows`). This is the
                   Rust scanner's exact counting unit: MARKERS, not
                   regex-alternation match spans.

The two modes therefore disagree in two structural ways: (a) matrix mode
finds matches ANYWHERE in the raw text, including free body prose that
is never tokenized into a "marker" at all (pattern 4,
`「[^\n]+」[のは]罫囲み`, has no bracket delimiters and can match plain
descriptive prose); rust mode can NEVER credit such an occurrence, since
`matching_rows` only ever tests already-tokenized `marker.raw` spans.
(b) rust mode's per-marker, per-row dedup and composite-adjacency rule
are marker-shaped, not regex-span-shaped: two markers that are
separately un-matching can jointly earn ONE credit under rust mode that
matrix mode's single-pass `finditer` cannot produce (finditer never
"combines" two non-overlapping matches), and conversely a single marker
whose raw text satisfies TWO of the 13 patterns is credited once by BOTH
modes (row-level dedup vs. alternation non-overlap are both dedup
mechanisms, just different in shape).

Regex dialect: the 13 patterns use only alternation, quantifiers, and
negated character classes -- no backreferences, lookaround, or
`(?i)` case-insensitivity flags. Rust's `regex` crate and Python's `re`
compile these identically (Unicode-aware character classes by default in
both engines); no dialect-specific translation was required for this
construct's pattern set. This is checked, not assumed: every pattern
round-trips through `re.compile` without adjustment, and the reference
report's 673 figure was already produced by this exact `re.finditer`
technique.

Usage
-----
    keigakomi-residual-attribution.py --corpus <store-path>
        [--matrix data/aozora-syntax-coverage.toml] [--out summary.json]
        [--jobs N] [--diff] [--expected-works 17886]

Exit code 2 if the discovered `work`-class universe != --expected-works
(default 17886) -- the totals below are meaningless against a different
universe.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import pathlib
import re
import sys
import tomllib
from dataclasses import dataclass
from typing import Any

_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from reports.lib.corpus_reader import (  # noqa: E402
    classify_candidate,
    decode_source,
    discover_entries,
    sha256_hex,
)
from reports.lib.io import write_json  # noqa: E402

SCHEMA_VERSION = "keigakomi-residual-attribution-v1"
ROW_ID = "decoration.keigakomi"
DEFAULT_MATRIX_TOML = _REPO_ROOT / "data" / "aozora-syntax-coverage.toml"
FROZEN_MATRIX_EXACT = 673
FROZEN_DENOMINATOR = 717


# --------------------------------------------------------------------------
# TOML pattern loading + matrix-mode alternation
# --------------------------------------------------------------------------


def load_keigakomi_patterns(toml_path: pathlib.Path = DEFAULT_MATRIX_TOML) -> list[str]:
    """Parse `data/aozora-syntax-coverage.toml` and return
    `decoration.keigakomi.source_patterns` verbatim (13 patterns)."""
    with toml_path.open("rb") as handle:
        doc = tomllib.load(handle)
    for row in doc.get("syntax", []):
        if row.get("id") == ROW_ID:
            patterns = list(row.get("source_patterns", []))
            if not patterns:
                raise ValueError(f"{ROW_ID} has no source_patterns in {toml_path}")
            return patterns
    raise KeyError(f"{ROW_ID} not found in {toml_path}")


def build_matrix_alternation(patterns: list[str]) -> re.Pattern[str]:
    """Union alternation of the construct's own patterns, deduplicated per
    construct by `re.finditer`'s non-overlapping match consumption -- the
    SAME technique that produced the frozen matrix-exact figure of 673.
    Each pattern gets its own named group so a match's originating
    pattern index can be recovered (`re.finditer` tries alternatives in
    the alternation's textual order and commits to the first branch that
    matches at a given start position; preserving the TOML's own pattern
    order matters for that reason)."""
    return re.compile(
        "|".join(f"(?P<p{index}>{pattern})" for index, pattern in enumerate(patterns))
    )


def matrix_matches(text: str, alternation: re.Pattern[str]) -> list[tuple[int, str, int]]:
    """Every non-overlapping alternation match: (line, matched_text, pattern_index)."""
    results: list[tuple[int, str, int]] = []
    for match in alternation.finditer(text):
        pattern_index = -1
        for name, value in match.groupdict().items():
            if value is not None:
                pattern_index = int(name[1:])
                break
        line = text.count("\n", 0, match.start()) + 1
        results.append((line, match.group(0), pattern_index))
    return results


# --------------------------------------------------------------------------
# Rust-scanner-mode: a faithful port of ab-source-syntax's marker tokenizer.
#
# Ports crates/ab-source-syntax/src/lib.rs's scan_markers/scan_next_marker
# case-for-case. Rust operates on UTF-8 byte offsets; this port operates on
# Python `str` code-point offsets -- the split/boundary DECISIONS below are
# all character-equality tests (literal prefixes, single delimiter chars),
# never a byte-length computation, so the two are functionally equivalent:
# every `+ 'X'.len_utf8()` in the Rust source becomes `+ 1` here (advance
# one code point), regardless of whether X is ASCII or multi-byte.
#
# Large-document performance note: the outer scan advances one code point
# at a time when no marker starts at the current offset. Using `text[off:]`
# (a full copy) at every such offset would be O(n^2) on multi-hundred-KB
# Aozora texts. Every helper below uses `str.find`/`str.startswith` with
# explicit `start`/`end` arguments (which do not copy) instead of slicing
# the remaining text.
# --------------------------------------------------------------------------

_NEWLINE_CHARS = "\r\n"


@dataclass(frozen=True)
class RawMarker:
    start: int
    end: int
    line: int
    kind: str
    raw: str


def _has_any(text: str, start: int, end: int, chars: str) -> bool:
    return any(text.find(ch, start, end) != -1 for ch in chars)


def _marker_end_on_same_line(text: str, content_start: int, end_marker: str) -> int | None:
    marker_idx = text.find(end_marker, content_start)
    if marker_idx == -1:
        return None
    if _has_any(text, content_start, marker_idx, _NEWLINE_CHARS):
        return None
    return marker_idx


def _marker_end(text: str, content_start: int, end_marker: str) -> int | None:
    idx = text.find(end_marker, content_start)
    return idx if idx != -1 else None


def _skip_until_any_bracket(text: str, offset: int) -> int:
    fullwidth = text.find("］", offset)
    ascii_ = text.find("]", offset)
    if fullwidth != -1 and ascii_ != -1:
        return (fullwidth + 1) if fullwidth <= ascii_ else (ascii_ + 1)
    if fullwidth != -1:
        return fullwidth + 1
    if ascii_ != -1:
        return ascii_ + 1
    return len(text)


def _bottom_text_correction_note_end(text: str, offset: int) -> int | None:
    for prefix in ("」は底本では「", "」はママ"):
        if text.startswith(prefix, offset):
            return _skip_until_any_bracket(text, offset)
    return None


def _ruby_correction_note_end(text: str, offset: int) -> int | None:
    prefix = "［ルビの「"
    if not text.startswith(prefix, offset):
        return None
    target_start = offset + len(prefix)
    separator = "」は底本では「"
    idx = text.find(separator, target_start)
    if idx == -1:
        return None
    target_end = idx
    if _has_any(text, target_start, target_end, _NEWLINE_CHARS):
        return None
    source_start = target_end + len(separator)
    suffix = "」］"
    idx2 = text.find(suffix, source_start)
    if idx2 == -1:
        return None
    source_end = idx2
    if _has_any(text, source_start, source_end, _NEWLINE_CHARS):
        return None
    return source_end + len(suffix)


def _terminal_provenance_note_end(text: str, offset: int) -> int | None:
    prefix = "［＃地付き］（"
    if not text.startswith(prefix, offset):
        return None
    note_content_start = offset + len(prefix)
    note_content_end = _marker_end_on_same_line(text, note_content_start, "）")
    if note_content_end is None:
        return None
    note_end = note_content_end + 1
    i = note_end
    n = len(text)
    strip_chars = "\r\n 　\t"
    while i < n and text[i] in strip_chars:
        i += 1
    return note_end if text.startswith("底本：", i) else None


def _starts_with_ruby_legend_delimiter(text: str, offset: int) -> bool:
    return offset < len(text) and text[offset] in "：；"


def _explicit_ruby_bounds(text: str, base_start: int) -> tuple[int, int] | None:
    """Returns (reading_end_exclusive_of_close, marker_end) or None."""
    base_end = text.find("《", base_start)
    if base_end == -1:
        return None
    if _has_any(text, base_start, base_end, "\r\n》"):
        return None
    reading_start = base_end + 1
    reading_end = _marker_end_on_same_line(text, reading_start, "》")
    if reading_end is None:
        return None
    return reading_end, reading_end + 1


def _bracket_marker_kind(body: str) -> str:
    if body.isascii() and "\r" not in body and "\n" not in body:
        return "AccentNotation"
    return "BracketNote"


def _command_end_on_same_line(text: str, content_start: int, end_marker: str) -> int | None:
    offset = content_start
    n = len(text)
    while offset < n:
        if text.startswith("※［＃", offset):
            nested_start = offset + 3
            end = _marker_end_on_same_line(text, nested_start, "］")
            if end is not None:
                offset = end + 1
                continue
        if text.startswith("※[#", offset):
            nested_start = offset + 3
            end = _marker_end_on_same_line(text, nested_start, "]")
            if end is not None:
                offset = end + 1
                continue
        if text.startswith("［＃", offset):
            nested_start = offset + 2
            end = _command_end_on_same_line(text, nested_start, "］")
            if end is not None:
                offset = end + 1
                continue
        if text.startswith("[#", offset):
            nested_start = offset + 2
            end = _command_end_on_same_line(text, nested_start, "]")
            if end is not None:
                offset = end + 1
                continue
        if end_marker == "］" and text.startswith("［", offset):
            nested_start = offset + 1
            end = _marker_end_on_same_line(text, nested_start, "］")
            if end is not None:
                offset = end + 1
                continue
        if end_marker == "]" and text.startswith("[", offset):
            nested_start = offset + 1
            end = _marker_end_on_same_line(text, nested_start, "]")
            if end is not None:
                offset = end + 1
                continue
        ch = text[offset]
        if ch == end_marker:
            return offset
        if ch in _NEWLINE_CHARS:
            return None
        offset += 1
    return None


def scan_next_marker(text: str, offset: int, line: int) -> RawMarker | None:
    """Faithful port of ab-source-syntax's `scan_next_marker`."""
    note_end = _bottom_text_correction_note_end(text, offset)
    if note_end is not None:
        return RawMarker(offset, note_end, line, "EditorialNoteBottomTextCorrection", text[offset:note_end])

    note_end = _ruby_correction_note_end(text, offset)
    if note_end is not None:
        return RawMarker(offset, note_end, line, "EditorialNoteRubyCorrection", text[offset:note_end])

    note_end = _terminal_provenance_note_end(text, offset)
    if note_end is not None:
        return RawMarker(offset, note_end, line, "SegmentBoundaryTerminalProvenance", text[offset:note_end])

    if text.startswith("※［＃", offset):
        content_start = offset + 3
        content_end = _marker_end_on_same_line(text, content_start, "］")
        if content_end is not None:
            marker_end = content_end + 1
            return RawMarker(offset, marker_end, line, "GaijiFullwidth", text[offset:marker_end])
        return RawMarker(offset, content_start, line, "MalformedGaiji", text[offset:content_start])

    if text.startswith("※[#", offset):
        content_start = offset + 3
        content_end = _marker_end_on_same_line(text, content_start, "]")
        if content_end is not None:
            marker_end = content_end + 1
            return RawMarker(offset, marker_end, line, "GaijiAscii", text[offset:marker_end])
        return RawMarker(offset, content_start, line, "MalformedGaijiAscii", text[offset:content_start])

    if text.startswith("［＃", offset):
        content_start = offset + 2
        content_end = _command_end_on_same_line(text, content_start, "］")
        if content_end is not None:
            marker_end = content_end + 1
            return RawMarker(offset, marker_end, line, "CommandFullwidth", text[offset:marker_end])
        return RawMarker(offset, content_start, line, "MalformedCommand", text[offset:content_start])

    if text.startswith("[#", offset):
        content_start = offset + 2
        content_end = _command_end_on_same_line(text, content_start, "]")
        if content_end is not None:
            marker_end = content_end + 1
            return RawMarker(offset, marker_end, line, "CommandAscii", text[offset:marker_end])
        return RawMarker(offset, content_start, line, "MalformedCommandAscii", text[offset:content_start])

    if text.startswith("｜", offset):
        base_start = offset + 1
        if _starts_with_ruby_legend_delimiter(text, base_start):
            return None
        bounds = _explicit_ruby_bounds(text, base_start)
        if bounds is not None:
            _reading_end, marker_end = bounds
            return RawMarker(offset, marker_end, line, "RubyExplicit", text[offset:marker_end])
        return None

    if text.startswith("《", offset):
        reading_start = offset + 1
        reading_end = _marker_end_on_same_line(text, reading_start, "》")
        if reading_end is not None:
            marker_end = reading_end + 1
            return RawMarker(offset, marker_end, line, "RubyImplicit", text[offset:marker_end])
        return RawMarker(offset, reading_start, line, "MalformedImplicitRuby", text[offset:reading_start])

    if text.startswith("〔", offset):
        body_start = offset + 1
        body_end = _marker_end(text, body_start, "〕")
        if body_end is not None:
            if body_start == body_end:
                return None
            marker_end = body_end + 1
            body = text[body_start:body_end]
            kind = _bracket_marker_kind(body)
            return RawMarker(offset, marker_end, line, kind, text[offset:marker_end])
        return RawMarker(offset, body_start, line, "MalformedAccentNotation", text[offset:body_start])

    return None


def scan_markers(text: str) -> list[RawMarker]:
    """Faithful port of ab-source-syntax's `scan_markers`/`source_markers`."""
    markers: list[RawMarker] = []
    offset = 0
    line = 1
    n = len(text)
    while offset < n:
        marker = scan_next_marker(text, offset, line)
        if marker is not None:
            end = marker.end
            line += text.count("\n", offset, end)
            markers.append(marker)
            offset = end
            continue
        ch = text[offset]
        offset += 1
        if ch == "\n":
            line += 1
    return markers


def _marker_matches_any(raw: str, compiled_patterns: list[re.Pattern[str]]) -> bool:
    """Port of `matching_rows`' `.any(|pattern| pattern.is_match(raw))` for a
    single row (`decoration.keigakomi`): `Regex::is_match` is an unanchored
    substring search, the same as Python's `.search`."""
    return any(pattern.search(raw) for pattern in compiled_patterns)


def rust_matches(text: str, compiled_patterns: list[re.Pattern[str]]) -> list[tuple[int, str, str]]:
    """Port of `inventory_document`'s per-marker, per-row counting for the
    `decoration.keigakomi` row: (line, matched_raw, occurrence_kind), where
    occurrence_kind is "marker" (a single marker's raw matched directly) or
    "composite" (two textually-adjacent markers matched only in
    combination, per `append_composite_matching_rows`)."""
    markers = scan_markers(text)
    occurrences: list[tuple[int, str, str]] = []
    count = len(markers)
    for index, marker in enumerate(markers):
        matched_alone = _marker_matches_any(marker.raw, compiled_patterns)
        if matched_alone:
            occurrences.append((marker.line, marker.raw, "marker"))
        if index + 1 < count:
            next_marker = markers[index + 1]
            if marker.end == next_marker.start:
                combined = marker.raw + next_marker.raw
                matches_composite = _marker_matches_any(combined, compiled_patterns)
                matches_part = matched_alone or _marker_matches_any(next_marker.raw, compiled_patterns)
                if matches_composite and not matches_part:
                    occurrences.append((marker.line, combined, "composite"))
    return occurrences


# --------------------------------------------------------------------------
# Corpus-wide scan (production universe via reports/lib/corpus_reader.py)
# --------------------------------------------------------------------------

# Populated by `_configure_patterns` before any worker (process-pool or
# in-process) touches `process_candidate` -- see the docstring on `main`
# for why this must happen before the pool forks.
MATRIX_ALTERNATION: re.Pattern[str] | None = None
KEIGAKOMI_COMPILED: list[re.Pattern[str]] = []


def _configure_patterns(patterns: list[str]) -> None:
    global MATRIX_ALTERNATION, KEIGAKOMI_COMPILED
    MATRIX_ALTERNATION = build_matrix_alternation(patterns)
    KEIGAKOMI_COMPILED = [re.compile(pattern) for pattern in patterns]


def process_candidate(corpus_root_str: str, entry_str: str) -> dict[str, Any]:
    assert MATRIX_ALTERNATION is not None, "_configure_patterns must run before process_candidate"
    corpus_root = pathlib.Path(corpus_root_str)
    candidate = classify_candidate(corpus_root, pathlib.Path(entry_str))
    record: dict[str, Any] = {
        "class": candidate.klass,
        "rel": candidate.rel,
        "detail": candidate.detail,
    }
    if candidate.data is None:
        return record
    text = decode_source(candidate.data)
    matrix_hits = matrix_matches(text, MATRIX_ALTERNATION)
    rust_hits = rust_matches(text, KEIGAKOMI_COMPILED)
    record.update(
        label=candidate.label,
        work_id=candidate.work_id,
        reader=candidate.reader,
        sha256=sha256_hex(candidate.data),
        matrix_count=len(matrix_hits),
        rust_count=len(rust_hits),
    )
    if matrix_hits or rust_hits:
        record["matrix_hits"] = [
            {"line": line, "text": matched, "pattern_index": idx} for line, matched, idx in matrix_hits
        ]
        record["rust_hits"] = [
            {"line": line, "text": matched, "kind": kind} for line, matched, kind in rust_hits
        ]
    return record


def build_diff(works: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """Every (work, line, matched-text) present in one mode's occurrence
    set and not the other's, for touched works only."""
    diff: list[dict[str, Any]] = []
    for record in works:
        matrix_hits = record.get("matrix_hits", [])
        rust_hits = record.get("rust_hits", [])
        if not matrix_hits and not rust_hits:
            continue
        matrix_keys = {(hit["line"], hit["text"]) for hit in matrix_hits}
        rust_keys = {(hit["line"], hit["text"]) for hit in rust_hits}
        label = record.get("label") or record.get("work_id") or record.get("rel")
        for line, text in sorted(matrix_keys - rust_keys):
            diff.append({"work": label, "line": line, "text": text, "mode": "matrix_only"})
        for line, text in sorted(rust_keys - matrix_keys):
            diff.append({"work": label, "line": line, "text": text, "mode": "rust_only"})
    return diff


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--corpus", required=True, type=pathlib.Path)
    parser.add_argument("--matrix", type=pathlib.Path, default=DEFAULT_MATRIX_TOML)
    parser.add_argument("--expected-works", type=int, default=17886)
    parser.add_argument("--out", type=pathlib.Path)
    parser.add_argument("--jobs", type=int, default=1)
    parser.add_argument(
        "--diff",
        action="store_true",
        help="also print the (work, line, matched-text, mode) diff table to stdout",
    )
    args = parser.parse_args()

    patterns = load_keigakomi_patterns(args.matrix)
    _configure_patterns(patterns)

    candidates = discover_entries(args.corpus)
    if args.jobs <= 1:
        records = [process_candidate(str(args.corpus), str(entry)) for entry in candidates]
    else:
        with concurrent.futures.ProcessPoolExecutor(max_workers=args.jobs) as pool:
            futures = [
                pool.submit(process_candidate, str(args.corpus), str(entry)) for entry in candidates
            ]
            records = [future.result() for future in futures]

    by_class: dict[str, list[dict[str, Any]]] = {
        "work": [], "non_work": [], "recovered_extra": [], "unreadable": [],
    }
    for record in records:
        by_class[record["class"]].append(record)

    works = by_class["work"]
    if len(works) != args.expected_works:
        print(
            f"FAIL: {len(works)} readable works, expected {args.expected_works} "
            f"(classes: {{k: len(v) for k, v in by_class.items()}})",
            file=sys.stderr,
        )
        return 2

    matrix_total = sum(record.get("matrix_count", 0) for record in works)
    rust_total = sum(record.get("rust_count", 0) for record in works)
    diff = build_diff(works)

    per_work = [
        {
            "work_id": record.get("work_id"),
            "label": record.get("label"),
            "matrix_count": record.get("matrix_count", 0),
            "rust_count": record.get("rust_count", 0),
        }
        for record in works
        if record.get("matrix_count", 0) or record.get("rust_count", 0)
    ]

    summary = {
        "schema_version": SCHEMA_VERSION,
        "corpus": str(args.corpus),
        "matrix_toml": str(args.matrix),
        "row_id": ROW_ID,
        "patterns": patterns,
        "candidates_discovered": len(records),
        "candidate_classes": {klass: len(entries) for klass, entries in by_class.items()},
        "works_expected": args.expected_works,
        "works_scanned": len(works),
        "matrix_total": matrix_total,
        "rust_total": rust_total,
        "frozen_matrix_exact": FROZEN_MATRIX_EXACT,
        "frozen_denominator": FROZEN_DENOMINATOR,
        "matrix_reproduces_frozen_673": matrix_total == FROZEN_MATRIX_EXACT,
        "rust_reproduces_frozen_717": rust_total == FROZEN_DENOMINATOR,
        "residual_rust_minus_matrix": rust_total - matrix_total,
        "residual_matches_44": (rust_total - matrix_total) == 44,
        "works_touched": len(per_work),
        "per_work": per_work,
        "diff_count": len(diff),
        "diff": diff,
    }

    if args.out:
        write_json(args.out, summary, indent=1)
    else:
        import json

        sys.stdout.write(json.dumps(summary, ensure_ascii=False, indent=1) + "\n")

    if args.diff:
        for entry in diff:
            print(f"{entry['mode']}\t{entry['work']}\tline={entry['line']}\t{entry['text']}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
