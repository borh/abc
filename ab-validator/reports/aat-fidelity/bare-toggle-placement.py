#!/usr/bin/env python3
"""Bare-toggle placement + adoption-grammar attribution (Revision 2).

Measures, over the EXACT production corpus universe, what the Phase 5
bare-toggle classifier (Contract 1 of
docs/superpowers/specs/2026-07-12-consolidated-parser-phase5-bare-toggle-inline-design.md)
would adopt and decline. Revision 2 supersedes the Revision 1 scanner,
which used a private corpus reader (17,878 zip-borne works, no plaintext
sources, silent skip of recoverable zips) and independent per-construct
stack pairing that did not model the adoption grammar.

Source enumeration and reading
------------------------------
Reuses, by import, the litigated reader of
``reports/source-regions/terminal-provenance-split.py`` — the instrument
whose entry universe was bound card-for-card to the Rust pipeline's
17,886-entry ``index.json`` during Phase 4 (split report Revision 2):
``discover_entries`` (every ``cards/*/files/*.zip`` and bare ``*.txt``),
content-sniffing zip-vs-plain dispatch (``sniffs_as_zip``), stdlib zip
reading with the local-header-trusting fallback for the two known
central-directory corruption patterns
(``_read_zip_member_bypassing_central_directory``), Shift_JIS decode with
``errors="replace"``, and ``work_id_from_index_path``. The zip read here
inlines the split reader's documented try-order (stdlib validated read,
then the bypass) so that RECOVERY is observable and recorded per entry;
any behavioral divergence from ``_read_zip_entry`` would surface as an
entry-count or hash drift against the split instrument.

Fail-closed universe binding: the run aborts (exit 2) unless the number of
readable entries equals ``--expected-entries`` (default 17886). Every
excluded candidate is recorded with its path and reason; every recovered
zip (local-header fallback) is recorded as recovered, not "unreadable".

Adoption grammar (normative Python model of Contract 1)
--------------------------------------------------------
``classify_line`` below is the normative reference model of the
classifier's per-line two-pass algorithm; the Rust implementation must
mirror it test-for-test (the ``reports/lib/terminal_provenance.py``
precedent). Tokens are the exact bare forms only:

    yokogumi:  ［＃横組み］ / ［＃横組み終わり］
    keigakomi: ［＃罫囲み］ / ［＃罫囲み終わり］

Pass 1 — one GLOBAL nesting stack over the line's tokens in order:
  * open K: if a frame of construct K is already on the stack, mark K
    invalid for this line (same-construct reopen); push regardless so
    scanning stays total.
  * close K with empty stack: mark K invalid (orphan close).
  * close K with top-of-stack K: pop; record a candidate pair.
  * close K with top-of-stack J != K: improper interleaving; mark BOTH
    J and K invalid; the close is not paired and nothing is popped.
  * end of line: every frame still on the stack marks its construct
    invalid (orphan open).
Pass 2 — adoption: a candidate pair is adopted iff its construct was not
marked invalid on that line. Every marker of an invalid construct on the
line stays a byte-identical raw node, including markers of candidate
pairs rolled back by construct invalidation. Invalidation is
construct-scoped per line: a valid construct's pair nested inside an
invalidated construct's markers still adopts (the invalid markers remain
raw inline nodes; the valid pair never depended on their extent).

Usage:
    bare-toggle-placement.py --corpus <store-path> [--expected-entries N]
        [--out summary.json] [--jobs N]
"""

from __future__ import annotations

import argparse
import concurrent.futures
import collections
import hashlib
import importlib.util
import json
import pathlib
import re
import sys
import zipfile
from dataclasses import dataclass, field
from typing import Any

_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

_SPLIT_SCRIPT = (
    _REPO_ROOT / "reports" / "source-regions" / "terminal-provenance-split.py"
)
_spec = importlib.util.spec_from_file_location(
    "terminal_provenance_split", _SPLIT_SCRIPT
)
assert _spec is not None and _spec.loader is not None
_split = importlib.util.module_from_spec(_spec)
sys.modules[_spec.name] = _split
_spec.loader.exec_module(_split)

discover_entries = _split.discover_entries
is_text_entry = _split.is_text_entry
sniffs_as_zip = _split.sniffs_as_zip
work_id_from_index_path = _split.work_id_from_index_path
read_zip_bypassing_cd = _split._read_zip_member_bypassing_central_directory

SCHEMA_VERSION = "bare-toggle-placement-v2"
LINE_SPLIT = re.compile(r"\r\n|\r|\n")
SAMPLE_LIMIT = 8

CONSTRUCTS = ("yokogumi", "keigakomi")
TOKENS: dict[str, tuple[str, str]] = {
    "yokogumi": ("［＃横組み］", "［＃横組み終わり］"),
    "keigakomi": ("［＃罫囲み］", "［＃罫囲み終わり］"),
}
CONTEXT_PATTERNS = {
    "yokogumi": re.compile(r"［＃[^］]*横組み[^］]*］"),
    "keigakomi": re.compile(r"［＃[^］]*罫囲み[^］]*］"),
}
_TOKEN_ALTERNATION = re.compile(
    "|".join(
        re.escape(token) for pair in TOKENS.values() for token in pair
    )
)
_TOKEN_KIND = {
    token: (construct, kind)
    for construct, (open_token, close_token) in TOKENS.items()
    for token, kind in ((open_token, "open"), (close_token, "close"))
}


@dataclass
class LineOutcome:
    """Result of running the Contract 1 grammar over one line."""

    adopted_pairs: dict[str, int] = field(
        default_factory=lambda: {c: 0 for c in CONSTRUCTS}
    )
    orphan_open: dict[str, int] = field(
        default_factory=lambda: {c: 0 for c in CONSTRUCTS}
    )
    orphan_close: dict[str, int] = field(
        default_factory=lambda: {c: 0 for c in CONSTRUCTS}
    )
    reopen: dict[str, int] = field(
        default_factory=lambda: {c: 0 for c in CONSTRUCTS}
    )
    interleave_events: int = 0
    proper_nestings: int = 0
    rollback_markers: int = 0
    invalid_constructs: set[str] = field(default_factory=set)
    total_markers: int = 0


def classify_line(line: str) -> LineOutcome:
    """Normative two-pass model of the Contract 1 per-line grammar."""
    outcome = LineOutcome()
    tokens = [
        _TOKEN_KIND[match.group(0)] for match in _TOKEN_ALTERNATION.finditer(line)
    ]
    outcome.total_markers = len(tokens)
    if not tokens:
        return outcome

    # Pass 1: global nesting stack.
    stack: list[str] = []
    # candidate: (construct, nested_inside_other_construct)
    candidates: list[tuple[str, bool]] = []
    orphan_close_counts: dict[str, int] = {c: 0 for c in CONSTRUCTS}
    for construct, kind in tokens:
        if kind == "open":
            if construct in stack:
                outcome.invalid_constructs.add(construct)
                outcome.reopen[construct] += 1
            stack.append(construct)
        else:  # close
            if not stack:
                outcome.invalid_constructs.add(construct)
                orphan_close_counts[construct] += 1
            elif stack[-1] == construct:
                stack.pop()
                candidates.append((construct, bool(stack)))
            else:
                outcome.invalid_constructs.add(construct)
                outcome.invalid_constructs.add(stack[-1])
                outcome.interleave_events += 1
    orphan_open_counts: dict[str, int] = {c: 0 for c in CONSTRUCTS}
    for construct in stack:
        outcome.invalid_constructs.add(construct)
        orphan_open_counts[construct] += 1

    # Pass 2: adoption / rollback.
    for construct, nested in candidates:
        if construct in outcome.invalid_constructs:
            outcome.rollback_markers += 2
        else:
            outcome.adopted_pairs[construct] += 1
            if nested:
                outcome.proper_nestings += 1
    for construct in CONSTRUCTS:
        if construct in outcome.invalid_constructs:
            outcome.orphan_open[construct] += orphan_open_counts[construct]
            outcome.orphan_close[construct] += orphan_close_counts[construct]
    return outcome


def sha256_hex(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def read_entry(
    corpus_root: pathlib.Path, entry: pathlib.Path
) -> dict[str, Any]:
    """Read one discovered candidate, mirroring the split reader's
    dispatch order exactly, but exposing the branch taken.

    Returns a dict with status "ok" (label, bytes, reader, recovered) or
    status "excluded" (reason) — the latter only for candidates the split
    reader also excludes (zip-shaped but unreadable by both paths, or a
    zip with no text member)."""
    rel = str(entry.relative_to(corpus_root))
    is_zip_shaped = entry.name.lower().endswith(".zip") or sniffs_as_zip(entry)
    if not is_zip_shaped:
        return {
            "status": "ok",
            "label": rel,
            "bytes": entry.read_bytes(),
            "reader": "plain",
            "recovered": False,
        }
    # Stdlib validated path first (split _read_zip_entry try-order).
    try:
        with zipfile.ZipFile(entry) as zf:
            member = next((n for n in zf.namelist() if is_text_entry(n)), None)
            if member is None:
                return {"status": "excluded", "label": rel, "reason": "zip_no_text_member"}
            try:
                return {
                    "status": "ok",
                    "label": f"{rel}::{member}",
                    "bytes": zf.read(member),
                    "reader": "zip_stdlib",
                    "recovered": False,
                }
            except (zipfile.BadZipFile, OSError):
                pass
    except zipfile.BadZipFile:
        pass
    recovered = read_zip_bypassing_cd(entry, is_text_entry)
    if recovered is None:
        return {"status": "excluded", "label": rel, "reason": "zip_unreadable_both_paths"}
    member, data = recovered
    return {
        "status": "ok",
        "label": f"{rel}::{member}",
        "bytes": data,
        "reader": "zip_local_header_fallback",
        "recovered": True,
    }


def process_entry(corpus_root_str: str, entry_str: str) -> dict[str, Any]:
    corpus_root = pathlib.Path(corpus_root_str)
    entry = pathlib.Path(entry_str)
    read = read_entry(corpus_root, entry)
    if read["status"] != "ok":
        return read
    data: bytes = read["bytes"]
    text = data.decode("shift_jis", errors="replace")
    record: dict[str, Any] = {
        "status": "ok",
        "label": read["label"],
        "work_id": work_id_from_index_path(read["label"]),
        "reader": read["reader"],
        "recovered": read["recovered"],
        "decoding": "shift_jis/replace",
        "sha256": sha256_hex(data),
    }
    form_frequency: dict[str, dict[str, int]] = {}
    for construct, pattern in CONTEXT_PATTERNS.items():
        counts = collections.Counter(pattern.findall(text))
        if counts:
            form_frequency[construct] = dict(counts)
    record["form_frequency"] = form_frequency
    if not any(
        token in text for pair in TOKENS.values() for token in pair
    ):
        record["touched"] = False
        return record
    record["touched"] = True
    totals: dict[str, Any] = {
        "adopted_pairs": {c: 0 for c in CONSTRUCTS},
        "orphan_open": {c: 0 for c in CONSTRUCTS},
        "orphan_close": {c: 0 for c in CONSTRUCTS},
        "reopen": {c: 0 for c in CONSTRUCTS},
        "interleave_events": 0,
        "proper_nestings": 0,
        "rollback_markers": 0,
        "lines_with_markers": 0,
        "invalid_lines": 0,
        "mixed_lines": 0,
        "total_markers": 0,
    }
    samples: dict[str, list[str]] = {"adopted": [], "declined": []}
    for line in LINE_SPLIT.split(text):
        outcome = classify_line(line)
        if not outcome.total_markers:
            continue
        totals["lines_with_markers"] += 1
        totals["total_markers"] += outcome.total_markers
        adopted_any = any(outcome.adopted_pairs[c] for c in CONSTRUCTS)
        if outcome.invalid_constructs:
            totals["invalid_lines"] += 1
            if adopted_any:
                totals["mixed_lines"] += 1
            if len(samples["declined"]) < SAMPLE_LIMIT:
                samples["declined"].append(line.strip()[:120])
        elif adopted_any and len(samples["adopted"]) < SAMPLE_LIMIT:
            samples["adopted"].append(line.strip()[:120])
        for construct in CONSTRUCTS:
            totals["adopted_pairs"][construct] += outcome.adopted_pairs[construct]
            totals["orphan_open"][construct] += outcome.orphan_open[construct]
            totals["orphan_close"][construct] += outcome.orphan_close[construct]
            totals["reopen"][construct] += outcome.reopen[construct]
        totals["interleave_events"] += outcome.interleave_events
        totals["proper_nestings"] += outcome.proper_nestings
        totals["rollback_markers"] += outcome.rollback_markers
    record["grammar"] = totals
    record["samples"] = samples
    return record


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--corpus", required=True, type=pathlib.Path)
    parser.add_argument("--expected-entries", type=int, default=17886)
    parser.add_argument("--out", type=pathlib.Path)
    parser.add_argument("--jobs", type=int, default=1)
    args = parser.parse_args()

    candidates = discover_entries(args.corpus)
    if args.jobs <= 1:
        results = [
            process_entry(str(args.corpus), str(entry)) for entry in candidates
        ]
    else:
        with concurrent.futures.ProcessPoolExecutor(max_workers=args.jobs) as pool:
            futures = [
                pool.submit(process_entry, str(args.corpus), str(entry))
                for entry in candidates
            ]
            results = [future.result() for future in futures]

    ok = [r for r in results if r["status"] == "ok"]
    excluded = [r for r in results if r["status"] != "ok"]
    touched = [r for r in ok if r.get("touched")]

    grammar_totals: dict[str, Any] = {
        "adopted_pairs": {c: 0 for c in CONSTRUCTS},
        "orphan_open": {c: 0 for c in CONSTRUCTS},
        "orphan_close": {c: 0 for c in CONSTRUCTS},
        "reopen": {c: 0 for c in CONSTRUCTS},
        "interleave_events": 0,
        "proper_nestings": 0,
        "rollback_markers": 0,
        "lines_with_markers": 0,
        "invalid_lines": 0,
        "mixed_lines": 0,
        "total_markers": 0,
    }
    form_frequency: dict[str, collections.Counter[str]] = {
        c: collections.Counter() for c in CONSTRUCTS
    }
    for record in ok:
        for construct, counts in record.get("form_frequency", {}).items():
            form_frequency[construct].update(counts)
    for record in touched:
        grammar = record["grammar"]
        for construct in CONSTRUCTS:
            for key in ("adopted_pairs", "orphan_open", "orphan_close", "reopen"):
                grammar_totals[key][construct] += grammar[key][construct]
        for key in (
            "interleave_events",
            "proper_nestings",
            "rollback_markers",
            "lines_with_markers",
            "invalid_lines",
            "mixed_lines",
            "total_markers",
        ):
            grammar_totals[key] += grammar[key]

    summary = {
        "schema_version": SCHEMA_VERSION,
        "corpus": str(args.corpus),
        "reader": {
            "provenance": (
                "reports/source-regions/terminal-provenance-split.py "
                "(discover_entries + content-sniffing dispatch + stdlib/"
                "local-header zip reading), imported; see module docstring"
            ),
            "decoding": "shift_jis/replace",
        },
        "entries_expected": args.expected_entries,
        "entries_scanned": len(ok),
        "entries_excluded": excluded,
        "recovered_zips": [
            {"label": r["label"], "sha256": r["sha256"]}
            for r in ok
            if r["recovered"]
        ],
        "grammar_totals": grammar_totals,
        "works_touched": len(touched),
        "works_with_invalid_lines": sum(
            1 for r in touched if r["grammar"]["invalid_lines"]
        ),
        "form_frequency": {
            construct: [
                {"form": form, "count": count}
                for form, count in counter.most_common()
            ]
            for construct, counter in form_frequency.items()
        },
        "touched_entries": [
            {
                key: record[key]
                for key in (
                    "work_id",
                    "label",
                    "reader",
                    "recovered",
                    "decoding",
                    "sha256",
                    "grammar",
                    "samples",
                )
            }
            for record in touched
        ],
    }

    payload = json.dumps(summary, ensure_ascii=False, indent=1) + "\n"
    if args.out:
        args.out.write_text(payload, encoding="utf-8")
    else:
        sys.stdout.write(payload)

    if len(ok) != args.expected_entries:
        print(
            f"FAIL: scanned {len(ok)} entries, expected {args.expected_entries} "
            f"({len(excluded)} excluded — see entries_excluded)",
            file=sys.stderr,
        )
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
