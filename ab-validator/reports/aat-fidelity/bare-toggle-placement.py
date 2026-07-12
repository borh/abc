#!/usr/bin/env python3
"""Bare-toggle placement + adoption-grammar attribution (Revision 3).

Measures, over the production corpus universe, what the Phase 5
bare-toggle classifier (Contract 1 of
docs/superpowers/specs/2026-07-12-consolidated-parser-phase5-bare-toggle-inline-design.md)
would adopt and decline.

Revision history:
  Rev 1 — private reader (17,878 zip works), naive per-construct pairing.
          Rejected by review (P5-2/P5-3, round 1).
  Rev 2 — split-scanner reader (17,886 works), exact adoption grammar.
          Review round 2 (P5-1) rejected the residual "unreadable both
          paths" bucket: candidates must be explicitly classified, with
          windows-31j member names and a tolerant 7zz recovery path.
  Rev 3 — reader moved to the shared ``reports/lib/corpus_reader.py``
          contract: every discovered candidate is classified as
          work / non_work / recovered_extra / unreadable. The grammar
          expectations bind to the ``work`` class (the production
          17,886-entry universe the AAT dumps contain); the
          ``recovered_extra`` class (readable only by tolerant 7zz,
          rejected by every production reader including ABC's strict
          fallback) is scanned and reported SEPARATELY so no readable
          text is silently excluded from the study.

Adoption grammar
----------------
``classify_tokens`` below is the normative reference model of the
classifier's per-line two-pass algorithm over an already-tokenized
marker sequence; ``classify_line`` tokenizes a text line and delegates.
The split lets the delta audit derive expected adoptions/reasons
INDEPENDENTLY from a baseline AAT dump's raw marker nodes (review P5-4)
while this instrument derives them from source text. The Rust classifier
must mirror the model test-for-test.

Pass 1 — one GLOBAL nesting stack over the line's tokens in order:
  * open K: if a frame of construct K is already on the stack, mark K
    invalid for this line (same-construct reopen); push regardless.
  * close K with empty stack: mark K invalid (orphan close).
  * close K with top-of-stack K: pop; record a candidate pair.
  * close K with top-of-stack J != K: improper interleaving; mark BOTH
    J and K invalid; nothing is popped.
  * end of line: every frame still on the stack marks its construct
    invalid (orphan open).
Pass 2 — a candidate pair is adopted iff its construct was not marked
invalid; every marker of an invalid construct stays raw (rollback for
candidates). Invalidation is construct-scoped per line.

Usage:
    bare-toggle-placement.py --corpus <store-path> [--expected-works N]
        [--out summary.json] [--jobs N]
"""

from __future__ import annotations

import argparse
import collections
import concurrent.futures
import json
import pathlib
import re
import sys
from dataclasses import dataclass, field
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

SCHEMA_VERSION = "bare-toggle-placement-v3"
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
    "|".join(re.escape(token) for pair in TOKENS.values() for token in pair)
)
TOKEN_KIND: dict[str, tuple[str, str]] = {
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


def classify_tokens(tokens: list[tuple[str, str]]) -> LineOutcome:
    """Normative two-pass model over a line's (construct, kind) tokens."""
    outcome = LineOutcome()
    outcome.total_markers = len(tokens)
    if not tokens:
        return outcome

    # Pass 1: global nesting stack.
    stack: list[str] = []
    candidates: list[tuple[str, bool]] = []  # (construct, nested)
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


def tokenize_line(line: str) -> list[tuple[str, str]]:
    return [
        TOKEN_KIND[match.group(0)]
        for match in _TOKEN_ALTERNATION.finditer(line)
    ]


def classify_line(line: str) -> LineOutcome:
    return classify_tokens(tokenize_line(line))


def _empty_grammar_totals() -> dict[str, Any]:
    return {
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


def scan_text(text: str) -> tuple[dict[str, Any], dict[str, list[str]], dict[str, dict[str, int]]]:
    """Grammar totals + samples + form frequency for one work's text."""
    totals = _empty_grammar_totals()
    samples: dict[str, list[str]] = {"adopted": [], "declined": []}
    form_frequency: dict[str, dict[str, int]] = {}
    for construct, pattern in CONTEXT_PATTERNS.items():
        counts = collections.Counter(pattern.findall(text))
        if counts:
            form_frequency[construct] = dict(counts)
    if not any(token in text for token in TOKEN_KIND):
        return totals, samples, form_frequency
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
    return totals, samples, form_frequency


def process_candidate(corpus_root_str: str, entry_str: str) -> dict[str, Any]:
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
    totals, samples, form_frequency = scan_text(text)
    record.update(
        label=candidate.label,
        work_id=candidate.work_id,
        reader=candidate.reader,
        decoding="shift_jis/replace (BOM-gated utf-8 first)",
        sha256=sha256_hex(candidate.data),
        form_frequency=form_frequency,
        touched=totals["total_markers"] > 0,
    )
    if record["touched"]:
        record["grammar"] = totals
        record["samples"] = samples
    return record


def accumulate(records: list[dict[str, Any]]) -> tuple[dict[str, Any], dict[str, collections.Counter[str]]]:
    grammar_totals = _empty_grammar_totals()
    form_frequency: dict[str, collections.Counter[str]] = {
        c: collections.Counter() for c in CONSTRUCTS
    }
    for record in records:
        for construct, counts in record.get("form_frequency", {}).items():
            form_frequency[construct].update(counts)
        if not record.get("touched"):
            continue
        grammar = record["grammar"]
        for construct in CONSTRUCTS:
            for key in ("adopted_pairs", "orphan_open", "orphan_close", "reopen"):
                grammar_totals[key][construct] += grammar[key][construct]
        for key in (
            "interleave_events", "proper_nestings", "rollback_markers",
            "lines_with_markers", "invalid_lines", "mixed_lines", "total_markers",
        ):
            grammar_totals[key] += grammar[key]
    return grammar_totals, form_frequency


def touched_detail(records: list[dict[str, Any]]) -> list[dict[str, Any]]:
    return [
        {
            key: record.get(key)
            for key in (
                "work_id", "label", "reader", "decoding", "sha256",
                "grammar", "samples",
            )
        }
        for record in records
        if record.get("touched")
    ]


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--corpus", required=True, type=pathlib.Path)
    parser.add_argument("--expected-works", type=int, default=17886)
    parser.add_argument("--out", type=pathlib.Path)
    parser.add_argument("--jobs", type=int, default=1)
    args = parser.parse_args()

    candidates = discover_entries(args.corpus)
    if args.jobs <= 1:
        records = [
            process_candidate(str(args.corpus), str(entry)) for entry in candidates
        ]
    else:
        with concurrent.futures.ProcessPoolExecutor(max_workers=args.jobs) as pool:
            futures = [
                pool.submit(process_candidate, str(args.corpus), str(entry))
                for entry in candidates
            ]
            records = [future.result() for future in futures]

    by_class: dict[str, list[dict[str, Any]]] = {
        "work": [], "non_work": [], "recovered_extra": [], "unreadable": [],
    }
    for record in records:
        by_class[record["class"]].append(record)

    work_totals, work_forms = accumulate(by_class["work"])
    extra_totals, extra_forms = accumulate(by_class["recovered_extra"])

    summary = {
        "schema_version": SCHEMA_VERSION,
        "corpus": str(args.corpus),
        "reader": {
            "provenance": "reports/lib/corpus_reader.py (shared contract; "
            "windows-31j member names, local-header bypass, tolerant 7zz)",
        },
        "candidates_discovered": len(records),
        "candidate_classes": {k: len(v) for k, v in by_class.items()},
        "works_expected": args.expected_works,
        "works_scanned": len(by_class["work"]),
        "non_work_archives": [
            {"rel": r["rel"], "detail": r["detail"]} for r in by_class["non_work"]
        ],
        "recovered_extra": [
            {
                "rel": r["rel"], "label": r.get("label"),
                "sha256": r.get("sha256"), "reader": r.get("reader"),
                "touched": r.get("touched", False),
            }
            for r in by_class["recovered_extra"]
        ],
        "unreadable": [
            {"rel": r["rel"], "detail": r["detail"]} for r in by_class["unreadable"]
        ],
        "grammar_totals": work_totals,
        "recovered_extra_grammar_totals": extra_totals,
        "works_touched": sum(1 for r in by_class["work"] if r.get("touched")),
        "works_with_invalid_lines": sum(
            1
            for r in by_class["work"]
            if r.get("touched") and r["grammar"]["invalid_lines"]
        ),
        "form_frequency": {
            construct: [
                {"form": form, "count": count}
                for form, count in counter.most_common()
            ]
            for construct, counter in work_forms.items()
        },
        "recovered_extra_form_frequency": {
            construct: [
                {"form": form, "count": count}
                for form, count in counter.most_common()
            ]
            for construct, counter in extra_forms.items()
        },
        "touched_entries": touched_detail(by_class["work"]),
    }

    payload = json.dumps(summary, ensure_ascii=False, indent=1) + "\n"
    if args.out:
        args.out.write_text(payload, encoding="utf-8")
    else:
        sys.stdout.write(payload)

    if len(by_class["work"]) != args.expected_works:
        print(
            f"FAIL: {len(by_class['work'])} readable works, expected "
            f"{args.expected_works} (classes: {summary['candidate_classes']})",
            file=sys.stderr,
        )
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
