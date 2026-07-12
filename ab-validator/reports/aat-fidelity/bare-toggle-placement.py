#!/usr/bin/env python3
"""Placement attribution for bare-toggle markers on the pinned corpus.

Scans every work in the pinned aozorabunko-corpus derivation for the exact
bare-toggle tokens (which cannot substring-match the verbose ここから/ここで
block forms or the ［＃「…」は…］ inline-attribute forms, because both carry
intervening characters between ＃ and the construct token):

    yokogumi:  open ［＃横組み］    close ［＃横組み終わり］
    keigakomi: open ［＃罫囲み］    close ［＃罫囲み終わり］

and reports, per construct:

  - form frequency of every ［＃…横組み…］ / ［＃…罫囲み…］ marker (context
    table, so the bare-token counts can be located inside the full marker
    vocabulary),
  - line placement of each bare token: line_isolated (the stripped line is
    exactly the marker) vs midline,
  - pairing shape under in-document-order stack pairing: same_line_pairs,
    cross_line_pairs, unpaired_open, unpaired_close,
  - work counts, plus bounded samples of same-line pair lines and unpaired
    works.

Corpus layout follows crates/ab-index/src/index.rs collect_source_files:
each work is the first non-README ``.txt`` member of a per-work zip under
``cards/<id>/files/*.zip``, Shift_JIS-decoded (BOM-gated UTF-8 first).
Unreadable zips are skipped with a count. Lines are split on ``\\r\\n``,
bare ``\\r``, and ``\\n`` (bare-CR sources exist in the corpus; see the
Phase 3 line-index lesson).

Usage:
    bare-toggle-placement.py --corpus /nix/store/…-aozorabunko-corpus
"""

from __future__ import annotations

import argparse
import collections
import json
import re
import sys
import zipfile
from pathlib import Path

TOKENS = {
    "yokogumi": ("［＃横組み］", "［＃横組み終わり］"),
    "keigakomi": ("［＃罫囲み］", "［＃罫囲み終わり］"),
}
CONTEXT_PATTERNS = {
    "yokogumi": re.compile(r"［＃[^］]*横組み[^］]*］"),
    "keigakomi": re.compile(r"［＃[^］]*罫囲み[^］]*］"),
}
LINE_SPLIT = re.compile(r"\r\n|\r|\n")
SAMPLE_LIMIT = 8


def decode_source(data: bytes) -> str:
    if data.startswith(b"\xef\xbb\xbf"):
        try:
            return data.decode("utf-8-sig")
        except UnicodeDecodeError:
            pass
    return data.decode("shift_jis", errors="replace")


def first_text_member(zpath: Path) -> bytes | None:
    with zipfile.ZipFile(zpath) as z:
        for name in z.namelist():
            if name.lower().endswith(".txt") and "readme" not in name.lower():
                return z.read(name)
    return None


def new_stats() -> dict[str, int]:
    return dict(
        open_total=0,
        close_total=0,
        open_line_isolated=0,
        close_line_isolated=0,
        open_midline=0,
        close_midline=0,
        same_line_pairs=0,
        cross_line_pairs=0,
        unpaired_open=0,
        unpaired_close=0,
        works=0,
        works_with_unpaired=0,
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--corpus", required=True, type=Path)
    args = parser.parse_args()

    stats = {construct: new_stats() for construct in TOKENS}
    form_frequency: dict[str, collections.Counter[str]] = {
        construct: collections.Counter() for construct in TOKENS
    }
    samples: dict[str, dict[str, list]] = {
        construct: {"same_line": [], "unpaired_works": []} for construct in TOKENS
    }
    works_scanned = 0
    zips_unreadable = 0

    for zpath in sorted(args.corpus.glob("cards/*/files/*.zip")):
        try:
            data = first_text_member(zpath)
        except Exception:
            zips_unreadable += 1
            continue
        if data is None:
            continue
        works_scanned += 1
        text = decode_source(data)
        for construct, pattern in CONTEXT_PATTERNS.items():
            for form in pattern.findall(text):
                form_frequency[construct][form] += 1
        lines = LINE_SPLIT.split(text)
        for construct, (open_token, close_token) in TOKENS.items():
            if open_token not in text:
                continue
            construct_stats = stats[construct]
            token_pattern = re.compile(
                re.escape(open_token) + "|" + re.escape(close_token)
            )
            occurrences: list[tuple[int, str]] = []
            for line_number, line in enumerate(lines):
                open_count = line.count(open_token)
                close_count = line.count(close_token)
                if not open_count and not close_count:
                    continue
                stripped = line.strip()
                for _ in range(open_count):
                    construct_stats["open_total"] += 1
                    if stripped == open_token:
                        construct_stats["open_line_isolated"] += 1
                    else:
                        construct_stats["open_midline"] += 1
                for _ in range(close_count):
                    construct_stats["close_total"] += 1
                    if stripped == close_token:
                        construct_stats["close_line_isolated"] += 1
                    else:
                        construct_stats["close_midline"] += 1
                for match in token_pattern.finditer(line):
                    kind = "open" if match.group(0) == open_token else "close"
                    occurrences.append((line_number, kind))
                    if (
                        kind == "open"
                        and close_token in line
                        and len(samples[construct]["same_line"]) < SAMPLE_LIMIT
                    ):
                        samples[construct]["same_line"].append(stripped[:120])
            if not occurrences:
                continue
            construct_stats["works"] += 1
            open_stack: list[int] = []
            unpaired_close = 0
            for line_number, kind in occurrences:
                if kind == "open":
                    open_stack.append(line_number)
                elif open_stack:
                    opened_at = open_stack.pop()
                    key = (
                        "same_line_pairs"
                        if opened_at == line_number
                        else "cross_line_pairs"
                    )
                    construct_stats[key] += 1
                else:
                    unpaired_close += 1
            construct_stats["unpaired_open"] += len(open_stack)
            construct_stats["unpaired_close"] += unpaired_close
            if open_stack or unpaired_close:
                construct_stats["works_with_unpaired"] += 1
                if len(samples[construct]["unpaired_works"]) < SAMPLE_LIMIT:
                    samples[construct]["unpaired_works"].append(
                        {
                            "work": zpath.parent.parent.name,
                            "opens_left": len(open_stack),
                            "closes_left": unpaired_close,
                        }
                    )

    result = {
        "corpus": str(args.corpus),
        "works_scanned": works_scanned,
        "zips_unreadable": zips_unreadable,
        "stats": stats,
        "form_frequency": {
            construct: [
                {"form": form, "count": count}
                for form, count in counter.most_common()
            ]
            for construct, counter in form_frequency.items()
        },
        "samples": samples,
    }
    json.dump(result, sys.stdout, ensure_ascii=False, indent=1)
    print()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
