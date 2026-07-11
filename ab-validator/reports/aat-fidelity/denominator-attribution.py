#!/usr/bin/env python3
"""Decompose keigakomi/yokogumi/jizume marker counts by form on the pinned corpus.

Forms counted per construct token (罫囲み / 横組み / 字詰め):
  block_start   ［＃ここから…<token>…］
  block_end     ［＃ここで…<token>…終わり…］ (or the token inside a ここで marker)
  inline_attr   ［＃「…」は…<token>…］  (per-part attribution form)
  other         any other ［＃…<token>…］ marker

Usage: denominator-attribution.py CORPUS_ROOT --summary-json OUT

CORPUS_ROOT layout note: the pinned `aozorabunko-corpus` nix derivation is a
website mirror, not a flat directory of plaintext — each work's Shift_JIS
text lives inside a per-work zip under `cards/<id>/files/*.zip` (a bare
`rglob("*.txt")` only turns up 8 stray plaintext files; everything else is
zipped). This scanner walks `cards/*/files/`, reads the Shift_JIS `.txt`
entry out of each zip (first non-README `.txt` member, mirroring
`ab-index`'s `zip_text_entries`/`collect_source_files` in
`crates/ab-index/src/index.rs`, which is what produced the
`works_scanned: 17886` figure this report reconciles against), plus any
bare `*.txt` sibling files, and decodes everything as Shift_JIS
(errors=replace).
"""

import argparse
import json
import pathlib
import re
import sys
import zipfile
from collections import Counter

TOKENS = {"keigakomi": "罫囲み", "yokogumi": "横組み", "jizume": "字詰め"}
MARKER_RE = re.compile(r"［＃[^］]*］")


def classify(marker, token):
    if token not in marker:
        return None
    if marker.startswith("［＃ここから"):
        return "block_start"
    if marker.startswith("［＃ここで"):
        return "block_end"
    if marker.startswith("［＃「"):
        return "inline_attr"
    return "other"


def is_text_entry(name: str) -> bool:
    return (
        not name.startswith("__MACOSX/")
        and not name.endswith("/")
        and name.lower().endswith(".txt")
        and pathlib.Path(name).name.lower() != "readme.txt"
    )


def iter_work_texts(corpus_root: pathlib.Path):
    """Yield (label, text) for every work source file under cards/*/files/."""
    for files_dir in sorted(corpus_root.glob("cards/*/files")):
        if not files_dir.is_dir():
            continue
        for path in sorted(files_dir.iterdir()):
            if not path.is_file():
                continue
            name_lower = path.name.lower()
            if name_lower.endswith(".zip"):
                try:
                    with zipfile.ZipFile(path) as zf:
                        entry = next((n for n in zf.namelist() if is_text_entry(n)), None)
                        if entry is None:
                            continue
                        data = zf.read(entry)
                except (zipfile.BadZipFile, KeyError) as exc:
                    print(f"warning: skipping unreadable zip {path}: {exc}", file=sys.stderr)
                    continue
                label = f"{path.relative_to(corpus_root)}::{entry}"
                yield label, data.decode("shift_jis", errors="replace")
            elif name_lower.endswith(".txt") and path.name.lower() != "readme.txt":
                label = str(path.relative_to(corpus_root))
                yield label, path.read_bytes().decode("shift_jis", errors="replace")


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("corpus_root")
    ap.add_argument("--summary-json", required=True)
    args = ap.parse_args()
    corpus_root = pathlib.Path(args.corpus_root)
    counts = {name: Counter() for name in TOKENS}
    works = {name: set() for name in TOKENS}
    files_scanned = 0
    for label, text in iter_work_texts(corpus_root):
        files_scanned += 1
        for marker in MARKER_RE.findall(text):
            for name, token in TOKENS.items():
                form = classify(marker, token)
                if form:
                    counts[name][form] += 1
                    works[name].add(label)
    if not files_scanned:
        print(f"ERROR: no work texts found under {corpus_root}", file=sys.stderr)
        return 2
    summary = {
        "files_scanned": files_scanned,
        "constructs": {
            name: {
                "forms": dict(counts[name]),
                "total": sum(counts[name].values()),
                "works": len(works[name]),
            }
            for name in TOKENS
        },
    }
    pathlib.Path(args.summary_json).write_text(
        json.dumps(summary, indent=2, ensure_ascii=False) + "\n"
    )
    print(json.dumps(summary, indent=2, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
