#!/usr/bin/env python3
"""Exploratory inventory of the pinned Aozora corpus. NOT an admission oracle.

Emits one record per archive with its source size and notation volume, so that
selections derived from it — see `tools/corpus_tail_set.py` — are reproducible
rather than sampled. Reads archives straight out of a pinned aozorabunko
checkout: the inventory is a function of the snapshot and nothing else, and it
depends on no ingested corpus.

WHAT THIS IS NOT
----------------
This deliberately does NOT implement ABC's admission semantics. Admission
decodes legacy entry names, applies semantic primary-text selection, rejects
archives with zero or several primary members, enforces archive limits and
collision rules, and recovers some trailing-garbage archives. This tool takes
the name-sorted first `.txt` member with stock `zipfile` and skips what that
cannot open.

It is therefore an approximation, suitable for exploratory measurement and for
proposing a stress selection. Governed qualification membership must be
projected from the authoritative source-bundle census
(`abc.tools.source-bundle-report`) and from publication's own source-selection
projection, never from this file. Two approximations of one population is
exactly the drift this repository binds hashes to prevent. See
`docs/superpowers/specs/2026-07-26-parser-rq-corpus-tiering-design.md`.

DETERMINISM
-----------
Directory traversal, file order, emitted works, and the skipped list are all
sorted, so the output is byte-identical across filesystems with differing
natural traversal order — not merely across repeated runs on one host.

Usage: python3 tools/corpus_inventory.py <aozorabunko-root> <output.json>
"""

from __future__ import annotations

import json
import os
import sys
import zipfile

# The name-sorted first `.txt` member. See "WHAT THIS IS NOT" above: admission
# selects a *semantic* primary text member and rejects ambiguity outright, so
# this agrees with it only where an archive holds exactly one text member.
TEXT_SUFFIX = ".txt"

# Aozora text is Shift_JIS-family; `cp932` covers the NEC/IBM extensions these
# files use. Decoding is lossy-tolerant because this counts notation markers
# rather than reproducing text, and U+FFFD cannot be mistaken for a marker.
WORK_ENCODING = "cp932"

# Every gaiji marker `※［＃…］` contains the general annotation opener `［＃`, so a
# bare count of the opener counts each gaiji twice over. Both are emitted: the
# raw opener count and the count with gaiji removed. Measured over the pinned
# snapshot the gaiji share is 8.9% of raw openers, and excluding them does not
# change the top-5 annotation ranking -- but the two are different metrics and
# could select different works on another snapshot.
GAIJI_MARKER = "※［＃"
ANNOT_MARKER = "［＃"
RUBY_MARKER = "《"


def notation_volume(text: str) -> dict[str, int]:
    """Occurrence counts of the constructs that drive parser cost.

    These are volumes, not densities: no normalization by length is applied,
    because total work is what the resource envelope responds to. A per-byte
    density would select short, dense works instead, which is a different
    question and would select a different tail.

    Openers are counted rather than matched pairs: an unterminated delimiter is
    exactly the input a stress tier exists to carry, so it must not be excluded
    from the count that would select it.
    """
    lines = text.split("\n")
    gaiji = text.count(GAIJI_MARKER)
    annot_markers = text.count(ANNOT_MARKER)
    return {
        "chars": len(text),
        "ruby": text.count(RUBY_MARKER),
        "gaiji": gaiji,
        "annot_markers": annot_markers,
        "annot_non_gaiji": annot_markers - gaiji,
        "lines": len(lines),
        "max_line": max((len(line) for line in lines), default=0),
    }


def inventory(root: str) -> tuple[list[dict[str, object]], list[dict[str, str]]]:
    rows: list[dict[str, object]] = []
    skipped: list[dict[str, str]] = []
    for dirpath, dirnames, names in os.walk(os.path.join(root, "cards")):
        # os.walk yields directories in filesystem order; sorting in place makes
        # traversal itself deterministic rather than relying on the final sort.
        dirnames.sort()
        for name in sorted(names):
            if not name.endswith(".zip"):
                continue
            path = os.path.join(dirpath, name)
            relative = os.path.relpath(path, root)
            try:
                with zipfile.ZipFile(path) as archive:
                    members = sorted(
                        n for n in archive.namelist() if n.lower().endswith(TEXT_SUFFIX)
                    )
                    if not members:
                        continue
                    body = archive.read(members[0])
            except (zipfile.BadZipFile, OSError) as error:
                # Reported, never treated as an empty work. ABC's admission path
                # recovers several of these; this tool does not reimplement that
                # recovery, so its totals are a lower bound on the corpus.
                skipped.append({"archive": relative, "reason": type(error).__name__})
                continue
            row: dict[str, object] = {
                "archive": relative,
                "member": members[0],
                "bytes": len(body),
            }
            row.update(notation_volume(body.decode(WORK_ENCODING, errors="replace")))
            rows.append(row)
    rows.sort(key=lambda r: str(r["archive"]))
    skipped.sort(key=lambda r: r["archive"])
    return rows, skipped


def main(root: str, out_path: str) -> int:
    rows, skipped = inventory(root)
    if not rows:
        print(f"no archives with a text member under {root}/cards", file=sys.stderr)
        return 1
    with open(out_path, "w", encoding="utf-8") as handle:
        json.dump(
            {
                "measurement_construction": "abc-corpus-exploratory-inventory-v1",
                "authoritative": False,
                "works": rows,
                "skipped": skipped,
            },
            handle,
            ensure_ascii=False,
        )

    sizes = sorted(int(r["bytes"]) for r in rows)
    total = sum(sizes)
    print("NOT an admission oracle; see module docstring before governing on this")
    print(f"archives with a text member : {len(rows)}")
    print(f"archives skipped            : {len(skipped)}")
    for entry in skipped:
        print(f"    {entry['reason']:16s} {entry['archive']}")
    print(f"total source bytes          : {total:,} ({total / 1e6:.0f} MB)")
    print(f"mean / median               : {total // len(rows):,} / {sizes[len(sizes) // 2]:,}")
    print(f"largest text member         : {sizes[-1]:,}")
    for key in ("ruby", "gaiji", "annot_markers", "annot_non_gaiji", "max_line", "lines"):
        print(f"max {key:15s}       : {max(int(r[key]) for r in rows):,}")
    return 0


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(__doc__, file=sys.stderr)
        raise SystemExit(2)
    raise SystemExit(main(sys.argv[1], sys.argv[2]))
