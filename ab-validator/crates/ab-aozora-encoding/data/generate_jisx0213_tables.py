#!/usr/bin/env python3
"""Generate the two JIS X 0213 mencode tables from the Project X0213 table.

`jisx0213-2004-std.txt` is the only input. It carries its own grant, so the
generated tables inherit terms a redistributor can act on; the earlier tables
were filtered out of glibc's EUC-JISX0213 charmap and out of the standard's
Annex F, neither of which stated terms in the file.

The two outputs differ only in arity. A JIS X 0213 cell whose Unicode mapping
is a combining sequence cannot be a Rust `char`, so those 25 cells go to a
separate string-valued map and the rest to a char-valued one.

Run with `--check` to compare against the committed tables without writing.
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

DATA = Path(__file__).resolve().parent
SOURCE = DATA / "jisx0213-2004-std.txt"
SINGLE = DATA / "jisx0213-2004.tsv"
COMBO = DATA / "jisx0213-combo.tsv"

# `3-RRCC` is plane 1 and `4-RRCC` plane 2, both in GL coordinates: row and
# cell are the hex bytes less 0x20, so 3-2121 is plane 1 row 1 cell 1.
JIS_CELL = re.compile(r"^([34])-([0-9A-F]{2})([0-9A-F]{2})$")

# A compatibility cell records its halfwidth form in the mapping column and its
# fullwidth form in a note. Aozora text is set fullwidth, so the note wins where
# it exists. This is the one place the tables are a reading of the source rather
# than a transcription of it.
FULLWIDTH = re.compile(r"Fullwidth: U\+([0-9A-F]+)")

SINGLE_HEADER = """\
# JIS X 0213:2004 mencode table — single-codepoint cells only.
#
# Generated from jisx0213-2004-std.txt by generate_jisx0213_tables.py.
# Do not edit: regenerate instead.
#
# Terms: inherited from the source table, whose header grants free use,
# modification and distribution (Project X0213, earthian@tama.or.jp, I'O).
#
# Filtered to gaiji territory: plane 1 cells the source marks [2000] or [2004],
# which are the ones JIS X 0213 introduced over JIS X 0208 (第3水準), plus all
# plane 2 cells (第4水準). Cells whose mapping is a combining sequence are in
# jisx0213-combo.tsv.
#
# Format: plane<TAB>row<TAB>cell<TAB>codepoint_hex
"""

COMBO_HEADER = """\
# JIS X 0213:2004 multi-codepoint cells.
#
# Generated from jisx0213-2004-std.txt by generate_jisx0213_tables.py.
# Do not edit: regenerate instead.
#
# Terms: inherited from the source table, whose header grants free use,
# modification and distribution (Project X0213, earthian@tama.or.jp, I'O).
#
# The 25 plane 1 第3水準 cells whose canonical mapping is a Unicode combining
# sequence, a base character followed by a combining diacritic, rather than a
# single scalar. They cannot be represented as a single `char` and therefore
# live in a separate string-valued phf::Map. The lookup API stitches both maps
# together so callers get a uniform `&str` result.
#
# Format: plane<TAB>row<TAB>cell<TAB>comma-separated codepoint hex
# (e.g. `1\\t4\\t87\\t304B,309A` = か + 半濁点 = か゚)
"""

# The combining cells fall into five writing-system groups, and saying which is
# worth more to a reader than the cell coordinates are. The source table has no
# such grouping, so the labels are written here, keyed by the cell that opens
# each group.
COMBO_GROUPS = {
    "1\t4\t87": "半濁点付きひらがな (か゚ き゚ く゚ け゚ こ゚ — Ainu / Tōhoku notation)",
    "1\t5\t87": "半濁点付きカタカナ (カ゚ キ゚ ク゚ ケ゚ コ゚ セ゚ ツ゚ ト゚ — Ainu)",
    "1\t6\t88": "Katakana phonetic extensions: ㇷ゚ (small katakana pu)",
    "1\t11\t36": "拡張ラテン文字 + 結合濁点 (アクセント記号)",
    "1\t11\t69": "声調記号の組合せ (IPA tone)",
}


def read_cells() -> tuple[list[str], list[str]]:
    """Return the single-codepoint and combining-sequence rows, in source order."""
    single: list[str] = []
    combo: list[str] = []
    for line in SOURCE.read_text(encoding="utf-8").splitlines():
        if line.startswith("##") or not line.strip():
            continue
        fields = line.split("\t")
        if len(fields) < 2:
            continue
        cell = JIS_CELL.match(fields[0])
        if cell is None:
            continue
        note = "\t".join(fields[2:])
        plane = 1 if cell.group(1) == "3" else 2
        if plane == 1 and "[2000]" not in note and "[2004]" not in note:
            continue
        row = int(cell.group(2), 16) - 0x20
        column = int(cell.group(3), 16) - 0x20
        fullwidth = FULLWIDTH.search(note)
        if fullwidth is not None:
            single.append(f"{plane}\t{row}\t{column}\t{fullwidth.group(1).lstrip('0')}")
            continue
        mapping = fields[1].strip()
        if not mapping.startswith("U+"):
            continue
        points = mapping[2:].split("+")
        if len(points) == 1:
            single.append(f"{plane}\t{row}\t{column}\t{points[0].lstrip('0')}")
        else:
            # The combining table pads to four digits; the single table does not.
            combo.append(f"{plane}\t{row}\t{column}\t" + ",".join(p.zfill(4) for p in points))
    return single, combo


def render(header: str, rows: list[str]) -> str:
    return header + "\n".join(rows) + "\n"


def render_combo(rows: list[str]) -> str:
    """Render the combining table with a blank line and a label opening each group."""
    lines: list[str] = []
    for row in rows:
        label = COMBO_GROUPS.get(row.rsplit("\t", 1)[0])
        if label is not None:
            lines.append("")
            lines.append(f"# {label}")
        lines.append(row)
    return COMBO_HEADER + "\n".join(lines) + "\n"


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--check",
        action="store_true",
        help="compare against the committed tables instead of writing them",
    )
    args = parser.parse_args()

    single, combo = read_cells()
    outputs = [(SINGLE, render(SINGLE_HEADER, single)), (COMBO, render_combo(combo))]

    if not args.check:
        for path, text in outputs:
            path.write_text(text, encoding="utf-8")
        return 0

    stale = [path.name for path, text in outputs if path.read_text(encoding="utf-8") != text]
    if stale:
        print(f"stale generated tables: {', '.join(stale)}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
