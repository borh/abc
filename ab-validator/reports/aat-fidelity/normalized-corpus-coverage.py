#!/usr/bin/env python3
"""Normalized frequency-weighted corpus coverage, per parser.

Answers: "how much of the markup that ACTUALLY occurs in the Aozora corpus can
each parser parse/represent?" — weighted by real construct frequency.

Key fix over the earlier classifier: recognition is NORMALIZED across adapter
representation variants, so the same construct counts the same regardless of how
an adapter encodes it. AAT *ought* to normalize these at the schema level (free
`style_type` string, typed-node vs raw+`x-source-marker-kind`); until it does, we
normalize here:
  - canonical style_type spellings collapse (boten==bouten, bousen==bosen)
  - a construct counts whether emitted as a typed `kind` node OR a `raw` node
    carrying `x-source-marker-kind` for it.

Numerators: normalized recognitions over each adapter's full-corpus AAT.
Denominators: source-occurrence counts (from the prior corpus-fidelity summary).
Only constructs whose normalized predicate aligns 1:1 with the denominator are
scored (fine sub-constructs that depend on adapter-dropped fields are excluded).
"""
import collections
import glob
import json
import sys
from pathlib import Path

# adapter label -> its full-corpus AAT glob
AAT_GLOBS = {
    "aozora": "/db/ab-validator/aat-corpus/aozora-full-20260705T015007Z/aat/aozora-adapter/*.json",
    "aozora2": "/db/ab-validator/aat-corpus/aozora2-full-20260705T083650Z-layout-fix5/aat/aozora2-adapter/*.json",
    "aozora-rs": "/db/ab-validator/fidelity-corpus/aozora-rs/aat/aozora-rs-adapter/*.json",
    "aozora2html": "/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter/*.json",
    "aozora-epub3": "/db/ab-validator/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter/*.json",
}


def styled(node, spellings):
    return node.get("kind") == "style" and node.get("style_type") in spellings


# construct -> normalized predicate over a single AAT node
NORM = {
    "ruby.basic": lambda n: n.get("kind") == "ruby" or n.get("x-source-marker-kind") == "ruby",
    "decoration.boten": lambda n: styled(n, {"boten", "bouten"})
        or n.get("x-source-marker-kind") in {"boten", "bouten"},
    "heading.basic": lambda n: n.get("kind") == "heading"
        or n.get("x-source-marker-kind") in {"heading", "headingHint"},
    "decoration.bousen": lambda n: styled(n, {"bousen", "bosen"})
        or n.get("x-source-marker-kind") in {"bousen", "bosen"},
    "decoration.font_size": lambda n: n.get("kind") == "font_size"
        or n.get("x-source-marker-kind") in {"lineFontSize", "font_size"},
    "layout.tcy": lambda n: n.get("kind") == "tcy"
        or n.get("x-source-marker-kind") in {"tcy", "combineUpright"},
    "figure.image_inline": lambda n: n.get("kind") == "figure"
        or n.get("x-source-marker-kind") in {"figure", "illustration"},
    # block indent: count the OPEN only (1 per occurrence) to align with denominator
    "indentation.jisage_block": lambda n: n.get("kind") == "jisage_block"
        or n.get("x-source-marker-kind") == "containerOpen",
}


def count_file(path, counts):
    try:
        blocks = json.load(open(path)).get("blocks", [])
    except Exception:
        return
    stack = [blocks]
    while stack:
        v = stack.pop()
        if isinstance(v, dict):
            for sid, pred in NORM.items():
                try:
                    if pred(v):
                        counts[sid] += 1
                except Exception:
                    pass
            stack.extend(v.values())
        elif isinstance(v, list):
            stack.extend(v)


def main():
    summary = json.load(open(sys.argv[1]))  # corpus-fidelity summary (for denominators)
    denom = {r["construct"]: r["denominator"] for r in summary["classified"]}
    out = {"schema_version": 1, "constructs": {}, "adapters": {}}
    for sid in NORM:
        out["constructs"][sid] = denom.get(sid)

    per_adapter = {}
    for label, pattern in AAT_GLOBS.items():
        files = glob.glob(pattern)
        counts = collections.Counter()
        for f in files:
            count_file(f, counts)
        per_adapter[label] = {"files": len(files), "counts": dict(counts)}
        print(f"{label}: {len(files)} files", file=sys.stderr)

    scored = [sid for sid in NORM if denom.get(sid)]
    total_occ = sum(denom[sid] for sid in scored)
    for label, data in per_adapter.items():
        rows = {}
        wsum = 0.0
        for sid in scored:
            d = denom[sid]
            num = data["counts"].get(sid, 0)
            rate = min(num / d, 1.0)  # cap: multiple nodes per occ shouldn't exceed 1
            rows[sid] = {"num": num, "denom": d, "rate": round(num / d, 3)}
            wsum += rate * d
        out["adapters"][label] = {
            "files": data["files"],
            "per_construct": rows,
            "frequency_weighted_coverage": round(wsum / total_occ, 3),
        }
    out["scored_constructs"] = scored
    out["total_weighted_occurrences"] = total_occ
    json.dump(out, sys.stdout, ensure_ascii=False, indent=1)


if __name__ == "__main__":
    main()
