#!/usr/bin/env python3
"""Fair, normalized, frequency-weighted corpus coverage per parser.

Answers "how much of the markup that ACTUALLY occurs in the Aozora corpus can each
parser parse/represent?" — weighted by real construct frequency.

FAIRNESS: recognition signatures are derived from a FULL-CORPUS vocabulary audit of
each adapter's real AAT output (kinds / style_types / x-source-marker-kinds), not
from guesses. Each construct's signature set is the UNION of every adapter's
encoding for it; the encodings are disjoint across adapters (ab-aozora uses only
markers; aozora2html uses HTML-flavoured style names like `sesame_dot`), so the
union credits each adapter for its own encoding without cross-contamination. Counts
are 1-per-occurrence (containers count the OPEN only). Rates are capped at 1.0 in
the weighted sum (some adapters emit >1 node per source occurrence).

Numerators: matches over each adapter's full-corpus AAT. Denominators: source
occurrences (prior corpus-fidelity summary). Every remaining 0.0 was verified in the
vocabulary audit to be a real gap (adapter emits no node for the construct), except
ab-aozora bousen which is `folded` (into a generic `emphasis` marker) — reported
distinctly.
"""

import argparse
import collections
import glob
import json
from pathlib import Path
import sys

REPORTS_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPORTS_ROOT / "lib"))
from fidelity_lock import load_lock, lock_aat_globs, lock_run_set_id  # noqa: E402


ORDER = ["aozora", "aozora2", "aozora-rs", "aozora2html", "aozora-epub3"]

# construct -> {"kinds":set, "styles":set(style_type), "markers":set(x-source-marker-kind)}
# derived from the full-corpus vocabulary audit (2026-07-08).
SIG = {
    "ruby.basic": {"kinds": {"ruby"}, "styles": set(), "markers": {"ruby"}},
    "decoration.boten": {
        "kinds": set(),
        "styles": {
            "boten",
            "bouten",
            "sesame_dot",
            "white_circle",
            "black_circle",
            "white_up-pointing_triangle",
            "black_up-pointing_triangle",
            "bullseye",
            "saltire",
            "white_circle_after",
        },
        "markers": {"boten", "bouten"},
    },
    "decoration.bousen": {  # ab-aozora folds bousen into a generic `emphasis` marker (see FOLDED)
        "kinds": set(),
        "styles": {"bousen", "bosen", "underline_solid", "overline_solid", "underline_wave"},
        "markers": {"bousen", "bosen"},
    },
    "heading.basic": {
        "kinds": {"heading"},
        # a2html: heading, level not canonicalised; aozora2 also emits `midashi` style.
        "styles": {
            "unmapped-h3",
            "unmapped-h4",
            "unmapped-h5",
            "midashi",
        },
        "markers": {"heading", "headingHint"},
    },
    # gaiji (外字): all gaiji regardless of resolution method. denom `gaiji.marker` is the
    # total (= jis_code + unicode + un_embed). aozora emits it as an x-source marker;
    # the natives as kind==gaiji. epub3 emits NONE (a real gap, previously invisible).
    "gaiji.marker": {"kinds": {"gaiji"}, "styles": set(), "markers": {"gaiji"}},
    "decoration.font_size": {
        "kinds": {"font_size"},
        "styles": set(),
        "markers": {"font_size", "lineFontSize"},
    },
    "layout.tcy": {"kinds": {"tcy"}, "styles": set(), "markers": {"tcy", "combineUpright"}},
    "figure.image_inline": {
        "kinds": {"figure"},
        "styles": set(),
        "markers": {"figure", "illustration"},
    },
    # jisage (字下げ) harmonised: block form (jisage_block kind / containerOpen marker) and
    # aozora2's per-line form (`jisage_line` style, which it uses INSTEAD of jisage_block in
    # many works) are the SAME source construct bucketed differently. Denominator is
    # jisage_block source occurrences (94,993; the +279 jisage_oneline is negligible).
    # Corpus-level cap absorbs aozora2/epub3 per-line over-emission.
    "indentation.jisage_block": {
        "kinds": {"jisage_block"},
        "styles": {"jisage_line"},
        "markers": {"containerOpen"},
    },
}
# cells that are represented-but-not-distinct (parser kept the span, lost the type)
FOLDED = {("aozora", "decoration.bousen"): "folded into generic `emphasis` marker"}


def match(node, sig):
    k = node.get("kind")
    if k in sig["kinds"]:
        return True
    if k == "style" and node.get("style_type") in sig["styles"]:
        return True
    m = node.get("x-source-marker-kind")
    return m in sig["markers"] if m else False


def count_file(path, counts):
    try:
        blocks = json.load(open(path)).get("blocks", [])
    except Exception:
        return
    stack = [blocks]
    while stack:
        v = stack.pop()
        if isinstance(v, dict):
            for sid, sig in SIG.items():
                if match(v, sig):
                    counts[sid] += 1
            stack.extend(v.values())
        elif isinstance(v, list):
            stack.extend(v)


def main():
    ap = argparse.ArgumentParser(description="§4.7 normalized, frequency-weighted corpus coverage")
    ap.add_argument("summary", help="fidelity summary JSON (denominators)")
    ap.add_argument("--lock", required=True, help="resolved fidelity lock (dump selection)")
    args = ap.parse_args()
    lock = load_lock(args.lock)
    aat_globs = lock_aat_globs(lock, order=ORDER)

    summary = json.load(open(args.summary))
    denom = {r["construct"]: r["denominator"] for r in summary["classified"]}
    scored = [sid for sid in SIG if denom.get(sid)]
    total = sum(denom[sid] for sid in scored)

    out = {
        "schema_version": 2,
        "aat_run_set_id": lock_run_set_id(lock),
        "aat_globs": aat_globs,
        "note": "fair union-signature normalization; see script docstring",
        "denominators": {s: denom[s] for s in scored},
        "total_weighted_occurrences": total,
        "adapters": {},
    }
    for label, pattern in aat_globs.items():
        files = glob.glob(pattern)
        counts = collections.Counter()
        for f in files:
            count_file(f, counts)
        rows, wsum = {}, 0.0
        for sid in scored:
            num, d = counts.get(sid, 0), denom[sid]
            capped = min(num / d, 1.0)
            rows[sid] = {"num": num, "rate": round(num / d, 3), "folded": FOLDED.get((label, sid))}
            wsum += capped * d
        out["adapters"][label] = {
            "files": len(files),
            "per_construct": rows,
            "frequency_weighted_coverage": round(wsum / total, 3),
        }
        print(f"{label}: {len(files)} files", file=sys.stderr)
    json.dump(out, sys.stdout, ensure_ascii=False, indent=1)


if __name__ == "__main__":
    main()
