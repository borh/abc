#!/usr/bin/env python3
"""Parity/support audit: which constructs each parser actually represents.

Corpus coverage (§4.7) scores only 9 high-mass constructs. The source-authority
denominators (`2026-07-08-corpus-adapter-fidelity.summary.json`) list 21. This audit
scores EVERY scoreable source construct against every adapter, using signatures
derived from the full-corpus vocabulary audit, to surface *parity gaps* — constructs a
parser drops entirely while others represent them. Each previous audit pass (ruby
normalization, boten sesame_dot, indentation harmonization, gaiji) found something;
this is the systematic completeness sweep.

Coverage per cell = min(adapter node count / source occurrences, 1.0). A cell < 0.05
is a DROP (parity gap); 'folded' marks a construct kept but not distinctly typed.

Reads denominators from argv[1] (the fidelity summary). Emits JSON to stdout and a
human matrix to stderr.
"""
import collections
import glob
import json
import os
import sys

# aozora points at the RE-PINNED dump if present, else the prior pin.
_repin = "/db/ab-validator/aat-corpus/aozora-full-repin-1a4f864/aat/aozora-adapter/*.json"
_aozora = _repin if glob.glob(_repin) else \
    "/db/ab-validator/aat-corpus/aozora-full-20260705T015007Z/aat/aozora-adapter/*.json"
AAT_GLOBS = {
    "aozora": _aozora,
    "aozora2": "/db/ab-validator/aat-corpus/aozora2-full-20260705T083650Z-layout-fix5/aat/aozora2-adapter/*.json",
    "aozora-rs": "/db/ab-validator/fidelity-corpus/aozora-rs/aat/aozora-rs-adapter/*.json",
    "aozora2html": "/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter/*.json",
    "aozora-epub3": "/db/ab-validator/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter/*.json",
}
ORDER = ["aozora", "aozora2", "aozora-rs", "aozora2html", "aozora-epub3"]

# Signatures = the UNION of each adapter's real encoding for the construct, from the
# full-corpus vocabulary audit (2026-07-08). Constructs beyond the tracked-9 are the
# audit's focus. Encodings are disjoint across adapters, so the union credits each for
# its own form. `note` records the mapping rationale / known folds.
SIG = {
    # --- the 9 already in the weighted metric (for reference) ---
    "ruby.basic": {"kinds": {"ruby"}, "markers": {"ruby"}},
    "decoration.boten": {"styles": {"boten", "bouten", "sesame_dot", "white_circle", "black_circle",
                         "white_up-pointing_triangle", "black_up-pointing_triangle", "bullseye",
                         "saltire", "white_circle_after"}, "markers": {"boten", "bouten"}},
    "decoration.bousen": {"styles": {"bousen", "bosen", "underline_solid", "overline_solid", "underline_wave"},
                          "markers": {"bousen", "bosen"}, "note": "aozora folds into `emphasis` marker"},
    "heading.basic": {"kinds": {"heading"}, "styles": {"unmapped-h3", "unmapped-h4", "unmapped-h5", "midashi"},
                      "markers": {"heading", "headingHint"}},
    "gaiji.marker": {"kinds": {"gaiji"}, "markers": {"gaiji"}},
    "decoration.font_size": {"kinds": {"font_size"}, "markers": {"font_size", "lineFontSize"}},
    "layout.tcy": {"kinds": {"tcy"}, "markers": {"tcy", "combineUpright"}},
    "figure.image_inline": {"kinds": {"figure"}, "markers": {"figure", "illustration"}},
    "indentation.jisage_block": {"kinds": {"jisage_block"}, "styles": {"jisage_line"}, "markers": {"containerOpen"}},
    # --- UNTRACKED constructs this audit adds (the parity surface) ---
    "indentation.chitsuki": {"styles": {"chitsuki"}, "markers": {"alignEnd"},
                             "note": "地付き flush-bottom; aozora=alignEnd marker, aozora2=chitsuki style"},
    "indentation.burasage": {"styles": {"burasage"}, "markers": {"burasage"},
                             "note": "ぶら下げ hanging indent"},
    "indentation.jizume": {"styles": {"jizume"}, "markers": {"jizume", "lineJizume"},
                           "note": "字詰め chars-per-line"},
    "decoration.bold_italic": {"styles": {"bold", "italic"}, "markers": {"lineBold", "lineItalic"},
                               "note": "aozora emits only lineBold (~drops bold/italic)"},
    "warichu.basic": {"kinds": {"warigaki"}, "styles": {"warichu", "warigaki"}, "markers": {"warigaki", "marginNote"},
                      "note": "割注 inline double-line annotation"},
    "layout.yokogumi": {"kinds": {"yokogumi", "yokogumi_block"}, "styles": {"yokogumi"},
                        "note": "横組 horizontal-in-vertical"},
}
# constructs emitted by SOME adapters but with NO source denominator in the summary
# (can't be scored, reported qualitatively): keigakomi (罫囲み box), accent.diacritic (denom 0).
UNSCOREABLE_NOTE = {
    "decoration.keigakomi": "aozora-rs 200 / a2html 420 / epub3 190(block); no source denominator",
    "accent.diacritic": "source denominator is 0",
    "iteration.kunoji": "emitted as a gaiji node (subsumed in gaiji.marker); denom 10,701",
}


def match(node, sig):
    k = node.get("kind")
    if k in sig.get("kinds", ()):
        return True
    if k == "style" and node.get("style_type") in sig.get("styles", ()):
        return True
    m = node.get("x-source-marker-kind")
    return bool(m) and m in sig.get("markers", ())


def count_dir(pattern):
    counts = collections.Counter()
    n = 0
    for path in glob.glob(pattern):
        try:
            blocks = json.load(open(path)).get("blocks", [])
        except Exception:
            continue
        n += 1
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
    return counts, n


def main():
    summary = json.load(open(sys.argv[1]))
    denom = {r["construct"]: r["denominator"] for r in summary["classified"]}
    # gaiji.marker is the total gaiji denominator (jis_code+unicode+un_embed sum into it)
    per = {}
    for a in ORDER:
        per[a], nfiles = count_dir(AAT_GLOBS[a])
        print(f"{a}: scanned {nfiles} files ({'REPIN' if 'repin' in AAT_GLOBS[a] else 'prior'} aozora dump)"
              if a == "aozora" else f"{a}: scanned {nfiles} files", file=sys.stderr)

    rows = []
    for sid, sig in SIG.items():
        d = denom.get(sid)
        row = {"construct": sid, "denominator": d, "note": sig.get("note"), "adapters": {}}
        for a in ORDER:
            num = per[a].get(sid, 0)
            rate = (num / d) if d else None
            status = "n/a"
            if rate is not None:
                status = "drop" if rate < 0.05 else ("folded" if rate < 0.60 else "ok")
            row["adapters"][a] = {"num": num, "rate": round(rate, 3) if rate is not None else None, "status": status}
        rows.append(row)

    out = {"schema_version": 1, "aozora_dump": AAT_GLOBS["aozora"],
           "constructs": rows, "unscoreable": UNSCOREABLE_NOTE}
    json.dump(out, sys.stdout, ensure_ascii=False, indent=1)

    # human matrix
    print(f"\n{'construct':26}{'denom':>8}  " + "".join(f"{a[:9]:>10}" for a in ORDER), file=sys.stderr)
    for r in rows:
        cells = "".join(
            f"{(str(r['adapters'][a]['rate']) + ('✗' if r['adapters'][a]['status']=='drop' else '')):>10}"
            for a in ORDER)
        print(f"{r['construct']:26}{(r['denominator'] or 0):>8}  {cells}", file=sys.stderr)
    print("\n✗ = drop (rate<0.05, parity gap). Unscoreable (no denom): "
          + "; ".join(UNSCOREABLE_NOTE), file=sys.stderr)


if __name__ == "__main__":
    main()
