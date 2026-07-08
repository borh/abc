#!/usr/bin/env python3
"""Corpus per-construct adapter fidelity classifier.

Numerator = faithful typed AAT nodes matching a per-construct predicate, counted
over the full-corpus AAT blocks (per adapter). Denominator = source-authority
occurrences. Classify preserved/lossy/dropped, coupling-consistent with parser
recognition. Emits ClassifierFindings JSON for ab-coverage-merge.

Only CONFIDENT predicates are emitted; ambiguous constructs are reported and skipped.
"""

import json
import glob
import sys
from collections import defaultdict
from pathlib import Path

SP = Path(sys.argv[1])
ROOT = Path(sys.argv[2])
inputs = json.load(open(SP / "build-inputs.json"))


# construct -> predicate(node dict) -> bool.  Only confidently-distinguishable constructs.
def has(n, f):
    v = n.get(f)
    return bool(v) and v != ""


P = {
    "ruby.basic": lambda n: n.get("kind") == "ruby",
    "gaiji.marker": lambda n: n.get("kind") == "gaiji",
    "gaiji.jis_code": lambda n: n.get("kind") == "gaiji" and has(n, "jis_code"),
    "gaiji.unicode_codepoint": lambda n: (
        n.get("kind") == "gaiji" and has(n, "resolved") and not has(n, "jis_code")
    ),
    "gaiji.un_embed": lambda n: (
        n.get("kind") == "gaiji"
        and has(n, "description")
        and not has(n, "jis_code")
        and not has(n, "resolved")
    ),
    "iteration.kunoji": lambda n: (
        n.get("kind") == "gaiji"
        and ("くの字" in (n.get("description") or "") or "〳" in (n.get("resolved") or ""))
    ),
    "decoration.boten": lambda n: n.get("kind") == "style" and n.get("style_type") == "boten",
    "decoration.bousen": lambda n: n.get("kind") == "style" and n.get("style_type") == "bousen",
    "decoration.bold_italic": lambda n: (
        n.get("kind") == "style" and n.get("style_type") in ("bold", "italic")
    ),
    "indentation.chitsuki": lambda n: (
        n.get("kind") == "style" and n.get("style_type") == "chitsuki"
    ),
    "indentation.burasage": lambda n: (
        n.get("kind") == "style" and n.get("style_type") == "burasage"
    ),
    "indentation.jizume": lambda n: n.get("kind") == "style" and n.get("style_type") == "jizume",
    "indentation.jisage_oneline": lambda n: (
        n.get("kind") == "style" and n.get("style_type") == "jisage_line"
    ),
    "indentation.jisage_block": lambda n: n.get("kind") == "jisage_block",
    "heading.basic": lambda n: n.get("kind") == "heading",
    "layout.tcy": lambda n: n.get("kind") == "tcy",
    "layout.yokogumi": lambda n: n.get("kind") in ("yokogumi", "yokogumi_block"),
    "decoration.font_size": lambda n: n.get("kind") == "font_size",
    "figure.image_inline": lambda n: n.get("kind") == "figure",
    "warichu.basic": lambda n: n.get("kind") == "warigaki",
    "accent.diacritic": lambda n: n.get("kind") == "accent",
}
# constructs deliberately NOT classified (ambiguous / not distinctly emitted / raw)
SKIP_REASON = {
    "emphasis.basic": "same node as decoration.boten (style_type=boten); indistinguishable",
    "kunten.kaeriten": "no distinct style_type/kind emitted by natives",
    "reference.frontref": "no distinct node signature",
    "decoration.keigakomi": "no keigakomi node observed in native blocks",
    "caption.inline": "no caption node observed in native blocks",
    "caption.block": "no caption_block node observed in native blocks",
    "figure.image_caption": "shares 'figure' kind with figure.image_inline",
    "heading.dogyo": "shares 'heading' kind; level not construct-distinguishing",
    "heading.mado": "shares 'heading' kind",
    "annotation.chuuki": "matrix maps to 'ruby'; not separable",
    "annotation.bouki": "matrix maps to 'ruby'; not separable",
    "kunten.okurigana": "matrix maps to 'ruby'; not separable",
    "indentation.basic": "overlaps indentation.jisage_block / jisage_line",
    "decoration.direction_override": "no distinct node signature",
    "decoration.typeface": "ambiguous style/raw",
}

counts = {ad: defaultdict(int) for ad in ("aozora2", "aozora-rs")}


def walk(v, ad):
    if isinstance(v, dict):
        for sid, pred in P.items():
            try:
                if pred(v):
                    counts[ad][sid] += 1
            except Exception:
                pass
        for x in v.values():
            walk(x, ad)
    elif isinstance(v, list):
        for x in v:
            walk(x, ad)


for ad in ("aozora2", "aozora-rs"):
    n = 0
    for f in glob.glob(f"{ROOT}/{ad}/aat/*/*.json"):
        try:
            walk(json.load(open(f)).get("blocks", []), ad)
            n += 1
        except Exception:
            pass
    print(f"{ad}: walked {n} files", file=sys.stderr)


def classify(rate, recognition):
    # coupling: unrecognised => synthesised|not_applicable; aborts => not_applicable
    if rate is None:
        return None
    if rate < 0.05:
        return "dropped" if recognition != "aborts" else "not_applicable"
    base = "preserved" if rate >= 0.60 else "lossy"
    if recognition == "unrecognised":
        return "synthesised"  # correct output but not native parse
    if recognition == "aborts":
        return "not_applicable"
    return base


report = []
for sid, pred in P.items():
    inp = inputs.get(sid, {})
    den = inp.get("occ", 0)
    row = {"construct": sid, "denominator": den, "adapters": {}}
    for ad in ("aozora2", "aozora-rs"):
        num = counts[ad][sid]
        rate = (num / den) if den else None
        rec = (inp.get("recognition") or {}).get(ad)
        cls = classify(rate, rec)
        row["adapters"][ad] = {
            "num": num,
            "rate": round(rate, 3) if rate is not None else None,
            "recognition": rec,
            "was": (inp.get("fidelity") or {}).get(ad),
            "now": cls,
        }
    report.append(row)
report.sort(key=lambda r: -r["denominator"])
json.dump(
    {"classified": report, "skipped": SKIP_REASON},
    open(SP / "corpus-fidelity.json", "w"),
    ensure_ascii=False,
    indent=1,
)

# sanity print
print(
    f"\n{'construct':26} {'den':>8}  aozora2(num rate was->now)      aozora-rs(num rate was->now)"
)
for r in report:
    a2 = r["adapters"]["aozora2"]
    rs = r["adapters"]["aozora-rs"]
    print(
        f"{r['construct']:26} {r['denominator']:>8}  a2:{a2['num']:>7} {str(a2['rate']):>6} {str(a2['was']):>10}->{str(a2['now'])!s:<11} rs:{rs['num']:>7} {str(rs['rate']):>6} {str(rs['was'])}->{rs['now']}"
    )
