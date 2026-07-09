#!/usr/bin/env python3
"""Rebuild the corpus-adapter fidelity summary with pinned-corpus denominators.

Handoff §1 (full-nix denominator recompute). The prior summary
(`2026-07-08-corpus-adapter-fidelity.summary.json`) carried source-authority `occ`
denominators from the ORIGINAL LOCAL extraction. This regenerates them from a
source inventory run on the PINNED nix corpus (`ab-source-inventory`), so numerators
(already on the pinned/repin AAT dumps via the run-set) and denominators share one
corpus. Numerators for aozora2/aozora-rs are recomputed here from the run-set AAT
dirs with the SAME predicates as corpus-adapter-fidelity-classifier.py, keeping the
summary internally consistent.

Usage:
  rebuild-fidelity-summary.py <inventory.json> <old-summary.json> > new-summary.json
"""

import argparse
import glob
import json
import sys
from collections import defaultdict
from pathlib import Path

REPORTS_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPORTS_ROOT / "lib"))
from fidelity_lock import load_lock, lock_aat_dirs, lock_run_set_id  # noqa: E402


def has(n, f):
    v = n.get(f)
    return bool(v) and v != ""


# Predicates copied verbatim from corpus-adapter-fidelity-classifier.py (keep in sync).
# keigakomi ADDED: aozora-rs emits a typed `keigakomi` kind (full-corpus vocab audit),
# and a source-authority denominator now exists on the pinned corpus, so it moves from
# skipped -> classified.
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
    "decoration.keigakomi": lambda n: n.get("kind") in ("keigakomi", "keigakomi_block"),
    "decoration.font_size": lambda n: n.get("kind") == "font_size",
    "figure.image_inline": lambda n: n.get("kind") == "figure",
    "warichu.basic": lambda n: n.get("kind") == "warigaki",
    "accent.diacritic": lambda n: n.get("kind") == "accent",
}


def classify(rate, recognition):
    if rate is None:
        return None
    if rate < 0.05:
        return "dropped" if recognition != "aborts" else "not_applicable"
    base = "preserved" if rate >= 0.60 else "lossy"
    if recognition == "unrecognised":
        return "synthesised"
    if recognition == "aborts":
        return "not_applicable"
    return base


def walk(v, ad, counts):
    if isinstance(v, dict):
        for sid, pred in P.items():
            try:
                if pred(v):
                    counts[ad][sid] += 1
            except Exception:
                pass
        for x in v.values():
            walk(x, ad, counts)
    elif isinstance(v, list):
        for x in v:
            walk(x, ad, counts)


def main():
    ap = argparse.ArgumentParser(description="rebuild the fidelity summary (denominators)")
    ap.add_argument("inventory", help="source-inventory JSON")
    ap.add_argument("old_summary", help="prior fidelity summary JSON (carries metadata)")
    ap.add_argument("--lock", required=True, help="resolved fidelity lock (dump selection)")
    args = ap.parse_args()
    lock = load_lock(args.lock)

    inventory = json.load(open(args.inventory))
    old = json.load(open(args.old_summary))
    occ = {row_id: v["occurrences"] for row_id, v in inventory["rows"].items()}

    # recognition/was metadata per (adapter, construct) carried from the prior summary
    meta = {}
    for r in old["classified"]:
        for ad, cell in r["adapters"].items():
            meta[(ad, r["construct"])] = {
                "recognition": cell.get("recognition"),
                "was": cell.get("was"),
            }

    dirs = lock_aat_dirs(lock, order=["aozora2", "aozora-rs"])
    counts = {ad: defaultdict(int) for ad in ("aozora2", "aozora-rs")}
    for ad in ("aozora2", "aozora-rs"):
        n = 0
        for f in glob.glob(f"{dirs[ad]}/*.json"):
            try:
                walk(json.load(open(f)).get("blocks", []), ad, counts)
                n += 1
            except Exception:
                pass
        print(f"{ad}: walked {n} files ({dirs[ad]})", file=sys.stderr)

    report = []
    for sid in P:
        den = occ.get(sid, 0)
        row = {"construct": sid, "denominator": den, "adapters": {}}
        for ad in ("aozora2", "aozora-rs"):
            num = counts[ad][sid]
            rate = (num / den) if den else None
            m = meta.get((ad, sid), {})
            rec = m.get("recognition")
            row["adapters"][ad] = {
                "num": num,
                "rate": round(rate, 3) if rate is not None else None,
                "recognition": rec,
                "was": m.get("was"),
                "now": classify(rate, rec),
            }
        report.append(row)
    report.sort(key=lambda r: -r["denominator"])

    # keigakomi has left `skipped`; keep the remaining skip reasons.
    skipped = {k: v for k, v in old.get("skipped", {}).items() if k != "decoration.keigakomi"}

    out = {
        "schema_version": 1,
        "note": "denominators recomputed on the pinned nix corpus via ab-source-inventory "
        "(handoff §1); numerators from the run-set AAT dumps. See "
        "2026-07-09-full-nix-denominator-recompute.md.",
        "source_inventory": {
            "corpus": inventory.get("inputs", {}).get("corpus"),
            "works_scanned": inventory.get("works_scanned"),
            "markers_total": inventory.get("markers_total"),
        },
        "run_set_id": lock_run_set_id(lock),
        "classified": report,
        "skipped": skipped,
    }
    json.dump(out, sys.stdout, ensure_ascii=False, indent=1)


if __name__ == "__main__":
    main()
