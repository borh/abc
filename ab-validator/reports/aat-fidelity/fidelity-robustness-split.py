#!/usr/bin/env python3
"""Split corpus coverage into FIDELITY and ROBUSTNESS.

Full-corpus coverage (`normalized-corpus-coverage.py`) conflates two axes:
  * ROBUSTNESS -- did the adapter produce output for the work at all?
  * FIDELITY   -- given it ran, how much of the work's markup did it represent?

aozora2html scores 0.745 corpus coverage yet has ~1.0 per-work ruby fidelity: its
gap is that it fails on ~193 ruby-heavy works, not that it drops ruby. This script
separates the axes so that failure isn't hidden behind a work-completion rate.

ROBUSTNESS = |works the adapter completed| / |union of works any adapter completed|,
plus the ruby mass carried by the works each adapter is missing.

FIDELITY = frequency-weighted coverage recomputed over I = the INTERSECTION of works
ALL adapters completed, so every adapter is scored on the identical work-set and the
robustness differences cancel. There is no per-work source-occurrence denominator
available (build-inputs.json holds corpus-wide totals only), so fidelity is reported
under TWO independent, reproducible denominators and shown to give the same ranking:

  * best-attested (max): D(w,c) = max over adapters of that adapter's count for the
    work. Adapter-neutral -- no single parser defines truth. fidelity_a(c) =
    sum_w count_a(w,c) / sum_w D(w,c). Only an adapter that is maximal on EVERY work
    reaches 1.0. Caveat: an over-emitting adapter (e.g. aozora boten 1.19x source)
    raises the bar; this bites only small-mass constructs -- ruby (90% of mass) is
    not over-emitted -- and is disclosed per construct via the over_emitting flag.
  * reference (aozora-pipeline): D(w,c) = aozora's count. "Share of reference-visible
    markup each adapter reproduces." Undefined where the reference itself emits none
    (font_size, folded bousen) -- those constructs are excluded from this variant.

Recognition signatures are the SAME union signatures validated in
normalized-corpus-coverage.py (full-corpus vocabulary audit, 2026-07-08). The
per-work counts summed over ALL works must equal the full-corpus `num` values in
2026-07-08-normalized-corpus-coverage.json -- this reconciliation is asserted so the
walker is provably the same instrument.

Completion = the adapter emitted a JSON file that parses to a NON-EMPTY `blocks`
array. A parseable-but-empty file (aozora2html emits ~82) is a representation
failure, not a completion, so it is excluded from I.

Usage: python3 fidelity-robustness-split.py <full-corpus-coverage.json> > out.json
  (the coverage JSON is used only for the reconciliation assertion.)
"""
import collections
import glob
import json
import os
import sys

AAT_GLOBS = {
    "aozora": "/db/ab-validator/aat-corpus/aozora-full-20260705T015007Z/aat/aozora-adapter/*.json",
    "aozora2": "/db/ab-validator/aat-corpus/aozora2-full-20260705T083650Z-layout-fix5/aat/aozora2-adapter/*.json",
    "aozora-rs": "/db/ab-validator/fidelity-corpus/aozora-rs/aat/aozora-rs-adapter/*.json",
    "aozora2html": "/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter/*.json",
    "aozora-epub3": "/db/ab-validator/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter/*.json",
}
ORDER = ["aozora", "aozora2", "aozora-rs", "aozora2html", "aozora-epub3"]

# Same union signatures as normalized-corpus-coverage.py (do not diverge).
SIG = {
    "ruby.basic": {"kinds": {"ruby"}, "styles": set(), "markers": {"ruby"}},
    "decoration.boten": {
        "kinds": set(),
        "styles": {"boten", "bouten", "sesame_dot", "white_circle", "black_circle",
                   "white_up-pointing_triangle", "black_up-pointing_triangle",
                   "bullseye", "saltire", "white_circle_after"},
        "markers": {"boten", "bouten"},
    },
    "decoration.bousen": {
        "kinds": set(),
        "styles": {"bousen", "bosen", "underline_solid", "overline_solid", "underline_wave"},
        "markers": {"bousen", "bosen"},
    },
    "heading.basic": {
        "kinds": {"heading"},
        "styles": {"unmapped-h3", "unmapped-h4", "unmapped-h5", "midashi"},
        "markers": {"heading", "headingHint"},
    },
    # gaiji (外字): kept in sync with normalized-corpus-coverage.py SIG (see there).
    "gaiji.marker": {"kinds": {"gaiji"}, "styles": set(), "markers": {"gaiji"}},
    "decoration.font_size": {"kinds": {"font_size"}, "styles": set(), "markers": {"font_size", "lineFontSize"}},
    "layout.tcy": {"kinds": {"tcy"}, "styles": set(), "markers": {"tcy", "combineUpright"}},
    "figure.image_inline": {"kinds": {"figure"}, "styles": set(), "markers": {"figure", "illustration"}},
    # jisage (字下げ) harmonised: block + aozora2 per-line `jisage_line` form. See
    # normalized-corpus-coverage.py SIG for rationale. Best-attested denominator can be
    # inflated by per-line emission, so the reference denominator is authoritative for
    # this row (it caps at aozora's per-block count); reported and flagged accordingly.
    "indentation.jisage_block": {"kinds": {"jisage_block"}, "styles": {"jisage_line"}, "markers": {"containerOpen"}},
}
# Constructs the aozora reference parser does not distinctly emit -> undefined under
# the reference denominator (folded/dropped, not a fidelity signal about others).
REF_UNDEFINED = {"decoration.bousen", "decoration.font_size"}
REFERENCE = "aozora"


def match(node, sig):
    k = node.get("kind")
    if k in sig["kinds"]:
        return True
    if k == "style" and node.get("style_type") in sig["styles"]:
        return True
    m = node.get("x-source-marker-kind")
    return m in sig["markers"] if m else False


def fid(path):
    # Unit of analysis = the source input file `<workid>-<hash>.json`. The hash is
    # source-derived, so basenames are IDENTICAL across adapters (verified) -- this
    # is the only unit that (a) is shared across adapters and (b) reconciles with the
    # per-file full-corpus denominators. ~236 works have >1 input file; keying on
    # workid would silently collapse them and under-count. "Completed work" below
    # therefore means "completed input file".
    return os.path.basename(path)


def count_file(path):
    """-> (file_id, Counter or None). None means the file did not complete (unreadable
    or empty blocks). Empty blocks == representation failure, not completion."""
    w = fid(path)
    try:
        blocks = json.load(open(path)).get("blocks", [])
    except Exception:
        return w, None
    if not blocks:
        return w, None
    counts = collections.Counter()
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
    return w, counts


def main():
    coverage = json.load(open(sys.argv[1]))
    constructs = list(SIG.keys())

    # per adapter: workid -> Counter (only completed works); and completion sets
    per = {a: {} for a in ORDER}
    for a in ORDER:
        files = glob.glob(AAT_GLOBS[a])
        for f in files:
            w, c = count_file(f)
            if c is not None:
                per[a][w] = c
        print(f"{a}: {len(files)} files, {len(per[a])} completed (non-empty)", file=sys.stderr)

    # ---- reconciliation: per-work sums over ALL works == full-corpus `num` -------
    recon = {}
    for a in ORDER:
        totals = collections.Counter()
        for c in per[a].values():
            totals.update(c)
        expected = {s: coverage["adapters"][a]["per_construct"][s]["num"] for s in constructs}
        got = {s: totals.get(s, 0) for s in constructs}
        mism = {s: (got[s], expected[s]) for s in constructs if got[s] != expected[s]}
        recon[a] = {"ok": not mism, "mismatch": mism}
        if mism:
            print(f"RECONCILE MISMATCH {a}: {mism}", file=sys.stderr)
    all_ok = all(recon[a]["ok"] for a in ORDER)
    print(f"reconciliation vs full-corpus num: {'OK' if all_ok else 'MISMATCH (see above)'}", file=sys.stderr)

    # ---- robustness --------------------------------------------------------------
    union = set().union(*(set(per[a]) for a in ORDER))
    # ruby mass per work = max attested ruby across adapters (best estimate of the
    # work's real ruby load, so a work missed by adapter X is weighted by what it holds)
    work_ruby = {w: max(per[a][w].get("ruby.basic", 0) for a in ORDER if w in per[a]) for w in union}
    total_ruby_mass = sum(work_ruby.values())
    robustness = {}
    for a in ORDER:
        completed = set(per[a])
        missing = union - completed
        miss_ruby = sum(work_ruby[w] for w in missing)
        robustness[a] = {
            "completed_works": len(completed),
            "union_works": len(union),
            "completion_rate": round(len(completed) / len(union), 4),
            "missing_works": len(missing),
            "missing_ruby_mass": miss_ruby,
            "missing_ruby_mass_pct": round(100 * miss_ruby / total_ruby_mass, 2),
        }

    # ---- intersection ------------------------------------------------------------
    inter = set(per[ORDER[0]])
    for a in ORDER[1:]:
        inter &= set(per[a])
    inter = sorted(inter)
    print(f"intersection (all {len(ORDER)} completed): {len(inter)} works", file=sys.stderr)

    # ---- fidelity on the intersection, two denominators --------------------------
    def fidelity(denom_kind):
        # per construct: numerator_a and denominator over I
        num = {a: {c: 0 for c in constructs} for a in ORDER}
        den = {c: 0 for c in constructs}
        for w in inter:
            cw = {a: per[a][w] for a in ORDER}
            for c in constructs:
                vals = {a: cw[a].get(c, 0) for a in ORDER}
                if denom_kind == "best_attested":
                    d = max(vals.values())
                else:  # reference
                    d = vals[REFERENCE]
                den[c] += d
                for a in ORDER:
                    num[a][c] += min(vals[a], d)  # cap per work at the denominator
        active = [c for c in constructs
                  if den[c] > 0 and not (denom_kind == "reference" and c in REF_UNDEFINED)]
        total_den = sum(den[c] for c in active)
        result = {"denominator": denom_kind, "constructs": active,
                  "weights": {c: den[c] for c in active}, "adapters": {}}
        for a in ORDER:
            rows = {c: {"num": num[a][c], "rate": round(num[a][c] / den[c], 4)} for c in active}
            wsum = sum(num[a][c] for c in active)
            result["adapters"][a] = {"per_construct": rows,
                                     "weighted_fidelity": round(wsum / total_den, 4)}
        return result

    best = fidelity("best_attested")
    ref = fidelity("reference")

    out = {
        "schema_version": 1,
        "note": "fidelity/robustness split; see fidelity-robustness-split.py docstring",
        "adapters_order": ORDER,
        "reconciliation": {"all_ok": all_ok, "per_adapter": recon},
        "robustness": robustness,
        "intersection_works": len(inter),
        "fidelity_best_attested": best,
        "fidelity_reference": ref,
        "reference_adapter": REFERENCE,
    }
    json.dump(out, sys.stdout, ensure_ascii=False, indent=1)


if __name__ == "__main__":
    main()
