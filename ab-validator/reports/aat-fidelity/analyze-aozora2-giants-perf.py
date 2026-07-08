#!/usr/bin/env python3
"""Analyze the aozora-core (aozora2) giants-vs-controls perf run (handoff §3).

Splits the perf measurements into the two designed cohorts and answers the §3
question: is aozora-core SLOW EVERYWHERE, or catastrophic only on WORK-SPECIFIC
inputs? The control set is the largest works aozora2 *completed* (median size
>= the giants'), so if controls run fast while the giants blow up, the pathology
is structural (input-specific), not size-driven.

Usage: python3 analyze-aozora2-giants-perf.py <results.json> <workset-meta.json>
"""
import collections
import json
import statistics
import sys


def cohort_stats(rows):
    ok = [r for r in rows if r["status"] == "ok"]
    walls = [r["wall_s"] for r in ok if isinstance(r.get("wall_s"), (int, float))]
    return {
        "n": len(rows),
        "ok": len(ok),
        "timeout": sum(1 for r in rows if r["status"] == "timeout"),
        "errors": sum(1 for r in rows if str(r["status"]).startswith("error:")),
        "wall_median": round(statistics.median(walls), 2) if walls else None,
        "wall_max": round(max(walls), 2) if walls else None,
        "wall_min": round(min(walls), 2) if walls else None,
    }


def main():
    results = json.load(open(sys.argv[1]))
    meta = json.load(open(sys.argv[2]))
    giants = set(meta["giants"])
    controls = set(meta["controls"])

    rows = [r for r in results["measurements"] if r["stage"] == "full_adapter"]
    g = [r for r in rows if r["work_id"] in giants]
    c = [r for r in rows if r["work_id"] in controls]

    print(f"limit_s = {results['limit_s']}")
    print(f"\nGIANTS   (aozora2 failed to complete these at full corpus): {cohort_stats(g)}")
    print(f"CONTROLS (largest works aozora2 DID complete):              {cohort_stats(c)}")

    # error-code distribution among giants (points at the failure mode)
    codes = collections.Counter(r["status"] for r in g if r["status"] != "ok")
    print(f"\nGiant failure modes: {dict(codes)}")
    # sample stderr previews from non-timeout errors (what blows up)
    for r in g:
        if str(r["status"]).startswith("error:"):
            print(f"  {r['work_id']} exit={r.get('exit_code')} size={r.get('size')}: "
                  f"{(r.get('stderr_preview') or '').strip()[:200]}")

    # do any giants complete within the limit? (slow-but-finite vs unbounded)
    g_ok = [r for r in g if r["status"] == "ok"]
    if g_ok:
        print(f"\n{len(g_ok)} giants completed within limit (slow-but-finite):")
        for r in sorted(g_ok, key=lambda r: -(r.get("wall_s") or 0))[:10]:
            print(f"  {r['work_id']} wall={r.get('wall_s')}s size={r.get('size')}")

    # verdict
    cs = cohort_stats(c)
    gs = cohort_stats(g)
    print("\n--- VERDICT ---")
    if cs["wall_max"] is not None and cs["timeout"] == 0 and (gs["timeout"] + gs["errors"]) > 0:
        print(f"Controls (larger works) all completed, max {cs['wall_max']}s; "
              f"giants show {gs['timeout']} timeouts + {gs['errors']} errors. "
              f"=> WORK-SPECIFIC pathology, not size-driven.")
    else:
        print("Pattern not clearly work-specific; inspect cohorts above.")


if __name__ == "__main__":
    main()
