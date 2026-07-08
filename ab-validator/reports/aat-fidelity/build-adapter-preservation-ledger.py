#!/usr/bin/env python3
"""Corrected gap ledger — live oracle as ground truth, matrix only for repr/prevalence."""

import tomllib
import json
import sys
from collections import defaultdict
from pathlib import Path

sp = Path(sys.argv[1])
cases = tomllib.load(open("data/aat-oracle-cases.toml", "rb"))["case"]
c2s = {c["id"]: c.get("syntax_row_ids", []) for c in cases}
rows = json.load(open(sp / "oracle-report.json"))["rows"]
matrix = {
    r["id"]: r for r in tomllib.load(open("data/aozora-syntax-coverage.toml", "rb"))["syntax"]
}
ADS = ["aozora2", "aozora-rs", "aozora"]

agg = defaultdict(lambda: defaultdict(lambda: [0, 0]))
cased = set()
for r in rows:
    for sid in c2s.get(r["case_id"], []):
        cased.add(sid)
        a = agg[sid][r["adapter"]]
        a[1] += 1
        a[0] += r["oracle_status"] == "pass"


def occ(row):
    return (row.get("corpus_prevalence") or {}).get("total_occurrences") or 0


ledger, stale = [], 0
for sid, row in matrix.items():
    prev = row.get("corpus_prevalence") or {}
    mad = row.get("adapters") or {}
    live = {}
    for a in ADS:
        p, t = agg[sid].get(a, [0, 0])
        live[a] = "pass" if (t and p == t) else ("fail" if t else "no-case")
    preserved_live = [a for a in ADS if live[a] == "pass"]
    mfid = {a: (mad.get(a, {}) or {}).get("aat_fidelity", "absent") for a in ADS}
    stale_here = [a for a in ADS if live[a] == "pass" and mfid[a] != "preserved"]
    stale += len(stale_here)
    ledger.append(
        {
            "id": sid,
            "category": row.get("category"),
            "representability": (row.get("representability") or {}).get("status"),
            "occ": occ(row),
            "works": prev.get("works_with_feature"),
            "has_case": sid in cased,
            "matrix_fidelity": mfid,
            "live": live,
            "preserved_live": preserved_live,
            "stale_adapters": stale_here,
        }
    )

n = len(ledger)
verified = [x for x in ledger if x["preserved_live"]]
uncovered = [x for x in ledger if not x["has_case"]]
uncovered_occ = [x for x in uncovered if x["occ"] > 0]
summary = {
    "constructs": n,
    "adapters": ADS,
    "verified_preserved": len(verified),
    "uncovered": len(uncovered),
    "uncovered_nonzero_occ": len(uncovered_occ),
    "stale_cells": stale,
    "no_adapter_preserves_with_case": sum(
        1 for x in ledger if x["has_case"] and not x["preserved_live"]
    ),
}


# order: uncovered-with-occ first (real unknowns), then verified by occ desc, then 0-occ uncovered
def key(x):
    grp = 0 if (not x["has_case"] and x["occ"] > 0) else (2 if not x["has_case"] else 1)
    return (grp, -x["occ"])


ledger.sort(key=key)
json.dump(
    {"summary": summary, "ledger": ledger},
    open(sp / "corrected-ledger.json", "w"),
    ensure_ascii=False,
    indent=1,
)
print(json.dumps(summary, indent=2))
print("\nstale cells (adapter live-passes but matrix != preserved):", stale)
print(
    "uncovered constructs:",
    [x["id"] for x in uncovered],
    "(all occ:",
    [x["occ"] for x in uncovered],
    ")",
)
