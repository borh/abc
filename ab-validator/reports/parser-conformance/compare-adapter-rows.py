#!/usr/bin/env python3
"""Compare one adapter's conformance rows across two summary JSONs.

Used by the Phase 3 gates: stage 0 requires the ab-aozora lane identical
to the frozen Phase 2 echo summaries; rotations diff against the previous
stage and the differences are reviewed against expectations.

Exit 0 = identical; 1 = any row differs / vector set differs; 2 = usage or
reference error (unreadable file, adapter absent from either file).
"""
import argparse
import json
import sys


def rows_for(path, adapter):
    try:
        doc = json.load(open(path))
    except (OSError, json.JSONDecodeError) as err:
        print(f"ERROR: {path}: {err}", file=sys.stderr)
        raise SystemExit(2)
    rows = {r["vector"]: r for r in doc.get("rows", []) if r.get("adapter") == adapter}
    if not rows:
        print(f"ERROR: {path}: no rows for adapter {adapter!r}", file=sys.stderr)
        raise SystemExit(2)
    return rows


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("old")
    ap.add_argument("new")
    ap.add_argument("--adapter", required=True)
    args = ap.parse_args()
    old = rows_for(args.old, args.adapter)
    new = rows_for(args.new, args.adapter)
    diffs = []
    for vector in sorted(set(old) | set(new)):
        if vector not in old:
            diffs.append(f"{vector}: only in new")
        elif vector not in new:
            diffs.append(f"{vector}: only in old")
        elif old[vector] != new[vector]:
            diffs.append(f"{vector}: old={json.dumps(old[vector], ensure_ascii=False)}\n"
                         f"  new={json.dumps(new[vector], ensure_ascii=False)}")
    for d in diffs:
        print(d)
    print(f"compared={len(set(old) | set(new))} differing={len(diffs)}")
    return 1 if diffs else 0


if __name__ == "__main__":
    raise SystemExit(main())
