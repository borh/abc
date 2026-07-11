#!/usr/bin/env python3
"""Vector-for-vector echo between two adapter lanes of one or more
run-aozora-notation-spec.py summary JSONs.

Two lanes echo when, for EVERY vector, status + failures + skips +
warnings are identical. Structural problems (a lane label absent, vector
sets differing between lanes, duplicate vector rows) are exit 2, never a
silent pass.

Exit 0 = echo; 1 = divergence; 2 = structural/usage error."""

import argparse
import json
import sys

FIELDS = ("status", "failures", "skips", "warnings")


def lane(rows, label, path):
    out = {}
    for row in rows:
        if row.get("adapter") != label:
            continue
        vector = row["vector"]
        if vector in out:
            print(
                f"ERROR: {path}: duplicate vector {vector!r} for adapter {label!r}", file=sys.stderr
            )
            raise SystemExit(2)
        out[vector] = json.dumps({f: row.get(f) for f in FIELDS}, sort_keys=True)
    if not out:
        print(f"ERROR: {path}: adapter label {label!r} absent", file=sys.stderr)
        raise SystemExit(2)
    return out


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("summaries", nargs="+")
    ap.add_argument("--lane-a", required=True)
    ap.add_argument("--lane-b", required=True)
    ap.add_argument("--out")
    args = ap.parse_args()
    compared, differing = 0, []
    for path in args.summaries:
        rows = json.load(open(path))["rows"]
        a = lane(rows, args.lane_a, path)
        b = lane(rows, args.lane_b, path)
        mismatched = sorted(set(a) ^ set(b))
        if mismatched:
            print(
                f"ERROR: {path}: vector sets differ between lanes: {mismatched[:10]}",
                file=sys.stderr,
            )
            raise SystemExit(2)
        for vector in sorted(a):
            compared += 1
            if a[vector] != b[vector]:
                differing.append(
                    {
                        "summary": path,
                        "vector": vector,
                        args.lane_a: json.loads(a[vector]),
                        args.lane_b: json.loads(b[vector]),
                    }
                )
    result = {
        "lane_a": args.lane_a,
        "lane_b": args.lane_b,
        "vectors_compared": compared,
        "differing_count": len(differing),
        "differing": differing[:50],
    }
    text = json.dumps(result, indent=2, ensure_ascii=False)
    print(text)
    if args.out:
        with open(args.out, "w") as handle:
            handle.write(text + "\n")
    return 0 if not differing else 1


if __name__ == "__main__":
    raise SystemExit(main())
