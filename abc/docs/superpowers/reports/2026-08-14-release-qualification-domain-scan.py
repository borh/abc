#!/usr/bin/env python3
"""Scalar-domain scan for the release-qualification design (spec E20).

rfc8785-safe-integer-json-string-v1 fails closed on non-integral numbers and
on integers outside the interoperable range, so every qualified output must
stay inside that domain. Reports floats, out-of-range integers, non-string
object keys, and unexpected types.

Usage: ./2026-08-14-release-qualification-domain-scan.py 'DIR/*.json'
"""
import json, glob, sys, collections
from concurrent.futures import ProcessPoolExecutor
SAFE = 9007199254740991
def scan(path):
    bad = collections.Counter()
    mx, mn = 0, 0
    with open(path, "rb") as fh:
        doc = json.load(fh)
    stack = [("", doc)]
    while stack:
        p, v = stack.pop()
        if isinstance(v, dict):
            for k, x in v.items():
                if not isinstance(k, str): bad["nonstring_key"] += 1
                stack.append((p + "." + k, x))
        elif isinstance(v, list):
            for x in v: stack.append((p + "[]", x))
        elif isinstance(v, bool) or v is None or isinstance(v, str):
            pass
        elif isinstance(v, int):
            if v > SAFE or v < -SAFE: bad["int_out_of_safe_range:" + p] += 1
        elif isinstance(v, float):
            bad["float:" + p] += 1
        else:
            bad["other:" + type(v).__name__] += 1
    return bad
def main(pat, jobs=32):
    files = sorted(glob.glob(pat))
    total = collections.Counter()
    with ProcessPoolExecutor(jobs) as ex:
        for b in ex.map(scan, files, chunksize=32):
            total.update(b)
    return len(files), total
if __name__ == "__main__":
    n, t = main(sys.argv[1])
    print("files=%d  violations=%d" % (n, sum(t.values())))
    for k, v in t.most_common(10): print("   %-60s %d" % (k, v))
import json, glob, sys, collections
from concurrent.futures import ProcessPoolExecutor
SAFE = 9007199254740991
def scan(path):
    bad = collections.Counter()
    mx, mn = 0, 0
    with open(path, "rb") as fh:
        doc = json.load(fh)
    stack = [("", doc)]
    while stack:
        p, v = stack.pop()
        if isinstance(v, dict):
            for k, x in v.items():
                if not isinstance(k, str): bad["nonstring_key"] += 1
                stack.append((p + "." + k, x))
        elif isinstance(v, list):
            for x in v: stack.append((p + "[]", x))
        elif isinstance(v, bool) or v is None or isinstance(v, str):
            pass
        elif isinstance(v, int):
            if v > SAFE or v < -SAFE: bad["int_out_of_safe_range:" + p] += 1
        elif isinstance(v, float):
            bad["float:" + p] += 1
        else:
            bad["other:" + type(v).__name__] += 1
    return bad
def main(pat, jobs=32):
    files = sorted(glob.glob(pat))
    total = collections.Counter()
    with ProcessPoolExecutor(jobs) as ex:
        for b in ex.map(scan, files, chunksize=32):
            total.update(b)
    return len(files), total
if __name__ == "__main__":
    n, t = main(sys.argv[1])
    print("files=%d  violations=%d" % (n, sum(t.values())))
    for k, v in t.most_common(10): print("   %-60s %d" % (k, v))
