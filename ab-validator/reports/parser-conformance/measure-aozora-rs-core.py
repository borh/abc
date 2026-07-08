#!/usr/bin/env python
"""Faithful capability measurement of aozora-rs-core, bypassing its adapter's
lexer fallback. Runs the adapter in `--mode retokenized` (dump.rs) to get the
parser's own token stream, projects it to the spec `kind` vocabulary, and scores
against the conformance suite's expected.nodes kind sequence.

Buckets each nodes-bearing vector:
  pass                 projected == expected
  recognized-different parser recognized construct(s) but sequence differs
  not-recognized       parser produced no construct tokens (only text/br) -> a
                       genuine aozora-rs-core capability gap
"""
from __future__ import annotations

import collections
import glob
import json
import subprocess
import sys
from pathlib import Path

BIN = "adapters/aozora-rs/target/release/aozora-rs-adapter"

# aozora-rs-core Deco tag -> spec kind (single inline node emitted on deco_begin).
INLINE = {
    "boten": "bouten",
    "bosen": "emphasis",
    "bold": "emphasis", "italic": "emphasis", "smaller": "emphasis",
    "bigger": "emphasis", "sub": "emphasis", "sup": "emphasis",
    "kerning": "emphasis", "mama": "emphasis",
    "ruby": "ruby",
    "a_head": "heading", "b_head": "heading", "c_head": "heading",
    "hin_v": "combineUpright",
}
# block-scope Deco -> paired containerOpen/containerClose.
BLOCK_PAIR = {"indent", "hanging", "low_flying", "horizontal_layout"}
# single-marker block Deco -> one spec node on deco_begin.
SINGLE = {"grounded": "alignEnd", "vh_centre": "center"}
# warichu, okurigana -> no spec-kind counterpart (left unmapped, recorded).


def project(tokens):
    out, unmapped, has_construct = [], set(), False
    for tk in tokens:
        t = tk["t"]
        if t == "deco_begin":
            has_construct = True
            d = tk["d"]
            if d in INLINE:
                out.append(INLINE[d])
            elif d in BLOCK_PAIR:
                out.append("containerOpen")
            elif d in SINGLE:
                out.append(SINGLE[d])
            else:
                unmapped.add(d)
        elif t == "deco_end":
            if tk["d"] in BLOCK_PAIR:
                out.append("containerClose")
        elif t == "kunten":
            has_construct = True
            out.append("kaeriten")
        elif t == "figure":
            has_construct = True
            out.append("illustration")
    return out, unmapped, has_construct


def main():
    vectors_dir = sys.argv[1]
    cats = collections.Counter()
    fam_gap = collections.Counter()
    must = collections.Counter()
    rows = []
    for p in sorted(glob.glob(f"{vectors_dir}/*/vector.json")):
        v = json.loads(Path(p).read_text(encoding="utf-8"))
        nodes = v["expected"].get("nodes")
        if nodes is None:
            continue
        expected = [n["kind"] for n in nodes]
        proc = subprocess.run([BIN, "--mode", "retokenized"], input=v["source"],
                              text=True, capture_output=True)
        try:
            tokens = json.loads(proc.stdout)["retokenized"]
        except Exception:
            tokens = []
        projected, _unmapped, has_construct = project(tokens)
        fam = v["meta"]["feature"]
        level = v["meta"]["level"]
        if projected == expected:
            cat = "pass"
        elif has_construct:
            cat = "recognized-different"
        else:
            cat = "not-recognized"
            fam_gap[fam] += 1
        cats[cat] += 1
        if level == "must":
            must[cat] += 1
        rows.append({"vector": v["name"], "family": fam, "level": level,
                     "category": cat, "expected": expected, "projected": projected})

    total = sum(cats.values())
    result = {
        "schema_version": 1,
        "parser": "aozora-rs-core",
        "measured_via": "retokenized dump (adapter fidelity gate bypassed)",
        "total_nodes_vectors": total,
        "categories": dict(cats),
        "must_categories": dict(must),
        "recognized_any": cats["pass"] + cats["recognized-different"],
        "not_recognized": cats["not-recognized"],
        "family_capability_gaps": dict(fam_gap),
        "rows": rows,
    }
    print(json.dumps(result, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
