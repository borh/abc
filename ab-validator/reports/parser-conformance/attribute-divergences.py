#!/usr/bin/env python
"""Attribute each AAT-mode parser's conformance divergences to a *cause*, so the
cross-parser comparison is faithful about parser capability vs adapter losses.

The conformance harness scores AAT adapters via a projected spec-kind sequence.
A non-pass can mean two very different things:

  * the underlying parser never recognized the construct (true capability gap), or
  * the parser saw it but the adapter flattened/mis-serialized it (mapper gap,
    recoverable without touching the parser).

For each diverging vector we inspect the raw AAT and bucket it:

  pass            projected kind sequence == expected
  typed-mismatch  AAT carries typed nodes (style/ruby/gaiji/...) but the sequence
                  differs (wrong kind/count/order) -> classified, mapper-level
  raw-preserved   AAT carries a `raw` node holding the source marker text -> the
                  parser saw the markup but did not classify it -> mapper/IR-level
  dropped-to-text AAT is only text/paragraph, the markup is gone -> the parser
                  lost the construct -> genuine capability gap

`pass + typed-mismatch + raw-preserved` = parser-capable, mapper-recoverable.
`dropped-to-text` = the real fork-relevant capability gap.

Scoped to the Rust AAT-mode fork candidates (aozora2 -> aozora-core,
aozora-rs -> aozora-rs-core). The reference `aozora` (aozora-pipeline) is measured
faithfully via inspect elsewhere and needs no attribution.
"""

from __future__ import annotations

import collections
import importlib.util
import json
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent  # ab-validator/

# Reuse the harness's exact projection (single source of truth).
spec = importlib.util.spec_from_file_location("conf_harness", HERE / "run-aozora-notation-spec.py")
harness = importlib.util.module_from_spec(spec)
sys.modules["conf_harness"] = harness  # dataclass resolution needs this registered
spec.loader.exec_module(harness)
project_aat = harness.project_aat
expected_kind_seq = harness.expected_kind_seq

CANDIDATES = {
    "aozora2": [str(REPO / "adapters/aozora2/target/release/aozora2-adapter"), "--mode", "aat"],
    "aozora-rs": [
        str(REPO / "adapters/aozora-rs/target/release/aozora-rs-adapter"),
        "--mode",
        "aat",
    ],
}


def walk_kinds(blocks):
    """Collect all node kinds present anywhere in the AAT tree."""
    seen = set()

    def rec(node):
        seen.add(node.get("kind"))
        for child in node.get("content") or []:
            rec(child)
        for child in node.get("children") or []:
            rec(child)

    for b in blocks:
        rec(b)
    return seen


def categorize(blocks, projected, expected):
    if projected == expected:
        return "pass"
    kinds = walk_kinds(blocks)
    non_text = kinds - {"text", "paragraph"}
    if "raw" in kinds:
        return "raw-preserved"
    if non_text:
        return "typed-mismatch"
    return "dropped-to-text"


def run(adapter_cmd, source):
    proc = subprocess.run(
        adapter_cmd,
        input=source,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if proc.returncode != 0:
        return None
    try:
        return json.loads(proc.stdout)
    except json.JSONDecodeError:
        return None


def main():
    vectors_dir = Path(sys.argv[1])
    vectors = []
    for p in sorted(vectors_dir.glob("*/vector.json")):
        vectors.append(json.loads(p.read_text(encoding="utf-8")))

    out = {"schema_version": 1, "candidates": {}}
    for label, cmd in CANDIDATES.items():
        per_cat = collections.Counter()
        per_family_cap = collections.Counter()  # capable (pass/typed/raw)
        per_family_gap = collections.Counter()  # dropped-to-text
        rows = []
        for v in vectors:
            exp = expected_kind_seq(v)
            if exp is None:
                continue
            aat = run(cmd, v["source"])
            if aat is None:
                per_cat["adapter-error"] += 1
                rows.append(
                    {
                        "vector": v["name"],
                        "family": v["meta"]["feature"],
                        "level": v["meta"]["level"],
                        "category": "adapter-error",
                    }
                )
                continue
            projected, _ = project_aat(aat.get("blocks", []) or [])
            cat = categorize(aat.get("blocks", []) or [], projected, exp)
            per_cat[cat] += 1
            fam = v["meta"]["feature"]
            if cat == "dropped-to-text":
                per_family_gap[fam] += 1
            else:
                per_family_cap[fam] += 1
            rows.append(
                {
                    "vector": v["name"],
                    "family": fam,
                    "level": v["meta"]["level"],
                    "category": cat,
                    "expected": exp,
                    "projected": projected,
                }
            )
        recoverable = per_cat["pass"] + per_cat["typed-mismatch"] + per_cat["raw-preserved"]
        out["candidates"][label] = {
            "categories": dict(per_cat),
            "parser_capable_or_recoverable": recoverable,
            "capability_gap_dropped_to_text": per_cat["dropped-to-text"],
            "family_gap_dropped": dict(per_family_gap),
            "rows": rows,
        }
    print(json.dumps(out, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
