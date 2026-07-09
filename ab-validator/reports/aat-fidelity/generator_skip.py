"""Decide whether an existing AAT dump is fresh for the current inputs.

Reads a dump's `metadata.json` (written by run-aat-full.sh, carrying the F6
`input_set_hash` + `output_content_hash`), and re-verifies its `aat/` tree.
A dump is FRESH iff its recorded `input_set_hash` equals the current inputs' hash
AND its `aat/` tree still hashes to the recorded `output_content_hash`. Any
anomaly (no/broken metadata, changed inputs, changed/missing outputs) → not fresh
→ recompute. Fail toward correctness. See
docs/superpowers/specs/2026-07-09-batch-run-staleness-skip-recompute-design.md.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Any

_LIB = Path(__file__).resolve().parents[1] / "lib"
sys.path.insert(0, str(_LIB))

import aat_hash  # noqa: E402
import freshness  # noqa: E402


def is_fresh(out_dir: str | Path, current_input_set_hash: str) -> dict[str, Any]:
    """Return ``{"fresh": bool, "reason": str}`` for an existing dump directory."""
    out = Path(out_dir)
    meta_path = out / "metadata.json"
    if not meta_path.is_file():
        return {"fresh": False, "reason": "no metadata.json"}
    try:
        meta = json.loads(meta_path.read_text(encoding="utf-8"))
    except (ValueError, OSError):
        return {"fresh": False, "reason": "malformed metadata.json"}
    if not isinstance(meta, dict):
        return {"fresh": False, "reason": "metadata.json is not an object"}
    decision = freshness.classify(
        meta.get("input_set_hash"),
        current_input_set_hash,
        meta.get("output_content_hash"),
        lambda: aat_hash.hash_aat_dir(out / "aat"),
    )
    if decision.verdict is freshness.Verdict.INPUT_MISMATCH:
        return {"fresh": False, "reason": "input_set_hash differs (inputs changed)"}
    if decision.verdict is freshness.Verdict.OUTPUT_UNREADABLE:
        return {"fresh": False, "reason": "aat outputs missing or unreadable"}
    if decision.verdict is freshness.Verdict.OUTPUT_MISMATCH:
        return {"fresh": False, "reason": "aat content hash mismatch"}
    return {"fresh": True, "reason": "inputs unchanged and outputs verify"}


def main(argv: list[str] | None = None) -> int:
    """Exit 0 if the dump is fresh (caller may skip), 1 if not (recompute).
    Prints the reason to stderr."""
    ap = argparse.ArgumentParser(description="Is an existing AAT dump fresh?")
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--input-set-hash", required=True)
    a = ap.parse_args(sys.argv[1:] if argv is None else argv)
    result = is_fresh(a.out_dir, a.input_set_hash)
    print(result["reason"], file=sys.stderr)
    return 0 if result["fresh"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
