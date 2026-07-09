"""Resolve → compute → record wrapper for morph-warehouse runs.

Gates the expensive `ab-morph-run analyze-aat` batch on the input-set identity:
compute the identity, look for a valid indexed run, and **skip** it if one is
fresh — otherwise run the compute step and record the run (manifest + by-input
index). The compute step is injected (a callable), exactly like the nix-bridge
`:runner`, so the gate logic is unit-testable without the 45 GB Rust batch. The
recipe supplies the real command; this module only decides skip-vs-run and records.

Skip-recompute is additive: `--force`, or any non-fresh index state
(missing/stale/invalid), runs the compute step exactly as today. See
docs/superpowers/specs/2026-07-09-batch-run-staleness-skip-recompute-design.md.
"""

from __future__ import annotations

import sys
from pathlib import Path
from typing import Any, Callable

_LIB = Path(__file__).resolve().parents[1] / "lib"
sys.path.insert(0, str(_LIB))

import run_identity  # noqa: E402

import warehouse_index  # noqa: E402  (sibling module)


def resolve_compute_record(
    *,
    warehouse_dir: str | Path,
    run_id: str,
    identity_object: dict[str, Any],
    compute: Callable[[], None],
    force: bool = False,
) -> dict[str, Any]:
    """Decide skip-vs-run for a warehouse run and record it if computed.

    - Computes `input_set_hash` from `identity_object`.
    - Unless `force`, resolves the by-input index: a `fresh` hit returns without
      calling `compute` (the skip).
    - Otherwise calls `compute` (which must materialize `runs/<run_id>/`; it may
      raise — the exception propagates and nothing is recorded), then writes the
      run manifest and by-input link.

    Returns `{"action": "skip"|"computed", "reason": <index-status>,
    "input_set_hash": ..., "run_id": ..., "run_dir": ...}`.
    """
    ish = run_identity.input_set_hash(identity_object)
    if not force:
        res = warehouse_index.check(warehouse_dir, ish)
        if res["status"] == "fresh":
            return {
                "action": "skip",
                "reason": "fresh",
                "input_set_hash": ish,
                "run_id": res.get("run_id", run_id),
                "run_dir": res.get("run_dir"),
            }
        reason = res["status"]  # missing | stale | invalid -> recompute
    else:
        reason = "forced"

    compute()  # raises on failure -> nothing recorded

    run_dir = Path(warehouse_dir) / "runs" / run_id
    warehouse_index.write_run_manifest(
        run_dir, identity_object=identity_object, input_set_hash=ish
    )
    warehouse_index.link_by_input(warehouse_dir, ish, run_id)
    return {
        "action": "computed",
        "reason": reason,
        "input_set_hash": ish,
        "run_id": run_id,
        "run_dir": str(run_dir),
    }
