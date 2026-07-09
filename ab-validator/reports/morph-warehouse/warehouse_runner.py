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

import argparse
import json
import subprocess
import sys
from pathlib import Path
from typing import Any, Callable

_LIB = Path(__file__).resolve().parents[1] / "lib"
sys.path.insert(0, str(_LIB))

import run_identity  # noqa: E402

import warehouse_identity  # noqa: E402  (sibling module)
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


def _parse_dicts(pairs: list[str]) -> dict[str, str]:
    """Turn ``--dict name=/store/path`` pairs into a {name: path} map."""
    out: dict[str, str] = {}
    for pair in pairs:
        name, sep, path = pair.partition("=")
        if not sep or not name:
            raise SystemExit(f"--dict expects name=path, got {pair!r}")
        out[name] = path
    return out


def main(argv: list[str] | None = None) -> int:
    """CLI: gate a warehouse run. Wrapper args describe the identity; the compute
    command follows a literal ``--``. On a fresh index hit the command is skipped;
    otherwise it is run verbatim (byte-identical to an ungated invocation) and the
    run is recorded.

    Example:
        warehouse_runner.py --aat-dir D --warehouse-dir W --run-id R \\
            --warehouse-profile full --analyzer vibrato --analyzer sudachi-a \\
            --dict sudachi=/nix/store/… --schema-file crates/…/schema.sql \\
            -- cargo run --release -p ab-morph-run -- analyze-aat --aat-dir D …
    """
    raw = list(sys.argv[1:] if argv is None else argv)
    if "--" in raw:
        cut = raw.index("--")
        wrapper_argv, command = raw[:cut], raw[cut + 1:]
    else:
        wrapper_argv, command = raw, []

    ap = argparse.ArgumentParser(description="Gate a morph-warehouse run on input-set identity.")
    ap.add_argument("--aat-dir", required=True)
    ap.add_argument("--warehouse-dir", required=True)
    ap.add_argument("--run-id", required=True)
    ap.add_argument("--warehouse-profile", required=True)
    ap.add_argument("--analyzer", action="append", default=[], dest="analyzers")
    ap.add_argument("--ortho-detect", default=None)
    ap.add_argument("--works-parquet", default=None)
    ap.add_argument("--dict", action="append", default=[], dest="dicts",
                    help="name=store_path (repeatable); nix store paths are content ids")
    ap.add_argument("--schema-file", action="append", default=[], dest="schema_files")
    ap.add_argument("--force", action="store_true")
    args = ap.parse_args(wrapper_argv)

    if not command:
        ap.error("missing compute command after '--'")

    identity_object = warehouse_identity.build_identity_object(
        aat_dir=args.aat_dir,
        dictionaries=_parse_dicts(args.dicts),
        analyzers=args.analyzers,
        warehouse_profile=args.warehouse_profile,
        schema_files=args.schema_files,
        ortho_detect=args.ortho_detect,
        works_parquet=args.works_parquet,
    )

    def compute() -> None:
        subprocess.run(command, check=True)

    try:
        result = resolve_compute_record(
            warehouse_dir=args.warehouse_dir,
            run_id=args.run_id,
            identity_object=identity_object,
            compute=compute,
            force=args.force,
        )
    except subprocess.CalledProcessError as exc:
        # Preserve the batch's own exit code (nothing was recorded — the
        # exception propagated before the manifest/index write).
        print(f"compute command failed (exit {exc.returncode})", file=sys.stderr)
        return exc.returncode
    if result["action"] == "skip":
        print(f"SKIP {result['run_id']}: fresh run already indexed for these inputs "
              f"({result['input_set_hash']}); pass --force to recompute.", file=sys.stderr)
    print(json.dumps(result))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
