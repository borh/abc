#!/usr/bin/env python3
"""Resolve a run-set manifest into a fidelity lock (Phase 2, Move B).

This is the single impure boundary: it reads the manifest, validates it (fails
closed), resolves every adapter's AAT directory to an absolute path, and emits a
`fidelity-lock/v1` value. The compute tools then read ONLY the lock — no env, no
CWD, no `/db` discovery. See
docs/superpowers/specs/2026-07-09-fidelity-phase2-resolve-compute-lock.md.

The lock is a pure function of the manifest (+ AB_DB_ROOT interpolation): no
timestamp is recorded, so `resolve` twice yields an identical lock.
"""

from __future__ import annotations

import argparse
import glob
import json
from pathlib import Path
import sys

REPORTS_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPORTS_ROOT / "lib"))
from aat_runs import adapter_aat_dirs, load_run_set, validate_run_set  # noqa: E402
from fidelity_lock import LOCK_FORMAT  # noqa: E402


def resolve_lock(
    run_set: dict,
    *,
    repo_root: str = ".",
    verify_dirs: bool = True,
) -> dict:
    """Validate the run-set and return the resolved lock, or raise on failure.

    Folds the two checks the pipeline previously ran separately (validate-aat-run-set
    coherence + the run-coverage-report.sh non-empty-dir gate) into the one resolve
    step, and fails closed if either fails. Descriptor (`metadata.json`) presence is
    NOT required — some dumps legitimately lack one today (behavior-preserving with the
    pre-Phase-2 pipeline, which validated coherence without `--require-paths`).
    """
    errors = validate_run_set(run_set, repo_root=repo_root, require_paths=False)
    if errors:
        raise ValueError(
            "run-set validation failed; refusing to emit a lock:\n  - "
            + "\n  - ".join(errors)
        )
    dirs = adapter_aat_dirs(run_set)  # all adapters, manifest order, absolute
    if verify_dirs:
        empty = [
            f"{label} -> {aat_dir}"
            for label, aat_dir in dirs.items()
            if not glob.glob(str(Path(aat_dir) / "*.json"))
        ]
        if empty:
            raise ValueError(
                "run-set resolves to empty AAT dir(s); refusing to emit a lock:\n  - "
                + "\n  - ".join(empty)
            )
    return {
        "lock_format": LOCK_FORMAT,
        "run_set_id": run_set.get("run_set_id"),
        "manifest": run_set.get("_run_set_path"),
        "adapters": {label: {"aat_dir": aat_dir} for label, aat_dir in dirs.items()},
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "run_set", nargs="?", help="Run-set manifest JSON path. Defaults to AB_AAT_RUN_SET."
    )
    parser.add_argument(
        "--repo-root", default=".", help="Repository root containing flake.lock files."
    )
    parser.add_argument("--out", help="Write the lock here (default: stdout).")
    parser.add_argument(
        "--no-verify-dirs",
        action="store_true",
        help="Skip the non-empty-AAT-dir gate (for tests without a populated /db).",
    )
    args = parser.parse_args()

    run_set = load_run_set(args.run_set)
    lock = resolve_lock(
        run_set,
        repo_root=args.repo_root,
        verify_dirs=not args.no_verify_dirs,
    )
    text = json.dumps(lock, ensure_ascii=False, indent=2) + "\n"
    if args.out:
        Path(args.out).write_text(text, encoding="utf-8")
    else:
        sys.stdout.write(text)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
