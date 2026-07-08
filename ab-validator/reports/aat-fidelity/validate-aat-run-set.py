#!/usr/bin/env python3
"""Validate the AAT run-set selected for fidelity reports."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import sys

REPORTS_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPORTS_ROOT / "lib"))
from aat_runs import load_run_set, validate_run_set  # noqa: E402


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("run_set", nargs="?", help="Run-set JSON path. Defaults to AB_AAT_RUN_SET.")
    parser.add_argument(
        "--repo-root", default=".", help="Repository root containing flake.lock files."
    )
    parser.add_argument(
        "--require-paths",
        action="store_true",
        help="Require selected AAT directories and run descriptors to exist.",
    )
    args = parser.parse_args()

    run_set = load_run_set(args.run_set)
    errors = validate_run_set(
        run_set,
        repo_root=args.repo_root,
        require_paths=args.require_paths,
    )
    report = {
        "schema_version": 1,
        "run_set_id": run_set.get("run_set_id"),
        "status": "failed" if errors else "passed",
        "error_count": len(errors),
        "errors": errors,
    }
    json.dump(report, sys.stdout, ensure_ascii=False, indent=2)
    sys.stdout.write("\n")
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
