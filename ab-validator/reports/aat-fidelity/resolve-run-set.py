#!/usr/bin/env python3
"""Resolve a run-set manifest into a fidelity lock.

This is the single impure boundary: it reads the manifest, validates it (fails
closed), resolves every adapter's AAT directory to an absolute path, and emits a
`fidelity-lock/v1` value. The compute tools then read ONLY the lock — no env, no
CWD, no `/db` discovery.

The lock is a pure function of the manifest (+ AB_DB_ROOT interpolation): no
timestamp is recorded, so `resolve` twice yields an identical lock.
"""

from __future__ import annotations

import argparse
import glob
import json
import os
from pathlib import Path
import sys

REPORTS_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPORTS_ROOT / "lib"))
from aat_runs import DEFAULT_DB_ROOT, adapter_aat_dirs, load_run_set, validate_run_set  # noqa: E402
from aat_hash import hash_aat_dir  # noqa: E402
from fidelity_lock import LOCK_FORMAT  # noqa: E402


def _expected_content_hash(run_set: dict, label: str) -> str | None:
    entry = run_set.get("adapters", {}).get(label)
    if isinstance(entry, dict):
        expected = entry.get("expected")
        if isinstance(expected, dict):
            value = expected.get("content_hash")
            if isinstance(value, str) and value:
                return value
    return None


def resolve_lock(
    run_set: dict,
    *,
    repo_root: str = ".",
    verify_dirs: bool = True,
    verify_content: bool = True,
) -> dict:
    """Validate the run-set and return the resolved lock, or raise on failure.

    Folds three checks into the one resolve step and fails closed if any fails:
    validate-aat-run-set coherence, the non-empty-dir gate, and (Move A) content
    verification — each pinned adapter's on-disk AAT tree is hashed and compared to the
    manifest's `expected.content_hash`, so a mutated/truncated/swapped `/db` dump is
    caught here (the idempotency gate), not silently computed on. Descriptor
    (`metadata.json`) presence is NOT required — some dumps legitimately lack one today.

    verify_content is on by default (hashing the pinned dumps is ~seconds); pass
    verify_content=False for fast dev iteration. Adapters without a pinned
    `content_hash` are recorded but not gated (allows gradual pinning).
    """
    errors = validate_run_set(run_set, repo_root=repo_root, require_paths=False)
    if errors:
        raise ValueError(
            "run-set validation failed; refusing to emit a lock:\n  - " + "\n  - ".join(errors)
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

    adapters: dict[str, dict] = {}
    hash_errors: list[str] = []
    for label, aat_dir in dirs.items():
        entry: dict[str, str] = {"aat_dir": aat_dir}
        expected = _expected_content_hash(run_set, label)
        if verify_content:
            actual = hash_aat_dir(aat_dir)
            if expected and actual != expected:
                hash_errors.append(
                    f"{label}: content hash mismatch — the dump at {aat_dir} does not "
                    f"match the pinned identity (mutated / truncated / regenerated?): "
                    f"expected {expected}, got {actual}"
                )
            entry["content_hash"] = actual
        elif expected:
            entry["content_hash"] = expected  # recorded but unverified (fast mode)
        adapters[label] = entry
    if hash_errors:
        raise ValueError(
            "dump content verification failed; refusing to emit a lock:\n  - "
            + "\n  - ".join(hash_errors)
        )

    return {
        "lock_format": LOCK_FORMAT,
        "run_set_id": run_set.get("run_set_id"),
        "manifest": run_set.get("_run_set_path"),
        # The deployment binding this lock was resolved against (F8: AB_DB_ROOT is a
        # per-deployment bulk-storage root, confined here to the resolve boundary).
        "db_root": os.environ.get("AB_DB_ROOT", DEFAULT_DB_ROOT),
        "adapters": adapters,
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
    parser.add_argument(
        "--no-verify-content",
        action="store_true",
        help="Skip hashing dumps to verify content against the pinned content_hash "
        "(fast dev iteration; the pinned hash is recorded unverified).",
    )
    args = parser.parse_args()

    run_set = load_run_set(args.run_set)
    lock = resolve_lock(
        run_set,
        repo_root=args.repo_root,
        verify_dirs=not args.no_verify_dirs,
        verify_content=not args.no_verify_content,
    )
    text = json.dumps(lock, ensure_ascii=False, indent=2) + "\n"
    if args.out:
        Path(args.out).write_text(text, encoding="utf-8")
    else:
        sys.stdout.write(text)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
