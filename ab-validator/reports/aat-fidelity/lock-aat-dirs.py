#!/usr/bin/env python3
"""Emit resolved AAT directories from a fidelity lock, one per line.

The parser-IR audit tools (`ab-aat-to-parser-ir audit-corpus` /
`tei-eaj-structural-expansion`) are Rust binaries that take repeated `--aat-dir`
flags; they have no lock/run-set knowledge (that is resolve's job — Phase 2). This
thin CLI turns a resolved lock into the `--aat-dir` values so those recipes source
their dumps from the SAME content-verified pins as the fidelity run-set, instead of
drifting `env_var_or_default` `/db` paths. Lock reading stays single-sourced in
`fidelity_lock` (the only place that understands the lock schema).

Usage:
  lock-aat-dirs.py <lock.json>            # one DIR per line
  lock-aat-dirs.py <lock.json> --labeled  # one LABEL=DIR per line
  lock-aat-dirs.py <lock.json> --order aozora-rs aozora2 ...  # fix emit order
"""

import argparse
import sys
from pathlib import Path

REPORTS_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPORTS_ROOT / "lib"))
from fidelity_lock import load_lock, lock_aat_dirs  # noqa: E402


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("lock", help="resolved fidelity lock (fidelity-lock/v1)")
    ap.add_argument(
        "--labeled",
        action="store_true",
        help="emit LABEL=DIR (default: DIR only)",
    )
    ap.add_argument(
        "--order",
        nargs="*",
        default=None,
        metavar="LABEL",
        help="adapter labels in the desired emit order (default: lock order); "
        "fails closed if a requested label is absent",
    )
    args = ap.parse_args()

    lock = load_lock(args.lock)
    dirs = lock_aat_dirs(lock, order=args.order)
    for label, path in dirs.items():
        print(f"{label}={path}" if args.labeled else path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
