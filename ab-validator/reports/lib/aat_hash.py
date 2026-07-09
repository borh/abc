"""Content hash of an AAT dump directory — the dump's identity (Phase 2, Move A).

The per-work AAT `*.json` files are deterministic (content-derived filenames, stable
key order, no timestamps/RNG — see the Move A scoping in
docs/superpowers/specs/2026-07-09-fidelity-phase2-resolve-compute-lock.md), so a hash
over their raw bytes is a stable content identity: it changes iff the dump's bytes
change. This lets resolve detect a mutated / truncated / swapped `/db` dump under a
fixed pin and fail closed.

Raw bytes (no JSON re-parse) keeps it fast — hashing 25 GB of dumps is I/O-bound, not
CPU-bound on a full parse of every file. The one adapter whose *regenerated* output was
not byte-stable, `aozora-rs` (embedded wall-clock `meta.metrics`), is fixed at the
source (A4b); its already-pinned dump still hashes stably here because the on-disk bytes
do not change unless the dump is re-generated.
"""

from __future__ import annotations

import hashlib
import os
from pathlib import Path

HASH_ALGO = "sha256"
HASH_PREFIX = "sha256:"
_READ_CHUNK = 1 << 20  # 1 MiB


def _file_digest(path: Path) -> bytes:
    h = hashlib.sha256()
    with path.open("rb") as f:
        while True:
            chunk = f.read(_READ_CHUNK)
            if not chunk:
                break
            h.update(chunk)
    return h.digest()


def hash_aat_dir(aat_dir: str | os.PathLike[str]) -> str:
    """Return `sha256:<hex>` content hash of every `*.json` under `aat_dir`.

    The hash folds the sorted relative POSIX paths together with each file's byte
    digest, so both the file set and every file's content are covered, and the result
    is independent of directory-walk order.
    """
    root = Path(aat_dir)
    if not root.is_dir():
        raise ValueError(f"not a directory: {root}")
    files = sorted(p.relative_to(root).as_posix() for p in root.rglob("*.json"))
    if not files:
        raise ValueError(f"no *.json files under {root}")
    top = hashlib.sha256()
    for rel in files:
        top.update(rel.encode("utf-8"))
        top.update(b"\0")
        top.update(_file_digest(root / rel))
        top.update(b"\n")
    return HASH_PREFIX + top.hexdigest()


def _main() -> int:
    import argparse
    import json

    ap = argparse.ArgumentParser(description="Content hash of an AAT dump directory.")
    ap.add_argument("aat_dir", help="AAT directory containing per-work *.json files")
    ap.add_argument("--json", action="store_true", help="emit {dir, file_count, content_hash}")
    args = ap.parse_args()
    root = Path(args.aat_dir)
    digest = hash_aat_dir(root)
    if args.json:
        n = sum(1 for _ in root.rglob("*.json"))
        json.dump({"aat_dir": str(root), "file_count": n, "content_hash": digest}, os.sys.stdout)
        os.sys.stdout.write("\n")
    else:
        print(digest)
    return 0


if __name__ == "__main__":
    raise SystemExit(_main())
