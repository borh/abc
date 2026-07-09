"""General content hash of a directory tree — the all-files generalization of
``aat_hash.hash_aat_dir``.

Folds sorted relative POSIX paths with each file's byte digest, so both the file
set and every file's content are covered, independent of directory-walk order —
the same construction `aat_hash` uses for `*.json`, generalized to an arbitrary
glob plus an exclude-list. Kept as a SEPARATE function (not a refactor of
`aat_hash.hash_aat_dir`) on purpose: `hash_aat_dir`'s exact output is pinned as
`expected.content_hash` across ~25 GB of fidelity dumps, so its bytes must not
move. See docs/superpowers/specs/2026-07-09-batch-run-staleness-skip-recompute-design.md.
"""

from __future__ import annotations

import hashlib
import os
from pathlib import Path

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


def tree_hash(
    root: str | os.PathLike[str],
    *,
    pattern: str = "*",
    exclude_names: tuple[str, ...] = (),
) -> str:
    """Return ``sha256:<hex>`` over every file matching ``pattern`` under ``root``.

    - ``pattern``: an ``rglob`` glob (default ``*`` = all files).
    - ``exclude_names``: top-level relative POSIX paths to skip (e.g. a manifest
      that lives in the tree it describes).
    Raises ``ValueError`` if ``root`` is not a directory or matches no files, so a
    caller that expected content fails closed rather than hashing emptiness.
    """
    base = Path(root)
    if not base.is_dir():
        raise ValueError(f"not a directory: {base}")
    exclude = set(exclude_names)
    rels = sorted(
        rel
        for p in base.rglob(pattern)
        if p.is_file()
        for rel in [p.relative_to(base).as_posix()]
        if rel not in exclude
    )
    if not rels:
        raise ValueError(f"no files matching {pattern!r} under {base}")
    top = hashlib.sha256()
    for rel in rels:
        top.update(rel.encode("utf-8"))
        top.update(b"\0")
        top.update(_file_digest(base / rel))
        top.update(b"\n")
    return HASH_PREFIX + top.hexdigest()
