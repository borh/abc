"""by-input index + run manifest for morph-warehouse skip-recompute.

A completed run under ``<warehouse>/runs/<run-id>/`` records a ``run-manifest.json``
(its ``input_set_hash`` + ``identity_object`` + a content hash over all of its
output files) and is indexed at ``<warehouse>/by-input/<hex> -> ../runs/<run-id>``.
The skip decision re-verifies the recorded output hash on read, mirroring the
workflow cache's ``valid-cached-node-result``:

- ``fresh``   — index resolves, manifest matches, outputs re-verify → reuse;
- ``stale``   — outputs changed/corrupted since publish → recompute;
- ``missing`` — no run indexed for these inputs → compute;
- ``invalid`` — index/manifest broken (dangling link, absent/malformed manifest,
  or manifest ``input_set_hash`` mismatch) → recompute.

Content only; no timestamps. Run manifests are rebuildable, so the index is
derived state, not identity. See
docs/superpowers/specs/2026-07-09-batch-run-staleness-skip-recompute-design.md.
"""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
from typing import Any

MANIFEST_NAME = "run-manifest.json"
MANIFEST_FORMAT = "morph-warehouse-run-manifest/v1"
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


def _index_name(input_set_hash: str) -> str:
    """by-input filenames are the bare hex (no ``sha256:`` colon) for path-safety."""
    return input_set_hash.split(":", 1)[-1]


def hash_run_dir(run_dir: str | Path, *, exclude_names: tuple[str, ...] = (MANIFEST_NAME,)) -> str:
    """Return ``sha256:<hex>`` over every file under ``run_dir`` (any extension).

    Folds sorted relative POSIX paths with each file's byte digest, so both the
    file set and every file's content are covered, independent of walk order.
    Top-level files whose name is in ``exclude_names`` are skipped, so the manifest
    can live in the run dir without being part of its own hash.
    """
    root = Path(run_dir)
    if not root.is_dir():
        raise ValueError(f"not a directory: {root}")
    exclude = set(exclude_names)
    rels = sorted(
        rel
        for p in root.rglob("*")
        if p.is_file()
        for rel in [p.relative_to(root).as_posix()]
        if rel not in exclude
    )
    if not rels:
        raise ValueError(f"no output files under {root}")
    top = hashlib.sha256()
    for rel in rels:
        top.update(rel.encode("utf-8"))
        top.update(b"\0")
        top.update(_file_digest(root / rel))
        top.update(b"\n")
    return "sha256:" + top.hexdigest()


def write_run_manifest(
    run_dir: str | Path, *, identity_object: dict[str, Any], input_set_hash: str
) -> Path:
    """Write ``<run_dir>/run-manifest.json`` recording the run's identity and a
    content hash over its outputs (excluding the manifest). Returns the path."""
    root = Path(run_dir)
    manifest = {
        "manifest_format": MANIFEST_FORMAT,
        "input_set_hash": input_set_hash,
        "identity_object": identity_object,
        "output_content_hash": hash_run_dir(root),
    }
    path = root / MANIFEST_NAME
    path.write_text(
        json.dumps(manifest, ensure_ascii=False, sort_keys=True, indent=2) + "\n",
        encoding="utf-8",
    )
    return path


def link_by_input(warehouse_dir: str | Path, input_set_hash: str, run_id: str) -> Path:
    """Create/replace ``<warehouse>/by-input/<hex> -> ../runs/<run_id>`` (relative
    symlink). Returns the link path."""
    index = Path(warehouse_dir) / "by-input"
    index.mkdir(parents=True, exist_ok=True)
    link = index / _index_name(input_set_hash)
    if link.is_symlink() or link.exists():
        link.unlink()
    link.symlink_to(Path("..") / "runs" / run_id)
    return link


def check(warehouse_dir: str | Path, input_set_hash: str) -> dict[str, Any]:
    """Resolve the by-input index for ``input_set_hash`` and re-verify the run's
    outputs. Returns ``{"status": fresh|stale|missing|invalid, ...}``."""
    link = Path(warehouse_dir) / "by-input" / _index_name(input_set_hash)
    if not link.exists():
        if link.is_symlink():
            return {"status": "invalid", "reason": "dangling by-input link"}
        return {"status": "missing"}
    run_dir = link.resolve()
    manifest_path = run_dir / MANIFEST_NAME
    if not manifest_path.is_file():
        return {"status": "invalid", "reason": "missing run manifest", "run_dir": str(run_dir)}
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (ValueError, OSError):
        return {"status": "invalid", "reason": "malformed run manifest", "run_dir": str(run_dir)}
    if not isinstance(manifest, dict):
        return {"status": "invalid", "reason": "run manifest is not an object",
                "run_dir": str(run_dir)}
    if manifest.get("input_set_hash") != input_set_hash:
        return {"status": "invalid", "reason": "manifest input_set_hash mismatch",
                "run_dir": str(run_dir)}
    recorded = manifest.get("output_content_hash")
    try:
        actual = hash_run_dir(run_dir)
    except (ValueError, OSError):
        # Outputs vanished/unreadable since publish (only the manifest survives, or
        # the run dir is gone). Fail toward recompute, not an uncaught exception.
        return {"status": "stale", "reason": "run outputs missing or unreadable",
                "run_dir": str(run_dir)}
    if actual != recorded:
        return {"status": "stale", "reason": "output content hash mismatch",
                "recorded": recorded, "actual": actual, "run_dir": str(run_dir)}
    return {"status": "fresh", "run_dir": str(run_dir), "run_id": run_dir.name}
