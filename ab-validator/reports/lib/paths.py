"""Resolve canonical paths within the Soranoha/ab-validator workspace."""

from __future__ import annotations

import os
from pathlib import Path

_LIB_DIR = Path(__file__).resolve().parent


def repo_root() -> Path:
    """Return the ab-validator repository root."""
    return _LIB_DIR.parents[1]


def workspace_root() -> Path:
    """Return the monorepo workspace root when present."""
    if value := os.environ.get("AB_WORKSPACE_ROOT"):
        return Path(value).resolve()

    root = repo_root().resolve()
    parent = root.parent
    if (parent / "abc").is_dir() and (parent / "ab-validator").is_dir():
        return parent
    return parent


def abc_root() -> Path:
    """Return the ABC component root or an explicit override."""
    if value := os.environ.get("AB_ABC_ROOT"):
        return Path(value).resolve()
    candidate = workspace_root() / "abc"
    if candidate.is_dir():
        return candidate
    return repo_root() / "data" / "abc-schemas"


def schemas_dir() -> Path:
    """Return the authoritative ABC JSON schema directory."""
    candidate = abc_root() / "schemas"
    if candidate.is_dir():
        return candidate
    return repo_root() / "data" / "abc-schemas" / "schemas"


def policy_dir() -> Path:
    """Return the authoritative ABC publication policy directory."""
    candidate = abc_root() / "data"
    if candidate.is_dir():
        return candidate
    return repo_root() / "data" / "abc-schemas" / "data"


def display_path(path: Path) -> str:
    """Return a stable repo/workspace-relative path for report JSON."""
    resolved = path.resolve()
    for root in (repo_root().resolve(), workspace_root().resolve()):
        try:
            return str(resolved.relative_to(root))
        except ValueError:
            pass
    return str(path)
