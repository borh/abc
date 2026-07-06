"""Resolve canonical paths within the ab-validator repository."""

from __future__ import annotations

from pathlib import Path

_LIB_DIR = Path(__file__).resolve().parent


def repo_root() -> Path:
    """Return the ab-validator repository root."""
    return _LIB_DIR.parents[1]


def schemas_dir() -> Path:
    """Return the vendored ABC JSON schema directory."""
    return repo_root() / "data" / "abc-schemas" / "schemas"


def policy_dir() -> Path:
    """Return the vendored ABC publication policy directory."""
    return repo_root() / "data" / "abc-schemas" / "data"
