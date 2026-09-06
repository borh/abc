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

    return repo_root().resolve().parent


def research_root() -> Path:
    """Return the research component root or an explicit override."""
    return (
        Path(os.environ["AB_RESEARCH_ROOT"]).resolve()
        if "AB_RESEARCH_ROOT" in os.environ
        else repo_root() / "research"
    )


def schemas_dir() -> Path:
    """Return the authoritative research JSON schema directory."""
    return research_root() / "schemas"


def policy_dir() -> Path:
    """Return the authoritative research policy directory."""
    return research_root() / "data"


def tei_p5_root() -> Path:
    """Return the configured TEI P5 reference root."""
    if value := os.environ.get("AB_TEI_P5_ROOT"):
        return Path(value).resolve()
    raise RuntimeError("AB_TEI_P5_ROOT is required for TEI P5 source lookup")


def display_path(path: Path) -> str:
    """Return a stable repo/workspace-relative path for report JSON."""
    resolved = path.resolve()
    for root in (repo_root().resolve(), workspace_root().resolve()):
        try:
            return str(resolved.relative_to(root))
        except ValueError:
            pass
    return str(path)
